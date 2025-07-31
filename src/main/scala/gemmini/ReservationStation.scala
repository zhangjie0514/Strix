
package gemmini

import chisel3._
import chisel3.util._
import freechips.rocketchip.tile.RoCCCommand
import freechips.rocketchip.util.PlusArg
import GemminiISA._
import Util._

import midas.targetutils.PerfCounter
import midas.targetutils.SynthesizePrintf


// TODO unify this class with GemminiCmdWithDeps
class ReservationStationIssue[T <: Data](cmd_t: T, id_width: Int) extends Bundle {
  val valid = Output(Bool())
  val ready = Input(Bool())
  val cmd = Output(cmd_t.cloneType)
  val rob_id = Output(UInt(id_width.W))

  def fire(dummy: Int=0) = valid && ready
}

// TODO we don't need to store the full command in here. We should be able to release the command directly into the relevant controller and only store the associated metadata in the ROB. This would reduce the size considerably
class ReservationStation[T <: Data : Arithmetic, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V],
                                                                       cmd_t: GemminiCmd) extends Module {
  import config._

  val block_rows = tileRows * meshRows      //块行数
  val block_cols = tileColumns * meshColumns//块列数

  val max_instructions_completed_per_type_per_cycle = 2 // Every cycle, at most two instructions of a single "type" (ld/st/ex) can be completed: one through the io.completed port, and the other if it is a "complete-on-issue" instruction
  //每种类型的指令在每个周期内最多完成两条

  val io = IO(new Bundle {
    val alloc = Flipped(Decoupled(cmd_t.cloneType))

    val completed = Flipped(Valid(UInt(ROB_ID_WIDTH.W))) //6.W

    val issue = new Bundle {
      val ld = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val st = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val ex = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH)
      val add_test = new ReservationStationIssue(cmd_t, ROB_ID_WIDTH) //2024.11.10修改
    }

    val conv_ld_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W)) //位宽是2
    val conv_ex_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val conv_st_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))

    val matmul_ld_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val matmul_ex_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))
    val matmul_st_completed = Output(UInt(log2Up(max_instructions_completed_per_type_per_cycle+1).W))

    val busy = Output(Bool())

    val counter = new CounterEventIO()
  })

  // TODO make this a ChiselEnum
  val ldq :: exq :: stq :: add_testq :: Nil = Enum(4) //2024.11.10修改
  val q_t = ldq.cloneType

  class OpT extends Bundle {
    val start = local_addr_t.cloneType
    val end = local_addr_t.cloneType
    val wraps_around = Bool()
    //判断两个操作是否重叠
    def overlaps(other: OpT): Bool = {
      ((other.start <= start && (start < other.end || other.wraps_around)) ||
        (start <= other.start && (other.start < end || wraps_around))) &&
        !(start.is_garbage() || other.start.is_garbage()) // TODO the "is_garbage" check might not really be necessary
    }
  }
  
  //定义一个寄存器记录已经分配的指令数量
  val instructions_allocated = RegInit(0.U(32.W))
  when (io.alloc.fire) {
    instructions_allocated := instructions_allocated + 1.U
  }
  dontTouch(instructions_allocated)

  //
  class Entry extends Bundle {
    val q = q_t.cloneType//表示操作队列类型?

    val is_config = Bool()//标志该条目是否是配置指令

    val opa = UDValid(new OpT)//操作数a?
    val opa_is_dst = Bool()   //表示 opa 是否是目的操作数
    val opb = UDValid(new OpT)//操作数b?

    // val op1 = UDValid(new OpT)
    // val op1 = UDValid(new OpT)
    // val op2 = UDValid(new OpT)
    // val dst = UDValid(new OpT)

    val issued = Bool()//表示这个条目是否已经被发出（即指令是否已经开始执行）

    val complete_on_issue = Bool()//表示这个条目是否在发出时就完成（即不需要等待执行完成）

    val cmd = cmd_t.cloneType//表示这个条目对应的指令

    // instead of one large deps vector, we need 3 separate ones if we want
    // easy indexing, small area while allowing them to be different sizes
    val deps_ld = Vec(reservation_station_entries_ld, Bool())
    val deps_ex = Vec(reservation_station_entries_ex, Bool())
    val deps_st = Vec(reservation_station_entries_st, Bool())
    val deps_add_test = Vec(4, Bool()) //2024.11.10修改

    //表示这个条目是否准备好被发出
    def ready(dummy: Int = 0): Bool = !(deps_ld.reduce(_ || _) || deps_ex.reduce(_ || _) || deps_st.reduce(_ || _) || deps_add_test.reduce(_ || _)) //2024.11.10修改

    // Debugging signals
    val allocated_at = UInt(instructions_allocated.getWidth.W)
  }

  assert(isPow2(reservation_station_entries_ld))
  assert(isPow2(reservation_station_entries_ex))
  assert(isPow2(reservation_station_entries_st))

  val entries_ld = Reg(Vec(reservation_station_entries_ld, UDValid(new Entry)))
  val entries_ex = Reg(Vec(reservation_station_entries_ex, UDValid(new Entry)))
  val entries_st = Reg(Vec(reservation_station_entries_st, UDValid(new Entry)))
  val entries_add_test = Reg(Vec(4, UDValid(new Entry))) //2024.11.10修改

  val entries = entries_ld ++ entries_ex ++ entries_st ++ entries_add_test //2024.11.10修改

  val empty_ld = !entries_ld.map(_.valid).reduce(_ || _)//加载条目是否为空
  val empty_ex = !entries_ex.map(_.valid).reduce(_ || _)//执行条目是否为空
  val empty_st = !entries_st.map(_.valid).reduce(_ || _)//存储条目是否为空
  val empty_add_test = !entries_add_test.map(_.valid).reduce(_ || _) //2024.11.10修改
  val full_ld = entries_ld.map(_.valid).reduce(_ && _)//加载条目是否已满
  val full_ex = entries_ex.map(_.valid).reduce(_ && _)//执行条目是否已满
  val full_st = entries_st.map(_.valid).reduce(_ && _)//存储条目是否已满
  val full_add_test = entries_add_test.map(_.valid).reduce(_ && _) //2024.11.10修改

  val empty = !entries.map(_.valid).reduce(_ || _)//检查整个条目集合是否为空
  val full = entries.map(_.valid).reduce(_ && _)  //检查整个条目集合是否已满

  //表示当前保留站中有效条目的数量
  val utilization = PopCount(entries.map(e => e.valid)) // TODO it may be cheaper to count the utilization in a register, rather than performing a PopCount
  
  //solitary_preload 用于跟踪保留站是否接收到一个"预加载"（preload）指令，但尚未接收到后续的"计算"（compute）指令。
  //这可以帮助判断保留站的状态，特别是在进行指令调度时，确保预加载指令被正确处理。
  val solitary_preload = RegInit(false.B) // This checks whether or not the reservation station received a "preload" instruction, but hasn't yet received the following "compute" instruction
  
  //表示保留站是否处于忙碌状态
  //保留站不为空，并且不是只有一个孤立的预加载指令
  io.busy := !empty && !(utilization === 1.U && solitary_preload)
  
  // Tell the conv and matmul FSMs if any of their issued instructions completed
  val conv_ld_issue_completed = WireInit(false.B)//表示那些在发出时立即完成的卷积加载指令
  val conv_st_issue_completed = WireInit(false.B)
  val conv_ex_issue_completed = WireInit(false.B)

  val conv_ld_completed = WireInit(false.B)//表示那些通过正常执行路径完成的卷积加载指令（即经过一定的执行周期后完成）
  val conv_st_completed = WireInit(false.B)
  val conv_ex_completed = WireInit(false.B)

  val matmul_ld_issue_completed = WireInit(false.B)
  val matmul_st_issue_completed = WireInit(false.B)
  val matmul_ex_issue_completed = WireInit(false.B)

  val matmul_ld_completed = WireInit(false.B)
  val matmul_st_completed = WireInit(false.B)
  val matmul_ex_completed = WireInit(false.B)
  
  io.conv_ld_completed := conv_ld_issue_completed +& conv_ld_completed
  io.conv_st_completed := conv_st_issue_completed +& conv_st_completed
  io.conv_ex_completed := conv_ex_issue_completed +& conv_ex_completed

  io.matmul_ld_completed := matmul_ld_issue_completed +& matmul_ld_completed
  io.matmul_st_completed := matmul_st_issue_completed +& matmul_st_completed
  io.matmul_ex_completed := matmul_ex_issue_completed +& matmul_ex_completed

  // Config values set by programmer
  val a_stride = Reg(UInt(a_stride_bits.W))//表示 a 矩阵的步长，位宽为 a_stride_bits
  val c_stride = Reg(UInt(c_stride_bits.W))//表示 c 矩阵的步长，位宽为 c_stride_bits
  val a_transpose = Reg(Bool())            //表示 a 矩阵是否转置
  val ld_block_strides = Reg(Vec(load_states, UInt(block_stride_bits.W)))//表示加载块的步长
  val st_block_stride = block_rows.U//表示存储块的步长
  val pooling_is_enabled = Reg(Bool())
  val ld_pixel_repeats = Reg(Vec(load_states, UInt(pixel_repeats_bits.W))) // This is the ld_pixel_repeat MINUS ONE;表示加载像素重复的次数（减一）?

  val new_entry = Wire(new Entry)
  new_entry := DontCare

  val new_allocs_oh_ld = Wire(Vec(reservation_station_entries_ld, Bool()))//表示加载条目是否已分配
  val new_allocs_oh_ex = Wire(Vec(reservation_station_entries_ex, Bool()))//表示执行条目是否已分配
  val new_allocs_oh_st = Wire(Vec(reservation_station_entries_st, Bool()))//表示存储条目是否已分配
  val new_allocs_oh_add_test = Wire(Vec(4, Bool())) //2024.11.10修改

  val new_entry_oh = new_allocs_oh_ld ++ new_allocs_oh_ex ++ new_allocs_oh_st ++ new_allocs_oh_add_test //2024.11.10修改
  new_entry_oh.foreach(_ := false.B)//初始化所有分配状态为 false，表示初始状态下没有条目被分配

  val alloc_fire = io.alloc.fire()//表示一个新的指令分配请求被接受

  io.alloc.ready := false.B

  //表示有新的指令到来需要分配到保留站中。这段代码在这种情况下执行以下操作
  when (io.alloc.valid) {
    val spAddrBits = 32//定义地址位宽?
    val cmd = io.alloc.bits.cmd//提取指令命令
    val funct = cmd.inst.funct //提取指令功能码
    val funct_is_compute = funct === COMPUTE_AND_STAY_CMD || funct === COMPUTE_AND_FLIP_CMD//判断指令是否为计算指令
    val config_cmd_type = cmd.rs1(1,0) // TODO magic numbers

    new_entry.issued := false.B   //初始化为 false，表示新条目尚未发出
    new_entry.cmd := io.alloc.bits//设置新条目的命令
    new_entry.is_config := funct === CONFIG_CMD//判断并设置新条目是否为配置指令

    val op1 = Wire(UDValid(new OpT))
    op1.valid := false.B
    op1.bits := DontCare
    val op2 = Wire(UDValid(new OpT))
    op2.valid := false.B
    op2.bits := DontCare
    val dst = Wire(UDValid(new OpT))
    dst.valid := false.B
    dst.bits := DontCare
    assert(!(op1.valid && op2.valid && dst.valid))

    new_entry.opa_is_dst := dst.valid//设置 opa 是否为目的操作数

    //根据 dst 是否有效，设置 opa 和 opb
    when (dst.valid) {
      new_entry.opa := dst
      new_entry.opb := Mux(op1.valid, op1, op2)
    } .otherwise {
      new_entry.opa := Mux(op1.valid, op1, op2)
      new_entry.opb := op2
    }

    op1.valid := funct === PRELOAD_CMD || funct_is_compute//根据指令类型设置 op1 的有效性
    op1.bits.start := cmd.rs1.asTypeOf(local_addr_t)//设置 op1 的起始地址
    when (funct === PRELOAD_CMD) {
      // TODO check b_transpose here iff WS mode is enabled
      val preload_rows = cmd.rs1(48 + log2Up(block_rows + 1) - 1, 48)//从 cmd.rs1 中提取指定位宽的数据，表示预加载的行数
      op1.bits.end := op1.bits.start + preload_rows//结束地址等于起始地址加上预加载的行数
      op1.bits.wraps_around := op1.bits.start.add_with_overflow(preload_rows)._2
    }.otherwise {
      val rows = cmd.rs1(48 + log2Up(block_rows + 1) - 1, 48)//从 cmd.rs1 中提取指定位宽的数据，表示计算的行数
      val cols = cmd.rs1(32 + log2Up(block_cols + 1) - 1, 32)//从 cmd.rs1 中提取指定位宽的数据，表示计算的列数
      val compute_rows = Mux(a_transpose, cols, rows) * a_stride//根据是否转置计算实际的行数
      op1.bits.end := op1.bits.start + compute_rows//结束地址等于起始地址加上计算的行数
      op1.bits.wraps_around := op1.bits.start.add_with_overflow(compute_rows)._2
    }

    op2.valid := funct_is_compute || funct === STORE_CMD//如果当前指令是计算指令或存储指令，则 op2 有效（op2.valid = true）
    op2.bits.start := cmd.rs2.asTypeOf(local_addr_t)
    when (funct_is_compute) {
      val compute_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48)
      op2.bits.end := op2.bits.start + compute_rows
      op2.bits.wraps_around := op2.bits.start.add_with_overflow(compute_rows)._2
    }.elsewhen (pooling_is_enabled) {
      // If pooling is enabled, then we assume that this command simply mvouts everything in this accumulator bank from
      // start to the end of the bank // TODO this won't work when acc_banks =/= 2
      val acc_bank = op2.bits.start.acc_bank()//获取累加器银行的编号

      val next_bank_addr = WireInit(0.U.asTypeOf(local_addr_t))//初始化一个新的本地地址类型信号
      next_bank_addr.is_acc_addr := true.B                     //设置为累加器地址
      next_bank_addr.data := (acc_bank + 1.U) << local_addr_t.accBankRowBits//计算下一个累加器银行的地址

      op2.bits.end := next_bank_addr//设置 op2 的结束地址为下一个累加器银行的地址
      op2.bits.wraps_around := next_bank_addr.acc_bank() === 0.U
    }.otherwise {
      val block_stride = st_block_stride

      val mvout_cols = cmd.rs2(32 + mvout_cols_bits - 1, 32)//从 cmd.rs2 中提取指定位宽的数据，表示移动输出的列数
      val mvout_rows = cmd.rs2(48 + mvout_rows_bits - 1, 48)//从 cmd.rs2 中提取指定位宽的数据，表示移动输出的行数

      val mvout_mats = mvout_cols / block_cols.U(mvout_cols_bits.W) + (mvout_cols % block_cols.U =/= 0.U)
      val total_mvout_rows = ((mvout_mats - 1.U) * block_stride) + mvout_rows

      op2.bits.end := op2.bits.start + total_mvout_rows
      op2.bits.wraps_around := pooling_is_enabled || op2.bits.start.add_with_overflow(total_mvout_rows)._2
    }

    dst.valid := funct === PRELOAD_CMD || funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD
    dst.bits.start := cmd.rs2(31, 0).asTypeOf(local_addr_t)
    when (funct === PRELOAD_CMD) {
      val preload_rows = cmd.rs2(48 + log2Up(block_rows + 1) - 1, 48) * c_stride
      dst.bits.end := dst.bits.start + preload_rows
      dst.bits.wraps_around := dst.bits.start.add_with_overflow(preload_rows)._2
    }.otherwise {
      val id = MuxCase(0.U, Seq((new_entry.cmd.cmd.inst.funct === LOAD2_CMD) -> 1.U,
        (new_entry.cmd.cmd.inst.funct === LOAD3_CMD) -> 2.U))
      val block_stride = ld_block_strides(id)
      val pixel_repeats = ld_pixel_repeats(id)

      val mvin_cols = cmd.rs2(32 + mvin_cols_bits - 1, 32)
      val mvin_rows = cmd.rs2(48 + mvin_rows_bits - 1, 48)

      val mvin_mats = mvin_cols / block_cols.U(mvin_cols_bits.W) + (mvin_cols % block_cols.U =/= 0.U)
      val total_mvin_rows = ((mvin_mats - 1.U) * block_stride) + mvin_rows

      // TODO We have to know how the LoopConv's internals work here. Our abstractions are leaking
      if (has_first_layer_optimizations) {
        val start = cmd.rs2(31, 0).asTypeOf(local_addr_t)
        // TODO instead of using a floor-sub that's hardcoded to the Scratchpad bank boundaries, we should find some way of letting the programmer specify the start address
        dst.bits.start := Mux(start.is_acc_addr, start,
          Mux(start.full_sp_addr() > (local_addr_t.spRows / 2).U,
            start.floorSub(pixel_repeats, (local_addr_t.spRows / 2).U)._1,
            start.floorSub(pixel_repeats, 0.U)._1,
          )
        )
      }

      dst.bits.end := dst.bits.start + total_mvin_rows
      dst.bits.wraps_around := dst.bits.start.add_with_overflow(total_mvin_rows)._2
    }

    val is_load     = funct === LOAD_CMD || funct === LOAD2_CMD || funct === LOAD3_CMD || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_ex       = funct === PRELOAD_CMD || funct_is_compute || (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)
    val is_store    = funct === STORE_CMD || (funct === CONFIG_CMD && (config_cmd_type === CONFIG_STORE || config_cmd_type === CONFIG_NORM))
    val is_norm     = funct === CONFIG_CMD && config_cmd_type === CONFIG_NORM // normalization commands are a subset of store commands, so they still go in the store queue
    val is_add_test = funct === ADD_TEST || funct === PRINT //2021.11.10修改

    //判断指令将被放置到哪个队列中
    new_entry.q := Mux1H(Seq(
      is_load -> ldq,
      is_store -> stq,
      is_ex -> exq,
      is_add_test -> add_testq
    )) //2024.11.10修改

    //2024.11.10修改
    assert(is_load || is_store || is_ex || is_add_test) 
    //2024.11.10修改
    assert(ldq < exq && exq < stq && stq < add_testq)        

    //设置新条目 new_entry 的依赖关系，确保指令在调度和执行时遵循数据依赖和顺序约束
    val not_config = !new_entry.is_config
    when (is_load) {
      // war (after ex/st) | waw (after ex)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && !e.bits.issued }) // same q

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !new_entry.is_config && (
        (new_entry.opa.bits.overlaps(e.bits.opa.bits) && e.bits.opa.valid) || // waw if preload, war if compute
        (new_entry.opa.bits.overlaps(e.bits.opb.bits) && e.bits.opb.valid))}) // war

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && e.bits.opa.valid && not_config &&
        new_entry.opa.bits.overlaps(e.bits.opa.bits)})  // war
      new_entry.deps_add_test := VecInit(Seq.fill(4)(false.B)) //2024.11.10修改 
    }.elsewhen (is_ex) {
      // raw (after ld) | war (after st) | waw (after ld)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && e.bits.opa.valid && not_config && (
        new_entry.opa.bits.overlaps(e.bits.opa.bits) || // waw if preload, raw if compute
        new_entry.opb.bits.overlaps(e.bits.opa.bits))}) // raw

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !e.bits.issued }) // same q

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && e.bits.opa.valid && not_config && new_entry.opa_is_dst &&
        new_entry.opa.bits.overlaps(e.bits.opa.bits)})  // war
      new_entry.deps_add_test := VecInit(Seq.fill(4)(false.B)) //2024.11.10修改
    }.elsewhen (is_add_test){ //2024.11.10修改
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && !e.bits.issued })
      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && !e.bits.issued })
      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && !e.bits.issued })
      new_entry.deps_add_test := VecInit(entries_add_test.map{ e => e.valid && !e.bits.issued}) 
    }.otherwise {
      // raw (after ld/ex)
      new_entry.deps_ld := VecInit(entries_ld.map { e => e.valid && e.bits.opa.valid && not_config &&
        new_entry.opa.bits.overlaps(e.bits.opa.bits)})  // raw

      new_entry.deps_ex := VecInit(entries_ex.map { e => e.valid && e.bits.opa.valid && not_config &&
        e.bits.opa_is_dst && new_entry.opa.bits.overlaps(e.bits.opa.bits)}) // raw only if ex is preload

      new_entry.deps_st := VecInit(entries_st.map { e => e.valid && !e.bits.issued }) // same q
      new_entry.deps_add_test := VecInit(Seq.fill(4)(false.B)) //2024.11.10修改
    }

    new_entry.allocated_at := instructions_allocated//将新条目分配时的指令计数记录下来

    new_entry.complete_on_issue := (new_entry.is_config && new_entry.q =/= exq) || new_entry.q === add_testq
    //配置指令通常在发出时就可以完成，因为它们主要用于设置硬件参数，而不需要实际数据处理。
    //但如果配置指令属于执行队列（exq），则可能涉及更多的操作，不一定能在发出时立即完成。

    Seq(
      (ldq, entries_ld, new_allocs_oh_ld, reservation_station_entries_ld),
      (exq, entries_ex, new_allocs_oh_ex, reservation_station_entries_ex),
      (stq, entries_st, new_allocs_oh_st, reservation_station_entries_st),
      (add_testq, entries_add_test, new_allocs_oh_add_test, 4)) //2024.11.10修改
      .foreach { case (q, entries_type, new_allocs_type, entries_count) =>
        when (new_entry.q === q) {
          val is_full = PopCount(Seq(dst.valid, op1.valid, op2.valid)) > 1.U//判断一个条目中是否有多个有效操作数
          when (q =/= exq) { assert(!is_full) }
          // looking for the first invalid entry；找到第一个无效条目
          val alloc_id = MuxCase((entries_count - 1).U, entries_type.zipWithIndex.map { case (e, i) => !e.valid -> i.U })

          when (!entries_type(alloc_id).valid) {     //如果找到的槽是无效的
            io.alloc.ready := true.B                 //表示可以接受新的分配请求
            entries_type(alloc_id).valid := true.B   //将找到的槽标记为有效
            entries_type(alloc_id).bits := new_entry //将新条目分配到找到的槽中
            new_allocs_type(alloc_id) := true.B      //标记该槽为新分配的
          }
        }
      }

    when (io.alloc.fire) {
      when (new_entry.is_config && new_entry.q === exq) {
        a_stride := new_entry.cmd.cmd.rs1(31, 16) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        c_stride := new_entry.cmd.cmd.rs2(63, 48) // TODO magic numbers // TODO this needs to be kept in sync with ExecuteController.scala
        val set_only_strides = new_entry.cmd.cmd.rs1(7) // TODO magic numbers
        when (!set_only_strides) {
          a_transpose := new_entry.cmd.cmd.rs1(8) // TODO magic numbers
        }
      }.elsewhen(new_entry.is_config && new_entry.q === ldq) {
        val id = new_entry.cmd.cmd.rs1(4,3) // TODO magic numbers
        val block_stride = new_entry.cmd.cmd.rs1(31, 16) // TODO magic numbers
        val repeat_pixels = maxOf(new_entry.cmd.cmd.rs1(8 + pixel_repeats_bits - 1, 8), 1.U) // TODO we use a default value of pixel repeats here, for backwards compatibility. However, we should deprecate and remove this default value eventually
        ld_block_strides(id) := block_stride
        ld_pixel_repeats(id) := repeat_pixels - 1.U
      }.elsewhen(new_entry.is_config && new_entry.q === stq && !is_norm) {
        val pool_stride = new_entry.cmd.cmd.rs1(5, 4) // TODO magic numbers
        pooling_is_enabled := pool_stride =/= 0.U
      }.elsewhen(funct === PRELOAD_CMD) {
        solitary_preload := true.B
      }.elsewhen(funct_is_compute) {
        solitary_preload := false.B
      }
    }
  }

  // Issue commands which are ready to be issued
  Seq((ldq, io.issue.ld, entries_ld), (exq, io.issue.ex, entries_ex), (stq, io.issue.st, entries_st), (add_testq, io.issue.add_test, entries_add_test)) //2024.11.10修改
    .foreach { case (q, io, entries_type) =>

    //生成一个布尔值列表，表示哪些条目可以发出：条目有效 + 条目准备好发出 + 条目尚未发出
    val issue_valids = entries_type.map(e => e.valid && e.bits.ready() && !e.bits.issued)
    //使用优先级编码器选择一个可以发出的条目，返回一个一热编码（one-hot encoding）的选择信号
    val issue_sel = PriorityEncoderOH(issue_valids)
    //将一热编码转换为无符号整数作为索引
    val issue_id = OHToUInt(issue_sel)
    //生成全局发出ID，包含队列标识符和条目索引
    val global_issue_id = Cat(q.asUInt, issue_id.pad(log2Up(res_max_per_type)))
    assert(q.getWidth == 2)
    assert(global_issue_id.getWidth == 2 + log2Up(res_max_per_type))

    val issue_entry = Mux1H(issue_sel, entries_type)//选择一个发射条目

    io.valid := issue_valids.reduce(_||_)//如果有任何条目可以发出，设置 io.valid 为 true
    io.cmd := issue_entry.bits.cmd       //将选择的条目的命令设置为输出命令
    // use the most significant 2 bits to indicate instruction type
    io.rob_id := global_issue_id         //将全局发出ID设置为重排序缓冲区ID?

    val complete_on_issue = entries_type(issue_id).bits.complete_on_issue //指示条目是否在发出时完成
    val from_conv_fsm = entries_type(issue_id).bits.cmd.from_conv_fsm     //指示条目是否来自卷积有限状态机
    val from_matmul_fsm = entries_type(issue_id).bits.cmd.from_matmul_fsm //指示条目是否来自矩阵乘法有限状态机


    when (io.fire()) {
      entries_type.zipWithIndex.foreach { case (e, i) =>
        when (issue_sel(i)) {
          e.bits.issued := true.B
          e.valid := !e.bits.complete_on_issue
        }
      }
      //更新条目状态和依赖关系
      // Update the "deps" vectors of all instructions which depend on the one that is being issued
      Seq((ldq, entries_ld), (exq, entries_ex), (stq, entries_st), (add_testq, entries_add_test)) //2024.11.10修改
        .foreach { case (q_, entries_type_) =>

        entries_type_.zipWithIndex.foreach { case (e, i) =>
          val deps_type = if (q == ldq) e.bits.deps_ld
                          else if (q == exq) e.bits.deps_ex 
                          else if (q == stq) e.bits.deps_st
                          else e.bits.deps_add_test //2021.11.10修改
          when (q === q_) {
            deps_type(issue_id) := false.B
          }.otherwise {
            when (issue_entry.bits.complete_on_issue) {
              deps_type(issue_id) := false.B
            }
          }
        }
      }

      // If the instruction completed on issue, then notify the conv/matmul FSMs that another one of their commands
      // completed
      when (q === ldq) { conv_ld_issue_completed := complete_on_issue && from_conv_fsm }
      when (q === stq) { conv_st_issue_completed := complete_on_issue && from_conv_fsm }
      when (q === exq) { conv_ex_issue_completed := complete_on_issue && from_conv_fsm }

      when (q === ldq) { matmul_ld_issue_completed := complete_on_issue && from_matmul_fsm }
      when (q === stq) { matmul_st_issue_completed := complete_on_issue && from_matmul_fsm }
      when (q === exq) { matmul_ex_issue_completed := complete_on_issue && from_matmul_fsm }
    }
  }

  // Mark entries as completed once they've returned
  //处理指令完成后的一些事项
  when (io.completed.fire) {
    val type_width = log2Up(res_max_per_type)
    val queue_type = io.completed.bits(type_width + 1, type_width) //（5，4）
    val issue_id = io.completed.bits(type_width - 1, 0) //（3，0）

    when (queue_type === ldq) {
      entries.foreach(_.bits.deps_ld(issue_id) := false.B)
      entries_ld(issue_id).valid := false.B

      conv_ld_completed := entries_ld(issue_id).bits.cmd.from_conv_fsm
      matmul_ld_completed := entries_ld(issue_id).bits.cmd.from_matmul_fsm

      assert(entries_ld(issue_id).valid)
    }.elsewhen (queue_type === exq) {
      entries.foreach(_.bits.deps_ex(issue_id) := false.B)
      entries_ex(issue_id).valid := false.B

      conv_ex_completed := entries_ex(issue_id).bits.cmd.from_conv_fsm
      matmul_ex_completed := entries_ex(issue_id).bits.cmd.from_matmul_fsm
      
      assert(entries_ex(issue_id).valid)
    }.elsewhen (queue_type === stq) {
      entries.foreach(_.bits.deps_st(issue_id) := false.B)
      entries_st(issue_id).valid := false.B

      conv_st_completed := entries_st(issue_id).bits.cmd.from_conv_fsm
      matmul_st_completed := entries_st(issue_id).bits.cmd.from_matmul_fsm
      
      assert(entries_st(issue_id).valid)
    }.elsewhen (queue_type === add_testq) { //2024.11.10修改
      entries.foreach(_.bits.deps_add_test(issue_id) := false.B)
      entries_add_test(issue_id).valid := false.B

      assert(entries_add_test(issue_id).valid)
    }.otherwise {
      //2024.11.10修改，原来是不等于3
      assert(queue_type =/= 4.U) 
    }
  }

  // Explicitly mark "opb" in all ld/st queues entries as being invalid.
  // This helps us to reduce the total reservation table area
  //这段代码的目的是显式地将加载队列（entries_ld）和存储队列（entries_st）中的所有条目
  //的 opb 字段标记为无效。这有助于减少保留站表的总面积，因为 opb 字段在这些队列中不是需要的。
  Seq(entries_ld, entries_st).foreach { entries_type =>
    entries_type.foreach { e =>
      e.bits.opb.valid := false.B
      e.bits.opb.bits := DontCare
    }
  }

  // val utilization = PopCount(entries.map(e => e.valid))
  val utilization_ld_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === ldq))//统计加载队列（ldq）中尚未发出的有效条目数量
  val utilization_st_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === stq))//统计存储队列（stq）中尚未发出的有效条目数量
  val utilization_ex_q_unissued = PopCount(entries.map(e => e.valid && !e.bits.issued && e.bits.q === exq))//统计执行队列（exq）中尚未发出的有效条目数量
  val utilization_ld_q = PopCount(entries_ld.map(e => e.valid))//统计加载队列（entries_ld）中所有有效条目的数量
  val utilization_st_q = PopCount(entries_st.map(e => e.valid))//统计存储队列（entries_st）中所有有效条目的数量
  val utilization_ex_q = PopCount(entries_ex.map(e => e.valid))//统计执行队列（entries_ex）中所有有效条目的数量

  val valids = VecInit(entries.map(_.valid))                  //创建一个向量，包含所有条目的有效标志
  val functs = VecInit(entries.map(_.bits.cmd.cmd.inst.funct))//创建一个向量，包含所有条目的指令功能码
  val issueds = VecInit(entries.map(_.bits.issued))           //创建一个向量，包含所有条目的发出标志
  val packed_deps = VecInit(entries.map(e =>                  //创建一个向量，包含所有条目的依赖关系
    Cat(Cat(e.bits.deps_ld.reverse), Cat(e.bits.deps_ex.reverse), Cat(e.bits.deps_st.reverse))))

  dontTouch(valids)
  dontTouch(functs)
  dontTouch(issueds)
  dontTouch(packed_deps)

  val pop_count_packed_deps = VecInit(entries.map(e => Mux(e.valid,
    PopCount(e.bits.deps_ld) + PopCount(e.bits.deps_ex) + PopCount(e.bits.deps_st), 0.U)))
  val min_pop_count = pop_count_packed_deps.reduce((acc, d) => minOf(acc, d))
  // assert(min_pop_count < 2.U)
  dontTouch(pop_count_packed_deps)
  dontTouch(min_pop_count)

  val cycles_since_issue = RegInit(0.U(16.W))//记录自上次指令发出以来经过了多少个周期

  when (io.issue.ld.fire() || io.issue.st.fire() || io.issue.ex.fire() || !io.busy || io.completed.fire) {
    cycles_since_issue := 0.U
  }.elsewhen(io.busy) {
    cycles_since_issue := cycles_since_issue + 1.U
  }
  assert(cycles_since_issue < PlusArg("gemmini_timeout", 10000), "pipeline stall")

  for (e <- entries) {
    dontTouch(e.bits.allocated_at)
  }

  val cntr = Counter(2000000)
  when (cntr.inc()) {
    printf(p"Utilization: $utilization\n")
    printf(p"Utilization ld q (incomplete): $utilization_ld_q_unissued\n")
    printf(p"Utilization st q (incomplete): $utilization_st_q_unissued\n")
    printf(p"Utilization ex q (incomplete): $utilization_ex_q_unissued\n")
    printf(p"Utilization ld q: $utilization_ld_q\n")
    printf(p"Utilization st q: $utilization_st_q\n")
    printf(p"Utilization ex q: $utilization_ex_q\n")

    if (use_firesim_simulation_counters) {
      printf(SynthesizePrintf("Utilization: %d\n", utilization))
      printf(SynthesizePrintf("Utilization ld q (incomplete): %d\n", utilization_ld_q_unissued))
      printf(SynthesizePrintf("Utilization st q (incomplete): %d\n", utilization_st_q_unissued))
      printf(SynthesizePrintf("Utilization ex q (incomplete): %d\n", utilization_ex_q_unissued))
      printf(SynthesizePrintf("Utilization ld q: %d\n", utilization_ld_q))
      printf(SynthesizePrintf("Utilization st q: %d\n", utilization_st_q))
      printf(SynthesizePrintf("Utilization ex q: %d\n", utilization_ex_q))
    }

    printf(p"Packed deps: $packed_deps\n")
  }

  if (use_firesim_simulation_counters) {
    PerfCounter(io.busy, "reservation_station_busy", "cycles where reservation station has entries")
    PerfCounter(!io.alloc.ready, "reservation_station_full", "cycles where reservation station is full")
  }

  when (reset.asBool) {
    entries.foreach(_.valid := false.B)
  }

  CounterEventIO.init(io.counter)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_LD_COUNT, utilization_ld_q)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_ST_COUNT, utilization_st_q)
  io.counter.connectExternalCounter(CounterExternal.RESERVATION_STATION_EX_COUNT, utilization_ex_q)
  io.counter.connectEventSignal(CounterEvent.RESERVATION_STATION_ACTIVE_CYCLES, io.busy)
  io.counter.connectEventSignal(CounterEvent.RESERVATION_STATION_FULL_CYCLES, !io.alloc.ready)
}
