
package gemmini

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tile._
import freechips.rocketchip.util.ClockGate
import freechips.rocketchip.tilelink.TLIdentityNode
import GemminiISA._
import Util._

class GemminiCmd(rob_entries: Int)(implicit p: Parameters) extends Bundle {
  val cmd = new RoCCCommand
  val rob_id = UDValid(UInt(log2Up(rob_entries).W))
  val from_matmul_fsm = Bool()//该命令是否来自矩阵乘法状态机
  val from_conv_fsm = Bool()  //该命令是否来自卷积状态机
}

class Gemmini[T <: Data : Arithmetic, U <: Data, V <: Data](val config: GemminiArrayConfig[T, U, V])
                                     (implicit p: Parameters)
  extends LazyRoCC (
    opcodes = config.opcodes,
    nPTWPorts = if (config.use_shared_tlb) 1 else 2) {

  Files.write(Paths.get(config.headerFilePath), config.generateHeader().getBytes(StandardCharsets.UTF_8))
  if (System.getenv("GEMMINI_ONLY_GENERATE_GEMMINI_H") == "1") {
    System.exit(1)
  }

  val xLen = p(XLen)
  val spad = LazyModule(new Scratchpad(config))

  override lazy val module = new GemminiModule(this)
  override val tlNode = if (config.use_dedicated_tl_port) spad.id_node else TLIdentityNode()
  override val atlNode = if (config.use_dedicated_tl_port) TLIdentityNode() else spad.id_node

  val node = if (config.use_dedicated_tl_port) tlNode else atlNode
}

class GemminiModule[T <: Data: Arithmetic, U <: Data, V <: Data]
    (outer: Gemmini[T, U, V])
    extends LazyRoCCModuleImp(outer)
    with HasCoreParameters {

  import outer.config._
  import outer.spad

  val ext_mem_io = if (use_shared_ext_mem) Some(IO(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks))) else None
  ext_mem_io.foreach(_ <> outer.spad.module.io.ext_mem.get)//在共享外部内存时，将ext_mem_io 的输入和输出与 outer.spad.module.io.ext_mem.get 连接起来

  val tagWidth = 32

  // Counters
  val counters = Module(new CounterController(outer.config.num_counter, outer.xLen))
  io.resp <> counters.io.out  // Counter access command will be committed immediately
  counters.io.event_io.external_values(0) := 0.U
  counters.io.event_io.event_signal(0) := false.B
  counters.io.in.valid := false.B
  counters.io.in.bits := DontCare
  counters.io.event_io.collect(spad.module.io.counter)

  // TLB
  implicit val edge = outer.spad.id_node.edges.out.head
  val tlb = Module(new FrontendTLB(2, tlb_size, dma_maxbytes, use_tlb_register_filter, use_firesim_simulation_counters, use_shared_tlb))
  (tlb.io.clients zip outer.spad.module.io.tlb).foreach(t => t._1 <> t._2)
  //通过 zip 和 foreach，将 FrontendTLB 的 clients 接口与外部模块的 TLB 接口一一对应连接起来。这样，FrontendTLB 就可以处理来自外部模块
  //的 TLB 请求，并返回响应

  tlb.io.exp.foreach(_.flush_skip := false.B) 
  tlb.io.exp.foreach(_.flush_retry := false.B)

  io.ptw <> tlb.io.ptw

  counters.io.event_io.collect(tlb.io.counter)//调用 collect 方法，将 FrontendTLB 的计数器事件汇总到计数器接口中

  spad.module.io.flush := tlb.io.exp.map(_.flush()).reduce(_ || _)//任何一个 TLB 异常接口的 flush 信号为 true，都会触发 spad.module.io.flush

  val clock_en_reg = RegInit(true.B)
  val gated_clock = if (clock_gate) ClockGate(clock, clock_en_reg, "gemmini_clock_gate") else clock
  outer.spad.module.clock := gated_clock

  /*
  //=========================================================================
  // Frontends: Incoming commands and ROB
  //=========================================================================

  // forward cmd to correct frontend. if the rob is busy, do not forward new
  // commands to tiler, and vice versa
  val is_cisc_mode = RegInit(false.B)

  val raw_cmd = Queue(io.cmd)
  val funct = raw_cmd.bits.inst.funct

  val is_cisc_funct = (funct === CISC_CONFIG) ||
                      (funct === ADDR_AB) ||
                      (funct === ADDR_CD) ||
                      (funct === SIZE_MN) ||
                      (funct === SIZE_K) ||
                      (funct === RPT_BIAS) ||
                      (funct === RESET) ||
                      (funct === COMPUTE_CISC)

  val raw_cisc_cmd = WireInit(raw_cmd)
  val raw_risc_cmd = WireInit(raw_cmd)
  raw_cisc_cmd.valid := false.B
  raw_risc_cmd.valid := false.B
  raw_cmd.ready := false.B

  //-------------------------------------------------------------------------
  // cisc
  val cmd_fsm = CmdFSM(outer.config)
  cmd_fsm.io.cmd <> raw_cisc_cmd
  val tiler = TilerController(outer.config)
  tiler.io.cmd_in <> cmd_fsm.io.tiler

  //-------------------------------------------------------------------------
  // risc
  val unrolled_cmd = LoopUnroller(raw_risc_cmd, outer.config.meshRows * outer.config.tileRows)
  */

  val reservation_station = withClock (gated_clock) { Module(new ReservationStation(outer.config, new GemminiCmd(reservation_station_entries))) }
  counters.io.event_io.collect(reservation_station.io.counter)

  //时钟门控设置
  when (io.cmd.valid && io.cmd.bits.inst.funct === CLKGATE_EN && !io.busy) {
    clock_en_reg := io.cmd.bits.rs1(0)
  }


  val raw_cmd_q = Module(new Queue(new GemminiCmd(reservation_station_entries), entries = 2))
  raw_cmd_q.io.enq.valid := io.cmd.valid//将输入命令的有效信号连接到队列的入队有效信号
  io.cmd.ready := raw_cmd_q.io.enq.ready//将队列的入队就绪信号连接到输入命令的就绪信号
  raw_cmd_q.io.enq.bits.cmd := io.cmd.bits//将输入命令的内容传递到队列的入队数据端口
  raw_cmd_q.io.enq.bits.rob_id := DontCare//将 rob_id 设置为无关状态，因为在这个上下文中不重要
  raw_cmd_q.io.enq.bits.from_conv_fsm := false.B  //表示这个命令不是来自卷积状态机
  raw_cmd_q.io.enq.bits.from_matmul_fsm := false.B//表示这个命令不是来自矩阵乘法状态机

  val raw_cmd = raw_cmd_q.io.deq//将出队接口连接到raw_cmd，这时还没有出队?

  val max_lds = reservation_station_entries_ld//定义加载队列中的最大条目数
  val max_exs = reservation_station_entries_ex//定义执行队列中的最大条目数
  val max_sts = reservation_station_entries_st//定义存储队列中的最大条目数

  val (conv_cmd, loop_conv_unroller_busy) = withClock (gated_clock) { LoopConv(raw_cmd, reservation_station.io.conv_ld_completed, reservation_station.io.conv_st_completed, reservation_station.io.conv_ex_completed,
    meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes,
    new ConfigMvinRs1(mvin_scale_t_bits, block_stride_bits, pixel_repeats_bits), new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new ConfigMvoutRs2(acc_scale_t_bits, 32), new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ConfigExRs1(acc_scale_t_bits), new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    has_training_convs, has_max_pool, has_first_layer_optimizations, has_dw_convs) }

  val (loop_cmd, loop_matmul_unroller_busy) = withClock (gated_clock) { LoopMatmul(conv_cmd, reservation_station.io.matmul_ld_completed, reservation_station.io.matmul_st_completed, reservation_station.io.matmul_ex_completed,
    meshRows*tileRows, coreMaxAddrBits, reservation_station_entries, max_lds, max_exs, max_sts, sp_banks * sp_bank_entries, acc_banks * acc_bank_entries,
    inputType.getWidth, accType.getWidth, dma_maxbytes, new MvinRs2(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new PreloadRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new PreloadRs(mvout_rows_bits, mvout_cols_bits, local_addr_t),
    new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t), new ComputeRs(mvin_rows_bits, mvin_cols_bits, local_addr_t),
    new MvoutRs2(mvout_rows_bits, mvout_cols_bits, local_addr_t)) }

  val unrolled_cmd = Queue(loop_cmd)
  unrolled_cmd.ready := false.B
  counters.io.event_io.connectEventSignal(CounterEvent.LOOP_MATMUL_ACTIVE_CYCLES, loop_matmul_unroller_busy)

  // Wire up controllers to ROB
  reservation_station.io.alloc.valid := false.B
  reservation_station.io.alloc.bits := unrolled_cmd.bits
  // 测量延迟周期数
/*   val latency_Reg_mvin = RegInit(0.U(50.W))
  dontTouch(latency_Reg_mvin)
  when((unrolled_cmd.bits.cmd.inst.funct === 1.U || unrolled_cmd.bits.cmd.inst.funct === 2.U) && unrolled_cmd.valid){
    latency_Reg_mvin := latency_Reg_mvin + 1.U
  }
  val latency_Reg_mvout = RegInit(0.U(50.W))
  dontTouch(latency_Reg_mvout)
  when((unrolled_cmd.bits.cmd.inst.funct === 3.U) && unrolled_cmd.valid){
    latency_Reg_mvout := latency_Reg_mvout + 1.U
  }
  val latency_Reg_compute = RegInit(0.U(50.W))
  dontTouch(latency_Reg_compute)
  when((unrolled_cmd.bits.cmd.inst.funct === 4.U || unrolled_cmd.bits.cmd.inst.funct === 5.U || unrolled_cmd.bits.cmd.inst.funct === 6.U) && unrolled_cmd.valid){
    latency_Reg_compute := latency_Reg_compute + 1.U
  }
  when((unrolled_cmd.bits.cmd.inst.funct === 23.U) && unrolled_cmd.valid){
    printf(p"frequency of mvin = $latency_Reg_mvin\n")
    printf(p"frequency of mvout = $latency_Reg_mvout\n")
    printf(p"frequency of compute = $latency_Reg_compute\n")
  } */
  /* val latency_Reg_nums = RegInit(0.U(50.W))
  dontTouch(latency_Reg_nums)
  when(unrolled_cmd.bits.cmd.inst.funct === 23.U && unrolled_cmd.valid){
    latency_Reg_nums := latency_Reg_nums + 1.U
  }
  when(unrolled_cmd.bits.cmd.inst.funct === 24.U && unrolled_cmd.valid){
    printf(p"nums = $latency_Reg_nums\n")
  } */



  /*
  //-------------------------------------------------------------------------
  // finish muxing control signals to rob (risc) or tiler (cisc)
  when (raw_cmd.valid && is_cisc_funct && !rob.io.busy) {
    is_cisc_mode       := true.B
    raw_cisc_cmd.valid := true.B
    raw_cmd.ready      := raw_cisc_cmd.ready
  }
  .elsewhen (raw_cmd.valid && !is_cisc_funct && !tiler.io.busy) {
    is_cisc_mode       := false.B
    raw_risc_cmd.valid := true.B
    raw_cmd.ready      := raw_risc_cmd.ready
  }
  */

  //=========================================================================
  // Controllers
  //=========================================================================
  val load_controller = withClock (gated_clock) { Module(new LoadController(outer.config, coreMaxAddrBits, local_addr_t)) }
  val store_controller = withClock (gated_clock) { Module(new StoreController(outer.config, coreMaxAddrBits, local_addr_t)) }
  val ex_controller = withClock (gated_clock) { Module(new ExecuteController(xLen, tagWidth, outer.config)) }
  val add_test = withClock (gated_clock) { Module(new Add_test(outer.config, coreMaxAddrBits, local_addr_t)) } //2024.11.11修改

  counters.io.event_io.collect(load_controller.io.counter)
  counters.io.event_io.collect(store_controller.io.counter)
  counters.io.event_io.collect(ex_controller.io.counter)

  /*
  tiler.io.issue.load.ready := false.B
  tiler.io.issue.store.ready := false.B
  tiler.io.issue.exec.ready := false.B
  */

  reservation_station.io.issue.ld.ready := false.B
  reservation_station.io.issue.st.ready := false.B
  reservation_station.io.issue.ex.ready := false.B
  reservation_station.io.issue.add_test.ready := false.B //2024.11.11修改

  /*
  when (is_cisc_mode) {
    load_controller.io.cmd  <> tiler.io.issue.load
    store_controller.io.cmd <> tiler.io.issue.store
    ex_controller.io.cmd  <> tiler.io.issue.exec
  }
  .otherwise {
    load_controller.io.cmd.valid := rob.io.issue.ld.valid
    rob.io.issue.ld.ready := load_controller.io.cmd.ready
    load_controller.io.cmd.bits.cmd := rob.io.issue.ld.cmd
    load_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.ld.cmd.inst.funct
    load_controller.io.cmd.bits.rob_id.push(rob.io.issue.ld.rob_id)

    store_controller.io.cmd.valid := rob.io.issue.st.valid
    rob.io.issue.st.ready := store_controller.io.cmd.ready
    store_controller.io.cmd.bits.cmd := rob.io.issue.st.cmd
    store_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.st.cmd.inst.funct
    store_controller.io.cmd.bits.rob_id.push(rob.io.issue.st.rob_id)

    ex_controller.io.cmd.valid := rob.io.issue.ex.valid
    rob.io.issue.ex.ready := ex_controller.io.cmd.ready
    ex_controller.io.cmd.bits.cmd := rob.io.issue.ex.cmd
    ex_controller.io.cmd.bits.cmd.inst.funct := rob.io.issue.ex.cmd.inst.funct
    ex_controller.io.cmd.bits.rob_id.push(rob.io.issue.ex.rob_id)
  }
  */

  //和测试额外加的spad有关的
  for(i <- 0 until sp_banks){
  spad.module.io.srams_test.read(i).resp.ready := true.B
  }
  for(i <- 0 until sp_banks){
  spad.module.io.srams_test.read(i).req.valid := DontCare
  spad.module.io.srams_test.read(i).req.bits := DontCare
  }

  load_controller.io.cmd.valid := reservation_station.io.issue.ld.valid
  reservation_station.io.issue.ld.ready := load_controller.io.cmd.ready
  load_controller.io.cmd.bits := reservation_station.io.issue.ld.cmd
  load_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.ld.rob_id)

  store_controller.io.cmd.valid := reservation_station.io.issue.st.valid
  reservation_station.io.issue.st.ready := store_controller.io.cmd.ready
  store_controller.io.cmd.bits := reservation_station.io.issue.st.cmd
  store_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.st.rob_id)

  ex_controller.io.cmd.valid := reservation_station.io.issue.ex.valid
  reservation_station.io.issue.ex.ready := ex_controller.io.cmd.ready
  ex_controller.io.cmd.bits := reservation_station.io.issue.ex.cmd
  ex_controller.io.cmd.bits.rob_id.push(reservation_station.io.issue.ex.rob_id)

  //2024.11.11修改
  add_test.io.cmd.valid := reservation_station.io.issue.add_test.valid
  reservation_station.io.issue.add_test.ready := add_test.io.cmd.ready
  add_test.io.cmd.bits := reservation_station.io.issue.add_test.cmd
  add_test.io.cmd.bits.rob_id.push(reservation_station.io.issue.add_test.rob_id)

  // Wire up scratchpad to controllers
  spad.module.io.dma.read <> load_controller.io.dma
  spad.module.io.dma.write <> store_controller.io.dma
  for(i <- 0 until 4){
    ex_controller.io.srams.read(i) <> spad.module.io.srams.read(i)
  }
  // ex_controller.io.srams.read <> spad.module.io.srams.read
  ex_controller.io.srams.write <> spad.module.io.srams.write
  spad.module.io.acc.read_req <> ex_controller.io.acc.read_req
  ex_controller.io.acc.read_resp <> spad.module.io.acc.read_resp
  ex_controller.io.acc.write <> spad.module.io.acc.write

  // Im2Col unit
  val im2col = withClock (gated_clock) { Module(new Im2Col(outer.config)) }

  // Wire up Im2col
  counters.io.event_io.collect(im2col.io.counter)
  // im2col.io.sram_reads <> spad.module.io.srams.read
  im2col.io.req <> ex_controller.io.im2col.req
  ex_controller.io.im2col.resp <> im2col.io.resp

  // Wire arbiter for ExecuteController and Im2Col scratchpad reads
  //仲裁器：负责在 ex_controller 和 im2col 的读取请求之间进行仲裁，决定哪个请求可以访问 SPAD
  (ex_controller.io.srams.read, im2col.io.sram_reads, spad.module.io.srams.read).zipped.foreach { case (ex_read, im2col_read, spad_read) =>
    val req_arb = Module(new Arbiter(new ScratchpadReadReq(n=sp_bank_entries), 2))

    req_arb.io.in(0) <> ex_read.req
    req_arb.io.in(1) <> im2col_read.req

    spad_read.req <> req_arb.io.out

    // TODO if necessary, change how the responses are handled when fromIm2Col is added to spad read interface

    ex_read.resp.valid := spad_read.resp.valid
    im2col_read.resp.valid := spad_read.resp.valid

    ex_read.resp.bits := spad_read.resp.bits
    im2col_read.resp.bits := spad_read.resp.bits

    spad_read.resp.ready := ex_read.resp.ready || im2col_read.resp.ready
  }

  // Wire up controllers to ROB
  reservation_station.io.alloc.valid := false.B
  // rob.io.alloc.bits := compressed_cmd.bits
  reservation_station.io.alloc.bits := unrolled_cmd.bits

  /*
  //=========================================================================
  // committed insn return path to frontends
  //=========================================================================

  //-------------------------------------------------------------------------
  // cisc
  tiler.io.completed.exec.valid := ex_controller.io.completed.valid
  tiler.io.completed.exec.bits := ex_controller.io.completed.bits

  tiler.io.completed.load <> load_controller.io.completed
  tiler.io.completed.store <> store_controller.io.completed

  // mux with cisc frontend arbiter
  tiler.io.completed.exec.valid  := ex_controller.io.completed.valid && is_cisc_mode
  tiler.io.completed.load.valid  := load_controller.io.completed.valid && is_cisc_mode
  tiler.io.completed.store.valid := store_controller.io.completed.valid && is_cisc_mode
  */

  //-------------------------------------------------------------------------
  // risc
  val reservation_station_completed_arb = Module(new Arbiter(UInt(log2Up(reservation_station_entries).W), 4)) //2024.11.11修改
  reservation_station_completed_arb.io.in(3) <> add_test.io.completed //2024.11.11修改
  
  reservation_station_completed_arb.io.in(0).valid := ex_controller.io.completed.valid //2024.11.11修改
  reservation_station_completed_arb.io.in(0).bits := ex_controller.io.completed.bits   //2024.11.11修改

  reservation_station_completed_arb.io.in(1) <> load_controller.io.completed  //2024.11.11修改
  reservation_station_completed_arb.io.in(2) <> store_controller.io.completed //2024.11.11修改

  // mux with cisc frontend arbiter
  //reservation_station_completed_arb.io.in(0).valid := ex_controller.io.completed.valid // && !is_cisc_mode；2024.11.11注释
  //reservation_station_completed_arb.io.in(1).valid := load_controller.io.completed.valid // && !is_cisc_mode；2024.11.11注释
  //reservation_station_completed_arb.io.in(2).valid := store_controller.io.completed.valid // && !is_cisc_mode；2024.11.11注释

  reservation_station.io.completed.valid := reservation_station_completed_arb.io.out.valid
  reservation_station.io.completed.bits := reservation_station_completed_arb.io.out.bits
  reservation_station_completed_arb.io.out.ready := true.B

  // Wire up global RoCC signals
  io.busy := raw_cmd.valid || loop_conv_unroller_busy || loop_matmul_unroller_busy || reservation_station.io.busy || spad.module.io.busy || unrolled_cmd.valid || loop_cmd.valid || conv_cmd.valid

  io.interrupt := tlb.io.exp.map(_.interrupt).reduce(_ || _)

  // assert(!io.interrupt, "Interrupt handlers have not been written yet")

  // Cycle counters
  val incr_ld_cycles = load_controller.io.busy && !store_controller.io.busy && !ex_controller.io.busy
  val incr_st_cycles = !load_controller.io.busy && store_controller.io.busy && !ex_controller.io.busy
  val incr_ex_cycles = !load_controller.io.busy && !store_controller.io.busy && ex_controller.io.busy

  val incr_ld_st_cycles = load_controller.io.busy && store_controller.io.busy && !ex_controller.io.busy
  val incr_ld_ex_cycles = load_controller.io.busy && !store_controller.io.busy && ex_controller.io.busy
  val incr_st_ex_cycles = !load_controller.io.busy && store_controller.io.busy && ex_controller.io.busy

  val incr_ld_st_ex_cycles = load_controller.io.busy && store_controller.io.busy && ex_controller.io.busy

  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_CYCLES, incr_ld_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_ST_CYCLES, incr_st_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_EX_CYCLES, incr_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_ST_CYCLES, incr_ld_st_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_EX_CYCLES, incr_ld_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_ST_EX_CYCLES, incr_st_ex_cycles)
  counters.io.event_io.connectEventSignal(CounterEvent.MAIN_LD_ST_EX_CYCLES, incr_ld_st_ex_cycles)

  // Issue commands to controllers
  // TODO we combinationally couple cmd.ready and cmd.valid signals here
  // when (compressed_cmd.valid) {
  when (unrolled_cmd.valid) {
    // val config_cmd_type = cmd.bits.rs1(1,0) // TODO magic numbers

    //val funct = unrolled_cmd.bits.inst.funct
    val risc_funct = unrolled_cmd.bits.cmd.inst.funct

    val is_flush = risc_funct === FLUSH_CMD
    val is_counter_op = risc_funct === COUNTER_OP
    val is_clock_gate_en = risc_funct === CLKGATE_EN

    /*
    val is_load = (funct === LOAD_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_LOAD)
    val is_store = (funct === STORE_CMD) || (funct === CONFIG_CMD && config_cmd_type === CONFIG_STORE)
    val is_ex = (funct === COMPUTE_AND_FLIP_CMD || funct === COMPUTE_AND_STAY_CMD || funct === PRELOAD_CMD) ||
    (funct === CONFIG_CMD && config_cmd_type === CONFIG_EX)
    */

    when (is_flush) {
      val skip = unrolled_cmd.bits.cmd.rs1(0)
      tlb.io.exp.foreach(_.flush_skip := skip)
      tlb.io.exp.foreach(_.flush_retry := !skip)

      unrolled_cmd.ready := true.B // TODO should we wait for an acknowledgement from the TLB?
    }

    .elsewhen (is_counter_op) {
      // If this is a counter access/configuration command, execute immediately
      counters.io.in.valid := unrolled_cmd.valid
      unrolled_cmd.ready := counters.io.in.ready
      counters.io.in.bits := unrolled_cmd.bits.cmd
    }

    .elsewhen (is_clock_gate_en) {
      unrolled_cmd.ready := true.B
    }

    //如果risc_funct不匹配前面的情况，默认将该命令分配给预留站
    .otherwise {
      reservation_station.io.alloc.valid := true.B

      when(reservation_station.io.alloc.fire) {
        // compressed_cmd.ready := true.B
        unrolled_cmd.ready := true.B
      }
    }
  }

  // Debugging signals
  val pipeline_stall_counter = RegInit(0.U(32.W))
  when (io.cmd.fire()) {
    pipeline_stall_counter := 0.U
  }.elsewhen(io.busy) {
    pipeline_stall_counter := pipeline_stall_counter + 1.U
  }
  assert(pipeline_stall_counter < 10000000.U, "pipeline stall")

  /*
  //=========================================================================
  // Wire up global RoCC signals
  //=========================================================================
  io.busy := raw_cmd.valid || unrolled_cmd.valid || rob.io.busy || spad.module.io.busy || tiler.io.busy
  io.interrupt := tlb.io.exp.interrupt

  // hack
  when(is_cisc_mode || !(unrolled_cmd.valid || rob.io.busy || tiler.io.busy)){
    tlb.io.exp.flush_retry := cmd_fsm.io.flush_retry
    tlb.io.exp.flush_skip  := cmd_fsm.io.flush_skip
  }
  */

  //=========================================================================
  // Performance Counters Access
  //=========================================================================

  val LatencySimulation_WriteAccumlator_test = Module(new LatencySimulation_WriteAccumlator(xLen, tagWidth, outer.config))
  LatencySimulation_WriteAccumlator_test.io.in.acc := MuxCase(DontCare, Seq(
                                                       ex_controller.io.acc.write(0).valid -> ex_controller.io.acc.write(0).bits.acc,
                                                       ex_controller.io.acc.write(1).valid -> ex_controller.io.acc.write(1).bits.acc
                                                      ))
  LatencySimulation_WriteAccumlator_test.io.in.addr := MuxCase(DontCare, Seq(
                                                       ex_controller.io.acc.write(0).valid -> ex_controller.io.acc.write(0).bits.addr,
                                                       ex_controller.io.acc.write(1).valid -> ex_controller.io.acc.write(1).bits.addr
                                                      ))
  LatencySimulation_WriteAccumlator_test.io.in.data := MuxCase(DontCare, Seq(
                                                       ex_controller.io.acc.write(0).valid -> ex_controller.io.acc.write(0).bits.data,
                                                       ex_controller.io.acc.write(1).valid -> ex_controller.io.acc.write(1).bits.data
                                                      ))
  LatencySimulation_WriteAccumlator_test.io.in.mask := MuxCase(DontCare, Seq(
                                                       ex_controller.io.acc.write(0).valid -> ex_controller.io.acc.write(0).bits.mask,
                                                       ex_controller.io.acc.write(1).valid -> ex_controller.io.acc.write(1).bits.mask
                                                      ))
  LatencySimulation_WriteAccumlator_test.io.addr_banks_in := MuxCase(DontCare, Seq(
                                                              ex_controller.io.acc.write(0).valid -> 0.U,
                                                              ex_controller.io.acc.write(1).valid -> 1.U
                                                             ))
  LatencySimulation_WriteAccumlator_test.io.valid_in := ex_controller.io.acc.write(0).valid ||
                                                        ex_controller.io.acc.write(1).valid
  LatencySimulation_WriteAccumlator_test.io.Verification_completed := ex_controller.io.Verification_completed
  
/*   val LatencySimulation_ReadSpad_test_0 = Module(new LatencySimulation_ReadSpad(inputType.getWidth * meshRows * tileRows , meshRows * tileRows))
  LatencySimulation_ReadSpad_test_0.io.in.data := spad.module.io.srams.read(0).resp.bits.data
  LatencySimulation_ReadSpad_test_0.io.in.fromDMA := spad.module.io.srams.read(0).resp.bits.fromDMA
  LatencySimulation_ReadSpad_test_0.io.valid_in := spad.module.io.srams.read(0).resp.valid
  LatencySimulation_ReadSpad_test_0.io.Verification_completed := Mux(spad.module.io.addr_banks === 0.U, spad.module.io.Verification_completed, false.B)
  val LatencySimulation_ReadSpad_test_1 = Module(new LatencySimulation_ReadSpad(inputType.getWidth * meshRows * tileRows , meshRows * tileRows))
  LatencySimulation_ReadSpad_test_1.io.in.data := spad.module.io.srams.read(1).resp.bits.data
  LatencySimulation_ReadSpad_test_1.io.in.fromDMA := spad.module.io.srams.read(1).resp.bits.fromDMA
  LatencySimulation_ReadSpad_test_1.io.valid_in := spad.module.io.srams.read(1).resp.valid
  LatencySimulation_ReadSpad_test_1.io.Verification_completed := Mux(spad.module.io.addr_banks === 1.U, spad.module.io.Verification_completed, false.B)
  val LatencySimulation_ReadSpad_test_2 = Module(new LatencySimulation_ReadSpad(inputType.getWidth * meshRows * tileRows , meshRows * tileRows))
  LatencySimulation_ReadSpad_test_2.io.in.data := spad.module.io.srams.read(2).resp.bits.data
  LatencySimulation_ReadSpad_test_2.io.in.fromDMA := spad.module.io.srams.read(2).resp.bits.fromDMA
  LatencySimulation_ReadSpad_test_2.io.valid_in := spad.module.io.srams.read(2).resp.valid
  LatencySimulation_ReadSpad_test_2.io.Verification_completed := Mux(spad.module.io.addr_banks === 2.U, spad.module.io.Verification_completed, false.B)
  val LatencySimulation_ReadSpad_test_3 = Module(new LatencySimulation_ReadSpad(inputType.getWidth * meshRows * tileRows , meshRows * tileRows))
  LatencySimulation_ReadSpad_test_3.io.in.data := spad.module.io.srams.read(3).resp.bits.data
  LatencySimulation_ReadSpad_test_3.io.in.fromDMA := spad.module.io.srams.read(3).resp.bits.fromDMA
  LatencySimulation_ReadSpad_test_3.io.valid_in := spad.module.io.srams.read(3).resp.valid
  LatencySimulation_ReadSpad_test_3.io.Verification_completed := Mux(spad.module.io.addr_banks === 3.U, spad.module.io.Verification_completed, false.B)

  ex_controller.io.srams.read(0).req <> spad.module.io.srams.read(0).req
  ex_controller.io.srams.read(0).resp.bits.data := LatencySimulation_ReadSpad_test_0.io.out.data
  ex_controller.io.srams.read(0).resp.bits.fromDMA := LatencySimulation_ReadSpad_test_0.io.out.fromDMA
  ex_controller.io.srams.read(0).resp.valid := LatencySimulation_ReadSpad_test_0.io.valid_out
  spad.module.io.srams.read(0).resp.ready := ex_controller.io.srams.read(0).resp.ready */
  
  // 批量生成 4 个 LatencySimulation_ReadSpad 模块并连接
  /* for (i <- 0 until 1) {
    // 1. 创建模块实例
    val latencyModule = Module(new LatencySimulation_ReadSpad(
      inputType.getWidth * meshRows * tileRows,
      meshRows * tileRows
    ))

    // 2. 连接输入信号（spad -> latencyModule）
    latencyModule.io.in.data         := spad.module.io.srams.read(i).resp.bits.data
    latencyModule.io.in.fromDMA      := spad.module.io.srams.read(i).resp.bits.fromDMA
    latencyModule.io.valid_in        := spad.module.io.srams.read(i).resp.valid
    latencyModule.io.Verification_completed := Mux(
      spad.module.io.addr_banks === i.U,
      spad.module.io.Verification_completed,
      false.B
    )

    // 3. 连接输出信号（latencyModule -> ex_controller）
    ex_controller.io.srams.read(i).req               <> spad.module.io.srams.read(i).req
    ex_controller.io.srams.read(i).resp.bits.data    := latencyModule.io.out.data
    ex_controller.io.srams.read(i).resp.bits.fromDMA := latencyModule.io.out.fromDMA
    ex_controller.io.srams.read(i).resp.valid        := latencyModule.io.valid_out
    spad.module.io.srams.read(i).resp.ready          := ex_controller.io.srams.read(i).resp.ready
  } */

  ex_controller.io.Verification_completed_in := spad.module.io.Verification_completed
  ex_controller.io.addr_banks := spad.module.io.addr_banks

  spad.module.io.checksum := ex_controller.io.checksum
  spad.module.io.checksum_addr := ex_controller.io.checksum_addr
  spad.module.io.checksum_valid := ex_controller.io.checksum_valid
  spad.module.io.a_rows := ex_controller.io.a_rows
  spad.module.io.checksum_addr_banks := ex_controller.io.checksum_addr_banks

  spad.module.io.data_foracc_mem_test := LatencySimulation_WriteAccumlator_test.io.out
  spad.module.io.valid_foracc_mem_test := LatencySimulation_WriteAccumlator_test.io.valid_out
  spad.module.io.addr_banks_foracc_mem_test := LatencySimulation_WriteAccumlator_test.io.addr_banks_out

  val latency_Reg_nums = RegInit(0.U(50.W))
  dontTouch(latency_Reg_nums)
  when((io.cmd.bits.inst.funct === 23.U || io.cmd.bits.inst.funct === 8.U) && io.cmd.fire){
    latency_Reg_nums := latency_Reg_nums + 1.U
  }
  val latency_Reg_compute = RegInit(0.U(50.W))
  dontTouch(latency_Reg_compute)
  when((io.cmd.bits.inst.funct === 4.U || io.cmd.bits.inst.funct === 5.U || io.cmd.bits.inst.funct === 6.U) && io.cmd.fire){
    latency_Reg_compute := latency_Reg_compute + 1.U
  }
  val latency_Reg_compute_1 = RegInit(0.U(50.W))
  dontTouch(latency_Reg_compute)
  when(ex_controller.io.cmd.fire && (ex_controller.io.cmd.bits.cmd.inst.funct === COMPUTE_AND_STAY_CMD || ex_controller.io.cmd.bits.cmd.inst.funct === COMPUTE_AND_FLIP_CMD)){
    latency_Reg_compute_1 := latency_Reg_compute_1 + 1.U
  }
  when(io.cmd.bits.inst.funct === 24.U && io.cmd.fire){
    printf(p"nums = $latency_Reg_nums\n")
    printf(p"frequency of compute = $latency_Reg_compute\n")
    printf(p"frequency of compute_1 = $latency_Reg_compute_1\n")
  }

  val Adder = Module(new Adder(tileColumns, inputType))
  Adder.io.dataIn := DontCare
  val AdderTree = Module(new AdderTree(tileColumns, inputType))
  AdderTree.io.dataIn := DontCare

}
