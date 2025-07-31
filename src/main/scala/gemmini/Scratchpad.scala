
package gemmini

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket._
import freechips.rocketchip.tile._
import freechips.rocketchip.tilelink._

import Util._

class ScratchpadMemReadRequest[U <: Data](local_addr_t: LocalAddr, scale_t_bits: Int)(implicit p: Parameters) extends CoreBundle {
  //local_addr_t表示本地地址的类型
  //scale_t_bits表示缩放因子的位宽
  val vaddr = UInt(coreMaxAddrBits.W)//虚拟地址
  val laddr = local_addr_t.cloneType //本地地址

  val cols = UInt(16.W) // TODO don't use a magic number for the width here
  val repeats = UInt(16.W) // TODO don't use a magic number for the width here
  val scale = UInt(scale_t_bits.W)//缩放因子
  val has_acc_bitwidth = Bool()//是否具有累加位宽
  val all_zeros = Bool()
  val block_stride = UInt(16.W) // TODO magic numbers；块步长，用于在读取数据时确定步长
  val pixel_repeats = UInt(8.W) // TODO magic numbers；像素重复次数？
  val cmd_id = UInt(8.W) // TODO don't use a magic number here；命令ID
  val status = new MStatus//当前状态

}

class ScratchpadMemWriteRequest(local_addr_t: LocalAddr, acc_t_bits: Int, scale_t_bits: Int)//acc_t_bits表示累加器数据位宽
                              (implicit p: Parameters) extends CoreBundle {
  val vaddr = UInt(coreMaxAddrBits.W)
  val laddr = local_addr_t.cloneType

  val acc_act = UInt(Activation.bitwidth.W) // TODO don't use a magic number for the width here；累加器激活类型
  val acc_scale = UInt(scale_t_bits.W)
  val acc_igelu_qb = UInt(acc_t_bits.W)//表示 I-GELU 算法中的参数
  val acc_igelu_qc = UInt(acc_t_bits.W)//表示 I-GELU 算法中的参数
  val acc_iexp_qln2 = UInt(acc_t_bits.W)//表示指数运算中的参数
  val acc_iexp_qln2_inv = UInt(acc_t_bits.W)//表示逆指数运算中的参数
  val acc_norm_stats_id = UInt(8.W) // TODO magic number；累加器的归一化统计量 ID

  val len = UInt(16.W) // TODO don't use a magic number for the width here；数据长度
  val block = UInt(8.W) // TODO don't use a magic number for the width here；数据块大小

  val cmd_id = UInt(8.W) // TODO don't use a magic number here
  val status = new MStatus

  // Pooling variables
  val pool_en = Bool()//是否池化
  val store_en = Bool()//是否存储

}

class ScratchpadMemWriteResponse extends Bundle {
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadMemReadResponse extends Bundle {
  val bytesRead = UInt(16.W) // TODO magic number here；读取的字节数
  val cmd_id = UInt(8.W) // TODO don't use a magic number here
}

class ScratchpadReadMemIO[U <: Data](local_addr_t: LocalAddr, scale_t_bits: Int)(implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemReadRequest(local_addr_t, scale_t_bits))
  val resp = Flipped(Valid(new ScratchpadMemReadResponse))
}

class ScratchpadWriteMemIO(local_addr_t: LocalAddr, acc_t_bits: Int, scale_t_bits: Int)
                         (implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new ScratchpadMemWriteRequest(local_addr_t, acc_t_bits, scale_t_bits))
  val resp = Flipped(Valid(new ScratchpadMemWriteResponse))
}

class ScratchpadReadReq(val n: Int) extends Bundle {
  val addr = UInt(log2Ceil(n).W)
  val fromDMA = Bool()
}

class ScratchpadReadResp(val w: Int) extends Bundle {
  val data = UInt(w.W)
  val fromDMA = Bool()
}

class ScratchpadReadIO(val n: Int, val w: Int) extends Bundle {
  val req = Decoupled(new ScratchpadReadReq(n))
  val resp = Flipped(Decoupled(new ScratchpadReadResp(w)))
}

class ScratchpadWriteIO(val n: Int, val w: Int, val mask_len: Int) extends Bundle {
  val en = Output(Bool())
  val addr = Output(UInt(log2Ceil(n).W))
  val mask = Output(Vec(mask_len, Bool()))
  val data = Output(UInt(w.W))
}

class ScratchpadBank_Simplify(n: Int, w: Int, aligned_to: Int, single_ported: Boolean, use_shared_ext_mem: Boolean, is_dummy: Boolean) extends Module {
  // This is essentially a pipelined SRAM with the ability to stall pipeline stages
  // n 表示存储单元的数量
  // w 表示存储单元的宽度（位数）
  // aligned_to 表示对齐要求
  // single_ported 表示是否是单端口存储器
  // use_shared_ext_mem 表示是否使用共享外部存储器
  // is_dummy 表示是否是虚拟存储器
  require(w % aligned_to == 0 || w < aligned_to)
  val mask_len = (w / (aligned_to * 8)) max 1 // 16 How many mask bits are there?；掩码长度
  val mask_elem = UInt((w min (aligned_to * 8)).W) // 8 What datatype does each mask bit correspond to?；掩码位宽

  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w))
    val write = Flipped(new ScratchpadWriteIO(n, w, mask_len))
    val ext_mem = if (use_shared_ext_mem) Some(new ExtMemIO) else None
  })

  val (read, write) = if (is_dummy) {     //如果是虚拟存储器，那么读写函数不需要进行实质性的操作
    def read(addr: UInt, ren: Bool): Data = 0.U
    def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]): Unit = { }
    (read _, write _)
  } else if (use_shared_ext_mem) {        //如果是使用外部共享存储器，将接口接到外部
    def read(addr: UInt, ren: Bool): Data = {
      io.ext_mem.get.read_en := ren
      io.ext_mem.get.read_addr := addr
      io.ext_mem.get.read_data
    }
    io.ext_mem.get.write_en := false.B
    io.ext_mem.get.write_addr := DontCare
    io.ext_mem.get.write_data := DontCare
    io.ext_mem.get.write_mask := DontCare
    def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = {
      io.ext_mem.get.write_en := true.B
      io.ext_mem.get.write_addr := addr
      io.ext_mem.get.write_data := wdata.asUInt
      io.ext_mem.get.write_mask := wmask.asUInt
    }
    (read _, write _)
  } else {                                 //如果是访问内部存储器，创建一个同步存储器并对其进行读写操作
    val mem = SyncReadMem(n, Vec(mask_len, mask_elem))
      // 新增初始化逻辑：上电时将内存全部清零
      val initAddr = RegInit(0.U(log2Ceil(n).W))
      val initialized = RegInit(false.B)
      when (!initialized) {
        // 生成全0数据和全掩码
        val zeroData = VecInit(Seq.fill(mask_len)(0.U((w min (aligned_to * 8)).W)))
        val fullMask = VecInit(Seq.fill(mask_len)(true.B))
        mem.write(initAddr, zeroData, fullMask)
        initAddr := initAddr + 1.U
        // 当遍历完所有地址后标记初始化完成
        when (initAddr === (n-1).U) { initialized := true.B }
      }
    def read(addr: UInt, ren: Bool): Data = mem.read(addr, ren)
    def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = mem.write(addr, wdata, wmask)
    (read _, write _)
  }

  // When the scratchpad is single-ported, the writes take precedence
  val singleport_busy_with_write = single_ported.B && io.write.en//在单端口模式下，写入操作是否正在进行

  when (io.write.en) {
    if (aligned_to >= w)//根据掩码全为1写入
      write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), VecInit((~(0.U(mask_len.W))).asBools))
    else                //根据输入的掩码写入
      write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), io.write.mask)
  }

  val raddr = io.read.req.bits.addr
  val ren = io.read.req.fire
  val rdata = if (single_ported) {
    assert(!(ren && io.write.en))
    read(raddr, ren && !io.write.en).asUInt
  } else {
    read(raddr, ren).asUInt
  }

  val fromDMA = io.read.req.bits.fromDMA

  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new ScratchpadReadResp(w), 1, true, true))
  // 1：指定队列的深度为 1，意味着这个队列最多只能存储一个元素。
  // true：第一个 true 参数表示队列是一个先入先出（FIFO）队列。
  // true：第二个 true 参数表示队列支持流（flow）模式。在流模式下，如果队列是空的且有新的数据到达，数据可以直接通过队列而不需要先被存储 
  q.io.enq.valid := RegNext(ren)
  q.io.enq.bits.data := rdata
  q.io.enq.bits.fromDMA := RegNext(fromDMA)

  val q_will_be_empty = (q.io.count +& q.io.enq.fire) - q.io.deq.fire === 0.U//在当前时钟周期之后，队列是否会变为空
  io.read.req.ready := q_will_be_empty && !singleport_busy_with_write

  io.read.resp <> q.io.deq
}

class ScratchpadBank(n: Int, w: Int, aligned_to: Int, single_ported: Boolean, use_shared_ext_mem: Boolean, is_dummy: Boolean) extends Module {
  // This is essentially a pipelined SRAM with the ability to stall pipeline stages
  // n 表示存储单元的数量
  // w 表示存储单元的宽度（位数）
  // aligned_to 表示对齐要求
  // single_ported 表示是否是单端口存储器
  // use_shared_ext_mem 表示是否使用共享外部存储器
  // is_dummy 表示是否是虚拟存储器
  require(w % aligned_to == 0 || w < aligned_to)
  val mask_len = (w / (aligned_to * 8)) max 1 // 16 How many mask bits are there?；掩码长度
  val mask_elem = UInt((w min (aligned_to * 8)).W) // 8 What datatype does each mask bit correspond to?；掩码位宽

  val io = IO(new Bundle {
    val read = Flipped(new ScratchpadReadIO(n, w))
    val write = Flipped(new ScratchpadWriteIO(n, w, mask_len))
    val ext_mem = if (use_shared_ext_mem) Some(new ExtMemIO) else None
  })

  val (read, write) = if (is_dummy) {     //如果是虚拟存储器，那么读写函数不需要进行实质性的操作
    def read(addr: UInt, ren: Bool): Data = 0.U
    def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]): Unit = { }
    (read _, write _)
  } else if (use_shared_ext_mem) {        //如果是使用外部共享存储器，将接口接到外部
    def read(addr: UInt, ren: Bool): Data = {
      io.ext_mem.get.read_en := ren
      io.ext_mem.get.read_addr := addr
      io.ext_mem.get.read_data
    }
    io.ext_mem.get.write_en := false.B
    io.ext_mem.get.write_addr := DontCare
    io.ext_mem.get.write_data := DontCare
    io.ext_mem.get.write_mask := DontCare
    def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = {
      io.ext_mem.get.write_en := true.B
      io.ext_mem.get.write_addr := addr
      io.ext_mem.get.write_data := wdata.asUInt
      io.ext_mem.get.write_mask := wmask.asUInt
    }
    (read _, write _)
  } else {                                 //如果是访问内部存储器，创建一个同步存储器并对其进行读写操作
    val mem = SyncReadMem(n, Vec(mask_len, mask_elem))
    def read(addr: UInt, ren: Bool): Data = mem.read(addr, ren)
    def write(addr: UInt, wdata: Vec[UInt], wmask: Vec[Bool]) = mem.write(addr, wdata, wmask)
    (read _, write _)
  }

  // When the scratchpad is single-ported, the writes take precedence
  val singleport_busy_with_write = single_ported.B && io.write.en//在单端口模式下，写入操作是否正在进行

  when (io.write.en) {
    if (aligned_to >= w)//根据掩码全为1写入
      write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), VecInit((~(0.U(mask_len.W))).asBools))
    else                //根据输入的掩码写入
      write(io.write.addr, io.write.data.asTypeOf(Vec(mask_len, mask_elem)), io.write.mask)
  }

  val raddr = io.read.req.bits.addr
  val ren = io.read.req.fire
  val rdata = if (single_ported) {
    assert(!(ren && io.write.en))
    read(raddr, ren && !io.write.en).asUInt
  } else {
    read(raddr, ren).asUInt
  }

  val fromDMA = io.read.req.bits.fromDMA

  // Make a queue which buffers the result of an SRAM read if it can't immediately be consumed
  val q = Module(new Queue(new ScratchpadReadResp(w), 1, true, true))
  // 1：指定队列的深度为 1，意味着这个队列最多只能存储一个元素。
  // true：第一个 true 参数表示队列是一个先入先出（FIFO）队列。
  // true：第二个 true 参数表示队列支持流（flow）模式。在流模式下，如果队列是空的且有新的数据到达，数据可以直接通过队列而不需要先被存储 
  q.io.enq.valid := RegNext(ren)
  q.io.enq.bits.data := rdata
  q.io.enq.bits.fromDMA := RegNext(fromDMA)

  val q_will_be_empty = (q.io.count +& q.io.enq.fire) - q.io.deq.fire === 0.U//在当前时钟周期之后，队列是否会变为空
  io.read.req.ready := q_will_be_empty && !singleport_busy_with_write

  io.read.resp <> q.io.deq
}


class Scratchpad[T <: Data, U <: Data, V <: Data](config: GemminiArrayConfig[T, U, V])
    (implicit p: Parameters, ev: Arithmetic[T]) extends LazyModule {

  import config._
  import ev._

  val maxBytes = dma_maxbytes//从配置中读取 DMA 的最大字节数
  val dataBits = dma_buswidth//从配置中读取 DMA 总线宽度

  val block_rows = meshRows * tileRows
  val block_cols = meshColumns * tileColumns
  val spad_w = inputType.getWidth *  block_cols//计算片上缓存的宽度，等于 inputType 的宽度乘以 block_cols
  val acc_w = accType.getWidth * block_cols    //计算累加缓存的宽度，等于 accType 的宽度乘以 block_cols

  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new StreamReader(config, max_in_flight_mem_reqs, dataBits, maxBytes, spad_w, acc_w, aligned_to,
    sp_banks * sp_bank_entries, acc_banks * acc_bank_entries, block_rows, use_tlb_register_filter,
    use_firesim_simulation_counters))//实例化一个 StreamReader 模块，用于处理内存读取操作
  val writer = LazyModule(new StreamWriter(max_in_flight_mem_reqs, dataBits, maxBytes,
    if (acc_read_full_width) acc_w else spad_w, aligned_to, inputType, block_cols, use_tlb_register_filter,
    use_firesim_simulation_counters))//实例化一个 StreamWriter 模块，用于处理内存写入操作

  // TODO make a cross-bar vs two separate ports a config option
  // id_node :=* reader.node
  // id_node :=* writer.node

  xbar_node := TLBuffer() := reader.node // TODO;将 reader 模块的节点通过缓冲区 (TLBuffer) 连接到交叉开关节点 (xbar_node)
  xbar_node := TLBuffer() := writer.node//将 writer 模块的节点通过缓冲区 (TLBuffer) 连接到交叉开关节点 (xbar_node)
  id_node := TLWidthWidget(config.dma_buswidth/8) := TLBuffer() := xbar_node//将 xbar_node 通过宽度调整器 (TLWidthWidget) 和缓冲区连接到 id_node

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      val checksum = Input(Vec(meshColumns * tileColumns, accType))
      val checksum_addr = Input(UInt(log2Ceil(acc_bank_entries).W))
      val checksum_addr_banks = Input(UInt(log2Ceil(acc_banks).W))
      val checksum_valid = Input(Bool())
      val a_rows = Input(UInt(log2Ceil(meshRows * tileRows).W))
      val addr_banks = Output(UInt(log2Ceil(sp_banks).W))
      val Verification_completed = Output(Bool())
      val AddressSequenceGenerator_test_outvalid = Output(Bool())
      val data_foracc_mem_test = Input(new AccumulatorWriteReq(acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType))))
      val valid_foracc_mem_test = Input(Bool())
      val addr_banks_foracc_mem_test = Input(UInt(log2Ceil(acc_banks).W))

      // DMA ports
      val dma = new Bundle {
        val read = Flipped(new ScratchpadReadMemIO(local_addr_t, mvin_scale_t_bits))
        val write = Flipped(new ScratchpadWriteMemIO(local_addr_t, accType.getWidth, acc_scale_t_bits))
      }

      // SRAM ports
      val srams = new Bundle {
        val read = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, spad_w)))
        val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, spad_w, (spad_w / (aligned_to * 8)) max 1)))
      }

      //再开一组io口用作新加的spad测试
      val srams_test = new Bundle {
        val read = Flipped(Vec(sp_banks, new ScratchpadReadIO(sp_bank_entries, spad_w)))
        val write = Flipped(Vec(sp_banks, new ScratchpadWriteIO(sp_bank_entries, spad_w, (spad_w / (aligned_to * 8)) max 1)))
      }

      // Accumulator ports
      val acc = new Bundle {
        val read_req = Flipped(Vec(acc_banks, Decoupled(new AccumulatorReadReq(
          acc_bank_entries, accType, acc_scale_t.asInstanceOf[V]
        ))))
        val read_resp = Vec(acc_banks, Decoupled(new AccumulatorScaleResp(
          Vec(meshColumns, Vec(tileColumns, inputType)),
          Vec(meshColumns, Vec(tileColumns, accType))
        )))
        val write = Flipped(Vec(acc_banks, Decoupled(new AccumulatorWriteReq(
          acc_bank_entries, Vec(meshColumns, Vec(tileColumns, accType))
        ))))
      }

      val ext_mem = if (use_shared_ext_mem) {
        Some(new ExtSpadMemIO(sp_banks, acc_banks, acc_sub_banks))
      } else {
        None
      }

      // TLB ports
      val tlb = Vec(2, new FrontendTLBIO)

      // Misc. ports
      val busy = Output(Bool())
      val flush = Input(Bool())
      val counter = new CounterEventIO()
    })

    dontTouch(io.srams_test)

    val write_dispatch_q = Queue(io.dma.write.req)
    // Write norm/scale queues are necessary to maintain in-order requests to accumulator norm/scale units
    // Writes from main SPAD just flow directly between scale_q and issue_q, while writes
    // From acc are ordered
    val write_norm_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits), spad_read_delay+2))
    val write_scale_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits), spad_read_delay+2))
    val write_issue_q = Module(new Queue(new ScratchpadMemWriteRequest(local_addr_t, accType.getWidth, acc_scale_t_bits), spad_read_delay+1, pipe=true))
    val read_issue_q = Module(new Queue(new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits), spad_read_delay+1, pipe=true)) // TODO can't this just be a normal queue?

    write_dispatch_q.ready := false.B

    write_norm_q.io.enq.valid := false.B
    write_norm_q.io.enq.bits := write_dispatch_q.bits
    write_norm_q.io.deq.ready := false.B

    write_scale_q.io.enq.valid := false.B
    write_scale_q.io.enq.bits  := write_norm_q.io.deq.bits
    write_scale_q.io.deq.ready := false.B
 
    write_issue_q.io.enq.valid := false.B
    write_issue_q.io.enq.bits := write_scale_q.io.deq.bits

    // Garbage can immediately fire from dispatch_q -> norm_q
    when (write_dispatch_q.bits.laddr.is_garbage()) {
      write_norm_q.io.enq <> write_dispatch_q
    }

    // Non-acc or garbage can immediately fire between norm_q and scale_q
    when (write_norm_q.io.deq.bits.laddr.is_garbage() || !write_norm_q.io.deq.bits.laddr.is_acc_addr) {
      write_scale_q.io.enq <> write_norm_q.io.deq
    }

    // Non-acc or garbage can immediately fire between scale_q and issue_q
    when (write_scale_q.io.deq.bits.laddr.is_garbage() || !write_scale_q.io.deq.bits.laddr.is_acc_addr) {
      write_issue_q.io.enq <> write_scale_q.io.deq
    }

    val writeData = Wire(Valid(UInt((spad_w max acc_w).W)))
    writeData.valid := write_issue_q.io.deq.bits.laddr.is_garbage()
    writeData.bits := DontCare
    val fullAccWriteData = Wire(UInt(acc_w.W))
    fullAccWriteData := DontCare
    val writeData_is_full_width = !write_issue_q.io.deq.bits.laddr.is_garbage() &&
      write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.read_full_acc_row
    val writeData_is_all_zeros = write_issue_q.io.deq.bits.laddr.is_garbage()

    writer.module.io.req.valid := write_issue_q.io.deq.valid && writeData.valid
    write_issue_q.io.deq.ready := writer.module.io.req.ready && writeData.valid
    writer.module.io.req.bits.vaddr := write_issue_q.io.deq.bits.vaddr
    writer.module.io.req.bits.len := Mux(writeData_is_full_width,
      write_issue_q.io.deq.bits.len * (accType.getWidth / 8).U,
      write_issue_q.io.deq.bits.len * (inputType.getWidth / 8).U)
    writer.module.io.req.bits.data := MuxCase(writeData.bits, Seq(
       writeData_is_all_zeros -> 0.U,
       writeData_is_full_width -> fullAccWriteData
    ))
    writer.module.io.req.bits.block := write_issue_q.io.deq.bits.block
    writer.module.io.req.bits.status := write_issue_q.io.deq.bits.status
    writer.module.io.req.bits.pool_en := write_issue_q.io.deq.bits.pool_en
    writer.module.io.req.bits.store_en := write_issue_q.io.deq.bits.store_en

    io.dma.write.resp.valid := false.B
    io.dma.write.resp.bits.cmd_id := write_dispatch_q.bits.cmd_id
    when (write_dispatch_q.bits.laddr.is_garbage() && write_dispatch_q.fire) {
      io.dma.write.resp.valid := true.B
    }

    read_issue_q.io.enq <> io.dma.read.req

    val zero_writer = Module(new ZeroWriter(config, new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits)))

    when (io.dma.read.req.bits.all_zeros) {
      read_issue_q.io.enq.valid := false.B
      io.dma.read.req.ready := zero_writer.io.req.ready
    }

    zero_writer.io.req.valid := io.dma.read.req.valid && io.dma.read.req.bits.all_zeros
    zero_writer.io.req.bits.laddr := io.dma.read.req.bits.laddr
    zero_writer.io.req.bits.cols := io.dma.read.req.bits.cols
    zero_writer.io.req.bits.block_stride := io.dma.read.req.bits.block_stride
    zero_writer.io.req.bits.tag := io.dma.read.req.bits

    val zero_writer_pixel_repeater = Module(new PixelRepeater(inputType, local_addr_t, block_cols, aligned_to, new ScratchpadMemReadRequest(local_addr_t, mvin_scale_t_bits), passthrough = !has_first_layer_optimizations))
    zero_writer_pixel_repeater.io.req.valid := zero_writer.io.resp.valid
    zero_writer_pixel_repeater.io.req.bits.in := 0.U.asTypeOf(Vec(block_cols, inputType))
    zero_writer_pixel_repeater.io.req.bits.laddr := zero_writer.io.resp.bits.laddr
    zero_writer_pixel_repeater.io.req.bits.len := zero_writer.io.resp.bits.tag.cols
    zero_writer_pixel_repeater.io.req.bits.pixel_repeats := zero_writer.io.resp.bits.tag.pixel_repeats
    zero_writer_pixel_repeater.io.req.bits.last := zero_writer.io.resp.bits.last
    zero_writer_pixel_repeater.io.req.bits.tag := zero_writer.io.resp.bits.tag
    zero_writer_pixel_repeater.io.req.bits.mask := {
      val n = inputType.getWidth / 8
      val mask = zero_writer.io.resp.bits.mask
      val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
      expanded
    }

    zero_writer.io.resp.ready := zero_writer_pixel_repeater.io.req.ready
    zero_writer_pixel_repeater.io.resp.ready := false.B

    reader.module.io.req.valid := read_issue_q.io.deq.valid
    read_issue_q.io.deq.ready := reader.module.io.req.ready
    reader.module.io.req.bits.vaddr := read_issue_q.io.deq.bits.vaddr
    reader.module.io.req.bits.spaddr := Mux(read_issue_q.io.deq.bits.laddr.is_acc_addr,
     read_issue_q.io.deq.bits.laddr.full_acc_addr(), read_issue_q.io.deq.bits.laddr.full_sp_addr())
    reader.module.io.req.bits.len := read_issue_q.io.deq.bits.cols
    reader.module.io.req.bits.repeats := read_issue_q.io.deq.bits.repeats
    reader.module.io.req.bits.pixel_repeats := read_issue_q.io.deq.bits.pixel_repeats
    reader.module.io.req.bits.scale := read_issue_q.io.deq.bits.scale
    reader.module.io.req.bits.is_acc := read_issue_q.io.deq.bits.laddr.is_acc_addr
    reader.module.io.req.bits.accumulate := read_issue_q.io.deq.bits.laddr.accumulate
    reader.module.io.req.bits.has_acc_bitwidth := read_issue_q.io.deq.bits.has_acc_bitwidth
    reader.module.io.req.bits.block_stride := read_issue_q.io.deq.bits.block_stride
    reader.module.io.req.bits.status := read_issue_q.io.deq.bits.status
    reader.module.io.req.bits.cmd_id := read_issue_q.io.deq.bits.cmd_id

    val (mvin_scale_in, mvin_scale_out) = VectorScalarMultiplier(
      config.mvin_scale_args,
      config.inputType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits),
      is_acc = false
    )
    val (mvin_scale_acc_in, mvin_scale_acc_out) = if (mvin_scale_shared) (mvin_scale_in, mvin_scale_out) else (
      VectorScalarMultiplier(
        config.mvin_scale_acc_args,
        config.accType, config.meshColumns * config.tileColumns, chiselTypeOf(reader.module.io.resp.bits),
        is_acc = true
      )
    )

    mvin_scale_in.valid := reader.module.io.resp.valid && (mvin_scale_shared.B || !reader.module.io.resp.bits.is_acc ||
      (reader.module.io.resp.bits.is_acc && !reader.module.io.resp.bits.has_acc_bitwidth))

    mvin_scale_in.bits.in := reader.module.io.resp.bits.data.asTypeOf(chiselTypeOf(mvin_scale_in.bits.in))
    mvin_scale_in.bits.scale := reader.module.io.resp.bits.scale.asTypeOf(mvin_scale_t)
    mvin_scale_in.bits.repeats := reader.module.io.resp.bits.repeats
    mvin_scale_in.bits.pixel_repeats := reader.module.io.resp.bits.pixel_repeats
    mvin_scale_in.bits.last := reader.module.io.resp.bits.last
    mvin_scale_in.bits.tag := reader.module.io.resp.bits

    val mvin_scale_pixel_repeater = Module(new PixelRepeater(inputType, local_addr_t, block_cols, aligned_to, mvin_scale_out.bits.tag.cloneType, passthrough = !has_first_layer_optimizations))
    mvin_scale_pixel_repeater.io.req.valid := mvin_scale_out.valid
    mvin_scale_pixel_repeater.io.req.bits.in := mvin_scale_out.bits.out
    mvin_scale_pixel_repeater.io.req.bits.mask := mvin_scale_out.bits.tag.mask take mvin_scale_pixel_repeater.io.req.bits.mask.size
    mvin_scale_pixel_repeater.io.req.bits.laddr := mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
    mvin_scale_pixel_repeater.io.req.bits.len := mvin_scale_out.bits.tag.len
    mvin_scale_pixel_repeater.io.req.bits.pixel_repeats := mvin_scale_out.bits.tag.pixel_repeats
    mvin_scale_pixel_repeater.io.req.bits.last := mvin_scale_out.bits.last
    mvin_scale_pixel_repeater.io.req.bits.tag := mvin_scale_out.bits.tag

    mvin_scale_out.ready := mvin_scale_pixel_repeater.io.req.ready
    mvin_scale_pixel_repeater.io.resp.ready := false.B

    if (!mvin_scale_shared) {
      mvin_scale_acc_in.valid := reader.module.io.resp.valid &&
        (reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.has_acc_bitwidth)
      mvin_scale_acc_in.bits.in := reader.module.io.resp.bits.data.asTypeOf(chiselTypeOf(mvin_scale_acc_in.bits.in))
      mvin_scale_acc_in.bits.scale := reader.module.io.resp.bits.scale.asTypeOf(mvin_scale_acc_t)
      mvin_scale_acc_in.bits.repeats := reader.module.io.resp.bits.repeats
      mvin_scale_acc_in.bits.pixel_repeats := 1.U
      mvin_scale_acc_in.bits.last := reader.module.io.resp.bits.last
      mvin_scale_acc_in.bits.tag := reader.module.io.resp.bits

      mvin_scale_acc_out.ready := false.B
    }

    reader.module.io.resp.ready := Mux(reader.module.io.resp.bits.is_acc && reader.module.io.resp.bits.has_acc_bitwidth,
      mvin_scale_acc_in.ready, mvin_scale_in.ready)

    val mvin_scale_finished = mvin_scale_pixel_repeater.io.resp.fire && mvin_scale_pixel_repeater.io.resp.bits.last
    val mvin_scale_acc_finished = mvin_scale_acc_out.fire && mvin_scale_acc_out.bits.last
    val zero_writer_finished = zero_writer_pixel_repeater.io.resp.fire && zero_writer_pixel_repeater.io.resp.bits.last

    val zero_writer_bytes_read = Mux(zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr,
      zero_writer_pixel_repeater.io.resp.bits.tag.cols * (accType.getWidth / 8).U,
      zero_writer_pixel_repeater.io.resp.bits.tag.cols * (inputType.getWidth / 8).U)

    // For DMA read responses, mvin_scale gets first priority, then mvin_scale_acc, and then zero_writer
    io.dma.read.resp.valid := mvin_scale_finished || mvin_scale_acc_finished || zero_writer_finished

    // io.dma.read.resp.bits.cmd_id := MuxCase(zero_writer.io.resp.bits.tag.cmd_id, Seq(
    io.dma.read.resp.bits.cmd_id := MuxCase(zero_writer_pixel_repeater.io.resp.bits.tag.cmd_id, Seq(
      // mvin_scale_finished -> mvin_scale_out.bits.tag.cmd_id,
      mvin_scale_finished -> mvin_scale_pixel_repeater.io.resp.bits.tag.cmd_id,
      mvin_scale_acc_finished -> mvin_scale_acc_out.bits.tag.cmd_id))

    io.dma.read.resp.bits.bytesRead := MuxCase(zero_writer_bytes_read, Seq(
      // mvin_scale_finished -> mvin_scale_out.bits.tag.bytes_read,
      mvin_scale_finished -> mvin_scale_pixel_repeater.io.resp.bits.tag.bytes_read,
      mvin_scale_acc_finished -> mvin_scale_acc_out.bits.tag.bytes_read))

    io.tlb(0) <> writer.module.io.tlb
    io.tlb(1) <> reader.module.io.tlb

    writer.module.io.flush := io.flush
    reader.module.io.flush := io.flush

    io.busy := writer.module.io.busy || reader.module.io.busy || write_issue_q.io.deq.valid || write_norm_q.io.deq.valid || write_scale_q.io.deq.valid || write_dispatch_q.valid

    val spad_mems = {
      val banks = Seq.fill(sp_banks) { Module(new ScratchpadBank(
        sp_bank_entries, spad_w,
        aligned_to, config.sp_singleported,
        use_shared_ext_mem, is_dummy
      )) }
      val bank_ios = VecInit(banks.map(_.io))
      // Reading from the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        if (use_shared_ext_mem) {
          io.ext_mem.get.spad(i) <> bio.ext_mem.get
        }

        val ex_read_req = io.srams.read(i).req //原来的
        val exread = ex_read_req.valid //原来的
        //val exread = AddressSequenceGenerator_test.io.outValid //修改过的
 
        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_norm_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_garbage() &&
          !(bio.write.en && config.sp_singleported.B) &&
          !write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.sp_bank() === i.U

        bio.read.req.valid := exread || dmawrite
        ex_read_req.ready := bio.read.req.ready //原来的
        //io.srams.read(i).req.ready := bio.read.req.ready //修改过的

        // The ExecuteController gets priority when reading from SRAMs
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr //原来的
          //bio.read.req.bits.addr := AddressSequenceGenerator_test.io.outAddr //修改过的
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire) {
            write_dispatch_q.ready := true.B
            write_norm_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val dma_read_resp = Wire(Decoupled(new ScratchpadReadResp(spad_w)))
        dma_read_resp.valid := bio.read.resp.valid && bio.read.resp.bits.fromDMA
        dma_read_resp.bits := bio.read.resp.bits
        val ex_read_resp = Wire(Decoupled(new ScratchpadReadResp(spad_w)))
        ex_read_resp.valid := bio.read.resp.valid && !bio.read.resp.bits.fromDMA
        ex_read_resp.bits := bio.read.resp.bits

        val dma_read_pipe = Pipeline(dma_read_resp, spad_read_delay)
        val ex_read_pipe = Pipeline(ex_read_resp, spad_read_delay)

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_read_resp.ready, ex_read_resp.ready)

        dma_read_pipe.ready := writer.module.io.req.ready &&
          !write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.sp_bank() === i.U && // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid
          !write_issue_q.io.deq.bits.laddr.is_garbage()
        when (dma_read_pipe.fire) {
          writeData.valid := true.B
          writeData.bits := dma_read_pipe.bits.data
        }

        io.srams.read(i).resp <> ex_read_pipe
      }

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.srams.write(i).en

        // val laddr = mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
        val laddr = mvin_scale_pixel_repeater.io.resp.bits.laddr

        // val dmaread = mvin_scale_out.valid && !mvin_scale_out.bits.tag.is_acc &&
        val dmaread = mvin_scale_pixel_repeater.io.resp.valid && !mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc &&
          laddr.sp_bank() === i.U

        // We need to make sure that we don't try to return a dma read resp from both zero_writer and either mvin_scale
        // or mvin_acc_scale at the same time. The scalers always get priority in those cases
        /* val zerowrite = zero_writer.io.resp.valid && !zero_writer.io.resp.bits.laddr.is_acc_addr &&
          zero_writer.io.resp.bits.laddr.sp_bank() === i.U && */
        val zerowrite = zero_writer_pixel_repeater.io.resp.valid && !zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr &&
          zero_writer_pixel_repeater.io.resp.bits.laddr.sp_bank() === i.U &&
          // !((mvin_scale_out.valid && mvin_scale_out.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))
          !((mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))

        bio.write.en := exwrite || dmaread || zerowrite
 
        when (exwrite) {
          bio.write.addr := io.srams.write(i).addr
          bio.write.data := io.srams.write(i).data
          bio.write.mask := io.srams.write(i).mask
        }.elsewhen (dmaread) {
          bio.write.addr := laddr.sp_row()
          bio.write.data := mvin_scale_pixel_repeater.io.resp.bits.out.asUInt
          bio.write.mask := mvin_scale_pixel_repeater.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)

          mvin_scale_pixel_repeater.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.elsewhen (zerowrite) {
          bio.write.addr := zero_writer_pixel_repeater.io.resp.bits.laddr.sp_row()
          bio.write.data := 0.U
          bio.write.mask := zero_writer_pixel_repeater.io.resp.bits.mask

          zero_writer_pixel_repeater.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.mask := DontCare
        }
      }
      banks
    }

    val acc_row_t = Vec(meshColumns, Vec(tileColumns, accType))
    val spad_row_t = Vec(meshColumns, Vec(tileColumns, inputType))

    //    val acc_norm_unit = Module(new Normalizer(
    //      max_len = block_cols,
    //      num_reduce_lanes = -1,
    //      num_stats = 4,
    //      latency = 4,
    //      fullDataType = acc_row_t,
    //      scale_t = acc_scale_t,
    //    ))

    val (acc_norm_unit_in, acc_norm_unit_out) = Normalizer(
      is_passthru = !config.has_normalizations,
      max_len = block_cols,
      num_reduce_lanes = -1,
      num_stats = 4,
      latency = 4,
      fullDataType = acc_row_t,
      scale_t = acc_scale_t,
    )

    acc_norm_unit_in.valid := false.B
    acc_norm_unit_in.bits.len := write_norm_q.io.deq.bits.len
    acc_norm_unit_in.bits.stats_id := write_norm_q.io.deq.bits.acc_norm_stats_id
    acc_norm_unit_in.bits.cmd := write_norm_q.io.deq.bits.laddr.norm_cmd
    acc_norm_unit_in.bits.acc_read_resp := DontCare

    val acc_scale_unit = Module(new AccumulatorScale(
      acc_row_t,
      spad_row_t,
      acc_scale_t.asInstanceOf[V],
      acc_read_small_width,
      acc_read_full_width,
      acc_scale_func,
      acc_scale_num_units,
      acc_scale_latency,
      has_nonlinear_activations,
      has_normalizations,
    ))

    val acc_waiting_to_be_scaled = write_scale_q.io.deq.valid &&
      !write_scale_q.io.deq.bits.laddr.is_garbage() &&
      write_scale_q.io.deq.bits.laddr.is_acc_addr &&
      write_issue_q.io.enq.ready

    acc_norm_unit_out.ready := acc_scale_unit.io.in.ready && acc_waiting_to_be_scaled
    acc_scale_unit.io.in.valid := acc_norm_unit_out.valid && acc_waiting_to_be_scaled
    acc_scale_unit.io.in.bits  := acc_norm_unit_out.bits

    when (acc_scale_unit.io.in.fire()) {
      write_issue_q.io.enq <> write_scale_q.io.deq
    }

    acc_scale_unit.io.out.ready := false.B

    val dma_resp_ready =
      writer.module.io.req.ready &&
        write_issue_q.io.deq.bits.laddr.is_acc_addr &&
        !write_issue_q.io.deq.bits.laddr.is_garbage()

    when (acc_scale_unit.io.out.bits.fromDMA && dma_resp_ready) {
      // Send the acc-scale result into the DMA
      acc_scale_unit.io.out.ready := true.B
      writeData.valid := acc_scale_unit.io.out.valid
      writeData.bits  := acc_scale_unit.io.out.bits.data.asUInt
      fullAccWriteData := acc_scale_unit.io.out.bits.full_data.asUInt
    }
    for (i <- 0 until acc_banks) {
      // Send the acc-sccale result to the ExController
      io.acc.read_resp(i).valid := false.B
      io.acc.read_resp(i).bits  := acc_scale_unit.io.out.bits
      when (!acc_scale_unit.io.out.bits.fromDMA && acc_scale_unit.io.out.bits.acc_bank_id === i.U) {
        acc_scale_unit.io.out.ready := io.acc.read_resp(i).ready
        io.acc.read_resp(i).valid := acc_scale_unit.io.out.valid
      }
    }

    val acc_adders = Module(new AccPipeShared(acc_latency-1, acc_row_t, acc_banks))

    val acc_mems = {
      val banks = Seq.fill(acc_banks) { Module(new AccumulatorMem(
        acc_bank_entries, acc_row_t, acc_scale_func, acc_scale_t.asInstanceOf[V],
        acc_singleported, acc_sub_banks,
        use_shared_ext_mem,
        acc_latency, accType, is_dummy
      )) }
      val bank_ios = VecInit(banks.map(_.io))

      // Getting the output of the bank that's about to be issued to the writer
      val bank_issued_io = bank_ios(write_issue_q.io.deq.bits.laddr.acc_bank()) 

      // Reading from the Accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        if (use_shared_ext_mem) {
          io.ext_mem.get.acc(i) <> bio.ext_mem.get
        }

        acc_adders.io.in_sel(i) := bio.adder.valid
        acc_adders.io.ina(i) := bio.adder.op1
        acc_adders.io.inb(i) := bio.adder.op2
        bio.adder.sum := acc_adders.io.out

        val ex_read_req = io.acc.read_req(i)
        val exread = ex_read_req.valid

        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_norm_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_garbage() &&
          write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.acc_bank() === i.U

        bio.read.req.valid := exread || dmawrite
        ex_read_req.ready := bio.read.req.ready
 
        // The ExecuteController gets priority when reading from accumulator banks
        when (exread) {
          bio.read.req.bits.addr := ex_read_req.bits.addr
          bio.read.req.bits.act := ex_read_req.bits.act
          bio.read.req.bits.igelu_qb := ex_read_req.bits.igelu_qb
          bio.read.req.bits.igelu_qc := ex_read_req.bits.igelu_qc
          bio.read.req.bits.iexp_qln2 := ex_read_req.bits.iexp_qln2
          bio.read.req.bits.iexp_qln2_inv := ex_read_req.bits.iexp_qln2_inv
          bio.read.req.bits.scale := ex_read_req.bits.scale
          bio.read.req.bits.full := false.B
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.acc_row()
          bio.read.req.bits.full := write_dispatch_q.bits.laddr.read_full_acc_row
          bio.read.req.bits.act := write_dispatch_q.bits.acc_act
          bio.read.req.bits.igelu_qb := write_dispatch_q.bits.acc_igelu_qb.asTypeOf(bio.read.req.bits.igelu_qb)
          bio.read.req.bits.igelu_qc := write_dispatch_q.bits.acc_igelu_qc.asTypeOf(bio.read.req.bits.igelu_qc)
          bio.read.req.bits.iexp_qln2 := write_dispatch_q.bits.acc_iexp_qln2.asTypeOf(bio.read.req.bits.iexp_qln2)
          bio.read.req.bits.iexp_qln2_inv := write_dispatch_q.bits.acc_iexp_qln2_inv.asTypeOf(bio.read.req.bits.iexp_qln2_inv)
          bio.read.req.bits.scale := write_dispatch_q.bits.acc_scale.asTypeOf(bio.read.req.bits.scale)
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire) {
            write_dispatch_q.ready := true.B
            write_norm_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }
        bio.read.resp.ready := false.B

        when (write_norm_q.io.deq.valid &&
          acc_norm_unit_in.ready &&
          bio.read.resp.valid &&
          write_scale_q.io.enq.ready &&
          write_norm_q.io.deq.bits.laddr.is_acc_addr &&
          !write_norm_q.io.deq.bits.laddr.is_garbage() &&
          write_norm_q.io.deq.bits.laddr.acc_bank() === i.U)
        {
          write_norm_q.io.deq.ready := true.B
          acc_norm_unit_in.valid := true.B
          bio.read.resp.ready := true.B

          // Some normalizer commands don't write to main memory, so they don't need to be passed on to the scaling units
          write_scale_q.io.enq.valid := NormCmd.writes_to_main_memory(write_norm_q.io.deq.bits.laddr.norm_cmd)

          acc_norm_unit_in.bits.acc_read_resp := bio.read.resp.bits
          acc_norm_unit_in.bits.acc_read_resp.acc_bank_id := i.U
        }
      }

      // Writing to the accumulator banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        // Order of precedence during writes is ExecuteController, and then mvin_scale, and then mvin_scale_acc, and
        // then zero_writer

        val exwrite = io.acc.write(i).valid
        io.acc.write(i).ready := true.B
        assert(!(exwrite && !bio.write.ready), "Execute controller write to AccumulatorMem was skipped")

        // val from_mvin_scale = mvin_scale_out.valid && mvin_scale_out.bits.tag.is_acc
        val from_mvin_scale = mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc
        val from_mvin_scale_acc = mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.tag.is_acc

        // val mvin_scale_laddr = mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
        val mvin_scale_laddr = mvin_scale_pixel_repeater.io.resp.bits.laddr
        val mvin_scale_acc_laddr = mvin_scale_acc_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_acc_out.bits.row

        val dmaread_bank = Mux(from_mvin_scale, mvin_scale_laddr.acc_bank(),
          mvin_scale_acc_laddr.acc_bank())
        val dmaread_row = Mux(from_mvin_scale, mvin_scale_laddr.acc_row(), mvin_scale_acc_laddr.acc_row())

        // We need to make sure that we don't try to return a dma read resp from both mvin_scale and mvin_scale_acc
        // at the same time. mvin_scale always gets priority in this cases
        val spad_last = mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last && !mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc

        val dmaread = (from_mvin_scale || from_mvin_scale_acc) &&
          dmaread_bank === i.U /* &&
          (mvin_scale_same.B || from_mvin_scale || !spad_dmaread_last) */

        // We need to make sure that we don't try to return a dma read resp from both zero_writer and either mvin_scale
        // or mvin_acc_scale at the same time. The scalers always get priority in those cases
        /* val zerowrite = zero_writer.io.resp.valid && zero_writer.io.resp.bits.laddr.is_acc_addr &&
          zero_writer.io.resp.bits.laddr.acc_bank() === i.U && */
        val zerowrite = zero_writer_pixel_repeater.io.resp.valid && zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr &&
          zero_writer_pixel_repeater.io.resp.bits.laddr.acc_bank() === i.U &&
          // !((mvin_scale_out.valid && mvin_scale_out.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))
          !((mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))

        val consecutive_write_block = RegInit(false.B)
        if (acc_singleported) {
          val consecutive_write_sub_bank = RegInit(0.U((1 max log2Ceil(acc_sub_banks)).W))
          when (bio.write.fire && bio.write.bits.acc &&
            (bio.write.bits.addr(log2Ceil(acc_sub_banks)-1,0) === consecutive_write_sub_bank)) {
            consecutive_write_block := true.B
          } .elsewhen (bio.write.fire && bio.write.bits.acc) {
            consecutive_write_block := false.B
            consecutive_write_sub_bank := bio.write.bits.addr(log2Ceil(acc_sub_banks)-1,0)
          } .otherwise {
            consecutive_write_block := false.B
          }
        }
        bio.write.valid := false.B

        // bio.write.bits.acc := MuxCase(zero_writer.io.resp.bits.laddr.accumulate,
        bio.write.bits.acc := MuxCase(zero_writer_pixel_repeater.io.resp.bits.laddr.accumulate,
          Seq(exwrite -> io.acc.write(i).bits.acc,
            // from_mvin_scale -> mvin_scale_out.bits.tag.accumulate,
            from_mvin_scale -> mvin_scale_pixel_repeater.io.resp.bits.tag.accumulate,
            from_mvin_scale_acc -> mvin_scale_acc_out.bits.tag.accumulate))

        // bio.write.bits.addr := MuxCase(zero_writer.io.resp.bits.laddr.acc_row(),
        bio.write.bits.addr := MuxCase(zero_writer_pixel_repeater.io.resp.bits.laddr.acc_row(),
          Seq(exwrite -> io.acc.write(i).bits.addr,
            (from_mvin_scale || from_mvin_scale_acc) -> dmaread_row))

        when (exwrite) {
          bio.write.valid := true.B
          bio.write.bits.data := io.acc.write(i).bits.data
          bio.write.bits.mask := io.acc.write(i).bits.mask
        }.elsewhen (dmaread && !spad_last && !consecutive_write_block) {
          bio.write.valid := true.B
          bio.write.bits.data := Mux(from_mvin_scale,
            // VecInit(mvin_scale_out.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t),
            VecInit(mvin_scale_pixel_repeater.io.resp.bits.out.map(e => e.withWidthOf(accType))).asTypeOf(acc_row_t),
            mvin_scale_acc_out.bits.out.asTypeOf(acc_row_t))
          bio.write.bits.mask :=
            Mux(from_mvin_scale,
              {
                val n = accType.getWidth / inputType.getWidth
                // val mask = mvin_scale_out.bits.tag.mask take ((spad_w / (aligned_to * 8)) max 1)
                val mask = mvin_scale_pixel_repeater.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)
                val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
                expanded
              },
              mvin_scale_acc_out.bits.tag.mask)

          when(from_mvin_scale) {
            mvin_scale_pixel_repeater.io.resp.ready := bio.write.ready
          }.otherwise {
            mvin_scale_acc_out.ready := bio.write.ready
          }
        }.elsewhen (zerowrite && !spad_last && !consecutive_write_block) {
          bio.write.valid := true.B
          bio.write.bits.data := 0.U.asTypeOf(acc_row_t)
          bio.write.bits.mask := {
            val n = accType.getWidth / inputType.getWidth
            val mask = zero_writer_pixel_repeater.io.resp.bits.mask
            val expanded = VecInit(mask.flatMap(e => Seq.fill(n)(e)))
            expanded
          }

          zero_writer_pixel_repeater.io.resp.ready := bio.write.ready
        }.otherwise {
          bio.write.bits.data := DontCare
          bio.write.bits.mask := DontCare
        }
      }
      banks
    }
    //测试地址转换器
    val AddressSequenceGenerator_test = Module(new AddressSequenceGenerator(sp_bank_entries, sp_banks))
    AddressSequenceGenerator_test.io.addr_banks_in := MuxCase(0.U, Seq(
                                                      io.srams.read(0).req.valid -> 0.U,
                                                      io.srams.read(1).req.valid -> 1.U,
                                                      io.srams.read(2).req.valid -> 2.U,
                                                      io.srams.read(3).req.valid -> 3.U
                                                      ))
    AddressSequenceGenerator_test.io.addr := MuxCase(0.U, Seq(
                                             io.srams.read(0).req.valid -> io.srams.read(0).req.bits.addr,
                                             io.srams.read(1).req.valid -> io.srams.read(1).req.bits.addr,
                                             io.srams.read(2).req.valid -> io.srams.read(2).req.bits.addr,
                                             io.srams.read(3).req.valid -> io.srams.read(3).req.bits.addr
                                            ))
    AddressSequenceGenerator_test.io.valid := io.srams.read(0).req.valid || 
                                              io.srams.read(1).req.valid ||
                                              io.srams.read(2).req.valid ||
                                              io.srams.read(3).req.valid 
    io.AddressSequenceGenerator_test_outvalid := AddressSequenceGenerator_test.io.outValid
    //测试acc的地址转换器
    val AddressSequenceGenerator_ForAccumlator_test = Module(new AddressSequenceGenerator_ForAccumlator(acc_bank_entries, meshRows * tileRows, acc_banks))
    AddressSequenceGenerator_ForAccumlator_test.io.addr  := write_dispatch_q.bits.laddr.acc_row()
    AddressSequenceGenerator_ForAccumlator_test.io.valid := write_dispatch_q.valid && write_norm_q.io.enq.ready &&
                                                            !write_dispatch_q.bits.laddr.is_garbage() &&
                                                            write_dispatch_q.bits.laddr.is_acc_addr && 
                                                            (write_dispatch_q.bits.laddr.acc_bank() === 0.U || write_dispatch_q.bits.laddr.acc_bank() === 1.U)
    AddressSequenceGenerator_ForAccumlator_test.io.addr_banks_in := MuxCase(DontCare, Seq(
                                                                     (write_dispatch_q.bits.laddr.acc_bank() === 0.U) -> 0.U,
                                                                     (write_dispatch_q.bits.laddr.acc_bank() === 1.U) -> 1.U
                                                                    ))
    AddressSequenceGenerator_ForAccumlator_test.io.blocks := io.a_rows
    AddressSequenceGenerator_ForAccumlator_test.io.blocks_valid := io.srams.read(0).req.valid
    // 再建一个acc做测试
    val acc_adders_4 = Module(new AccPipeShared(acc_latency-1, acc_row_t, acc_banks))
    val acc_mems_test = {
      val banks = Seq.fill(acc_banks){Module(new AccumulatorMem_Simplify(
        acc_bank_entries, acc_row_t, acc_scale_func, acc_scale_t.asInstanceOf[V],
        acc_singleported, acc_sub_banks,
        use_shared_ext_mem,
        acc_latency, accType, is_dummy
      ))}
      val bank_ios = VecInit(banks.map(_.io))
      // 读控制
      bank_ios.zipWithIndex.foreach{ case (bio, i) =>
      bio.read.req.bits.addr := AddressSequenceGenerator_ForAccumlator_test.io.outAddr
 
      bio.read.req.valid := AddressSequenceGenerator_ForAccumlator_test.io.outValid && AddressSequenceGenerator_ForAccumlator_test.io.addr_banks_out === i.U
      bio.read.resp.ready := true.B
 
      acc_adders_4.io.in_sel(i) := bio.adder.valid
      acc_adders_4.io.ina(i) := bio.adder.op1
      acc_adders_4.io.inb(i) := bio.adder.op2
      bio.adder.sum := acc_adders_4.io.out
      }
      // 写控制
      bank_ios.zipWithIndex.foreach{ case (bio, i) =>
      bio.write.bits.addr := io.data_foracc_mem_test.addr.asTypeOf(bio.write.bits.addr)
      bio.write.bits.data := io.data_foracc_mem_test.data.asTypeOf(bio.write.bits.data)
      bio.write.bits.acc := true.B
      bio.write.bits.mask := VecInit(Seq.fill(meshColumns * tileColumns * 4)(true.B))
      bio.write.valid := io.valid_foracc_mem_test && io.addr_banks_foracc_mem_test === i.U
      }

      banks
    }
    // 修正模块，放在这里纯粹是为了资源开销
    val ErrorCorrector = Module(new ErrorCorrector(meshColumns * tileColumns, meshRows * tileRows, accType))
    ErrorCorrector.io.data_in := io.srams.read(0).resp.bits.data.asTypeOf(ErrorCorrector.io.data_in)
    ErrorCorrector.io.valid_in := io.srams.read(0).resp.valid
    ErrorCorrector.io.col_coordinate := 0.U
    ErrorCorrector.io.row_coordinate := 0.U
    ErrorCorrector.io.difference := inputType.zero
    val outputBits = ErrorCorrector.io.data_out.flatMap { elem =>
    // 如果accType是自定义Bundle，需要类似之前的Cat转换
    // 这里假设accType可以直接转UInt，否则需要自定义展开
    Seq(elem.asUInt, 0.U(1.W)) // 添加分隔位防止相邻元素位混淆
    }.reduce(_ ## _)
    val combined_1 = outputBits.xorR && ErrorCorrector.io.valid_out
    // 读spad延迟模块,放在这里纯粹是为了资源开销
    val LatencySimulation_ReadSpad = Module(new LatencySimulation_ReadSpad(meshColumns * tileColumns * inputType.getWidth, meshRows * tileRows))
    LatencySimulation_ReadSpad.io.in := io.srams.read(0).resp.bits   
    LatencySimulation_ReadSpad.io.valid_in := io.srams.read(0).resp.valid
    LatencySimulation_ReadSpad.io.Verification_completed := false.B
    // 手动展开结构体转换
    val respBits = Cat(
      LatencySimulation_ReadSpad.io.out.data,   // w位数据
      LatencySimulation_ReadSpad.io.out.fromDMA // 1位标志位
    ) // 总位宽 = w + 1
    // 归约异或所有有效位
    val respXor = respBits.xorR
    // 合并所有有效信号
    val combined = respXor && LatencySimulation_ReadSpad.io.valid_out

    //测试检错器
    val ErrorChecker_test = Module(new ErrorChecker(meshColumns * tileColumns, meshRows * tileRows, inputType, accType, sp_banks))
    // 对于Mvout的检错器
    val ErrorChecker_forMvout = Module(new ErrorChecker_forMvout(meshColumns * tileColumns, meshRows * tileRows, inputType, accType, sp_banks))
    //再建一个spad做测试
    val spad_mems_test = {
      val banks = Seq.fill(sp_banks) { Module(new ScratchpadBank(
        sp_bank_entries, spad_w,
        aligned_to, config.sp_singleported,
        use_shared_ext_mem, is_dummy
      )) }
      val bank_ios = VecInit(banks.map(_.io))
      // Reading from the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        if (use_shared_ext_mem) {
          io.ext_mem.get.spad(i) <> bio.ext_mem.get
        }

        //val ex_read_req = io.srams.read(i).req //原来的
        //val exread = ex_read_req.valid //原来的
        val exread = AddressSequenceGenerator_test.io.outValid && AddressSequenceGenerator_test.io.addr_banks_out === i.U//修改过的
 
        // TODO we tie the write dispatch queue's, and write issue queue's, ready and valid signals together here
        val dmawrite = write_dispatch_q.valid && write_norm_q.io.enq.ready &&
          !write_dispatch_q.bits.laddr.is_garbage() &&
          !(bio.write.en && config.sp_singleported.B) &&
          !write_dispatch_q.bits.laddr.is_acc_addr && write_dispatch_q.bits.laddr.sp_bank() === i.U 


        bio.read.req.valid := exread || dmawrite
        //ex_read_req.ready := bio.read.req.ready //原来的
        io.srams_test.read(i).req.ready := bio.read.req.ready //修改过的

        // The ExecuteController gets priority when reading from SRAMs
        when (exread) {
          //bio.read.req.bits.addr := ex_read_req.bits.addr //原来的
          bio.read.req.bits.addr := AddressSequenceGenerator_test.io.outAddr //修改过的
          bio.read.req.bits.fromDMA := false.B
        }.elsewhen (dmawrite) {
          bio.read.req.bits.addr := write_dispatch_q.bits.laddr.sp_row()
          bio.read.req.bits.fromDMA := true.B

          when (bio.read.req.fire) {
            write_dispatch_q.ready := true.B
            write_norm_q.io.enq.valid := true.B

            io.dma.write.resp.valid := true.B
          }
        }.otherwise {
          bio.read.req.bits := DontCare
        }

        val dma_read_resp = Wire(Decoupled(new ScratchpadReadResp(spad_w)))
        dma_read_resp.valid := bio.read.resp.valid && bio.read.resp.bits.fromDMA
        dma_read_resp.bits := bio.read.resp.bits
        val ex_read_resp = Wire(Decoupled(new ScratchpadReadResp(spad_w)))
        ex_read_resp.valid := bio.read.resp.valid && !bio.read.resp.bits.fromDMA
        ex_read_resp.bits := bio.read.resp.bits

        val dma_read_pipe = Pipeline(dma_read_resp, spad_read_delay)
        val ex_read_pipe = Pipeline(ex_read_resp, spad_read_delay)

        bio.read.resp.ready := Mux(bio.read.resp.bits.fromDMA, dma_read_resp.ready, ex_read_resp.ready)

        dma_read_pipe.ready := writer.module.io.req.ready &&
          !write_issue_q.io.deq.bits.laddr.is_acc_addr && write_issue_q.io.deq.bits.laddr.sp_bank() === i.U && // I believe we don't need to check that write_issue_q is valid here, because if the SRAM's resp is valid, then that means that the write_issue_q's deq should also be valid
          !write_issue_q.io.deq.bits.laddr.is_garbage()
        when (dma_read_pipe.fire) {
          writeData.valid := true.B
          writeData.bits := dma_read_pipe.bits.data
        }

        io.srams_test.read(i).resp <> ex_read_pipe
      }

      // Writing to the SRAM banks
      bank_ios.zipWithIndex.foreach { case (bio, i) =>
        val exwrite = io.srams.write(i).en

        // val laddr = mvin_scale_out.bits.tag.addr.asTypeOf(local_addr_t) + mvin_scale_out.bits.row
        val laddr = mvin_scale_pixel_repeater.io.resp.bits.laddr

        // val dmaread = mvin_scale_out.valid && !mvin_scale_out.bits.tag.is_acc &&
        val dmaread = mvin_scale_pixel_repeater.io.resp.valid && !mvin_scale_pixel_repeater.io.resp.bits.tag.is_acc &&
          laddr.sp_bank() === i.U // &&
          // !ErrorChecker_test.io.errorDetected &&     // 为了保证模块不被优化掉加的。没什么逻辑上的道理
          // !ErrorChecker_forMvout.io.errorDetected && // 为了保证模块不被优化掉加的。没什么逻辑上的道理
          // !combined &&                               // 为了保证模块不被优化掉加的。没什么逻辑上的道理
          // !combined_1                                // 为了保证模块不被优化掉加的。没什么逻辑上的道理

        // We need to make sure that we don't try to return a dma read resp from both zero_writer and either mvin_scale
        // or mvin_acc_scale at the same time. The scalers always get priority in those cases
        /* val zerowrite = zero_writer.io.resp.valid && !zero_writer.io.resp.bits.laddr.is_acc_addr &&
          zero_writer.io.resp.bits.laddr.sp_bank() === i.U && */
        val zerowrite = zero_writer_pixel_repeater.io.resp.valid && !zero_writer_pixel_repeater.io.resp.bits.laddr.is_acc_addr &&
          zero_writer_pixel_repeater.io.resp.bits.laddr.sp_bank() === i.U &&
          // !((mvin_scale_out.valid && mvin_scale_out.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))
          !((mvin_scale_pixel_repeater.io.resp.valid && mvin_scale_pixel_repeater.io.resp.bits.last) || (mvin_scale_acc_out.valid && mvin_scale_acc_out.bits.last))

        bio.write.en := exwrite || dmaread || zerowrite
 
        when (exwrite) {
          bio.write.addr := io.srams.write(i).addr
          bio.write.data := io.srams.write(i).data
          bio.write.mask := io.srams.write(i).mask
        }.elsewhen (dmaread) {
          bio.write.addr := laddr.sp_row()
          bio.write.data := mvin_scale_pixel_repeater.io.resp.bits.out.asUInt
          bio.write.mask := mvin_scale_pixel_repeater.io.resp.bits.mask take ((spad_w / (aligned_to * 8)) max 1)

          //mvin_scale_pixel_repeater.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.elsewhen (zerowrite) {
          bio.write.addr := zero_writer_pixel_repeater.io.resp.bits.laddr.sp_row()
          bio.write.data := 0.U
          bio.write.mask := zero_writer_pixel_repeater.io.resp.bits.mask

          //zero_writer_pixel_repeater.io.resp.ready := true.B // TODO we combinationally couple valid and ready signals
        }.otherwise {
          bio.write.addr := DontCare
          bio.write.data := DontCare
          bio.write.mask := DontCare
        }
      }
      banks
    }

    //测试mvin累加器
    // val SegmentAccumulator_test = Module(new SegmentAccumulator(sp_bank_entries, sp_bank_entries / 8, meshColumns * tileColumns, inputType, accType, sp_banks))
    // 定义输入源的有效信号
    val mvin_valid = mvin_scale_pixel_repeater.io.resp.valid
    val zero_valid = zero_writer_pixel_repeater.io.resp.valid

    // 确保两个有效信号不会同时为高
    //assert(!(mvin_valid && zero_valid), "mvin_valid and zero_valid should not be high at the same time")

/*     when(mvin_valid){
      SegmentAccumulator_test.io.addr := mvin_scale_pixel_repeater.io.resp.bits.laddr.sp_row()
      SegmentAccumulator_test.io.dataIn := mvin_scale_pixel_repeater.io.resp.bits.out
      SegmentAccumulator_test.io.maskIn := mvin_scale_pixel_repeater.io.resp.bits.mask
      SegmentAccumulator_test.io.validIn := mvin_scale_pixel_repeater.io.resp.valid
      SegmentAccumulator_test.io.addr_banks_in := mvin_scale_pixel_repeater.io.resp.bits.laddr.sp_bank()
      SegmentAccumulator_test.io.gemminiWrite.ready := true.B
    }.elsewhen(zero_valid){
      SegmentAccumulator_test.io.addr := zero_writer_pixel_repeater.io.resp.bits.laddr.sp_row()
      SegmentAccumulator_test.io.dataIn := 0.U.asTypeOf(SegmentAccumulator_test.io.dataIn.cloneType)
      SegmentAccumulator_test.io.maskIn := zero_writer_pixel_repeater.io.resp.bits.mask
      SegmentAccumulator_test.io.validIn := zero_writer_pixel_repeater.io.resp.valid
      SegmentAccumulator_test.io.addr_banks_in := zero_writer_pixel_repeater.io.resp.bits.laddr.sp_bank()
      SegmentAccumulator_test.io.gemminiWrite.ready := true.B
    }.otherwise{
      // 当没有有效信号时，驱动默认值
      SegmentAccumulator_test.io.addr := 0.U
      SegmentAccumulator_test.io.dataIn := 0.U.asTypeOf(SegmentAccumulator_test.io.dataIn.cloneType)
      SegmentAccumulator_test.io.maskIn := 0.U.asTypeOf(SegmentAccumulator_test.io.maskIn.cloneType)
      SegmentAccumulator_test.io.validIn := false.B
      SegmentAccumulator_test.io.addr_banks_in := 0.U
      SegmentAccumulator_test.io.gemminiWrite.ready := true.B
    } */

    //测试mvin累加器简化版
    val SegmentAccumulator_Simplify_test = Module(new SegmentAccumulator_Simplify_DoubleBuffer(meshColumns * tileColumns, inputType, accType, sp_bank_entries))

    // 确保两个有效信号不会同时为高
    //assert(!(mvin_valid && zero_valid), "mvin_valid and zero_valid should not be high at the same time")

    when(mvin_valid){
      SegmentAccumulator_Simplify_test.io.addr := mvin_scale_pixel_repeater.io.resp.bits.laddr.sp_row()
      SegmentAccumulator_Simplify_test.io.dataIn := mvin_scale_pixel_repeater.io.resp.bits.out
      SegmentAccumulator_Simplify_test.io.maskIn := mvin_scale_pixel_repeater.io.resp.bits.mask
      SegmentAccumulator_Simplify_test.io.validIn := mvin_scale_pixel_repeater.io.resp.valid
      SegmentAccumulator_Simplify_test.io.gemminiWrite.ready := true.B
    }.elsewhen(zero_valid){
      SegmentAccumulator_Simplify_test.io.addr := zero_writer_pixel_repeater.io.resp.bits.laddr.sp_row()
      SegmentAccumulator_Simplify_test.io.dataIn := 0.U.asTypeOf(SegmentAccumulator_Simplify_test.io.dataIn.cloneType)
      SegmentAccumulator_Simplify_test.io.maskIn := zero_writer_pixel_repeater.io.resp.bits.mask
      SegmentAccumulator_Simplify_test.io.validIn := zero_writer_pixel_repeater.io.resp.valid
      SegmentAccumulator_Simplify_test.io.gemminiWrite.ready := true.B
    }.otherwise{
      // 当没有有效信号时，驱动默认值
      SegmentAccumulator_Simplify_test.io.addr := 0.U
      SegmentAccumulator_Simplify_test.io.dataIn := 0.U.asTypeOf(SegmentAccumulator_Simplify_test.io.dataIn.cloneType)
      SegmentAccumulator_Simplify_test.io.maskIn := 0.U.asTypeOf(SegmentAccumulator_Simplify_test.io.maskIn.cloneType)
      SegmentAccumulator_Simplify_test.io.validIn := false.B
      SegmentAccumulator_Simplify_test.io.gemminiWrite.ready := true.B
    }

    //测试简化acc,用来存mvin的校验位
    val acc_adders_2 = Module(new AccPipeShared(acc_latency-1, acc_row_t, sp_banks))

    val acc_Simplify_forMvin = {
      val banks = Seq.fill(sp_banks){ Module(new AccumulatorMem_Simplify(
        sp_bank_entries / 8, acc_row_t, acc_scale_func, acc_scale_t.asInstanceOf[V],
        acc_singleported, acc_sub_banks,
        use_shared_ext_mem,
        acc_latency, accType, is_dummy
      )) }
      val bank_ios = VecInit(banks.map(_.io))
      //读控制
      bank_ios.zipWithIndex.foreach{ case (bio, i) =>
        bio.read.req.bits.addr := AddressSequenceGenerator_test.io.outAddr_acc
        bio.read.req.valid := AddressSequenceGenerator_test.io.outValid_acc && AddressSequenceGenerator_test.io.addr_banks_out === i.U
        bio.read.resp.ready := true.B
  
        acc_adders_2.io.in_sel(i) := bio.adder.valid
        acc_adders_2.io.ina(i) := bio.adder.op1
        acc_adders_2.io.inb(i) := bio.adder.op2
        bio.adder.sum := acc_adders_2.io.out
      }
      //写控制
      bank_ios.zipWithIndex.foreach{ case (bio, i) =>
        bio.write.bits.addr := SegmentAccumulator_Simplify_test.io.gemminiWrite.bits.addr
        bio.write.bits.data := SegmentAccumulator_Simplify_test.io.gemminiWrite.bits.data.asTypeOf(acc_row_t)
        bio.write.bits.acc := true.B
        bio.write.bits.mask := VecInit(Seq.fill(meshColumns * tileColumns * accType.getWidth / 8)(true.B))
        bio.write.valid := SegmentAccumulator_Simplify_test.io.gemminiWrite.valid && mvin_scale_pixel_repeater.io.resp.bits.laddr.sp_bank() === i.U
      }
      banks
    }

    //用简化acc装乘法结果的校验位
    val acc_adders_3 = Module(new AccPipeShared(acc_latency-1, acc_row_t, acc_banks))

    val acc_Simplify_ForMatmulResult = {
      val banks = Seq.fill(acc_banks){Module(new AccumulatorMem_Simplify(
        acc_bank_entries, acc_row_t, acc_scale_func, acc_scale_t.asInstanceOf[V],
        acc_singleported, acc_sub_banks,
        use_shared_ext_mem,
        acc_latency, accType, is_dummy
      ))}
      val bank_ios = VecInit(banks.map(_.io))
      // 读控制
      bank_ios.zipWithIndex.foreach{ case (bio, i) =>
      bio.read.req.bits.addr := AddressSequenceGenerator_ForAccumlator_test.io.outAddr_acc
 
      bio.read.req.valid := AddressSequenceGenerator_ForAccumlator_test.io.outValid_acc && AddressSequenceGenerator_ForAccumlator_test.io.addr_banks_out === i.U
      bio.read.resp.ready := true.B
 
      acc_adders_3.io.in_sel(i) := bio.adder.valid
      acc_adders_3.io.ina(i) := bio.adder.op1
      acc_adders_3.io.inb(i) := bio.adder.op2
      bio.adder.sum := acc_adders_3.io.out
      }
      // 写控制
      bank_ios.zipWithIndex.foreach{ case (bio, i) =>
      bio.write.bits.addr := io.checksum_addr.asTypeOf(bio.write.bits.addr)
      bio.write.bits.data := io.checksum.asTypeOf(bio.write.bits.data)
      bio.write.bits.acc := true.B
      bio.write.bits.mask := VecInit(Seq.fill(meshColumns * tileColumns * accType.getWidth / 8)(true.B))
      bio.write.valid := io.checksum_valid && io.checksum_addr_banks === i.U
      }

      banks
    }


    ErrorChecker_test.io.dataIn1 := MuxCase(DontCare, Seq(
                                     spad_mems_test(0).io.read.resp.valid -> spad_mems_test(0).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn1.cloneType),
                                     spad_mems_test(1).io.read.resp.valid -> spad_mems_test(1).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn1.cloneType),
                                     spad_mems_test(2).io.read.resp.valid -> spad_mems_test(2).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn1.cloneType),
                                     spad_mems_test(3).io.read.resp.valid -> spad_mems_test(3).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn1.cloneType)
                                    ))
    ErrorChecker_test.io.dataIn1_valid := spad_mems_test(0).io.read.resp.valid ||
                                          spad_mems_test(1).io.read.resp.valid ||
                                          spad_mems_test(2).io.read.resp.valid ||
                                          spad_mems_test(3).io.read.resp.valid 
    ErrorChecker_test.io.dataIn2 := MuxCase(DontCare, Seq(
                                     acc_Simplify_forMvin(0).io.read.resp.valid -> acc_Simplify_forMvin(0).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn2.cloneType),
                                     acc_Simplify_forMvin(1).io.read.resp.valid -> acc_Simplify_forMvin(1).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn2.cloneType),
                                     acc_Simplify_forMvin(2).io.read.resp.valid -> acc_Simplify_forMvin(2).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn2.cloneType),
                                     acc_Simplify_forMvin(3).io.read.resp.valid -> acc_Simplify_forMvin(3).io.read.resp.bits.data.asTypeOf(ErrorChecker_test.io.dataIn2.cloneType)
                                    ))
    ErrorChecker_test.io.dataIn2_valid := acc_Simplify_forMvin(0).io.read.resp.valid ||
                                          acc_Simplify_forMvin(1).io.read.resp.valid ||
                                          acc_Simplify_forMvin(2).io.read.resp.valid ||
                                          acc_Simplify_forMvin(3).io.read.resp.valid 
    ErrorChecker_test.io.last := AddressSequenceGenerator_test.io.last
    ErrorChecker_test.io.addr_banks_in := MuxCase(DontCare, Seq(
                                           spad_mems_test(0).io.read.resp.valid -> 0.U,
                                           spad_mems_test(1).io.read.resp.valid -> 1.U,
                                           spad_mems_test(2).io.read.resp.valid -> 2.U,
                                           spad_mems_test(3).io.read.resp.valid -> 3.U
                                          ))

    ErrorChecker_forMvout.io.dataIn1 := MuxCase(DontCare, Seq(
                                     acc_mems_test(0).io.read.resp.valid -> acc_mems_test(0).io.read.resp.bits.data.asTypeOf(ErrorChecker_forMvout.io.dataIn1.cloneType),
                                     acc_mems_test(1).io.read.resp.valid -> acc_mems_test(1).io.read.resp.bits.data.asTypeOf(ErrorChecker_forMvout.io.dataIn1.cloneType)
                                    ))
    ErrorChecker_forMvout.io.dataIn1_valid := acc_mems_test(0).io.read.resp.valid ||
                                              acc_mems_test(1).io.read.resp.valid
    ErrorChecker_forMvout.io.dataIn2 := MuxCase(DontCare, Seq(
                                     acc_Simplify_ForMatmulResult(0).io.read.resp.valid -> acc_Simplify_ForMatmulResult(0).io.read.resp.bits.data.asTypeOf(ErrorChecker_forMvout.io.dataIn2.cloneType),
                                     acc_Simplify_ForMatmulResult(1).io.read.resp.valid -> acc_Simplify_ForMatmulResult(1).io.read.resp.bits.data.asTypeOf(ErrorChecker_forMvout.io.dataIn2.cloneType)
                                    ))                                          
    ErrorChecker_forMvout.io.dataIn2_valid := acc_Simplify_ForMatmulResult(0).io.read.resp.valid ||
                                              acc_Simplify_ForMatmulResult(1).io.read.resp.valid
    ErrorChecker_forMvout.io.last := DontCare
    ErrorChecker_forMvout.io.addr_banks_in := DontCare          

                                        
    // Counter connection
    io.counter.collect(reader.module.io.counter)
    io.counter.collect(writer.module.io.counter)

    io.Verification_completed := ErrorChecker_test.io.Verification_completed
    io.addr_banks := ErrorChecker_test.io.addr_banks_out
  }
} 
