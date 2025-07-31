
package gemmini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import gemmini.AccumulatorScale.iexp
import hardfloat.{DivSqrtRecFN_small, INToRecFN, consts, fNFromRecFN}

class NormalizedInput[T <: Data: Arithmetic, U <: Data](max_len: Int, num_stats: Int, fullDataType: Vec[Vec[T]],
                                                        scale_t: U) extends Bundle {
  val acc_read_resp = new AccumulatorReadResp[T,U](fullDataType, scale_t)
  val len = UInt(log2Up(max_len + 1).W)
  val stats_id = UInt(log2Up(num_stats).W)
  val cmd = NormCmd()
}

class NormalizedOutput[T <: Data: Arithmetic, U <: Data](fullDataType: Vec[Vec[T]], scale_t: U) extends Bundle {
  val acc_read_resp = new AccumulatorReadResp[T,U](fullDataType, scale_t)
  val mean = fullDataType.head.head.cloneType
  val max = fullDataType.head.head.cloneType
  val inv_stddev = scale_t.cloneType
  val inv_sum_exp = scale_t.cloneType
}

class IExpConst[T <: Data](acc_t: T) extends Bundle {
  val qb = acc_t.cloneType
  val qc = acc_t.cloneType
  val qln2 = acc_t.cloneType
  val qln2_inv = acc_t.cloneType
}

class AccumulationLanes[T <: Data](num_stats: Int, acc_t: T, n_lanes: Int, latency: Int)(implicit ev: Arithmetic[T])
  extends Module {
  // Each lane computes a sum, or an error-squared sum
  //num_stats: 统计数据的数量
  //acc_t: 累积数据的类型
  //n_lanes: 计算通道的数量
  //latency: 延迟
  import ev._

  ////表示每个通道的输出
  class LaneOutput extends Bundle {
    val result = acc_t.cloneType              //累积结果
    val stats_id = UInt(log2Up(num_stats).W)  //统计数据 ID
  }

  val io = IO(new Bundle {
    val ins = Flipped(Valid(new Bundle {
      val len = UInt(log2Up(n_lanes+1).W)      //有效数据长度，范围为 0 到 n_lanes
      val data = Vec(n_lanes, acc_t)           //输入数据向量，每个元素类型为 acc_t
      val mean = acc_t.cloneType               //均值，用于计算方差
      val max = acc_t.cloneType                //数据的最大值，用于指数计算
      val iexp_const = new IExpConst(acc_t)    //用于指数计算的常数
      val cmd = NormCmd()                      //命令，指示执行哪种操作
      val stats_id = UInt(log2Up(num_stats).W) //统计数据的 ID
    }))

    val out = Valid(new LaneOutput)

    val busy = Output(Bool())
  })

  val cmd = io.ins.bits.cmd           //提取输入中的命令
  val mean = io.ins.bits.mean         //提取输入中的均值
  val iexp_c = io.ins.bits.iexp_const //提取输入中的指数常数

  val data = io.ins.bits.data.zipWithIndex.map { case (d, i) =>
    val iexp_result = iexp(d - io.ins.bits.max, iexp_c.qln2, iexp_c.qln2_inv, iexp_c.qb, iexp_c.qc) //计算了指数结果，计算时减去最大值，防止指数爆炸，所以iexp可以看到是先取反再求指数
    Mux(i.U < io.ins.bits.len,                                                                      //判断当前索引 i 是否小于输入的有效长度 len，如果是，则根据命令 cmd 执行不同的操作
      MuxCase(d, Seq(                                                                               //如果没有匹配的命令，则返回原始数据 d
        (cmd === NormCmd.VARIANCE || cmd === NormCmd.INV_STDDEV) -> (d-mean)*(d-mean),              //如果 cmd 是 NormCmd.VARIANCE 或 NormCmd.INV_STDDEV，则计算 (d - mean) * (d - mean)，即方差
        (cmd === NormCmd.SUM_EXP || cmd === NormCmd.INV_SUM_EXP) -> iexp_result                     //如果 cmd 是 NormCmd.SUM_EXP 或 NormCmd.INV_SUM_EXP，则使用之前计算的 iexp_result
        //iexp(d - io.ins.bits.max, iexp_c.qln2, iexp_c.qln2_inv, iexp_c.qb, iexp_c.qc)
      )).withWidthOf(acc_t),
      d.zero)                                                                                       //如果 i.U >= io.ins.bits.len，则返回 d.zero，即零值
  }

  val result = data.reduce(_ + _)                                                                   //对处理后的数据向量进行求和，得到最终的结果 result

  val pipe = Module(new Pipeline[LaneOutput](new LaneOutput, latency)())

  pipe.io.in.valid := io.ins.valid
  // io.ins.ready := pipe.io.in.ready
  pipe.io.in.bits.result := result
  pipe.io.in.bits.stats_id := io.ins.bits.stats_id

  io.out.valid := pipe.io.out.valid
  pipe.io.out.ready := true.B
  // pipe.io.out.ready := io.out.ready
  io.out.bits := pipe.io.out.bits

  io.busy := pipe.io.busy
}

class MaxLanes[T <: Data](num_stats: Int, acc_t: T, n_lanes: Int, latency: Int)(implicit ev: Arithmetic[T])
  extends Module {
  // Each lane computes a sum, or an error-squared sum

  import ev._
  import NormCmd._

  class LaneOutput extends Bundle {
    val result = acc_t.cloneType
    val stats_id = UInt(log2Up(num_stats).W)
  }

  val io = IO(new Bundle {
    val ins = Flipped(Valid(new Bundle {
      val len = UInt(log2Up(n_lanes + 1).W)
      val data = Vec(n_lanes, acc_t)
      val stats_id = UInt(log2Up(num_stats).W)
    }))

    val out = Valid(new LaneOutput)

    val busy = Output(Bool())
  })

  val data = io.ins.bits.data.zipWithIndex.map { case (d, i) => // 索引位置有效时取原始数据，如果索引超出有效数据个数，将原始数据取该类型数据的最小值，为了不影响接下来的最大值判断
    Mux(i.U < io.ins.bits.len, d.withWidthOf(acc_t), d.minimum)
  }

  val result = data.reduce({ (max, x) => Mux(x > max, x, max) }) // 选出最大值

  val pipe = Module(new Pipeline[LaneOutput](new LaneOutput, latency)())

  pipe.io.in.valid := io.ins.valid
  // io.ins.ready := pipe.io.in.ready
  pipe.io.in.bits.result := result
  pipe.io.in.bits.stats_id := io.ins.bits.stats_id

  io.out.valid := pipe.io.out.valid
  pipe.io.out.ready := true.B
  // pipe.io.out.ready := io.out.ready
  io.out.bits := pipe.io.out.bits

  io.busy := pipe.io.busy
}

class Normalizer[T <: Data, U <: Data](max_len: Int, num_reduce_lanes: Int, num_stats: Int, latency: Int,
                                       fullDataType: Vec[Vec[T]], scale_t: U)
                                      (implicit ev: Arithmetic[T]) extends Module {
  import ev._
  val acc_t = fullDataType.head.head.cloneType
  val vec_size = fullDataType.flatten.size
  val n_lanes = if (num_reduce_lanes < 0) vec_size else num_reduce_lanes

  assert(isPow2(n_lanes))

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new NormalizedInput[T,U](max_len, num_stats, fullDataType, scale_t)))
    val out = Decoupled(new NormalizedOutput(fullDataType, scale_t))
  })

  object State extends ChiselEnum {
    // NOTE: We assume that "idle" and "output" are the first two states. We also assume that all the enums on the same
    //   line keep the order below
    val idle, output = Value
    val get_sum = Value
    val get_mean, waiting_for_mean = Value
    val get_variance, waiting_for_variance, get_stddev, waiting_for_stddev, get_inv_stddev, waiting_for_inv_stddev = Value
    val get_max = Value
    val get_inv_sum_exp, waiting_for_inv_sum_exp = Value
  }
    // get_variance：获取方差的状态
    // waiting_for_variance：等待方差计算完成的状态
    // get_stddev：获取标准差的状态
    // waiting_for_stddev：等待标准差计算完成的状态
    // get_inv_stddev：获取标准差倒数的状态
    // waiting_for_inv_stddev：等待标准差倒数计算完成的状态
    // get_inv_sum_exp：获取指数和倒数的状态
    // waiting_for_inv_sum_exp：等待指数和倒数计算完成的状态
  import State._
  //State._ 表示导入 State 对象中的所有公共成员。在这个上下文中，这意味着所有定义的枚举值（如 idle、output、get_sum 等）都可以在当前作用域中直接使用。
  //例如，如果你在某个地方想要使用 get_mean，在导入语句之前，你需要写成 State.get_mean，而在导入语句之后，可以直接写成 get_mean。

  // Buffers for normalization stats
  class Stats extends Bundle {
    val req = new NormalizedInput[T,U](max_len, num_stats, fullDataType, scale_t)
    val state = State()

    // Running state
    val sum = acc_t.cloneType
    val count = UInt(16.W) // TODO magic number
    val running_max = acc_t.cloneType
    val max = acc_t.cloneType

    // Iterative state
    val mean = acc_t.cloneType
    val inv_stddev = acc_t.cloneType
    val inv_sum_exp = acc_t.cloneType

    val elems_left = req.len.cloneType // 当前还剩下多少个元素未处理?

    def vec_grouped = VecInit(req.acc_read_resp.data.flatten.grouped(n_lanes).map(v => VecInit(v)).toSeq) // 将data中的数据展平后按n_lanes个为一组分组
    def vec_groups_left = elems_left / n_lanes.U + (elems_left % n_lanes.U =/= 0.U)                       // 返回剩余的向量组数

    def cmd = req.cmd

    //表示在当前状态下是否等待某些通道的输出
    def waiting_for_lanes_to_drain =
      (cmd === NormCmd.MEAN && (state === get_sum || state === get_mean)) ||
      (cmd === NormCmd.INV_STDDEV && (state === get_sum || state === get_variance)) ||
      (cmd === NormCmd.MAX && (state === get_max)) ||
      (cmd === NormCmd.INV_SUM_EXP && (state === get_sum))
  }

  val stats = Reg(Vec(num_stats, new Stats))                    // stats包含了与归一化操作相关的各种状态和统计信息
  val done_with_functional_units = Wire(Vec(num_stats, Bool())) // 表示每个统计单元是否已经完成其功能单元的操作
  val next_states = Wire(Vec(num_stats, State()))               // 用于存储每个统计单元的下一个状态

  (stats.map(_.state) zip next_states).foreach { case (s, ns) => s := ns } // 将下一个状态ns赋值给当前状态s

  // IO
  val in_stats_id = io.in.bits.stats_id // 当前输入数据包的统计ID
  io.in.ready := (stats(in_stats_id).state === idle || done_with_functional_units(in_stats_id)) &&
    stats.map(!_.waiting_for_lanes_to_drain).reduce(_ && _) // 表示模块是否准备好接收新的输入数据

  val out_stats_id = MuxCase((num_stats-1).U,               // 使用MuxCase选择当前处于输出状态的统计单元的ID。如果没有单元处于输出状态，默认返回(num_stats-1).U 
    stats.zipWithIndex.map { case (s,i) => (s.state === output) -> i.U }
  )

  io.out.valid := stats(out_stats_id).state === output
  io.out.bits.acc_read_resp := stats(out_stats_id).req.acc_read_resp
  io.out.bits.mean := stats(out_stats_id).mean
  io.out.bits.max := stats(out_stats_id).max
  io.out.bits.inv_stddev := stats(out_stats_id).inv_stddev.asTypeOf(scale_t)
  io.out.bits.inv_sum_exp := stats(out_stats_id).inv_sum_exp.asTypeOf(scale_t)

  // Lanes and functional units
  val lanes = Module(new AccumulationLanes(num_stats, acc_t, n_lanes, latency)) // 方差、指数相关基础计算
  val max_lanes = Module(new MaxLanes(num_stats, acc_t, n_lanes, latency))      // TODO: change latency?寻最大值
 
  {
    // Lanes input
    val in_lanes_stats_id = MuxCase((num_stats-1).U,
      stats.zipWithIndex.map { case (s,i) => (s.state === get_sum) -> i.U }    // 确定哪个统计单元当前需要进行累加操作
    )

    val stat = stats(in_lanes_stats_id)

    val len = Mux(stat.elems_left % n_lanes.U === 0.U, n_lanes.U, stat.elems_left % n_lanes.U) // 确定当前数据组的有效长度。确保在最后一组时不会超出剩余元素

    lanes.io.ins.valid := stat.state === get_sum && stat.vec_groups_left > 0.U // 当且仅当当前状态为get_sum且有剩余数据组时，输入有效
    lanes.io.ins.bits.data := stat.vec_grouped(stat.vec_groups_left-1.U)       // 输入数据，是当前剩余数据组的最后一组
    lanes.io.ins.bits.mean := stat.mean                                        // 当前统计单元的均值，用于累加计算
    lanes.io.ins.bits.max := stat.max                                          // 当前统计单元的最大值，用于累加计算

    val iexp_const = Wire(new IExpConst(acc_t))//计算过程中使用的常量
    iexp_const.qln2 := io.in.bits.acc_read_resp.iexp_qln2.asTypeOf(iexp_const.qln2)
    iexp_const.qln2_inv := io.in.bits.acc_read_resp.iexp_qln2_inv.asTypeOf(iexp_const.qln2_inv)
    iexp_const.qb := io.in.bits.acc_read_resp.igelu_qb.asTypeOf(iexp_const.qb)
    iexp_const.qc := io.in.bits.acc_read_resp.igelu_qc.asTypeOf(iexp_const.qc)

    lanes.io.ins.bits.cmd := stat.cmd
    lanes.io.ins.bits.len := len
    lanes.io.ins.bits.stats_id := in_lanes_stats_id
    lanes.io.ins.bits.iexp_const := iexp_const

    when (lanes.io.ins.fire()) {               // 当输入信号有效且被接受时触发
      stat.elems_left := stat.elems_left - len // 更新剩余元素计数，减少已处理的元素数
    }
  }

  {
    // Lanes output
    val out_lanes_stats_id = lanes.io.out.bits.stats_id // 从lanes模块的输出中获取统计单元ID

    val stat = stats(out_lanes_stats_id)                // 选定的统计单元数据

    when (lanes.io.out.fire()) {                        // 当输出信号有效且被接受时触发
      stat.sum := stat.sum + lanes.io.out.bits.result   // 更新当前统计单元的累加和
    }
  }

  {
    // Max lanes input
    val max_in_lanes_stats_id = MuxCase((num_stats-1).U, // 使用MuxCase从stats向量中选择当前状态为get_max的统计单元ID
      stats.zipWithIndex.map { case (s,i) => (s.state === get_max) -> i.U }
    )

    val stat = stats(max_in_lanes_stats_id) // 选定的统计单元数据

    val len = Mux(stat.elems_left % n_lanes.U === 0.U, n_lanes.U, stat.elems_left % n_lanes.U) // 确定当前数据组的有效长度

    max_lanes.io.ins.valid := stat.state === get_max && stat.vec_groups_left > 0.U
    max_lanes.io.ins.bits.data := stat.vec_grouped(stat.vec_groups_left-1.U)
    max_lanes.io.ins.bits.len := len
    max_lanes.io.ins.bits.stats_id := max_in_lanes_stats_id

    when (max_lanes.io.ins.fire()) {
      stat.elems_left := stat.elems_left - len // 更新剩余元素计数，减少已处理的元素数
    }
  }

  {
    // Max lanes output
    val max_out_lanes_stats_id = max_lanes.io.out.bits.stats_id

    val stat = stats(max_out_lanes_stats_id)

    when (max_lanes.io.out.fire()) {
      stat.running_max := Mux(max_lanes.io.out.bits.result > stat.running_max, max_lanes.io.out.bits.result, stat.running_max) // 更新最大值
      //stat.max := Mux(max_lanes.io.out.bits.result > stat.max, max_lanes.io.out.bits.result, stat.max)
    }
  }

   
  // 用于计算均值和方差，将之前所作的加和求均值
  val sum_to_divide_id = MuxCase((num_stats-1).U,
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_mean || s.state === get_variance) -> i.U }
  )
  val sum_to_divide = stats(sum_to_divide_id).sum
  val (divider_in, divider_out) = sum_to_divide.divider(stats.head.count).get

  {
    // Divider input
    val stat = stats(sum_to_divide_id)

    divider_in.valid := (stat.state === get_mean || stat.state === get_variance) && !lanes.io.busy // 当统计单元的状态为get_mean或get_variance，且lanes模块不忙时，设置输入有效
    divider_in.bits := stat.count                                                                  // 提供除数，即统计单元的count
  }

  {
    // Divider output
    val waiting_for_divide_id = MuxCase((num_stats-1).U, // 选择当前等待除法结果的统计单元ID
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_mean || s.state === waiting_for_variance) -> i.U }
    )
    val stat = stats(waiting_for_divide_id)

    divider_out.ready := stat.state === waiting_for_mean || stat.state === waiting_for_variance // 当统计单元处于waiting_for_mean或waiting_for_variance状态时，准备接收除法结果

    when(stat.state === waiting_for_mean) {
      stat.mean := divider_out.bits       // 如果状态为waiting_for_mean，将输出结果赋值给stat.mean
    }.elsewhen(stat.state === waiting_for_variance) {
      stat.inv_stddev := divider_out.bits // 如果状态为waiting_for_variance，将输出结果赋值给stat.inv_stddev
    }
  }


  // 计算标准差的逻辑
  val variance_to_sqrt_id = MuxCase((num_stats-1).U,           // 使用MuxCase从stats向量中选择当前需要进行平方根计算的统计单元ID
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_stddev) -> i.U }
  )
  val variance_to_sqrt = stats(variance_to_sqrt_id).inv_stddev // 从选定的统计单元中提取的inv_stddev字段，这里它表示需要计算平方根的方差倒数
  val (sqrt_in, sqrt_out) = variance_to_sqrt.sqrt.get

  {
    // Sqrt input
    val stat = stats(variance_to_sqrt_id) //选中当前需要进行平方根计算的统计单元

    sqrt_in.valid := stat.state === get_stddev //当统计单元的状态为get_stddev时，设置输入有效
  }

  {
    // Sqrt output
    val waiting_for_sqrt_id = MuxCase((num_stats-1).U, //选择当前等待平方根结果的统计单元ID
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_stddev) -> i.U }
    )
    val stat = stats(waiting_for_sqrt_id) //选中当前正在等待平方跟结果的统计单元

    sqrt_out.ready := stat.state === waiting_for_stddev //当统计单元处于waiting_for_stddev状态时，准备接收平方根结果

    // TODO this fallback for stddev === 0 only works if acc_t is an SInt
    assert(acc_t.isInstanceOf[SInt])

    when (stat.state === waiting_for_stddev) { //如果状态为waiting_for_stddev，检查输出结果。如果结果为零（可能由于精度问题导致），则用1替代
      stat.inv_stddev := Mux(sqrt_out.bits.asUInt === acc_t.zero.asUInt,
        1.S(acc_t.getWidth.W).asTypeOf(acc_t),
        sqrt_out.bits
      )
    }
  }

  //计算标准差的倒数
  val stddev_to_inv_id = MuxCase((num_stats-1).U,        // 通过MuxCase选择当前需要进行标准差倒数计算的统计单元ID
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_inv_stddev) -> i.U }
  )
  val stddev_to_inv = stats(stddev_to_inv_id).inv_stddev // 从选定的统计单元中提取的inv_stddev字段，这里它表示需要计算倒数的标准差
  val (reciprocal_in, reciprocal_out) = stddev_to_inv.reciprocal(scale_t).get

  {
    // Reciprocal input
    val stat = stats(stddev_to_inv_id)

    reciprocal_in.valid := stat.state === get_inv_stddev
    reciprocal_in.bits := DontCare
  }

  {
    // Reciprocal output
    val waiting_for_reciprocal_id = MuxCase((num_stats-1).U, //选择当前等待标准差倒数结果的统计单元ID
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_inv_stddev) -> i.U }
    )
    val stat = stats(waiting_for_reciprocal_id)

    reciprocal_out.ready := stat.state === waiting_for_inv_stddev //当统计单元处于waiting_for_inv_stddev状态时，准备接收倒数结果

    when (stat.state === waiting_for_inv_stddev) {                //如果状态为waiting_for_inv_stddev，将倒数计算结果赋值给stat.inv_stddev
      stat.inv_stddev := reciprocal_out.bits.asTypeOf(stat.inv_stddev)
    }
  }

  //计算指数和的倒数
  val sum_exp_to_inv_id = MuxCase((num_stats-1).U,        // 使用MuxCase选择当前需要进行指数和倒数计算的统计单元ID
    stats.zipWithIndex.map { case (s,i) =>
      (s.state === get_inv_sum_exp) -> i.U }
  )
  val sum_exp_to_inv = stats(sum_exp_to_inv_id).sum       // 从选定的统计单元中提取的sum字段，这里它表示需要计算倒数的指数和
  val exp_divider_in = Wire(Decoupled(UInt(0.W)))         // 用于指数和倒数计算的输入接口
  val exp_divider_out = Wire(Decoupled(scale_t.cloneType))// 用于指数和倒数计算的输出接口

  scale_t match {
    case Float(expWidth, sigWidth) =>

      exp_divider_in.bits := DontCare

      // We translate our integer to floating-point form so that we can use the hardfloat divider
      //将整数转换为浮点格式，以便使用浮点除法器
      def in_to_float(x: SInt) = {
        val in_to_rec_fn = Module(new INToRecFN(intWidth = sum_exp_to_inv.getWidth, expWidth, sigWidth))
        in_to_rec_fn.io.signedIn := true.B
        in_to_rec_fn.io.in := x.asUInt
        in_to_rec_fn.io.roundingMode := consts.round_near_even // consts.round_near_maxMag
        in_to_rec_fn.io.detectTininess := consts.tininess_afterRounding

        in_to_rec_fn.io.out
      }

      val self_rec = in_to_float(sum_exp_to_inv.asUInt.asSInt)// 将sum_exp_to_inv转换为浮点数
      val one_rec = in_to_float(127.S)                        // softmax maximum is 127 for signed int8

      // Instantiate the hardloat divider
      val divider = Module(new DivSqrtRecFN_small(expWidth, sigWidth, 0))

      exp_divider_in.ready := divider.io.inReady
      divider.io.inValid := exp_divider_in.valid
      divider.io.sqrtOp := false.B // 表示执行除法操作而不是平方根操作
      divider.io.a := one_rec      // 这是除法的分子（通常是1或其他常数），用来计算倒数
      divider.io.b := self_rec     // 这是除法的分母，即需要计算倒数的指数和
      divider.io.roundingMode := consts.round_near_even
      divider.io.detectTininess := consts.tininess_afterRounding

      exp_divider_out.valid := divider.io.outValid_div
      exp_divider_out.bits := fNFromRecFN(expWidth, sigWidth, divider.io.out).asTypeOf(scale_t)
  }


  {
    // Divider input
    val stat = stats(sum_exp_to_inv_id)

    exp_divider_in.valid := (stat.state === get_inv_sum_exp) && !lanes.io.busy
    exp_divider_in.bits := sum_exp_to_inv.asUInt
  }

  {
    // Divider output
    val waiting_for_divide_id = MuxCase((num_stats-1).U, // 选择当前等待指数和倒数结果的统计单元ID
      stats.zipWithIndex.map { case (s,i) =>
        (s.state === waiting_for_inv_sum_exp) -> i.U }
    )
    val stat = stats(waiting_for_divide_id)

    exp_divider_out.ready := stat.state === waiting_for_inv_sum_exp

    when (stat.state === waiting_for_inv_sum_exp) {
      stat.inv_sum_exp := exp_divider_out.bits.asTypeOf(stat.inv_sum_exp) //将接收到的倒数结果转换并赋值给stat.inv_sum_exp
    }
  }

  // State transitions
  for (((stat, next_state), id) <- (stats zip next_states).zipWithIndex) {
    val state = stat.state
    val cmd = stat.cmd

    val done = done_with_functional_units(id)

    when (state === idle) {
      // We have a different "when" statement below to support the case where a new row is input into the normalizer
      next_state := idle
      done := DontCare
    }.elsewhen(state === output) {
      next_state := Mux(io.out.fire() && out_stats_id === id.U, idle, state) // 当输出有效且当前统计单元ID匹配时，转到idle状态
      done := io.out.fire() && out_stats_id === id.U
    }.elsewhen(state === get_max) {
      // 判断是否是最后一个输入组
      val is_last_lane_input = stat.vec_groups_left === 0.U || // 检查当前统计单元是否已经没有剩余的向量组需要处理
        (stat.vec_groups_left === 1.U &&                       // 检查是否只剩下一个向量组
          max_lanes.io.ins.bits.stats_id === id.U &&           // 确认当前处理的输入属于这个统计单元
          max_lanes.io.ins.fire())                             // 确认输入数据已经被成功接收（即有效且准备好）

      next_state := Mux(
        is_last_lane_input,
        MuxCase(state, Seq(                                                   // 如果is_last_lane_input为true，使用MuxCase选择下一个状态
          (cmd === NormCmd.MAX) -> idle,                                      // 如果命令是MAX，下一个状态为idle
          (cmd === NormCmd.SUM_EXP || cmd === NormCmd.INV_SUM_EXP) -> get_sum // 如果命令是SUM_EXP或INV_SUM_EXP，下一个状态为get_sum
        )),
        state               // 如果is_last_lane_input为false，保持当前状态state
      )

      done := is_last_lane_input && cmd === NormCmd.MAX // 这意味着在处理完最后一个输入组并且命令是MAX时，操作完成
    }.elsewhen(state === get_sum) {
      val is_last_lane_input = stat.vec_groups_left === 0.U ||
        (stat.vec_groups_left === 1.U &&
          lanes.io.ins.bits.stats_id === id.U &&
          lanes.io.ins.fire())

      next_state := Mux(
        is_last_lane_input,
        MuxCase(state, Seq(
          (cmd === NormCmd.SUM || cmd === NormCmd.VARIANCE || cmd === NormCmd.SUM_EXP) -> idle,
          (cmd === NormCmd.MEAN) -> get_mean,
          (cmd === NormCmd.INV_STDDEV) -> get_variance,
          (cmd === NormCmd.INV_SUM_EXP) -> get_inv_sum_exp,
        )),
        state
      )
//      next_state := Mux(cmd === NormCmd.SUM || cmd === NormCmd.VARIANCE,
//        Mux(is_last_lane_input, idle, state),
//        Mux(is_last_lane_input,
//          Mux(cmd === NormCmd.MEAN, get_mean, get_variance),
//          state)
//      )

      done := is_last_lane_input && cmd =/= NormCmd.MEAN && cmd =/= NormCmd.INV_STDDEV && cmd =/= NormCmd.INV_SUM_EXP
    }.elsewhen(state === get_mean || state === get_variance) {
      next_state := Mux(divider_in.fire() && sum_to_divide_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_mean) {
      next_state := Mux(divider_out.fire(), idle, state)
      done := divider_out.fire()
    }.elsewhen(state === waiting_for_variance) {
      next_state := Mux(divider_out.fire(), get_stddev, state)
      done := false.B
    }.elsewhen(state === get_stddev) {
      next_state := Mux(sqrt_in.fire() && variance_to_sqrt_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_stddev) {
      next_state := Mux(sqrt_out.fire(), state.next, state)
      done := false.B
    }.elsewhen(state === get_inv_stddev) {
      next_state := Mux(reciprocal_in.fire() && stddev_to_inv_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_inv_stddev) {
      next_state := Mux(reciprocal_out.fire(), idle, state)
      done := reciprocal_out.fire()
    }.elsewhen(state === get_inv_sum_exp) {
      next_state := Mux(exp_divider_in.fire() && sum_exp_to_inv_id === id.U, state.next, state)
      done := false.B
    }.elsewhen(state === waiting_for_inv_sum_exp) {
      next_state := Mux(exp_divider_out.fire(), idle, state)
      done := exp_divider_out.fire()
    }.otherwise {
      assert(false.B, "invalid state in Normalizer")
      next_state := DontCare
      done := DontCare
    }

    // 当有新的输入数据到达时，状态机如何根据输入命令来更新状态和数据
    when (io.in.fire() && in_stats_id === id.U) {                 // 检查当前处理的统计单元ID是否与输入数据的统计ID匹配
      next_state := Mux(io.in.bits.cmd === NormCmd.RESET, output, // 如果输入命令是RESET，则将下一个状态设置为output
        Mux(io.in.bits.cmd === NormCmd.MAX, get_max, get_sum))    // 如果输入命令是MAX，则将下一个状态设置为get_max; 如果输入命令不是RESET或MAX，则默认下一个状态为get_sum
      when (io.in.bits.cmd === NormCmd.SUM_EXP) {                 // 检查输入命令是否为SUM_EXP
        stat.max := stat.running_max                              // 将当前的running_max（运行过程中计算的最大值）赋值给stat.max
      }
    }
  }

  // Update stats variables
  for (((stat, next_state), id) <- (stats zip next_states).zipWithIndex) {
    val state = stat.state

    val reset_running_state =                                   // 用于判断是否需要重置运行中的状态
      state === output ||                                       // 如果当前状态是output，则需要重置
        (state === get_mean && next_state =/= get_mean) ||      // 如果从get_mean状态转移到其他状态，则需要重置
        (state === get_variance && next_state =/= get_variance) // 如果从get_variance状态转移到其他状态，则需要重置

    val is_input = io.in.fire() && in_stats_id === id.U

    when (is_input) {
      stat.req := io.in.bits                    // 将输入数据请求赋值给当前统计单元
      stat.count := stat.count + io.in.bits.len // 更新统计单元的计数，增加输入数据的长度？？
      stat.elems_left := io.in.bits.len         // 更新剩余元素数为输入数据的长度？？
    }

    // 重置运行状态
    when(reset_running_state) {
      stat.sum := acc_t.zero                           // 重置累加和为零
      stat.count := Mux(is_input, io.in.bits.len, 0.U) // 如果有输入，重置计数为输入长度，否则为零
    }

    // 更新运行最大值
    when (state =/= get_inv_sum_exp && next_state === get_inv_sum_exp) {
      stat.running_max := acc_t.minimum // 当从非get_inv_sum_exp状态转移到get_inv_sum_exp状态时，重置运行最大值为最小值
    }
  }

  dontTouch(stats)

  // Assertions
  assert(PopCount(stats.map(s => s.state === waiting_for_mean || s.state === waiting_for_variance)) <= 1.U, "we don't support pipelining the divider/sqrt-unit/inv-unit right now")
  assert(PopCount(stats.map(_.state === waiting_for_stddev)) <= 1.U, "we don't support pipelining the divider/sqrt-unit/inv-unit right now")
  assert(PopCount(stats.map(_.state === waiting_for_inv_stddev)) <= 1.U, "we don't support pipelining the divider/sqrt-unit/inv-unit right now")
  // assert(PopCount(stats.map(_.state === output)) <= 1.U, "multiple outputs at same time")
  assert(acc_t.getWidth == scale_t.getWidth, "we use the same variable to hold both the variance and the inv-stddev, so we need them to see the width")

  // Resets
  when (reset.asBool) {
    stats.foreach(_.state := idle)
    stats.foreach(_.sum := acc_t.zero)
    stats.foreach(_.max := acc_t.minimum)
    stats.foreach(_.running_max := acc_t.minimum)
    stats.foreach(_.count := 0.U)
    stats.foreach(_.inv_sum_exp := acc_t.zero)
  }
}

object Normalizer {
  def apply[T <: Data, U <: Data](is_passthru: Boolean, max_len: Int, num_reduce_lanes: Int, num_stats: Int,
                                  latency: Int, fullDataType: Vec[Vec[T]], scale_t: U)(implicit ev: Arithmetic[T]):
  (DecoupledIO[NormalizedInput[T,U]], DecoupledIO[NormalizedOutput[T,U]]) = {
    if (is_passthru) {
      passthru(max_len = max_len, num_stats = num_stats, fullDataType = fullDataType, scale_t = scale_t)
    } else {
      gen(max_len = max_len, num_reduce_lanes = num_reduce_lanes, num_stats = num_stats, latency = latency,
        fullDataType = fullDataType, scale_t = scale_t)
    }
  }

  def gen[T <: Data, U <: Data](max_len: Int, num_reduce_lanes: Int, num_stats: Int, latency: Int,
                                  fullDataType: Vec[Vec[T]], scale_t: U)(implicit ev: Arithmetic[T]): (DecoupledIO[NormalizedInput[T,U]], DecoupledIO[NormalizedOutput[T,U]]) = {
    val norm_unit_module = Module(new Normalizer(max_len, num_reduce_lanes, num_stats, latency, fullDataType, scale_t))
    (norm_unit_module.io.in, norm_unit_module.io.out)
  }

  def passthru[T <: Data, U <: Data](max_len: Int, num_stats: Int, fullDataType: Vec[Vec[T]], scale_t: U)
                                    (implicit ev: Arithmetic[T]): (DecoupledIO[NormalizedInput[T,U]], DecoupledIO[NormalizedOutput[T,U]]) = {

    val norm_unit_passthru_q = Module(new Queue(new NormalizedInput[T,U](max_len, num_stats, fullDataType, scale_t), 2))
    val norm_unit_passthru_out = Wire(Decoupled(new NormalizedOutput(fullDataType, scale_t)))

    norm_unit_passthru_out.valid := norm_unit_passthru_q.io.deq.valid
    norm_unit_passthru_out.bits.acc_read_resp := norm_unit_passthru_q.io.deq.bits.acc_read_resp
    norm_unit_passthru_out.bits.mean := DontCare
    norm_unit_passthru_out.bits.max := DontCare
    norm_unit_passthru_out.bits.inv_stddev := DontCare
    norm_unit_passthru_out.bits.inv_sum_exp := DontCare

    norm_unit_passthru_q.io.deq.ready := norm_unit_passthru_out.ready

    (norm_unit_passthru_q.io.enq, norm_unit_passthru_out)
  }
}
