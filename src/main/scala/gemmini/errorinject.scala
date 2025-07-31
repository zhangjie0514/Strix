// package gemmini

// import chisel3._
// import chisel3.util._
// import scala.math.exp

// class FaultController(numUnits: Int, clockFreq: Int) extends Module {
//   val io = IO(new Bundle {
//     // 配置接口
//     val targetFaultRate = Input(UInt(32.W)) // 每百万周期故障数 (ppm)
//     // 监控输出
//     val actualFaultRate = Output(UInt(32.W))
//     // 故障注入控制
//     val faultEnable = Output(Vec(numUnits, Bool()))
//   })

//   // 伪随机数生成器 (32位 LFSR)
//   val lfsr = RegInit(1.U(32.W))
//   lfsr := Cat(lfsr(0)^lfsr(1)^lfsr(21)^lfsr(31), lfsr(31,1))

//   // 故障率计算
//   val cyclesPerMillion = (clockFreq * 1000000).U
//   val faultProbability = RegInit(0.U(32.W))
  
//   // 故障率控制逻辑
//   val cycleCounter = RegInit(0.U(32.W))
//   val faultCounter = RegInit(0.U(32.W))
  
//   // 每百万周期更新故障率
//   when(cycleCounter === cyclesPerMillion - 1.U) {
//     faultProbability := io.targetFaultRate
//     cycleCounter := 0.U
//     faultCounter := 0.U
//   }.otherwise {
//     cycleCounter := cycleCounter + 1.U
//   }

//   // 生成单元故障使能信号
//   val unitFaultEnable = Wire(Vec(numUnits, Bool()))
  
//   for (i <- 0 until numUnits) {
//     // 使用LFSR的不同位段实现独立随机性
//     val unitRandom = lfsr(i*3+2, i*3)
    
//     // 指数分布故障间隔 (硬件友好的近似)
//     val faultThreshold = Wire(UInt(32.W))
//     when(faultProbability > 0.U) {
//       faultThreshold := (1000000.U / faultProbability) // 平均故障间隔
//     }.otherwise {
//       faultThreshold := 0xFFFFFFFF.U // 无故障
//     }
    
//     // 生成故障事件
//     val faultInterval = RegInit(0.U(32.W))
//     val faultActive = RegInit(false.B)
    
//     when(faultInterval === 0.U) {
//       // 新故障事件
//       faultActive := true.B
//       // 指数分布随机间隔
//       faultInterval := Mux(faultThreshold > 0.U, 
//                          (faultThreshold * unitRandom) >> 2, 
//                          0xFFFFFFFF.U)
//       faultCounter := faultCounter + 1.U
//     }.otherwise {
//       faultActive := false.B
//       faultInterval := faultInterval - 1.U
//     }
    
//     unitFaultEnable(i) := faultActive
//   }

//   io.faultEnable := unitFaultEnable
//   io.actualFaultRate := Mux(cycleCounter === 0.U, 0.U, 
//                           (faultCounter * 1000000.U) / cycleCounter)
// }

// class FaultInjector(dataWidth: Int) extends Module {
//   val io = IO(new Bundle {
//     val dataIn = Input(UInt(dataWidth.W))
//     val dataOut = Output(UInt(dataWidth.W))
//     val faultEnable = Input(Bool())
//   })
  
//   // 随机故障类型
//   val faultType = RegInit(0.U(2.W))
//   val lfsr = RegInit(1.U(16.W))
//   lfsr := Cat(lfsr(0)^lfsr(2)^lfsr(3)^lfsr(5), lfsr(15,1))
  
//   when(io.faultEnable) {
//     faultType := lfsr(1,0) // 随机选择故障类型
//   }
  
//   // 故障注入逻辑
//   io.dataOut := io.dataIn // 默认直通
  
//   when(io.faultEnable) {
//     switch(faultType) {
//       is(0.U) { // 单比特翻转
//         val flipBit = lfsr(log2Ceil(dataWidth)-1, 0) % dataWidth.U
//         io.dataOut := io.dataIn ^ (1.U << flipBit)
//       }
//       is(1.U) { // 数据全翻转
//         io.dataOut := ~io.dataIn
//       }
//       is(2.U) { // 固定值故障
//         io.dataOut := "hDEADBEEF".U(dataWidth.W)
//       }
//       is(3.U) { // 随机值故障
//         io.dataOut := lfsr(dataWidth-1, 0)
//       }
//     }
//   }
// }

// // 示例：集成到处理器模块
// class ProcessorCore extends Module {
//   val io = IO(new Bundle {
//     // 处理器接口
//   })
  
//   // 实例化故障控制器 (假设10个关键单元)
//   val faultCtrl = Module(new FaultController(10, 100)) // 100MHz时钟
  
//   // 配置目标故障率 (例如 1000 ppm)
//   faultCtrl.io.targetFaultRate := 1000.U
  
//   // 关键单元列表
//   val criticalUnits = List(
//     Module(new RegisterFile), 
//     Module(new ALU),
//     Module(new CacheController),
//     // ...其他单元
//   )
  
//   // 为每个单元添加故障注入器
//   val faultInjectors = criticalUnits.map { unit =>
//     val injector = Module(new FaultInjector(unit.dataWidth))
//     injector.io.dataIn := unit.io.dataOut
//     unit.io.dataIn := injector.io.dataOut
//     injector
//   }
  
//   // 连接故障使能信号
//   for ((injector, i) <- faultInjectors.zipWithIndex) {
//     injector.io.faultEnable := faultCtrl.io.faultEnable(i)
//   }
  
//   // 监控实际故障率
//   val monitor = faultCtrl.io.actualFaultRate
// }
