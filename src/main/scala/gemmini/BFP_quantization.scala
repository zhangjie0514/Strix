package gemmini

import chisel3._
import chisel3.util._
import Util._
import breeze.numerics.exp

class BFPQuantizer_From_FP32(val blockSize: Int, val mantissaBits: Int) extends Module {
  val io = IO(new Bundle {
    val data_in  = Input(Vec(blockSize, UInt(32.W)))         // 输入为 IEEE754 的 FP32 原始位表示
    val mantissa = Output(Vec(blockSize, SInt(7.W)))         // 输出为量化后尾数
    val exponent = Output(UInt(8.W))                         // 输出共享指数（8-bit）
  })

  // Helper function to extract exponent and mantissa
  def extractFloatParts(in: UInt): (UInt, UInt, UInt) = {
    val sign     = in(31)
    val exponent = in(30, 23)
    val mantissa = in(22, 0)
    (sign, exponent, mantissa)
  }

  // STEP 1: 计算每个元素的绝对值
  val absVals = Wire(Vec(blockSize, UInt(32.W)))
  for (i <- 0 until blockSize) {
    absVals(i) := Mux(io.data_in(i)(31) === 1.U, (~io.data_in(i) + 1.U)(31, 0), io.data_in(i))
  }

  // STEP 2: 找出最大绝对值
  val maxVal = absVals.reduceTree((a, b) => Mux(a > b, a, b))

  // STEP 3: 提取最大值的指数
  val (_, maxExp, _) = extractFloatParts(maxVal)
  val sharedExp      = Wire(UInt(8.W))
  sharedExp         := maxExp

  // STEP 4: 计算缩放因子 scale = 2^(sharedExp - (mantissaBits - 1))
  val scaleExp       = Wire(SInt(32.W))
  scaleExp          := sharedExp.zext - (mantissaBits - 1).S 
  val scale          = Wire(UInt(32.W))
    when (scaleExp  >= 0.S) {
      scale         := 1.U << scaleExp.asUInt    // 正数指数 -> 左移
    } .otherwise {
      scale         := 1.U >> (-scaleExp).asUInt // 负数指数 -> 右移，这里是错误的
    }
      
  // STEP 5: 对每个数除以 scale，round，clip，得到量化后尾数
  for (i <- 0 until blockSize) {
    val inFloatBits  = io.data_in(i)
    val sign         = inFloatBits(31)
    val exponent     = inFloatBits(30, 23)
    val mantissa     = inFloatBits(22, 0) | (1.U << 23)   // 补上隐藏位
 
    val shiftAmt     = exponent.zext - scaleExp
    val shifted      = (mantissa << 8).asSInt >> shiftAmt.asUInt
 
    val rounded      = (shifted + 128.S) >> 8             // 模拟 round
    val clipped      = Mux(rounded > 127.S, 127.S, Mux(rounded < -128.S, -128.S, rounded))

    io.mantissa(i)  := clipped
  }

  io.exponent       := sharedExp
}

class BFPQuantizer_From_FP16(val blockSize: Int) extends Module {
  val io = IO(new Bundle {
    val data_in  = Input (Vec(blockSize, UInt(16.W)))        // 输入为 IEEE754 的 FP32 原始位表示
    val mantissa = Output(Vec(blockSize, UInt(7.W)))         // 输出为量化后尾数
    val sign     = Output(Vec(blockSize, UInt(1.W)))         // 输出为原先符号位
    val exponent = Output(UInt(5.W))                         // 输出共享指数（5-bit）
  })

  dontTouch(io.mantissa)
  dontTouch(io.sign)
  dontTouch(io.exponent)

  // 拆分fp16
  def extractFloatParts(in: UInt): (UInt, UInt, UInt) = {
    val sign     = in(15)
    val exponent = in(14, 10)
    val mantissa = in(9, 0)
    (sign, exponent, mantissa)
  }

  val sign_in     = io.data_in.map {in =>in(15)}      
  val exp_in      = io.data_in.map {in =>in(14, 10)}  
  val mantissa_in = io.data_in.map {in =>in(9, 0)}    
  val exp_in_max  = exp_in.reduce(_ max _)            

  val shift       = Wire(Vec(blockSize, UInt(5.W)))
  for (i <- 0 until blockSize){
    shift(i)       := exp_in_max - exp_in(i)
    io.mantissa(i) := Cat(1.U, mantissa_in(i)(9, 1)) >> shift(i)
  }

  io.exponent := exp_in_max + 1.U
  io.sign     := sign_in
}