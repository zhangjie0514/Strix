package gemmini

import chisel3._
import chisel3.util._
import Util._

/**
  * A Tile is a purely combinational 2D array of passThrough PEs.
  * a, b, s, and in_propag are broadcast across the entire array and are passed through to the Tile's outputs
  * @param width The data width of each PE in bits
  * @param rows Number of PEs on each row
  * @param columns Number of PEs on each column
  */
class Tile_BIST[T <: Data](inputType: T, outputType: T, accType: T, df: Dataflow.Value, tree_reduction: Boolean, max_simultaneous_matmuls: Int, val rows: Int, val columns: Int)(implicit ev: Arithmetic[T]) extends Module {
  val io = IO(new Bundle {
    val in_a         = Input (Vec(rows, inputType))
    val in_b         = Input (Vec(columns, outputType)) // This is the output of the tile next to it
    val in_d         = Input (Vec(columns, outputType))
    val in_control   = Input (Vec(columns, new PEControl(accType)))
    val in_id        = Input (Vec(columns, UInt(log2Up(max_simultaneous_matmuls).W)))
    val in_last      = Input (Vec(columns, Bool()))
    val out_a        = Output(Vec(rows, inputType))
    val out_c        = Output(Vec(columns, outputType))
    val out_b        = Output(Vec(columns, outputType))
    val out_control  = Output(Vec(columns, new PEControl(accType)))
    val out_id       = Output(Vec(columns, UInt(log2Up(max_simultaneous_matmuls).W))) 
    val out_last     = Output(Vec(columns, Bool()))
    val in_valid     = Input (Vec(columns, Bool())) 
    val out_valid    = Output(Vec(columns, Bool()))
    val bad_dataflow = Output(Bool())
    val bist_control = Input (UInt(2.W)) // 自检流控制信号
    // 00表示正常计算，部分和寄存器之间不会之间相连，乘加器会连接上一个pe的部分和寄存器；
    // 01表示每个pe各自独立进行计算，部分和寄存器不会直接相连，乘加器也不连接上一个pe的部分和寄存器；
    // 10表示输运各自计算得到的部分和，部分和寄存器之间会直接相连，这时乘加器怎么连接无所谓
  })

  import ev._

  val tile = Seq.fill(rows, columns)(Module(new PE_BIST(inputType, outputType, accType, df, max_simultaneous_matmuls)))
  val tileT = tile.transpose

  //每一行中PE的out_a接到下一个PE的in_a上
  for (r <- 0 until rows) {
    tile(r).foldLeft(io.in_a(r)) {
      case (in_a, pe) =>
        pe.io.in_a := in_a
        pe.io.out_a
    }
  }

  //每一列中PE的out_b接到下一个PE的in_b上;tree_reduction为1时置零
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_b(c)) {
      case (in_b, pe) =>
        pe.io.in_b := (if (tree_reduction) in_b.zero else in_b)
        pe.io.out_b
    }
  }

  //每一列中PE的out_c接到下一个PE的in_d上
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_d(c)) {
      case (in_d, pe) =>
        pe.io.in_d := in_d
        pe.io.out_c
    }
  }

  //control按列连接
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_control(c)) {
      case (in_ctrl, pe) =>
        pe.io.in_control := in_ctrl
        pe.io.out_control
    }
  }

  //valid按列连接
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_valid(c)) {
      case (v, pe) =>
        pe.io.in_valid := v
        pe.io.out_valid
    }
  }

  //id按列连接
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_id(c)) {
      case (id, pe) =>
        pe.io.in_id := id
        pe.io.out_id
    }
  }

  //last按列连接
  for (c <- 0 until columns) {
    tileT(c).foldLeft(io.in_last(c)) {
      case (last, pe) =>
        pe.io.in_last := last
        pe.io.out_last
    }
  }

  // bist信号赋值
  for(i <- 0 until rows){
    for(j <- 0 until columns){
        tile(i)(j).io.bist_control := io.bist_control
    }
  }

  // Drive the Tile's bottom IO
  for (c <- 0 until columns) {
    io.out_c(c) := tile(rows-1)(c).io.out_c
    io.out_control(c) := tile(rows-1)(c).io.out_control
    io.out_id(c) := tile(rows-1)(c).io.out_id
    io.out_last(c) := tile(rows-1)(c).io.out_last
    io.out_valid(c) := tile(rows-1)(c).io.out_valid

    io.out_b(c) := {
      if (tree_reduction) {
        val prods = tileT(c).map(_.io.out_b)
        accumulateTree(prods :+ io.in_b(c))
      } else {
        tile(rows - 1)(c).io.out_b
      }
    }
  }
  io.bad_dataflow := tile.map(_.map(_.io.bad_dataflow).reduce(_||_)).reduce(_||_)

  // Drive the Tile's right IO
  for (r <- 0 until rows) {
    io.out_a(r) := tile(r)(columns-1).io.out_a
  }
}
