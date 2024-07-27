package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.util._
import freechips.rocketchip.regmapper.RRTest0Map.ar

class AXI4DataWidthConverter64to32IO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(
    new AXI4Bundle(
      AXI4BundleParameters(addrBits = 32, dataBits = 64, idBits = 4)
    )
  )
  val out = new AXI4Bundle(
    AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)
  )
}

class AXI4DataWidthConverter64to32 extends BlackBox {
  val io = IO(new AXI4DataWidthConverter64to32IO)
}

class AXI4DataWidthConverter64to32Chisel extends Module {
  val io = IO(new AXI4DataWidthConverter64to32IO)

  // --- write channel ---
  val wToOut = WireDefault(io.in.w)
  val wToIn = WireDefault(io.out.w)

  val aw2BitReg = RegEnable(io.in.aw.bits.addr(2), io.out.aw.fire)
  val aw2Bit = Mux(io.in.aw.valid, io.in.aw.bits.addr(2), aw2BitReg)
  // block w until got aw
  val wToOutValidReg = RegInit(false.B)
  wToOutValidReg := Mux(
    io.in.aw.fire,
    true.B,
    Mux(io.in.w.fire && io.in.w.bits.last, false.B, wToOutValidReg)
  )
  val wToOutValid = io.in.w.valid || wToOutValidReg
  wToOut.valid := Mux(wToOutValid, io.in.w.valid, false.B)
  wToOut.bits.data := Mux(
    aw2Bit,
    io.in.w.bits.data(63, 32),
    io.in.w.bits.data(31, 0)
  )
  wToOut.bits.strb := Mux(
    aw2Bit,
    io.in.w.bits.strb(7, 4),
    io.in.w.bits.strb(3, 0)
  )
  wToIn.ready := io.out.w.ready & wToOutValid

  io.out.aw <> io.in.aw
  io.out.w <> wToOut
  io.in.w <> wToIn
  io.out.b <> io.in.b

  // --- read channel ---
  val rToIn = Wire(io.in.r.cloneType)
  rToIn := io.out.r
  val rToOut = WireDefault(io.in.r)

  rToIn.bits.data := Fill(2, io.out.r.bits.data)
  io.in.ar <> io.out.ar
  io.in.r <> rToIn
  io.out.r <> rToOut
}
