package ysyx

import chisel3._
import chisel3.util._
import scala.math.pow

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.CSRs.time

class AXI4DelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(
    new AXI4Bundle(
      AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)
    )
  )
  val out = new AXI4Bundle(
    AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)
  )
}

class axi4_delayer extends BlackBox {
  val io = IO(new AXI4DelayerIO)
}

class AXI4DelayerChisel extends Module {
  val io = IO(new AXI4DelayerIO)

  val s = 3
  val freq = 733
  val r = ((freq.toDouble / 100) * pow(2, s)).toInt

  // read channel
  val burstLen = 8
  val rPtr = Reg(UInt(3.W))
  val wPtr = Reg(UInt(3.W))
  val rData = Reg(Vec(burstLen, io.out.r.bits.cloneType))
  val rDelay = Reg(Vec(burstLen, UInt(32.W)))

  val sReadIdle :: sReadDelay :: Nil = Enum(2)
  val state = RegInit(sReadIdle)
  val delayCycles = Reg(UInt(32.W))
  val timer = Reg(UInt(32.W))
  val lastDelayCycle = io.in.r.fire && io.in.r.bits.last
  val valid = (io.in.ar.valid || state === sReadDelay) && !lastDelayCycle
  state := MuxLookup(state, sReadIdle)(
    Seq(
      sReadIdle -> Mux(valid, sReadDelay, sReadIdle),
      sReadDelay -> Mux(
        lastDelayCycle,
        sReadIdle,
        sReadDelay
      )
    )
  )

  rData(wPtr) := Mux(io.out.r.fire, io.out.r.bits, rData(wPtr))
  rDelay(wPtr) := Mux(io.out.r.fire, delayCycles, rDelay(wPtr))

  rPtr := Mux(
    valid,
    Mux(io.in.r.fire, rPtr + 1.U, rPtr),
    0.U
  )

  wPtr := Mux(
    valid,
    Mux(io.out.r.fire, wPtr + 1.U, wPtr),
    0.U
  )

  timer := Mux(
    valid,
    timer + (1.U << s),
    (1.U << s)
  )

  delayCycles := Mux(
    valid,
    delayCycles + r.U,
    r.U
  )

  val toOutAr = WireDefault(io.in.ar)
  val toInAr = WireDefault(io.out.ar)
  io.out.ar <> toOutAr
  io.in.ar <> toInAr

  toInAr.ready := io.out.ar.ready

  val toInR = WireDefault(io.out.r)
  val toOutR = WireDefault(io.in.r)
  io.out.r <> toOutR
  io.in.r <> toInR

  toInR.valid := rPtr < wPtr && rDelay(rPtr) <= timer
  toInR.bits := rData(rPtr)
  toOutR.ready := true.B

  // write channel
  val sWriteIdle :: sWriteDelay :: Nil = Enum(2)
  val writeState = RegInit(sWriteIdle)
  val writeDelayCycles = Reg(UInt(32.W))
  val writeTimer = Reg(UInt(32.W))
  val lastWriteDelayCycle = io.in.b.fire
  val writeValid =
    (io.in.aw.valid || io.in.w.valid || writeState === sWriteDelay) && !lastWriteDelayCycle

  val bDelay = RegEnable(writeDelayCycles, io.out.b.fire)
  val bData = RegEnable(io.out.b.bits, io.out.b.fire)
  val bValid = Reg(Bool())

  writeState := MuxLookup(writeState, sWriteIdle)(
    Seq(
      sWriteIdle -> Mux(writeValid, sWriteDelay, sWriteIdle),
      sWriteDelay -> Mux(
        lastWriteDelayCycle,
        sWriteIdle,
        sWriteDelay
      )
    )
  )

  writeDelayCycles := Mux(
    writeValid,
    writeDelayCycles + r.U,
    r.U
  )

  writeTimer := Mux(
    writeValid,
    writeTimer + (1.U << s),
    (1.U << s)
  )

  bValid := Mux(
    writeValid,
    Mux(io.out.b.fire, true.B, bValid),
    false.B
  )

  val toOutAw = WireDefault(io.in.aw)
  val toInAw = WireDefault(io.out.aw)
  io.out.aw <> toOutAw
  io.in.aw <> toInAw

  toInAw.ready := io.out.aw.ready

  val toInW = WireDefault(io.out.w)
  val toOutW = WireDefault(io.in.w)
  io.out.w <> toOutW
  io.in.w <> toInW

  toInW.ready := io.out.w.ready

  val toInB = WireDefault(io.out.b)
  val toOutB = WireDefault(io.in.b)
  io.out.b <> toOutB
  io.in.b <> toInB

  toInB.valid := bValid && bDelay <= writeTimer
  toInB.bits := bData
  toOutB.ready := true.B
}

class AXI4DelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = AXI4IdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new AXI4DelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object AXI4Delayer {
  def apply()(implicit p: Parameters): AXI4Node = {
    val axi4delay = LazyModule(new AXI4DelayerWrapper)
    axi4delay.node
  }
}
