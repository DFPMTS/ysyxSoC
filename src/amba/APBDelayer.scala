package ysyx

import chisel3._
import chisel3.util._
import scala.math.pow

import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.amba._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.regmapper.RRTestCombinational.delay

class APBDelayerIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(
    new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
  )
  val out = new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
}

class apb_delayer extends BlackBox {
  val io = IO(new APBDelayerIO)
}

class APBDelayerChisel extends Module {
  val io = IO(new APBDelayerIO)

  val s = 3
  // calculation: (Freq / 100MHz - 1) * (2 ^ s)
  val freq = 733
  val r = ((freq.toDouble / 100 - 1) * pow(2, s)).toInt

  val sIdle :: sWaitReady :: sDelay :: Nil = Enum(3)
  val state = RegInit(sIdle)
  val delayCycles = Reg(SInt(32.W))
  val lastDelayCycle = delayCycles <= (1.S << s)
  state := MuxLookup(state, sIdle)(
    Seq(
      sIdle -> Mux(io.in.psel, sWaitReady, sIdle),
      sWaitReady -> Mux(io.out.pready, sDelay, sWaitReady),
      sDelay -> Mux(lastDelayCycle, sIdle, sDelay)
    )
  )

  delayCycles := Mux(
    io.in.psel,
    Mux(
      io.out.psel,
      delayCycles + r.S,
      Mux(state === sDelay, delayCycles - (1.S << s), delayCycles)
    ),
    0.S
  )

  val inWire = WireDefault(io.in)
  inWire.psel := io.in.psel && state =/= sDelay
  inWire.penable := io.in.penable && state =/= sDelay
  io.out <> inWire

  val toLatchOut = io.out.psel && io.out.penable && io.out.pready
  val prdataBuffer = RegEnable(io.out.prdata, toLatchOut)
  val pslverrBuffer = RegEnable(io.out.pslverr, toLatchOut)
  val outWire = WireDefault(io.out)
  outWire.prdata := prdataBuffer
  outWire.pslverr := pslverrBuffer
  outWire.pready := state === sDelay && lastDelayCycle
  io.in <> outWire
}

class APBDelayerWrapper(implicit p: Parameters) extends LazyModule {
  val node = APBIdentityNode()

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    (node.in zip node.out) foreach { case ((in, edgeIn), (out, edgeOut)) =>
      val delayer = Module(new APBDelayerChisel)
      delayer.io.clock := clock
      delayer.io.reset := reset
      delayer.io.in <> in
      out <> delayer.io.out
    }
  }
}

object APBDelayer {
  def apply()(implicit p: Parameters): APBNode = {
    val apbdelay = LazyModule(new APBDelayerWrapper)
    apbdelay.node
  }
}
