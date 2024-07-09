package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class PS2IO extends Bundle {
  val clk = Input(Bool())
  val data = Input(Bool())
}

class PS2CtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(
    new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
  )
  val ps2 = new PS2IO
}

class ps2_top_apb extends BlackBox {
  val io = IO(new PS2CtrlIO)
}

class ps2Chisel extends Module {
  val io = IO(new PS2CtrlIO)

  val ps2_clk_sync = RegInit(0.U(3.W))
  ps2_clk_sync := Cat(ps2_clk_sync(1, 0), io.ps2.clk)

  val buffer = Reg(Vec(10, Bool()))
  val counter = RegInit(0.U(4.W))

  val fifo = Reg(Vec(8, UInt(8.W)))
  val w_ptr = RegInit(0.U(3.W))
  val r_ptr = RegInit(0.U(3.W))

  val ready_reg = RegInit(false.B)
  val overflow_reg = dontTouch(RegInit(false.B))

  val ps2_clk_sample = ps2_clk_sync(2) & ~ps2_clk_sync(1)
  when(ps2_clk_sample) {
    when(counter === 10.U) {
      when(~buffer(0) && buffer.asUInt(9, 1).xorR && io.ps2.data) {
        fifo(w_ptr) := buffer.asUInt(8, 1)
        w_ptr := w_ptr + 1.U
        overflow_reg := overflow_reg | (ready_reg && w_ptr === r_ptr)
        ready_reg := true.B
      }
      counter := 0.U
    }.otherwise {
      buffer(counter) := io.ps2.data
      counter := counter + 1.U
    }
  }

  io.in.pready := io.in.psel && io.in.penable && ~io.in.pwrite
  io.in.prdata := Mux(ready_reg, fifo(r_ptr), 0.U)
  io.in.pslverr := false.B
  when(io.in.pready && ready_reg) {
    r_ptr := r_ptr + 1.U
    when(r_ptr + 1.U === w_ptr) {
      ready_reg := false.B
    }
  }
}

class APBKeyboard(address: Seq[AddressSet])(implicit p: Parameters)
    extends LazyModule {
  val node = APBSlaveNode(
    Seq(
      APBSlavePortParameters(
        Seq(
          APBSlaveParameters(
            address = address,
            executable = true,
            supportsRead = true,
            supportsWrite = true
          )
        ),
        beatBytes = 4
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val ps2_bundle = IO(new PS2IO)

    val mps2 = Module(new ps2Chisel)
    mps2.io.clock := clock
    mps2.io.reset := reset
    mps2.io.in <> in
    ps2_bundle <> mps2.io.ps2
  }
}
