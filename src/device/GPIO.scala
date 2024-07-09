package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import scala.collection.View.Tabulate

class GPIOIO extends Bundle {
  val out = Output(UInt(16.W))
  val in = Input(UInt(16.W))
  val seg = Output(Vec(8, UInt(8.W)))
}

class GPIOCtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Reset())
  val in = Flipped(
    new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
  )
  val gpio = new GPIOIO
}

class gpio_top_apb extends BlackBox {
  val io = IO(new GPIOCtrlIO)
}

class gpioChisel extends Module {
  val io = IO(new GPIOCtrlIO)
  val addr = io.in.paddr(3, 2)
  val data = io.in.pwdata(15, 0)
  val led = RegInit(VecInit(Seq.fill(2)(0.U(8.W))))
  val seg = RegInit(VecInit(Seq.fill(4)(0.U(8.W))))
  io.in.prdata := 0.U
  when(io.in.psel && io.in.penable) {
    switch(addr) {
      is(0.U) {
        // led
        when(io.in.pwrite) {
          for (i <- 0 until 2) {
            led(i) := Mux(
              io.in.pstrb(i),
              io.in.pwdata((i + 1) * 8 - 1, i * 8),
              led(i)
            )
          }
        }.otherwise {
          io.in.prdata := led.asUInt
        }
      }
      is(1.U) {
        // switch
        when(!io.in.pwrite) {
          io.in.prdata := io.gpio.in
        }
      }
      is(2.U) {
        when(io.in.pwrite) {
          for (i <- 0 until 4) {
            seg(i) := Mux(
              io.in.pstrb(i),
              io.in.pwdata((i + 1) * 8 - 1, i * 8),
              seg(i)
            )
          }
        }.otherwise {
          io.in.prdata := seg.asUInt
        }
      }
    }
  }

  def hex_to_seg(hex: UInt) = {
    val seg = MuxLookup(hex, 0.U(7.W))(
      Seq(
        0.U -> "b1111_110".U,
        1.U -> "b0110_000".U,
        2.U -> "b1101_101".U,
        3.U -> "b1111_001".U,
        4.U -> "b0110_011".U,
        5.U -> "b1011_011".U,
        6.U -> "b1011_111".U,
        7.U -> "b1110_000".U,
        8.U -> "b1111_111".U,
        9.U -> "b1111_011".U,
        10.U -> "b1110_111".U,
        11.U -> "b0011_111".U,
        12.U -> "b1001_110".U,
        13.U -> "b0111_101".U,
        14.U -> "b1001_111".U,
        15.U -> "b1000_111".U
      )
    )
    Cat(~seg, ~0.U(1.W))
  }

  io.gpio.out := Cat(led(1), led(0))
  io.gpio.seg := VecInit.tabulate(8)(i =>
    hex_to_seg(seg.asUInt((i + 1) * 4 - 1, i * 4))
  )
  io.in.pready := io.in.psel && io.in.penable
  io.in.pslverr := false.B
}

class APBGPIO(address: Seq[AddressSet])(implicit p: Parameters)
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
    val gpio_bundle = IO(new GPIOIO)

    val mgpio = Module(new gpioChisel)
    mgpio.io.clock := clock
    mgpio.io.reset := reset
    mgpio.io.in <> in
    gpio_bundle <> mgpio.io.gpio
  }
}
