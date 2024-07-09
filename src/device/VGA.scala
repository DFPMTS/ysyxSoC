package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class VGAIO extends Bundle {
  val r = Output(UInt(8.W))
  val g = Output(UInt(8.W))
  val b = Output(UInt(8.W))
  val hsync = Output(Bool())
  val vsync = Output(Bool())
  val valid = Output(Bool())
}

class VGACtrlIO extends Bundle {
  val clock = Input(Clock())
  val reset = Input(Bool())
  val in = Flipped(
    new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
  )
  val vga = new VGAIO
}

class vgaFrameBuffer extends Module {
  val io = IO(new Bundle {
    val raddr = Input(UInt(24.W))
    val waddr = Input(UInt(24.W))
    val wdata = Input(UInt(32.W))
    val rdata = Output(UInt(32.W))
    val wen = Input(Bool())
    val ren = Input(Bool())
  })

  val mem = SyncReadMem(640 * 480, UInt(32.W))
  val forward_wdata = RegNext(io.wdata)
  val forward_en = RegNext(io.wen && io.ren && io.waddr === io.raddr)
  val read_val = mem.read(io.raddr, io.ren)
  when(io.wen) {
    mem.write(io.waddr, io.wdata)
  }

  io.rdata := Mux(forward_en, forward_wdata, read_val)
}

class vga_top_apb extends BlackBox {
  val io = IO(new VGACtrlIO)
}

class vgaChisel extends Module {
  val io = IO(new VGACtrlIO)

  val vga_mem = Module(new vgaFrameBuffer)
  vga_mem.io.waddr := io.in.paddr(25, 2)
  vga_mem.io.wdata := io.in.pwdata
  val w_req = io.in.psel && io.in.penable && io.in.pwrite
  vga_mem.io.wen := w_req
  io.in.pready := w_req
  io.in.prdata := 0.U
  io.in.pslverr := false.B

  val HTotal = 800.U(10.W)
  val HFrontPorch = 144.U(10.W)
  val HS = 96.U(10.W)

  val VTotal = 525.U(10.W)
  val VFrontPorch = 35.U(10.W)
  val VS = 2.U(10.W)

  val x_cnt = RegInit(0.U(10.W))
  val y_cnt = RegInit(0.U(10.W))

  x_cnt := Mux(x_cnt === HTotal - 1.U, 0.U, x_cnt + 1.U)
  y_cnt := Mux(
    x_cnt === (HTotal - 1.U),
    Mux(y_cnt === (VTotal - 1.U), 0.U, y_cnt + 1.U),
    y_cnt
  )

  val x_valid = x_cnt >= HFrontPorch && x_cnt < 640.U + HFrontPorch
  val y_valid = y_cnt >= VFrontPorch && y_cnt < 480.U + VFrontPorch
  val xy_valid = x_valid && y_valid
  val mem_x = Mux(x_valid, x_cnt - HFrontPorch, 0.U)
  val mem_y = Mux(y_valid, y_cnt - VFrontPorch, 0.U)
  vga_mem.io.raddr := mem_y * 640.U + mem_x
  vga_mem.io.ren := xy_valid

  io.vga.hsync := x_cnt >= HS
  io.vga.vsync := y_cnt >= VS
  io.vga.valid := xy_valid
  io.vga.r := vga_mem.io.rdata(23, 16)
  io.vga.g := vga_mem.io.rdata(15, 8)
  io.vga.b := vga_mem.io.rdata(7, 0)
}

class APBVGA(address: Seq[AddressSet])(implicit p: Parameters)
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
    val vga_bundle = IO(new VGAIO)

    val mvga = Module(new vgaChisel)
    mvga.io.clock := clock
    mvga.io.reset := reset
    mvga.io.in <> in
    vga_bundle <> mvga.io.vga
  }
}
