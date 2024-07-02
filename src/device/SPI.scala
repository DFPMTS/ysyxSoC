package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

class SPIIO(val ssWidth: Int = 8) extends Bundle {
  val sck = Output(Bool())
  val ss = Output(UInt(ssWidth.W))
  val mosi = Output(Bool())
  val miso = Input(Bool())
}

class spi_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val spi = new SPIIO
    val spi_irq_out = Output(Bool())
  })
}

class flash extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class flashXPI extends Module {
  val io = IO(new Bundle {
    val apb_out =
      new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32))
    val apb_in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val flash_req = Input(Bool())
    val spi_intr = Input(Bool())
  })

  val s_Idle :: s_CTRL :: s_Tx :: s_DIVIDER :: s_SS :: s_GO_BSY :: s_Wait_Intr :: s_Read_Rx :: Nil =
    Enum(8)
  val state = RegInit(s_Idle)

  /*
      .wb_clk_i(clock),
      .wb_rst_i(reset),
      .wb_adr_i(in_paddr[4:0]),
      .wb_dat_i(in_pwdata),
      .wb_dat_o(in_prdata),
      .wb_sel_i(in_pstrb),
      .wb_we_i (in_pwrite),
      .wb_stb_i(in_psel),
      .wb_cyc_i(in_penable),
      .wb_ack_o(in_pready),
      .wb_err_o(in_pslverr),
      .wb_int_o(spi_irq_out),
   */

  state := MuxLookup(state, s_Idle)(
    Seq(
      s_Idle -> Mux(
        io.flash_req,
        s_CTRL,
        s_Idle
      ),
      s_CTRL -> Mux(
        io.apb_out.pready,
        s_DIVIDER,
        s_CTRL
      ),
      s_DIVIDER -> Mux(
        io.apb_out.pready,
        s_SS,
        s_DIVIDER
      ),
      s_SS -> Mux(
        io.apb_out.pready,
        s_Tx,
        s_SS
      ),
      s_Tx -> Mux(
        io.apb_out.pready,
        s_GO_BSY,
        s_Tx
      ),
      s_GO_BSY -> Mux(
        io.apb_out.pready,
        s_Wait_Intr,
        s_GO_BSY
      ),
      s_Wait_Intr -> Mux(
        io.spi_intr,
        s_Read_Rx,
        s_Wait_Intr
      ),
      s_Read_Rx -> Mux(
        io.apb_out.pready,
        s_Idle,
        s_Read_Rx
      )
    )
  )

  io.apb_out.penable := false.B
  io.apb_out.psel := false.B
  io.apb_out.pwrite := false.B
  io.apb_out.paddr := 0.U
  io.apb_out.pprot := 1.U
  io.apb_out.pwdata := 0.U
  io.apb_out.pstrb := "hF".U

  //                      13 12  11 10  9  8  7  6  :  0
  //                     ASS IE LSB TN RN GO  X  CHAR_LEN
  val SPI_CTRL_FLASH = "b_1___1__0___1__0__0__0__1000000".U
  val SPI_CTRL_GO_BSY = 1.U << 8

  val SPI_BASE = "h10001000".U
  val SPI_Rx0 = 0.U
  val SPI_Tx0 = 0.U
  val SPI_Tx1 = "h4".U
  val SPI_CTRL = "h10".U
  val SPI_DIVIDER = "h14".U
  val SPI_SS = "h18".U
  when(state === s_CTRL) {
    io.apb_out.paddr := SPI_BASE + SPI_CTRL
    io.apb_out.penable := true.B
    io.apb_out.psel := true.B
    io.apb_out.pwrite := true.B
    io.apb_out.pwdata := SPI_CTRL_FLASH;
  }.elsewhen(state === s_DIVIDER) {
    io.apb_out.paddr := SPI_BASE + SPI_DIVIDER
    io.apb_out.penable := true.B
    io.apb_out.psel := true.B
    io.apb_out.pwrite := true.B
    io.apb_out.pwdata := 1.U;
  }.elsewhen(state === s_SS) {
    io.apb_out.paddr := SPI_BASE + SPI_SS
    io.apb_out.penable := true.B
    io.apb_out.psel := true.B
    io.apb_out.pwrite := true.B
    io.apb_out.pwdata := 1.U;
  }.elsewhen(state === s_Tx) {
    io.apb_out.paddr := SPI_BASE + SPI_Tx1
    io.apb_out.penable := true.B
    io.apb_out.psel := true.B
    io.apb_out.pwrite := true.B
    io.apb_out.pwdata := Cat("h03".U(8.W), io.apb_in.paddr(23, 0));
  }.elsewhen(state === s_GO_BSY) {
    io.apb_out.paddr := SPI_BASE + SPI_CTRL
    io.apb_out.penable := true.B
    io.apb_out.psel := true.B
    io.apb_out.pwrite := true.B
    io.apb_out.pwdata := SPI_CTRL_FLASH | SPI_CTRL_GO_BSY
  }.elsewhen(state === s_Read_Rx) {
    io.apb_out.paddr := SPI_Rx0
    io.apb_out.penable := true.B
    io.apb_out.psel := true.B
    io.apb_out.pwrite := false.B
  }

  io.apb_in.pready := (state === s_Read_Rx) && io.apb_out.pready
  io.apb_in.pslverr := false.B
  val flash_data =
    io.apb_out.prdata // big endian, since it's read byte by byte from low to high addr
  io.apb_in.prdata := Cat(
    flash_data(7, 0),
    flash_data(15, 8),
    flash_data(23, 16),
    flash_data(31, 24)
  )
}

class APBSPI(address: Seq[AddressSet])(implicit p: Parameters)
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
    val spi_bundle = IO(new SPIIO)

    val flash_addr_start = "h3000_0000".U
    val flash_addr_end = "h3fff_ffff".U
    val flash_req =
      in.psel && in.penable && (in.paddr >= flash_addr_start && in.paddr <= flash_addr_end)
    val flashXPI = Module(new flashXPI)
    flashXPI.io.flash_req := flash_req
    flashXPI.io.apb_in <> in

    val mspi = Module(new spi_top_apb)
    mspi.io.clock := clock
    mspi.io.reset := reset
    for (apb_master <- List(flashXPI.io.apb_out)) {
      apb_master.prdata := 0.U
      apb_master.pslverr := 0.U
      apb_master.pready := false.B
    }

    when(flash_req) {
      mspi.io.in <> flashXPI.io.apb_out
    }.otherwise {
      mspi.io.in <> in
    }
    flashXPI.io.spi_intr := mspi.io.spi_irq_out
    spi_bundle <> mspi.io.spi
  }
}
