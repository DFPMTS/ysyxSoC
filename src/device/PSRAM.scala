package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import os.stat
import scala.runtime.BoxedUnit
import freechips.rocketchip.diplomacy.BindingScope.add

class QSPIIO extends Bundle {
  val sck = Output(Bool())
  val ce_n = Output(Bool())
  val dio = Analog(4.W)
}

class psram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val qspi = new QSPIIO
  })
}

class psram extends BlackBox {
  val io = IO(Flipped(new QSPIIO))
}

class psramChisel_cmd extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk = Input(Bool())
    val addr = Input(UInt(32.W))
    val en = Input(Bool())
    val cmd = Input(UInt(8.W))
    val wdata = Input(UInt(8.W))
    val rdata = Output(UInt(32.W))
  })
  setInline(
    "psramChisel_cmd.v",
    """module psramChisel_cmd(
    |    input         clk,
    |    input  [31:0] addr,
    |    input         en,
    |    input   [7:0] cmd,
    |    input   [7:0] wdata,
    |    output [31:0] rdata
    |  );
    |    import "DPI-C" function void psram_read(input int addr, output int rdata);
    |    import "DPI-C" function void psram_write(input int addr, input byte wdata);
    |   
    |    always@(posedge clk) begin
    |      if(en)begin
    |        case(cmd)
    |          8'hEB: psram_read(addr, rdata);
    |          8'h38: psram_write(addr, wdata);
    |          default: begin
    |            $display("Unsupport command: %xh\n", cmd);
    |          end
    |        endcase
    |      end
    |    end
    |  endmodule
  """.stripMargin
  )
}

class psramChisel_mode extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val sck = Input(Bool())
    val enter_qpi = Input(Bool())
    val is_qpi = Output(Bool())
  })
  setInline(
    "psramChisel_mode.v",
    """module psramChisel_mode(
    |    input sck,
    |    input enter_qpi,
    |    output reg is_qpi
    |  );
    |    initial begin 
    |      is_qpi = 0;
    |    end
    |
    |    always@(posedge sck)begin
    |      is_qpi |= enter_qpi;
    |    end
    |endmodule
    """.stripMargin
  )
}

class psramChisel extends RawModule {
  val io = IO(Flipped(new QSPIIO))
  val d_o = Wire(UInt(4.W))
  val d_en = Wire(Bool())
  val d_i = TriStateInBuf(io.dio, d_o, d_en) // change this if you need
  withClockAndReset(io.sck.asClock, io.ce_n.asAsyncReset) {
    val cmd = RegInit(0.U(8.W))
    val addr = RegInit(0.U(24.W))
    val cnt = RegInit(0.U(3.W))
    val data = Reg(UInt(32.W))

    val s_cmd :: s_addr :: s_wait :: s_data_in :: s_data_out :: Nil =
      Enum(5)
    val psram_mode = Module(new psramChisel_mode)
    val enter_qpi = Wire(Bool())
    psram_mode.io.sck := io.sck
    psram_mode.io.enter_qpi := enter_qpi
    val qpi_mode = psram_mode.io.is_qpi

    val state = RegInit(s_cmd)

    val CMD_35H = "h35".U(8.W)
    val CMD_EBH = "hEB".U(8.W)
    val CMD_38H = "h38".U(8.W)
    val CMD_CYCLES = 8.U
    val CMD_QPI_CYCLES = 2.U
    val ADDR_CYCLES = 6.U
    // add one more cycle, see Datasheet Figure 5.3
    // cycle = 0 before posedge 14
    // cycle = 6 before posedge 20
    // at cycle 6, prepare for psram read (vaild high)
    val WAIT_CYCLES = 6.U + 1.U
    // PSRAM controller READ always read a word (32bits)
    val DATA_OUT_CYCLES = 8.U
    // PSRAM controller WRITE size can vary, my choice is every 2 cycles, write byte, increase addr, it will stay at s_data_in until next reset
    val DATA_IN_CYCLES = 2.U

    val cmd_last_cycle =
      Mux(qpi_mode, cnt === CMD_QPI_CYCLES - 1.U, cnt === CMD_CYCLES - 1.U)
    val addr_last_cycle = cnt === ADDR_CYCLES - 1.U
    val wait_last_cycle = cnt === WAIT_CYCLES - 1.U

    state := MuxLookup(state, s_cmd)(
      Seq(
        s_cmd -> Mux(cmd_last_cycle, s_addr, s_cmd),
        s_addr -> Mux(
          addr_last_cycle,
          Mux(cmd === CMD_38H, s_data_in, s_wait),
          s_addr
        ),
        s_data_in -> s_data_in,
        s_wait -> Mux(wait_last_cycle, s_data_out, s_wait),
        s_data_out -> Mux(cnt === DATA_OUT_CYCLES - 1.U, s_cmd, s_data_out)
      )
    )

    cnt := MuxLookup(state, cnt + 1.U)(
      Seq(
        s_cmd -> Mux(cmd_last_cycle, 0.U, cnt + 1.U),
        s_addr -> Mux(addr_last_cycle, 0.U, cnt + 1.U),
        s_wait -> Mux(wait_last_cycle, 0.U, cnt + 1.U),
        s_data_in -> Mux(cnt === DATA_IN_CYCLES - 1.U, 0.U, cnt + 1.U)
      )
    )
    cmd := Mux(
      state === s_cmd,
      Mux(qpi_mode, Cat(cmd(3, 0), d_i), Cat(cmd(6, 0), d_i(0))),
      cmd
    )

    addr := MuxLookup(state, addr)(
      Seq(
        s_addr -> Cat(addr(19, 0), d_i),
        s_data_in -> Mux(cnt === DATA_IN_CYCLES - 1.U, addr + 1.U, addr)
      )
    )

    val psram_cmd = Module(new psramChisel_cmd)

    def endianness_convert(data: UInt): UInt = {
      Cat(data(7, 0), data(15, 8), data(23, 16), data(31, 24))
    }

    val psram_rdata_big_endian = endianness_convert(psram_cmd.io.rdata)

    data := MuxLookup(state, data)(
      Seq(
        s_data_in -> Cat(data(27, 0), d_i),
        s_data_out -> Mux(
          cnt === 0.U,
          Cat(psram_rdata_big_endian(27, 0), 0.U(4.W)),
          Cat(data(27, 0), 0.U(4.W))
        )
      )
    )

    val valid =
      (cmd === CMD_EBH && state === s_wait && cnt === WAIT_CYCLES - 1.U) || (cmd === CMD_38H && state === s_data_in && cnt === DATA_IN_CYCLES - 1.U)

    enter_qpi := Cat(
      cmd(6, 0),
      d_i(0)
    ) === CMD_35H && state === s_cmd && cnt === CMD_CYCLES - 1.U

    psram_cmd.io.clk := io.sck
    psram_cmd.io.cmd := cmd
    psram_cmd.io.en := valid
    psram_cmd.io.addr := addr
    psram_cmd.io.wdata := Cat(data(3, 0), d_i)
    d_o := Mux(cnt === 0.U, psram_rdata_big_endian(31, 28), data(31, 28))
    d_en := (state === s_data_out)
  }
}

class APBPSRAM(address: Seq[AddressSet])(implicit p: Parameters)
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
    val qspi_bundle = IO(new QSPIIO)

    val mpsram = Module(new psram_top_apb)
    mpsram.io.clock := clock
    mpsram.io.reset := reset
    mpsram.io.in <> in
    qspi_bundle <> mpsram.io.qspi
  }
}
