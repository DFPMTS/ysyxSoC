package ysyx

import chisel3._
import chisel3.util._
import chisel3.experimental.Analog

import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._
import freechips.rocketchip.rocket.CSR.mode
import freechips.rocketchip.rocket.PRV.U
import chisel3.experimental.attach

class SDRAM_CHIP_IO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we = Output(Bool())
  val a = Output(UInt(13.W))
  val ba = Output(UInt(2.W))
  val dqm = Output(UInt(2.W))
  val d_o = Input(UInt(16.W))
  val d_i = Output(UInt(16.W))
  val d_en = Input(Bool())
}
class SDRAMIO extends Bundle {
  val clk = Output(Bool())
  val cke = Output(Bool())
  val cs = Output(Bool())
  val ras = Output(Bool())
  val cas = Output(Bool())
  val we = Output(Bool())
  val a = Output(UInt(13.W))
  val ba = Output(UInt(3.W))
  val dqm = Output(UInt(4.W))
  val dq = Analog(32.W)
}

class sdram_top_axi extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in = Flipped(
      new AXI4Bundle(
        AXI4BundleParameters(addrBits = 32, dataBits = 32, idBits = 4)
      )
    )
    val sdram = new SDRAMIO
  })
}

class sdram_top_apb extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Bool())
    val in =
      Flipped(new APBBundle(APBBundleParameters(addrBits = 32, dataBits = 32)))
    val sdram = new SDRAMIO
  })
}

class sdram extends BlackBox {
  val io = IO(Flipped(new SDRAMIO))
}

class sdramChisel_cmd extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk = Input(Bool())
    val addr = Input(UInt(32.W))
    val en = Input(Bool())
    val cmd = Input(UInt(4.W))
    val dqm = Input(UInt(2.W))
    val wdata = Input(UInt(16.W))
    val rdata = Output(UInt(16.W))
  })
  setInline(
    "sdramChisel_cmd.v",
    """module sdramChisel_cmd(
    |    input         clk,
    |    input  [31:0] addr,
    |    input         en,
    |    input   [3:0] cmd,
    |    input   [1:0] dqm,
    |    input  [15:0] wdata,
    |    output [15:0] rdata
    |  );
    |    import "DPI-C" function void sdram_read(input int addr, output shortint rdata);
    |    import "DPI-C" function void sdram_write(input int addr, input byte dqm, input shortint wdata);
    |   
    |    always@(posedge clk) begin
    |      if(en)begin
    |        case(cmd)
    |          4'b0101: sdram_read(addr, rdata);
    |          4'b0100: sdram_write(addr, {6'b0,dqm}, wdata);
    |          default: begin
    |            $display("SDRAM: Unsupport command: %b\n", cmd);
    |          end
    |        endcase
    |      end
    |    end
    |  endmodule
  """.stripMargin
  )
}

class sdramChip(word_ext: UInt) extends RawModule {
  val io = IO(Flipped(new SDRAM_CHIP_IO))

  withClockAndReset(io.clk.asClock, ~io.cke) {
    val cmd = Cat(io.cs, io.ras, io.cas, io.we)
    val cmd_r = Reg(cmd.cloneType)
    val cnt = RegInit(0.U(4.W))
    // col   9-bits  {addr[9:2],1'b0}
    // row  13-bits   addr[24:12]
    // bank  2-bits   addr[11:10]
    val mode_reg = Reg(UInt(13.W))
    /* Burst Length
     * 000: 1
     * 001: 2
     * 010: 4
     * 011: 8
     * 111: Full page
       Others are reserved
     */
    val BL = MuxLookup(mode_reg(2, 0), 2.U(3.W))(
      Seq(
        0.U -> 1.U,
        1.U -> 2.U,
        2.U -> 4.U,
        3.U -> 8.U
      )
    )

    /* CAS Latency
     * 010: 2
     * 011: 3
       Others are reserved
     */
    val CL = mode_reg(6, 4) - 1.U // -1 to match SDRAM controller
    val s_Idle :: s_Read :: s_Write :: Nil = Enum(3)

    val state = RegInit(s_Idle)
    val counter = RegInit(0.U(3.W))

    val NOP = "b0111".U
    val ACTIVE = "b0011".U
    val READ = "b0101".U
    val WRITE = "b0100".U
    val BURST_TERMINATE = "b0110".U
    val PRECHARGE = "b0010".U
    val REFRESH = "b0001".U
    val LOAD_MODE = "b0000".U
    val ALL_BANK = 10.U

    val row_open = RegInit(VecInit(Seq.fill(4)(false.B)))
    val active_row = Reg(Vec(4, UInt(13.W)))

    val ba_r = RegEnable(io.ba, cmd === READ || cmd === WRITE)
    val ba = Mux(cmd === READ || cmd === WRITE, io.ba, ba_r)
    def next_state_by_cmd(cmd: UInt): UInt = {
      MuxLookup(cmd, s_Idle)(
        Seq(
          READ -> s_Read,
          WRITE -> s_Write
        )
      )
    }
    state := MuxLookup(state, s_Idle)(
      Seq(
        s_Idle -> next_state_by_cmd(cmd),
        s_Read -> Mux(
          cnt === BL + CL - 1.U,
          next_state_by_cmd(cmd),
          s_Read
        ),
        s_Write -> next_state_by_cmd(cmd)
      )
    )
    when(cmd === ACTIVE) {
      row_open(io.ba) := true.B
      active_row(io.ba) := io.a
    }

    when(cmd === PRECHARGE) {
      when(io.a(ALL_BANK)) {
        row_open := VecInit(Seq.fill(4)(false.B))
      }.otherwise {
        row_open(io.ba) := false.B
      }
    }

    when(cmd === LOAD_MODE) {
      mode_reg := io.a
    }

    val col_w = io.a(8, 0)
    val col = Reg(col_w.cloneType)
    col := MuxLookup(state, col)(
      Seq(
        s_Read -> (col + 1.U),
        s_Idle -> MuxLookup(cmd, col)(
          Seq(
            READ -> col_w,
            WRITE -> (col_w + 1.U)
          )
        )
      )
    )

    cmd_r := Mux(cmd === READ || cmd === WRITE, cmd, cmd_r)

    cnt := MuxLookup(cmd, cnt + 1.U)(
      Seq(
        READ -> 0.U,
        WRITE -> 0.U
      )
    )

    val sdram_cmd = Module(new sdramChisel_cmd)
    sdram_cmd.io.clk := io.clk
    sdram_cmd.io.addr := Cat(
      active_row(ba),
      word_ext,
      ba,
      Mux(state === s_Idle && cmd === WRITE, col_w, col),
      0.U(1.W)
    )
    sdram_cmd.io.en := ((state === s_Read && cnt >= (CL - 1.U) && cnt < (CL - 1.U) + BL)
      || cmd === WRITE) && row_open(ba)
    sdram_cmd.io.cmd := Mux(state === s_Idle, cmd, cmd_r)
    sdram_cmd.io.dqm := io.dqm
    sdram_cmd.io.wdata := io.d_i

    io.d_en := (state === s_Read && cnt >= CL && cnt < CL + BL)
    io.d_o := sdram_cmd.io.rdata
  }
}

class sdramChisel extends RawModule {
  val io = IO(Flipped(new SDRAMIO))
  val d_o = Wire(UInt(32.W))
  val d_en = Wire(Bool())
  val d_i = TriStateInBuf(io.dq, d_o, d_en)
  withClock(io.clk.asClock) {
    val NOP = "b0111".U
    val ACTIVE = "b0011".U
    val READ = "b0101".U
    val WRITE = "b0100".U
    val cmd = Cat(io.cs, io.ras, io.cas, io.we)
    val ba_r = RegEnable(io.ba, cmd === READ || cmd === WRITE)
    val ba = Mux(cmd === READ || cmd === WRITE, io.ba, ba_r)

    val chips = Seq(
      Module(new sdramChip(0.U(1.W))),
      Module(new sdramChip(0.U(1.W))),
      Module(new sdramChip(1.U(1.W))),
      Module(new sdramChip(1.U(1.W)))
    )

    for ((chip, idx) <- chips.zipWithIndex) {
      chip.io.clk := io.clk
      chip.io.cke := io.cke
      chip.io.ba := io.ba(1, 0)
      when(
        (cmd === ACTIVE || cmd === READ || cmd === WRITE) && io
          .ba(2) =/= idx.asUInt(2.W)(1)
      ) {
        chip.io.cs := 0.U
        chip.io.ras := 1.U
        chip.io.cas := 1.U
        chip.io.we := 1.U
      }.otherwise {
        chip.io.cs := io.cs
        chip.io.ras := io.ras
        chip.io.cas := io.cas
        chip.io.we := io.we
      }
      val high = (idx & 1) == 1
      val chip_d_i = if (high) d_i(31, 16) else d_i(15, 0)
      chip.io.d_i := chip_d_i
      val chip_a =
        if (high) Mux(cmd === READ || cmd === WRITE, io.a + 1.U, io.a)
        else io.a
      chip.io.a := chip_a
      val chip_dqm = if (high) io.dqm(3, 2) else io.dqm(1, 0)
      chip.io.dqm := chip_dqm
    }

    d_o := Mux(
      ba(2),
      Cat(chips(3).io.d_o, chips(2).io.d_o),
      Cat(chips(1).io.d_o, chips(0).io.d_o)
    )

    d_en := Mux(
      ba(2),
      chips(3).io.d_en & chips(2).io.d_en,
      chips(0).io.d_en & chips(1).io.d_en
    )
  }
}

class AXI4SDRAM(address: Seq[AddressSet])(implicit p: Parameters)
    extends LazyModule {
  val beatBytes = 8
  val node = AXI4SlaveNode(
    Seq(
      AXI4SlavePortParameters(
        Seq(
          AXI4SlaveParameters(
            address = address,
            executable = true,
            supportsWrite = TransferSizes(1, beatBytes),
            supportsRead = TransferSizes(1, beatBytes),
            interleavedId = Some(0)
          )
        ),
        beatBytes = beatBytes
      )
    )
  )

  lazy val module = new Impl
  class Impl extends LazyModuleImp(this) {
    val (in, _) = node.in(0)
    val sdram_bundle = IO(new SDRAMIO)

    val converter = Module(new AXI4DataWidthConverter64to32)
    converter.io.clock := clock
    converter.io.reset := reset.asBool
    converter.io.in <> in

    val msdram = Module(new sdram_top_axi)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> converter.io.out
    sdram_bundle <> msdram.io.sdram
  }
}

class APBSDRAM(address: Seq[AddressSet])(implicit p: Parameters)
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
    val sdram_bundle = IO(new SDRAMIO)

    val msdram = Module(new sdram_top_apb)
    msdram.io.clock := clock
    msdram.io.reset := reset.asBool
    msdram.io.in <> in
    sdram_bundle <> msdram.io.sdram
  }
}
