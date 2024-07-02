package ysyx

import chisel3._
import chisel3.util._

class bitrev extends BlackBox {
  val io = IO(Flipped(new SPIIO(1)))
}

class bitrevChisel extends RawModule { // we do not need clock and reset
  val io = IO(Flipped(new SPIIO(1)))
  withClockAndReset((~io.sck).asClock, io.ss.asBool.asAsyncReset) {
    val rev_buf = Reg(UInt(8.W));
    val cnt = RegInit(0.U(3.W))
    val rev = RegInit(false.B)
    rev_buf := Mux(
      rev,
      Cat(rev_buf(6, 0), io.mosi),
      Cat(io.mosi, rev_buf(7, 1))
    )
    cnt := cnt + 1.U
    rev := Mux(cnt === 7.U, true.B, rev)
    io.miso := Mux(~io.ss.asBool, rev_buf(7), 1.U);
  }
}
