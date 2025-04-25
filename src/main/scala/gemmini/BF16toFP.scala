package gemmini
import chisel3._
import chisel3.util._ // needed for Cat()

class BF16toFP(val intBits: Int = 2, val fracBits: Int = 4) extends Module { // FixedPoint will have 1(Sign) + intBits + fracBits total bits
  val io = IO(new Bundle {
    val bf16in = Input(UInt(16.W))         // BF16 input
    val intout = Output(UInt(intBits.W))   // Integer part of fixed point output
    val fracout = Output(UInt(fracBits.W)) // Fractional part of fixed-point output
    val signout = Output(UInt(1.W))        // Sign bit output
  })

  // Decompose BF16 fields
  val sign     = io.bf16in(15)
  val exponent = io.bf16in(14, 7)
  val mantissa = io.bf16in(6, 0)

  // Bias for BF16 exponent
  val bias = 127.S

  
  when (exponent === 0.U(8.W) && mantissa === 0.U(7.W)) { // if exponent and mantissa 0, return +0 or -0
    io.intout := 0.U(intBits.W) // integer part
    io.fracout := 0.U(fracBits.W) // fractional part
    io.signout := sign // sign bit

  }.otherwise {// Normalize mantissa: add implicit leading 1 for normal numbers
    val normalizedMantissa = Cat(1.U(1.W), mantissa)//.U(8.W) // 8 bits total

    // calculate needed shift
    val expVal = exponent.asSInt - bias // real exponent
    val shift = Wire(SInt(6.W))
    shift := expVal - (7.S - fracBits.asSInt) // 7 - 4(fracBits) = 3, coming from an 8bit normalizedMantissa, we need to shift right by 3 places if e.g. expval=0

    // Value without sign
    val unsignedValue = Wire(UInt((intBits + fracBits).W)) // Bits to hold shifted mantissa

    // shift mantissa
    // the positive shift never happens when we use this module is used in LUTsilu.scala 
    // since in LUTsilu.scala we only use this module for inputs that have expVals <= 1 (bf16 inputs in the range (-4, 4)).
    // anything outside that range doesn't use LUTs for the silu and thus does not need this conversion module to get the FP format.
    when(shift >= 0.S) { // exp >= (127+3)
      unsignedValue := normalizedMantissa << shift.asUInt // arithmetic shift left
    }.otherwise { // exp < (127+3)
      unsignedValue := normalizedMantissa >> (-shift).asUInt // arithmetic shift right, adds padding zeroes at the left.
    }

    io.intout := unsignedValue(intBits + fracBits - 1, fracBits) // integer part
    io.fracout := unsignedValue(fracBits - 1, 0) // fractional part
    io.signout := sign // sign bit
  }
}