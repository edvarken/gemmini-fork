package gemmini

import chisel3._
import chisel3.util._ // needed for Cat()
import hardfloat._ // needed for recoded FN converters etc..

// // _root_ disambiguates from package chisel3.util.circt if user imports chisel3.util._
// import _root_.circt.stage.ChiselStage

/**
  * This is a Chisel implementation that uses a Lookup Table to approximate the SiLU activation function between -4 and +4
  * Below -4 and above +4, the function returns 0 and the input itself respectively.
  * The implementation only supports BF16 floating point representation
  */
class siluUsingLUT extends Module {
    val io = IO(new Bundle {
        val in_a = Input(Bits(16.W)) // define as raw Bits collection, but represents BF16
        val out_a = Output(Bits(16.W))
    })

    val a = io.in_a // a must be a BigInt
    val sign = a(15).asUInt
    val exp = a(14,7).asUInt
    val actual_exp = (exp.asSInt - 127.S(8.W)).asSInt // actual_exp can be negative!
    val mantissa = a(6,0).asUInt

    val lut = Module(new siluLUT) // LUT for the values between -4 and 4
    val bf16tofp = Module(new BF16toFP(2, 4)) // BF16 to FP converter, 2 bits for integer part and 4 bits for fractional part

    bf16tofp.io.bf16in := a
    val a_int = bf16tofp.io.intout 
    val a_frac = bf16tofp.io.fracout
    val a_sign = bf16tofp.io.signout
    val index = Cat(a_sign, a_int, a_frac) // create the index
    lut.io.indexIn := index // LUT input
    val lutValue = lut.io.valueOut
    val outputReg = RegInit(0.U(16.W)) // register for the output

    when (sign === 1.U) { // in_a <= -0
        when (a === "b1_00000000_0000000".U) { // in_a = -0
            outputReg := 0.U
        }.elsewhen (actual_exp >= 2.S) { // in_a <= -4
            outputReg := 0.U
        }.otherwise { // -4 < in_a <= -0
            outputReg := lutValue
        }

    }.otherwise { // in_a > 0
        when (a === "b0_00000000_0000000".U) { // in_a = +0
            outputReg := 0.U
        }.elsewhen (actual_exp >= 2.S) { // in_a >= 4
            outputReg := a // SiLU(a) = a
        }.otherwise { // 0 < in_a < 4
            outputReg := lutValue
        }
    }
    io.out_a := outputReg // output the result
}

/**
 * Generate Verilog sources and save it in generated/siluUsingLUT.v
 */
// object siluUsingLUT extends App {
//     ChiselStage.emitSystemVerilogFile(
//         new siluUsingLUT,
//         firtoolOpts = Array("-disable-all-randomization", "-strip-debug-info"),
//         args = Array("--target-dir", "generated")
//     )
// }