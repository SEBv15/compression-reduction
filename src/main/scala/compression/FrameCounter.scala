package compression

import chisel3._
import chisel3.util._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

/** Keeps track of the frame number and whether the first sync pulse was received based on the sync pulse and data valid input.
 *
 *  @author Sebastian Strempfer
 *
 *  @param counterwidth How many bits to use for the frame counter
 */
class FrameCounter(counterwidth: Int = 16) extends Module {
    require(counterwidth > 4)

    val io = IO(new Bundle {
        // Frame sync should be set high every 16 ticks on the first shift of a new frame. 
        // It resets the last 4 bits of the shift number to 0 and increases the frame number.
        val frame_sync = Input(Bool())

        // If data valid is low, the data will not be used and not be counted as a shift (the shift number won't increase).
        // This can be used to artificially lower the framerate by selectively skipping shifts.
        val data_valid = Input(Bool())

        // Soft reset will reset the shift number to zero and make the module wait until the next frame sync before setting received_first_sync to high again.
        val soft_rst = Input(Bool())

        // The current shift number (lowest 4 bits indicate shift number, the rest the frame number)
        val shift_num = Output(UInt(counterwidth.W))

        // Wether the first sync pulse was received (start processing data)
        val received_first_sync = Output(Bool())
    })


    // Register to store whether the first sync pulse was received and we should start sending data
    val received_first_sync = RegInit(0.B)
    when (io.soft_rst) {
        received_first_sync := 0.B
    }
    when (io.frame_sync && ~io.soft_rst) {
        received_first_sync := 1.B
    }

    // Register which increments every tick
    val shift_num = RegInit(0.U(counterwidth.W))
    when (io.soft_rst) {
        shift_num := 0.U
    }.otherwise {
        when (io.frame_sync && io.data_valid) {
            // On frame sync, extract the frame number (discarding the last 4 shift number bits), increase it by one, and set the shift number to zero.
            // This way if the sync pulse and shift_num get out of sync, we start over on a new frame number. If they are in sync, the frame number would've been advanced that tick anyways.
            shift_num := Cat(shift_num(counterwidth-1, 4) + 1.U, 0.U(4.W))
        }.otherwise {
            when (io.data_valid && (received_first_sync || io.frame_sync)) {
                shift_num := shift_num + 1.U
            }.otherwise {
                shift_num := shift_num
            }
        }
    }

    io.shift_num := shift_num
    io.received_first_sync := received_first_sync
}

object FrameCounter extends App {    
    (new chisel3.stage.ChiselStage).execute(
        Array("-X", "verilog"),
        Seq(ChiselGeneratorAnnotation(() => new FrameCounter))
    )
}