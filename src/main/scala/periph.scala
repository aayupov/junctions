package junctions

import Chisel._

object PeripheralConsts {
  val BLOCKDEV_WIDTH = 64
}
import PeripheralConsts._

class BlockDeviceIO extends Bundle {
  val clk = Bool(OUTPUT)
  val clk_edge = Bool(OUTPUT)
  val in = Decoupled(Bits(width = BLOCKDEV_WIDTH)).flip
  val out = Decoupled(Bits(width = BLOCKDEV_WIDTH))
}
