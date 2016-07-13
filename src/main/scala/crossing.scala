package junctions
import Chisel._

class Crossing[T <: Data](gen: T, enq_sync: Boolean, deq_sync: Boolean) extends Bundle {
    val enq = Decoupled(gen.cloneType).flip()
    val deq = Decoupled(gen.cloneType)
    val enq_clock = if (enq_sync) Some(Clock(INPUT)) else None
    val deq_clock = if (deq_sync) Some(Clock(INPUT)) else None
    val enq_reset = if (enq_sync) Some(Bool(INPUT))  else None
    val deq_reset = if (deq_sync) Some(Bool(INPUT))  else None
}

object GrayCounter {
  def apply(bits: Int, increment: Bool = Bool(true)): UInt = {
    val binary = RegInit(UInt(0, width = bits))
    val incremented = binary + increment.asUInt()
    binary := incremented
    incremented ^ (incremented >> UInt(1))
  }
}

object AsyncGrayCounter {
  def apply(in: UInt, sync: Int): UInt = {
    val syncv = RegInit(Vec.fill(sync){UInt(0, width = in.width)})
    syncv.last := in
    (syncv.init zip syncv.tail).foreach { case (sink, source) => sink := source }
    syncv(0)
  }
}

class AsyncQueueSource[T <: Data](gen: T, depth: Int, sync: Int, clock: Clock, reset: Bool)
    extends Module(_clock = clock, _reset = reset) {
  val io = new Bundle {
    // These come from the source domain
    val enq  = Decoupled(gen.cloneType).flip()
    // These cross to the sink clock domain
    val ridx = UInt(INPUT,  width = depth+1)
    val widx = UInt(OUTPUT, width = depth+1)
  }

  val widx = GrayCounter(depth+1, io.enq.fire())
  val ridx = AsyncGrayCounter(io.ridx, sync)
  val start = ridx ^ (UInt(1) << UInt(depth))

  io.enq.ready := RegNext(widx =/= start)
  io.widx := RegNext(widx)
}

class AsyncQueueSink[T <: Data](gen: T, depth: Int, sync: Int, clock: Clock, reset: Bool)
    extends Module(_clock = clock, _reset = reset) {
  val io = new Bundle {
    // These come from the sink domain
    val deq  = Decoupled(gen.cloneType)
    // These cross to the source clock domain
    val ridx = UInt(OUTPUT, width = depth+1)
    val widx = UInt(INPUT,  width = depth+1)
    // These go to the SeqMem
    val addr = UInt(OUTPUT, width = depth+1)
  }

  val ridx = GrayCounter(depth+1, io.deq.fire())
  val widx = AsyncGrayCounter(io.widx, sync)
  val end = widx

  io.deq.valid := RegNext(ridx =/= end)
  io.ridx := RegNext(ridx)

  // async address, because the register is inside the SeqMem
  io.addr := ridx
}

class AsyncQueue[T <: Data](gen: T, depth: Int, sync: Int = 2) extends Module {
  val io = new Crossing(gen, true, true)
  require (sync >= 2)

  val none = Vec(0, Bool(OUTPUT))
  val source = Module(new AsyncQueueSource(none, depth, sync, io.enq_clock.get, io.enq_reset.get))
  val sink   = Module(new AsyncQueueSink  (none, depth, sync, io.deq_clock.get, io.deq_reset.get))

  source.io.enq.valid := io.enq.valid
  io.enq.ready := source.io.enq.ready
  sink.io.deq.ready := io.deq.ready
  io.deq.valid := sink.io.deq.ready

  sink.io.widx := source.io.widx
  source.io.ridx := sink.io.ridx

  val mem = SeqMem(1 << depth, gen)
  when (io.enq.fire()) { mem.writeOnClock(source.io.widx(depth-1, 0), io.enq.bits, io.enq_clock.get) }
  io.deq.bits := mem.readOnClock(sink.io.addr(depth-1, 0), io.deq_clock.get)
}

// Output is 1 for one cycle after any edge of 'in'
object AsyncHandshakePulse {
  def apply(in: Bool, sync: Int): Bool = {
    val syncv = RegInit(Vec.fill(sync+1){Bool(false)})
    syncv.last := in
    (syncv.init zip syncv.tail).foreach { case (sink, source) => sink := source }
    syncv(0) =/= syncv(1)
  }
}

class AsyncHandshakeSource[T <: Data](gen: T, sync: Int, clock: Clock, reset: Bool)
    extends Module(_clock = clock, _reset = reset) {
  val io = new Bundle {
    // These come from the source clock domain
    val enq  = Decoupled(gen.cloneType).flip()
    // These cross to the sink clock domain
    val bits = gen.cloneType
    val push = Bool(OUTPUT)
    val pop  = Bool(INPUT)
  }

  val ready = RegInit(Bool(true))
  val bits = Reg(gen.cloneType)
  val push = RegInit(Bool(false))

  io.enq.ready := ready
  io.bits := bits
  io.push := push

  val pop = AsyncHandshakePulse(io.pop, sync)
  assert (!pop || !ready)

  when (pop) {
    ready := Bool(true)
  }

  when (io.enq.fire()) {
    ready := Bool(false)
    bits := io.enq.bits
    push := !push
  }
}

class AsyncHandshakeSink[T <: Data](gen: T, sync: Int, clock: Clock, reset: Bool) 
    extends Module(_clock = clock, _reset = reset) {
  val io = new Bundle {
    // These cross to the source clock domain
    val bits = gen.cloneType.flip()
    val push = Bool(INPUT)
    val pop  = Bool(OUTPUT)
    // These go to the sink clock domain
    val deq = Decoupled(gen.cloneType)
  }

  val valid = RegInit(Bool(false))
  val bits  = Reg(gen.cloneType)
  val pop   = RegInit(Bool(false))

  io.deq.valid := valid
  io.deq.bits  := bits
  io.pop := pop

  val push = AsyncHandshakePulse(io.push, sync)
  assert (!push || !valid)

  when (push) {
    valid := Bool(true)
    bits  := io.bits
  }

  when (io.deq.fire()) {
    valid := Bool(false)
    pop := !pop
  }
}

class AsyncHandshake[T <: Data](gen: T, sync: Int = 2) extends Module {
  val io = new Crossing(gen, true, true)
  require (sync >= 2)

  val source = Module(new AsyncHandshakeSource(gen, sync, io.enq_clock.get, io.enq_reset.get))
  val sink   = Module(new AsyncHandshakeSink  (gen, sync, io.deq_clock.get, io.deq_reset.get))

  source.io.enq <> io.enq
  io.deq <> sink.io.deq

  sink.io.bits := source.io.bits
  sink.io.push := source.io.push
  source.io.pop := sink.io.pop
}

class AsyncDecoupledTo[T <: Data](gen: T, depth: Int = 0, sync: Int = 2) extends Module {
  val io = new Crossing(gen, false, true)

  val crossing = if (depth == 0) {
    Module(new AsyncHandshake(gen, sync)).io
  } else {
    Module(new AsyncQueue(gen, depth, sync)).io
  }

  crossing.enq_clock.get := clock
  crossing.enq_reset.get := reset
  crossing.enq <> io.enq
  crossing.deq_clock.get := io.deq_clock.get
  crossing.deq_reset.get := io.deq_reset.get
  io.deq <> crossing.deq
}

object AsyncDecoupledTo {
  // source is in our clock domain, output is in the 'to' clock domain
  def apply[T <: Data](to_clock: Clock, to_reset: Bool, source: DecoupledIO[T], depth: Int = 0, sync: Int = 2): DecoupledIO[T] = {
    val to = Module(new AsyncDecoupledTo(source.bits, depth, sync))
    to.io.deq_clock.get := to_clock
    to.io.deq_reset.get := to_reset
    to.io.enq <> source
    to.io.deq
  }
}

class AsyncDecoupledFrom[T <: Data](gen: T, depth: Int = 0, sync: Int = 2) extends Module {
  val io = new Crossing(gen, true, false)

  val crossing = if (depth == 0) {
    Module(new AsyncHandshake(gen, sync)).io
  } else {
    Module(new AsyncQueue(gen, depth, sync)).io
  }

  crossing.enq_clock.get := io.enq_clock.get
  crossing.enq_reset.get := io.enq_reset.get
  crossing.enq <> io.enq
  crossing.deq_clock.get := clock
  crossing.deq_reset.get := reset
  io.deq <> crossing.deq
}

object AsyncDecoupledFrom {
  // source is in the 'from' clock domain, output is in our clock domain
  def apply[T <: Data](from_clock: Clock, from_reset: Bool, source: DecoupledIO[T], depth: Int = 0, sync: Int = 2): DecoupledIO[T] = {
    val from = Module(new AsyncDecoupledFrom(source.bits, depth, sync))
    from.io.enq_clock.get := from_clock
    from.io.enq_reset.get := from_reset
    from.io.enq <> source
    from.io.deq
  }
}
