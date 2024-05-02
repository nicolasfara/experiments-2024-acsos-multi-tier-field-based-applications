package it.unibo.modularization

class EmergencyService extends MyAggregateProgram {

  override def main() = {
    val hopCount: Int = G[Int](mid() == 80, 0, acc => acc + 1, nbrRange _)
    writeEnv("hopCount", hopCount)
  }
}
