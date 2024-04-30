package it.unibo.modularization

class EmergencyService extends MyAggregateProgram {

  override def main(): Any = {
    rep(0.0)(_ + 1)
  }
}
