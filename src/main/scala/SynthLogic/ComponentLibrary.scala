package SynthLogic

object ComponentLibrary {

  /**
   * A really janky example of building a SynthComponent.
   */
  class PassThrough extends SynthComponent[DoubleSignal]():
    val parameters: Seq[Parameter[DoubleSignal]] =
      Vector(Parameter[DoubleSignal]("input","", true, DoubleSignal(0.5)))
    // I am fairly satisfied as to how this looks.
    override def output: DoubleSignal = param[DoubleSignal]("input").getOrElse(DoubleSignal(0.0))

  def passthrough: PassThrough = PassThrough()

}
