package SynthLogic

object ComponentLibrary {

  class ConstantFloat(d:Double) extends FloatValue:
    def value: Double = d

  /**
   * A really janky example of building a SynthComponent.
   * TODO: Build type facilities to make this easier
   */
  class PassThrough extends SynthComponent[FloatValue]:
    override val parameters: Vector[Parameter[FloatValue]] =
      Vector(Parameter[FloatValue]("input","", true, ConstantFloat(0.5)))


    // TODO: No joke, this is terrible. It should not stay in the code.
    override def output: FloatValue = parameters.map(_.value).headOption.getOrElse(ConstantFloat(0.0))

  def passthrough: PassThrough = PassThrough()

}
