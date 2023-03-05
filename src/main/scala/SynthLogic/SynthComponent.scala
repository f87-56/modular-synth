package SynthLogic


trait SynthComponent[U<:Signal] {

  val parameters:Vector[Parameter[U]]
  def output:U
}
object SynthComponent:

end SynthComponent

