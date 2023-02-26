package SynthLogic

// the different types of data our parameters can take
enum ParameterType:
  case Boolean
  case Enumeration

class Parameter[T <: ParameterType](name:String, descrition:String, value:T)
