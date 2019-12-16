package utils


sealed class Option
data class Some<T>(val inner: T) : Option()
object None : Option()
