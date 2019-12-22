package utils


fun <U> (() -> U).memoize(): () -> U {
    var cache: Option = None
    return {
        @Suppress("UNCHECKED_CAST")
        when (cache) {
            is None -> {
                cache = Some(this())
                (cache as Some<U>).inner
            }
            is Some<*> -> (cache as Some<U>).inner
        }
    }
}

fun <T, U> ((T) -> U).memoize(): (T) -> U {
    val cache: MutableMap<T, U> = HashMap()
    return {input ->
        cache.getOrPut(input, { this(input) })
    }
}