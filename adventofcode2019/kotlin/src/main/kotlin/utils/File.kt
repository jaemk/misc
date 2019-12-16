package utils

import java.io.BufferedReader
import java.io.File


fun readFile(fileName: String): BufferedReader {
    return File(fileName).bufferedReader()
}
