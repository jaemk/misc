import strformat
import strutils

proc checksum(input: string, get_other_index: proc (index: int, size: int): int): int =
  let size = input.len
  var sum = 0
  for i, c in input:
    let other_index = get_other_index(i, size)
    if input[i] == input[other_index]:
      sum += parseInt($input[i])
  return sum

proc p1_indexer(index: int, size: int): int =
  if index >= size - 1:
    return 0
  return index + 1

proc p2_indexer(index: int, size: int): int =
  var next = index + (size div 2)
  if next >= size:
    return next - size
  return next

proc part_1*(input: string): int =
  return checksum(input, p1_indexer)

proc part_2*(input: string): int =
  return checksum(input, p2_indexer)

when isMainModule:
  var input = readFile("../input.txt").strip(chars = Whitespace)
  echo &"part-1: {part_1(input)}"
  echo &"part-2: {part_2(input)}"
