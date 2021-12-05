# AdventOfCode2021

## Harness

To avoid repetitive boilerplate each day I've abstracted everything out into the [AppWithInput](https://github.com/tOverney/AdventOfCode2021/blob/main/src/main/scala/ch/overney/aoc/harness/AppWithInput.scala)

I then just need to extend this harness trait with:
  * The folderName where all my dataset files are
  * The correct answer to the sample sample dataset

This will then take care of always sanity checking the sample dataset and give me the answer for every other dataset I have configured
