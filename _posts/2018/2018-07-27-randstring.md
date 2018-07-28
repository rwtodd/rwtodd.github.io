---
layout: post
title: "Strings in Encrypted Data"
date: 2018-07-27  14:00
categories: scala math
---

(This is just a random musing, performed at a `scala` REPL)

In an encrypted file I expect every byte value to be evenly distributed.
In other words, I would expect any byte I choose to be anything
from 0 to 255 with an equal probablility (1/256, or 0.39%).
Given that, how likely am I to find the word "dog" in my
encrypted file?

Well, if we take the bytes of the file to be a permutation of 256 options
with repitition, then there are 256^n permutations of `n` chars:

```scala
def perms(len: Int) = Math.pow(256,len)

perms("dog".length)
// res19: Double = 1.6777216E7
```
That's hard to read as a double, but it's 16,777,216.  I went with
a double because I knew we'd be dealing with percentages in the coming
steps.

The chance that a given set of chars in a file matches
my chosen permutation is just `1.0/perms(...)`:

```scala
def chances(str: String) = 1.0/perms(str.length)

chances("dog")
// res21: Double = 5.9604644775390625E-8
```

Alright, so at every group of 3 chars, I've got another shot
at seeing "dog."  That means, for a file of size N, I have
`N - (3 - 1)` chances to spot my word. I'll compute that, and
go ahead and multiply by 100 to get a percentage:

```scala
def fileProbability(str: String, fsize: Long) =
   chances(str)*(fsize - str.length + 1)*100.0

def printTable(str: String) =
  for( i <- 0 to 24 ) {
    val fsize = 1024L << i
    val prob = fileProbability(str, fsize)
    println(f"File size $fsize%15d, '$str' is $prob%.2f%% likely")
  }

printTable("dog")
// File size            1024, 'dog' is 0.01% likely
// File size            2048, 'dog' is 0.01% likely
// File size            4096, 'dog' is 0.02% likely
// File size            8192, 'dog' is 0.05% likely
// File size           16384, 'dog' is 0.10% likely
// File size           32768, 'dog' is 0.20% likely
// File size           65536, 'dog' is 0.39% likely
// File size          131072, 'dog' is 0.78% likely
// File size          262144, 'dog' is 1.56% likely
// File size          524288, 'dog' is 3.12% likely
// File size         1048576, 'dog' is 6.25% likely
// File size         2097152, 'dog' is 12.50% likely
// File size         4194304, 'dog' is 25.00% likely
// File size         8388608, 'dog' is 50.00% likely
// File size        16777216, 'dog' is 100.00% likely
// File size        33554432, 'dog' is 200.00% likely
// File size        67108864, 'dog' is 400.00% likely
// File size       134217728, 'dog' is 800.00% likely
// File size       268435456, 'dog' is 1600.00% likely
// File size       536870912, 'dog' is 3200.00% likely
// File size      1073741824, 'dog' is 6400.00% likely
// File size      2147483648, 'dog' is 12800.00% likely
// File size      4294967296, 'dog' is 25600.00% likely
// File size      8589934592, 'dog' is 51200.00% likely
// File size     17179869184, 'dog' is 102400.00% likely
```

You can see from the math that making the string just one character longer
drops the probabilities by a factor of 256:

```scala
printTable("bird")
// File size            1024, 'bird' is 0.00% likely
// File size            2048, 'bird' is 0.00% likely
// File size            4096, 'bird' is 0.00% likely
// File size            8192, 'bird' is 0.00% likely
// File size           16384, 'bird' is 0.00% likely
// File size           32768, 'bird' is 0.00% likely
// File size           65536, 'bird' is 0.00% likely
// File size          131072, 'bird' is 0.00% likely
// File size          262144, 'bird' is 0.01% likely
// File size          524288, 'bird' is 0.01% likely
// File size         1048576, 'bird' is 0.02% likely
// File size         2097152, 'bird' is 0.05% likely
// File size         4194304, 'bird' is 0.10% likely
// File size         8388608, 'bird' is 0.20% likely
// File size        16777216, 'bird' is 0.39% likely
// File size        33554432, 'bird' is 0.78% likely
// File size        67108864, 'bird' is 1.56% likely
// File size       134217728, 'bird' is 3.12% likely
// File size       268435456, 'bird' is 6.25% likely
// File size       536870912, 'bird' is 12.50% likely
// File size      1073741824, 'bird' is 25.00% likely
// File size      2147483648, 'bird' is 50.00% likely
// File size      4294967296, 'bird' is 100.00% likely
// File size      8589934592, 'bird' is 200.00% likely
// File size     17179869184, 'bird' is 400.00% likely
```

Indeed, when I run `strings` on a large set of encryped files, I had
to go through about 3GB of data before finding the string "bird", while
"dog" was far more prevalent.  So it looks like the math--and, more importantly,
my encryption--checks out.

