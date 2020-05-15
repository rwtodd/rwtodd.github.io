---
layout: post
title: "Scala Random Sentences"
categories: [ musings ]
---

I ran across Peter Norvig's page [comparing lisp and python][1],
and wanted to see how a scala version would look.  I think it turned out
pretty well; the biggest difference is that I have to define
what I mean by "Tree" for the type system's benefit:

```scala
sealed class Tree
case class Terminal(str: String) extends Tree
case class NonTerminal(cat: String, expansion: Seq[Tree]) extends Tree
```

... but that is just 3 lines, and you get a nice benefit for it: in
python 3.6, when I run Norvig's `generate_tree()`, it just gives me an opaque
`<map object at 0x000001A0C45F9780>`.  However, thanks to scala `List`
and `case class`, I get human-readable output for free.

I was easily able to match the grammar definition syntax
provided by Norvig's idiomatic python example:

Python:

```python
grammar = Grammar(
  S   = 'NP VP',
  NP  = 'Art N',
  VP  = 'V NP',
  Art = 'the | a',
  N   = 'man | ball | woman | table',
  V   = 'hit | took | saw | liked'
  )
```
Scala:

```scala
val grammar = new Grammar(
     'S   -> "NP VP",
     'NP  -> "Art N",
     'VP  -> "V NP",
     'Art -> "the | a",
     'N   -> "man | ball | woman | table",
     'V   -> "hit | took | saw | liked")
```

I checked the code into my [small programs 2018][2] repo on github.

[1]: http://norvig.com/python-lisp.html
[2]: https://github.com/rwtodd/small_programs_2018/

