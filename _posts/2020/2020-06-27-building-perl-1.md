---
layout: post
title: "Building Perl version 1"
categories: [ computing ]
---

I've been enjoying Perl lately, but I've never looked at the implementation
before.  It was a pleasant surprise to see that the [github repo][1] has a
pretty complete history, all the way to Perl 1.  In no time, I found myself
wondering:

> Can I build Perl 1 today?

I checked out a worktree of Perl 1, patch 14.  This was the last revision in
the Perl 1 series available, and it is dated Feb 1, 1988.  Let's see if I can
build it with gcc 10 on Fedora 32.

``` bash
git worktree add ../perl1 a4de7c03d0bdc29d9d3a18abad4ac2628182ed7b
```

## Configure  ##

Ok, the README file says to run Configure.  I don't expect it to work
unmodified, plus I'm scared it will throw files around, so I read through it
first.  Some observations:

- It says to trim the comments manually if your shell doesn't do that.
  Apparently in 1988 not all bourne shells understood comments.
- On line 30 it's still just trying to double-check it is running in a bourne
  shell.  Stuff we take for granted today!
- It's very conversational.  It will echo things like "I found it *here*, of
  all places."  This would happen more often in the 80's, I think, than it
  does today.  There must have been a theory that conversational programs
  were a good idea--maybe with an eye toward the eventual HAL9000.  TeX, for
  example, also sometimes speaks to you as if it's an entity.
- It warns you that `make depend` will take awhile, when of course I know
  it's going to run instantaneously.  Modern computing has benefits!
 
 Running Configure, the only time it really got stumped was when it tried to
 find libc.  Rather than giving up, it just asked me for the path.  I
 knew from skimming the script that it wanted an libc.a static archive, but I
 tried giving it "/lib64/libc-2.31.so" anyway.  It actually seemed to be
 happy.

 I ran "make depend" and it put some garbage lines in the makefile (where
 files depend on nonsense like "\<built-in\>" and "\<command-line\>".  I just
 deleted these lines manually.

## Make ## 

The make went very smoothly, all things considered.  Almost all the issues I
needed to correct were due to incomplete function prototypes and a few
overlaps between provided functions and standard functions.  I also needed
to adjust the LDFLAGS to link to libcrypt.  Soon enough, I had a working
Perl.

I'll include the patch between my version and the git source at the bottom of
this post.  The entire unified diff is only about 200 lines long.

The distribution has a set of tests in a "t" directory, and nearly all of
them pass.  One bug this binary has, in particular, is that you can't run
a one liner... using the "-e" argument leads to a segmentation fault.  Since
it runs scripts ok, I did not try to track that issue down.  It's not going
to be the Perl I use day-to-day, after all!

## The Perl 1 Language ##

This is what I was most interested to understand: 
*is Perl version 1 anything like the Perl I know?*  Looking at the test
scripts, the distinctive flavor of Perl is decidedly there.

- Sigils? check.
- Lists and hashes? check.
- Double-quote interpolation? check.
- Diamond operator? check.
- if/unless/while/until as statment modifiers? check.
- list arguments flatten? partially. 
- 'last' instead of 'break'?  check.
- formats? check.
- Regex? check.
  - choose-your-own-delimiters?  yep
- Topic variables $\_ and @\_? absolutely.
- Subs without signatures, shifting arguments from @\_?  yes.

Let me cut to the chase: the test suite looks so much like Perl that most of
the tests pass when I run **Perl 5.30** on them!  Here are the main
differences I noticed:

- You seem to need more parens to make the parser happy than you need in a
  current Perl.  Example:  `$j = join ':', (1,2,3,4);` does not work in Perl
  1; you need to put parens around the arguments to "join."
- There don't seem to be any auto-quoting barewords.  So use `$h{'name'}`, not `$h{name}`.
- I expected to need to call subs with `&mysub();`, but in Perl 1 the
  incantation is actually `do mysub();`.  That's the main thing that causes
  Perl 5 to fail when running the test suite.

... and obviously no lexical variables or modules, etc.  There are things
missing, clearly, but nearly everything in Perl 1 made it to Perl 5
unchanged.  

## No Need to Continue ##

I had initially planned to get Perl 2 and 3 to compile as well, and chart the
progression toward familiar Perl.  I don't need to: the first Perl is so
*Perl* that it would be a waste of time.  I really didn't expect that.

## The Patch ##

If you'd like to build it yourself, maybe this patch will help you:
[perl1-modern.patch](/assets/2020/10/perl1-modern.patch).  It should apply
against a checkout of commit a4de7c03d0bdc29d9d3a18abad4ac2628182ed7b in the
perl5 github repository (Perl 1, patch 14).

[1]: https://github.com/Perl/perl5

