---
layout: post
title: "Powershell Code-Page Experiments"
date: 2018-10-12 21:00
categories: windows console
---

## Overview

I wanted to get my head more fully around how Powershell deals with code
pages, since I keep running into annoyances.  I picked a simple case,
some CP437 text, and just want to reliably `cat` it to the screen on
both Windows Powershell and Powershell core.


## Prelude: Binary Writing Utiltity

To make sure my test files have exacly what I expect (rather than
messing with the encoding settings of my text editors and then
double-checking the results), I wrote a short powershell script:

~~~~~ powershell
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# A script to write binary bytes to a file, as-is.
# Usage: write-bin.ps1 FILENAME XX XX XX XX ...
#
# Where XX are hex digits (e.g., 2f a9 etc.)
# The file is always written relative to the current
# directory.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
param([string] $fname)

$fullpath = Join-Path (Get-Location) $fname
$bytes = foreach ($xx in $args) {
    [Convert]::ToUint32($xx,16)
}

[IO.File]::WriteAllBytes($fullpath, $bytes)
~~~~~

## Test 1: OEM 437 text

Ok, as I said in the intro, I just want to start easy and put together
some OEM437 code-page text:

~~~~~ powershell
> write-bin 437.txt db de de 20 ea 20 dd dd db
~~~~~

Printed properly, this should produce:

    █▐▐ Ω ▌▌█

And indeed, if my code-page is set to 437, I can go to `cmd.exe` and
`type` the file with no problem.  How about the powershells?

Powershell core with code-page set to
437 displays missing-unicode-char garbage (with `get-content 437.txt`).
Setting `$OutputEncoding` to match `[Console]::OutputEncoding`
had no effect.  It's the same story with the original
windows Powershell, only it looks like latin-1 output instead of
missing unicode chars.  That's strange, but I'll worry about that
later..

I **can** make it work in both powershells by giving the explicit `OEM`
encoding on the command itself:

~~~~~~ powershell
> Get-Content -Encoding oem 437.txt
~~~~~~

... but if I don't specify the encoding, Powershell clearly doesn't
respect code-pages on input.

## Test 2: Bypass Get-Content

So maybe powershell only applies the code-pages to external commands?

~~~~~~ powershell
> cmd.exe /c type .\437.txt
█▐▐ Ω ▌▌█
~~~~~~

**Success**!  Does the text survive through a pipe ok?

~~~~~~ powershell
> cmd.exe /c type .\437.txt | Select-String Ω 

█▐▐ Ω ▌▌█

~~~~~~

**Yes it does**!   What if the pipe is to an external program?  For
this, I just wrote a short program to push bytes as-is from stdin to
stdout:

~~~~~~ powershell
> cmd.exe /c type .\437.txt | .\TypeBytes.exe
█▐▐ Ω ▌▌█
> $OutputEncoding = [Text.Encoding]::ASCII
> cmd.exe /c type .\437.txt | TypeBytes.exe
??? ? ???
~~~~~~

So this example shows that Powershell doesn't just pass data as-is on
those pipes like UNIX would.  It expects text, and alters the encoding
of the text according to `$OutputEncoding` when piping to an external
program.  Going to another cmdlet just stays unicode-to-unicode.

Ok, so the answer so far is:

 - If you want your code-page respected on input, use 
   an external program to generate the text, _OR_ use
   a cmdlet that lets you specify an encoding, and 
   specify it explicitly.
 - If you pipe the text to a powershell cmdlet, it will
   be in unicode.
 - If you pipe the text to an external program, it will 
   be encoded as specified in `$OutputEncoding`.

## Test 3: Default Output

Now that I can read in some code-page text, how does powershell put it
back out?

~~~~~~ powershell
> cmd.exe /c type 437.txt | Out-File pscore.txt  # On PWSH.EXE 
> cmd.exe /c type 437.txt | Out-File pswin.txt   # On WINDOWS POWERSHELL
~~~~~~

Now, let's try to just `Get-Content` them.  First on Powershell Core:

~~~~~~ powershell
> get-content .\pscore.txt  # UTF-8
█▐▐ Ω ▌▌█
> get-content .\pswin.txt  # UTF-16 + BOM
█▐▐ Ω ▌▌█
~~~~~~

It seems that Powershell Core looks for and respects a BOM, and is
assuming UTF-8 otherwise. Not a bad assumption in a modern program.

On Windows powershell, I get:

~~~~~ powershell
> get-content .\pscore.txt  # UTF-8
â–ˆâ–â– Î© â–Œâ–Œâ–ˆ
> get-content .\pswin.txt  # UTF-16 + BOM
█▐▐ Ω ▌▌█
~~~~~

It doesn't expect UTF-8 by default, clearly.  What if I prepend a
BOM to the UTF-8 version?  It's a simple matter of putting 0xEF, 
0xBB, 0xBF at the front:

~~~~~~ powershell
> $plusbom  = @( 0xef, 0xbb, 0xbf  ) + 
>> @( Get-Content -AsByteStream .\pscore.txt ) | 
>> %{ [Convert]::ToString($_,16) }
> write-bin.ps1 utf8bom.txt $plusbom
~~~~~~

Now, going to Windows powershell (with code-page still set to 437) can I
`Get-Content` utf8bom.txt?

~~~~~~ powershell
PS C:\Users\richa\tst> get-content .\utf8bom.txt
█▐▐ Ω ▌▌█
~~~~~~

**YES** I can!  So, powershell looks for and respects a BOM, and
otherwise goes to its default encoding.  As far as I can tell, the
default encodings are:

  - **Core** expects to see UTF-8 w/o BOM everywhere, unless a BOM
    tells it otherwise.
  - **Windows** outputs UTF-16 w/BOM, and when reading text that doesn't
    have a BOM it assumes it is latin-1 codepage 1252.
  - **Neither** care about your code page, it seems, unless an external
    program is involved.

As a point of interest, CMD.EXE's `type` command understands UTF-16
BOMs but not UTF-8 BOMs.  So, I can `type` pswin.txt even on codepage
437 with no problems, but I have to switch to codepage 65001 to see the
utf-8 files correctly (and even then, it prints an extra blank space
where the UTF-8 BOM is).

