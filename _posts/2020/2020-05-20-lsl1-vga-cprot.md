---
layout: post
title: "Simplifying \"LSL1 VGA\"'s Age Protection"
categories: [games hacking]
tags: [ sci-game-patching ]
---

I've done [several]({% post_url 2020/2020-05-15-lsl1-cprot %})
[of]({% post_url 2020/2020-05-16-lsl2-cprot %})
[these]({% post_url 2020/2020-05-17-lsl3-cprot %})
[posts]({% post_url 2020/2020-05-12-colonels-beq %})
at this point, so simplifying these schemes has become pretty
routine.  I wondered if doing a VGA 256-color game would be
different, but it was not.

**As a reminder**: I don't really want to _bypass_ the age protection. I
like seeing it; it's a nostalgia thing.  I just don't want to have to get
the answer correct to get to the other side of it.

The patch I produced is at the bottom of this post.

## Getting Started

As always, I used the excellent [SCI Companion](http://scicompanion.com/)
software.  To spot the right script, I usually scan the text resources for
tell-tale strings.  This time, the background image for the quiz jumped out
at me, though... number 720:

![background image screenshot](/assets/2020/09/lsl1-vga-distinctive.jpg)

Almost always, the numbers in these games line up, so background 720 goes with script 720.

## The Protection Scheme

Looking at the script, I made the following observations:

  - This game uses the `LARRY.DRV` file for random digits just like LSL3 did.
  - The script is an event-driven state machine, like so many SCI scripts are.

Here's the code for when you get it wrong vs right (states 2 and 3), which I've
annotated with comments:

~~~~~~
(2
    (if (or (== local3 local4) (== local4 0))
        (localproc_082c local3 global138)
        (gLongSong3 number: 711 play: 127)
        (proc255_0 720 13 67 200 34 25 3)
        (++ local1)                          ;; count of correct answers
        (++ state)
    else
        (localproc_082c local3 global132)
        (gLongSong3 number: 712 play:)
    )
    (= cycles 22)
)
(3
    (if (not local407)
        (++ local407)           ;; flag to say you've missed one
        (if (== local1 4)
            (proc255_0 720 14)  ;; you got close so you can still play
        else
            (proc255_0 720 15)  ;; oops, don't miss again!
        )
    else
        (proc255_0 720 16 67 -1 19 70 280)  ;; you are a kid! game over
        (= global4 1)
    )
    (= cycles 22)
)
~~~~~~

I think I'll just replace the `(++ local407)` with `(++ local1)` and change
the `(== local1 4) ` to `(== local1 5)` so that that message
still works when you miss the last question.  The disassembly is for that
section of code is:

~~~~~~
06d5:82 0197      lal local407 
06d8:18           not 
06d9:30 0025      bnt code_0701 
06dc:c2 0197      +al local407   // CHANGE to C2 0001 
06df:8b 01        lsl local1 
06e1:35 04        ldi 4          // CHANGE to 35 05
06e3:1a           eq? 
06e4:30 000d      bnt code_06f4 
06e7:7a           push2 
06e8:38 02d0      pushi 2d0 // $2d0 sel_720
06eb:39 0e        pushi e // $e lsLeft
06ed:47 ff 00 04  calle ff procedure_0000 4 // proc255_0 
06f1:32 0027      jmp code_071b 
~~~~~~

From past experience, I'd normally use `C3 01` to increment `local1`.  However, the code I
was replacing was a 3-byte `C2 0197` variant. I wasn't sure if I could use `C2`
on `local1`, but I tried it successfully on both ScummVM and DosBox.

![screenshot of the patch](/assets/2020/09/lsl-vga-hexedit.png)

## The Patch

Just put [this 720.scr patch](/assets/2020/09/720.scr) as `720.scr` in
your game directory, and it should work.  I guess the VGA-era games use
`${NUMBER}.scr` instead of the `script.${NUMBER}` pattern of the EGA games.

It's nice that this change is
specific to the copy protection script, because it gives some confidence that
the overall game isn't broken by the change I made.  Also, none of the original
game assets were modified.  That's the nice thing about the SCI engine... it
looks for uncompressed overrides of the in-game scripts on the file-system.  If
you delete the patch file (or rename it), the game will resume checking your
answer.
