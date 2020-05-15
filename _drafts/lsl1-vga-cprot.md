---
layout: post
title: "Simplifying \"LSL1 VGA\"'s Age Protection"
categories: [games hacking]
tags: [ sci-game-patching ]
---

I spotted the quiz by the number on the background PIC 721

This has LARRY.DRV just like LSL3 did.

Here's the code for when you get it wrong vs right:

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
					(++ local407)      ;; flag to say you've missed one
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


I think I'll just replace the (++ local407) with (++ local1) and change the (== local1 4)  to (== local1 5) so that that message
still works when you miss the last question.

Disassembly is:

 06d5:82 0197            lal local407 
  06d8:18                 not 
  06d9:30 0025            bnt code_0701 
  06dc:c2 0197            +al local407   // RWT: change to c3 01 and a 1-byte NO-OP?  or can it be c2 0001 ?
  06df:8b 01              lsl local1 
  06e1:35 04              ldi 4           // RWT change to 35 05
  06e3:1a                 eq? 
  06e4:30 000d            bnt code_06f4 
  06e7:7a               push2 
  06e8:38 02d0          pushi 2d0 // $2d0 sel_720
  06eb:39 0e            pushi e // $e lsLeft
  06ed:47 ff 00 04      calle ff procedure_0000 4 // proc255_0 

  06f1:32 0027            jmp code_071b 

I tried C2 0001 for the add, since that meant I didn't need to find a NO-OP or shift the remaining code around. I checked in on ScummVM
and DosBOX, and it worked in both cases.

