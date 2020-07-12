---
layout: post
title: "Moving to Emacs: Common Scenarios"
categories: [ computing ]
tags: [ moving-to-emacs ]
---

As I move more of my work to emacs, I find myself comparing my edits
to the methods I would have used in vim.  Here are a few scenarios.

## Splitting and Joining Text ##

Imagine I have the following text:

```
ColorPalette=#000000;#AA0000;#00AA00;#00AAAA;#AAAAAA;#555555;
```

... and I want to split the entries, make edits, and re-roin them.  In
vim, I'd use a regex to split the line on semicolons
and then re-join them:

```
ma:s/;/^M/g<cr>
<<edit, and go to bottom line>>
:'a,.s/\n/;/<cr>
```

Note that, to type the `^M` on the first, line, I have to use
`^V<cr>`, and it is a little off-putting that I have to then go on to
use `\n` when putting the lines back together--another `^M` doesn't
work.

In emacs, the closest sequence I could discover was:

```
C-x r <space> a C-<space> C-e C-M-% ; <cr> C-q C-j <cr> !
<<edit, and go to bottom line>>
C-<space> C-x r j a C-M-% C-q C-j <cr> ; <cr> !
```

If we count the chords as single keys, then vim and emacs both have 25
keystrokes each.  The prevalence of chords in the emacs set certainly
make it *feel* like more work, though.

One **benefit** I get from the emacs method is that the regexp
replacement is interactive.  As a result, if I'm unsure that I've got
the regexp correct I can try it interactively before blindly applying
it with the exclamation-mark.  (*Edit*: I see that vim has an
interactive option with modifier '`c`', but I did not know of it, and
using it would add two keystrokes to both regexp replacements.)

In emacs, I found that I could eliminate some transient marking if I
use emacs' ability to `narrow` the buffer.  Here's that sequence:

```
C-<space> C-e C-x n n C-a C-M-% ; <cr> C-q C-j <cr> !
<<edit, then go to the start of the buffer>>
C-M-% C-q C-j <cr> ; <cr> ! C-x n w
```

... which gives only 23 keystrokes.

## Ephemeral Bookmarks ##

Often when browsing files in vim, I'll set `marks` at points I'll want
to refer to later.  Emacs has registers that can serve the same
purpose (as well as other purposes), but it is definitely more
verbose. Here is vim:

```
ma  ...  'a
```

... and here's emacs:

```
C-x r <space> a  ...  C-x r j a
```

That's 4 keystrokes versus 8.  I will say, though, that I really like
how emacs uses global registers.  That way, I can jump back to the
correct location even if I'm looking at another file at the time.
Vim's marks are per-buffer.

## Single-Character Searches ##

Often in vim I'll say "I want to change everything until the period at
the end of this sentence."  To do that, I hit '`ct.`', and type my
replacement.  The best option in emacs is:

```
M-z .
```

... which is 2 keystrokes instead of three (ignoring that this deletes
the period, making me re-type it).

A variation I often hit is: "I want to edit at the second t on this
line," which in vim equates to '`2ft`'.  In emacs, I found two options
which are both 4 keystrokes, though all the chording makes it *feel*
heavier than that:

```
C-s t C-s <cr>
```

or

```
M-2 M-z t C-y
```

The second one takes advantage of the way `zap-to-char` adds the text
to the kill-ring, so I can `yank` it right back into the buffer.

## Vim Text Objects ##

In vim, several times a day I will want to change the text inside some
quotes or parentheses.  Text-objects make this very concise: '`ci"`'
for the quotes case.  How can I do this in emacs?  The closest thing I
can find is:

```
M-z " M-- M-z "
```

... which really is not as convenient, especially since I also have to
re-type the quotes.  Without an extension or custom `defun`, I don't
see an easier equivalent in vanilla emacs.

``` emacs-lisp
(defun rwt/search-delimiters (dir delim other)
  "Search for `delim` either forward or backward based on `dir`,
   up to the end of the line. Every time you find `other`, search
   for additional `delim`.  This method handles nested delimiters
   as long as they are balanced."
  (let ((start-pt (point))
	(limit    (if (> dir 0)
		      (min (point-max) (1+ (line-end-position)))
		    (max (point-min) (1- (line-beginning-position)))))
	(count    1))
    (while (and (> count 0) (/= (point) limit))
      (let ((curch (char-after)))
	(cond ((char-equal curch delim) (setq count (1- count)))
	      ((char-equal curch other) (setq count (1+ count))))
	(forward-char dir)))
    (if (= count 0)
	(progn (when (< dir 0) (forward-char 1))
	       t)
      (goto-char start-pt)
      nil)))
	
(defun rwt/delete-around-point (arg delim)
  "Kills the region around the point, marked by delimiters,
   which may be any character.  If the character is a bracket,
   then matching brackets are assumed.  With C-u modifier, also
   erases the brackets."
  (interactive "*P\ncDelimiter: ")
  (let* ((other (or (cdr (assoc delim
			       '((?\{ . ?\}) (?\< . ?\>)
				    (?\( . ?\)) (?\[ . ?\]))
			       #'char-equal))
		    delim)))
    (when (rwt/search-delimiters -1 delim other)
      (let ((start-pt (+ (point) (if arg 0 1))))
	(forward-char 1)
	(when (rwt/search-delimiters 1 other delim)
	  (delete-region start-pt (- (point) (if arg 0 1)))
	  (goto-char start-pt))))))
```
