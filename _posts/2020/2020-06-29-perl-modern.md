---
layout: post
title: "A Post-Modern Language, Now with Modern Defaults!"
tags: [ ]
categories: [ computing ]
---

I just re-read [Larry Wall's 1999 Linux World Talk][1], entitled:

> Perl, the first postmodern computer language

... which, if you haven't heard/read it yet, focuses quite a bit on 
distinctions between Modernism and Postmodernism, and how Perl is decidedly
postmodern.  It's a cool talk; check it out.

So, imagine my **horror** when I scrolled back up to the top and saw the Perl
7 announcement's tag line (over on the right):

![irony, the irony](/assets/2020/11/postmod-mod.png)

> Perl 5 with modern defaults

Let that sink in.

Looking at the [guac talk][2] is also a little concerning.  I know it's
technically not guidance on the direction Perl itself is going, but it tells
you what the current leader is interested in (and the words 
"[remove syntax from the language][3]" in the Perl 7 announcement sound more
ominous in that context).

I noted in a [recent post]({% post_url 2020/2020-06-27-building-perl-1 %})
that Perl 5.30 runs almost all of the Perl 1 unit tests successfully.  It's
kind of sad that Perl 7 is likely to run none of them unmodified.  Zero
percent.

I have made [a post on perlmonks][4], to collect some wisdom on the topic.

[1]: https://www.perl.com/pub/1999/03/pm.html/
[2]: https://www.youtube.com/watch?v=sTEshbh2lYQ&t=8s
[3]: https://www.nntp.perl.org/group/perl.perl5.porters/2020/06/msg257565.html
[4]: https://perlmonks.org/?node_id=11118665
