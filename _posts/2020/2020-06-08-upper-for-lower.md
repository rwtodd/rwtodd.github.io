---
layout: post
title: "For Lower-Case, Use the Upper-Case Option"
categories: [ computing ]
tags: []
---

I wanted to change the format of the clock in my XFCE environment from
24-hour format to 12-hour with AM/PM.  It turns out the clock allows
you to specify a `strftime`-looking format string, so you can have
whatever you want. Great!  While looking up the AM/PM setting, I
noticed this:

| %p | locale's equivalent of either AM or PM; blank if not known |
| %P | like %p, but lower case                                    |

So, the **upper case** option is like the **lower case** option,
except the **upper case** option gives you **lower case**.  You can't
make this stuff up!

Looking into it, I found on the `strftime` man page that `%P` is a GNU
extension, so it's clear what happened: the `%p` option came first,
and used upper case.  Then later, GNU wasn't free to change it when
they added the lower case version.

That's a perfect example of how a design gets less coherent as small
decisions accrete over time.

## An Extended Tangent  ##

Thinking about the sequence of events made me curious about when
`date` got formatting options in the first place.  I started looking
through historical source archives.  Here are the ones I checked:

| Version                 | Date | Formatting?       |
|-------------------------|------|-------------------|
| [v1][1]                 | 1971 | No                |
| [PWB 1.0][8]            | 1977 | Yes               |
| [System III][4]         | 1981 | Yes (from PWB)    |
| [pdp11v][9]             | 1982 | Yes               |
| [v8][2]                 | 1985 | No                |
| [SunOS 2.0][15]         | 1985 | No                |
| [SunOS 3.2][16]         | 1986 | Yes (from SysV)   |
| [4.3BSD-Quasijarus][11] | 1986 | No                |
| [4.3BSD-UWisc][13]      | 1987 | Yes (from SunOS?) |
| [4.3BSD-Reno][12]       | 1990 | No                |
| [Plan 9][3]             | 1992 | No                |
| [GNU Coreutils][7]      | 1992 | Yes (strftime)    |
| [386BSD 0.1][10]        | 1993 | Yes (strftime)    |

### PWB 1.0 ###

So, as far as I can tell, PWB 1.0 Unix was the first Unix to have date
formatting options.  According
to [the man page](https://www.tuhs.org/cgi-bin/utree.pl?file=PWB1/usr/man/man1/date.1),
the following flags are supported:

| Flag | Meaning                          |
|------|----------------------------------|
| H    | hour - 00 to 23                  |
| M    | minute - 00 to 59                |
| S    | second - 00 to 59                |
| a    | abbreviated weekday - Sun to Sat |
| d    | day of month - 01 to 31          |
| h    | abbreviated month - Jan to Dec   |
| j    | julian date - 001 to 366         |
| m    | month of year - 01 to 12         |
| n    | insert a newline character       |
| r    | time in AM / PM notation         |
| t    | insert a tab character           |
| w    | day of week - Sunday = 0         |
| y    | last 2 digits of year - 00 to 99 |


### System III  ###

System III seems to have gotten its implementation from PWB 2.0, and
the only added options from the ones given above are `%T` as a shorthand
for `%H:%M:%S`, and `%%` as a way to produce a percent-sign.

### PDP11v ###

I don't have much information on this Unix, but it appears to be a
pretty early one.  I noted its formatting code is similar to System
III, so I'm guessing that's the origin, but I don't know for certain.

### SunOS ###

I don't see the source for SunOS 2.0 anywhere, but 
in the [tapes for SunOS 2.0][13] (1985), there's nothing in
the man page about formatting.  Here's the top of the man page:

``` troff
\" @(#)date.1 1.2 85/04/04 SMI; from UCB 4.1
.SH NAME
date \- display or set the date
.SH SYNOPSIS
.B date
.RB "[ \fB\-u\fP ] [ yymmddhhmm [ " . "ss ] ]"
.SH DESCRIPTION
.IX "date command"  ""  "\fLdate\fP \(em date and time"
.IX display  date
.IX display  "date and time"
.IX display  "time and date"
.IX time  "display date and"
\fIDate\fP displays the current date and time when used without an argument.
.LP
```

**However**, in the binary [tapes for SunOS 3.2][14] (1986), we can see the `date` binary itself, with embedded strings:

```
@(#)date.c 1.2 86/08/25 SMI
<<binary trash elided>>
JanFebMarAprMayJunJulAugSepOctNovDecSunMonTueWedThuFriSat
<<binary trash elided>>
usage: date [-u] [+format] [yymmddhhmm[.ss]] [-a sss.fff]
```

... and elsewhere in the tapes I found the manual source which clearly
cites System V:

``` troff
TH DATE 1V "16 July 1983"
.SH NAME
date \- display or set the date
.SH SYNOPSIS
.B date
[ \fB\-u\fR ]
[ \fB\-a\fR [\fB\-\fR]\fIsss\fB.\fIfff\fR ]
[ \fIyymmddhhmm\fR[\fB.\fIss\fR] ]
[ \fB+\fIformat\fR ]
.SH SYSTEM V SYNOPSIS
.B date
[ \fB\-u\fR ]
[ \fB\-a\fR [\fB\-\fR]\fIsss\fB.\fIfff\fR ]
[ \fImmddhhmm\fR[\fIyy\fR] ]
[ \fB+\fIformat\fR ]
.SH DESCRIPTION
```

... so it seems that some time between those two versions, SunOS got
an infusion of System V ideas, and got date formatting along with it.

### BSDs ###

BSD was late to get on the `date`-formatting train.
Berkley's [4.3BSD-Quasijarus date.c][11] from
1986 doesn't do any formatting, and neither does [4.3BSD-Reno date.c][12]
from 1990.  However, in [4.3BSD-UWisc date.c][13] I see some
formatting functionality behind `#ifdef UW`:

``` C++
#ifdef UW
static char *usage = "usage: date [-n] [-u] [+format] [yymmddhhmm[.ss]]\n";

#define MONTH   itoa(tp->tm_mon+1,cp,2)
#define DAY     itoa(tp->tm_mday,cp,2)
#define YEAR    itoa(tp->tm_year,cp,2)
#define HOUR    itoa(tp->tm_hour,cp,2)
#define MINUTE  itoa(tp->tm_min,cp,2)
#define SECOND  itoa(tp->tm_sec,cp,2)
#define JULIAN  itoa(tp->tm_yday+1,cp,3)
#define WEEKDAY itoa(tp->tm_wday,cp,1)
#define MODHOUR itoa(h,cp,2)
#define ZONE    itoa(tz.tz_minuteswest,cp,4)
#define ROMAN   roman(tp->tm_mon,cp)
#define dysize(A) (((A)%4)? 365: 366)

char    month[12][3] = {
	"Jan","Feb","Mar","Apr",
	"May","Jun","Jul","Aug",
	"Sep","Oct","Nov","Dec"
};

char	days[7][3] = {
	"Sun","Mon","Tue","Wed",
	"Thu","Fri","Sat"
};

char	*long_days[] = {
	"Sunday","Monday","Tuesday",
	"Wednesday","Thursday","Friday","Saturday"
};

char	*dst_type = "nuawme";
char	*itoa(),*roman();

#else
static	char *usage = "usage: date [-n] [-u] [yymmddhhmm[.ss]]\n";
#endif UW
```

This code bears a strong resemblance--note the `#define`s for `HOUR`
and friends--all the way back to [the PWB 1.0 code][8], so I assume it
was copied from somewhere and not original.  From what little I know,
I bet the code came from SunOS, since the UWisc distribution also
pulled in NFS.

The flags are somewhat expanded from the other sets I've seen.  I
especially like the `%R` option to give the month as a roman numeral!
Here's a summary of what I see in the code:

| Flag | Meaning                                    |
|------|--------------------------------------------|
| %    | literal %                                  |
| n    | newline                                    |
| t    | tab                                        |
| m    | month (2-digit)                            |
| d    | day (2-digit)                              |
| y    | year (2-digit)                             |
| D    | like %m/%d/%y                              |
| H    | hour (2-digit)                             |
| M    | minute (2-digit)                           |
| S    | second (2-digit)                           |
| R    | month as a roman numeral                   |
| T    | like %H:%M:%S                              |
| j    | julian date                                |
| w    | weekday as a 1-digit number                |
| r    | 12-hr version of %H:%M:%S (AM\|PM)         |
| h    | first three letters of month (Jan,Feb,etc) |
| A    | full day name (Monday, Tuesday, etc)       |
| a    | first three letters of weekday             |
| z    | time zone                                  |
| s    | dst_type                                   |

The [man page source][14] doesn't give a source for the idea or the
code, only saying that the function is a "local mod":

``` troff
.SH "LOCAL MODS"
Added the "+format" option.
```

Looking at further BSDs, I can see in [386BSD 0.1's date.c][10] from
1992 that it has formatting, and uses `strftime` for formatting like
modern systems.

## What about `%P`? ##

Ok, so I've traced the history of `date (1)` formatting, but what
about that whacky `%P` option?  For that, I first checked the
version control history of GNU coreutils.  There, I can see the the
`%P` option [was documented in 2002][5], but even the first version of
`date.c` from 1992 [uses `strftime`][7].  A search in glibc's
version history shows that the captial-for-lower-case `%P` option was
added to `strftime` [on December 12, 1996][6].


[1]: http://man.cat-v.org/unix-1st/1/date
[2]: http://man.cat-v.org/unix_8th/1/date
[3]: http://man.cat-v.org/plan_9/1/date
[4]: https://www.tuhs.org/cgi-bin/utree.pl?file=SysIII/usr/src/man/man1/date.1
[5]: https://github.com/coreutils/coreutils/commit/154e260c9d61264ea99a0375f36de76868264a31
[6]: https://sourceware.org/git/?p=glibc.git;a=commitdiff;h=f8b87ef0d47ea0db7304a9c59d5479aebdaea347
[7]: https://github.com/coreutils/coreutils/blob/ccbd1d7dc5189f4637468a8136f672e60ee0e531/src/date.c
[8]: https://www.tuhs.org/cgi-bin/utree.pl?file=PWB1/sys/source/s1/date.c
[9]: https://www.tuhs.org/cgi-bin/utree.pl?file=pdp11v/usr/man/u_man/man1/date.1
[10]: https://github.com/386bsd/386bsd/blob/0.1/usr/src/bin/date/date.c
[11]: https://github.com/abs0/4.3BSD-Quasijarus/blob/master/bin/date.c
[12]: https://www.tuhs.org/cgi-bin/utree.pl?file=4.3BSD-Reno/src/bin/date/date.c
[13]: https://www.tuhs.org/cgi-bin/utree.pl?file=4.3BSD-UWisc/src/bin/date/date.c
[14]: https://www.tuhs.org/cgi-bin/utree.pl?file=4.3BSD-UWisc/man/man1/date.1
[15]: https://winworldpc.com/product/sunos/2x
[16]: https://winworldpc.com/product/sunos/3x
