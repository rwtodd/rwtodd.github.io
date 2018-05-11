---
layout: post
title: "Decoding C:\$Recycle.Bin"
date: 2018-05-10 22:00
categories: perl windows
---

I got a call today that someone on a Windows 7 machine
could not open her Recycle Bin and needed a file from
it ASAP.  There was no time for me to travel to her
location, but with some effort I managed to get a
remote connection to the machine as an Administrator.

So, first step: see if the `C:\$Recycle.Bin\` folder
seems OK.  It does!  However, there are like 30 
subfolders, which I recognized as [SIDs][2].  So, off
to the registry (`HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList`) 
to find out which SID corresponds to her normal login.

Next: the view from the `$Recycle.Bin` folder doesn't have
the original file names or timestamps... and she had 422 files in there.  
Is there any way to decode them?
Fortunately, google
quickly found [the information I needed][3]: there are `$I` files
with the metadata about corresponding `$R` data files.  The metadata
files are in a fixed-length binary format.

But, with a remote connection to a random Win7 box, what would
be the easiest way to extract a bunch of binary metadata?
I considered my options.  I knew Win7 would have an older version
of Powershell on it, but dealing with encodings can be tricky. I thought
maybe I would write a Go program on my local machine, and then
transfer it to the remote machine.  It would work, but as I was thinking
about that option, I had a stroke of luck!  
Against all odds, **this machine had Perl 5.18 on it**.

I don't use Perl often enough to be fluent, but Stack Overflow
filled in the gaps. In a few minutes, I had a working script.  
The biggest stumbling block was that Perl wants to calculate
time from the UNIX epoch, and windows filesystems use an epoch
from the year 1601.  Once I recognized the problem, I was a quick
google search away from the right constants to use when adusting 
the timestamps. 

I put the script in my [small programs 2018][1] repo, 
for posterity.

The script outputs CSV, which I pulled into Excel, and I was able
to identify the urgent file based on her recollection of it. 

[1]: https://github.com/rwtodd/small_programs_2018
[2]: https://en.wikipedia.org/wiki/Security_Identifier

