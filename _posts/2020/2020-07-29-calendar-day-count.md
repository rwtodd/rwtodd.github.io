---
layout: post
title: "Calendar Day Counts"
tags: [ paper lua ]
categories: [ computing ]
---
Just for laughs I made a [Discordian Calendar][1] module in lua recently.
One of its functions is to compute the number of days from a user-given date
until the "X-Day" when the end of the world will occur. Or not. As it
happens, this date is in the year 8661 (1998 upside-down!).

My first cut at the problem was to simply loop through the years, being
careful to check for leap-years along the way, and count up the days. I also
checked the code for the common linux **ddate** utility, which does the same.
But, as I mentioned, the date is more than 6 millennia from now, and I wanted
to do better.

I found a nice closed-form way to count days between arbitrary dates,
including Gregorian-compatible leap years, with no loops whatsoever.  Here's
the relevant line of code:

``` lua
local ceil = math.ceil
local total = (365 * (year_e - year_s) + day_e - day_s
	+ ceil(year_e/4)   - ceil(year_s/4)
	- ceil(year_e/100) + ceil(year_s/100)
	+ ceil(year_e/400) - ceil(year_s/400))
```

The writeup is formatted with groff+eqn here: [calendar.pdf][2]

(if you care, lua module is part of my new but growing [strength][3]
collection of libraries)

[1]:  https://en.wikipedia.org/wiki/Discordian_calendar 
[2]:  /assets/2020/12/calendar.pdf
[3]:  https://github.com/rwtodd/strength
