---
layout: post
title: "Lua GCs Blocks and Functions"
tags: [ lua ]
categories: [ computing ]
---
I did a brief experiment today to make sure Lua garbage-collects blocks and
functions.  It turns out: **it does**.  That's good news, because it opens the
door for scripts that JIT data into Lua code for the interpreter to run
directly, and then throw the code blocks away.

``` lua
-- let's make a TON of loaded strings...
for x=1,300000 do
	local v =
		load("function dummy(n) print(n+" .. x .. ") end " ..
			"dummy(" .. x .. ")")
	v()
end
dummy(123) -- can call the function from the program, too
io.read('l')
```

This script runs in constant memory, and tests three things I wanted to make
sure I understood:

1. Blocks are garbage-collected
2. Redefinitions of the same function name make the old version collectible
3. Global functions defined in loaded blocks are accessible outside the block
   (as long as I don't put a custom environment on the `load` call)

If any of those things weren't true, I'd expect to see memory grow.
