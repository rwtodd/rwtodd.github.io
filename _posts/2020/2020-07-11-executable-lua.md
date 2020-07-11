---
layout: post
title: "Make Precompiled Lua Scripts Executable"
tags: [ lua ]
categories: [ computing ]
---
I didn't find this clearly spelled out anywhere in the 'luac' documentation,
but it's *really* easy to make precompiled lua scripts executable: just insert
a `#!/usr/bin/env lua` line above luac's binary output.

This works because `luaL_loadfilex()` will automatically skip the first line
of any file starting with '#'--even when loading precompiled chunks!

In Makefiles, you can automate the process:

``` make
my-script: my-script.lua
	(echo "#!/usr/bin/env lua"; luac -o - $<) > $@
	chmod +x $@
```

