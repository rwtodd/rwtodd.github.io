---
layout: post
title: "Useful git cmd examples"
tags: [ ]
categories: [ ]
---

## Git Worktrees ##

This is a great feature I only recently discovered.  You can check out
multiple trees from the same `.git` metadata, instead of cloning multiple
times.

``` bash
git worktree add <path> <hash/branch/tag/etc>
```

## Git Log Exploration ##

I don't have to go through logs often, so I always have to look everything up
again, **every** time.  Here are some examples for my own reference:

### Abbreviated Logs ###

```
git log --format=oneline
git log --format=oneline --until=1991-01-01
```

