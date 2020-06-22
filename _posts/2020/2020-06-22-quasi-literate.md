---
layout: post
title: "Quasi-Literate Programming"
categories: [ computing ]
---

Consider a C function that performs a few steps, and has to deal with potential
errors at every step, and has to free any resources it may have acquired
properly for all code paths.  That's just about every C function there is!

## The GOTO Way ##
One popular way getting the cleanup correct is to `goto` a cleanup section at
the end of a function after noting an error:

``` cpp
bool myfunc(void) {
  char *buffer = malloc(...);
  if (buffer == NULL) {
     fputs(...);
     return false;
  }

  bool result = false;
  int fd = open(...);
  if (fd <0) {
     perror(...);
     goto cleanup;
  }

  if(!do_stuff(fd)) {
     fputs(...);
     goto cleanup2; 
  }

  if(!do_more_stuff(fd)) {
     fputs(...);
     goto cleanup2; 
  }

  result = true;

  cleanup2:
     close(fd);
  cleanup:
     free(buffer);
  return result;
}
```

We all know what `goto` is considered, but all-in-all this isn't hard to
understand once you know the pattern.  It's fiddly and error-prone, though.
It's *very* easy to adjust the code and forget to adjust the cleanup
sections.

## The Nested Way ##

We can get rid of the `goto` jumps by checking for success and nesting the
cases.  Let's see what it looks like:

``` cpp
bool myfunc(void) {
   char *buffer = malloc(...);
   bool result = false;
   if (buffer != NULL) {
      int fd = open(...);
      if(fd >= 0) {
         if(do_stuff(fd)) {
           if(do_more_stuff(fd)) {
              result = true;  /* everything succeeded! */
           } else {
            fputs(...);
           }
         } else {
            fputs(...);
         }
         close(fd);
      } else {
         perror(...);
      }
      free(buffer);
   } else {
      fputs(...);
   }
   return result;
}
```

... and now we see the benefit of the `goto`-version... at least the steps
formed an orderly, straightforward list, and the error-handling was co-located
with the error!  In the nested version, the error-handling is so far removed,
and spans so many nested levels, that you'll usually see comments in the
code pointing out which case is which.

But at least we aren't using unstructured jumps, right?

## Literate Programming ##

This comparison immediately reminded me of my experiments with literate
programming.  One of the benefits of the literate style is that you can
present the code in any order you want.  As a result, nesting in the code as
seen by the compiler tends to *increase*, but code you actually deal with in
your editor remains much flatter.  In fact, I can write the nested code
above, without having to deal with the nesting first-hand.  Let's see how:

``` cpp
bool myfunc(void) {
  char *buffer = malloc(...);
  bool result = false;
  if(buffer != NULL) {
     @<open the file and parse it@>
     free(buffer);
  } else {
     fputs(...);
  }
  return result;
}

@<open the file and parse it@>=
int fd = open(...);
if(fd >= 0) {
   @<parse the file@>
   close(fd);
} else {
   perror(...);
}

@<parse the file@>=
if(do_stuff(fd)) {
   @<do calculations on the parse@>
} else {
   fputs(...);
}

@<do calculations on the parse@>=
if(do_more_stuff(fd)) {
   result = true;  /* everything succeeded! */
} else {
   fputs(...);
}
```

By splitting out the code into independent chunks, I have kept the cleanup
in the same, nest-free chunk as the acquisition (the `open`/`close` and
`malloc`/`free` pairs are co-located).  In literate programming, you
split the code up for the sake of exposition, and typically most of the
named chunks would have a paragraph of text above them describing the action.
Even leaving out the commentary, as I have in this example, it seems there 
there may be a code-organization benefit to be had.

The downside here is that the last example isn't valid C code, and you need a
simple preprocessor to re-create the nested version for the compiler.  In
literate programming lore, this is called "*tangling*" the source.

I may give this a try: call it "quasi-literate programming," where I forget
about trying to make a typeset paper out of the code, and just use the
tangling process to enable more flexible notation of the code. 

