---
layout: post
title: "An EGA Color Scheme Windows Terminal"
categories: [ computing ]
---

I like old-school EGA/VGA terminal colors, and tend to add them to
whatever terminal I am using.  For example, I have a github repo
for [xfce4-terminal vga colors][1] for when I run Linux.  Today, I set 
up my [Windows Terminal][2] with the classic theme.

To do so is easy; one just edits the `profiles.json` file in 
`%LOCALAPPDATA%\Packages\Microsoft.WindowsTerminal_8wekyb3d8bbwe\LocalState`.
First, in the "schemes" section, I added the colors:

``` json
"schemes": [
    {
      "name": "EGA",

      "cursorColor": "#FFFFFF",
      "selectionBackground": "#FFFFFF",

      "background": "#000000",
      "foreground": "#AAAAAA",

      "black": "#000000",
      "blue": "#0000AA",
      "cyan": "#00AAAA",
      "green": "#00AA00",
      "purple": "#AA00AA",
      "red": "#AA0000",
      "white": "#AAAAAA",
      "yellow": "#AA5500",
      "brightBlack": "#555555",
      "brightBlue": "#5555FF",
      "brightCyan": "#55FFFF",
      "brightGreen": "#55FF55",
      "brightPurple": "#FF55FF",
      "brightRed": "#FF5555",
      "brightWhite": "#FFFFFF",
      "brightYellow": "#FFFF55"
    }
],
```

Then, in the "profiles" section, I made `EGA` the default for all
themes:

``` json
"defaults": {
      // Put settings here that you want to apply to all profiles.
      "colorScheme": "EGA"
},
```
... and that was it!

[1]: https://github.com/rwtodd/vga-xfce4-terminal
[2]: https://github.com/Microsoft/Terminal
