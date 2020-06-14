---
layout: post
title: "Setting Up Windows"
categories: [ computing ]
tags: []
---

I recently installed Windows from scratch, and thought I
would document the steps I took here.  There are a number of little things I
like to have on all my machines.

## Windows Configuration ##

### Installation Setup Notes ###

- It's key to **not** connect to Wi-Fi when it begs you to, so that you can
  force it to give you a local account.  That way, you have control over your
  user directory name.
- By default, Windows wants to send a lot of telemetry.  Turn those off as
  desired.

## Post-Install  ##

### First Steps ###

- Connect to Wi-Fi
- In Settings:
  - Change the machine name
  - Connect your user to your microsoft account
  - Set mspaint as the default picture viewer
  - Make sure Windows activated as Pro, not Home
	- If necessary, put in the Pro key and re-activate
  - Check for updates and run them
  - Turn on Bitlocker, save the key in OneDrive
- Run `main.cpl` and turn off the trackpad when a mouse is attached
- Run `sysdm.cpl` and give 4% of the drive to system restore points
- Run `desk.cpl` and set the brightness/night-light features as desired

### Device Manager ###

- Run `devmgmt.msc` as Administrator
- Look for any unknown devices, and find the missing drivers
  - During this install, I only had one: `AMD AS4`

### OneDrive ###

- Turn on Desktop, Pictures, Documents backup
- Run `taskschd.msc` and check that the scheduled task(s) for OneDrive look
  reasonable.  For this install, it looks fine (runs once a day if an
  internet connection is available).
  
### Set Up Mapped Drives ###

I like to set a couple mapped drives for areas of my hard drive so that they
are easier to visit.  For example, my `J:` drive maps to
`C:\Users\rwtodd\source\repos`, so most git repositories I have checked out
will live directly under the `J:` drive.  It's very convenient, especially
since `cmd.exe` doesn't support a tilde (~) shortcut to `%userprofile%`.
Powershell *does* have the tilde, but for many uses I prefer to live in the
old-fashioned DOS prompt.

So there are two main ways to accomplish this.  One is to list the drives in
the registry under `HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\DOS
Devices`.  Another is to create a local logon script
([reference](https://support.microsoft.com/en-us/help/324803/how-to-assign-a-logon-script-to-a-profile-for-a-local-user-in-windows)).
I never use the registry option because it only seems to work for `HKLM`,
when these drives are specific to my user.

- Create the necessary `%systemroot%\System32\Repl\Import\Scripts` 
  directory as Administrator.
- Put the login script below in that directory.
- Run `lusrmgr.msc` to set the login script for my user, using only the file
  name since the path is assumed.
- Log out and in again, to test it.

``` batchfile
subst H: /D
subst J: /D
subst H: C:\Users\rwtodd
subst J: C:\Users\rwtodd\source\repos
```

## Install Programs ##

During the setup I'm describing here, I downloaded software packages
manually, but next time I'm considering trying something like
[Chocolatey](https://chocolatey.org/).  We'll see.

### Edge-Chromium ###

- For this, just open Edge "classic", and it will almost certainly tell you
  about chredge.  Install it.
- Sign in to chredge, and it should sync up your extensions etc.
- Run `taskschd.msc` and adjust the Edge update tasks so that they run once a
  week instead of once an hour (!!)
  
### Windows Terminal ###

- This comes from the Microsoft Store.
- Add EGA colors as described
  [in my older post]({% post_url 2020/2020-06-12-windows-terminal-ega %})
- Set `cmd.exe` as the default shell
- Set %PROMPT% to `[$E[32m$P $E[1;33m$T$E[0m]$_$$$S` (see
  [ss64.com](https://ss64.com/nt/prompt.html) for reference)

### Git ###

- Download the Git installer from
  [https://git-scm.com/download/win](https://git-scm.com/download/win)
- When it asks about an editor, point it to vim.
- Run the global config:

```
git config --global user.name "Richard Todd"
git config --global user.email "rwtodd@noreply.users.github.com"
```

### SSH Keys ###

- It looks like OpenSSH is just installed by default on windows these
  days.  If it's not, go to "optional windows features" and add it.
- Put public and private keys in `%userprofile%\.ssh`.
  - It's kind of picky about line endings and final newlines, so try it out
    and adjust the key-files as needed.

### Emacs ###

- Download the latest emacs, and copy it to `C:\Program Files\Emacs`
- Add `C:\Program Files\Emacs\bin` to `%PATH%`
- Clone `git@github.com:rwtodd/emacs-dot-d` to `%APPDATA%\.emacs.d`
- In emacs:
  - `M-x package-refresh-contents`
  - `M-x package-install-selected-packages`
  - `C-u 0 M-x byte-recompile-directory` on `.emacs.d\rwt-lisp`

### Misc Apps ###

Most of the data I care about is either in a backed-up git repository, or on
my OneDrive. So, all I have left to do is to install the applications I use
on Windows regularly.  These are:

- [Visual Studio Community Edition](https://visualstudio.microsoft.com/)
- [Powershell 7](https://github.com/PowerShell/PowerShell/releases),
  usually via `dotnet tool install --global PowerShell`, since I have dotnet
  core installed as part of VS.
- [7-zip](https://www.7-zip.org/)
- [Foobar2000](https://foobar2000.org)
- [SumatraPDF](https://www.sumatrapdfreader.org/)
  - Set it as the default PDF and Epub reader
- [ScummVM](https://www.scummvm.org/)
  - Set the savegame directory to OneDrive
- [DosBox](https://www.dosbox.com/)
- [Mesen NES Emulator](https://mesen.ca/)
- [ImageMagick](https://imagemagick.org/index.php)
  64-bit 8-bit DLL version
  - Also get [Ghostscript](https://www.ghostscript.com/) GPL version 
	for magick.exe PDF support
- [FileZilla](https://filezilla-project.org/) for FTP
- [MS Office](https://office.com)
- MS Todo from the Microsoft Store

## Command-Line Reference ##

The following are quick ways to get to various controls in Windows, rather
than clicking through everything.  I use them often when first setting up a
machine.

| Command            | Controls                             |
|--------------------|--------------------------------------|
| appwiz.cpl         | Classic uninstall program menu       |
| control admintools | Opens a list of administrative tools |
| control desktop    | Background image                     |
| desk.cpl           | Screen resolution/brightness         |
| lusrmgr.msc        | Local user manager                   |
| mmsys.cpl          | Sound controls                       |
| sysdm.cpl          | System Properties                    |
| taskschd.msc       | Task Scheduler                       |


<!-- Local Variables: -->
<!-- fill-column: 77 -->
<!-- End: -->
