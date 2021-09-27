---
layout: post
title: "Cleaning Up Windows, Part 1"
categories: [ computing ]
tags: [ windows ]
---

## Initial Measurments

After a restart, I opened only the Task Manager and a Powershell terminal.  I had:

- 202 Processes (90 of which are all named `svchost`)
- 132 Running Services
- Memory:
  - 4 GB memory used (27 GB Free)
  - 6.0/36.4 GB Committed
  - 2.3 GB cached
  - Pools:  219 MB Paged / 369 MB Non-Paged

The top 15 memory-users (as given by:
 `get-process | Sort-Object WorkingSet -Descending | Select-Object -First 15`), are:

| Working Set | Name                             |
|-------------|----------------------------------|
| 224.48      | MsMpEng                          |
| 182.15      | explorer                         |
| 161.71      | SearchApp                        |
| 149.61      | OneDrive                         |
| 114.82      | WindowsTerminal                  |
| 101.29      | dwm                              |
| 101.09      | svchost                          |
|  99.49      | StartMenuExperienceHost          |
|  96.22      | Cortana                          |
|  87.59      | Registry                         |
|  87.47      | pwsh                             |
|  75.51      | IGCCTray                         |
|  75.46      | LockApp                          |
|  71.71      | TouchpointAnalyticsClientService |
|  70.83      | TextInputHost                    |

## Services

So, as an easy first step, let's make sure I need all the running services.

First up is Aarsvc_4c7f3.  That doesn't sound very useful.  The service description is: "Runtime for activating conversational agent applications."  No thanks!  Except, when I try to disable the service it complains.  The internet says I can disable it via the registry, so I do:

`Set-ItemProperty -Path "Registry::HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Services\AarSvc*" -Name "Start" -Value 4`

Next sketchy one is AVCTP service.  The actual (non-display) name starts with "bth" which makes me think it's for bluetooth.  Yep, the internet says it is.  I don't use bluetooth headphones with my laptop.  Disabled!

Same with Bluetooth Audio Gateway Service and Bluetooth Support Service.

There is a "Connected Device Platform User Service" which doesn't want to be disabled.  At this point I'm getting pretty annoyed at services not letting me turn them off.  Am I the Admin or not??  Anyway, its *also* related to bluetooth, if you can believe it.  So, I'm stopping it and disabling its non-user version "CDPSVC."  I'm setting the registry key manually for the user service, as a matter of principle.

Next up is "Connected User Experiences and Telemetry"... which I suspect is really just about "Telemetry", expecially since the non-display name is "DiagTrack."  Well, what do you know?  It lets me disable it.

Next interesting one is "DPS" or "Diagnostic Policy Service."  On the details view of the task manager, it eats 17MB of ram.  It says it enables problem detection and troubleshooting on Windows.  I... have never had the troubleshooters actually fix anything except maybe as a quick way to reset a network interface.  Disabled (and the Diagnostic Service Host also).

Next up is the FMService64.exe "Fortemedia APO Control Service."  Basic web searches can't seem to tell me what the "APO" stands for, but theoretically this one is about sound.  I'm leaving it alone for now.

Next up:  HP Touchpoint Analytics Service. That's an easy one... gone!  HP Comm Recovery?  Gone!   HP Diagnostics HSA Service?  Gone!  HP Lan/VWAN Switching Service?  Gone!

It's strange that the "Manual" LxssManager service is running, since it says it's for running ELF binaries.  That sounds like a WSL1 thing, and I run WSL2... and haven't run either during this session.
