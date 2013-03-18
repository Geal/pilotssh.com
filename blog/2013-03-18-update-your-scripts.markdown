---
title: Update your scripts
---

# Update your scripts

I pushed fixes to the [Github repository of Pilot SSH scripts](https://github.com/Geal/PilotSSH-scripts):

* the WordPress script will not crash weirdly because of a JSON parsing issue
* the network interfaces script displays more information, and warns you if the netifaces Python module is not available
* the logs script shows the content of the /var/log/ folder, and you can browse subfolders to find other log files
* the index script will now parse the .pilotssh directory to find scripts, and generate a list dynamically. It requires script files without an extension, though

There is also a new script available to make DNS requests from the server. It handles A, AAAA, MX, NS and TXT records.

The Android version is in development, expect some updates in the following weeks.
