---
title: Sudo support for iPhone version
image: /img/sudo-iphone.png
imagecaption: Sudo support for Pilot SSH on iPhone
---

# Sudo support for iPhone version

An update will soon be available for the iPhone version to add sudo support (and fix various bugs). It is something that a lot of people asked for, but it took some time to develop to make sure that it wa stable enough. Enabling sudo for a script will be as easy as transforming this:

> { "version": 1, "title": "Commands", "type":"commands", "values" : [ {"name":"Logs", "value":"", "command":".pilotssh/logs/logs"} ]}'

in:

> { "version": 1, "title": "Commands", "type":"commands", "values" : [ {"name":"Logs", "value":"", "command":".pilotssh/logs/logs", "sudo":true} ]}'

This will result in the following screen:

<img src="/img/sudo-iphone.png" />

Also, great news on the Android front: the release will come very soon! I am wrapping up the last bugs and updating the graphics, but it works really well :)
