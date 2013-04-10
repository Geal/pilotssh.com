---
title: Pilot SSH, soon on Android
---

# Pilot SSH, soon on Android

Following the release of the [iPhone version of Pilot SSH](https://itunes.apple.com/us/app/pilot-ssh/id591678815?l=fr&ls=1&mt=8) and the encouraging messages I received, I started working on the Android version. It builds upon what I learned building and promoting the iphone version. For now, the goal is to obtain the same features on both versions, then add new ones on both platforms at the same time.

![Manage your processes](/img/android.png)

## Security

I used again [SQLCipher](http://sqlcipher.net/), the open source encrypted library based on SQLite. This will keep your server credentials safe. Some users were concerned about putting their server's password in the application, so it also supports public key authentication.

## Key generation

![Copy easily your public key string](/img/key-generation.png)

The iPhone version can use keys that you will transfer by iTunes, or by opening files with the .dsa or .rsa extension. On the Android version, it will be easier: the application can generate directly 2048-bits RSA keys, and store them in the internal storage of the applications. they cannot be accessed by other applications. You can then export the public part and upload it to your server.

## Beta test

People are already testing the application, finding bugs and writing great scripts. If you want to be part of the beta testers, please <a href onmouseover="this.setAttribute('href', 'moc.hsstolip@tcatnoc:otliam'.split('').reverse().join(''))">contact me</a>.

In the mean time, if you have nice scripting ideas or suggestions about the future release, don't hesitate to tell me all about it!
