---
title: Frequently Asked Questions
---

# Frequently Asked Questions

## Security

* Is my data safe?
Pilot SSH uses Zetetic's SQLCipher, a library providing a encryption wrapper to SQLite. It uses AES 256 to encrypt the data, and has been thoroughly audited.

* Do I need to set the application's password?
The application's password is used to encrypt the database. Without it, the application will not launch. If you do not want to enter a password every time you launch the application, do not set it, or set it to "secret", the default database password.

* Do I need to enter my server's password?
No, you do not have to store the server's password. If the server's password is not set, the app will ask you about it on every connection.

* How do I add a private key?
You can add it through iTunes file sharing. You can also "open" files with .rsa or .dsa extension through Pilot SSH.

* How do I set the private key's password?
You can use the normal password field to set the private key's password. Otherwise, the app will ask you about it on every connection.

## Scripts

* I want to write my script in language X, is it supported?
* You can use any language you want to develop the scripts, as long as they follow <a href="https://github.com/Geal/PilotSSH-scripts/blob/master/API.md">the API</a>.

* I need to use sudo in my script, how can I do that?
Please wait for future versions, it is <a href="https://github.com/Geal/PilotSSH-scripts/issues/7">currently in development</a>.

## General

* When will Android be supported?
It is currently in development, please consult the <a href="https://github.com/Geal/PilotSSH-scripts/issues/10">Github issue</a> for more information.

* Where can I report bugs?
* Please write a <a href="https://github.com/Geal/PilotSSH-scripts/issues">Github issue</a>.
