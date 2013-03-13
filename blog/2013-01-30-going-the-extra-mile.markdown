---
title: Going the extra mile
---

# Going the extra mile

What is this silly idea of running scripts on the server? Where is my shell?
Couldn't you do like everyone and copy Prompt or iSSH?

Well, no.

Writing yet another shell application would have been quite easy. After all, that is the purpose of SSH and its libraries, and even if writing a terminal emulator might not be trivial, I could have taken inspiration from many pieces of code. And that is the safe thing to do: the shell is a familiar place, developers and sysadmins know how to use it, and that is the easiest access they have to their servers.

Well, it's not.

Mobile platforms have a fundamentally different user experience. Using a shell application might make sense with a tablet and a bluetooth keyboard, but with the touchscreen of s phone, that is a whole different matter. I cannot expect people to enjoy entering commands with their thumbs, and read a terminal on a small screen (Retina or not).

I needed to adapt the interface to the phones. Panic developers solved this in Prompt by autocompleting commands. This is a nice idea, because it reduces the time spent typing commands. Mobilwerx had another approach for Script Kitty (on Android): the app can store scripts, that you run on the server you want in one touch. This looks better, but it is not useful enough, because you still need to type the commands on your phone, and they are only stored on the phone.

I also checked out the common remote administration solutions like webmin. It could have been a good idea to use the mobile interface of Webmin. It is simple and easy to use. But I don't really like it from a security point of view, and deploying yet another site for server admin is a bit overkill.

Let's think of something simpler. Adapted to smartphone user interfaces. Customizable. Easy to install.

Generating a user interface from a web service is common in mobile applications. Maybe I could use that? But without deploying a webservice, I don't want to worry about its security. I know! Let's use scripts! Common, normal scripts. They could be written in any language! Writing and using scripts is an everyday task for system administrators, so they would feel at home with an application using server side scripts.

Those were my reasons and ideas to build Pilot SSH. The rest was easy: handling SSH connections, parsing the script results, generating the interface, storing safely the credentials...

I could have just open a text window and piped the keyboard through SSH to the server and be done with it. But by going the extra mile, trying to get better than what those old systems give me, I can build new, useful experiences. The world is full of old technology that needs to be adapte to the current world: payment interfaces, heating systems, transport, internet protocols... Which one do you want to hack?

