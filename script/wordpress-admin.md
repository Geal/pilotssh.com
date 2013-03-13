---
title: WordPress admin
image: /img/wordpress.png
link: https://github.com/Geal/PilotSSH-scripts/tree/master/wordpress
---

This script will display your WordPress website's domain and version.

## Backup the database

You can fully back up the database of your website. You do not need access to mysqldump, the script only uses PHP functions. It reads the wp-config.php file to find the database credentials.

## Restore the database

The script shows a list of the available database backups, and the corresponding date. You will be able to restore entirely the database in one touch. Be aware that it could take a long time if the database is huge.

## Upgrade WordPress versions

The app will automatically check if there is a new version of WordPress, and you will be able to upgrade directly. It creates a backup of all the files before upgrading

## Restore file backups

If the WordPress upgrade failed, you can restore all the old files directly, and restore a database backup afterwards, to get back the original website
