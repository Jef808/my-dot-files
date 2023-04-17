#!/usr/bin/env sh

### Toggle the current locale and keymap between 'french canadian' and 'us english'.

locale=$(localectl status | grep 'System Locale' | awk '{ print $3 }' | sed 's/LANG=//')

case $locale in

    en_US.UTF-8)
        read -s -p "Enter sudo password: " sudoPW
        echo $sudoPW | sudo -S localectl set-locale fr_CA.UTF-8
        echo $sudoPW | sudo -S localectl set-keymap ca_multi
        echo 'Switched from en_US.UTF-8 to fr_CA.UTF-8'
        ;;
    fr_CA.UTF-8)
        read -s -p "Enter sudo password: " sudoPW
        echo $sudoPW | sudo -S localectl set-locale en_US.UTF-8
        echo $sudoPW | sudo -S localectl set-keymap us
        echo 'Switched from fr_CA.UTF-8 to en_US.UTF-8'
        ;;
    *)
        echo -n "Current locale is ${locale} but should be either en_US.UTF-8 or fr_CA.UTF-8"
        exit 1
        ;;

esac
