#!/bin/bash
set -ex

# on raspbian, build the program and reboot to it

#./build.sh
./buildall

sudo rm -rf /boot/hex
sudo cp -r hex /boot
sudo cp ultibomicrobit-kernel-rpi3.img /boot
sudo cp ultibomicrobit-config.txt ultibomicrobit-cmdline.txt /boot
sudo cp /boot/ultibomicrobit-config.txt /boot/config.txt
sleep 2
sudo reboot
