mkdir ~/repos
cd ~/repos

git clone https://gitlab.com/babdulkarim/dotfiles

git config --global user.email "boogs@venatores.group"
git config --global user.name "Bugi Idris"

sudo -s
# /dev/sdX empty partition without filesystem that will be
# cryptrypted
cryptsetup luksFormat /dev/sda crypt
cryptsetup luksOpen /dev/sda crypt

mount -t btrfs /dev/mapper/crypt /mnt

btrfs subvolume create /mnt/root
btrfs subvolume create /mnt/boot
btrfs subvolume create /mnt/home
btrfs subvolume create /mnt/gnu
btrfs subvolume create /mnt/data
btrfs subvolume create /mnt/log

btrfs subvolume snapshot -r /mnt/root /mnt/root-blank

umount /mnt

mount -o subvol=root /dev/mapper/crypt /mnt

cd /mnt
mkdir home
mkdir gnu
mkdir data
mkdir var/log -p
mkdir boot

mount -o subvol=home /dev/mapper/crypt home
mount -o subvol=gnu /dev/mapper/crypt gnu
mount -o subvol=data /dev/mapper/crypt data
mount -o subvol=log /dev/mapper/crypt var/log
mount -o subvol=boot /dev/mapper/crypt boot

mount /dev/sda5 /mnt/boot

herd start cow-store /mnt

cp /etc/configuration/desktop.scm /home/guest/repos/dotfiles/.config/guix
cd /home/guest/repos/dotfiles/.config/guix
chown guest:users desktop.scm
chmod u+w desktop.scm

cryptsetup luksUUID /dev/sda

guix system init /home/guest/repos/dotfiles/.config/guix/desktop.scm /mnt
