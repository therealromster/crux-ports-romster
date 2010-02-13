#!/bin/sh

echo "Uncompressing papi-3.5.0.tar.gz"
tar xzf papi-3.5.0.tar.gz

PDIR="/usr/ports/yhafri/papi/papi-3.5.0/src/perfctr-2.6.x"

cd /usr/src/linux-2.*
cp .config .config-papi

make mrproper
$PDIR/update-kernel

cp .config-papi .config

echo "!!! IMPORTANT !!!"
echo "You should either configure the kernel with the options:"
echo "   CONFIG_MODULES=y and CONFIG_MODVERSIONS=y"
echo "or completely without modules (CONFIG_MODULES=n)"
echo ""
echo "You should also enable at least CONFIG_PERFCTR," 
echo "CONFIG_PERFCTR_VIRTUAL, and CONFIG_PERFCTR_GLOBAL"
echo ""
echo "You may also select CONFIG_PERFCTR=m"
echo ""
echo "You also need to build your kernel APIC support"
echo "in order for hardware overflow to work. Test it with:"
echo "  grep PIC /usr/src/linux/.config"
echo "  /usr/src/linux/.config:CONFIG_X86_GOOD_APIC=y"
echo "  /usr/src/linux/.config:CONFIG_X86_UP_APIC=y"
echo "  /usr/src/linux/.config:CONFIG_X86_UP_IOAPIC=y"
echo "  /usr/src/linux/.config:CONFIG_X86_LOCAL_APIC=y"
echo "  /usr/src/linux/.config:CONFIG_X86_IO_APIC=y"

echo ""
echo ""
echo "press any key to continue..."
read

make menuconfig
make all
make modules_install

echo "Finish with this command:"
echo "      cp arch/i386/boot/bzImage /boot/vmlinuz"
echo "      cp System.map /boot"
echo "Edit: /etc/lilo.conf"
echo "run : lilo"

echo "Delete papi-3.5.0 temporary dir:"
echo "cd /usr/ports/yhafri/papi"
echo "rm -rf papi-3.5.0"


echo "...enjoy"


