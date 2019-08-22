main: test.s
	as test.s -o test.o
	ld -Ttext 0x7c00 --oformat=binary test.o -o test.bin
	#dd if=test.bin of=floppy.img
	qemu-system-i386 -hda test.bin -hdb debian.qcow2

