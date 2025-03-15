#!/bin/bash          

# build script for Retro68 ROM

# Binary image names
SWTPC_BASIC=SWTPC_8K_BASIC_v22_7768.S19

# delete old outpout file
rm retro68.hex

# build TBUG. Output srec format, then convert to ihex
as02 TBUG_retro68.asm -sl
objcopy -I srec -O ihex TBUG_retro68.s19 TBUG_retro68.hex
mv TBUG_retro68.hex retro68.hex

# strip off last line of tbug image hex file so we can concatenate later on
sed -i '$ d' retro68.hex

# build rom copy routine
cp ./romcopy/romcopy.asm .
as02 romcopy.asm -s2l

# strip off last line (end record) from romcopy.hex
# so it can be concatenated later on in this script
sed -i '$ d' romcopy.hex

# SWTPC BASIC
#************

# copy SWTPC BASIC binary image from whammy repo
cp ./8KBasic/$SWTPC_BASIC .

#strip off s9 from end of BASIC file, because srec_cat doesn't like it
truncate -s-4 $SWTPC_BASIC

# relocate SWTPC BASIC to C100H
srec_cat $SWTPC_BASIC --offset=0xc100 > swtpc.s19

# convert SWTPC BASIC to Intel HEX
objcopy -I srec -O ihex swtpc.s19 swtpc.hex

# strip off last line of swtpc basic hex file so it can be concatenated
sed -i '$ d' swtpc.hex

# build final Intel hex image file
#*********************************

cat swtpc.hex >> retro68.hex
cat romcopy.hex >> retro68.hex

dos2unix retro68.hex	#strip off ctrl-Ms

# clean up
rm swtpc.hex 
rm swtpc.s19 
rm $SWTPC_BASIC
rm romcopy.hex
rm romcopy.asm
rm romcopy.lst
rm TBUG_retro68.s19



