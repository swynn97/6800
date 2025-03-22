#!/bin/bash          

# Build script for Retro68 ROM that includes a image of TSC 4K MicroBasic
# BASIC is concatenated onto the end of TBUG hex file as is based on a pre-built binary.
#
# Note, this build does not have a separate 'romcopy' program, but rather this is
# internal to TBUG (unlike the SWTPC 8K BASIC version of this script)
#
# Also, check TBUG soure for any conditional switches to make sure all the correct
# code is included!
#

# TSC binary image name
MICRO_BASIC=4KBasic_Mot_Asm.s19

# delete old outpout file
rm retro68.hex

# build TBUG. Output srec format, then convert to ihex
as02 TBUG_retro68.asm -sl
objcopy -I srec -O ihex TBUG_retro68.s19 TBUG_retro68.hex
mv TBUG_retro68.hex retro68.hex

# strip off last line of tbug image hex file so we can concatenate later on
sed -i '$ d' retro68.hex

# TSC MicroBasic
#***************

# copy MicroBasic binary image from 4KBasic directory
cp ./4KBasic/$MICRO_BASIC .

#Sstrip off s9 from end of MicroBasic file because srec_cat doesn't like it
#Also strip off the EXTERN RTS code since this is easier to just write an RTS
#using the copy code - so just run sed twice to delete 2 last lines
sed -i '$d' $MICRO_BASIC
sed -i '$d' $MICRO_BASIC

# Relocate MicroBasic BASIC to F2A3H, which means the actual code starts
# F3A3H (original code has a 0100H offset from 0H) and ends at FFF7H, one
# byte before the reset vectors at FFF8H. That was lots of math...
srec_cat $MICRO_BASIC --offset=0xf2a3 > microbasic.s19

# convert MicroBasic BASIC to Intel HEX
objcopy -I srec -O ihex microbasic.s19 microbasic.hex

# strip off last line of MicroBasic basic hex file so it can be concatenated
sed -i '$ d' microbasic.hex

# build final Intel hex image file
#*********************************

cat microbasic.hex >> retro68.hex

dos2unix retro68.hex	#strip off ctrl-Ms

# clean up
rm microbasic.hex 
rm microbasic.s19 
rm $MICRO_BASIC
rm TBUG_retro68.s19



