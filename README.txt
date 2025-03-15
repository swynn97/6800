Source for various ROMs and software for the 68Retro 6802 based system.

- To build everything, run ./build.sh. You will need the as02 assembler in your path.
- TBUG_retro68.asm contains the source for a modified Newbear computing TBUG monitor ROM orginally designed for their 77/68 system.
- TBUG has had its proprietary VDU code repleced with code to run a LCD display hooked up to PIAB.
- The ROM has a copy of SWTPC BASIC, and routine to copy this to RAM located at C000H. Press 'Z' or J C000 to run this- User guide for TBUG can be found at ./docs/TBUG.pdf
 
NOTE: Code is mapped to a 64K address space, so expects a 64K ROM such as a W27C512, etc.

Release history:

- 3/15/25	Initial commit to GitHub. Includes SWTPC BASIC v2.2, LCD driver code for a 2x16 display (partial support for larger displays, however scroll is broken). 



