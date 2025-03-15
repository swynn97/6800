#!/usr/bin/python3

import sys
import time
import os

delay = 0.005 
crdelay = 0.005

with open(sys.argv[1]) as f:
  while True:
    c = f.read(1)
    if not c:
      break
    if c == 'S':
      time.sleep(crdelay)	
      print('\r' + c, end = '') 
    else:	
      print(c, end = '')
      sys.stdout.flush()
      time.sleep(delay)	
print('\rS9')
