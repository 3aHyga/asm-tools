asm-tools
======
Licence: MIT

Small tools for DOS written in assembler.

Keys:
 /M	 - Mark a diskette
	   Parameters: <Drive> [Label] [Volume] [OEM Type]
	      Drive	- a drive letter
	      Label	- Character string ( character max 11 )
	      Volume	- Number of diskette in decimal
	      OEM Type	- Type in decimal from 1 to 3, 0 the same meaning
 /S	 - Prepare file for a printing (shift file on N columns right)
	   Parameters: <N:columns> <File>
	      Columns	- Number of columns in decimal [0,32]
	      File	- File name of shifting file
 /N	 - Set computer name for net
	   Parameter: <Compnum> <Name>
	      Compnum	- Computer number [0,255]
	      Name	- Character string of computer name
