asm-tools
======
Licence: MIT

Small tools for DOS written in assembler.

Keys:
 * /M - Mark a diskette. Its parameters are <Drive> [Label] [Volume] [OEM Type], where Drive is a drive letter, Label is a Character string ( character max 11 ), Volume is a Number of diskette in decimal, and OEM Type is a Type in decimal from 1 to 3, 0 means the same
 * /S - Prepare file for a printing (shift file on N columns right). Its Parameters are <N:columns> <File>, where Columns is Number of columns in decimal [0,32], File is a File name of shifting file,
 * /N - Set computer name for net. Its Parameters are <Compnum> <Name>, where Compnum is Computer number [0,255], and Name is a Character string of computer name
