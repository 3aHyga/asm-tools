.8086
.MODEL  SMALL
.STACK  100H
COD     SEGMENT PARA PUBLIC 'CODE'
ASSUME  CS:COD,DS:DATA,SS:STACK
BUFFF   EQU     INL+(BUFF-FTB+15)/16+10
BPB     EQU     11
FAT     EQU     ('F'+'A'+'T')-(('F'+'A'+'T')/256)*256
ez      equ     es
START:
        PUSH    DS
        MOV     AX,DATA
        MOV     DS,AX
        LEA     SI,CREDITS
        CALL    WW
        POP     DS
        MOV     SI,80H
        MOV     AX,INL
        MOV     ES,AX
        LEA     AX,FTB
        LEA     BX,STB
        LEA     DX,NTB
        LEA     DI,BUFF
        CALL    RL
        MOV     AX,DATA
        MOV     DS,AX
        JC      USAGE
        MOV     SI,DX
        MOV     BX,-2
        MOV     AX,INL
        MOV     ES,AX
A1:
        MOV     AX,ES:[SI]
        ADD     BX,2
        ADD     SI,2
        OR      AX,AX
        JZ      A1
        LEA     SI,TBL
        mov     ds:[si+6],cx
        CALL    WORD PTR CS:[BX+PROGS_OFFSETS]
        JMP     SHORT   END1
USAGE:
        LEA     SI,USAGE_
        CALL    WW
END1:
        MOV     AX,4C00H
        INT     21H
;Mark a diskette
MARK    PROC
        ;IN DS:SI - TABLE
        ;+0 FREE SEG
        ;+2 DATA SEG
        ;+4 INPUT LINE SEG
        ;+6 NUM OF FILES IN INPUT LINE SEG

        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    BP
        PUSH    DS
        PUSH    ES
        CALL    CHECK_MARK_ERRORS
        JC      MARK_ER
        CALL    MARK_BOOT
        JC      MARK_ER
        CALL    MARK_ROOT
        LEA     SI,DISK_MARKED
        JNC     MARK_EX
MARK_ER:
        LEA     SI,DISK_ERROR
        OR      AH,AH
        JNZ     MARK_ER1
        LEA     SI,VERY_MANY
        SUB     AL,80H
        JZ      MARK_ER1
        LEA     SI,VERY_SMALL
        DEC     AL
        JZ      MARK_ER1
        LEA     SI,NO_FLOPPY
        DEC     AL
        JZ      MARK_ER1
        LEA     SI,NON_FAT
MARK_ER1:
        STC
MARK_EX:
        CALL    WW
        POP     ES
        POP     DS
        POP     BP
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
CHECK_MARK_ERRORS       PROC
        ;IN DS:SI TABLE
        ;+0 FREE SEG
        ;+2 DATA SEG
        ;+4 INPUT LINE SEG
        ;+6 NUM OF FILES IN INPUT LINE SEG
        PUSH    SI
        PUSH    DS
        MOV     CX,DS:[SI+6]
        MOV     AX,100H
        CMP     CX,5
        JNB     CHECK_MARK_ER
        OR      CX,CX
        JZ      CHECK_MARK_ER
        MOV     BP,DS:[SI]
        MOV     ES,DS:[SI+2]
        MOV     DS,DS:[SI+4]
        MOV     DI,SI
        LEA     SI,INL:BUFF
        LODSB
        SUB     AL,'A'
        CMP     AL,'Z'-'A'
        JBE     CHECK_MARK_ERRORS3
        SUB     AL,20H
CHECK_MARK_ERRORS3:
        MOV     BYTE PTR ES:[DI+4],AL
CHECK_MARK_ERRORS2:
        LODSB
        OR      AL,AL
        JNZ     CHECK_MARK_ERRORS2
        PUSH    BP
        MOV     BP,CX
        XOR     AX,AX
        MOV     DL,BYTE PTR ES:[TBL+4]
        INT     13H
        POP     CX
        JC      CHECK_MARK_ER
        PUSH    ES
        MOV     ES,CX
        MOV     AX,201H
        XOR     CX,CX
        MOV     DH,CH
        MOV     BX,CX
        INC     CX
        INT     13H
        POP     ES
        JC      CHECK_MARK_ER
CHECK_MARK_OK:
        MOV     AX,DI
        ADD     DI,9
        MOV     CX,11
        CALL    COPY&OVERWRITE
        DEC     BP
        JZ      CHECK_MARK_EX
        MOV     DI,AX
        ADD     DI,5
        CALL    DECI2HEX
        DEC     BP
        JZ      CHECK_MARK_EX
        MOV     DI,AX
        ADD     DI,20
        LODSB
        SUB     AL,30H
        STOSB
CHECK_MARK_EX:
        POP     DS
        POP     SI
        CLC
        RET
CHECK_MARK_ER:
        POP     DS
        POP     SI
        STC
        RET
        ;OUT DS:SI TABLE
        ;+0 FREE SEG
        ;+2 DATA SEG
        ;+4 DISK TO MARK;FLOPPY ONLY
        ;+5 SERIAL NUMBER
        ;+9 LABEL
        ;+20 OEM TYPE(0,1,2)

        ;error
        ;100h - OK-disk marked
        ;101h - Неверно задана строка
ENDP
DECI2HEX        PROC
        PUSHF
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
DECI2HEX3:
        LODSB
        OR      AL,AL
        JNZ     DECI2HEX3
        PUSH    SI
        PUSH    BP
        XOR     AX,AX
        STOSW
        STOSW
        STD
        SUB     SI,2
        MOV     BP,0AH
        MOV     BX,1
        MOV     CX,5
DECI2HEX1:
        XOR     AX,AX
        LODSB
        OR      AL,AL
        JZ      DECI2HEX2
        SUB     AL,30H
        MUL     BX
        ADD     ES:[DI-2],DX
        ADD     ES:[DI-4],AX
        MOV     AX,BX
        MUL     BP
        MOV     BX,AX
        LOOP    DECI2HEX1
DECI2HEX2:
        POP     BP
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        POPF
        RET
ENDP
MARK_BOOT       PROC
        ;OUT AL - NUMBER OF SECTORS OF ROOT
        ;CH - CYLINDER
        ;CL SECTOR (0-5) , CYLINDER(6-7)
        ;DH - HEAD
        PUSH    SI
        PUSH    DS
        MOV     ES,DS:[SI]
        MOV     DL,DS:[SI+4]
        XOR     BX,BX
        MOV     AX,201H
        MOV     CX,1
        XOR     DH,DH
        INT     13H
        JC      MARK_BOOT_EX
        ;CHECK ERRORS
        MOV     AX,102H;NO FLOPPY
        CMP     BYTE PTR ES:[36],1
        JNC     MARK_BOOT_EX1
        CALL    CHECKSUMFAT;AX - CONTROL SUM
        CMP     AX,FAT
        MOV     AX,103H;NON FAT TYPE
        STC
        JNZ     MARK_BOOT_EX
        ;END OF CHECK ERRORS
MARK_BOOT_OK:
        CALL    UPDATE_BOOT
        MOV     AX,301H
        MOV     CX,1
        XOR     BX,BX
        INT     13H
        JC      MARK_BOOT_EX
        XOR     AX,AX
        MOV     AL,ES:[BPB+5];FAT NUMBER
        MUL     WORD PTR ES:[BPB+11];FAT LENGHT
        ADD     AX,ES:[BPB+3];RERSERVED SECTORS
        XOR     DX,DX
        DIV     WORD PTR ES:[BPB+13];DIVIDE BY SECTORS
        MOV     CX,DX;SECTORS REMAINS
        INC     CX
        XOR     DX,DX
        DIV     WORD PTR ES:[BPB+15];DIVIDE BY HEADS
        XCHG    DL,DH;SWAP HEADS
        MOV     CH,AL;LOW REG OF CYLINDERS
        ROR     AH,2;SHIFT HIGH REG OF CYLINDERS
        OR      CL,AH
        PUSH    DX
        MOV     AX,ES:[BPB]
        SHR     AX,5
        XOR     DX,DX
        XCHG    AX,WORD PTR EZ:[BPB+6]
        MOV     BP,AX
        DIV     WORD PTR EZ:[BPB+6]
        POP     DX
        STC
MARK_BOOT_EX1:
        CMC
MARK_BOOT_EX:
        POP     DS
        POP     SI
        RET
        ;ERROR IN AX
        ;102H   - Это не FLOPPY диск
        ;103H   - Файловая система не FAT
ENDP
UPDATE_BOOT     PROC
        ;UPDATE BOOT
        ADD     SI,5
        MOV     DI,39
        MOV     CX,11+4
        REP     MOVSB
        MOV     DS,DS:[SI-18]
        XOR     AX,AX
        LODSB
        OR      AL,AL
        JZ      UPDATE_BOOT1
        DEC     AL
        SHL     AL,1
        LEA     SI,MARK_ADDR
        ADD     SI,AX
        MOV     SI,DS:[SI]
        XOR     CX,CX
        MOV     DI,3
        MOV     CX,8
        CALL    COPY&OVERWRITE
UPDATE_BOOT1:
        RET
        ;END    UPDATE BOOT
ENDP
CHECKSUMFAT     PROC
        PUSH    DI
        MOV     DI,54
        MOV     CX,3
        XOR     AL,AL
CHECKSUM1:
        ADD     AL,ES:[DI]
        INC     DI
        LOOP    CHECKSUM1
        POP     DI
        RET
ENDP

COPY&OVERWRITE  PROC
        PUSH    AX
        PUSH    CX
COPY&OVERWRITE2:
        LODSB
        OR      AL,AL
        JZ      COPY&OVERWRITE3
        STOSB
        LOOP    COPY&OVERWRITE2
        LODSB
        JMP     SHORT   COPY&OVERWRITE1
COPY&OVERWRITE3:
        MOV     AL,20H
        REP     STOSB
COPY&OVERWRITE1:
        POP     CX
        POP     AX
        RET
ENDP
MARK_ROOT       PROC
        MOV     ES,DS:[SI]
        MOV     DL,DS:[SI+4]
        XOR     BX,BX
        MOV     AH,2
        MOV     DI,AX
        INT     13H
        JC      MARK_ROOT_EX
        MOV     AX,DI
        CALL    FIND&RECORD
        MOV     AH,3
        INT     13H
MARK_ROOT_EX:
        RET
ENDP
FIND&RECORD     PROC
        PUSH    AX
        PUSH    CX
        MOV     CX,BP
        XOR     DI,DI
FIND_AGAIN:
        TEST    BYTE PTR ES:[DI+11],1000B
        JNZ     FIND_OK
        MOV     AL,ES:[DI]
        CMP     AL,'х'
        JE      FIND_OK
        OR      AL,AL
        JE      FIND_OK
        ADD     DI,32
        LOOP    FIND_AGAIN
        JMP     SHORT   FIND&RECORD1
FIND_OK:
        ADD     DI,22
        ADD     SI,5
        MOV     CX,4
        REP     MOVSB
        SUB     DI,26
        MOV     CX,11
        REP     MOVSB
        MOV     AL,0E8H
        STOSB
FIND&RECORD1:
        POP     CX
        POP     AX
        RET
ENDP

;Shifting a text
SHIFT_TEXT PROC
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    BP
        PUSH    DS
        PUSH    ES
        CALL    SHIFT_TEXT_SETUP
        JC      SHIFT_TEXT_ERR
        CALL    SHIFT_TEXT_EXE
        LEA     SI,FILE_SHIFTED
        JNC     SHIFT_TEXT_EX
SHIFT_TEXT_ERR:
        LEA     SI,BIGNUM
        OR      AX,AX
        JZ      SHIFT_TEXT_EX
        LEA     SI,DISK_ERROR
SHIFT_TEXT_EX:
        CALL    WW
        POP     ES
        POP     DS
        POP     BP
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
SHIFT_TEXT_SETUP   PROC
        ;IN DS:SI SHIFT TEXT TBL
        ;+0 FREE
        ;+2 DATA
        ;+4 INPUT LINE SEG
        ;+6 NUM OF FILES IN INPUT LINE SEG
        PUSH    DS
        PUSH    SI
        PUSH    DS
        POP     ES
        MOV     CX,DS:[SI+6]
        MOV     AX,200H
        CMP     CX,2
        JNZ     SHIFT_TEXT_SETUP_ER
        ADD     SI,4
        MOV     DS,DS:[SI]
        MOV     DI,SI
        LEA     SI,INL:BUFF
        CALL    DECI2HEX
        MOV     AX,201H
        CMP     WORD PTR ES:[DI-2],0
        JNZ     SHIFT_TEXT_SETUP_ER
        CMP     WORD PTR ES:[DI-4],32
        JNB     SHIFT_TEXT_SETUP_ER
        XOR     AX,AX
        CALL    COPY_TO
        STC
SHIFT_TEXT_SETUP_ER:
        CMC
        POP     SI
        POP     DS
        RET
        ;OUT DS:SI SHIFT TEXT TBL
        ;+0 FREE
        ;+2 DATA
        ;+4 OFFSET
        ;+8 FILENAME

        ;ERRORS IN AX
        ;200H   - Нет строки параматров (Нет имени файла и числа для сдвигов)
        ;201H   - Число для сдвига вылезает за границы диапазона
ENDP
SHIFT_TEXT_EXE  PROC
        ;IN DS:SI SHIFT TEXT TBL
        ;+0 FREE
        ;+2 DATA
        ;+4 OFFSET
        ;+8 FILENAME
        PUSH    DS
        MOV     BP,DS:[SI+4]
        PUSH    DS
        POP     ES
        MOV     DS,ES:[SI]
        ADD     SI,8
        XOR     DX,DX
        MOV     AX,-1
        CALL    LOAD_FILE
        JC      SHIFT_TEXT_ER
        PUSH    ES
        PUSH    SI
        MOV     AX,DS
        ADD     AX,1000H
        MOV     ES,AX
        XOR     SI,SI
        XOR     DI,DI
        PUSH    ES
SHIFT_TEXT1:
        PUSH    CX
        MOV     AL,20H
        MOV     CX,BP
        ADD     DX,CX
        REP     STOSB
        POP     CX
SHIFT_TEXT2:
        LODSB
        STOSB
        INC     DX
        CMP     AL,0AH
        LOOPZ   SHIFT_TEXT1
        INC     CX
        LOOP    SHIFT_TEXT2
        POP     DS
        POP     SI
        POP     ES
        MOV     CX,DX
        XOR     BX,BX
        XOR     DX,DX
        CALL    SAVE_FILE
SHIFT_TEXT_ER:
        POP     DS
        RET
ENDP
COPY_TO PROC
        LODSB
        STOSB
        OR      AL,AL
        JNZ     COPY_TO
        RET
ENDP
;Setting a computer name
NAME_COMP       PROC
        PUSH    AX
        PUSH    BX
        PUSH    CX
        PUSH    DX
        PUSH    SI
        PUSH    DI
        PUSH    BP
        PUSH    DS
        PUSH    ES
        CALL    NAME_COMP_SETUP
        JC      NAME_COMP_ER
        CALL    NAME_COMPUTER
        LEA     SI,COMPNAME_SETTED
        JMP     SHORT   NAME_COMP_EX
NAME_COMP_ER:
        LEA     SI,BIGNUM
NAME_COMP_EX:
        CALL    WW
        POP     ES
        POP     DS
        POP     BP
        POP     DI
        POP     SI
        POP     DX
        POP     CX
        POP     BX
        POP     AX
        RET
ENDP
NAME_COMP_SETUP   PROC
        ;IN DS:SI SHIFT TEXT TBL
        ;+0 FREE
        ;+2 DATA
        ;+4 INPUT LINE SEG
        ;+6 NUM OF FILES IN INPUT LINE SEG
        PUSH    DS
        PUSH    SI
        PUSH    DS
        POP     ES
        MOV     CX,DS:[SI+6]
        MOV     AX,300H
        CMP     CX,2
        JNZ     NAME_COMP_SETUP_ER
        ADD     SI,4
        MOV     DS,DS:[SI]
        MOV     DI,SI
        LEA     SI,INL:BUFF
        CALL    DECI2HEX
        MOV     AX,301H
        CMP     WORD PTR ES:[DI-2],0
        JNZ     NAME_COMP_SETUP_ER
        CMP     WORD PTR ES:[DI-4],32
        JNB     NAME_COMP_SETUP_ER
        MOV     CX,15
        CALL    COPY&OVERWRITE
        STC
NAME_COMP_SETUP_ER:
        CMC
        POP     SI
        POP     DS
        RET
        ;ERROR IN AX
        ;300H   - Неправильные параметры строки
        ;301H   - Номер компьютера вылезает за диапазон
ENDP
NAME_COMPUTER   PROC
        ;IN DS:SI SHIFT TEXT TBL
        ;+0 FREE
        ;+2 DATA
        ;+4 NUMBER OF MACHINE
        ;+8 MACHINE NAME
        MOV     DX,SI
        ADD     DX,8
        MOV     CL,DS:[SI+4]
        MOV     AX,5E01H
        MOV     CH,AL
        INT     21H
        RET
ENDP
INCLUDE WW.LIB
INCLUDE READLINE.LIB
INCLUDE IO.LIB
PROGS_OFFSETS        DW         MARK
                     DW         SHIFT_TEXT
                     DW         NAME_COMP
ECS:
ENDS
DATA    SEGMENT PARA    PUBLIC 'DATA'
CREDITS DB      'Small Tools 1.1 by Pavel A. Skrylev (C)1997',0dh,0ah,0
USAGE_  DB      'Usage: TOOL <command> [parameters]',0dh,0ah,0
;MARK_DISK_DATA
MARK_ADDR       DW      MARK_OTHE,MARK_LOA,MARK_SYST
MARK_OTHE       DB      'Other',0
MARK_LOA        DB      'Loading',0
MARK_SYST       DB      'System',0
;END OF MARK_DISK_DATA

;TABLE
TBL     DW      BUFFF
        DW      DATA
        DW      INL
        DB      20H     DUP      (0)
;END OF TABLE

;OK
DISK_MARKED     DB      'Diskette marked',0dh,0ah,0
FILE_SHIFTED    DB      'File shifted',0dh,0ah,0
COMPNAME_SETTED DB      'New computername setted',0dh,0ah,0
;END OK

;ERRORS
DISK_ERROR      DB      'Disk Error',0dh,0ah,0
VERY_MANY       DB      'Very many parameters',0dh,0ah,0
VERY_SMALL      DB      'Where parameters ?',0dh,0ah,0
NO_FLOPPY       DB      'This is no floppy drive',0dh,0ah,0
NON_FAT         DB      'Non FAT type of file system',0dh,0ah,0

BIGNUM          DB      'Number very big',0dh,0ah,0
;END OF ERRORS
ENDS

INL     SEGMENT PARA    PUBLIC 'INL'
FTB     DB      '/MSN',0FFH
STB     DB      0FFH
NTB     DW      0,0
BUFF    DB      0
ENDS
END     START
