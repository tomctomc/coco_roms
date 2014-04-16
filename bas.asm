
; ------------------------------------------
; Color BASIC ( v1.0, v1.1, v1.2, and v1.3 )
; ------------------------------------------

; Copied from the PDF version of Color BASIC Unravelled.
; Fixed up to assemble in Mamou

; Revision History

; 04/19/2013 added conditional assembly for many versions:
; VERBAS=10 ; for Color Basic 1.0
; VERBAS=11 ; for Color Basic 1.1
; VERBAS=12 ; for Color Basic 1.2
; VERBAS=13 ; for Color Basic 1.3
; VERBAS=20 ; for Color Basic 2.0 (coco3 basic - labeled as "1.2" in code)
;
; 04/04/2009 r21 Color BASIC 1.3 (match ROM)
; 04/03/2009 r18 Color BASIC 1.2 (match ROM)

; $Id: $

                ORG         $A000
POLCAT          FDB         KEYIN           ; GET A KEYSTROKE
CHROUT          FDB         PUTCHR          ; OUTPUT A CHARACTER
CSRDON          FDB         CASON           ; TURN ON CASSETTE MOTOR, START READING
BLKIN           FDB         GETBLK          ; READ A BLOCK FROM CASSETTE
BLKOUT          FDB         SNDBLK          ; WRITE A BLOCK TO CASSETTE
JOYIN           FDB         GETJOY          ; READ JOYSTICKS
WRTLDR          FDB         WRLDR           ; TURN ON MOTOR AND WRITE $55S TO CASSETTE

LA00E           LDS         #LINBUF+LBUFMX+1 ; SET STACK TO TOP OF LINE INPUT BUFFER
                LDA         #$37
                STA         PIA1+3          ; ENABLE 63.5 MICROSECOND INTERRUPT
                LDA         RSTFLG          ; GET WARM START FLAG
                CMPA        #$55            ; IS IT A WARM START?
                BNE         BACDST          ; NO - D0 A COLD START
                LDX         RSTVEC          ; WARM START VECTOR
                LDA         ,X              ; GET FIRST BYTE OF WARM START ADDR
                CMPA        #$12            ; IS IT NOP?
                BNE         BACDST          ; NO - DO A COLD START
                JMP         ,X              ; YES, G0 THERE
; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
RESVEC          LDU         #LA00E          ; BASIC WARM START ENTRY (RESET)
LA02A           CLRB                        ;
                TFR         B,DP            ; USE PAGE 0 AS DIRECT PAGE
                LDX         #PIA0           ; POINT X TO PIA0
                CLR         $1,X            ; CLEAR CONTROL REGISTER A ON PIA0(U8)
                CLR         $3,X            ; CLEAR CONTROL REGISTER B
                CLR         ,X              ; A SIDE IS INPUT
                LDD         #$FF34
                STA         $2,X            ; B SIDE IS OUTPUT
                STB         $1,X            ; ENABLE PERIPHERAL REGISTERS
                STB         $3,X            ; AND CA2, CB2 AS OUTPUTS
                LDX         #PIA1           ; POINT X TO PIA1
                CLR         $1,X            ; CLEAR CONTROL REGISTER A ON PIA1(U4)
                CLR         $3,X            ; CLEAR CONTROL REGISTER B
                DECA                        ;  A - REG NOW HAS $FE
                STA         ,X              ; BITS 1-7 ARE OUTPUTS, BIT 0 IS INPUT ON SIDE A
                LDA         #$F8
                STA         $2,X            ; BITS 0-2 ARE INPUTS, BITS 3-7 ARE OUTPUTS ON B SIDE
                STB         $1,X            ; ENABLE PERIPHERAL REGISTERS
                STB         $3,X            ; AND CA2, CB2 AS OUTPUTS
                CLR         $2,X            ; ZEROS TO 6847
                LDA         #$02
                STA         ,X              ; MAKE SERIAL OUTPUT MARKING
                LDA         $2,X            ; READ PORT B OF U4 (TO GET RAM SIZE)
                LDX         #SAMREG         ; SAM CONTROL REGISTER ADDR
                LDB         #16             ; 16 SAM CONTROL REGISTER BITS
LA05E           STA         ,X++            ; ZERO OUT SAM CONTROL REGISTER (CLEAR BITS)
                DECB                        ;  DECREMENT REGISTER COUNTER
                BNE         LA05E           ; BRANCH IF NOT DONE
                STA         SAMREG+9        ; SET DISPLAY PAGE AT $400
                ANDA        #$04            ; MASK OFF ALL BUT RAM SIZE BIT
                BEQ         LA06C           ; BRANCH IF 4K RAM
                STA         -$5,X           ; SET FOR 16K DYNAMIC
LA06C           JMP         ,U              ; GO DO A WARM START


BACDST          LDX         #0              ; POINT X TO TOP OF DIRECT PAGE
LA071           CLR         ,X+             ; CLEAR FIRST 1K OF RAM
                CMPX        #VIDRAM         ; COMPARE TO TOP OF DISPLAY (1K)
                BNE         LA071           ; BRANCH IF NOT DONE
                JSR         >CLRSCRN        ; CLEAR SCREEN
                LDX         #LA10D          ; POINT X TO ROM IMAGE OF DIRECT PAGE VARS
                LDU         #CMPMID         ; POINT U TO RAM DESTINATION
                LDB         #28             ; 28 BYTES
                JSR         >LA59A          ; MOVE (B) BYTES FROM (X) TO (U)
                LDU         #IRQVEC         ; POINT U TO NON-DIRECT PAGE VARIABLES
                LDB         #30             ; 30 BYTES
                JSR         >LA59A          ; MOVE (B) BYTES FROM (X) TO (U)
                LDX         #LB277          ; ADDR OF SYNTAX ERROR ROUTINE
                STX         $3,U            ; SET EXBAS PRIMARY AND SECONDARY COMMAND INTERPRETATION TABLES TO
                STX         $8,U            ; SYNTAX ERROR (U POINTS TO $12A AT THIS POINT)
                LDX         #RVEC0          ; POINT X TO RAM VECTORS
                LDA         #$39            ; OP CODE OF RTS
LA09A           STA         ,X+             ; PUT RTS'S IN THE RAM VECTORS
                CMPX        #RVEC0+25*3     ; END OF RAM VECTORS?
                BNE         LA09A           ; NO KEEP INSERTING RTS
                STA         LINHDR-1        ; PUT RTS IN $2D9
                LDX         #VIDRAM+$200    ; POINT TO COLOR BASIC'S START OF PROGRAM
                CLR         ,X+             ; PUT A ZERO AT THE START OF BASIC
                STX         TXTTAB          ; BEGINNING OF BASIC PROGRAM
LA0AB           LDA         $2,X            ; LOOK FOR END OF PROGRAM
                COMA                        ;
                STA         $2,X            ; STORE IN RAM
                CMPA        $2,X            ; IS VALUE IN MEMORY THE SAME AS WHAT WAS JUST PUT THERE?
                BNE         $A0BA           ; IF NOT, THEN IT IS NOT RAM OR THE RAM IS BAD
                LEAX        $1,X            ; MOVE TO NEXT RAM LOCATION
LA0B6           COM         $1,X            ; RESTORE VALUE OF MEMORY JUST CHANGED
                BRA         LA0AB           ; KEEP CHECKING RAM
LA0BA           STX         TOPRAM          ; SET TOP OF RAM POINTER
                STX         MEMSIZ          ; TOP OF STRING SPACE
                STX         STRTAB          ; START OF STRING VARIABLES
                LEAX        -200,X          ; CLEAR 200 BYTES ON A COLD START -
                STX         FRETOP          ; SAVE NEW TOP OF FREE RAM
                TFR         X,S             ; PUT STACK THERE (AT MEMEND-200)
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
                if          VERBAS<20
; -----------------------------------------------------------------------------
RESVEC          LEAY        <LA00E,PC       ; POINT Y TO WARM START CHECK CODE
LA02A           LDX         #PIA1           ; POINT X TO PIA1
                CLR         -3,X            ; CLEAR PIA0 CONTROL REGISTER A
                CLR         -1,X            ; CLEAR PIA0 CONTROL REGISTER B
                CLR         -4,X            ; SET PIA0 SIDE A TO INPUT
                LDD         #$FF34
                STA         -2,X            ; SET PIA0 SIDE B TO OUTPUT
                STB         -3,X            ; ENABLE PIA0 PERIPHERAL REGISTERS, DISABLE
                STB         -1,X            ; MPU INTERRUPTS, SET CA2, CA1 TO OUTPUTS



                CLR         1,X             ; CLEAR CONTROL REGISTER A ON PIA1
                CLR         3,X             ; CLEAR CONTROL REGISTER B ON PIA1
                DECA                        ;  A REG NOW HAS $FE
                STA         ,X              ; BITS 1-7 ARE OUTPUTS, BIT 0 IS INPUT ON PIA1
                LDA         #$F8
                STA         2,X             ; BITS 0-2 ARE INPUTS, BITS 3-7 ARE OUTPUTS
                STB         1,X             ; ENABLE PERIPHERAL REGISTERS, DISABLE PIA1
                STB         3,X             ; INTERRUPTS AND SET CA2, CB2 AS OUTPUTS
                CLR         2,X             ; SET 6847 MODE TO ALPHA-NUMERIC
                LDB         #$02
                STB         ,X              ; MAKE RS232 OUTPUT MARKING



                LDU         #SAMREG         ; SAM CONTROL REGISTER ADDR
                LDB         #16             ; 16 SAM CONTROL REGISTER BITS
LA056           STA         ,U++            ; ZERO OUT SAM CONTROL REGISTER BIT
                DECB                        ;  DECREMENT COUNTER AND
                BNE         LA056           ; BRANCH IF NOT DONE
                STA         SAMREG+9        ; SET DISPLAY PAGE AT $400
LA05E           TFR         B,DP            ; SET DIRECT PAGE TO ZERO
                LDB         #$04            ; USE AS A MASK TO CHECK RAMSZ INPUT
; -----------------------------------------------------------------------------
                if          VERBAS<13
; -----------------------------------------------------------------------------
                STA         -2,X            ; SET RAMSZ STROBE HIGH
                BITB        2,X             ; CHECK RAMSZ INPUT
                BEQ         LA072           ; BRANCH IF JUMPER SET FOR 4K RAMS
                CLR         -2,X            ; SET RAMSZ STROBE LOW
                BITB        2,X             ; CHECK RAMSZ INPUT
                BEQ         LA070           ; BRANCH IF JUMPER SET FOR 64K RAMS
                LEAU        -2,U            ; ADJUST POINTER TO SET SAM FOR 16K RAMS
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                CLR         -2,X
                BITB        2,X
                BEQ         LA06E
                STA         -5,U
                STA         -11,U
                BRA         LA072
LA06E           NOP                         ;
                NOP                         ;
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
LA070           STA         -3,U            ; PROGRAM SAM FOR 16K OR 64K RAMS
LA072           JMP         ,Y              ; GO DO A WARM OR COLD START
; COLD START ENTRY
BACDST          LDX         #VIDRAM+1       ; POINT X TO CLEAR 1ST 1K OF RAM
LA077           CLR         ,--X            ; MOVE POINTER DOWN TWO-CLEAR BYTE
                LEAX        1,X             ; ADVANCE POINTER ONE
                BNE         LA077           ; KEEP GOING IF NOT AT BOTTOM OF PAGE 0
                JSR         >CLRSCRN        ; CLEAR SCREEN
                CLR         ,X+             ; CLEAR 1ST BYTE OF BASIC PROGRAM
                STX         TXTTAB          ; BEGINNING OF BASIC PROGRAM
LA084           LDA         2,X             ; LOOK FOR END OF MEMORY
                COMA                        ;  COMPLEMENT IT AND PUT IT BACK
                STA         2,X             ; INTO SYSTEM MEMORY
                CMPA        2,X             ; IS IT RAM?
                BNE         LA093           ; BRANCH IF NOT (ROM, BAD RAM OR NO RAM)
                LEAX        1,X             ; MOVE POINTER UP ONE
                COM         1,X             ; RE-COMPLEMENT TO RESTORE BYTE
                BRA         LA084           ; KEEP LOOKING FOR END OF RAM

; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
RESVEC          LEAY        <LA00E,PC       ; POINT Y TO WARM START CHECK CODE
LA02A           LDA         #$3A
                STA         >MMUREG+2
                LDX         #PIA1
                LDD         #$FF34
                CLR         1,X
                CLR         3,X
                DECA                        ;
                STA         ,X
                LDA         #$F8
                STA         2,X
                STB         1,X
                STB         3,X
                CLR         2,X
                LDA         #$02
                STA         ,X
                LDA         #$FF
                LDX         #PIA0
                CLR         1,X
                CLR         3,X
                CLR         ,X
                STA         2,X
                STB         1,X
                STB         3,X
                JMP         >LA072
LA05E           JSR         >$8C2E          ; in ext basic (all one rom on coco3)
                JMP         >DOSBAS         ; JUMP TO COCO 3 BASIC
                BITB        2,X
                BEQ         LA072
                CLR         -2,X
                BITB        2,X
                BEQ         LA070
                LEAU        -2,U
LA070           STA         -3,U            ; PROGRAM SAM FOR 16K OR 64K RAMS
LA072           JMP         ,Y              ; GO DO A WARM OR COLD START
; COLD START ENTRY
BACDST          LDX         #VIDRAM+1       ; POINT X TO CLEAR 1ST 1K OF RAM
LA077           CLR         ,--X            ; MOVE POINTER DOWN TWO-CLEAR BYTE
                LEAX        1,X             ; ADVANCE POINTER ONE
                BNE         LA077           ; KEEP GOING IF NOT AT BOTTOM OF PAGE 0
                JSR         >CLRSCRN        ; CLEAR SCREEN
                CLR         ,X+             ; CLEAR 1ST BYTE OF BASIC PROGRAM
                STX         TXTTAB          ; BEGINNING OF BASIC PROGRAM
                LDX         #$7FFF
                BRA         LA093
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

LA093           STX         TOPRAM          ; SAVE ABSOLUTE TOP OF RAM
                STX         MEMSIZ          ; SAVE TOP OF STRING SPACE
                STX         STRTAB          ; SAVE START OF STRING VARIABLES
                LEAX        -200,X          ; CLEAR 200 - DEFAULT STRING SPACE TO 200 BYTES
                STX         FRETOP          ; SAVE START OF STRING SPACE
                TFR         X,S             ; PUT STACK THERE
                LDX         #LA10D          ; POINT X TO ROM SOURCE DATA
                LDU         #CMPMID         ; POINT U TO RAM DESTINATION
                LDB         #28             ; MOVE 28 BYTES
                JSR         >LA59A          ; MOVE 28 BYTES FROM ROM TO RAM
                LDU         #IRQVEC         ; POINT U TO NEXT RAM DESTINATION
                LDB         #30             ; MOVE 30 MORE BYTES
                JSR         >LA59A          ; MOVE 30 BYTES FROM ROM TO RAM
                LDX         -12,X           ; POINT X TO SYNTAX ERROR ADDRESS
LA0B6           STX         3,U             ; SET EXBAS COMMAND INTERPRETATION
                STX         8,U             ; HANDLERS TO SYNTAX ERROR
                LDX         #RVEC0          ; POINT X TO START OF RAM VECTORS
                LDD         #$394B          ; SET UP TO SAVE 75 RTS
LA0C0           STA         ,X+             ; FILL THE RAM VECTORS WITH RTS
                DECB                        ;  DECREMENT COUNTER AND
                BNE         LA0C0           ; BRANCH IF NOT DONE
                STA         LINHDR-1        ; PUT RTS IN LINHDR-1
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------


                JSR         >LAD19          ; G0 DO A NEW

; -----------------------------------------------------------------------------
                if          VERBAS<20
; -----------------------------------------------------------------------------
                LDX         #$4558          ; ASCII EX (FIRST TWO LETTERS OF EXTENDED)
LA0CE           CMPX        EXBAS           ; SEE IF EXTENDED ROM IS THERE
                LBEQ        EXBAS+2         ; IF IT IS, BRANCH TO IT
                ANDCC       #$AF            ; ENABLE IRQ, FIRQ
                LDX         #LA147-1        ; POINT X TO COLOR BASIC COPYRIGHT MESSAGE
                JSR         >LB99C          ; PRINT COLOR BASIC
                LDX         #BAWMST         ; WARM START ADDRESS
                STX         RSTVEC          ; SAVE IT
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                JMP         >$8002          ; CPYROM copy rom to ram (coco3)
                PSHS        X,B
                TST         HRWIDTH
                LBNE        <$F77E          ; ALINK24
LA0D6           JSR         >LA199
                JSR         >KEYIN
                BEQ         LA0D6
                JMP         >LA1B9
                FCB         $72
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------


LA0E2           LDA         #$55            ; WARM START FLAG
                STA         RSTFLG          ; SAVE IT
                BRA         LA0F3           ; GO TO BASICS MAIN LOOP
BAWMST          NOP                         ;  NOP REQD FOR WARM START
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         >LAD33          ; DO PART OF A NEW
                ANDCC       #$AF            ; ENABLE IRQ,FIRQ



; -----------------------------------------------------------------------------
                if          VERBAS<20
; -----------------------------------------------------------------------------
                JSR         >CLRSCRN        ; CLEAR SCREEN
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                JSR         >CLS            ; CLEAR SCREEN
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



LA0F3           JMP         >LAC73          ; GO TO MAIN LOOP OF BASIC

; FIRQ SERVICE ROUTINE
BFRQSV          TST         PIA1+3          ; CARTRIDGE INTERRUPT?
                BMI         LA0FC           ; YES
                RTI


; -----------------------------------------------------------------------------
                if          VERBAS<20
; -----------------------------------------------------------------------------
LA0FC           JSR         >LA7D1          ; KEEP DELAYING
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
LA0FC           JSR         >$8C28          ; ?
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



                JSR         >LA7D1          ; KEEP DELAYING



; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
LA102           LDU         #LA108
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
LA102           LEAY        <LA108,PC       ; Y = ROM-PAK START UP VECTOR
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                JMP         >LA02A          ; GO DO INITIALIZATION
LA108           CLR         RSTFLG          ; CLEAR WARM START FLAG
                JMP         >ROMPAK         ; JUMP TO EXTERNAL ROM PACK

; THESE BYTES ARE MOVED TO ADDRESSES $8F - $AA THE DIRECT PAGE
LA10D           FCB         18              ; MID BAND PARTITION OF 1200/2400 HERTZ PERIOD
                FCB         24              ; UPPER LIMIT OF 1200 HERTZ PERIOD
                FCB         10              ; UPPER LIMIT OF 2400 HERTZ PERIOD
                FDB         128             ; NUMBER OF 55S TO CASSETTE LEADER
                FCB         11              ; CURSOR BLINK DELAY
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
                FDB         $57             ; LINE PRINTER BAUD RATE
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FDB         $58             ; CONSTANT FOR 600 BAUD VER 1.2 & UP
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                FDB         1               ; PRINTER CARRIAGE RETURN DELAY
                FCB         16              ; TAB FIELD WIDTH
                FCB         112             ; LAST TAB ZONE
                FCB         132             ; PRINTER WIDTH
                FCB         0               ; LINE PRINTER POSITION
                FDB         LB44A           ; ARGUMENT OF EXEC COMMAND - SET TO FC ERROR
; LINE INPUT ROUTINE
                INC         CHARAD+1
                BNE         LA123
                INC         CHARAD
LA123           LDA         >0000
                JMP         >BROMHK

; THESE BYTES ARE MOVED TO ADDRESSES $10C-$129
                JMP         >BIRQSV         ; IRQ SERVICE
                JMP         >BFRQSV         ; FIRQ SERVICE
                JMP         >LB44A          ; USR ADDRESS FOR 8K BASIC (INITIALIZED TO FC ERROR)
                FCB         $80             ; RANDOM SEED
                FDB         $4FC7           ; RANDON SEED OF MANTISSA
                FDB         $5259           ; .811635157
                FCB         $FF             ; UPPER CASE/LOWER CASE FLAG (STARTS SET TO UPPER)
                FDB         DEBDEL          ; KEYBOARD DEBOUNCE DELAY
                JMP         >LB277          ; DISPATCH FOR EXPONENTIATION (INITIALIZED TO SYNTAX ERROR)
; BASIC COMMAND INTERPRETATION TABLE ROM IMAGE
LA13D           FCB         53              ; 53 BASIC COMMANDS
LA13E           FDB         LAA66           ; POINTS TO RESERVED WORDS
LA140           FDB         LAB67           ; POINTS TO JUMP TABLE FOR COMMANDS
LA142           FCB         20              ; 20 BASIC SECONDARY COMMANDS
LA143           FDB         LAB1A           ; POINTS TO SECONDARY FUNCTION RESERVED WORDS
LA145           FDB         LAA29           ; POINTS TO SECONDARY FUNCTION JUMP TABLE
; COPYRIGHT MESSAGES
LA147           FCC         'COLOR BASIC '
; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
                FCC         '1.0'
; -----------------------------------------------------------------------------
                else
                if          VERBAS<12
; -----------------------------------------------------------------------------
                FCC         '1.1'
; -----------------------------------------------------------------------------
                else
                if          VERBAS<13
; -----------------------------------------------------------------------------
                FCC         '1.2'
; -----------------------------------------------------------------------------
                else
                if          VERBAS<20
; -----------------------------------------------------------------------------
                FCC         '1.3'
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCC         '1.2'           ; for some reason, coco3 has "1.2" in there ;
; but we are still calling it VERBAS=20 for our conditional assemble
; -----------------------------------------------------------------------------
                endif
                endif
                endif
                endif
; -----------------------------------------------------------------------------
LA156           FCB         CR
LA157           FCC         '(C) 198'
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
                FCC         '0'
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCC         '2'
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                FCC         ' TANDY'
LA165           FCB         $00
LA166           FCC         'MICROSOFT'
LA16F           FCB         CR,$00
LA171           BSR         LA176           ; GET A CHARACTER FROM CONSOLE IN
                ANDA        #$7F            ; MASK OFF BIT 7
                RTS
; CONSOLE IN
LA176           JSR         >RVEC4          ; HOOK INTO RAM
                CLR         CINBFL          ; RESET CONSOLE IN BUFFER FLAG = FULL
LA17B           TST         DEVNUM          ; CHECK DEVICE NUMBER
                BEQ         LA1B1           ; G0 DO CURSOR AND GET A KEY IF SCREEN MODE
                TST         CINCTR          ; TEST CHARACTER COUNTER
                BNE         LA186           ; NOT EMPTY - READ IN SOME CASSETTE DATA
                COM         CINBFL          ; SET TO $FF: CONSOLE IN BUFFER EMPTY
LA185           RTS

LA186           PSHS        U,Y,X,B         ; SAVE REGISTERS
                LDX         CINPTR          ; PICK UP BUFFER POINTER
                LDA         ,X+             ; GET NEXT CHAR
                PSHS        A               ; SAVE CHAR ON STACK
                STX         CINPTR          ; SAVE NEW BUFFER POINTER
LA190           DEC         CINCTR          ; DECR CHAR COUNT
                BNE         LA197           ; RETURN IF BUFFER NOT EMPTY
                JSR         >LA635          ; GO READ TAPE
LA197           PULS        A,B,X,Y,U,PC    ; RESTORE REGISTERS

LA199           DEC         BLKCNT          ; CURSOR BLINK DELAY
                BNE         LA1AB           ; NOT TIME FOR NEW COLOR
                LDB         #11
                STB         BLKCNT          ; RESET DELAY COUNTER
                LDX         CURPOS          ; GET CURSOR POSITION
                LDA         ,X              ; GET CURRENT CURSOR CHAR
                ADDA        #$10            ; BUMP TO NEXT COLOR
                ORA         #$8F            ; MAKE SURE ITS A SOLID GRAPHICS BLOCK
                STA         ,X              ; STORE TO SCREEN
LA1AB           LDX         #DEBDEL         ; CURSOR BLINK DELAY
LA1AE           JMP         >LA7D3          ; DELAY WHILE X DECREMENTS TO ZERO
; BLINK CURSOR WHILE WAITING FOR A KEYSTROKE
LA1B1           PSHS        X,B             ; SAVE REGISTERS
LA1B3           BSR         LA199           ; GO DO CURSOR
                BSR         KEYIN           ; GO CHECK KEYBOARD
                BEQ         LA1B3           ; LOOP IF NO KEY DOWN
LA1B9           LDB         #$60            ; BLANK
                STB         [CURPOS]        ; BLANK CURRENT CURSOR CHAR ON SCREEN
LA1BF           PULS        B,X,PC

; THIS ROUTINE GETS A KEYSTROKE FROM THE KEYBOARD IF A KEY
; IS DOWN. IT RETURNS ZERO TRUE IF THERE WAS NO KEY DOWN.
; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
KEYIN1          EQU         *               ; needed symbol
KEYIN           PSHS        B,X             ; SAVE REGISTERS
                BSR         LA1C8           ; GET KEYSTROKE
                TSTA                        ;  SET FLAGS
                PULS        B,X,PC          ; RESTORE REGISTERS
LA1C8           LEAS        -3,S            ; ALLOCATE 3 STORAGE BYTES ON STACK
                LDX         #KEYBUF         ; KEYBOARD MEMORY BUFFER
                CLR         0,S             ; RESET COLUMN COUNTER
                LDB         #$FE            ; COLUMN STROBE DATA, CHECK BIT 0 FIRST
; A COLUMN IS BEING CHECKED IF THE CORRESPONDING BUT IN THE COLUMN
; STROBE REGISTER ($FF02) HAS A ZERO IN IT.
                STB         PIA0+2          ; STORE IN COLUMN STROBE REGISTER
LA1D4           BSR         LA238           ; GET KEY DATA
                STA         1,S             ; TEMP STORE KEY DATA
                EORA        ,X              ; COMPARE WITH KEY MEMORY DATA
                ANDA        ,X              ; ACCA=0 IF THIS KEY WAS DOWN LAST TIME, TOO
                LDB         1,S             ; GET NEW KEY DATA
                STB         ,X+             ; STORE IT IN KEY MEMORY
                TSTA                        ;  WAS NEW KEY DOWN?
                BNE         LA1ED           ; YES
                INC         0,S             ; NO, INCREMENT COLUMN COUNTER
                COMB                        ;  SET CARRY FLAG
                ROL         PIA0+2          ; ROTATE COLUMN STROBE DATA LEFT ONE BIT
                BCS         LA1D4           ; ALL COLUMNS CHECKED WHEN ZERO IN THE COLUMN STROBE DATA IS ROTATED INTO THE CARRY FLAG
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
LA1ED           LDB         PIA0+2          ; GET COLUMN STROBE DATA

; THIS ROUTINE CONVERTS THE KEY DEPRESSION INTO A NUMBER
; FROM 0-50 IN ACCB CORRESPONDING TO THE KEY THAT WAS DOWN
                STB         2,S             ; TEMP STORE IT
                LDB         #$F8            ; TO MAKE SURE ACCB=0 AFTER FIRST ADDB #8


LA1F4           ADDB        #8              ; ADD 8 FOR EACH ROW OF KEYBOARD
                LSRA                        ;  ACCA CONTAINS THE ROW NUMBER OF THIS KEY
                BCC         LA1F4           ; GO ON UNTIL A ZERO APPEARS IN THE CARRY
                ADDB        0,S             ; ADD IN THE COLUMN NUMBER

; NOW CONVERT THE VALUE IN ACCB INTO ASCII
                BEQ         LA245           ; THE 'AT SIGN' KEY WAS DOWN
                CMPB        #26             ; WAS IT A LETTER?
                BHI         LA247           ; NO
                ORB         #$40            ; YES, CONVERT TO UPPER CASE ASCII
                BSR         LA22D           ; CHECK FOR THE SHIFT KEY
                BEQ         LA20E           ; IT WAS DOWN
                LDA         CASFLG          ; NOT DOWN, CHECK THE UPPER/LOWER CASE FLAG
                BNE         LA20E           ; UPPER CASE
                ORB         #$20            ; CONVERT TO LOWER CASE
LA20E           STB         0,S             ; TEMP STORE ASCII VALUE
                LDX         DEBVAL          ; GET KEYBOARD DEBOUNCE
                JSR         >LA7D3          ; GO WAIT A WHILE
                LDB         2,S             ; GET COLUMN STROBE DATA
                STB         PIA0+2          ; STORE IT
                BSR         LA238           ; READ A KEY
                CMPA        1,S             ; IS IT THE SAME KEY AS BEFORE DEBOUNCE?
                PULS        A               ; PUT THE ASCII VALUE OF KEY BACK IN ACCA
                BNE         LA22A           ; NOT THE SAME KEY
                CMPA        #$12            ; IS SHIFT ZERO DOWN?
                BNE         LA22B           ; NO
                COM         CASFLG          ; YES, TOGGLE UPPER/LOWER CASE FLAG
LA22A           CLRA                        ;  SET ZERO FLAG TO INDICATE NO NEW KEY DOWN
LA22B           PULS        X,PC            ; REMOVE TEMP STORAGE SLOTS FROM STACK AND RETURN

; TEST FOR THE SHIFT KEY
LA22D           LDA         #$7F            ; COLUMN STROBE
                STA         PIA0+2          ; STORE TO PIA
                LDA         PIA0            ; READ KEY DATA
                ANDA        #$40            ; CHECK FOR SHIFT KEY, SET ZERO FLAG IF DOWN
                RTS                         ; RETURN

; READ THE KEYBOARD
LA238           LDA         PIA0            ; READ PIA0, PORT A TO SEE IF KEY IS DOWN
; A BIT WILL BE ZERO IF ONE IS
                ORA         #$80            ; MASK OFF THE JOYSTICK COMPARATOR INPUT
                TST         PIA0+2          ; ARE WE STROBING COLUMN 7
                BMI         LA244           ; NO
                ORA         #$C0            ; YES, FORCE ROW 6 TO BE HIGH -THIS WILL CAUSE THE SHIFT KEY TO BE IGNORED
LA244           RTS                         ; RETURN

LA245           LDB         #51             ; CODE FOR 'AT SIGN'
LA247           LDX         #CONTAB-$36     ; POINT X TO CONTROL CODE TABLE
                CMPB        #33             ; KEY NUMBER <33?
                BLO         LA264           ; YES (ARROW KEYS, SPACE BAR, ZERO)
                LDX         #CONTAB-$54     ; POINT X TO MIDDLE OF CONTROL TABLE
                CMPB        #48             ; KEY NUMBER > 48?
                BHS         LA264           ; YES (ENTER, CLEAR, BREAK, AT SIGN)
                BSR         LA22D           ; CHECK SHIFT KEY (ACCA WILL CONTAIN STATUS)
                CMPB        #43             ; IS KEY A NUMBER, COLON OR SEMICOLON?
                BLS         LA25D           ; YES
                EORA        #$40            ; TOGGLE BIT 6 OF ACCA WHICH CONTAINS THE SHIFT DATA ONLY FOR SLASH, HYPHEN, PERIOD, COMMA
LA25D           TSTA                        ;  SHIFT KEY DOWN?
                BEQ         LA20E           ; YES
                ADDB        #$10            ; NO, ADD IN ASCII OFFSET CORRECTION
                BRA         LA20E           ; GO CHECK FOR DEBOUNCE
LA264           ASLB                        ;  MULT ACCB BY 2 - THERE ARE 2 ENTRIES IN CONTROL TABLE FOR EACH KEY - ONE SHIFTED, ONE NOT
                BSR         LA22D           ; CHECK SHIFT KEY
                BNE         LA26A           ; NOT DOWN
                INCB                        ;  ADD ONE TO GET THE SHIFTED VALUE
LA26A           LDB         B,X             ; GET ASCII CODE FROM CONTROL TABLE
                BRA         LA20E           ; GO CHECK DEBOUNCE
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
KEYIN1          EQU         *               ; needed symbol
KEYIN           PSHS        B,X,U           ; SAVE REGISTERS
                BSR         LA1C8           ; GET KEYSTROKE
                TSTA                        ;  SET FLAGS
                PULS        B,X,U,PC        ; RESTORE REGISTERS
LA1C8           LDU         #PIA0           ; POINT TO PIA0
                LDX         #KEYBUF         ; KEYBOARD MEMORY BUFFER
                CLRA                        ;  CLEAR CARRY FLAG, SET COLUMN COUNTER
                DECA                        ;  (ACCA) TO $FF
                PSHS        X,A             ; SAVE COLUMN CTR & 2 BLANK (X REG) ON STACK
                STA         2,U             ; INITIALIZE COLUMN STROBE TO $FF
                FCB         SKP1            ; SKIP ONE BYTE
LA1D5           COMB                        ;  SET CARRY FLAG
                ROL         2,U             ; ROTATE COLUMN STROBE DATA LEFT 1 BIT,
                BCC         LA1BF           ; CARRY INTO BIT 0-RETURN IF 8 BITS DONE
                INC         0,S             ; INCREMENT COLUMN POINTER
                BSR         LA239           ; READ KEYBOARD DATA ROW
                STA         1,S             ; TEMP STORE KEY DATA
                EORA        ,X              ; SET ANY BIT WHERE A KEY HAS MOVED
                ANDA        ,X              ; ACCA=0 IF NO NEW KEY DOWN, <70 IF KEY WAS RELEASED
                LDB         1,S             ; GET NEW KEY DATA
                STB         ,X+             ; STORE IT IN KEY MEMORY
                TSTA                        ;  WAS A NEW KEY DOWN?
                BEQ         LA1D5           ; NO-CHECK ANOTHER COLUMN
                LDB         2,U             ; GET COLUMN STROBE DATA AND
                STB         2,S             ; TEMP STORE IT ON THE STACK

; THIS ROUTINE CONVERTS THE KEY DEPRESSION INTO A NUMBER
; FROM 0-50 IN ACCB CORRESPONDING TO THE KEY THAT WAS DOWN
                LDB         #$F8            ; TO MAKE SURE ACCB=0 AFTER FIRST ADDB #8
LA1F1           ADDB        #8              ; ADD 8 FOR EACH ROW OF KEYBOARD
                LSRA                        ;  ACCA CONTAINS THE ROW NUMBER OF THIS KEY
; ADD 8 FOR EACH ROW
                BCC         LA1F1           ; GO ON UNTIL A ZERO APPEARS IN THE CARRY
                ADDB        0,S             ; ADD IN THE COLUMN NUMBER

; NOW CONVERT THE VALUE IN ACCB INTO ASCII
                BEQ         LA244           ; THE 'AT SIGN' KEY WAS DOWN
                CMPB        #26             ; WAS IT A LETTER?
                BHI         LA246           ; NO
                ORB         #$40            ; YES, CONVERT TO UPPER CASE ASCII
                BSR         LA22E           ; CHECK FOR THE SHIFT KEY
                BEQ         LA20B           ; IT WAS DOWN
                LDA         CASFLG          ; NOT DOWN, CHECK THE UPPER/LOWER CASE FLAG
                BNE         LA20B           ; UPPER CASE
                ORB         #$20            ; CONVERT TO LOWER CASE
LA20B           STB         0,S             ; TEMP STORE ASCII VALUE
                LDX         DEBVAL          ; GET KEYBOARD DEBOUNCE
                JSR         >LA7D3          ; GO WAIT A WHILE
                LDB         #$FF            ; SET COLUMN STROBE TO ALL ONES (NO
                BSR         LA237           ; STROBE) AND READ KEYBOARD
                INCA                        ;  INCR ROW DATA, ACCA NOW 0 IF NO JOYSTK
                BNE         LA220           ; BUTTON DOWN. BRANCH IF JOYSTK BUTTON DN
LA21A           LDB         2,S             ; GET COLUMN STROBE DATA
                BSR         LA237           ; READ A KEY
                CMPA        1,S             ; IS IT THE SAME KEY AS BEFORE DEBOUNCE?
LA220           PULS        A               ; PUT THE ASCII VALUE OF KEY BACK IN ACCA
                BNE         LA22B           ; NOT THE SAME KEY
                CMPA        #$12            ; IS SHIFT ZERO DOWN?
                BNE         LA22C           ; NO
                COM         CASFLG          ; YES, TOGGLE UPPER/LOWER CASE FLAG
LA22B           CLRA                        ;  SET ZERO FLAG TO INDICATE NO NEW KEY DOWN
LA22C           PULS        X,PC            ; REMOVE TEMP STORAGE SLOTS FROM STACK AND RETURN
; TEST FOR THE SHIFT KEY
LA22E           LDA         #$7F            ; COLUMN STROBE
                STA         2,U             ; STORE TO PIA
                LDA         ,U              ; READ KEY DATA
                ANDA        #$40            ; CHECK FOR SHIFT KEY, SET ZERO FLAG IF DOWN
                RTS                         ; RETURN
; READ THE KEYBOARD
LA237           STB         2,U             ; SAVE NEW COLUMN STROBE VALUE
LA239           LDA         ,U              ; READ PIA0, PORT A TO SEE IF KEY IS DOWN
; A BIT WILL BE ZERO IF ONE IS
                ORA         #$80            ; MASK OFF THE JOYSTICK COMPARATOR INPUT
                TST         2,U             ; ARE WE STROBING COLUMN 7
                BMI         LA243           ; NO
                ORA         #$C0            ; YES, FORCE ROW 6 TO BE HIGH -THIS WILL
; CAUSE THE SHIFT KEY TO BE IGNORED
LA243           RTS                         ; RETURN
LA244           LDB         #$33            ; CODE FOR 'AT SIGN'
LA246           LDX         #CONTAB-$36     ; POINT X TO CONTROL CODE TABLE
                CMPB        #33             ; KEY NUMBER <33?
                BLO         LA263           ; YES (ARROW KEYS, SPACE BAR, ZERO)
                LDX         #CONTAB-$54     ; POINT X TO MIDDLE OF CONTROL TABLE
                CMPB        #48             ; KEY NUMBER > 48?
                BHS         LA263           ; YES (ENTER, CLEAR, BREAK, AT SIGN)
                BSR         LA22E           ; CHECK SHIFT KEY (ACCA WILL CONTAIN STATUS)
                CMPB        #43             ; IS KEY A NUMBER, COLON OR SEMICOLON?
                BLS         LA25C           ; YES
                EORA        #$40            ; TOGGLE BIT 6 OF ACCA WHICH CONTAINS THE SHIFT DATA ONLY FOR SLASH, HYPHEN, PERIOD, COMMA
LA25C           TSTA                        ;  SHIFT KEY DOWN?
                BEQ         LA20B           ; YES
                ADDB        #$10            ; NO, ADD IN ASCII OFFSET CORRECTION
                BRA         LA20B           ; GO CHECK FOR DEBOUNCE
LA263           ASLB                        ;  MULT ACCB BY 2 - THERE ARE 2 ENTRIES IN CONTROL TABLE FOR EACH KEY - ONE SHIFTED, ONE NOT
                BSR         LA22E           ; CHECK SHIFT KEY
                BNE         LA269           ; NOT DOWN
                INCB                        ;  ADD ONE TO GET THE SHIFTED VALUE
LA269           LDB         B,X             ; GET ASCII CODE FROM CONTROL TABLE
                BRA         LA20B           ; GO CHECK DEBOUNCE
                FCB         0               ; WASTED SPACE IN VERSION 1.1
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------




; -----------------------------------------------------------------------------
                if          VERBAS<20
; -----------------------------------------------------------------------------
KEYIN1          CLR         PIA0+2          ; CLEAR COLUMN STROBE
                LDA         PIA0            ; READ KEY ROWS
                COMA                        ;  COMPLEMENT ROW DATA
                ASLA                        ;  SHIFT OFF JOYSTICK DATA
                BEQ         LA244           ; RETURN IF NO KEYS OR FIRE BUTTONS DOWN
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; THIS ROUTINE GETS A KEYSTROKE FROM THE KEYBOARD IF A KEY
; IS DOWN. IT RETURNS ZERO TRUE IF THERE WAS NO KEY DOWN.

KEYIN1          JMP         >KEYIN
                RTS
                RTS
                RTS
                RTS
                RTS
                RTS
                RTS
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



KEYIN           PSHS        U,X,B           ; SAVE REGISTERS
                LDU         #PIA0           ; POINT U TO PIA0
                LDX         #KEYBUF         ; POINT X TO KEYBOARD MEMORY BUFFER
                CLRA                        ;  CLEAR CARRY FLAG, SET COLUMN COUNTER (ACCA)
                DECA                        ;  TO $FF
                PSHS        X,A             ; SAVE COLUMN CTR & 2 BLANK (X REG) ON STACK
                STA         2,U             ; INITIALIZE COLUMN STROBE TO $FF
LA1D9           ROL         2,U             ; ROTATE COLUMN STROBE DATA LEFT 1 BIT, CARRY
                BCC         LA220           ; INTO BIT 0 - BRANCH IF 8 SHIFTS DONE
                INC         0,S             ; INCREMENT COLUMN COUNTER
                BSR         LA23A           ; READ KEYBOARD ROW DATA
                STA         1,S             ; TEMP STORE KEY DATA
                EORA        ,X              ; SET ANY BIT WHERE A KEY HAS MOVED
                ANDA        ,X              ; ACCA=0 IF NO NEW KEY DOWN, <70 IF KEY WAS RELEASED
                LDB         1,S             ; GET NEW KEY DATA
                STB         ,X+             ; STORE IT IN KEY MEMORY
                TSTA                        ;  WAS A NEW KEY DOWN?
                BEQ         LA1D9           ; NO-CHECK ANOTHER COLUMN
                LDB         2,U             ; GET COLUMN STROBE DATA AND
                STB         2,S             ; TEMP STORE IT ON THE STACK
; THIS ROUTINE CONVERTS THE KEY DEPRESSION INTO A NUMBER
; FROM 0-50 IN ACCB CORRESPONDING TO THE KEY THAT WAS DOWN
                LDB         #$F8            ; TO MAKE SURE ACCB=0 AFTER FIRST ADDB #8
LA1F4           ADDB        #$08            ; ADD 8 FOR EACH ROW OF KEYBOARD
                LSRA                        ;  ACCA HAS THE ROW NUMBER OF THIS KEY - ADD 8 FOR EACH ROW
                BCC         LA1F4           ; GO ON UNTIL A ZERO APPEARS IN THE CARRY FLAG
                ADDB        0,S             ; ADD IN THE COLUMN NUMBER
; NOW CONVERT THE VALUE IN ACCB INTO ASCII
                BEQ         LA245           ; THE AT SIGN KEY WAS DOWN
                CMPB        #26             ; WAS IT A LETTER?
                BHI         LA247           ; NO
                ORB         #$40            ; YES, CONVERT TO UPPER CASE ASCII
                BSR         LA22E           ; CHECK FOR THE SHIFT KEY
                ORA         CASFLG          ; OR IN THE CASE FLAG & BRANCH IF IN UPPER
                BNE         LA20C           ; CASE MODE OR SHIFT KEY DOWN
                ORB         #$20            ; CONVERT TO LOWER CASE
LA20C           STB         0,S             ; TEMP STORE ASCII VALUE
                LDX         DEBVAL          ; GET KEYBOARD DEBOUNCE
                BSR         LA1AE
                LDB         #$FF            ; SET COLUMN STROBE TO ALL ONES (NO
                BSR         LA238           ; STROBE) AND READ KEYBOARD
                INCA                        ;  INCR ROW DATA, ACCA NOW 0 IF NO JOYSTICK
                BNE         LA220           ; BUTTON DOWN. BRANCH IF JOYSTICK BUTTON DOWN
LA21A           LDB         2,S             ; GET COLUMN STROBE DATA
                BSR         LA238           ; READ A KEY
                CMPA        1,S             ; IS IT THE SAME KEY AS BEFORE DEBOUNCE?
LA220           PULS        A,X             ; REMOVE TEMP SLOTS FROM THE STACK AND RECOVER
; THE ASCII VALUE OF THE KEY
                BNE         LA22B           ; NOT THE SAME KEY OR JOYSTICK BUTTON
                CMPA        #$12            ; IS SHIFT ZERO DOWN?
                BNE         LA22C           ; NO
                COM         CASFLG          ; YES, TOGGLE UPPER CASE/LOWER CASE FLAG
LA22B           CLRA                        ;  SET ZERO FLAG TO INDICATE NO NEW KEY DOWN
LA22C           PULS        B,X,U,PC        ; RESTORE REGISTERS
; TEST FOR THE SHIFT KEY
LA22E           LDA         #$7F            ; COLUMN STROBE
                STA         2,U             ; STORE TO PlA
                LDA         ,U              ; READ KEY DATA
                COMA                        ;
                ANDA        #$40            ; SET BIT 6 IF SHIFT KEY DOWN
                RTS         RETURN
; READ THE KEYBOARD
LA238           STB         2,U             ; SAVE NEW COLUMN STROBE VALUE
LA23A           LDA         ,U              ; READ PIA0, PORT A TO SEE IF KEY IS DOWN
; A BIT WILL BE ZERO IF ONE IS
                ORA         #$80            ; MASK OFF THE JOYSTICK COMPARATOR INPUT
                TST         $02,U           ; ARE WE STROBING COLUMN 7?
                BMI         LA244           ; NO
                ORA         #$C0            ; YES, FORCE ROW 6 TO BE HIGH - THIS WILL CAUSE
; THE SHIFT KEY TO BE IGNORED
LA244           RTS         RETURN
LA245           LDB         #51             ; CODE FOR AT SIGN
LA247           LDX         #CONTAB-$36     ; POINT X TO CONTROL CODE TABLE
                CMPB        #33             ; KEY NUMBER <33?
                BLO         LA264           ; YES (ARROW KEYS, SPACE BAR, ZERO)
                LDX         #CONTAB-$54     ; POINT X TO MIDDLE OF CONTROL TABLE
                CMPB        #48             ; KEY NUMBER >48?
                BHS         LA264           ; YES (ENTER,CLEAR,BREAK,AT SIGN)
                BSR         LA22E           ; CHECK SHIFT KEY (ACCA WILL CONTAIN STATUS)
                CMPB        #43             ; IS KEY A NUMBER, COLON OR SEMICOLON?
                BLS         LA25D           ; YES
                EORA        #$40            ; TOGGLE BIT 6 OF ACCA WHICH CONTAINS THE SHIFT DATA
; ONLY FOR SLASH,HYPHEN,PERIOD,COMMA
LA25D           TSTA                        ;  SHIFT KEY DOWN?
                BNE         LA20C           ; YES
                ADDB        #$10            ; NO, ADD IN ASCII OFFSET CORRECTION
                BRA         LA20C           ; GO CHECK FOR DEBOUNCE
LA264           ASLB                        ;  MULT ACCB BY 2 - THERE ARE 2 ENTRIES IN CONTROL
; TABLE FOR EACH KEY - ONE SHIFTED, ONE NOT
                BSR         LA22E           ; CHECK SHIFT KEY
                BEQ         LA26A           ; NOT DOWN
                INCB                        ;  ADD ONE TO GET THE SHIFTED VALUE
LA26A           LDB         B,X             ; GET ASCII CODE FROM CONTROL TABLE
                BRA         LA20C           ; GO CHECK DEBOUNCE
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

; CONTROL TABLE UNSHIFTED, SHIFTED VALUES
CONTAB          FCB         $5E,$5F         ; UP ARROW
                FCB         $0A,$5B         ; DOWN ARROW
                FCB         $08,$15         ; RIGHT ARROW
                FCB         $09,$5D         ; LEFT ARROW
                FCB         $20,$20         ; SPACE BAR
                FCB         $30,$12         ; ZERO
                FCB         $0D,$0D         ; ENTER
                FCB         $0C,$5C         ; CLEAR
                FCB         $03,$03         ; BREAK
                FCB         $40,$13         ; AT SIGN
; CONSOLE OUT
PUTCHR          JSR         >RVEC3          ; HOOK INTO RAM
                PSHS        B               ; SAVE ACCB
                LDB         DEVNUM          ; GET DEVICE NUMBER
                INCB                        ;  SET FLAGS
                PULS        B               ; RESTORE ACCB
                BMI         LA2BF           ; SEND TO LINE PRINTER
                BNE         LA30A           ; SEND TO SCREEN
; SEND TO CASSETTE
                PSHS        X,B,A           ; RESTORE REGISTERS
                LDB         FILSTA          ; GET FILE STATUS
                DECB                        ;  INPUT FILE?
                BEQ         LA2A6           ; YES
                LDB         CINCTR          ; TEMP CHAR CTR
                INCB                        ;  IS THE BUFFER FULL
                BNE         LA29E           ; NO
                BSR         LA2A8           ; YES, WRITE DATA BLOCK TO TAPE
LA29E           LDX         CINPTR          ; GET BUFFER POINTER
                STA         ,X+             ; PUT CHAR IN CASSETTE BUFFER
                STX         CINPTR          ; STORE NEW BUFFER POINTER
                INC         CINCTR          ; INCR BYTE COUNT
LA2A6           PULS        A,B,X,PC
; WRITE A BLOCK OF DATA TO TAPE
LA2A8           LDB         #1              ; DATA BLOCK TYPE - NOT A HEADER BLOCK
LA2AA           STB         BLKTYP          ; BLOCK NUMBER
                LDX         #CASBUF         ; CASSETTE BUFFER
                STX         CBUFAD          ; STARTING ADDRESS
                LDB         CINCTR          ; GET NUMBER OF BYTES
                STB         BLKLEN          ; BYTE COUNT
                PSHS        U,Y,A           ; SAVE REGISTERS
                JSR         >LA7E5          ; WRITE A BLOCK ON TAPE
                PULS        A,Y,U           ; RESTORE REGISTERS
                JMP         >LA650          ; RESET BUFFER POINTERS
; SOFTWARE UART TO L1NE PRINTER
LA2BF           PSHS        X,B,A,CC        ; SAVE REGISTERS AND INTERRUPT STATUS
                ORCC        #$50            ; DISABLE IRQ,FIRQ
; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
                BSR         LA2FB           ; SET OUTPUT TO MARKING
                ASLA                        ;  SEND 7 BITS AND ONE STOP BIT (BIT 7=0)
                LDB         #$08            ; SEND 8 BITS
LA2C8           PSHS        B               ; SAVE BIT COUNTER
                CLRB                        ;  CLEAR DA IMAGE 1 ZEROS TO DA WHEN SENDING RS-232 DATA
                LSRA                        ;  ROTATE NEXT BIT OF OUTPUT CHARACTER TO CARRY FLAG
                ROLB                        ;  ROTATE CARRY FLAG INTO BIT ONE
                ROLB                        ;  AND ALL OTHER BITS SET TO ZERO
                STB         DA              ; STORE IT TO DA CONVERTER
                BSR         LA302           ; GO WAIT A WHILE
                NOP                         ;
                NOP                         ;
                NOP                         ;
                BSR         LA302           ; GO WAIT SOME MORE
                PULS        B               ; GET BIT COUNTER
                DECB                        ;  SENT ALL 8 BITS?
                BNE         LA2C8           ; NO
                BSR         LA2FB           ; SEND STOP BIT (ACCB=0)
                PULS        CC,A            ; RESTORE OUTPUT CHARACTER & INTERRUPT STATS
                CMPA        #CR             ; IS IT A CARRIAGE RETURN?
                BEQ         LA2ED           ; YES
                INC         LPTPOS          ; INCREMENT CHARACTER COUNTER
                LDB         LPTPOS          ; CHECK FOR END OF LINE PRINTER LINE
                CMPB        LPTWID          ; AT END OF LINE PRINTER LINE?
                BLO         $A2F3           ; NO
LA2ED           CLR         LPTPOS          ; RESET CHARACTER COUNTER
                BSR         LA305
                BSR         LA305           ; DELAY FOR CARRIAGE RETURN
LA2F3           LDB         PIA1+2          ; WAIT FOR HANDSHAKE
                LSRB                        ;  CHECK FOR RS232 STATUS
                BCS         LA2F3           ; NOT YET READY
                PULS        B,X,PC          ; RESTORE REGISTERS
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
                BSR         LA2FB           ; SET OUTPUT TO MARKING
                CLRB                        ;
                BSR         LA2FD           ; TRANSMIT ONE START BIT
                LDB         #$08            ; SEND 8 BITS
LA2CA           PSHS        B               ; SAVE BIT COUNTER
                CLRB                        ;  CLEAR DA IMAGE 1 ZEROS TO DA WHEN SENDING RS-232 DATA
                LSRA                        ;  ROTATE NEXT BIT OF OUTPUT CHARACTER TO CARRY FLAG
                ROLB                        ;  ROTATE CARRY FLAG INTO BIT ONE
                ASLB                        ;  AND ALL OTHER BITS SET TO ZERO
                BSR         LA2FD           ; TRANSMIT DATA BYTE
                PULS        B               ; GET BIT COUNTER
                DECB                        ;  SENT ALL 8 BITS?
                BNE         LA2CA           ; NO
                BSR         LA2FB           ; SEND STOP BIT (ACCB=0)
                PULS        CC,A            ; RESTORE OUTPUT CHARACTER & INTERRUPT STATS
                CMPA        #CR             ; IS IT A CARRIAGE RETURN?
                BEQ         LA2E7           ; YES
                INC         LPTPOS          ; INCREMENT CHARACTER COUNTER
                LDB         LPTPOS          ; CHECK FOR END OF LINE PRINTER LINE
                CMPB        LPTWID          ; AT END OF LINE PRINTER LINE?
                BLO         $A2ED           ; NO
LA2E7           CLR         LPTPOS          ; RESET CHARACTER COUNTER
                BSR         LA305
                BSR         LA305           ; DELAY FOR CARRIAGE RETURN
LA2ED           LDB         PIA1+2          ; WAIT FOR HANDSHAKE
                LSRB                        ;  CHECK FOR RS232 STATUS
                BCS         LA2ED           ; NOT YET READY
LA2F3           PULS        B,X,PC          ; RESTORE REGISTERS
                FDB         0,0,0           ; WASTED SPACE IN VERSION 1.1
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
LA2C3           LDB         PIA1+2          ; GET RS 232 STATUS
                LSRB                        ;  SHIFT RS 232 STATUS BIT INTO CARRY
                BCS         LA2C3           ; LOOP UNTIL READY
                BSR         LA2FB           ; SET OUTPUT TO MARKING
                CLRB                        ;
                BSR         LA2FD           ; TRANSMIT ONE START BIT
                LDB         #8              ; SEND 8 BITS
LA2D0           PSHS        B               ; SAVE BIT COUNTER
                CLRB                        ;  CLEAR DA IMAGE I ZEROES TO DA WHEN SENDING RS 232 DATA
                LSRA                        ;  ROTATE NEXT BIT OF OUTPUT CHARACTER TO CARRY FLAG
                ROLB                        ;  ROTATE CARRY FLAG INTO BIT ONE
                ASLB                        ;  AND ALL OTHER BITS SET TO ZERO
                BSR         LA2FD           ; TRANSMIT DATA BYTE
                PULS        B               ; GET BIT COUNTER
                DECB                        ;  SENT ALL 8 BITS?
                BNE         LA2D0           ; NO
                BSR         LA2FB           ; SEND STOP BIT (ACCB:0)
                PULS        CC,A            ; RESTORE OUTPUT CHARACTER & INTERRUPT STATUS
                CMPA        #CR             ; IS IT CARRIAGE RETURN?
                BEQ         LA2ED           ; YES
                INC         LPTPOS          ; INCREMENT CHARACTER COUNTER
                LDB         LPTPOS          ; CHECK FOR END OF LINE PRINTER LINE
                CMPB        LPTWID          ; AT END OF LINE PRINTER LINE?
                BLO         LA2F3           ; NO
LA2ED           CLR         LPTPOS          ; RESET CHARACTER COUNTER
                BSR         LA305
                BSR         LA305           ; DELAY FOR CARRIAGE RETURN
LA2F3           LDB         PIA1+2          ; WAIT FOR HANDSHAKE
                LSRB                        ;  CHECK FOR R5232 STATUS?
                BCS         LA2F3           ; NOT YET READY
                PULS        B,X,PC          ; RESTORE REGISTERS
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

LA2FB           LDB         #2              ; SET RS232 OUTPUT HIGH (MARKING)
LA2FD           STB         DA              ; STORE TO THE D/A CONVERTER REGISTER
                BSR         LA302           ; GO WAIT A WHILE
LA302           LDX         LPTBTD          ; GET BAUD RATE
                FCB         SKP2            ; SKIP NEXT TWO BYTES
LA305           LDX         LPTLND          ; PRINTER CARRIAGE RETURN DELAY
                JMP         >LA7D3          ; DELAY ON DECREMENTING X
; PUT A CHARACTER ON THE SCREEN
LA30A           PSHS        X,B,A           ; SAVE REGISTERS
                LDX         CURPOS          ; POINT X TO CURRENT CHARACTER POSITION
LA30E           CMPA        #BS             ; IS IT BACKSPACE?
                BNE         LA31D           ; NO
                CMPX        #VIDRAM         ; AT TOP OF SCREEN?
                BEQ         LA35D           ; YES - DO NOT ALLOW BACKSPACE
                LDA         #$60            ; BLANK
                STA         ,-X             ; PUT IN PREVIOUS POSITION
                BRA         LA344           ; SAVE NEW CURPOS
LA31D           CMPA        #CR             ; ENTER KEY?
                BNE         LA32F           ; BRANCH IF NOT
                LDX         CURPOS          ; GET CURRENT CHAR POSITION
LA323           LDA         #$60            ; BLANK
                STA         ,X+             ; PUT IT ON SCREEN
                TFR         X,D
                BITB        #$1F            ; TEST FOR BEGINNING OF NEW LINE
                BNE         LA323           ; PUT OUT BLANKS TILL NEW LINE
                BRA         LA344           ; CHECK FOR SCROLLING
LA32F           CMPA        #SPACE
                BCS         LA35D           ; BRANCH IF CONTROL CHARACTER
                TSTA                        ;  SET FLAGS
                BMI         LA342           ; IT IS GRAPHIC CHARACTER
                CMPA        #$40
                BCS         LA340           ; BRANCH IF NUMBER OR SPECIAL CHARACTER
                CMPA        #$60            ; UPPER/LOWER CASE?
                BCS         LA342           ; BRANCH IF UPPER CASE ALPHA
                ANDA        #$DF            ; CLEAR BIT 5, FORCE ASCII LOWER CASE TO BE UPPER CASE
LA340           EORA        #$40            ; INVERT BIT 6, CHANGE UPPER CASE TO LOWER & VICE VERSA
LA342           STA         ,X+             ; STORE CHARACTER TO SCREEN
LA344           STX         CURPOS          ; SAVE CURRENT CHAR POSITION
                CMPX        #VIDRAM+511     ; END OF SCREEN BUFFER?
                BLS         LA35D           ; RETURN IF NO NEED TO SCROLL
                LDX         #VIDRAM         ; TOP OF SCREEN
; SCROLL THE SCREEN
LA34E           LDD         32,X            ; GET TWO BYTES
                STD         ,X++            ; MOVE THEM UP ONE ROW
                CMPX        #VIDRAM+$1E0    ; AT THE LAST LINE?
                BCS         LA34E           ; NO
                LDB         #$60            ; BLANK
                JSR         >LA92D          ; BLANK LAST LINE
LA35D           PULS        A,B,X,PC        ; RESTORE REGISTERS
; SET UP TAB FIELD WIDTH, TAB ZONE, CURRENT POSITION
; AND LINE WIDTH ACCORDING TO THE DEVICE SELECTED
LA35F           JSR         >RVEC2          ; HOOK INTO RAM
                PSHS        X,B,A           ; SAVE REGISTERS
                CLR         PRTDEV          ; RESET PRINT DEVICE NUMBER
                LDA         DEVNUM          ; GET DEVICE NUMBER
                BEQ         LA373           ; BRANCH IF SCREEN
                INCA                        ;  CHECK FOR CASSETTE
                BEQ         LA384           ; BRANCH IF CASSETTE
; END UP HERE IF PRINTER
                LDX         LPTCFW          ; TAB FIELD WIDTH AND TAB ZONE
                LDD         LPTWID          ; PRINTER WIDTH AND POSITION
                BRA         LA37C           ; SET PRINT PARAMETERS
; SCREEN DISPLAY VALUES
LA373           LDB         CURPOS+1        ; GET CURSOR LOC LS BYTE
                ANDB        #$1F            ; KEEP ONLY COLUMN POSITION
                LDX         #$1010          ; TAB FIELD WIDTH AND LAST TAB ZONE
                LDA         #32             ; DISPLAY SCREEN LINE WIDTH
LA37C           STX         DEVCFW          ; SAVE TAB FIELD WIDTH AND ZONE
                STB         DEVPOS          ; SAVE PRINT POSITION
                STA         DEVWID          ; SAVE PRINT WIDTH
                PULS        A,B,X,PC        ; RESTORE REGISTERS
LA384           COM         PRTDEV          ; SET TO $FF FOR CASSETTE
                LDX         #$0100          ; TAB FIELD WIDTH = 1; ALL OTHER
                CLRA                        ;  PARAMETERS = 0
                CLRB                        ;
                BRA         LA37C           ; SET PRINT PARAMETERS
; THIS IS THE ROUTINE THAT GETS AN INPUT LINE FOR BASIC
; EXIT WITH BREAK KEY: CARRY = 1
; EXIT WITH ENTER KEY: CARRY = 0
LA38D           JSR         >CLRSCRN        ; CLEAR SCREEN
LA390           JSR         >RVEC12         ; HOOK INTO RAM
                CLR         IKEYIM          ; RESET BREAK CHECK KEY TEMP KEY STORAGE
                LDX         #LINBUF+1       ; INPUT LINE BUFFER
                LDB         #1              ; ACCB CHAR COUNTER: SET TO 1 TO ALLOW A
; BACKSPACE AS FIRST CHARACTER
LA39A           JSR         >LA171          ; GO GET A CHARACTER FROM CONSOLE IN
                TST         CINBFL          ; GET CONSOLE IN BUFFER FLAG
                BNE         LA3CC           ; BRANCH IF NO MORE CHARACTERS IN INPUT FILE
                TST         DEVNUM          ; CHECK DEVICE NUMBER
                BNE         LA3C8           ; BRANCH IF NOT SCREEN
                CMPA        #FORMF          ; FORM FEED
                BEQ         LA38D           ; YES - CLEAR SCREEN
                CMPA        #BS             ; BACKSPACE
                BNE         LA3B4           ; NO
                DECB                        ;  YES - DECREMENT CHAR COUNTER
                BEQ         LA390           ; BRANCH IF BACK AT START OF LINE AGAIN
                LEAX        -1,X            ; DECREMENT BUFFER POINTER
                BRA         LA3E8           ; ECHO CHAR TO SCREEN
LA3B4           CMPA        #$15            ; SHIFT RIGHT ARROW?
                BNE         LA3C2           ; NO
; YES, RESET BUFFER TO BEGINNING AND ERASE CURRENT LINE
LA3B8           DECB                        ;  DEC CHAR CTR
                BEQ         LA390           ; GO BACK TO START IF CHAR CTR = 0
                LDA         #BS             ; BACKSPACE?
                JSR         >PUTCHR         ; SEND TO CONSOLE OUT (SCREEN)
                BRA         LA3B8           ; KEEP GOING
LA3C2           CMPA        #3              ; BREAK KEY?
                ORCC        #1              ; SET CARRY FLAG
LA3C6           BEQ         LA3CD           ; BRANCH IF BREAK KEY DOWN
LA3C8           CMPA        #CR             ; ENTER KEY?
                BNE         LA3D9           ; NO
LA3CC           CLRA                        ;  CLEAR CARRY FLAG IF ENTER KEY - END LINE ENTRY
LA3CD           PSHS        CC              ; SAVE CARRY FLAG
                JSR         >LB958          ; SEND CR TO SCREEN
                CLR         ,X              ; MAKE LAST BYTE IN INPUT BUFFER = 0
                LDX         #LINBUF         ; RESET INPUT BUFFER POINTER
                PULS        CC,PC           ; RESTORE CARRY FLAG
; INSERT A CHARACTER INTO THE BASIC LINE INPUT BUFFER
LA3D9           CMPA        #$20            ; IS IT CONTROL CHAR?
                BLO         LA39A           ; BRANCH IF CONTROL CHARACTER
                CMPA        #'z+1
                BCC         LA39A           ; IGNORE IF > LOWER CASE Z
                CMPB        #LBUFMX         ; HAVE 250 OR MORE CHARACTERS BEEN ENTERED?
                BCC         LA39A           ; YES, IGNORE ANY MORE
                STA         ,X+             ; PUT IT IN INPUT BUFFER
                INCB                        ;  INCREMENT CHARACTER COUNTER
LA3E8           JSR         >PUTCHR         ; ECHO IT TO SCREEN
                BRA         LA39A           ; GO SET SOME MORE
; INPUT DEVICE NUMBER CHECK
LA3ED           JSR         >RVEC5          ; HOOK INTO RAM
                LDA         DEVNUM          ; DEVICE NUMBER
                BEQ         LA415           ; RETURN IF SCREEN
                INCA                        ;
                BNE         LA403           ; BRANCH IF NOT CASSETTE (BAD FILE MODE)
                LDA         FILSTA          ; GET FILE STATUS
                BNE         LA400           ; FILE IS OPEN
LA3FB           LDB         #22*2           ; FILE NOT OPEN ERROR
                JMP         >LAC46          ; JUMP TO ERROR SERVICING ROUTINE
LA400           DECA                        ;
                BEQ         LA415           ; FILE IS IN INPUT MODE, RETURN
LA403           JMP         >LA616          ; BAD FILE MODE ERROR
; PRINT DEVICE NUMBER CHECK
LA406           JSR         >RVEC6          ; HOOK INTO RAM
                LDA         DEVNUM          ; GET DEVICE NUMBER
                INCA                        ;
                BNE         LA415           ; RETURN IF NOT TAPE
                LDA         FILSTA          ; GET FILE STATUS
                BEQ         LA3FB           ; FILE NOT OPEN ERROR
                DECA                        ;
                BEQ         LA403           ; BAD FILE MODE - FILE IN INPUT MODE
LA415           RTS
; CLOSE
CLOSE           BEQ         LA426           ; BRANCH IF NO NAME SPECIFIED
                JSR         >LA5A5          ; CHECK DEVICE NUMBER
LA41B           BSR         LA42D           ; GO CLOSE A FILE
                JSR         GETCCH          ; GET CURRENT BASIC CHARACTER
                BEQ         LA44B           ; RETURN IF NO MORE FILES
                JSR         >LA5A2          ; CHECK SYNTAX AND DEVICE NUMBER
                BRA         LA41B           ; KEEP CLOSING FILES
; CLOSE ALL FILES HANDLER
LA426           JSR         >RVEC7          ; HOOK INTO RAM
LA429           LDA         #-1             ; CASSETTE DEVICE NUMBER
                STA         DEVNUM          ; SET DEVICE NUMBER
; CLOSE FILE HANDLER
LA42D           JSR         >RVEC8          ; HOOK INTO RAM
                LDA         DEVNUM          ; GET DEVICE NUMBER
                CLR         DEVNUM          ; SET TO SCREEN
                INCA                        ;
                BNE         LA44B           ; BRANCH IF WAS NOT CASSETTE
                LDA         FILSTA          ; GET FILE STATUS
                CMPA        #2              ; IS IT OUTPUT MODE
                BNE         LA449           ; NO
                LDA         CINCTR          ; GET CHARACTER BUFFER CTR
; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
                BEQ         LA449
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                BEQ         LA444           ; WRITE END OF PROG BLOCK IF BUFFER EMPTY
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                JSR         >LA2A8          ; WRITE A BLOCK TO TAPE
LA444           LDB         #$FF            ; END OF FILE TYPE BLOCK NUMBER
                JSR         >LA2AA          ; WRITE END OF FILE TYPE BLOCK
LA449           CLR         FILSTA          ; CASSETTE FILE STATUS CLOSED
LA44B           RTS
; CSAVE
CSAVE           JSR         >LA578          ; GO SCAN OFF NAME
                JSR         GETCCH          ; GET CURRENT CHARACTER IN THE BASIC LINE
                BEQ         LA469           ; BRANCH IF NONE
                JSR         >LB26D          ; SYNTAX ERROR IF NOT COMMA
                LDB         #'A             ; IS THIS AN ASCII SAVE?
                JSR         >LB26F          ; SYNTAX ERROR IF NOT A
                BNE         LA44B           ; RETURN IF NOT END OF LINE
                CLRA                        ;  FILE TYPE = 0
                JSR         >LA65C          ; WRITE OUT HEADER BLOCK
                LDA         #-1             ; CASSETTE CODE
                STA         DEVNUM          ; SET DEVICE NUMBER TO CASSETTE
                CLRA                        ;  CLEAR CARRY - FORCE LIST TO BEGIN AT PROGRAM START
                JMP         >LIST           ; GO DO A LIST TO CASSETTE
; NON-ASCII CSAVE
LA469           CLRA                        ;  FILE TYPE = 0
                LDX         ZERO            ; ZERO OUT ASCII FLAG AND FILE MODE
                JSR         >LA65F          ; WRITE HEADER BLOCK
                CLR         FILSTA          ; CLOSE FILES
                INC         BLKTYP          ; INCREMENT BLOCK NUMBER
                JSR         >WRLDR          ; WRITE 55S TO CASSETTE
                LDX         TXTTAB          ; ADDRESS OF PROGRAM START
LA478           STX         CBUFAD          ; STORE CURRENT BLOCK START ADDR
                LDA         #255            ; 255 BYTE BLOCKS
                STA         BLKLEN          ; BLOCK SIZE
                LDD         VARTAB          ; END OF PROGRAM
                SUBD        CBUFAD          ; CURRENT BLOCK STARTING ADDR
                BEQ         LA491           ; BRANCH IF IT CAME OUT EXACT
                CMPD        #255            ; MORE THAN 255 BYTES LEFT?
                BHS         LA48C           ; YES
                STB         BLKLEN          ; USE ACTUAL BLOCK SIZE IF LESS THAN 255
LA48C           JSR         >SNDBLK         ; WRITE BLOCK TO CASSETTE
                BRA         LA478           ; DO ANOTHER BLOCK
LA491           NEG         BLKTYP          ; MAKE BLOCK NUMBER NEGATIVE (EOF BLOCK)
                CLR         BLKLEN          ; ZERO BLOCK SIZE
                JMP         >LA7E7          ; WRITE A BLOCK, TURN OFF MOTOR
; CLOAD
CLOAD           CLR         FILSTA          ; CLOSE FILES
                CMPA        #'M             ; IS IT CLOADM?
                BEQ         LA4FE           ; BRANCH IF SO
                LEAS        2,S             ; GET RID OF THE RETURN
                JSR         >LA5C5          ; GO GET FILE NAME
                JSR         >LA648          ; SEARCH FOR FILE
                TST         CASBUF+10       ; GET FILE MODE (NON-ZERO=DATA OR ASCII)
                BEQ         LA4C8           ; ZERO = CRUNCHED BASIC OR MACHINE LANG
                LDA         CASBUF+9        ; GET ASCII FLAG
                BEQ         LA4CD           ; BAD FILE NODE 0 = CRUNCHED OR MACH LANG
                JSR         >LAD19          ; DO A NEW
                LDA         #-1             ; TAPE DEVICE NUMBER
                STA         DEVNUM          ; SET DEVICE NUMBER TO TAPE
                INC         FILSTA          ; FILE TYPE = INPUT
                JSR         >LA635          ; GO LOAD ASCII RECORD
                JMP         >LAC7C          ; GO LOAD AND CRUNCH INPUT
; COME HERE FROM BASICS DIRECT LOOP IF CONSOLE
; IN BUFFER EMPTY
LA4BF           JSR         >RVEC13         ; HOOK INTO RAM
                JSR         >LA42D          ; CLOSE ACTIVE FILE
                JMP         >LAC73          ; GO TO BASICS DIRECT LOOP
; CLOAD A CRUNCHED BASIC
LA4C8           LDA         CASBUF+8        ; FILE TYPE
                BEQ         LA4D0           ; ZERO IS CSAVE TYPE
LA4CD           JMP         >LA616          ; BAD FILE MODE IF NOT BASIC FILE
LA4D0           JSR         >LAD19          ; DO A NEW
                JSR         >CASON          ; TURN ON TAPE, START READING
                LDX         TXTTAB          ; GET START OF PROGRAM ADDRESS
LA4D8           STX         CBUFAD          ; STORE IT IN LOAD BUFFER
                LDD         CBUFAD          ; GET START ADDRESS TO D REG
                INCA                        ;  ADD 256 TO LOAD ADDRESS
                JSR         >LAC37          ; SEE IF ROOM BELOW STACK FOR ONE BLOCK
                JSR         >GETBLK         ; READ A BLOCK
                BNE         LA4F8           ; GOT AN ERROR DURING READ
                LDA         BLKTYP          ; BLOCK NUMBER
                BEQ         LA4F8           ; I/O ERROR IF HEADER BLOCK TYPE
                BPL         LA4D8           ; REAR MORE IF BLOCK NUMBER POSITIVE
                STX         VARTAB          ; SET END OF PROGRAM ADDRESS
                BSR         LA53B           ; TURN OFF TAPE DECK
                LDX         #LABED-1        ; POINT TO OK MESSAGE
                JSR         >LB99C          ; PRINT OK TO CONSOLE OUT
                JMP         >LACE9          ; RESET INPUT POINTER, CLEAR VARIABLES AND
; RETURN TO MAIN LOOP OF BASIC
LA4F8           JSR         >LAD19          ; DO A NEW
LA4FB           JMP         >LA619          ; I/O ERROR
; CLOADM
LA4FE           JSR         GETNCH          ; GET NEXT CHARACTER IN BASIC LINE
                BSR         LA578           ; GO SCAN OFF NAME
                JSR         >LA648          ; SEARCH FOR FILE
LA505           LDX         ZERO            ; STORE ZERO TO X REG, DEFAULT OFFSET VALUE
                JSR         GETCCH          ; CHECK FOR AN OFFSET
                BEQ         LA511           ; BRANCH IF NO OFFSET
                JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVALUATE OFFSET; RETURN VALUE IN X
LA511           LDA         CASBUF+8        ; CHECK FILE MODE
                CMPA        #2              ; IS IT MACHINE LANGUAGE?
                BNE         LA4CD           ; BAD FILE MODE ERROR IF NOT
                LDD         CASBUF+11       ; GET TRANSFER ADDR FROM TAPE
                LEAU        D,X             ; ADD OFFSET
                STU         EXECJP          ; STORE TRANSFER ADDR IN EXEC ARGUMENT
                TST         CASBUF+10       ; CHECK FILE MODE
                BNE         LA4CD           ; BAD FILE MODE ERROR
                LDD         CASBUF+13       ; GET LOAD ADDR FROM TAPE
                LEAX        D,X             ; ADD OFFSET
                STX         CBUFAD          ; STORE IN BUFFER START ADDRESS POINTER
                JSR         >CASON          ; START UP TAPE
LA52E           JSR         >GETBLK         ; READ A BLOCK
                BNE         LA4FB           ; BRANCH IF I/O ERROR
                STX         CBUFAD          ; STORE NEW START ADDR (ONE BLOCK HIGHER)
                TST         BLKTYP          ; CHECK BLOCK NUMBER
                BEQ         LA4FB           ; BRANCH IF I/O ERROR (HEADER BLOCK)
                BPL         LA52E           ; GO READ SOME MORE
LA53B           JMP         >LA7E9          ; GO TURN OFF TAPE DECK
; EXEC
EXEC            BEQ         LA545           ; BRANCH IF NO ARGUMENT
                JSR         >LB73D          ; EVALUATE ARGUMENT - ARGUMENT RETURNED IN X
                STX         EXECJP          ; STORE X TO EXEC JUMP ADDRESS
LA545           JMP         [EXECJP]        ; GO DO IT
; BREAK CHECK
LA549           JSR         >RVEC11         ; HOOK INTO RAM
                LDA         DEVNUM          ; GET DEVICE NUMBER
                INCA                        ;  CHECK FOR TAPE
                BEQ         LA5A1           ; RETURN IF TAPE
                JMP         >LADEB          ; GO DO BREAK KEY CHECK
; THIS ROUTINE EVALUATES AN ARGUMENT
; AND MAKES SURE IT IS WITHIN LIMITS OF VIDEO DISPLAY RAM
LA554           JSR         >LB3E4          ; EVALUATE EXPRESSION AND RETURN VALUE IN ACCD
                SUBD        #511            ; ONLY 512 VIDEO DISPLAY LOCATIONS
                LBHI        LB44A           ; BRANCH IF > 511 TO ILLEGAL FUNCTION CALL
                ADDD        #VIDRAM+511     ; ADD BACK IN OFFSET + START OF VIDEO RAM
                STD         CURPOS          ; PUT THE CURSOR THERE
                RTS
; INKEY$
INKEY           LDA         IKEYIM          ; WAS A KEY DOWN IN THE BREAK CHECK?
                BNE         LA56B           ; YES
                JSR         >KEYIN          ; GO GET A KEY
LA56B           CLR         IKEYIM          ; CLEAR INKEY RAM IMAGE
                STA         FPA0+3          ; STORE THE KEY IN FPA0
                LBNE        LB68F           ; CONVERT FPA0+3 TO A STRING
                STA         STRDES          ; SET LENGTH OF STRING = 0 IF NO KEY DOWN
                JMP         >LB69B          ; PUT A NULL STRING ONTO THE STRING STACK
; STRIP A FILENAME OFF OF THE BASIC INPUT LINE
LA578           LDX         #CFNBUF         ; POINT TO FILE NAME BUFFER
                CLR         ,X+             ; CLEAR THE FIRST BYTE - IT WILL CONTAIN THE COUNT
; OF THE NUMBER OF CHARACTERS IN THE NAME
                LDA         #SPACE          ; SPACE
LA57F           STA         ,X+             ; BLANK FILL 8 CHARS
                CMPX        #CASBUF         ; DONE?
                BNE         LA57F           ; NO
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                BEQ         LA5A1           ; RETURN IF NO NAME
                JSR         >LB156          ; GET THE FILE NAME - EVALUATE EXPRESSION
                JSR         >LB654          ; POINT X TO START OF NAME (TOP STRING ON STRING STACK)
                LDU         #CFNBUF         ; CASSETTE FILE NAME BUFFER
                STB         ,U+             ; STORE THE NUMBER OF BYTES IN THE NAME
                BEQ         LA5A1           ; NULL NAME (BLANK NAME)
                FCB         SKP2            ; SKIP THE NEXT TWO BYTES
LA598           LDB         #8              ; MOVE 8 BYTES
; MOVE ACCB BYTES FROM (X) TO (U)
LA59A           LDA         ,X+             ; GET BYTE FROM X
                STA         ,U+             ; STORE IT AT U
                DECB                        ;  MOVED ALL BYTES?
                BNE         LA59A           ; NO
LA5A1           RTS
; GET DEVICE NUMBER FROM BASIC LINE - CHECK VALIDITY
LA5A2           JSR         >LB26D          ; CHECK FOR COMMA, SYNTAX ERROR IF NONE
LA5A5           CMPA        #'#             ; IS NEXT CHARACTER A NUMBER?
                BNE         LA5AB           ; NO
                JSR         GETNCH          ; GET NEXT BASIC INPUT CHARACTER
LA5AB           JSR         >LB141          ; EVALUATE EXPRESSION
LA5AE           JSR         >INTCNV         ; CONVERT FPA0 TO INTEGER, RETURN VALUE IN ACCD
                ROLB                        ;  MSB OF ACCB TO CARRY
                ADCA        #0              ; ADD MSB OF ACCB TO ACCA
                BNE         LA61F           ; DEVICE # ERROR IF ACCA<FF80 OR >007F
                RORB                        ;  RESTORE ACCB
                STB         DEVNUM          ; STORE B IN DEVICE NUMBER
                JSR         >RVEC1          ; HOOK INTO RAM
                BEQ         LA5C4           ; BRANCH IF DEVICE NUMBER SET TO SCREEN
                BPL         LA61F           ; DEVICE NUMBER ERROR IF POSITIVE DEVICE NUMBER
                CMPB        #-2             ; LOWEST LEGAL DEVICE NUMBER
                BLT         LA61F           ; DEVICE NUMBER ERROR
LA5C4           RTS
; THIS ROUTINE WILL SCAN OFF THE FILE NAME FROM A BASIC LINE
; AND RETURN A SYNTAX ERROR IF THERE ARE ANY CHARACTERS
; FOLLOWING THE END OF THE NAME
LA5C5           BSR         LA578           ; SCAN OFF NAME
LA5C7           JSR         GETCCH          ; GET CURRENT INPUT CHAR FROM BASIC LINE
LA5C9           BEQ         LA5C4           ; RETURN IF END OF LINE
                JMP         >LB277          ; SYNTAX ERROR IF ANY MORE CHARACTERS
; EOF
EOF             JSR         >RVEC14         ; HOOK INTO RAM
                LDA         DEVNUM          ; GET DEVICE NUMBER
                PSHS        A               ; SAVE IT
                BSR         LA5AE           ; CHECK DEVICE NUMBER
                JSR         >LA3ED          ; CHECK FOR PROPER FILE AND MODE
LA5DA           CLRB                        ;  NOT EOF FLAG = 0
                LDA         DEVNUM          ; TEST DEVICE NUMBER
                BEQ         LA5E4           ; BRANCH IF NOT SET TO DISPLAY
                TST         CINCTR          ; ANY CHARACTERS LEFT TO SEND?
                BNE         LA5E4           ; YES
                COMB                        ;  NO - EOF: SET FLAG = -1 ($FF)
LA5E4           PULS        A               ; GET DEVICE NUMBER BACK AGAIN
                STA         DEVNUM          ; RESTORE IT
LA5E8           SEX         CONVERT         ; ACCB TO 2 DIGIT SIGNED INTEGER
                JMP         >GIVABF         ; CONVERT ACCD TO FLOATING POINT
; SKIPF
SKIPF           BSR         LA5C5           ; SCAN OFF THE BASIC FILE NAME
                BSR         LA648           ; LOOK FOR THAT FILE ON TAPE
                JSR         >LA6D1          ; READ THE FILE
                BNE         LA619           ; I/O ERROR
                RTS
; OPEN
OPEN            JSR         >RVEC0          ; HOOK INTO RAM
                JSR         >LB156          ; GET FILE STATUS (INPUT,OUTPUT)
                JSR         >LB6A4          ; GET FIRST BYTE OF STATUS STRING TO ACCB
                PSHS        B               ; SAVE IT ON STACK
                BSR         LA5A2           ; CHECK FOR SYNTAX AND GET DEVICE NUMBER
LA603           JSR         >LB26D          ; SYNTAX CHECK FOR COMMA, SYNTAX ERROR IF NOT
                BSR         LA5C5           ; GET FILE NAME
                LDA         DEVNUM          ; GET DEVICE NUMBER
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                PULS        B               ; GET STATUS AGAIN
                CMPB        #'I             ; IS IT INPUT MODE?
                BEQ         LA624           ; YES
                CMPB        #'O             ; IS IT OUTPUT MODE?
                BEQ         LA658           ; YES
; IF IT ISNT INPUT OR OUTPUT, BAD FILE MODE
LA616           LDB         #21*2           ; ERROR # 21 BAD FILE MODE
                FCB         SKP2            ; SKIP TWO BYTES
LA619           LDB         #20*2           ; ERROR # 20 I/O ERROR
                FCB         SKP2            ; SKIP TWO BYTES
LA61C           LDB         #18*2           ; ERROR # 18 FILE ALREADY OPEN
                FCB         SKP2            ; SKIP TWO BYTES
LA61F           LDB         #19*2           ; ERROR # 19 DEVICE NUMBER ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER

LA624           INCA                        ;  DEVICE NUMBER SET TO TAPE?
                BMI         LA616           ; BAD FILE MODE IF DEVNUM = NEG BUT NOT CASSETTE
                BNE         LA657           ; RETURN IF DEVNUM WAS SET TO SCREEN OR DISK
; SET TO TAPE
                BSR         LA648           ; GET HEADER BLOCK
                LDA         CASBUF+9        ; GET ASCII FLAG
                ANDA        CASBUF+10       ; AND IT WITH FILE MODE
                BEQ         LA616           ; BAD FILE MODE - CRUNCHED FlLE OR MACH LANG
                INC         FILSTA          ; OPEN FILE FOR INPUT
LA635           JSR         >LA701          ; START TAPE, READ A BLOCK
                BNE         LA619           ; I/O ERROR
                TST         BLKTYP          ; CHECK BLOCK NUMBER
                BEQ         LA619           ; I/O ERROR IF HEADER BLOCK
                BMI         LA657           ; BRANCH IF THIS IS THE LAST BLOCK
                LDA         BLKLEN          ; CHAR COUNT
                BEQ         LA635           ; READ ANOTHER BLOCK IF NULL BLOCK
LA644           STA         CINCTR          ; STORE IN TEMP CHARACTER COUNTER
                BRA         LA652           ; RESET BUFFER POINTER
; SEARCH FOR FILE NAME IN CNMBUF
LA648           TST         FILSTA          ; IS THE FILE OPEN?
                BNE         LA61C           ; YES- FILE ALREADY OPEN
                BSR         LA681           ; SEARCH FOR CORRECT FILE NAME
                BNE         LA619           ; I/O ERROR
LA650           CLR         CINCTR          ; CLEAR CHARACTER COUNTER
LA652           LDX         #CASBUF         ; CASSETTE INPUT BUFFER ADDRESS
                STX         CINPTR          ; RESET IT
LA657           RTS
; WRITE OUT THE HEADER BLOCK
; CASBUF FILE NAME
; CASBUF+8 FILE TYPE
; CASBUF+9 ASCII FLAG
; CASBUF+10 FILE MODE
; CASBUF+11,12 TRANSFER ADDRESS
; CASBUF+13,14 START ADDRESS
; ENTER HERE FOR DATA FILES W/DEVICE NUMBER IN ACCA
LA658           INCA                        ;  CHECK FOR CASSETTE DEVICE NUMBER
                BNE         LA657           ; RETURN IF DEVICE NUMBER WASNT TAPE
                INCA                        ;  MAKE FILE TYPE = 1
; ENTER HERE FOR ASCII FILES
LA65C           LDX         #$FFFF          ; SET ASCII FLAG AND MODE = $FF
LA65F           TST         FILSTA          ; IS FILE OPEN?
                BNE         LA61C           ; YES- FILE ALREADY OPEN
                LDU         #CASBUF         ; CASSETTE INPUT BUFFER
                STU         CBUFAD          ; STORE IN STARTING ADDRESS
                STA         8,U             ; FILE TYPE IN CASBUF+8
                STX         9,U             ; ASCII FLAG & MODE IN CASBUF+9, CASBUF+10
; CASBUF +8 +9 +10
; TYPE ASCII MODE
; BASIC CRUNCHED 00 00 00
; BASIC ASCII 00 FF FF
; DATA 01 FF FF
; MACHINE LANGUAGE 02 00 00
; MACHINE BLK LOAD 02 00 FF
                LDX         #CFNBUF+1       ; POINT X TO FILE NAME BUFFER
                JSR         >LA598          ; MOVE 8 BYTES FROM (X) TO (U)
                CLR         BLKTYP          ; ZERO BLOCK NUMBER
                LDA         #15             ; 15 BYTES IN THE HEADER BLOCK
                STA         BLKLEN          ; CHAR COUNT
                JSR         >LA7E5          ; GO WRITE ONE BLOCK
                LDA         #2              ; OUTPUT FILE
                STA         FILSTA          ; STORE IN FILE MODE
                BRA         LA650           ; RESET POINTERS
; SEARCH FOR CORRECT CASSETTE FILE NAME
LA681           LDX         #CASBUF         ; CASSETTE BUFFER
                STX         CBUFAD          ; LOAD ADDRESS POINTER
LA686           LDA         CURLIN          ; GET CURRENT LINE NUMBER MSB (CURLIN)
                INCA                        ;  IN DIRECT MODE IF ACCA = $FF
                BNE         LA696           ; BRANCH IF NOT DIRECT MODE
                JSR         >CLRSCRN        ; CLEAR SCREEN
                LDX         CURPOS          ; CURRENT SCREEN CHAR POSITION
                LDB         #'S             ; S MEANS SEARCHING
                STB         ,X++            ; PUT AN S ON THE SCREEN
                STX         CURPOS          ; STORE NEW CURSOR LOCATION
LA696           BSR         LA701           ; READ ONE BLOCK FROM TAPE
                ORB         BLKTYP          ; OR ERROR FLAG WITH BLOCK NUMBER
                BNE         LA6D0           ; BRANCH IF NOT BLOCK ZERO OR ERROR
                LDX         #CASBUF         ; POINT TO CASSETTE BUFFER
                LDU         #CFNBUF+1       ; POINT TO DESIRED NAME
                LDB         #8              ; EIGHT CHARACTERS MAX IN NAME
                CLR         ,-S             ; ZERO A BYTE ON THE STACK
LA6A6           LDA         ,X+             ; GET CHAR FROM CASSETTE BLOCK
                LDY         CURLIN          ; GET CURLIN
                LEAY        1,Y             ; DIRECT MODE?
                BNE         LA6B4           ; FALL THROUGH IF DIRECT MODE
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         >PUTCHR         ; OUTPUT A CHAR
LA6B4           SUBA        ,U+             ; SUBTRACT A CHAR FROM DESIRED NAME NON-ZERO RESULT IF NO MATCH
                ORA         ,S              ; OR WITH TOP OF STACK, RESULT WILL BE NON-ZERO IF MISMATCH
                STA         ,S              ; SAVE IT
                DECB                        ;  DONE ALL 8 CHARACTERS?
                BNE         LA6A6           ; NO
                LDA         ,S+             ; SEE IF ALL CHARS WERE OK
                BEQ         LA6CB           ; BRANCH IF GOOD COMPARE
                TST         -9,U            ; CHECK THE NUMBER OF CHARACTERS IN THE CLOAD STATEMENT
                BEQ         LA6CB           ; IF NO NAME SPECIFIED, ANY FILE IS OK
; DIDN'T FIND THE RIGHT FILE IF HERE
                BSR         LA6D1           ; LOOK FOR FILE
                BNE         LA6D0           ; RETURN IF ERROR
                BRA         LA686           ; GO LOOK SOME MORE
LA6CB           LDA         #'F
                BSR         LA6F8           ; PUT F ON THE SCREEN IF DIRECT MODE
                CLRA                        ;  SET ZERO FLAG TO INDICATE NO ERRORS
LA6D0           RTS
LA6D1           TST         CASBUF+10       ; CHECK FILE MODE
                BNE         LA6DF           ; BRANCH IF ASCII OR DATA
                JSR         >CASON          ; TURN ON TAPE DECK
LA6D9           BSR         GETBLK          ; LOAD A BLOCK FROM TAPE
                BSR         LA6E5           ; CHECK FOR ERROR OR LAST BLOCK
                BRA         LA6D9           ; KEEP GOING
LA6DF           BSR         LA701           ; READ ONE BLOCK FROM TAPE
                BSR         LA6E5           ; CHECK FOR ERROR OR LAST BLOCK
                BRA         LA6DF           ; KEEP READING BLOCKS
LA6E5           BNE         LA6ED           ; GOT AN ERROR ON READING IN BLOCK
                LDA         BLKTYP          ; GET BLOCK NUMBER
                NEGA                        ;  CHECK FOR LAST BLOCK
; -----------------------------------------------------------------------------
                if          VERBAS<11
; -----------------------------------------------------------------------------
                BMI         LA6F3           ; RETURN IF NOT AN END OF PROGRAM BLOCK
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; make cursor in upper left blink
                BMI         LA700           ; RETURN IF NOT AN END OF PROGRAM BLOCK
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                DECA                        ;  IF BLOCK NUMBER WAS $FF, ACCA IS NOW ZERO - THIS WILL
; CAUSE CLOAD TO IGNORE ERRORS IN THE
; BLOCKS WHICH IT IS SKIPPING WHILE
; LOOKING FOR THE CORRECT FILE NAME.
LA6ED           STA         CSRERR          ; STORE ACCA TO ERROR FLAG
                LEAS        2,S             ; REMOVE RETURN ADDRESS FROM STACK
                BRA         LA705           ; TURN OFF MOTOR
LA6F3           LDA         VIDRAM          ; GET FIRST CHAR ON SCREEN
                EORA        #$40            ; REVERSE THE VIDEO
LA6F8           LDB         CURLIN          ; GET CURLIN MSB
                INCB                        ;  CHECK FOR DIRECT MODE
                BNE         LA700           ; BRANCH IF NOT DIRECT MODE
                STA         VIDRAM          ; PUT IT ON SCREEN
LA700           RTS
; READ A BLOCK FROM CASSETTE
LA701           BSR         CASON           ; START TAPE, AND LOOK FOR A BUNCH OF $55 OR $AA BYTES
                BSR         GETBLK          ; READ A BLOCK
LA705           JSR         >LA7E9          ; TURN OFF MOTOR
                LDB         CSRERR          ; GET ERROR STATUS
                RTS
GETBLK          ORCC        #$50            ; DISABLE IRQ,FIRQ
                BSR         LA6F3           ; REVERSE VIDEO UPPER LEFT CHAR IF DIRECT MODE
                LDX         CBUFAD          ; GET LOAD ADDRESS
                CLRA                        ;  RESET ACCA
LA712           BSR         LA755           ; READ A BIT FROM TAPE, RETURN IT IN CARRY FLAG
                RORA                        ;  PUT BIT IN MSB OF ACCA
                CMPA        #$3C            ; GET SYNCED ON $3C
                BNE         LA712           ; NOT SYNCED YET
                BSR         LA749           ; GET BLOCK NUMBER
                STA         BLKTYP          ; SAVE IT
                BSR         LA749           ; GET CHAR COUNT
                STA         BLKLEN          ; SAVE IT
                ADDA        BLKTYP          ; ACCUMULATE CHECKSUM
                STA         CCKSUM          ; SAVE IT
                LDA         BLKLEN          ; GET BACK CHAR COUNT
                STA         CSRERR          ; TEMP SAVE
                BEQ         LA73B           ; NULL SET OF CHARACTERS
LA72B           BSR         LA749           ; GET BYTE FROM TAPE
                STA         ,X              ; FILL MEMORY WITH TAPE DATA
                CMPA        ,X+             ; SEE IF WE READ BACK SAME THING
                BNE         LA744           ; BRANCH IF NOT PUTTING IT IN RAM
                ADDA        CCKSUM          ; ACCUMULATE CHECKSUM
                STA         CCKSUM          ; TEMP STORE CHECKSUM
                DEC         CSRERR          ; DECR TEMP CHAR COUNT
                BNE         LA72B           ; GET ANOTHER CHARACTER
LA73B           BSR         LA749           ; GET CHECKSUM FROM TAPE
                SUBA        CCKSUM          ; COMPARE TO CALCULATED CHECKSUM
                BEQ         LA746           ; BRANCH IF OK
                LDA         #1              ; CHECKSUM ERROR FLAG
                FCB         SKP2            ; SKIP TWO BYTES
LA744           LDA         #2              ; NON-RAM ERROR FLAG
LA746           STA         CSRERR          ; 1 IF CHECKSUM ERROR, 2 IF LOADING INTO NON-RAM
                RTS
; GET A BYTE FROM TAPE
LA749           LDA         #8              ; 8 BITS/BYTE
                STA         CPULWD          ; TEMP COUNTER
LA74D           BSR         LA755           ; READ A BIT FROM TAPE
                RORA                        ;  PUT IT INTO ACCA
                DEC         CPULWD          ; GOT ALL 8 BITS
                BNE         LA74D           ; NO
                RTS
; READ A BIT FROM THE TAPE
LA755           BSR         LA75D           ; GET THE TIME BETWEEN TRANSITIONS
                LDB         CPERTM          ; GET PERIOD TIMER
                DECB                        ;
                CMPB        CMPMID          ; CMPMID CONTAINS 18 INITIALLY, AND IS USED TO DETERMINE
; WHETHER THE BIT READ IS A ONE OR ZERO
; IF THE PERIOD TIMER IS < 18, THE BIT
; IS CONSIDERED TO BE A ONE, IF > 18, IT IS ZERO
                RTS
; MAIN TIMING LOOP
LA75D           CLR         CPERTM          ; RESET PERIOD TIMER
                TST         CBTPHA          ; CHECK TO SEE IF SYNCED ON THE HI-LO TRANSITION OR LO-HI
                BNE         LA773           ; BRANCH ON HI-LO TRANSITION
; LO - HI TRANSITION
LA763           BSR         LA76C           ; READ CASSETTE INPUT BIT
                BCS         LA763           ; LOOP UNTIL IT IS LO
LA767           BSR         LA76C           ; READ CASSETTE INPUT DATA
                BCC         LA767           ; WAIT UNTIL IT GOES HI
                RTS
; READ CASSETTE INPUT BIT OF THE PIA
LA76C           INC         CPERTM          ; INCREMENT PERIOD TIMER
                LDB         PIA1            ; GET CASSETTE INPUT BIT
                RORB                        ;  PUT CASSETTE BIT INTO THE CARRY FLAG
                RTS
; WAIT FOR HI - LO TRANSITION
LA773           BSR         LA76C           ; READ CASSETTE INPUT DATA
                BCC         LA773           ; LOOP UNTIL IT IS HI
LA777           BSR         LA76C           ; READ CASSETTE INPUT
                BCS         LA777           ; LOOP UNTIL IT IS LO
                RTS
; LOOK FOR THE SYNC BYTES - RETURN WITH ACCA = 0 IF SYNCED
; ON HI - LO TRANSITION, ACCA = $A0 IF SYNCED ON THE
; LO - HI TRANSITION OF THE INPUT SIGNAL FROM THE CASSETTE.
CASON           ORCC        #$50            ; DISABLE IRQ,FIRQ
                BSR         LA7CA           ; TURN ON TAPE DECK MOTOR
                CLR         CPULWD          ; RESET UP TO SPEED COUNTER
LA782           BSR         LA763           ; WAIT FOR LO-HI TRANSITION
LA784           BSR         LA7AD           ; WAIT FOR HI-LO TRANSITION
                BHI         LA797           ; CASSETTE SPEED IN RANGE FOR 1200 HZ
LA788           BSR         LA7A7           ; WAIT FOR LO-HI TRANSITION
                BCS         LA79B           ; CASSETTE SPEED IN RANGE FOR 2400 HZ
                DEC         CPULWD          ; DECREMENT UP TO SPEED COUNTER IF SYNCED ON LO-HI
                LDA         CPULWD          ; GET IT
                CMPA        #-96            ; HAVE THERE BEEN 96 CONSECUTIVE 1-0-1-0 PATTERNS
LA792           BNE         LA782           ; NO
                STA         CBTPHA          ; SAVE WHICH TRANSITION (HI-LO OR LO-HI)
                RTS
LA797           BSR         LA7A7           ; WAIT FOR LO-HI TRANSITION
                BHI         LA784           ; BRANCH IF TWO CONSECUTIVE 1200 HZ PULSES
LA79B           BSR         LA7AD           ; WAIT FOR HI-LO TRANSITION
                BCS         LA788           ; BRANCH IF TWO CONSECUTIVE 2400 HZ PULSES
                INC         CPULWD          ; INCREMENT UP TO SPEED COUNTER IF SYNCED ON HI-LO
                LDA         CPULWD          ; GET IT
                SUBA        #96             ; GOT ENOUGH SYNC PULSES? - ACCA WILL BE ZERO IF
; THERE HAVE BEEN 96 CONSECUTIVE 0-1-0-1 PATTERNS
                BRA         LA792
LA7A7           CLR         CPERTM          ; RESET PERIOD TIMER
                BSR         LA767           ; WAIT UNTIL CASSETTE INPUT GOES HI
                BRA         LA7B1
LA7AD           CLR         CPERTM          ; RESET PERIOD TIMER
                BSR         LA777           ; WAIT UNTIL CASSETTE GOES LO
LA7B1           LDB         CPERTM          ; GET PERIOD TIMER
                CMPB        CMP0            ; UPPER LIMIT OF 1200 HZ PERIOD
                BHI         LA7BA           ; BRANCH IF CASSETTE SPEED IS TOO SLOW OR DROPOUT
                CMPB        CMP1            ; UPPER LIMIT OF 2400 HZ PERIOD
                RTS
LA7BA           CLR         CPULWD          ; RESET UP TO SPEED COUNTER
                RTS
; MOTOR
MOTOR           TFR         A,B             ; SAVE CURRENT TOKEN IN ACCB
                JSR         GETNCH          ; GET NEXT INPUT CHARACTER FROM BASIC
                CMPB        #$AA            ; OFF TOKEN
                BEQ         LA7E9           ; YES
                CMPB        #$88            ; ON TOKEN
                JSR         >LA5C9          ; SYNTAX ERROR IF IT WASNT ON OR OFF
LA7CA           LDA         PIA1+1          ; READ CRA OF U4
                ORA         #$08            ; TURN ON BIT 3 WHICH ENABLES MOTOR DELAY
                BSR         LA7F0           ; PUT IT BACK
LA7D1           LDX         ZERO            ; GET READY TO WAIT A WHILE
; DELAY WHILE DECREMENTING X TO ZERO
LA7D3           LEAX        -1,X            ; DECREMENT X
                BNE         LA7D3           ; BRANCH IF NOT ZERO
                RTS
; SEND SYNCLN $55S TO TAPE
WRLDR           ORCC        #$50            ; DISABLE INTERRUPTS
                BSR         LA7CA           ; TURN ON TAPE DECK MOTOR
                LDX         SYNCLN          ; GET COUNT OF $55S TO SEND
LA7DE           BSR         LA828           ; SEND $55 TO TAPE
                LEAX        -1,X            ; ARE ALL $55S SENT?
                BNE         LA7DE           ; NO
                RTS
; WRITE SYNC BYTES AND A BLOCK TO TAPE
LA7E5           BSR         WRLDR           ; WRITE SYNC BYTES TO TAPE
LA7E7           BSR         SNDBLK          ; GO WRITE A BLOCK
; TURN OFF TAPE DECK MOTOR
LA7E9           ANDCC       #$AF            ; ENABLE IRQ,FIRQ
                LDA         PIA1+1          ; READ CRA OF U4
                ANDA        #$F7            ; TURN OFF BIT 3
LA7F0           STA         PIA1+1          ; PUT IT BACK
                RTS
; WRITE A BLOCK TO CASSETTE
; BUFFER SIZE IN BLKLEN
; STARTING ADDR IN CBUFAD
; BLOCK NUMBER IN BLKTYP
SNDBLK          ORCC        #$50            ; DISABLE IRQ,FIRQ
                LDB         BLKLEN          ; GET CHAR COUNT
                STB         CSRERR          ; TEMP CHAR COUNT
                LDA         BLKLEN          ; GET CHAR COUNT (INCLUDED IN CHECKSUM)
                BEQ         LA805           ; BRANCH IF NO CHARACTERS - NULL
                LDX         CBUFAD          ; GET STARTING ADDRESS
LA800           ADDA        ,X+             ; CHECKSUM THE BUFFER
                DECB                        ;  DONE ALL CHARACTERS?
                BNE         LA800           ; NO
LA805           ADDA        BLKTYP          ; ADD IN THE BLOCK NUMBER
                STA         CCKSUM          ; SAVE THE CHECKSUM
                LDX         CBUFAD          ; GET STARTING ADDRESS
                BSR         LA828           ; SEND $55 TO TAPE
                LDA         #$3C            ; SYNC CHAR
                BSR         LA82A           ; SEND TO TAPE
                LDA         BLKTYP          ; GET BLOCK NUMBER
                BSR         LA82A           ; SEND BLOCK NUMBER TO TAPE
                LDA         BLKLEN          ; GET CHARACTER COUNT
                BSR         LA82A           ; SEND CHAR COUNT TO TAPE
                TSTA                        ;  SET FLAGS
                BEQ         LA824           ; BRANCH IF CHAR COUNT IS ZERO
LA81C           LDA         ,X+             ; GET BUFFER CHARACTER
                BSR         LA82A           ; SEND BUFFER TO TAPE
                DEC         CSRERR          ; DECR TEMP CHAR COUNT
                BNE         LA81C           ; NOT DONE YET
LA824           LDA         CCKSUM          ; GET CHECKSUM
                BSR         LA82A           ; SEND CHECKSUM TO TAPE
LA828           LDA         #$55            ; SEND A $55 TO TAPE
; THIS ROUTINE SENDS THE A REG TO TAPE
LA82A           PSHS        A               ; SAVE OUTPUT CHARACTER
                LDB         #1              ; ACCB CONTAINS A MASK USED TO DETERMINE WHETHER A
; BIT IN THE OUTPUT CHARACTER IS HI OR LO
LA82E           LDA         CLSTSN          ; GET THE ENDING VALUE OF THE LAST SINE CYCLE
                STA         DA              ; STORE IN THE D/A CONVERTER
                LDY         #LA85C          ; SINE LOOK-UP TABLE FOR GENERATING FSK
                BITB        ,S              ; IS THE CURRENT BIT A ONE OR A ZERO ?
                BNE         LA848           ; IF A 1, DO HIGH FREQ
; LOW FREQUENCY LOOK UP
LA83B           LDA         ,Y+             ; USE EVERY BYTE IN TABLE IF LOW FREQUENCY
                CMPY        #LA85C+36       ; END OF SINE TABLE?
                BEQ         LA855           ; YES
                STA         DA              ; SEND NEXT VALUE TO D/A CONVERTER
                BRA         LA83B           ; GET NEXT VALUE
; HIGH FREQUENCY LOOK UP
LA848           LDA         ,Y++            ; USE EVERY OTHER BYTE IF HIGH FREQUENCY
                CMPY        #LA85C+36       ; END OF SINE TABLE?
                BEQ         LA855           ; YES
                STA         DA              ; SEND NEXT VALUE TO D/A CONVERTER
                BRA         LA848           ; GET NEXT VALUE
LA855           STA         CLSTSN          ; SAVE THE LAST VALUE SENT TO THE D/A CONVERTER
                ASLB                        ;  SHIFT MASK BIT LEFT
                BCC         LA82E           ; DONE WHEN MASK BIT IS SHIFTED INTO CARRY FLAG
                PULS        A,PC            ; RESTORE OUTPUT CHARACTER AND RETURN
; THIS IS A LOOK-UP TABLE OF SINE VALUES FOR THE TAPE DECK FSK
; (BIT 1 IS USED TO KEEP THE SERIAL OUTPUT MARKING)
LA85C           FCB         $82,$92,$AA,$BA,$CA,$DA
                FCB         $EA,$F2,$FA,$FA,$FA,$F2
                FCB         $EA,$DA,$CA,$BA,$AA,$92
                FCB         $7A,$6A,$52,$42,$32,$22
                FCB         $12,$0A,$02,$02,$02,$0A
                FCB         $12,$22,$32,$42,$52,$6A
; SET
SET             BSR         LA8C1           ; GET ABSOLUTE SCREEN POSITION OF GRAPHICS BLOCK
                PSHS        X               ; SAVE CHARACTER LOCATION
                JSR         >LB738          ; SYNTAX CHECK FOR COMMA - RETURN EXPR VALUE IN ACCB
                PULS        X               ; REGET CHARACTER LOCATION
                CMPB        #8              ; NINE ALLOWABLE COLORS
                BHI         LA8D5           ; ILLEGAL COLOR - ILLEGAL FUNCTION CALL
                DECB                        ;  CHANGE COLOR NUMBERS FROM 0-8 TO (-1 TO 7)
                BMI         LA895           ; BRANCH IF SET (X,Y,0)
                LDA         #$10            ; $10 OFFSET BETWEEN DIFFERENT COLORS
                MUL                         ;  MULT BY COLOR FOR TOTAL OFFSET
                BRA         LA89D           ; GO SAVE THE COLOR
LA895           LDB         ,X              ; GET CURRENT CHAR FROM SCREEN
                BPL         LA89C           ; BRANCH IF NOT GRAPHIC
                ANDB        #$70            ; SAVE ONLY THE COLOR INFO
                FCB         SKP1            ; SKIP THE NEXT BYTE
LA89C           CLRB                        ;  RESET ASCII BLOCK TO ZERO COLOR
LA89D           PSHS        B               ; SAVE COLOR INFO
                BSR         LA90D           ; SYNTAX CHECK FOR )
                LDA         ,X              ; GET CURRENT CHARACTER FROM SCREEN
                BMI         LA8A6           ; BRANCH IF GRAPHIC
                CLRA                        ;  RESET ASCII CHARACTER TO ALL PIXELS OFF
LA8A6           ANDA        #$0F            ; SAVE ONLY PIXEL ON/OFF INFO
                ORA         GRBLOK          ; OR WITH WHICH PIXEL TO TURN ON
                ORA         ,S+             ; OR IN THE COLOR
LA8AC           ORA         #$80            ; FORCE GRAPHIC
                STA         ,X              ; DISPLAY IT ON THE SCREEN
                RTS
; RESET
RESET           BSR         LA8C1           ; GET ABSOLUTE SCREEN ADDRESS OF THIS CHARACTER
                BSR         LA90D           ; SYNTAX CHECK FOR ")"
                CLRA                        ;  ACCA=ZERO GRAPHIC BLOCK - FOR USE IN CASE YOURE
; TRYING TO RESET A NON GRAPHIC BLOCK
                LDB         ,X              ; GET CURRENT CHAR FROM SCREEN
                BPL         LA8AC           ; BRANCH IF NON-GRAPHIC
                COM         GRBLOK          ; INVERT PIXEL ON/OFF MASK
                ANDB        GRBLOK          ; AND IT WITH CURRENT ON/OFF DATA
                STB         ,X              ; DISPLAY IT
                RTS
; THIS ROUTINE WILL CHECK SYNTAX AND CHECK FOR LEGAL VALUES
; OF SET,RESET & POINT HORIZONTAL AND VERTICAL PARAMETERS
; AND RETURN THEIR ABSOLUTE SCREEN ADDRESS IN THE X REGISTER
; WHICH OF THE FOUR PIXELS OF THE GRAPHIC BLOCK SELECTED
; IS RETURNED IN GRBLOK.
LA8C1           JSR         >LB26A          ; SYNTAX CHECK FOR "("
LA8C4           JSR         >RVEC21         ; HOOK INTO RAM
                JSR         >LB70B          ; EVALUATE EXPRESSION - RETURN VALUE IN ACCB
                CMPB        #63             ; ONLY 64 HORIZONTAL GRAPHIC BLOCKS
                BHI         LA8D5           ; ILLEGAL FUNCTION CALL
                PSHS        B               ; SAVE HOR COORD
                JSR         >LB738          ; SYNTAX CHECK FOR COMMA AND EVALUATE EXPR
                CMPB        #31             ; ONLY 32 VERTICAL BLOCKS
LA8D5           BHI         LA948           ; ILLEGAL FUNCTION CALL
                PSHS        B               ; SAVE VERT COORD
                LSRB                        ;  DIVIDE BY TWO BECAUSE THERE ARE 2 GRAPHIC PIXELS/HOR
; CHARACTER POSITION (BYTE)
                LDA         #32             ; 32 BYTES/ROW
                MUL                         ;  GET ROW OFFSET OF CHAR POSITION
                LDX         #VIDRAM         ; SCREEN BUFFER ADDRESS
                LEAX        D,X             ; ADD ROW OFFSET TO SCREEN BUFFER ADDRESS
                LDB         1,S             ; GET HOR COORD
                LSRB                        ;  2 VERTICAL PIXELS/CHARACTER POSITION
                ABX                         ;  ADD VERTICAL OFFSET TO CHARACTER ADDRESS
                PULS        A,B             ; GET VER COORD TO ACCA, HOR COORD TO ACCB
                ANDA        #1              ; KEEP ONLY LSB OF VER COORD
                RORB                        ;  LSB OF HOR COORD TO CARRY FLAG
                ROLA                        ;  LSB OF HOR TO BIT 0 OF ACCA
                LDB         #$10            ; MAKE A BIT MASK - TURN ON BIT 4
LA8EE           LSRB                        ;  SHIFT IT RIGHT ONCE
                DECA                        ;  SHIFTED IT ENOUGH?
                BPL         LA8EE           ; NO
                STB         GRBLOK          ; ACCB=8 FOR UPPER LEFT PIXEL, =4 FOR UPPER RIGHT
; PIXEL =2 FOR LOWER LEFT, =1 FOR LOWER RIGHT
                RTS
; POINT
POINT           BSR         LA8C4           ; EVALUATE EXPRESSION
                LDB         #$FF            ; INITIAL VALUE OF ON/OFF FLAG = OFF (FALSE)
                LDA         ,X              ; GET CURRENT GRAPHIC CHARACTER
                BPL         LA90A           ; BRANCH IF NON-GRAPHIC (ALWAYS FALSE)
                ANDA        GRBLOK          ; AND CURR CHAR WITH THE PIXEL IN QUESTION
                BEQ         LA909           ; BRANCH IF THE ELEMENT IS OFF
                LDB         ,X              ; GET CURRENT CHARACTER
                LSRB                        ;  SHIFT RIGHT
                LSRB                        ;  SHIFT RIGHT
                LSRB                        ;  SHIFT RIGHT
                LSRB                        ;  SHIFT RIGHT - NOW THE HIGH NIBBLE IS IN THE LOW NIBBLE
                ANDB        #7              ; KEEP ONLY THE COLOR INFO
LA909           INCB                        ;  ACCB=0 FOR NO COLOR, =1 T0 8 OTHERWISE
LA90A           JSR         >LA5E8          ; CONVERT ACCB TO FLOATING POINT
LA90D           JMP         >LB267          ; SYNTAX CHECK FOR )
; CLS
CLS             JSR         >RVEC22         ; HOOK INTO RAM
LA913           BEQ         CLRSCRN         ; BRANCH IF NO ARGUMENT
                JSR         >LB70B          ; CALCULATE ARGUMENT, RETURN VALUE IN ACCB
                CMPB        #8              ; VALID ARGUMENT?
                BHI         LA937           ; IF ARGUMENT >8, GO PRINT MICROSOFT
                TSTB                        ;  SET FLAGS
                BEQ         LA925           ; COLOR 0
                DECB                        ;  ACCB NOW CONTAINS 0-7
                LDA         #$10            ; EACH GRAPHIC BLOCK SEPARATED BY $10 FROM ONE ANOTHER
                MUL                         ;  ACCB CONTAINS ONE OF 8 OFFSETS
                ORB         #$0F            ; BITS 0-3 SET FOR SOLID COLOR GRAPHIC BLOCK
LA925           ORB         #$80            ; BIT 7 SET FOR GRAPHICS
                FCB         SKP2            ; SKIP TWO BYTES
; CLEAR SCREEN
CLRSCRN         LDB         #$60            ; BLANK
                LDX         #VIDRAM         ; GET ADDR OF START OF SCREEN BUFFER
LA92D           STX         CURPOS          ; SAVE IT IN CURPOS
LA92F           STB         ,X+             ; FILL SCREEN WITH CONTENTS OF ACCB
                CMPX        #VIDRAM+511     ; END OF SCREEN?
                BLS         LA92F           ; NO
                RTS
LA937           BSR         CLRSCRN         ; CLEAR SCREEN
                LDX         #LA166-1
                JMP         >LB99C          ; PRINT MICROSOFT
LA93F           JSR         >LB26D          ; SYNTAX CHECK FOR A COMMA
LA942           JSR         >LB70B          ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                TSTB                        ;  SET FLAGS
                BNE         LA984           ; RETURN IF NON ZERO
LA948           JMP         >LB44A          ; ILLEGAL FUNCTION CALL IF ZERO
; SOUND
SOUND           BSR         LA942           ; EVALUATE EXPRESSION (FREQUENCY)
                STB         SNDTON          ; SAVE IT
                BSR         LA93F           ; EVALUATE EXPRESSION (SOUND LENGTH)
LA951           LDA         #4              ; CONSTANT FACTOR
                MUL                         ;  EXPAND LENGTH EXPRESSION
                STD         SNDDUR          ; SAVE LENGTH OF SOUND
                LDA         PIA0+3          ; GET CONTROL REGISTER OF PIA0, PORT B
                ORA         #1
                STA         PIA0+3          ; ENABLE 60 HZ INTERRUPT (PIA0 IRQ)
                CLR         ARYDIS          ; CLEAR THE ARRAY DISABLE FLAG - FOR NO APPARENT REASON
                BSR         LA9A2           ; CONNECT D/A SOUND INPUT TO OUTPUT OF SOUND MUX
                BSR         LA976           ; TURN ON AUDIO - ENABLE SOUND MUX
LA964           BSR         LA985           ; STORE 2.5 VOLTS TO D/A AND WAIT
                LDA         #$FE            ; DATA TO MAKE D/A OUT = 5 VOLTS
                BSR         LA987           ; STORE IT TO D/A AND WAIT
                BSR         LA985           ; STORE 2.5 VOLTS TO D/A AND WAIT
                LDA         #2              ; DATA TO MAKE D/A OUT = 0 VOLTS
                BSR         LA987           ; STORE IT TO D/A AND WAIT
                LDX         SNDDUR          ; IS SNDDUR = 0? - THE IRQ INTERRUPT SERVICING
; ROUTINE WILL DECREMENT SNDDUR
                BNE         LA964           ; NOT DONE YET
; THESE ROUTINES WILL ENABLE/DISABLE THE ANALOG MUX
LA974           CLRA                        ;  BIT 3 OF ACCA = 0, DISABLE ANALOG MUX
                FCB         SKP2            ; SKIP TWO BYTES
LA976           LDA         #8              ; BIT 3 OF ACCA = 1, ENABLE ANALOG MUX
                STA         ,-S             ; SAVE ACCA ON STACK
                LDA         PIA1+3          ; GET CONTROL REGISTER OF PIA1, PORT B
                ANDA        #$F7            ; RESET BIT 3
                ORA         ,S+             ; OR IN BIT 3 OF ACCA (SAVED ON STACK)
                STA         PIA1+3          ; SET/RESET CB2 OF U4
LA984           RTS
LA985           LDA         #$7E            ; DATA VALUE TO MAKE D/A OUTPUT = 2.5 VOLTS
LA987           STA         DA              ; STORE IT IN D/A
                LDA         SNDTON          ; GET FREQUENCY
LA98C           INCA                        ;  INCREMENT IT
                BNE         LA98C           ; LOOP UNTIL DONE
                RTS
; AUDIO
AUDIO           TFR         A,B             ; SAVE ON/OFF TOKEN IN ACCB
                JSR         GETNCH          ; MOVE BASIC POINTER TO NEXT CHARACTER
                CMPB        #$AA            ; OFF TOKEN?
                BEQ         LA974           ; YES - TURN OFF ANALOG MUX
                SUBB        #$88            ; ON TOKEN
                JSR         >LA5C9          ; SYNTAX ERROR IF NOT OFF OR ON
                INCB                        ;  NOW ACCB = 1
                BSR         LA9A2           ; ROUTE CASSETTE TO SOUND MULTIPLEXER
                BRA         LA976           ; ENABLE SOUND MULTIPLEXER
; THIS ROUTINE WILL TRANSFER BIT 0 OF ACCB TO SEL 1 OF
; THE ANALOG MULTIPLEXER AND BIT 1 OF ACCB TO SEL 2.
LA9A2           LDU         #PIA0+1         ; POINT U TO PIA0 CONTROL REG
                BSR         LA9A7           ; PROGRAM 1ST CONTROL REGISTER
LA9A7           LDA         ,U              ; GET PIA CONTROL REGISTER
                ANDA        #$F7            ; RESET CA2 (CB2) OUTPUT BIT
                ASRB                        ;  SHIFT ACCB BIT 0 TO CARRY FLAG
                BCC         LA9B0           ; BRANCH IF CARRY = ZERO
                ORA         #$08            ; FORCE BIT 3=1; SET CA2(CB2)
LA9B0           STA         ,U++            ; PUT IT BACK IN THE PIA CONTROL REGISTER
                RTS
; IRQ SERVICE
BIRQSV          LDA         PIA0+3          ; CHECK FOR 60HZ INTERRUPT
                BPL         LA9C5           ; RETURN IF 63.5 MICROSECOND INTERRUPT
                LDA         PIA0+2          ; RESET PIA0, PORT B INTERRUPT FLAG
LA9BB           LDX         >SNDDUR         ; GET INTERRUPT TIMER (SOUND COMMAND)
                BEQ         LA9C5           ; RETURN IF TIMER = 0
                LEAX        -1,X            ; DECREMENT TIMER IF NOT = 0
                STX         >SNDDUR         ; SAVE NEW TIMER VALUE
LA9C5           RTI         RETURN          ; FROM INTERRUPT
; JOYSTK
JOYSTK          JSR         >LB70E          ; EVALUATE JOYSTICK ARGUMENT
                CMPB        #3              ; TWO JOYSTICKS MAXIMUM (HOR & VER FOR EACH)
                LBHI        LB44A           ; ILLEGAL FUNCTION CALL IF >3
                TSTB                        ;  SET FLAGS
                BNE         LA9D4           ; GET NEW DATA ONLY IF JOYSTK(0)
                BSR         GETJOY          ; GET NEW DATA FOR ALL JOYSTICKS
LA9D4           LDX         #POTVAL         ; POINT X TO JOYSTICK DATA BUFFER
                LDB         FPA0+3          ; WHICH JOYSTICK DID YOU WANT?
                LDB         B,X             ; PUT ITS DATA INTO ACCB
                JMP         >LB4F3          ; CONVERT ACCB INTO FLOATING POINT NUMBER

; JOYSTK DATA AT:
; $15A $15B $15C $15D
; LEFT LEFT RIGHT RIGHT
; VERT HORIZ VERT HORIZ
; THIS IS A 6 BIT SOFTWARE A/D CONVERSION ROUTINE
GETJOY          BSR         LA974           ; TURN OFF AUDIO
                LDX         #POTVAL+4       ; POINT X TO JOYSTICK DATA BUFFER
                LDB         #3              ; GET FOUR SETS OF DATA (4 JOYSTICKS)
LA9E5           LDA         #10             ; 10 TRIES TO GET STABLE READING
                STD         ,--S            ; STORE JOYSTICK NUMBER AND TRY NUMBER ON THE STACK
                BSR         LA9A2           ; SET THE SELECT INPUTS ON ANALOG MULTIPLEXER
LA9EB           LDD         #$4080          ; ACCA IS A SHIFT COUNTER OF HOW MANY BITS TO CONVERT
; AND WIlL BE $40 (6 BITS) FOR THE COLOR
; COMPUTER. ACCB CONTAINS A VALUE EQUAL TO 1/2
; THE CURRENT TRIAL DIFFERENCE. INITIALLY =$80 (2.5 VOLTS).
LA9EE           STA         ,-S             ; TEMP STORE SHIFT COUNTER ON STACK
                ORB         #2              ; KEEP RS 232 SERIAL OUT MARKING
                STB         DA              ; STORE IN D/A CONVERTER
                EORB        #2              ; PUT R5232 OUTPUT BIT BACK TO ZERO
                LDA         PIA0            ; HIGH BIT IS FROM COMPARATOR
                BMI         LA9FF           ; BRANCH IF COMPARATOR OUTPUT IS HIGH
                SUBB        ,S              ; SUBTRACT 1/2 THE CURRENT TRIAL DIFFERENCE
                FCB         SKP2            ; SKIP NEXT TWO BYTES
LA9FF           ADDB        ,S              ; ADD 1/2 OF THE CURRENT TRIAL DIFFERENCE
                LDA         ,S+             ; PULL SHIFT COUNTER OFF THE STACK
                LSRA                        ;  SHIFT IT RIGHT ONCE
                CMPA        #1              ; HAVE ALL THE SHIFTS BEEN DONE?
                BNE         LA9EE           ; NO
                LSRB                        ;  YES - THE DATA IS IN THE TOP 6 BYTES OF ACCB
                LSRB                        ;  PUT IT INTO THE BOTTOM SIX
                CMPB        -1,X            ; IS THIS VALUE EQUAL TO THE LAST TRY?
                BEQ         LAA12           ; YES - GO SAVE THE VALUE
                DEC         ,S              ; NO-DECREMENT TRIES COUNTER
                BNE         LA9EB           ; BRANCH IF YOU HAVENT TRIED 10 TIMES
; IF YOU FALL THROUGH HERE YOU HAVE TRIED TO GET THE SAME READING
; 10 TIMES AND NEVER GOTTEN A MATCH. AS A RESULT YOU JUST FALL
; THROUGH AND USE THE LAST VALUE READ IN.
LAA12           STB         ,-X             ; SAVE THE DIGITIZED VALUE
                LDD         ,S++            ; GET THE NUMBER OF THE JOYSTICK JUST DONE
                DECB                        ;  DECR JOYSTK NUMBER
                BPL         LA9E5           ; BRANCH IF THE LAST ONE DONE WASNT NUMBER 0
                RTS

; SET CARRY IF NUMERIC - RETURN WITH
; ZERO FLAG SET IF ACCA = 0 OR 3A(:) - END
; OF BASIC LINE OR SUB LINE
BROMHK          CMPA        #'9+1           ; IS THIS CHARACTER >=(ASCII 9)+1?
                BHS         LAA28           ; BRANCH IF > 9; Z SET IF = COLON
                CMPA        #SPACE          ; SPACE?
                BNE         LAA24           ; NO - SET CARRY IF NUMERIC
                JMP         GETNCH          ; IF SPACE, GET NECT CHAR (IGNORE SPACES)
LAA24           SUBA        #'0             ; SET CARRY IF
                SUBA        #-'0            ; CHARACTER > ASCII 0
LAA28           RTS

; DISPATCH TABLE FOR SECONDARY FUNCTIONS
; TOKENS ARE PRECEEDED BY $FF TOKEN #
LAA29           FDB         SGN             ; SGN 80
                FDB         INT             ; INT 81
                FDB         ABS             ; ABS 82
                FDB         $0112           ; USR 83
                FDB         RND             ; RND 84
                FDB         SIN             ; SIN 85
                FDB         PEEK            ; PEEK 86
                FDB         LEN             ; LEN 87
                FDB         STR             ; STR$ 88
                FDB         VAL             ; VAL 89
                FDB         ASC             ; ASC 8A
                FDB         CHR             ; CHR$ 8B
                FDB         EOF             ; EOF 8C
                FDB         JOYSTK          ; JOYSTK 8D
                FDB         LEFT            ; LEFT$ 8E
                FDB         RIGHT           ; RIGHT$ 8F
                FDB         MID             ; MID$ 90
                FDB         POINT           ; POINT 91
                FDB         INKEY           ; INKEY$ 92
                FDB         MEM             ; MEM 93

; THIS TABLE CONTAINS PRECEDENCES AND DISPATCH ADDRESSES FOR ARITHMETIC
; AND LOGICAL OPERATORS - THE NEGATION OPERATORS DO NOT ACT ON TWO OPERANDS
; S0 THEY ARE NOT LISTED IN THIS TABLE. THEY ARE TREATED SEPARATELY IN THE
; EXPRESSION EVALUATION ROUTINE. THEY ARE:
; UNARY NEGATION (-), PRECEDENCE &7D AND LOGICAL NEGATION (NOT), PRECEDENCE $5A
; THE RELATIONAL OPERATORS < > = ARE ALSO NOT LISTED, PRECEDENCE $64.
; A PRECEDENCE VALUE OF ZERO INDICATES END OF EXPRESSION OR PARENTHESES

LAA51           FCB         $79
                FDB         LB9C5           ; +
                FCB         $79
                FDB         LB9BC           ; -
                FCB         $7B
                FDB         $BACC
                FCB         $7B
                FDB         $BB91           ; /
                FCB         $7F
                FDB         $011D           ; EXPONENTIATION
                FCB         $50
                FDB         $B2D5           ; AND
                FCB         $46
                FDB         LB2D4           ; OR

; THIS IS THE RESERVED WORD TABLE
; TOKEN #
LAA66           FCS         'FOR'           ; 80
                FCS         'GO'            ; 81
                FCS         'REM'           ; 82
                FCB         ''+$80          ; 83
                FCS         'ELSE'          ; 84
                FCS         'IF'            ; 85
                FCS         'DATA'          ; 86
                FCS         'PRINT'         ; 87
                FCS         'ON'            ; 88
                FCS         'INPUT'         ; 89
                FCS         'END'           ; 8A
                FCS         'NEXT'          ; 8B
                FCS         'DIM'           ; 8C
                FCS         'READ'          ; 8D
                FCS         'RUN'           ; 8E
                FCS         'RESTORE'       ; 8F
                FCS         'RETURN'        ; 90
                FCS         'STOP'          ; 91
                FCS         'POKE'          ; 92
                FCS         'CONT'          ; 93
                FCS         'LIST'          ; 94
                FCS         'CLEAR'         ; 95
                FCS         'NEW'           ; 96
                FCS         'CLOAD'         ; 97
                FCS         'CSAVE'         ; 98
                FCS         'OPEN'          ; 99
                FCS         'CLOSE'         ; 9A
                FCS         'LLIST'         ; 9B
                FCS         'SET'           ; 9C
                FCS         'RESET'         ; 9D
                FCS         'CLS'           ; 9E
                FCS         'MOTOR'         ; 9F
                FCS         'SOUND'         ; A0
                FCS         'AUDIO'         ; A1
                FCS         'EXEC'          ; A2
                FCS         'SKIPF'         ; A3
                FCS         'TAB('          ; A4
                FCS         'TO'            ; A5
                FCS         'SUB'           ; A6
                FCS         'THEN'          ; A7
                FCS         'NOT'           ; A8
                FCS         'STEP'          ; A9
                FCS         'OFF'           ; AA
                FCS         '+'             ; AB
                FCS         '-'             ; AC
                FCS         '*'             ; AD
                FCS         '/'             ; AE
                FCS         '^'             ; AF
                FCS         'AND'           ; B0
                FCS         'OR'            ; B1
                FCS         '>'             ; B2
                FCS         '='             ; B3
                FCS         '<'             ; B4

; TOKENS FOR THE SECONDARY FUNCTIONS ARE PRECEEDED BY $FF
; TOKEN #
LAB1A           FCS         'SGN'           ; 80
                FCS         'INT'           ; 81
                FCS         'ABS'           ; 82
                FCS         'USR'           ; 83
                FCS         'RND'           ; 84
                FCS         'SIN'           ; 85
                FCS         'PEEK'          ; 86
                FCS         'LEN'           ; 87
                FCS         'STR$'          ; 88
                FCS         'VAL'           ; 89
                FCS         'ASC'           ; 8A
                FCS         'CHR$'          ; 8B
                FCS         'EOF'           ; 8C
                FCS         'JOYSTK'        ; 8D
                FCS         'LEFT$'         ; 8E
                FCS         'RIGHT$'        ; 8F
                FCS         'MID$'          ; 90
                FCS         'POINT'         ; 91
                FCS         'INKEY$'        ; 92
                FCS         'MEM'           ; 93

; DISPATCH TABLE FOR COMMANDS TOKEN #
LAB67           FDB         FOR             ; FOR 80
                FDB         GO              ; GO 81
                FDB         REM             ; REM 82
                FDB         REM             ; REM 83
                FDB         REM             ; ELSE 84
                FDB         IFTOK           ; IF 85
                FDB         DATA            ; DATA 86
                FDB         PRINT           ; PRINT 87
                FDB         ON              ; ON 88
                FDB         INPUT           ; INPUT 89
                FDB         ENDTOK          ; END 8A
                FDB         NEXT            ; NEXT 8B
                FDB         DIM             ; DIM 8C
                FDB         READ            ; READ 8D
                FDB         RUN             ; RUN 8E
                FDB         RESTOR          ; RESTORE 8F
                FDB         RETURN          ; RETURN 90
                FDB         STOP            ; STOP 91
                FDB         POKE            ; POKE 92
                FDB         CONT            ; CONTINUE93
                FDB         LIST            ; LIST 94
                FDB         CLEAR           ; CLEAR 95
                FDB         NEW             ; NEW 96
                FDB         CLOAD           ; CLOAD 97
                FDB         CSAVE           ; CSAVE 98
                FDB         OPEN            ; OPEN 99
                FDB         CLOSE           ; CLOSE 9A
                FDB         LLIST           ; LLIST 9B
                FDB         SET             ; SET 9C
                FDB         RESET           ; RESET 9D
                FDB         CLS             ; CLS 9E
                FDB         MOTOR           ; MOTOR 9F
                FDB         SOUND           ; SOUND A0
                FDB         AUDIO           ; AUDIO A1
                FDB         EXEC            ; EXEC A2
                FDB         SKIPF           ; SKIPF A3

; ERROR MESSAGES AND THEIR NUMBERS AS USED INTERNALLY
LABAF           FCC         'NF'            ; 0 NEXT WITHOUT FOR
                FCC         'SN'            ; 1 SYNTAX ERROR
                FCC         'RG'            ; 2 RETURN WITHOUT GOSUB
                FCC         'OD'            ; 3 OUT OF DATA
                FCC         'FC'            ; 4 ILLEGAL FUNCTION CALL
                FCC         'OV'            ; 5 OVERFLOW
                FCC         'OM'            ; 6 OUT OF MEMORY
                FCC         'UL'            ; 7 UNDEFINED LINE NUMBER
                FCC         'BS'            ; 8 BAD SUBSCRIPT
                FCC         'DD'            ; 9 REDIMENSIONED ARRAY
                FCC         '/0'            ; 10 DIVISION BY ZERO
                FCC         'ID'            ; 11 ILLEGAL DIRECT STATEMENT
                FCC         'TM'            ; 12 TYPE MISMATCH
                FCC         'OS'            ; 13 OUT OF STRING SPACE
                FCC         'LS'            ; 14 STRING TOO LONG
                FCC         'ST'            ; 15 STRING FORMULA TOO COMPLEX
                FCC         'CN'            ; 16 CAN'T CONTINUE
                FCC         'FD'            ; 17 BAD FILE DATA
                FCC         'AO'            ; 18 FILE ALREADY OPEN
                FCC         'DN'            ; 19 DEVICE NUMBER ERROR
                FCC         'IO'            ; 20 I/O ERROR
                FCC         'FM'            ; 21 BAD FILE MODE
                FCC         'NO'            ; 22 FILE NOT OPEN
                FCC         'IE'            ; 23 INPUT PAST END OF FILE
                FCC         'DS'            ; 24 DIRECT STATEMENT IN FILE
LABE1           FCC         ' ERROR'
                FCB         $00
LABE8           FCC         ' IN '
                FCB         $00
LABED           FCB         CR
LABEE           FCC         'OK'
                FCB         CR,$00
LABF2           FCB         CR
                FCC         'BREAK'
                FCB         $00
; SEARCH THE STACK FOR GOSUB/RETURN OR FOR/NEXT DATA.
; THE FOR/NEXT INDEX VARIABLE DESCRIPTOR ADDRESS BEING
; SOUGHT IS STORED IN VARDES. EACH BLOCK OF FOR/NEXT DATA IS 18
; BYTES WITH A $80 LEADER BYTE AND THE GOSUB/RETURN DATA IS 5 BYTES
; WITH AN $A6 LEADER BYTE. THE FIRST NON "FOR/NEXT" DATA
; IS CONSIDERED GOSUB/RETURN
LABF9           LEAX        4,S             ; POINT X TO 3RD ADDRESS ON STACK - IGNORE THE
; FIRST TWO RETURN ADDRESSES ON THE STACK
LABFB           LDB         #18             ; 18 BYTES SAVED ON STACK FOR EACH FOR LOOP
                STX         TEMPTR          ; SAVE POINTER
                LDA         ,X              ; GET 1ST BYTE
                SUBA        #$80            ; CHECK FOR TYPE OF STACK JUMP FOUND
                BNE         LAC1A           ; BRANCH IF NOT FOR/NEXT
                LDX         1,X             ; GET INDEX VARIABLE DESCRIPTOR
                STX         TMPTR1          ; POINTER AND SAVE IT IN TMPTR1
                LDX         VARDES          ; GET INDEX VARIABLE BEING SEARCHED FOR
                BEQ         LAC16           ; BRANCH IF DEFAULT INDEX VARIABLE - USE THE
; FIRST FOR/NEXT DATA FOUND ON STACK
; IF NO INDEX VARIABLE AFTER NEXT
                CMPX        TMPTR1          ; DOES THE STACK INDEX MATCH THE ONE
; BEING SEARCHED FOR?
                BEQ         LAC1A           ; YES
                LDX         TEMPTR          ; RESTORE INITIAL POINTER, ADD
                ABX                         ;  18 TO IT AND LOOK FOR
                BRA         LABFB           ; NEXT BLOCK OF DATA
LAC16           LDX         TMPTR1          ; GET 1ST INDEX VARIABLE FOUND AND
                STX         VARDES          ; SAVE AS NEXT INDEX
LAC1A           LDX         TEMPTR          ; POINT X TO START OF FOR/NEXT DATA
                TSTA                        ;  SET ZERO FLAG IF FOR/NEXT DATA
                RTS
; CHECK FOR MEMORY SPACE FOR NEW TOP OF
; ARRAYS AND MOVE ARRAYS TO NEW LOCATION
LAC1E           BSR         LAC37           ; ACCD = NEW BOTTOM OF FREE RAM - IS THERE
; ROOM FOR THE STACK?
; MOVE BYTES FROM V43(X) TO V41(U) UNTIL (X) = V47 AND
; SAVE FINAL VALUE OF U IN V45
LAC20           LDU         V41             ; POINT U TO DESTINATION ADDRESS (V41)
                LEAU        1,U             ; ADD ONE TO U - COMPENSATE FOR FIRST PSHU
                LDX         V43             ; POINT X TO SOURCE ADDRESS (V43)
                LEAX        1,X             ; ADD ONE - COMPENSATE FOR FIRST LDA ,X
LAC28           LDA         ,-X             ; GRAB A BYTE FROM SOURCE
                PSHU        A               ; MOVE IT TO DESTINATION
                CMPX        V47             ; DONE?
                BNE         LAC28           ; NO - KEEP MOVING BYTES
                STU         V45             ; SAVE FINAL DESTINATION ADDRESS
LAC32           RTS
; CHECK TO SEE IF THERE IS ROOM TO STORE 2*ACCB
; BYTES IN FREE RAM - OM ERROR IF NOT
LAC33           CLRA                        ;  ACCD CONTAINS NUMBER OF EXTRA
                ASLB                        ;  BYTES TO PUT ON STACK
                ADDD        ARYEND          ; END OF PROGRAM AND VARIABLES
LAC37           ADDD        #STKBUF         ; ADD STACK BUFFER - ROOM FOR STACK?
                BCS         LAC44           ; BRANCH IF GREATER THAN $FFFF
                STS         BOTSTK          ; CURRENT NEW BOTTOM OF STACK STACK POINTER
                CMPD        BOTSTK          ; ARE WE GOING TO BE BELOW STACK?
                BCS         LAC32           ; YES - NO ERROR
LAC44           LDB         #6*2            ; OUT OF MEMORY ERROR
; ERROR SERVICING ROUTINE
LAC46           JSR         >RVEC16         ; HOOK INTO RAM
LAC49           JSR         >RVEC17         ; HOOK INTO RAM
                JSR         >LA7E9          ; TURN OFF CASSETTE
                JSR         >LA974          ; DISABLE ANA MUX
                JSR         >LAD33          ; RESET STACK, STRING STACK, CONTINUE POINTER
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         >LB95C          ; SEND A CR TO SCREEN
                JSR         >LB9AF          ; SEND A ? TO SCREEN
                LDX         #LABAF          ; POINT TO ERROR TABLE
LAC60           ABX                         ;  ADD MESSAGE NUMBER OFFSET
                BSR         LACA0           ; GET TWO CHARACTERS FROM X AND
                BSR         LACA0           ; SEND TO CONSOLE OUT (SCREEN)
LAC65           LDX         #LABE1-1        ; POINT TO "ERROR" MESSAGE
LAC68           JSR         >LB99C          ; PRINT MESSAGE POINTED TO BY X
                LDA         CURLIN          ; GET CURRENT LINE NUMBER (CURL IN)
                INCA                        ;  TEST FOR DIRECT MODE
                BEQ         LAC73           ; BRANCH IF DIRECT MODE
                JSR         >LBDC5          ; PRINT IN ****
; THIS IS THE MAIN LOOP OF BASIC WHEN IN DIRECT MODE
LAC73           JSR         >LB95C          ; MOVE CURSOR TO START OF LINE
LAC76           LDX         #LABEE-1        ; POINT X TO OK, CR MESSAGE
                JSR         >LB99C          ; PRINT OK, CR
LAC7C           JSR         >LA390          ; GO GET AN INPUT LINE
                LDU         #$FFFF          ; THE LINE NUMBER FOR DIRECT MODE IS $FFFF
                STU         CURLIN          ; SAVE IT IN CURLIN
                BCS         LAC7C           ; BRANCH IF LINE INPUT TERMINATED BY BREAK
                TST         CINBFL          ; CHECK CONSOLE INPUT BUFFER STATUS
                LBNE        LA4BF           ; BRANCH IF BUFFER EMPTY - CLOSE FILE IF EMPTY
                STX         CHARAD          ; SAVE (X) AS CURRENT INPUT POINTER - THIS WILL
; ENABLE THE LIVE KEYBOARD (DIRECT) MODE. THE
; LINE JUST ENTERED WILL BE INTERPRETED
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                BEQ         LAC7C           ; NO LINE INPUT - GET ANOTHER LINE
                BCS         LACA5           ; BRANCH IF NUMER1C - THERE WAS A LINE NUMBER BEFORE
; THE STATEMENT ENTERED, SO THIS STATEMENT
; WILL BE MERGED INTO THE BASIC PROGRAM
                LDB         #2*24           ; DIRECT STATEMENT IN FILE ERROR
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                BNE         LAC46           ; ISSUE DS ERROR IF DEVNUM <> 0
                JSR         >LB821          ; GO CRUNCH LINE
LAC9D           JMP         >LADC0          ; GO EXECUTE THE STATEMENT (LIVE KEYBOARD)

LACA0           LDA         ,X+             ; GET A CHARACTER
                JMP         >LB9B1          ; SEND TO CONSOLE OUT
; TAKE A LINE FROM THE LINE INPUT BUFFER
; AND INSERT IT INTO THE BASIC PROGRAM
LACA5           JSR         >LAF67          ; CONVERT LINE NUMBER TO BINARY
LACA8           LDX         BINVAL          ; GET CONVERTED LINE NUMBER
                STX         LINHDR          ; STORE IT IN LINE INPUT HEADER
                JSR         >LB821          ; GO CRUNCH THE LINE
                STB         TMPLOC          ; SAVE LINE LENGTH
                BSR         LAD01           ; FIND OUT WHERE TO INSERT LINE
                BCS         LACC8           ; BRANCH IF LINE NUMBER DOES NOT ALREADY EXIST
                LDD         V47             ; GET ABSOLUTE ADDRESS OF LINE NUMBER
                SUBD        ,X              ; SUBTRACT ADDRESS OF NEXT LINE NUMBER
                ADDD        VARTAB          ; ADD TO CURRENT END OF PROGRAM - THIS WILL REMOVE
                STD         VARTAB          ; THE LENGTH OF THIS LINE NUMBER FROM THE PROGRAM
                LDU         ,X              ; POINT U TO ADDRESS OF NEXT LINE NUMBER
; DELETE OLD LINE FROM BASIC PROGRAM
LACC0           PULU        A               ; GET A BYTE FROM WHATS LEFT OF PROGRAM
                STA         ,X+             ; MOVE IT DOWN
                CMPX        VARTAB          ; COMPARE TO END OF BASIC PROGRAM
                BNE         LACC0           ; BRANCH IF NOT AT END
LACC8           LDA         LINBUF          ; CHECK TO SEE IF THERE IS A LINE IN
                BEQ         LACE9           ; THE BUFFER AND BRANCH IF NONE
                LDD         VARTAB          ; SAVE CURRENT END OF
                STD         V43             ; PROGRAM IN V43
                ADDB        TMPLOC          ; ADD LENGTH OF CRUNCHED LINE,
                ADCA        #0              ; PROPOGATE CARRY AND SAVE NEW END
                STD         V41             ; OF PROGRAM IN V41
                JSR         >LAC1E          ; MAKE SURE THERES ENOUGH RAM FOR THIS
; LINE & MAKE A HOLE IN BASIC FOR NEW LINE
                LDU         #LINHDR-2       ; POINT U TO LINE TO BE INSERTED
LACDD           PULU        A               ; GET A BYTE FROM NEW LINE
                STA         ,X+             ; INSERT IT IN PROGRAM
                CMPX        V45             ; COMPARE TO ADDRESS OF END OF INSERTED
                BNE         LACDD           ; LINE AND BRANCH IF NOT DONE
                LDX         V41             ; GET AND SAVE
                STX         VARTAB          ; END OF PROGRAM
LACE9           BSR         LAD21           ; RESET INPUT POINTER, CLEAR VARIABLES, INITIALIZE
                BSR         LACEF           ; ADJUST START OF NEXT LINE ADDRESSES
                BRA         LAC7C           ; REENTER BASICS INPUT LOOP
; COMPUTE THE START OF NEXT LINE ADDRESSES FOR THE BASIC PROGRAM
LACEF           LDX         TXTTAB          ; POINT X TO START OF PROGRAM
LACF1           LDD         ,X              ; GET ADDRESS OF NEXT LINE
                BEQ         LAD16           ; RETURN IF END OF PROGRAM
                LEAU        4,X             ; POINT U TO START OF BASIC TEXT IN LINE
LACF7           LDA         ,U+             ; SKIP THROUGH THE LINE UNTIL A
                BNE         LACF7           ; ZERO (END OF LINE) IS FOUND
                STU         ,X              ; SAVE THE NEW START OF NEXT LINE ADDRESS
                LDX         ,X              ; POINT X TO START OF NEXT LINE
                BRA         LACF1           ; KEEP GOING

; FIND A LINE NUMBER IN THE BASIC PROGRAM
; RETURN WITH CARRY SET IF NO MATCH FOUND
LAD01           LDD         BINVAL          ; GET THE LINE NUMBER TO FIND
                LDX         TXTTAB          ; BEGINNING OF PROGRAM
LAD05           LDU         ,X              ; GET ADDRESS OF NEXT LINE NUMBER
                BEQ         LAD12           ; BRANCH IF END OF PROG
                CMPD        2,X             ; IS IT A MATCH?
                BLS         LAD14           ; CARRY SET IF LOWER; CARRY CLEAR IF MATCH
                LDX         ,X              ; X = ADDRESS OF NEXT LINE
                BRA         LAD05           ; KEEP LOOPING FOR LINE NUMBER
LAD12           ORCC        #1              ; SET CARRY FLAG
LAD14           STX         V47             ; SAVE MATCH LINE NUMBER OR NUMBER OF LINE JUST AFTER WHERE IT SHOULD HAVE BEEN
LAD16           RTS
; NEW
NEW             BNE         LAD14           ; BRANCH IF ARGUMENT GIVEN
LAD19           LDX         TXTTAB          ; GET START OF BASIC
                CLR         ,X+             ; PUT 2 ZERO BYTES THERE - ERASE
                CLR         ,X+             ; THE BASIC PROGRAM
                STX         VARTAB          ; AND THE NEXT ADDRESS IS NOW THE END OF PROGRAM
LAD21           LDX         TXTTAB          ; GET START OF BASIC
                JSR         >LAEBB          ; PUT INPUT POINTER ONE BEFORE START OF BASIC
; ERASE ALL VARIABLES
LAD26           LDX         MEMSIZ          ; RESET START OF STRING VARIABLES
                STX         STRTAB          ; TO TOP OF STRING SPACE
                JSR         >RESTOR         ; RESET DATA POINTER TO START OF BASIC
                LDX         VARTAB          ; GET START OF VARIABLES AND USE IT
                STX         ARYTAB          ; TO RESET START OF ARRAYS
                STX         ARYEND          ; RESET END OF ARRAYS
LAD33           LDX         #STRSTK         ; RESET STRING STACK POINTER TO
                STX         TEMPPT          ; BOTTOM OF STRING STACK
                LDX         ,S              ; GET RETURN ADDRESS OFF STACK
                LDS         FRETOP          ; RESTORE STACK POINTER
                CLR         ,-S             ; PUT A ZERO BYTE ON STACK - TO CLEAR ANY RETURN OF
; FOR/NEXT DATA FROM THE STACK
LAD3F           CLR         OLDPTR          ; RESET CONT ADDRESS SO YOU
                CLR         OLDPTR+1        ; CANT CONTINUE
LAD43           CLR         ARYDIS          ; CLEAR THE ARRAY DISABLE FLAG
                JMP         ,X              ; RETURN TO CALLING ROUTINE - THIS IS NECESSARY
; SINCE THE STACK WAS RESET

; FOR

; THE FOR COMMAND WILL STORE 18 BYTES ON THE STACK FOR
; EACH FOR-NEXT LOOP WHICH IS BEING PROCESSED. THESE
; BYTES ARE DEFINED AS FOLLOWS: 0- $80 (FOR FLAG);
; 1,2=INDEX VARIABLE DESCRIPTOR POINTER; 3-7=FP VALUE OF STEP;
; 8=STEP DIRECTION: $FF IF NEGATIVE; 0 IF ZERO; 1 IF POSITIVE;
; 9-13=FP VALUE OF TO PARAMETER;
; 14,15=CURRENT LINE NUMBER; 16,17=RAM ADDRESS OF THE END
; OF THE LINE CONTAINING THE FOR STATEMENT
FOR             LDA         #$80            ; SAVE THE DISABLE ARRAY FLAG IN VO8
                STA         ARYDIS          ; DO NOT ALLOW THE INDEX VARIABLE TO BE AN ARRAY
                JSR         >LET            ; SET INDEX VARIABLE TO INITIAL VALUE
                JSR         >LABF9          ; SEARCH THE STACK FOR FOR/NEXT DATA
                LEAS        2,S             ; PURGE RETURN ADDRESS OFF OF THE STACK
                BNE         LAD59           ; BRANCH IF INDEX VARIABLE NOT ALREADY BEING USED
                LDX         TEMPTR          ; GET (ADDRESS + 18) OF MATCHED FOR/NEXT DATA
                LEAS        B,X             ; MOVE THE STACK POINTER TO THE BEGINNING OF THE
; MATCHED FOR/NEXT DATA SO THE NEW DATA WILL
; OVERLAY THE OLD DATA. THIS WILL ALSO DESTROY
; ALL OF THE RETURN AND FOR/NEXT DATA BELOW
; THIS POINT ON THE STACK
LAD59           LDB         #$09            ; CHECK FOR ROOM FOR 18 BYTES
                JSR         >LAC33          ; IN FREE RAM
                JSR         >LAEE8          ; GET ADDR OF END OF SUBLINE IN X
                LDD         CURLIN          ; GET CURRENT LINE NUMBER
                PSHS        X,B,A           ; SAVE LINE ADDR AND LINE NUMBER ON STACK
                LDB         #$A5            ; TOKEN FOR TO
                JSR         >LB26F          ; SYNTAX CHECK FOR TO
                JSR         >LB143          ; TM ERROR IF INDEX VARIABLE SET TO STRING
                JSR         >LB141          ; EVALUATE EXPRESSION

                LDB         FP0SGN          ; GET FPA0 MANTISSA SIGN
                ORB         #$7F            ; FORM A MASK TO SAVE DATA BITS OF HIGH ORDER MANTISSA
                ANDB        FPA0            ; PUT THE MANTISSA SIGN IN BIT 7 OF HIGH ORDER MANTISSA
                STB         FPA0            ; SAVE THE PACKED HIGH ORDER MANTISSA
                LDY         #LAD7F          ; LOAD FOLLOWING ADDRESS INTO Y AS A RETURN
                JMP         >LB1EA          ; ADDRESS - PUSH FPA0 ONTO THE STACK
LAD7F           LDX         #LBAC5          ; POINT X TO FLOATING POINT NUMBER 1.0 (DEFAULT STEP VALUE)
                JSR         >LBC14          ; MOVE (X) TO FPA0
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #$A9            ; STEP TOKEN
                BNE         LAD90           ; BRANCH IF NO STEP VALUE
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                JSR         >LB141          ; EVALUATE NUMERIC EXPRESSION
LAD90           JSR         >LBC6D          ; CHECK STATUS OF FPA0
                JSR         >LB1E6          ; SAVE STATUS AND FPA0 ON THE STACK
                LDD         VARDES          ; GET DESCRIPTOR POINTER FOR THE STEP
                PSHS        B,A             ; VARIABLE AND SAVE IT ON THE STACK
                LDA         #$80            ; GET THE FOR FLAG AND
                PSHS        A               ; SAVE IT ON THE STACK

; MAIN COMMAND INTERPRETATION LOOP
LAD9E           JSR         >RVEC20         ; HOOK INTO RAM
                ANDCC       #$AF            ; ENABLE IRQ,FIRQ
                BSR         LADEB           ; CHECK FOR KEYBOARD BREAK
                LDX         CHARAD          ; GET BASICS INPUT POINTER
                STX         TINPTR          ; SAVE IT
                LDA         ,X+             ; GET CURRENT INPUT CHAR & MOVE POINTER
                BEQ         LADB4           ; BRANCH IF END OF LINE
                CMPA        #':             ; CHECK FOR LINE SEPARATOR
                BEQ         LADC0           ; BRANCH IF COLON
LADB1           JMP         >LB277          ; SYNTAX ERROR-IF NOT LINE SEPARATOR
LADB4           LDA         ,X++            ; GET MS BYTE OF ADDRESS OF NEXT BASIC LINE
                STA         ENDFLG          ; SAVE IN STOP/END FLAG - CAUSE A STOP IF
; NEXT LINE ADDRESS IS < $8000; CAUSE
; AN END IF ADDRESS > $8000
                BEQ         LAE15           ; BRANCH TO STOP - END OF PROGRAM
                LDD         ,X+             ; GET CURRENT LINE NUMBER
                STD         CURLIN          ; SAVE IN CURLIN
                STX         CHARAD          ; SAVE ADDRESS OF FIRST BYTE OF LINE
LADC0           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BSR         LADC6           ; GO PROCESS COMMAND
LADC4           BRA         LAD9E           ; GO BACK TO MAIN LOOP
LADC6           BEQ         LAE40           ; RETURN IF END OF LINE
                TSTA                        ;  CHECK FOR TOKEN - BIT 7 SET (NEGATIVE)
                LBPL        LET             ; BRANCH IF NOT A TOKEN - GO DO A LET WHICH
; IS THE DEFAULT TOKEN FOR MICROSOFT BASIC
                CMPA        #$A3            ; SKIPF TOKEN - HIGHEST EXECUTABLE COMMAND IN BASIC
                BHI         LADDC           ; BRANCH IF > A BASIC COMMAND
                LDX         COMVEC+3        ; GET ADDRESS OF BASICS COMMAND TABLE
LADD4           ASLA                        ;  X2 (2 BYTE/JUMP ADDRESS) & DISCARD BIT 7
                TFR         A,B             ; SAVE COMMAND OFFSET IN ACCB
                ABX                         ;  NON X POINTS TO COMMAND JUMP ADDR
                JSR         GETNCH          ; GET AN INPUT CHAR

; HERE IS WHERE WE BRANCH TO DO A COMMAND
                JMP         [,X]            ; GO DO A COMMAND
LADDC           CMPA        #$B4            ; $B4 IS HIGHEST BASIC TOKEN
                BLS         LADB1           ; SYNTAX ERROR IF NON-EXECUTABLE TOKEN
                JMP         [COMVEC+13]     ; JUMP TO AN EX BAS COMMAND

; RESTORE
RESTOR          LDX         TXTTAB          ; BEGINNING OF PROGRAM ADDRESS
                LEAX        -1,X            ; MOVE TO ONE BYTE BEFORE PROGRAM
LADE8           STX         DATPTR          ; SAVE NEW DATA POINTER
                RTS

; BREAK CHECK
LADEB           JSR         >KEYIN1         ; GET A KEYSTROKE ENTRY
                BEQ         LADFA           ; RETURN IF NO INPUT
LADF0           CMPA        #3              ; CONTROL C? (BREAK)
                BEQ         STOP            ; YES
LADF4           CMPA        #$13            ; CONTROL S? (PAUSE)
                BEQ         LADFB           ; YES
                STA         IKEYIM          ; SAVE KEYSTROKE IN INKEY IMAGE
LADFA           RTS
LADFB           JSR         >KEYIN          ; GET A KEY
                BEQ         LADFB           ; BRANCH IF NO KEY DOWN
                BRA         LADF0           ; CONTINUE - DO A BREAK CHECK

; END
ENDTOK          JSR         >LA426          ; CLOSE FILES
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                BRA         LAE0B

; STOP
STOP            ORCC        #$01            ; SET CARRY FLAG
LAE0B           BNE         LAE40           ; BRANCH IF ARGUMENT EXISTS
                LDX         CHARAD          ; SAVE CURRENT POSITION OF
                STX         TINPTR          ; BASICS INPUT POINTER
LAE11           ROR         ENDFLG          ; ROTATE CARRY INTO BIT 7 OF STOP/END FLAG
                LEAS        2,S             ; PURGE RETURN ADDRESS OFF STACK
LAE15           LDX         CURLIN          ; GET CURRENT LINE NUMBER
                CMPX        #$FFFF          ; DIRECT MODE?
                BEQ         LAE22           ; YES
                STX         OLDTXT          ; SAVE CURRENT LINE NUMBER
                LDX         TINPTR          ; GET AND SAVE CURRENT POSITION
                STX         OLDPTR          ; OF BASICS INPUT POINTER
LAE22           CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                LDX         #LABF2-1        ; POINT TO CR, BREAK MESSAGE
                TST         ENDFLG          ; CHECK STOP/END FLAG
                LBPL        LAC73           ; BRANCH TO MAIN LOOP OF BASIC IF END
                JMP         >LAC68          ; PRINT BREAK AT #### AND GO TO
; BASICS MAIN LOOP IF STOP
; CONT
CONT            BNE         LAE40           ; RETURN IF ARGUMENT GIVEN
                LDB         #2*16           ; CANT CONTINUE ERROR
                LDX         OLDPTR          ; GET CONTINUE ADDRESS (INPUT POINTER)
                LBEQ        LAC46           ; CN ERROR IF CONTINUE ADDRESS = 0
                STX         CHARAD          ; RESET BASICS INPUT POINTER
                LDX         OLDTXT          ; GET LINE NUMBER
                STX         CURLIN          ; RESET CURRENT LINE NUMBER
LAE40           RTS

; CLEAR
CLEAR           BEQ         LAE6F           ; BRANCH IF NO ARGUMENT
                JSR         >LB3E6          ; EVALUATE ARGUMENT
                PSHS        B,A             ; SAVE AMOUNT OF STRING SPACE ON STACK
                LDX         MEMSIZ          ; GET CURRENT TOP OF CLEARED SPACE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         LAE5A           ; BRANCH IF NO NEW TOP OF CLEARED SPACE
                JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVALUATE EXPRESSlON; RETURN VALUE IN X
                LEAX        -1,X            ; X = TOP OF CLEARED SPACE
                CMPX        TOPRAM          ; COMPARE TO TOP OF RAM
                BHI         LAE72           ; OM ERROR IF > TOP OF RAM
LAE5A           TFR         X,D             ; ACCD = TOP OF CLEARED SPACE
                SUBD        ,S++            ; SUBTRACT OUT AMOUNT OF CLEARED SPACE
                BCS         LAE72           ; OM ERROR IF FREE MEM < 0
                TFR         D,U             ; U = BOTTOM OF CLEARED SPACE
                SUBD        #STKBUF         ; SUBTRACT OUT STACK BUFFER
                BCS         LAE72           ; OM ERROR IF FREE MEM < 0
                SUBD        VARTAB          ; SUBTRACT OUT START OF VARIABLES
                BCS         LAE72           ; OM ERROR IF FREE MEM < 0
                STU         FRETOP          ; SAVE NEW BOTTOM OF CLEARED SPACE
                STX         MEMSIZ          ; SAVE NEW TOP OF CLEARED SPACE
LAE6F           JMP         >LAD26          ; ERASE ALL VARIABLES, INITIALIZE POINTERS, ETC
LAE72           JMP         >LAC44          ; OM ERROR

; RUN
RUN             JSR         >RVEC18         ; HOOK INTO RAM
                JSR         >LA426          ; CLOSE ANY OPEN FILES
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                LBEQ        LAD21           ; IF NO LINE NUMBER
                JSR         >LAD26          ; ERASE ALL VARIABLES
                BRA         LAE9F           ; GOTO THE RUN ADDRESS

; GO
GO              TFR         A,B             ; SAVE INPUT CHARACTER IN ACCB
LAE88           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                CMPB        #$A5            ; TO TOKEN
                BEQ         LAEA4           ; BRANCH IF GOTO
                CMPB        #$A6            ; SUB TOKEN
                BNE         LAED7           ; SYNTAX ERROR IF NEITHER
                LDB         #3              ; ROOM FOR 6
                JSR         >LAC33          ; BYTES ON STACK?
                LDU         CHARAD          ; SAVE CURRENT BASIC INPUT POINTER, LINE
                LDX         CURLIN          ; NUMBER AND SUB TOKEN ON STACK
                LDA         #$A6
                PSHS        U,X,A
LAE9F           BSR         LAEA4           ; GO DO A GOTO
                JMP         >LAD9E          ; JUMP BACK TO BASICS MAIN LOOP
; GOTO
LAEA4           JSR         GETCCH          ; GET CURRENT INPUT CHAR
                JSR         >LAF67          ; GET LINE NUMBER TO BINARY IN BINVAL
                BSR         LAEEB           ; ADVANCE BASICS POINTER TO END OF LINE
                LEAX        $01,X           ; POINT TO START OF NEXT LINE
                LDD         BINVAL          ; GET THE LINE NUMBER TO RUN
                CMPD        CURLIN          ; COMPARE TO CURRENT LINE NUMBER
                BHI         LAEB6           ; IF REOD LINE NUMBER IS > CURRENT LINE NUMBER,
; DONT START LOOKING FROM
; START OF PROGRAM
                LDX         TXTTAB          ; BEGINNING OF PROGRAM
LAEB6           JSR         >LAD05          ; GO FIND A LINE NUMBER
                BCS         LAED2           ; UNDEFINED LINE NUMBER
LAEBB           LEAX        -1,X            ; MOVE BACK TO JUST BEFORE START OF LINE
                STX         CHARAD          ; RESET BASICS INPUT POINTER
LAEBF           RTS

; RETURN
RETURN          BNE         LAEBF           ; EXIT ROUTINE IF ARGUMENT GIVEN
                LDA         #$FF            ; PUT AN ILLEGAL VARIABLE NAME IN FIRST BYTE OF
                STA         VARDES          ; VARDES WHICH WILL CAUSE FOR/NEXT DATA ON THE
; STACK TO BE IGNORED
                JSR         >LABF9          ; CHECK FOR RETURN DATA ON THE STACK
                TFR         X,S             ; RESET STACK POINTER - PURGE TWO RETURN ADDRESSES
; FROM THE STACK
                CMPA        #$A6-$80        ; SUB TOKEN - $80
                BEQ         LAEDA           ; BRANCH IF RETURN FROM SUBROUTINE
                LDB         #2*2            ; ERROR #2 RETURN WITHOUT GOSUB
                FCB         SKP2            ; SKIP TWO BYTES
LAED2           LDB         #7*2            ; ERROR #7 UNDEFINED LINE NUMBER
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
LAED7           JMP         >LB277          ; SYNTAX ERROR
LAEDA           PULS        A,X,U           ; RESTORE VALUES OF CURRENT LINE NUMBER AND
                STX         CURLIN          ; BASICS INPUT POINTER FOR THIS SUBROUTINE
                STU         CHARAD          ; AND LOAD ACCA WITH SUB TOKEN ($A6)

; DATA
DATA            BSR         LAEE8           ; MOVE INPUT POINTER TO END OF SUBLINE OR LINE
                FCB         SKP2            ; SKIP 2 BYTES
; REM, ELSE
ELSE            EQU         *
REM             BSR         LAEEB           ; MOVE INPUT POINTER TO END OF LINE
                STX         CHARAD          ; RESET BASICS INPUT POINTER
LAEE7           RTS
; ADVANCE INPUT POINTER TO END OF SUBLINE OR LINE
LAEE8           LDB         #':             ; COLON = SUBLINE TERMINATOR CHARACTER
LAEEA           FCB         SKP1LD          ; SKPILD SKIP ONE BYTE; LDA #$5F
; ADVANCE BASICS INPUT POINTER TO END OF
; LINE - RETURN ADDRESS OF END OF LINE+1 IN X
LAEEB           CLRB                        ;  0 LINE TERMINATOR CHARACTER
                STB         CHARAC          ; TEMP STORE PRIMARY TERMINATOR CHARACTER
                CLRB                        ;  0 (END OF LINE) = ALTERNATE TERM. CHAR.
                LDX         CHARAD          ; LOAD X W/BASICS INPUT POINTER
LAEF1           TFR         B,A             ; CHANGE TERMINATOR CHARACTER
                LDB         CHARAC          ; FROM ACCB TO CHARAC - SAVE OLD TERMINATOR
; IN CHARAC
                STA         CHARAC          ; SWAP PRIMARY AND SECONDARY TERMINATORS
LAEF7           LDA         ,X              ; GET NEXT INPUT CHARACTER
                BEQ         LAEE7           ; RETURN IF 0 (END OF LINE)
                PSHS        B               ; SAVE TERMINATOR ON STACK
                CMPA        ,S+             ; COMPARE TO INPUT CHARACTER
                BEQ         LAEE7           ; RETURN IF EQUAL
                LEAX        1,X             ; MOVE POINTER UP ONE
                CMPA        #'"             ; CHECK FOR DOUBLE QUOTES
                BEQ         LAEF1           ; BRANCH IF " - TOGGLE TERMINATOR CHARACTERS
                INCA                        ;  CHECK FOR $FF AND BRANCH IF
                BNE         LAF0C           ; NOT SECONDARY TOKEN
                LEAX        1,X             ; MOVE INPUT POINTER 1 MORE IF SECONDARY
LAF0C           CMPA        #$85+1          ; TOKEN FOR IF?
                BNE         LAEF7           ; NO - GET ANOTHER INPUT CHARACTER
                INC         IFCTR           ; INCREMENT IF COUNTER - KEEP TRACK OF HOW MANY
; IF STATEMENTS ARE NESTED IN ONE LINE
                BRA         LAEF7           ; GET ANOTHER INPUT CHARACTER
; IF
IFTOK           JSR         >LB141          ; EVALUATE NUMERIC EXPRESSION
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #$81            ; TOKEN FOR GO
                BEQ         LAF22           ; TREAT GO THE SAME AS THEN
                LDB         #$A7            ; TOKEN FOR THEN
                JSR         >LB26F          ; DO A SYNTAX CHECK ON ACCB
LAF22           LDA         FP0EXP          ; CHECK FOR TRUE/FALSE - FALSE IF FPA0 EXPONENT = ZERO
                BNE         LAF39           ; BRANCH IF CONDITION TRUE
                CLR         IFCTR           ; CLEAR FLAG - KEEP TRACK OF WHICH NESTED ELSE STATEMENT
; TO SEARCH FOR IN NESTED IF LOOPS
LAF28           BSR         DATA            ; MOVE BASICS POINTER TO END OF SUBLINE
                TSTA                        ;  CHECK TO SEE IF END OF LINE OR SUBLINE
                BEQ         LAEE7           ; AND RETURN IF END OF LINE
                JSR         GETNCH          ; GET AN INPUT CHARACTER FROM BASIC
                CMPA        #$84            ; TOKEN FOR ELSE
                BNE         LAF28           ; IGNORE ALL DATA EXCEPT ELSE UNTIL
; END OF LINE (ZERO BYTE)
                DEC         IFCTR           ; CHECK TO SEE IF YOU MUST SEARCH ANOTHER SUBLINE
                BPL         LAF28           ; BRANCH TO SEARCH ANOTHER SUBLINE FOR ELSE
                JSR         GETNCH          ; GET AN INPUT CHARACTER FROM BASIC
LAF39           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                LBCS        LAEA4           ; BRANCH TO GOTO IF NUMERIC CHARACTER
                JMP         >LADC6          ; RETURN TO MAIN INTERPRETATION LOOP
; ON
ON              JSR         >LB70B          ; EVALUATE EXPRESSION
LAF45           LDB         #$81            ; TOKEN FOR GO
                JSR         >LB26F          ; SYNTAX CHECK FOR GO
                PSHS        A               ; SAVE NEW TOKEN (TO,SUB)
                CMPA        #$A6            ; TOKEN FOR SUB?
                BEQ         LAF54           ; YES
                CMPA        #$A5            ; TOKEN FOR TO?
LAF52           BNE         LAED7           ; SYNTAX ERROR IF NOT SUB OR TO
LAF54           DEC         FPA0+3          ; DECREMENT IS BYTE OF MANTISSA OF FPA0 - THIS
; IS THE ARGUMENT OF THE ON STATEMENT
                BNE         LAF5D           ; BRANCH IF NOT AT THE PROPER GOTO OR GOSUB LINE NUMBER
                PULS        B               ; GET BACK THE TOKEN FOLLOWING GO
                JMP         >LAE88          ; GO DO A GOTO OR GOSUB
LAF5D           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BSR         LAF67           ; CONVERT BASIC LINE NUMBER TO BINARY
                CMPA        #',             ; IS CHARACTER FOLLOWING LINE NUMBER A COMMA?
                BEQ         LAF54           ; YES
                PULS        B,PC            ; IF NOT, FALL THROUGH TO NEXT COMMAND
LAF67           LDX         ZERO            ; DEFAULT LINE NUMBER OF ZERO
                STX         BINVAL          ; SAVE IT IN BINVAL

; CONVERT LINE NUMBER TO BINARY - RETURN VALUE IN BINVAL

LAF6B           BCC         LAFCE           ; RETURN IF NOT NUMERIC CHARACTER
                SUBA        #'0             ; MASK OFF ASCII
                STA         CHARAC          ; SAVE DIGIT IN VO1
                LDD         BINVAL          ; GET ACCUMULATED LINE NUMBER VALUE
                CMPA        #24             ; LARGEST LINE NUMBER IS $F9FF (63999) -
; (24*256+255)*10+9
                BHI         LAF52           ; SYNTAX ERROR IF TOO BIG
; MULT ACCD X 10
                ASLB                        ;
                ROLA                        ;  TIMES 2
                ASLB                        ;
                ROLA                        ;  TIMES 4
                ADDD        BINVAL          ; ADD 1 = TIMES 5
                ASLB                        ;
                ROLA                        ;  TIMES 10
                ADDB        CHARAC          ; ADD NEXT DIGIT
                ADCA        #0              ; PROPAGATE CARRY
                STD         BINVAL          ; SAVE NEW ACCUMULATED LINE NUMBER
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                BRA         LAF6B           ; LOOP- PROCESS NEXT DIGIT

; LET (EXBAS)
; EVALUATE A NON-TOKEN EXPRESSION
; TARGET = REPLACEMENT
LET             JSR         >LB357          ; FIND TARGET VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE DESCRIPTOR ADDRESS OF 1ST EXPRESSION
                LDB         #$B3            ; TOKEN FOR "="
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR =
                LDA         VALTYP          ; GET VARIABLE TYPE AND
                PSHS        A               ; SAVE ON THE STACK
                JSR         >LB156          ; EVALUATE EXPRESSION
LAF9A           PULS        A               ; REGET VARIABLE TYPE OF 1ST EXPRESSION AND
                RORA                        ;  SET CARRY IF STRING
                JSR         >LB148          ; TYPE CHECK-TM ERROR IF VARIABLE TYPES ON
; BOTH SIDES OF EQUALS SIGN NOT THE SAME
                LBEQ        LBC33           ; GO PUT FPA0 INTO VARIABLE DESCRIPTOR IF NUMERIC
; MOVE A STRING WHOSE DESCRIPTOR IS LOCATED AT
; FPA0+2 INTO THE STRING SPACE. TRANSFER THE
; DESCRIPTOR ADDRESS TO THE ADDRESS IN VARDES
; DONT MOVE THE STRING IF IT IS ALREADY IN THE
; STRING SPACE. REMOVE DESCRIPTOR FROM STRING
; STACK IF IT IS LAST ONE ON THE STACK
LAFA4           LDX         FPA0+2          ; POINT X TO DESCRIPTOR OF REPLACEMENT STRING
                LDD         FRETOP          ; LOAD ACCD WITH START OF STRING SPACE
                CMPD        2,X             ; IS THE STRING IN STRING SPACE?
                BCC         LAFBE           ; BRANCH IF ITS NOT IN THE STRING SPACE
                CMPX        VARTAB          ; COMPARE DESCRIPTOR ADDRESS TO START OF VARIABLES
                BCS         LAFBE           ; BRANCH IF DESCRIPTOR ADDRESS NOT IN VARIABLES
LAFB1           LDB         ,X              ; GET LENGTH OF REPLACEMENT STRING
                JSR         >LB50D          ; RESERVE ACCB BYTES OF STRING SPACE
                LDX         V4D             ; GET DESCRIPTOR ADDRESS BACK
                JSR         >LB643          ; MOVE STRING INTO STRING SPACE
                LDX         #STRDES         ; POINT X TO TEMP STRING DESCRIPTOR ADDRESS
LAFBE           STX         V4D             ; SAVE STRING DESCRIPTOR ADDRESS IN V4D
                JSR         >LB675          ; REMOVE STRING DESCRIPTOR IF LAST ONE
; ON STRING STACK
                LDU         V4D             ; POINT U TO REPLACEMENT DESCRIPTOR ADDRESS
                LDX         VARDES          ; GET TARGET DESCRIPTOR ADDRESS
                PULU        A,B,Y           ; GET LENGTH AND START OF REPLACEMENT STRING
                STA         ,X              ; SAVE STRING LENGTH AND START IN
                STY         2,X             ; TARGET DESCRIPTOR LOCATION
LAFCE           RTS
LAFCF           FCC         '?REDO'         ; ?REDO MESSAGE
                FCB         CR,$00
LAFD6           LDB         #2*17           ; BAD FILE DATA ERROR
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND BRANCH
                BEQ         LAFDF           ; IF SET TO SCREEN
LAFDC           JMP         >LAC46          ; JMP TO ERROR HANDLER
LAFDF           LDA         INPFLG          ; GET THE INPUT FLAG AND BRANCH
                BEQ         LAFEA           ; IF INPUT
                LDX         DATTXT          ; GET LINE NUMBER WHERE THE ERROR OCCURRED
                STX         CURLIN          ; AND USE IT AS THE CURRENT LINE NUMBER
                JMP         >LB277          ; SYNTAX ERROR
LAFEA           LDX         #LAFCF-1        ; POINT X TO ?REDO AND PRINT
                JSR         >LB99C          ; IT ON THE SCREEN
                LDX         TINPTR          ; GET THE SAVED ABSOLUTE ADDRESS OF
                STX         CHARAD          ; INPUT POINTER AND RESTORE IT
                RTS

; INPUT
INPUT           LDB         #11*2           ; ID ERROR
                LDX         CURLIN          ; GET CURRENT LINE NUMBER
                LEAX        1,X             ; ADD ONE
                BEQ         LAFDC           ; ID ERROR BRANCH IF DIRECT MODE
                BSR         LB002           ; GET SOME INPUT DATA
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                RTS
LB002           CMPA        #'#             ; CHECK FOR DEVICE NUMBER
                BNE         LB00F           ; NO DEVICE NUMBER GIVEN
                JSR         >LA5A5          ; CHECK SYNTAX AND GET DEVICE NUMBER
                JSR         >LA3ED          ; CHECK FOR VALID INPUT FILE
LB00C           JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
LB00F           CMPA        #'"             ; CHECK FOR PROMPT STRING DELIMITER
                BNE         LB01E           ; BRANCH IF NO PROMPT STRING
                JSR         >LB244          ; PUT PROMPT STRING ON STRING STACK
                LDB         #';
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR SEMICOLON
                JSR         >LB99F          ; PRINT MESSAGE TO CONSOLE OUT
LB01E           LDX         #LINBUF         ; POINT TO BASICS LINE BUFFER
                CLR         ,X              ; CLEAR 1ST BYTE - FLAG TO INDICATE NO DATA
; IN LINE BUFFER
                TST         DEVNUM          ; CHECK DEVICE NUMBER
                BNE         LB049           ; BRANCH IF NOT SET TO SCREEN
                BSR         LB02F           ; INPUT A STRING TO LINE BUFFER
                LDB         #',             ; INSERT A COMMA AT THE END
                STB         ,X              ; OF THE LINE INPUT BUFFER
                BRA         LB049
; FILL BASICS LINE INPUT BUFFER CONSOLE IN
LB02F           JSR         >LB9AF          ; SEND A "?" TO CONSOLE OUT
                JSR         >LB9AC          ; SEND A SPACE TO CONSOLE OUT
LB035           JSR         >LA390          ; GO READ IN A BASIC LINE
                BCC         LB03F           ; BRANCH IF ENTER KEY ENDED ENTRY
                LEAS        4,S             ; PURGE TWO RETURN ADDRESSES OFF THE STACK
LB03C           JMP         >LAE11          ; GO DO A STOP IF BREAK KEY ENDED LINE ENTRY
LB03F           LDB         #2*23           ; INPUT PAST END OF FILE ERROR
                TST         CINBFL          ; CHECK FOR MORE CHARACTERS IN CONSOLE IN BUFFER
                BNE         LAFDC           ; IE ERROR IF EMPTY
                RTS

; READ
READ            LDX         DATPTR          ; GET READ START ADDRESS
                FCB         SKP1LD          ; SKIP ONE BYTE - LDA #*$4F
LB049           CLRA                        ;  INPUT ENTRY POINT: INPUT FLAG = 0
                STA         INPFLG          ; SET INPUT FLAG; 0 = INPUT: <> 0 = READ
                STX         DATTMP          ; SAVE READ START ADDRESS/INPUT BUFFER START
LB04E           JSR         >LB357          ; EVALUATE A VARIABLE
                STX         VARDES          ; SAVE DESCRIPTOR ADDRESS
                LDX         CHARAD          ; GET BASICS INPUT POINTER
                STX         BINVAL          ; AND SAVE IT
                LDX         DATTMP          ; GET READ ADDRESS START/INPUT BUFFER POINTER
                LDA         ,X              ; GET A CHARACTER FROM THE BASIC PROGRAM
                BNE         LB069           ; BRANCH IF NOT END OF LINE
                LDA         INPFLG          ; CHECK INPUT FLAG AND BRANCH
                BNE         LB0B9           ; IF LOOKING FOR DATA (READ)
; NO DATA IN INPUT LINE BUFFER AND/OR INPUT
; NOT COMING FROM SCREEN
                JSR         >RVEC10         ; HOOK INTO RAM IF INPUT
                JSR         >LB9AF          ; SEND A '?' TO CONSOLE OUT
                BSR         LB02F           ; FILL INPUT BUFFER FROM CONSOLE IN
LB069           STX         CHARAD          ; RESET BASICS INPUT POINTER
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                LDB         VALTYP          ; CHECK VARIABLE TYPE AND
                BEQ         LB098           ; BRANCH IF NUMERIC
; READ/INPUT A STRING VARIABLE
                LDX         CHARAD          ; LOAD X WITH CURRENT BASIC INPUT POINTER
                STA         CHARAC          ; SAVE CURRENT INPUT CHARACTER
                CMPA        #'"             ; CHECK FOR STRING DELIMITER
                BEQ         LB08B           ; BRANCH IF STRING DELIMITER
                LEAX        -1,X            ; BACK UP POINTER
                CLRA                        ;  ZERO = END OF LINE CHARACTER
                STA         CHARAC          ; SAVE AS TERMINATOR
                JSR         >LA35F          ; SET UP PRINT PARAMETERS
                TST         PRTDEV          ; CHECK PRINT DEVICE NUMBER
                BNE         LB08B           ; BRANCH IF CASSETTE - USE TWO ZEROS AS TERMINATOR
; CHARACTERS FOR CASSETTE
                LDA         #':             ; END OF SUBLINE CHARACTER
                STA         CHARAC          ; SAVE AS TERMINATOR I
                LDA         #',             ; COMMA
LB08B           STA         ENDCHR          ; SAVE AS TERMINATOR 2
                JSR         >LB51E          ; STRIP A STRING FROM THE INPUT BUFFER
                JSR         >LB249          ; MOVE INPUT POINTER TO END OF STRING
                JSR         >LAFA4          ; PUT A STRING INTO THE STRING SPACE IF NECESSARY
                BRA         LB09E           ; CHECK FOR ANOTHER DATA ITEM
; SAVE A NUMERIC VALUE IN A READ OR INPUT DATA ITEM
LB098           JSR         >LBD12          ; CONVERT AN ASCII STRING TO FP NUMBER
                JSR         >LBC33          ; PACK FPA0 AND STORE IT IN ADDRESS IN VARDES -
; INPUT OR READ DATA ITEM
LB09E           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         LB0A8           ; BRANCH IF END OF LINE
                CMPA        #',             ; CHECK FOR A COMMA
                LBNE        LAFD6           ; 'BAD FILE DATA' ERROR OR RETRY
LB0A8           LDX         CHARAD          ; GET CURRENT INPUT
                STX         DATTMP          ; POINTER (USED AS A DATA POINTER) AND SAVE IT
                LDX         BINVAL          ; RESET INPUT POINTER TO INPUT OR
                STX         CHARAD          ; READ STATEMENT
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                BEQ         LB0D5           ; BRANCH IF END OF LINE - EXIT COMMAND
                JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
                BRA         LB04E           ; GET ANOTHER INPUT OR READ ITEM
; SEARCH FROM ADDRESS IN X FOR
; 1ST OCCURENCE OF THE TOKEN FOR DATA
LB0B9           STX         CHARAD          ; RESET BASICS INPUT POINTER
                JSR         >LAEE8          ; SEARCH FOR END OF CURRENT LINE OR SUBLINE
                LEAX        1,X             ; MOVE X ONE PAST END OF LINE
                TSTA                        ;  CHECK FOR END OF LINE
                BNE         LB0CD           ; BRANCH IF END OF SUBLINE
                LDB         #2*3            ; OUT OF DATA ERROR
                LDU         ,X++            ; GET NEXT 2 CHARACTERS
                BEQ         LB10A           ; OD ERROR IF END OF PROGRAM
                LDD         ,X++            ; GET BASIC LINE NUMBER AND
                STD         DATTXT          ; SAVE IT IN DATTXT
LB0CD           LDA         ,X              ; GET AN INPUT CHARACTER
                CMPA        #$86            ; DATA TOKEN?
                BNE         LB0B9           ; NO KEEP LOOKING
                BRA         LB069           ; YES
; EXIT READ AND INPUT COMMANDS
LB0D5           LDX         DATTMP          ; GET DATA POINTER
                LDB         INPFLG          ; CHECK INPUT FLAG
                LBNE        LADE8           ; SAVE NEW DATA POINTER IF READ
                LDA         ,X              ; CHECK NEXT CHARACTER IN INPUT BUFFER
                BEQ         LB0E7           ; RETURN IF NO MORE DATA FOR INPUT
                LDX         #LB0E8-1        ; POINT X TO ?EXTRA IGNORED
                JMP         >LB99C          ; PRINT THE MESSAGE
LB0E7           RTS
LB0E8           FCC         '?EXTRA IGNORED' ; ?EXTRA IGNORED MESSAGE
                FCB         CR,$00
; NEXT
NEXT            BNE         LB0FE           ; BRANCH IF ARGUMENT GIVEN
                LDX         ZERO            ; X = 0: DEFAULT FOR NO ARGUMENT
                BRA         LB101
LB0FE           JSR         >LB357          ; EVALUATE AN ALPHA EXPRESSION
LB101           STX         VARDES          ; SAVE VARIABLE DESCRIPTOR POINTER
                JSR         >LABF9          ; GO SCAN FOR FOR/NEXT DATA ON STACK
                BEQ         LB10C           ; BRANCH IF DATA FOUND
                LDB         #0              ; NEXT WITHOUT FOR ERROR (SHOULD BE CLRB)
LB10A           BRA         LB153           ; PROCESS ERROR
LB10C           TFR         X,S             ; POINT S TO START OF FOR/NEXT DATA
                LEAX        3,X             ; POINT X TO FP VALUE OF STEP
                JSR         >LBC14          ; COPY A FP NUMBER FROM (X) TO FPA0
                LDA         8,S             ; GET THE DIRECTION OF STEP
                STA         FP0SGN          ; SAVE IT AS THE SIGN OF FPA0
                LDX         VARDES          ; POINT (X) TO INDEX VARIABLE DESCRIPTOR
                JSR         >LB9C2          ; ADD (X) TO FPA0 (STEP TO INDEX)
                JSR         >LBC33          ; PACK FPA0 AND STORE IT IN ADDRESS
; CONTAINED IN VARDES
                LEAX        9,S             ; POINT (X) TO TERMINAL VALUE OF INDEX
                JSR         >LBC96          ; COMPARE CURRENT INDEX VALUE TO TERMINAL VALUE OF INDEX
                SUBB        8,S             ; ACCB = 0 IF TERMINAL VALUE=CURRENT VALUE AND STEP=0 OR IF
; STEP IS POSITIVE AND CURRENT VALUE>TERMINAL VALUE OR
; STEP IS NEGATIVE AND CURRENT VALUE<TERMINAL VALUE
                BEQ         LB134           ; BRANCH IF FOR/NEXT LOOP DONE
                LDX         14,S            ; GET LINE NUMBER AND
                STX         CURLIN          ; BASIC POINTER OF
                LDX         16,S            ; STATEMENT FOLLOWING THE
                STX         CHARAD          ; PROPER FOR STATEMENT
LB131           JMP         >LAD9E          ; JUMP BACK TO COMMAND INTEPR. LOOP
LB134           LEAS        18,S            ; PULL THE FOR-NEXT DATA OFF THE STACK
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #',             ; CHECK FOR ANOTHER ARGUMENT
                BNE         LB131           ; RETURN IF NONE
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                BSR         LB0FE           ; BSR SIMULATES A CALL TO NEXT FROM COMMAND LOOP
; EVALUATE A NUMERIC EXPRESSION
LB141           BSR         LB156           ; EVALUATE EXPRESSION AND DO A TYPE CHECK FOR NUMERIC
LB143           ANDCC       #$FE            ; CLEAR CARRY FLAG
LB145           FCB         $7D             ; OP CODE OF TST $1A01 - SKIP TWO BYTES (DO
; NOT CHANGE CARRY FLAG)
LB146           ORCC        #1              ; SET CARRY
; STRING TYPE MODE CHECK - IF ENTERED AT LB146 THEN VALTYP PLUS IS 'TM' ERROR
; NUMERIC TYPE MODE CHECK - IF ENTERED AT LB143 THEN VALTYP MINUS IS 'TM' ERROR
; IF ENTERED AT LB148, A TYPE CHECK IS DONE ON VALTYP
; IF ENTERED WITH CARRY SET, THEN 'TM' ERROR IF NUMERIC
; IF ENTERED WITH CARRY CLEAR, THEN 'TM' ERROR IF STRING.
LB148           TST         VALTYP          ; TEST TYPE FLAG; DO NOT CHANGE CARRY
                BCS         LB14F           ; BRANCH IF STRING
                BPL         LB0E7           ; RETURN ON PLUS
                FCB         SKP2            ; SKIP 2 BYTES - TM ERROR
LB14F           BMI         LB0E7           ; RETURN ON MINUS
LB151           LDB         #12*2           ; TYPE MISMATCH ERROR
LB153           JMP         >LAC46          ; PROCESS ERROR
; EVALUATE EXPRESSION
LB156           BSR         LB1C6           ; BACK UP INPUT POINTER
LB158           CLRA                        ;  END OF OPERATION PRECEDENCE FLAG
                FCB         SKP2            ; SKIP TWO BYTES
LB15A           PSHS        B               ; SAVE FLAG (RELATIONAL OPERATOR FLAG)
                PSHS        A               ; SAVE FLAG (PRECEDENCE FLAG)
                LDB         #1
                JSR         >LAC33          ; SEE IF ROOM IN FREE RAM FOR (B) WORDS
                JSR         >LB223          ; GO EVALUATE AN EXPRESSION
LB166           CLR         TRELFL          ; RESET RELATIONAL OPERATOR FLAG
LB168           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
; CHECK FOR RELATIONAL OPERATORS
LB16A           SUBA        #$B2            ; TOKEN FOR >
                BCS         LB181           ; BRANCH IF LESS THAN RELATIONAL OPERATORS
                CMPA        #3
                BCC         LB181           ; BRANCH IF GREATER THAN RELATIONAL OPERATORS
                CMPA        #1              ; SET CARRY IF >
                ROLA                        ;  CARRY TO BIT 0
                EORA        TRELFL          ; CARRY SET IF
                CMPA        TRELFL          ; TRELFL = ACCA
                BCS         LB1DF           ; BRANCH IF SYNTAX ERROR : == << OR >>
                STA         TRELFL          ; BIT 0: >, BIT 1 =, BIT 2: < SAVE DESIRED RELATIONAL COMPARISON
                JSR         GETNCH          ; GET AN INPUT CHARACTER
                BRA         LB16A           ; CHECK FOR ANOTHER RELATIONAL OPERATOR

LB181           LDB         TRELFL          ; GET RELATIONAL OPERATOR FLAG
                BNE         LB1B8           ; BRANCH IF RELATIONAL COMPARISON
                LBCC        LB1F4           ; BRANCH IF > RELATIONAL OPERATOR
                ADDA        #7              ; SEVEN ARITHMETIC/LOGICAL OPERATORS
                BCC         LB1F4           ; BRANCH IF NOT ARITHMETIC/LOGICAL OPERATOR
                ADCA        VALTYP          ; ADD CARRY, NUMERIC FLAG AND MODIFIED TOKEN NUMBER
                LBEQ        LB60F           ; BRANCH IF VALTYP = FF, AND ACCA = + TOKEN CONCATENATE TWO STRINGS
                ADCA        #-1             ; RESTORE ARITHMETIC/LOGICAL OPERATOR NUMBER
                PSHS        A               ; STORE OPERATOR NUMBER ON STACK; MULTIPLY IT BY 2
                ASLA                        ;  THEN ADD THE STORED STACK DATA = MULTIPLY
                ADDA        ,S+             ; X 3; 3 BYTE/TABLE ENTRY
                LDX         #LAA51          ; JUMP TABLE FOR ARITHMETIC & LOGICAL OPERATORS
                LEAX        A,X             ; POINT X TO PROPER TABLE
LB19F           PULS        A               ; GET PRECEDENCE FLAG FROM STACK
                CMPA        ,X              ; COMPARE TO CURRENT OPERATOR
                BCC         LB1FA           ; BRANCH IF STACK OPERATOR > CURRENT OPERATOR
                BSR         LB143           ; TM ERROR IF VARIABLE TYPE = STRING
; OPERATION BEING PROCESSED IS OF HIGHER PRECEDENCE THAN THE PREVIOUS OPERATION.
LB1A7           PSHS        A               ; SAVE PRECEDENCE FLAG
                BSR         LB1D4           ; PUSH OPERATOR ROUTINE ADDRESS AND FPA0 ONTO STACK
                LDX         RELPTR          ; GET POINTER TO ARITHMETIC/LOGICAL TABLE ENTRY FOR
; LAST CALCULATED OPERATION
                PULS        A               ; GET PRECEDENCE FLAG OF PREVIOUS OPERATION
                BNE         LB1CE           ; BRANCH IF NOT END OF OPERATION
                TSTA                        ;  CHECK TYPE OF PRECEDENCE FLAG
                LBEQ        LB220           ; BRANCH IF END OF EXPRESSION OR SUB-EXPRESSION
                BRA         LB203           ; EVALUATE AN OPERATION
; DO A RELATIONAL COMPARISON HERE
LB1B8           ASL         VALTYP          ; BIT 7 OF TYPE FLAG TO CARRY
                ROLB                        ;  SHIFT RELATIONAL FLAG LEFT - VALTYP TO BIT 0
                BSR         LB1C6           ; MOVE THE INPUT POINTER BACK ONE
                LDX         #LB1CB          ; POINT X TO RELATIONAL COMPARISON JUMP TABLE
                STB         TRELFL          ; SAVE RELATIONAL COMPARISON DATA
                CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                BRA         LB19F           ; PERFORM OPERATION OR SAVE ON STACK
LB1C6           LDX         CHARAD          ; GET BASICS INPUT POINTER AND
                JMP         >LAEBB          ; MOVE IT BACK ONE
; RELATIONAL COMPARISON JUMP TABLE
LB1CB           FCB         $64             ; RELATIONAL COMPARISON FLAG
LB1CC           FDB         LB2F4           ; JUMP ADDRESS
LB1CE           CMPA        ,X              ; COMPARE PRECEDENCE OF LAST DONE OPERATION TO
; NEXT TO BE DONE OPERATION
                BCC         LB203           ; EVALUATE OPERATION IF LOWER PRECEDENCE
                BRA         LB1A7           ; PUSH OPERATION DATA ON STACK IF HIGHER PRECEDENCE
; PUSH OPERATOR EVALUATION ADDRESS AND FPA0 ONTO STACK AND EVALUATE ANOTHER EXPR
LB1D4           LDD         1,X             ; GET ADDRESS OF OPERATOR ROUTINE
                PSHS        B,A             ; SAVE IT ON THE STACK
                BSR         LB1E2           ; PUSH FPA0 ONTO STACK
                LDB         TRELFL          ; GET BACK RELATIONAL OPERATOR FLAG
                LBRA        LB15A           ; EVALUATE ANOTHER EXPRESSION
LB1DF           JMP         >LB277          ; SYNTAX ERROR
; PUSH FPA0 ONTO THE STACK. ,S = EXPONENT
; 1-2,S =HIGH ORDER MANTISSA 3-4,S = LOW ORDER MANTISSA
; 5,S = SIGN RETURN WITH PRECEDENCE CODE IN ACCA
LB1E2           LDB         FP0SGN          ; GET SIGN OF FPA0 MANTISSA
                LDA         ,X              ; GET PRECEDENCE CODE TO ACCA
LB1E6           PULS        Y               ; GET RETURN ADDRESS FROM STACK & PUT IT IN Y
                PSHS        B               ; SAVE ACCB ON STACK
LB1EA           LDB         FP0EXP          ; PUSH FPA0 ONTO THE STACK
                LDX         FPA0
                LDU         FPA0+2
                PSHS        U,X,B
                JMP         ,Y              ; JUMP TO ADDRESS IN Y
; BRANCH HERE IF NON-OPERATOR CHARACTER FOUND - USUALLY ) OR END OF LINE
LB1F4           LDX         ZERO            ; POINT X TO DUMMY VALUE (ZERO)
                LDA         ,S+             ; GET PRECEDENCE FLAG FROM STACK
                BEQ         LB220           ; BRANCH IF END OF EXPRESSION
LB1FA           CMPA        #$64            ; CHECK FOR RELATIONAL COMPARISON FLAG
                BEQ         LB201           ; AND BRANCH IF RELATIONAL COMPARISON
                JSR         >LB143          ; TM ERROR IF VARIABLE TYPE = STRING
LB201           STX         RELPTR          ; SAVE POINTER TO OPERATOR ROUTINE
LB203           PULS        B               ; GET RELATIONAL OPERATOR FLAG FROM STACK
                CMPA        #$5A            ; CHECK FOR NOT OPERATOR
                BEQ         LB222           ; RETURN IF NOT - NO RELATIONAL COMPARISON
                CMPA        #$7D            ; CHECK FOR NEGATION (UNARY) FLAG
                BEQ         LB222           ; RETURN IF NEGATION - NO RELATIONAL COMPARISON
; EVALUATE AN OPERATION. EIGHT BYTES WILL BE STORED ON STACK, FIRST SIX BYTES
; ARE A TEMPORARY FLOATING POINT RESULT THEN THE ADDRESS OF ROUTINE WHICH
; WILL EVALUATE THE OPERATION. THE RTS AT END OF ROUTINE WILL VECTOR
; TO EVALUATING ROUTINE.
                LSRB                        ;  ROTATE VALTYP BIT INTO CARRY
                STB         RELFLG          ; FLAG AND SAVE NEW RELFLG
                PULS        A,X,U           ; PULL A FP VALUE OFF OF THE STACK
                STA         FP1EXP          ; AND SAVE IT IN FPA1
                STX         FPA1
                STU         FPA1+2
                PULS        B               ; GET MANTISSA SIGN AND
                STB         FP1SGN          ; SAVE IT IN FPA1
                EORB        FP0SGN          ; EOR IT WITH FPA1 MANTISSA SIGN
                STB         RESSGN          ; SAVE IT IN RESULT SIGN BYTE
LB220           LDB         FP0EXP          ; GET EXPONENT OF FPA0
LB222           RTS
LB223           JSR         >RVEC15         ; HOOK INTO RAM
                CLR         VALTYP          ; INITIALIZE TYPE FLAG TO NUMERIC
LB228           JSR         GETNCH          ; GET AN INPUT CHAR
                BCC         LB22F           ; BRANCH IF NOT NUMERIC
LB22C           JMP         >LBD12          ; CONVERT ASCII STRING TO FLOATING POINT -
; RETURN RESULT IN FPA0
; PROCESS A NON NUMERIC FIRST CHARACTER
LB22F           JSR         >LB3A2          ; SET CARRY IF NOT ALPHA
                BCC         LB284           ; BRANCH IF ALPHA CHARACTER
                CMPA        #'.             ; IS IT . (DECIMAL POINT)?
                BEQ         LB22C           ; CONVERT ASCII STRING TO FLOATING POINT
                CMPA        #$AC            ; MINUS TOKEN
                BEQ         LB27C           ; YES - GO PROCESS THE MINUS OPERATOR
                CMPA        #$AB            ; PLUS TOKEN
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
                BEQ         LB228           ; YES - GET ANOTHER CHARACTER
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                BEQ         LB223           ; YES - GET ANOTHER CHARACTER
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                CMPA        #'"             ; STRING DELIMITER?
                BNE         LB24E           ; NO
LB244           LDX         CHARAD          ; CURRENT BASIC POINTER TO X
                JSR         >LB518          ; SAVE STRING ON STRING STACK
LB249           LDX         COEFPT          ; GET ADDRESS OF END OF STRING AND
                STX         CHARAD          ; PUT BASICS INPUT POINTER THERE
                RTS
LB24E           CMPA        #$A8            ; NOT TOKEN?
                BNE         LB25F           ; NO
; PROCESS THE NOT OPERATOR
                LDA         #$5A            ; NOT PRECEDENCE FLAG
                JSR         >LB15A          ; PROCESS OPERATION FOLLOWING NOT
                JSR         >INTCNV         ; CONVERT FPA0 TO INTEGER IN ACCD
                COMA                        ;  NOT THE INTEGER
                COMB                        ;
                JMP         >GIVABF         ; CONVERT ACCD TO FLOATING POINT (FPA0)
LB25F           INCA                        ;  CHECK FOR TOKENS PRECEEDED BY 5FF
                BEQ         LB290           ; IT WAS PRECEEDED BY 5FF
LB262           BSR         LB26A           ; SYNTAX CHECK FOR A (
                JSR         >LB156          ; EVALUATE EXPRESSIONS WITHIN PARENTHESES AT
; HIGHEST PRECEDENCE
LB267           LDB         #')             ; SYNTAX CHECK FOR )
                FCB         SKP2            ; SKIP 2 BYTES
LB26A           LDB         #'(             ; SYNTAX CHECK FOR (
                FCB         SKP2            ; SKIP 2 BYTES
SYNCOMMA        EQU         *
LB26D           LDB         #',             ; SYNTAX CHECK FOR COMMA
LB26F           CMPB        [CHARAD]        ; COMPARE ACCB TO CURRENT INPUT
                BNE         LB277           ; CHARACTER - SYNTAX ERROR IF NO MATCH
                JMP         GETNCH          ; GET A CHARACTER FROM BASIC
LB277           LDB         #2*1            ; SYNTAX ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
; PROCESS THE MINUS (UNARY) OPERATOR
LB27C           LDA         #$7D            ; MINUS (UNARY) PRECEDENCE FLAG
                JSR         >LB15A          ; PROCESS OPERATION FOLLOWING UNARY NEGATION
                JMP         >LBEE9          ; CHANGE SIGN OF FPA0 MANTISSA
; EVALUATE ALPHA EXPRESSION
LB284           JSR         >LB357          ; FIND THE DESCRIPTOR ADDRESS OF A VARIABLE
LB287           STX         FPA0+2          ; SAVE DESCRIPTOR ADDRESS IN FPA0
                LDA         VALTYP          ; TEST VARIABLE TYPE
                BNE         LB222           ; RETURN IF STRING
                JMP         >LBC14          ; COPY A FP NUMBER FROM (X) TO FPA0
; EVALUATING A SECONDARY TOKEN
LB290           JSR         GETNCH          ; GET AN INPUT CHARACTER (SECONDARY TOKEN)
                TFR         A,B             ; SAVE IT IN ACCB
                ASLB                        ;  X2 & BET RID OF BIT 7
                JSR         GETNCH          ; GET ANOTHER INPUT CHARACTER
                CMPB        #2*19           ; 19 SECONDARY FUNCTIONS IN BASIC
                BLS         LB29F           ; BRANCH IF COLOR BASIC TOKEN
                JMP         [COMVEC+18]     ; JUMP TO EXBAS SECONDARY TOKEN HANDLER
LB29F           PSHS        B               ; SAVE TOKEN OFFSET ON STACK
                CMPB        #2*14           ; CHECK FOR NUMERIC ARGUMENT TOKEN
                BCS         LB2C7           ; DO SECONDARIES $8D (JOYSTK) OR LESS
                CMPB        #2*18
                BCC         LB2C9           ; DO SECONDARIES $92 (INKEY$) OR >
                BSR         LB26A           ; SYNTAX CHECK FOR A (
                LDA         ,S              ; GET TOKEN NUMBER
                CMPA        #2*17           ; CHECK FOR POINT COMMAND
                BCC         LB2C9           ; DO POINT COMMAND ($91)
; DO SECONDARIES $8E, $8F, $90 (LEFT$, RIGHT$, MID$)
                JSR         >LB156          ; EVALUATE FIRST STRING IN ARGUMENT
                BSR         LB26D           ; SYNTAX CHECK FOR A COMMA
                JSR         >LB146          ; TM ERROR IF NUMERIC VARiABLE
                PULS        A               ; GET TOKEN OFFSET FROM STACK
                LDU         FPA0+2          ; POINT U TO STRING DESCRIPTOR
                PSHS        U,A             ; SAVE TOKEN OFFSET AND DESCRIPTOR ADDRESS
                JSR         >LB70B          ; EVALUATE FIRST NUMERIC ARGUMENT
                PULS        A               ; GET TOKEN OFFSET FROM STACK
                PSHS        B,A             ; SAVE TOKEN OFFSET AND NUMERIC ARGUMENT
                FCB         $8E             ; OP CODE OF LDX# - SKlP 2 BYTES
LB2C7           BSR         LB262           ; SYNTAX CHECK FOR A (
LB2C9           PULS        B               ; GET TOKEN OFFSET
                LDX         COMVEC+8        ; GET SECONDARY FUNCTION JUMP TABLE ADDRESS
LB2CE           ABX                         ;  ADD IN COMMAND OFFSET

; HERE IS WHERE WE BRANCH TO A SECONDARY FUNCTION
                JSR         [,X]            ; GO DO AN SECONDARY FUNCTION
                JMP         >LB143          ; TM ERROR IF VARIABLE TYPE = STRING
; LOGICAL OPERATOR OR JUMPS HERE
LB2D4           FCB         SKP1LD          ; SKIP ONE BYTE - OR FLAG = $4F
; LOGICAL OPERATOR AND JUMPS HERE
LB2D5           CLRA                        ;  AND FLAG = 0
                STA         TMPLOC          ; AND/OR FLAG
                JSR         >INTCNV         ; CONVERT FPA0 INTO AN INTEGER IN ACCD
                STD         CHARAC          ; TEMP SAVE ACCD
                JSR         >LBC4A          ; MOVE FPA1 TO FPA0
                JSR         >INTCNV         ; CONVERT FPA0 INTO AN INTEGER IN ACCD
                TST         TMPLOC          ; CHECK AND/OR FLAG
                BNE         LB2ED           ; BRANCH IF OR
                ANDA        CHARAC          ; AND ACCD WITH FPA0 INTEGER
                ANDB        ENDCHR          ; STORED IN ENDCHR
                BRA         LB2F1           ; CONVERT TO FP
LB2ED           ORA         CHARAC          ; OR ACCD WITH FPA0 INTEGER
                ORB         ENDCHR          ; STORED IN CHARAC
LB2F1           JMP         >GIVABF         ; CONVERT THE VALUE IN ACCD INTO A FP NUMBER
; RELATIONAL COMPARISON PROCESS HANDLER
LB2F4           JSR         >LB148          ; TM ERROR IF TYPE MISMATCH
                BNE         LB309           ; BRANCH IF STRING VARIABLE
                LDA         FP1SGN          ; PACK THE MANTISSA
                ORA         #$7F            ; SIGN OF FPA1 INTO
                ANDA        FPA1            ; BIT 7 OF THE
                STA         FPA1            ; MANTISSA MS BYTE
                LDX         #FP1EXP         ; POINT X TO FPA1
                JSR         >LBC96          ; COMPARE FPA0 TO FPA1
                BRA         LB33F           ; CHECK TRUTH OF RELATIONAL COMPARISON
; RELATIONAL COMPARISON OF STRINGS
LB309           CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                DEC         TRELFL          ; REMOVE STRING TYPE FLAG (BIT0=1 FOR STRINGS) FROM THE
; DESIRED RELATIONAL COMPARISON DATA
                JSR         >LB657          ; GET LENGTH AND ADDRESS OF STRING WHOSE
; DESCRIPTOR ADDRESS IS IN THE BOTTOM OF FPA0
                STB         STRDES          ; SAVE LENGTH AND ADDRESS IN TEMPORARY
                STX         STRDES+2        ; DESCRIPTOR (STRING B)
                LDX         FPA1+2          ; RETURN LENGTH AND ADDRESS OF STRING
                JSR         >LB659          ; WHOSE DESCRIPTOR ADDRESS IS STORED IN FPA1+2
                LDA         STRDES          ; LOAD ACCA WITH LENGTH OF STRING B
                PSHS        B               ; SAVE LENGTH A ON STACK
                SUBA        ,S+             ; SUBTRACT LENGTH A FROM LENGTH B
                BEQ         LB328           ; BRANCH IF STRINGS OF EQUAL LENGTH
                LDA         #1              ; TRUE FLAG
                BCC         LB328           ; TRUE IF LENGTH B > LENGTH A
                LDB         STRDES          ; LOAD ACCB WITH LENGTH B
                NEGA                        ;  SET FLAG = FALSE (1FF)
LB328           STA         FP0SGN          ; SAVE TRUE/FALSE FLAG
                LDU         STRDES+2        ; POINT U TO START OF STRING
                INCB                        ;  COMPENSATE FOR THE DECB BELOW
; ENTER WITH ACCB CONTAINING LENGTH OF SHORTER STRING
LB32D           DECB                        ;  DECREMENT SHORTER STRING LENGTH
                BNE         LB334           ; BRANCH IF ALL OF STRING NOT COMPARED
                LDB         FP0SGN          ; GET TRUE/FALSE FLAB
                BRA         LB33F           ; CHECK TRUTH OF RELATIONAL COMPARISON
LB334           LDA         ,X+             ; GET A BYTE FROM STRING A
                CMPA        ,U+             ; COMPARE TO STRING B
                BEQ         LB32D           ; CHECK ANOTHER CHARACTER IF =
                LDB         #$FF            ; FALSE FLAG IF STRING A > B
                BCC         LB33F           ; BRANCH IF STRING A > STRING B
                NEGB                        ;  SET FLAG = TRUE
; DETERMINE TRUTH OF COMPARISON - RETURN RESULT IN FPA0
LB33F           ADDB        #1              ; CONVERT $FF,0,1 TO 0,1,2
                ROLB                        ;  NOW ITS 1,2,4 FOR > = <
                ANDB        RELFLG          ; AND THE ACTUAL COMPARISON WITH THE DESIRED COMPARISON
                BEQ         LB348           ; BRANCH IF FALSE (NO MATCHING BITS)
                LDB         #$FF            ; TRUE FLAG
LB348           JMP         >LBC7C          ; CONVERT ACCB INTO FP NUMBER IN FPA0
; DIM
LB34B           JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
DIM             LDB         #1              ; DIMENSION FLAG
                BSR         LB35A           ; SAVE ARRAY SPACE FOR THIS VARIABLE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BNE         LB34B           ; KEEP DIMENSIONING IF NOT END OF LINE
                RTS
; EVALUATE A VARIABLE - RETURN X AND
; VARPTR POINTING TO VARIABLE DESCRIPTOR
; EACH VARIABLE REQUIRES 7 BYTES - THE FIRST TWO
; BYTES ARE THE VARIABLE NAME AND THE NEXT 5
; BYTES ARE THE DESCRIPTOR. IF BIT 7 OF THE
; FIRST BYTE OF VARlABLE NAME IS SET, THE
; VARIABLE IS A DEF FN VARIABLE. IF BIT 7 OF
; THE SECOND BYTE OF VARIABLE NAME IS SET, THE
; VARIABLE IS A STRING, OTHERWISE THE VARIABLE
; IS NUMERIC.
; IF THE VARIABLE IS NOT FOUND, A ZERO VARIABLE IS
; INSERTED INTO THE VARIABLE SPACE
LB357           CLRB                        ;  DIMENSION FLAG = 0 DO NOT SET UP AN ARRAY
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
LB35A           STB         DIMFLG          ; SAVE ARRAY FLAG
LB35C           STA         VARNAM          ; SAVE INPUT CHARACTER
; ENTRY POINT FOR DEF FN VARIABLE SEARCH
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BSR         LB3A2           ; SET CARRY IF NOT ALPHA
                LBCS        LB277           ; SYNTAX ERROR IF NOT ALPHA
                CLRB                        ;  DEFAULT 2ND VARIABLE CHARACTER TO ZERO
                STB         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                JSR         GETNCH          ; GET ANOTHER CHARACTER FROM BASIC
                BCS         LB371           ; BRANCH IF NUMERIC (2ND CHARACTER IN
; VARIABLE MAY BE NUMERIC)
                BSR         LB3A2           ; SET CARRY IF NOT ALPHA
                BCS         LB37B           ; BRANCH IF NOT ALPHA
LB371           TFR         A,B             ; SAVE 2ND CHARACTER IN ACCB
; READ INPUT CHARACTERS UNTIL A NON ALPHA OR
; NON NUMERIC IS FOUND - IGNORE ALL CHARACTERS
; IN VARIABLE NAME AFTER THE 1ST TWO
LB373           JSR         GETNCH          ; GET AN INPUT CHARACTER
                BCS         LB373           ; BRANCH IF NUMERIC
                BSR         LB3A2           ; SET CARRY IF NOT ALPHA
                BCC         LB373           ; BRANCH IF ALPHA
LB37B           CMPA        #'$             ; CHECK FOR A STRING VARIABLE
                BNE         LB385           ; BRANCH IF IT IS NOT A STRING
                COM         VALTYP          ; SET VARIABLE TYPE TO STRING
                ADDB        #$80            ; SET BIT 7 OF 2ND CHARACTER (STRING)
                JSR         GETNCH          ; GET AN INPUT CHARACTER
LB385           STB         VARNAM+1        ; SAVE 2ND CHARACTER IN VARNAM+1
                ORA         ARYDIS          ; OR IN THE ARRAY DISABLE FLAG - IF = $80,
; DONT SEARCH FOR VARIABLES IN THE ARRAYS
                SUBA        #'(             ; IS THIS AN ARRAY VARIABLE?
                LBEQ        LEVARYV         ; BRANCH IF IT IS
                CLR         ARYDIS          ; RESET THE ARRAY DISABLE FLAG
                LDX         VARTAB          ; POINT X TO THE START OF VARIABLES
                LDD         VARNAM          ; GET VARIABLE IN QUESTION
LB395           CMPX        ARYTAB          ; COMPARE X TO THE END OF VARIABLES
                BEQ         LB3AB           ; BRANCH IF END OF VARIABLES
                CMPD        ,X++            ; COMPARE VARIABLE IN QUESTION TO CURRENT
                BEQ         LB3DC           ; VARIABLE AND BRANCH IF MATCH
                LEAX        5,X             ; MOVE POINTER TO NEXT VARIABLE AND
                BRA         LB395           ; KEEP LOOKING
; SET CARRY IF NOT UPPER CASE ALPHA
LB3A2           CMPA        #'A             ; CARRY SET IF < A
                BCS         LB3AA
                SUBA        #'Z+1
                SUBA        #-('Z+1)        ; CARRY CLEAR IF <= 'Z'
LB3AA           RTS
; PUT A NEW VARIABLE IN TABLE OF VARIABLES
LB3AB           LDX         #ZERO           ; POINT X TO ZERO LOCATION
                LDU         ,S              ; GET CURRENT RETURN ADDRESS
                CMPU        #LB287          ; DID WE COME FROM EVALUATE ALPHA EXPR?
                BEQ         LB3DE           ; YES - RETURN A ZERO VALUE
                LDD         ARYEND          ; GET END OF ARRAYS ADDRESS AND
                STD         V43             ; SAVE IT AT V43
                ADDD        #7              ; ADD 7 TO END OF ARRAYS (EACH
                STD         V41             ; VARIABLE = 7 BYTES) AND SAVE AT V41
                LDX         ARYTAB          ; GET END OF VARIABLES AND SAVE AT V47
                STX         V47
                JSR         >LAC1E          ; MAKE A SEVEN BYTE SLOT FOR NEW VARIABLE AT
; TOP OF VARIABLES
                LDX         V41             ; GET NEW END OF ARRAYS AND SAVE IT
                STX         ARYEND
                LDX         V45             ; GET NEW END OF VARIABLES AND SAVE IT
                STX         ARYTAB
                LDX         V47             ; GET OLD END OF VARIABLES
                LDD         VARNAM          ; GET NEW VARIABLE NAME
                STD         ,X++            ; SAVE VARIABLE NAME
                CLRA                        ;  ZERO OUT THE FP VALUE OF THE NUMERIC
                CLRB                        ;  VARIABLE OR THE LENGTH AND ADDRESS
                STD         ,X              ; OF A STRING VARIABLE
                STD         2,X
                STA         4,X
LB3DC           STX         VARPTR          ; STORE ADDRESS OF VARIABLE VALUE
LB3DE           RTS

LB3DF           FCB         $90,$80,$00,$00,$00 ; FLOATING POINT -32768
; SMALLEST SIGNED TWO BYTE INTEGER

LB3E4           JSR         GETNCH          ; GET AN INPUT CHARACTER FROM BASIC
LB3E6           JSR         >LB141          ; GO EVALUATE NUMERIC EXPRESSION
LB3E9           LDA         FP0SGN          ; GET FPA0 MANTISSA SIGN
                BMI         LB44A           ; FC ERROR IF NEGATIVE NUMBER
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
INTCNV          LDA         FP0EXP          ; GET FPA0 EXPONENT
                CMPA        #$90            ; COMPARE TO 32768 - LARGEST INTEGER
                BCS         LB3FB           ; EXPONENT AND BRANCH IF FPA0 < 32768
                LDX         #LB3DF          ; POINT X TO FP VALUE OF -32768
                JSR         >LBC96          ; COMPARE -32768 TO FPA0
                BNE         LB44A           ; 'FC' ERROR IF NOT =
LB3FB           JSR         >LBCC8          ; CONVERT FPA0 TO A TWO BYTE INTEGER
                LDD         FPA0+2          ; GET THE INTEGER
                RTS                         ; RETURN

; EVALUATE AN ARRAY VARIABLE
LEVARYV         LDB         DIMFLG          ; GET ARRAY FLAG
                LDA         VALTYP          ; GET VARIABLE TYPE
                PSHS        B,A             ; SAVE THEM ON THE STACK
                CLRB                        ;  RESET DIMENSION COUNTER
LB408           LDX         VARNAM          ; GET VARIABLE NAME
                PSHS        B,X             ; SAVE VARIABLE NAME AND DIMENSION COUNTER
                BSR         LB3E4           ; EVALUATE EXPRESSION (DIMENSION LENGTH)
                PULS        B,X,Y           ; PULL OFF VARIABLE NAME, DIMENSION COUNTER
; ARRAY FLAG
                STX         VARNAM          ; SAVE VARIABLE NAME AND VARIABLE TYPE
                LDU         FPA0+2          ; GET DIMENSION LENGTH
                PSHS        Y,U             ; SAVE DIMENSION LENGTH, ARRAY FLAG,VARIABLE TYPE
                INCB                        ;  INCREASE DIMENSION COUNTER
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #',             ; CHECK FOR ANOTHER DIMENSION
                BEQ         LB408           ; BRANCH IF MORE
                STB         TMPLOC          ; SAVE DIMENSION COUNTER
                JSR         >LB267          ; SYNTAX CHECK FOR A ")"
                PULS        A,B             ; RESTORE VARIABLE TYPE AND ARRAY
                STA         VALTYP          ; FLAG - LEAVE DIMENSION LENGTH ON STACK
                STB         DIMFLG
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; CONVERT FPA0 TO A SIGNED TWO BYTE INTEGER; RETURN VALUE IN ACCD
INTCNV          JSR         >LB143          ; TM ERROR IF STRING VARIABLE
                LDA         FP0EXP          ; GET FPA0 EXPONENT
                CMPA        #$90            ; COMPARE TO 32768 - LARGEST INTEGER EXPONENT AND
                BCS         LB3FE           ; BRANCH IF FPA0 < 32768
                LDX         #LB3DF          ; POINT X TO FP VALUE OF -32768
                JSR         >LBC96          ; COMPARE -32768 TO FPA0
                BNE         LB44A           ; FC ERROR IF NOT =
LB3FE           JSR         >LBCC8          ; CONVERT FPA0 TO A TWO BYTE INTEGER
                LDD         FPA0+2          ; GET THE INTEGER
                RTS
; EVALUATE AN ARRAY VARIABLE
LEVARYV         LDD         DIMFLG          ; GET ARRAY FLAG AND VARIABLE TYPE
                PSHS        B,A             ; SAVE THEM ON STACK
                NOP                         ;  DEAD SPACE CAUSED BY 1.2 REVISION
                CLRB                        ;  RESET DIMENSION COUNTER
LB40A           LDX         VARNAM          ; GET VARIABLE NAME
                PSHS        X,B             ; SAVE VARIABLE NAME AND DIMENSION COUNTER
                BSR         LB3E4           ; EVALUATE EXPRESSION (DIMENSlON LENGTH)
                PULS        B,X,Y           ; PULL OFF VARIABLE NAME, DIMENSlON COUNTER,
; ARRAY FLAG
                STX         VARNAM          ; SAVE VARIABLE NAME AND VARIABLE TYPE
                LDU         FPA0+2          ; GET DIMENSION LENGTH
                PSHS        U,Y             ; SAVE DIMENSION LENGTH, ARRAY FLAG, VARIABLE TYPE
                INCB                        ;  INCREASE DIMENSION COUNTER
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #',             ; CHECK FOR ANOTHER DIMENSION
                BEQ         LB40A           ; BRANCH IF MORE
                STB         TMPLOC          ; SAVE DIMENSION COUNTER
                JSR         >LB267          ; SYNTAX CHECK FOR A )
                PULS        A,B             ; RESTORE VARIABLE TYPE AND ARRAY
                STD         DIMFLG          ; FLAG - LEAVE DIMENSION LENGTH ON STACK
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                LDX         ARYTAB          ; GET START OF ARRAYS
LB42A           CMPX        ARYEND          ; COMPARE TO END OF ARRAYS
                BEQ         LB44F           ; BRANCH IF NO MATCH FOUND
                LDD         VARNAM          ; GET VARIABLE IN QUESTION
                CMPD        ,X              ; COMPARE TO CURRENT VARIABLE
                BEQ         LB43B           ; BRANCH IF =
                LDD         2,X             ; GET OFFSET TO NEXT ARRAY VARIABLE
                LEAX        D,X             ; ADD TO CURRENT POINTER
                BRA         LB42A           ; KEEP SEARCHING
LB43B           LDB         #2*9            ; REDIMENSIONED ARRAY ERROR
                LDA         DIMFLG          ; TEST ARRAY FLAG - IF <>0 YOU ARE TRYING
                BNE         LB44C           ; TO REDIMENSION AN ARRAY
                LDB         TMPLOC          ; GET NUMBER OF DIMENSIONS IN ARRAY
                CMPB        4,X             ; COMPARE TO THIS ARRAYS DIMENSIONS
                BEQ         LB4A0           ; BRANCH IF =
LB447           LDB         #8*2            ; BAD SUBSCRIPT
                FCB         SKP2            ; SKIP TWO BYTES
LB44A           LDB         #4*2            ; ILLEGAL FUNCTION CALL
LB44C           JMP         >LAC46          ; JUMP TO ERROR SERVICING ROUTINE
; INSERT A NEW ARRAY INTO ARRAY VARIABLES
; EACH SET OF ARRAY VARIABLES IS PRECEEDED BY A DE-
; SCRIPTOR BLOCK COMPOSED OF 5+2*N BYTES WHERE N IS THE
; NUMBER OF DIMENSIONS IN THE ARRAY. THE BLOCK IS DEFINED
; AS FOLLOWS: BYTES 0,1:VARIABLES NAME; 2,3:TOTAL LENGTH
; OF ARRAY ITEMS AND DESCRIPTOR BLOCK; 4:NUMBER OF DIMEN-
; ISIONS; 5,6:LENGTH OF DIMENSION 1; 7,8:LENGTH OF DIMEN-
; SION 2; 4+N,5+N:LENGTH OF DIMENSION N.
LB44F           LDD         #5              ; 5 BYTES/ARRAY ENTRY SAVE AT COEFPT
                STD         COEFPT
                LDD         VARNAM          ; GET NAME OF ARRAY AND SAVE IN
                STD         ,X              ; FIRST 2 BYTES OF DESCRIPTOR
                LDB         TMPLOC          ; GET NUMBER OF DIMENSIONS AND SAVE IN
                STB         4,X             ; 5TH BYTE OF DESCRIPTOR
                JSR         >LAC33          ; CHECK FOR ROOM FOR DESCRIPTOR IN FREE RAM
                STX         V41             ; TEMPORARILY SAVE DESCRIPTOR ADDRESS
LB461           LDB         #11             ; DEFAULT DIMENSION VALUE:X(10)
                CLRA                        ;
                TST         DIMFLG          ; CHECK ARRAY FLAG AND BRANCH IF
                BEQ         LB46D           ; NOT DIMENSIONING AN ARRAY
                PULS        A,B             ; GET DIMENSION LENGTH
                ADDD        #1              ; ADD ONE (X(0) HAS A LENGTH OF ONE)
LB46D           STD         5,X             ; SAVE LENGTH OF ARRAY DIMENSION
                BSR         LB4CE           ; MULTIPLY ACCUM ARRAY SIZE NUMBER LENGTH
; OF NEW DIMENSION
                STD         COEFPT          ; TEMP STORE NEW CURRENT ACCUMULATED ARRAY SIZE
                LEAX        2,X             ; BUMP POINTER UP TWO
                DEC         TMPLOC          ; DECREMENT DIMENSION COUNTER AND BRANCH IF
                BNE         LB461           ; NOT DONE WITH ALL DIMENSIONS
                STX         TEMPTR          ; SAVE ADDRESS OF (END OF ARRAY DESCRIPTOR - 5)
                ADDD        TEMPTR          ; ADD TOTAL SIZE OF NEW ARRAY
                LBCS        LAC44           ; OM ERROR IF > $FFFF
                TFR         D,X             ; SAVE END OF ARRAY IN X
                JSR         >LAC37          ; MAKE SURE THERE IS ENOUGH FREE RAM FOR ARRAY
                SUBD        #STKBUF-5       ; SUBTRACT OUT THE (STACK BUFFER - 5)
                STD         ARYEND          ; SAVE NEW END OF ARRAYS
                CLRA                        ;  ZERO TERMINATOR BYTE
LB48C           LEAX        -1,X            ; STORE TWO TERMINATOR BYTES AT
                STA         5,X             ; THE END OF THE ARRAY DESCRIPTOR
                CMPX        TEMPTR
                BNE         LB48C
                LDX         V41             ; GET ADDRESS OF START OF DESCRIPTOR
                LDA         ARYEND          ; GET MSB OF END OF ARRAYS; LSB ALREADY THERE
                SUBD        V41             ; SUBTRACT OUT ADDRESS OF START OF DESCRIPTOR
                STD         2,X             ; SAVE LENGTH OF (ARRAY AND DESCRIPTOR)
                LDA         DIMFLG          ; GET ARRAY FLAG AND BRANCH
                BNE         LB4CD           ; BACK IF DIMENSIONING
; CALCULATE POINTER TO CORRECT ELEMENT
LB4A0           LDB         4,X             ; GET THE NUMBER OF DIMENSIONS
                STB         TMPLOC          ; TEMPORARILY SAVE
                CLRA                        ;  INITIALIZE POINTER
                CLRB                        ;  TO ZERO
LB4A6           STD         COEFPT          ; SAVE ACCUMULATED POINTER
                PULS        A,B             ; PULL DIMENSION ARGUMENT OFF THE
                STD         FPA0+2          ; STACK AND SAVE IT
                CMPD        5,X             ; COMPARE TO STORED DIM ARGUMENT
                BCC         LB4EB           ; BS ERROR IF > = "DIM" ARGUMENT
                LDU         COEFPT          ; GET ACCUMULATED POINTER AND
                BEQ         LB4B9           ; BRANCH IF 1ST DIMENSION
                BSR         LB4CE           ; MULTIPLY ACCUMULATED POINTER AND DIMENSION
                ADDD        FPA0+2          ; LENGTH AND ADD TO CURRENT ARGUMENT
LB4B9           LEAX        2,X             ; MOVE POINTER TO NEXT DIMENSION
                DEC         TMPLOC          ; DECREMENT DIMENSION COUNTER AND
                BNE         LB4A6           ; BRANCH IF ANY DIMENSIONS LEFT
; MULTIPLY ACCD BY 5 - 5 BYTES/ARRAY VALUE
                STD         ,--S
                ASLB                        ;
                ROLA                        ;  TIMES 2
                ASLB                        ;
                ROLA                        ;  TIMES 4
                ADDD        ,S++            ; TIMES 5
                LEAX        D,X             ; ADD OFFSET TO START OF ARRAY
                LEAX        5,X             ; ADJUST POINTER FOR SIZE OF DESCRIPTOR
                STX         VARPTR          ; SAVE POINTER TO ARRAY VALUE
LB4CD           RTS
; MULTIPLY 2 BYTE NUMBER IN 5,X BY THE 2 BYTE NUMBER IN COEFPT. RETURN RESULT IN ACCD, BS ERROR IF > $FFFF
LB4CE           LDA         #16             ; 16 SHIFTS TO DO A MULTIPLY
                STA         V45             ; SHIFT COUNTER
                LDD         5,X             ; GET SIZE OF DIMENSION
                STD         BOTSTK          ; AND SAVE IT
                CLRA                        ;  ZERO
                CLRB                        ;  ACCD
LB4D8           ASLB                        ;  SHIFT ACCB LEFT
                ROLA                        ;  ONE BIT
                BCS         LB4EB           ; 'BS' ERROR IF CARRY
                ASL         COEFPT+1        ; SHIFT MULTIPLICAND LEFT ONE
                ROL         COEFPT          ; BIT - ADD MULTIPLIER TO ACCUMULATOR
                BCC         LB4E6           ; IF CARRY <> 0
                ADDD        BOTSTK          ; ADD MULTIPLIER TO ACCD
                BCS         LB4EB           ; 'BS' ERROR IF CARRY (>$FFFF)
LB4E6           DEC         V45             ; DECREMENT SHIFT COUNTER
                BNE         LB4D8           ; IF NOT DONE
                RTS
LB4EB           JMP         >LB447          ; 'BS' ERROR

; MEM
; THIS IS NOT A TRUE INDICATOR OF FREE MEMORY BECAUSE
; BASIC REQUIRES A STKBUF SIZE BUFFER FOR THE STACK
; FOR WHICH MEM DOES NOT ALLOW.

MEM             TFR         S,D             ; PUT STACK POINTER INTO ACCD
                SUBD        ARYEND          ; SUBTRACT END OF ARRAYS
                FCB         SKP1            ; SKIP ONE BYTE
; THE VALUE IN ACCB INTO A FP NUMBER IN FPA0
LB4F3           CLRA                        ;  CLEAR MS BYTE OF ACCD
; CONVERT THE VALUE IN ACCD INTO A FLOATING POINT NUMBER IN FPA0
GIVABF          CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                STD         FPA0            ; SAVE ACCD IN TOP OF FACA
                LDB         #$90            ; EXPONENT REQUIRED IF THE TOP TWO BYTES
; OF FPA0 ARE TO BE TREATED AS AN INTEGER IN FPA0
                JMP         >LBC82          ; CONVERT THE REST OF FPA0 TO AN INTEGER
; STR$
STR             JSR         >LB143          ; 'TM' ERROR IF STRING VARIABLE
                LDU         #STRBUF+2       ; CONVERT FP NUMBER TO ASCII STRING IN
                JSR         >LBDDC          ; THE STRING BUFFER
                LEAS        2,S             ; PURGE THE RETURN ADDRESS FROM THE STACK
                LDX         #STRBUF+1       ; POINT X TO STRING BUFFER AND SAVE
                BRA         LB518           ; THE STRING IN THE STRING SPACE
; RESERVE ACCB BYTES OF STRING SPACE. RETURN START
; ADDRESS IN (X) AND FRESPC
LB50D           STX         V4D             ; SAVE X IN V4D
LB50F           BSR         LB56D           ; RESERVE ACCB BYTES IN STRING SPACE
LB511           STX         STRDES+2        ; SAVE NEW STRING ADDRESS
                STB         STRDES          ; SAVE LENGTH OF RESERVED BLOCK
                RTS
LB516           LEAX        -1,X            ; MOVE POINTER BACK ONE
; SCAN A LINE FROM (X) UNTIL AN END OF LINE FLAG (ZERO) OR
; EITHER OF THE TWO TERMINATORS STORED IN CHARAC OR ENDCHR IS MATCHED.
; THE RESULTING STRING IS STORED IN THE STRING SPACE
; ONLY IF THE START OF THE STRING IS <= STRBUF+2
LB518           LDA         #'"             ; INITIALIZE
LB51A           STA         CHARAC          ; TERMINATORS
                STA         ENDCHR          ; TO "
LB51E           LEAX        1,X             ; MOVE POINTER UP ONE
                STX         RESSGN          ; TEMPORARILY SAVE START OF STRING
                STX         STRDES+2        ; SAVE START OF STRING IN TEMP DESCRIPTOR
                LDB         #-1             ; INITIALIZE CHARACTER COUNTER TO - 1
LB526           INCB                        ;  INCREMENT CHARACTER COUNTER
                LDA         ,X+             ; GET CHARACTER
                BEQ         LB537           ; BRANCH IF END OF LINE
                CMPA        CHARAC          ; CHECK FOR TERMINATORS
                BEQ         LB533           ; IN CHARAC AND ENDCHR
                CMPA        ENDCHR          ; DONT MOVE POINTER BACK
                BNE         LB526           ; ONE IF TERMINATOR IS "MATCHED"
LB533           CMPA        #'"             ; COMPARE CHARACTER TO STRING DELIMITER
                BEQ         LB539           ; & DONT MOVE POINTER BACK IF SO
LB537           LEAX        -1,X            ; MOVE POINTER BACK ONE
LB539           STX         COEFPT          ; SAVE END OF STRING ADDRESS
                STB         STRDES          ; SAVE STRING LENGTH IN TEMP DESCRIPTOR
                LDU         RESSGN          ; GET INITlAL STRING START
                CMPU        #STRBUF+2       ; COMPARE TO START OF STRING BUFFER
LB543           BHI         LB54C           ; BRANCH IF > START OF STRING BUFFER
                BSR         LB50D           ; GO RESERVE SPACE FOR THE STRING
                LDX         RESSGN          ; POINT X TO THE BEGINNING OF THE STRING
                JSR         >LB645          ; MOVE (B) BYTES FROM (X) TO
; [FRESPC] - MOVE STRING DATA
; PUT DIRECT PAGE STRING DESCRIPTOR BUFFER DATA
; ON THE STRING STACK. SET VARIABLE TYPE TO STRING
LB54C           LDX         TEMPPT          ; GET NEXT AVAILABLE STRING STACK DESCRIPTOR
                CMPX        #CFNBUF         ; COMPARE TO TOP OF STRING DESCRIPTOR STACK
                BNE         LB558           ; FORMULA O.K.
                LDB         #15*2           ; 'STRING FORMULA TOO COMPLEX' ERROR
LB555           JMP         >LAC46          ; JUMP TO ERROR SERVICING ROUTINE
LB558           LDA         STRDES          ; GET LENGTH OF STRING AND SAVE IT
                STA         0,X             ; IN BYTE 0 OF DESCRIPTOR
                LDD         STRDES+2        ; GET START ADDRESS OF ACTUAL STRING
                STD         2,X             ; AND SAVE IN BYTES 2,3 OF DESCRIPTOR
                LDA         #$FF            ; VARIABLE TYPE = STRING
                STA         VALTYP          ; SAVE IN VARIABLE TYPE FLAG
                STX         LASTPT          ; SAVE START OF DESCRIPTOR
                STX         FPA0+2          ; ADDRESS IN LASTPT AND FPA0
                LEAX        5,X             ; 5 BYTES/STRING DESCRIPTOR
                STX         TEMPPT          ; NEXT AVAILABLE STRING VARIABLE DESCRIPTOR
                RTS
; RESERVE ACCB BYTES IN STRING STORAGE SPACE
; RETURN WITH THE STARTING ADDRESS OF THE
; RESERVED STRING SPACE IN (X) AND FRESPC
LB56D           CLR         GARBFL          ; CLEAR STRING REORGANIZATION FLAG
LB56F           CLRA                        ;  PUSH THE LENGTH OF THE
                PSHS        B,A             ; STRING ONTO THE STACK
                LDD         STRTAB          ; GET START OF STRING VARIABLES
                SUBD        ,S+             ; SUBTRACT STRING LENGTH
                CMPD        FRETOP          ; COMPARE TO START OF STRING STORAGE
                BCS         LB585           ; IF BELOW START, THEN REORGANIZE
                STD         STRTAB          ; SAVE NEW START OF STRING VARIABLES
                LDX         STRTAB          ; GET START OF STRING VARIABLES
                LEAX        1,X             ; ADD ONE
                STX         FRESPC          ; SAVE START ADDRESS OF NEWLY RESERVED SPACE
                PULS        B,PC            ; RESTORE NUMBER OF BYTES RESERVED AND RETURN
LB585           LDB         #2*13           ; 'OUT OF STRING SPACE' ERROR
                COM         GARBFL          ; TOGGLE REORGANIZATiON FLAG
                BEQ         LB555           ; ERROR IF FRESHLY REORGANIZED
                BSR         LB591           ; GO REORGANIZE STRING SPACE
                PULS        B               ; GET BACK THE NUMBER OF BYTES TO RESERVE
                BRA         LB56F           ; TRY TO RESERVE ACCB BYTES AGAIN
; REORGANIZE THE STRING SPACE
LB591           LDX         MEMSIZ          ; GET THE TOP OF STRING SPACE
LB593           STX         STRTAB          ; SAVE TOP OF UNORGANIZED STRING SPACE
                CLRA                        ;  ZERO OUT ACCD
                CLRB                        ;  AND RESET VARIABLE
                STD         V4B             ; POINTER TO 0
                LDX         FRETOP          ; POINT X TO START OF STRING SPACE
                STX         V47             ; SAVE POINTER IN V47
                LDX         #STRSTK         ; POINT X TO START OF STRING DESCRIPTOR STACK
LB5A0           CMPX        TEMPPT          ; COMPARE TO ADDRESS OF NEXT AVAILABLE DESCRIPTOR
                BEQ         LB5A8           ; BRANCH IF TOP OF STRING STACK
                BSR         LB5D8           ; CHECK FOR STRING IN UNORGANIZED STRING SPACE
                BRA         LB5A0           ; KEEP CHECKING
LB5A8           LDX         VARTAB          ; GET THE END OF BASIC PROGRAM
LB5AA           CMPX        ARYTAB          ; COMPARE TO END OF VARIABLES
                BEQ         LB5B2           ; BRANCH IF AT TOP OF VARIABLES
                BSR         LB5D2           ; CHECK FOR STRING IN UNORGANIZED STRING SPACE
                BRA         LB5AA           ; KEEP CHECKING VARIABLES
LB5B2           STX         V41             ; SAVE ADDRESS OF THE END OF VARIABLES
LB5B4           LDX         V41             ; GET CURRENT ARRAY POINTER
LB5B6           CMPX        ARYEND          ; COMPARE TO THE END OF ARRAYS
                BEQ         LB5EF           ; BRANCH IF AT END OF ARRAYS
                LDD         2,X             ; GET LENGTH OF ARRAY AND DESCRIPTOR
                ADDD        V41             ; ADD TO CURRENT ARRAY POINTER
                STD         V41             ; AND SAVE IT
                LDA         1,X             ; GET 1ST CHARACTER OF VARIABLE NAME
                BPL         LB5B4           ; BRANCH IF NUMERIC ARRAY
                LDB         4,X             ; GET THE NUMBER OF DIMENSIONS IN THIS ARRAY
                ASLB                        ;  MULTIPLY BY 2
                ADDB        #5              ; ADD FIVE BYTES (VARIABLE NAME, ARRAY
; LENGTH, NUMBER DIMENSIONS)
                ABX                         ;  X NOW POINTS TO START OF ARRAY ELEMENTS
LB5CA           CMPX        V41             ; AT END OF THIS ARRAY?
                BEQ         LB5B6           ; YES - CHECK FOR ANOTHER
                BSR         LB5D8           ; CHECK FOR STRING LOCATED IN
; UNORGANIZED STRING SPACE
                BRA         LB5CA           ; KEEP CHECKING ELEMENTS IN THIS ARRAY
LB5D2           LDA         1,X             ; GET F1RST BYTE OF VARIABLE NAME
                LEAX        2,X             ; MOVE POINTER TO DESCRIPTOR
                BPL         LB5EC           ; BRANCH IF VARIABLE IS NUMERIC
; SEARCH FOR STRING - ENTER WITH X POINTING TO
; THE STRING DESCRIPTOR. IF STRING IS STORED
; BETWEEN V47 AND STRTAB, SAVE DESCRIPTOR POINTER
; IN V4B AND RESET V47 TO STRING ADDRESS
LB5D8           LDB         ,X              ; GET THE LENGTH OF THE STRING
                BEQ         LB5EC           ; BRANCH IF NULL - NO STRING
                LDD         2,X             ; GET STARTING ADDRESS OF THE STRING
                CMPD        STRTAB          ; COMPARE TO THE START OF STRING VARIABLES
                BHI         LB5EC           ; BRANCH IF THIS STRING IS STORED IN
; THE STRING VARIABLES
                CMPD        V47             ; COMPARE TO START OF STRING SPACE
                BLS         LB5EC           ; BRANCH IF NOT STORED IN THE STRING SPACE
                STX         V4B             ; SAVE VARIABLE POINTER IF STORED IN STRING SPACE
                STD         V47             ; SAVE STRING STARTING ADDRESS
LB5EC           LEAX        5,X             ; MOVE TO NEXT VARIABLE DESCRIPTOR
LB5EE           RTS
LB5EF           LDX         V4B             ; GET ADDRESS OF THE DESCRIPTOR FOR THE
; STRING WHICH IS STORED IN THE HIGHEST RAM ADDRESS IN
; THE UNORGANIZED STRING SPACE
                BEQ         LB5EE           ; BRANCH IF NONE FOUND AND REORGANIZATION DONE
                CLRA                        ;  CLEAR MS BYTE OF LENGTH
                LDB         ,X              ; GET LENGTH OF STRING
                DECB                        ;  SUBTRACT ONE
                ADDD        V47             ; ADD LENGTH OF STRING TO ITS STARTING ADDRESS
                STD         V43             ; SAVE AS MOVE STARTING ADDRESS
                LDX         STRTAB          ; POINT X TO THE START OF ORGANIZED STRING VARIABLES
                STX         V41             ; SAVE AS MOVE ENDING ADDRESS
                JSR         >LAC20          ; MOVE STRING FROM CURRENT POSITION TO THE
; TOP OF UNORGANIZED STRING SPACE
                LDX         V4B             ; POINT X TO STRING DESCRIPTOR
                LDD         V45             ; GET NEW STARTING ADDRESS OF STRING AND
                STD         2,X             ; SAVE IT IN DESCRIPTOR
                LDX         V45             ; GET NEW TOP OF UNORGANIZED STRING SPACE
                LEAX        -1,X            ; MOVE POINTER BACK ONE
                JMP         >LB593          ; JUMP BACK AND REORGANIZE SOME MORE
; CONCATENATE TWO STRINGS
LB60F           LDD         FPA0+2          ; GET DESCRIPTOR ADDRESS OF STRING A
                PSHS        B,A             ; AND SAVE IT ON THE STACK
                JSR         >LB223          ; GET DESCRIPTOR ADDRESS OF STRING B
                JSR         >LB146          ; 'TM' ERROR IF NUMERIC VARIABLE
                PULS        X               ; POINT X TO STRING A DESCRIPTOR
                STX         RESSGN          ; ADDRESS AND SAVE IT IN RESSGN
                LDB         ,X              ; GET LENGTH OF STRING A
                LDX         FPA0+2          ; POINT X TO DESCRIPTOR OF STRING B
                ADDB        ,X              ; ADD LENGTH OF STRING B TO STR1NG A
                BCC         LB62A           ; BRANCH IF LENGTH < 256
                LDB         #2*14           ; 'STRING TOO LONG' ERROR IF LENGTH > 255
                JMP         >LAC46          ; JUMP TO ERROR SERVICING ROUTINE
LB62A           JSR         >LB50D          ; RESERVE ROOM IN STRING SPACE FOR NEW STRING
                LDX         RESSGN          ; GET DESCRIPTOR ADDRESS OF STRING A
                LDB         ,X              ; GET LENGTH OF STRING A
                BSR         LB643           ; MOVE STRING A INTO RESERVED BUFFER IN STRING SPACE
                LDX         V4D             ; GET DESCRIPTOR ADDRESS OF STRING B
                BSR         LB659           ; GET LENGTH AND ADDRESS OF STRING B
                BSR         LB645           ; MOVE STRING B INTO REST OF RESERVED BUFFER
                LDX         RESSGN          ; POINT X TO DESCRIPTOR OF STRING A
                BSR         LB659           ; DELETE STRING A IF LAST STRING ON STRING STACK
                JSR         >LB54C          ; PUT STRING DESCRIPTOR ON THE STRING STACK
                JMP         >LB168          ; BRANCH BACK TO EXPRESSION EVALUATION
; MOVE (B) BYTES FROM 2,X TO FRESPC
LB643           LDX         2,X             ; POINT X TO SOURCE ADDRESS
LB645           LDU         FRESPC          ; POINT U TO DESTINATION ADDRESS
                INCB                        ;  COMPENSATION FOR THE DECB BELOW
                BRA         LB64E           ; GO MOVE THE BYTES
; MOVE B BYTES FROM (X) TO (U)
LB64A           LDA         ,X+             ; GET A SOURCE BYTE AND MOVE IT
                STA         ,U+             ; TO THE DESTINATION
LB64E           DECB                        ;  DECREMENT BYTE COUNTER
                BNE         LB64A           ; BRANCH IF ALL BYTES NOT MOVED
                STU         FRESPC          ; SAVE ENDING ADDRESS IN FRESPC
                RTS
; RETURN LENGTH (ACCB) AND ADDRESS (X) OF
; STRING WHOSE DESCRIPTOR IS IN FPA0+2
; DELETE THE STRING IF IT IS THE LAST ONE
; PUT ON THE STRING STACK. REMOVE STRING FROM STRING
; SPACE IF IT IS AT THE BOTTOM OF STRING VARIABLES.
LB654           JSR         >LB146          ; 'TM' ERROR IF VARIABLE TYPE = NUMERIC
LB657           LDX         FPA0+2          ; GET ADDRESS OF SELECTED STRING DESCRIPTOR
LB659           LDB         ,X              ; GET LENGTH OF STRING
                BSR         LB675           ; CHECK TO SEE IF THIS STRING DESCRIPTOR WAS
                BNE         LB672           ; THE LAST ONE PUT ON THE STRING STACK AND
; BRANCH IF NOT
                LDX         5+2,X           ; GET START ADDRESS OF STRING JUST REMOVED
                LEAX        -1,X            ; MOVE POINTER DOWN ONE
                CMPX        STRTAB          ; COMPARE TO START OF STRING VARIABLES
                BNE         LB66F           ; BRANCH IF THIS STRING IS NOT AT THE BOTTOM
; OF STRING VARIABLES
                PSHS        B               ; SAVE LENGTH; ACCA WAS CLEARED
                ADDD        STRTAB          ; ADD THE LENGTH OF THE JUST REMOVED STRING
                STD         STRTAB          ; TO THE START OF STRING VARIABLES - THIS WILL
; REMOVE THE STRING FROM THE STRING SPACE
                PULS        B               ; RESTORE LENGTH
LB66F           LEAX        1,X             ; ADD ONE TO POINTER
                RTS
LB672           LDX         2,X             ; POINT X TO ADDRESS OF STRING NOT
                RTS         *ON             ; THE STRING STACK
; REMOVE STRING FROM STRING STACK. ENTER WITH X
; POINTING TO A STRING DESCRIPTOR - DELETE THE
; STRING FROM STACK IF IT IS ON TOP OF THE
; STACK. IF THE STRING IS DELETED, SET THE ZERO FLAG
LB675           CMPX        LASTPT          ; COMPARE TO LAST USED DESCRIPTOR ADDRESS
                BNE         LB680           ; ON THE STRING STACK, RETURN IF DESCRIPTOR
; ADDRESS NOT ON THE STRING STACK
                STX         TEMPPT          ; SAVE LAST USED DESCRIPTOR AS NEXT AVAILABLE
                LEAX        -5,X            ; MOVE LAST USED DESCRIPTOR BACK 5 BYTES
                STX         LASTPT          ; AND SAVE AS THE LAST USED DESCRIPTOR ADDR
                CLRA                        ;  SET ZERO FLAG
LB680           RTS
; LEN
LEN             BSR         LB686           ; POINT X TO PROPER STRING AND GET LENGTH
LB683           JMP         >LB4F3          ; CONVERT ACCB TO FP NUMBER IN FPA0
; POINT X TO STRING ADDRESS LOAD LENGTH INTO
; ACCB. ENTER WITH THE STRING DESCRIPTOR IN
; BOTTOM TWO BYTES OF FPA0
LB686           BSR         LB654           ; GET LENGTH AND ADDRESS OF STRING
                CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                TSTB                        ;  SET FLAGS ACCORDING TO LENGTH
                RTS
; CHR$
CHR             JSR         >LB70E          ; CONVERT FPA0 TO AN INTEGER IN ACCD
LB68F           LDB         #1              ; RESERVE ONE BYTE IN
                JSR         >LB56D          ; THE STRING SPACE
                LDA         FPA0+3          ; GET ASCII STRING VALUE
                JSR         >LB511          ; SAVE RESERVED STRING DESCRIPTOR IN TEMP DESCRIPTOR
                STA         ,X              ; SAVE THE STRING (ITS ONLY ONE BYTE)
LB69B           LEAS        2,S             ; PURGE THE RETURN ADDRESS OFF OF THE STACK
LB69D           JMP         >LB54C          ; PUT TEMP DESCRIPTOR DATA ONTO STRING STACK
; ASC$
ASC             BSR         LB6A4           ; PUT 1ST CHARACTER OF STRING INTO ACCB
                BRA         LB683           ; CONVERT ACCB INTO FP NUMBER IN FPA0
LB6A4           BSR         LB686           ; POINT X TO STRING DESCRIPTOR
                BEQ         LB706           ; 'FC' ERROR IF NULL STRING
                LDB         ,X              ; GET FIRST BYTE OF STRING
                RTS
; LEFT$
LEFT            BSR         LB6F5           ; GET ARGUMENTS FROM STACK
LB6AD           CLRA                        ;  CLEAR STRING POINTER OFFSET - OFFSET = 0 FOR LEFT$
LB6AE           CMPB        ,X              ; COMPARE LENGTH PARAMETER TO LENGTH OF
                BLS         LB6B5           ; STRING AND BRANCH IF LENGTH OF STRING >= LENGTH PARAMETER
                LDB         ,X              ; USE LENGTH OF STRING OTHERWISE
                CLRA                        ;  CLEAR STRING POINTER OFFSET (0 FOR LEFT$)
LB6B5           PSHS        B,A             ; PUSH PARAMETERS ONTO STACK
                JSR         >LB50F          ; RESERVE ACCB BYTES IN THE STRING SPACE
                LDX         V4D             ; POINT X TO STRING DESCRIPTOR
                BSR         LB659           ; GET ADDRESS OF OLD STRING (X=ADDRESS)
                PULS        B               ; PULL STRING POINTER OFFSET OFF OF THE STACK
                ABX                         ;  AND ADD IT TO STRING ADDRESS
                PULS        B               ; PULL LENGTH PARAMETER OFF OF THE STACK
                JSR         >LB645          ; MOVE ACCB BYTES FROM (X) TO [FRESPC]
                BRA         LB69D           ; PUT TEMP STRING DESCRIPTOR ONTO THE STRING STACK
; RIGHT$
RIGHT           BSR         LB6F5           ; GET ARGUMENTS FROM STACK
                SUBA        ,X              ; ACCA=LENGTH PARAMETER - LENGTH OF OLD STRING
                NEGA                        ;  NOW ACCA = LENGTH OF OLD STRING
                BRA         LB6AE           ; PUT NEW STRING IN THE STRING SPACE
; MID$
MID             LDB         #$FF            ; GET DEFAULT VALUE OF LENGTH AND
                STB         FPA0+3          ; SAVE IT IN FPA0
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                CMPA        #')             ; ARGUMENT DELIMITER?
                BEQ         LB6DE           ; YES - NO LENGTH PARAMETER GIVEN
                JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
                BSR         LB70B           ; EVALUATE NUMERIC EXPRESSION (LENGTH)
LB6DE           BSR         LB6F5           ; GET ARGUMENTS FROM STACK
                BEQ         LB706           ; 'FC' ERROR IF NULL STRING
                CLRB                        ;  CLEAR LENGTH COUNTER (DEFAULT VALUE)
                DECA                        ;  SUOTRACT ONE FROM POSITION PARAMETER (THESE
                CMPA        ,X              ; ROUTINES EXPECT 1ST POSITION TO BE ZERO, NOT ONE)
; AND COMPARE IT TO LENGTH OF OLD STRING
                BCC         LB6B5           ; IF POSITION > LENGTH OF OLD STRING, THEN NEW
; STRING WILL BE A NULL STRING
                TFR         A,B             ; SAVE ABSOLUTE POSITION PARAMETER IN ACCB
                SUBB        ,X              ; ACCB=POSITION-LENGTH OF OLD STRING
                NEGB                        ;  NOW ACCB=LENGTH OF OLDSTRING-POSITION
                CMPB        FPA0+3          ; IF THE AMOUNT OF OLD STRING TO THE RIGHT OF
                BLS         LB6B5           ; POSITION IS <= THE LENGTH PARAMETER, BRANCH AND
; ALL OF THE STRING TO THE RIGHT OF THE POSITION
; OF THE LENGTH PARAMETER
                LDB         FPA0+3          ; GET LENGTH OF NEW STRING
                BRA         LB6B5           ; PUT NEW STRING IN STRING SPACE
; DO A SYNTAX CHECK FOR ")", THEN PULL THE PREVIOUSLY CALCULATED NUMERIC
; ARGUMENT (ACCD) AND STRING ARGUMENT DESCRIPTOR ADDR OFF OF THE STACK
LB6F5           JSR         >LB267          ; SYNTAX CHECK FOR A ")"
                LDU         ,S              ; LOAD THE RETURN ADDRESS INTO U REGISTER
                LDX         5,S             ; GET ADDRESS OF STRING AND
                STX         V4D             ; SAVE IT IN V4D
                LDA         4,S             ; PUT LENGTH OF STRING IN
                LDB         4,S             ; BOTH ACCA AND ACCB
                LEAS        7,S             ; REMOVE DESCRIPTOR AND RETURN ADDRESS FROM STACK
                TFR         U,PC            ; JUMP TO ADDRESS IN U REGISTER
LB706           JMP         >LB44A          ; 'ILLEGAL FUNCTION CALL'
; EVALUATE AN EXPRESSION - RETURN AN INTEGER IN
; ACCB - 'FC' ERROR IF EXPRESSION > 255
LB709           JSR         GETNCH          ; GET NEXT BASIC INPUT CHARACTER
EVALEXPB        EQU         *
LB70B           JSR         >LB141          ; EVALUATE A NUMERIC EXPRESSION
LB70E           JSR         >LB3E9          ; CONVERT FPA0 TO INTEGER IN ACCD
                TSTA                        ;  TEST MS BYTE OF INTEGER
                BNE         LB706           ; 'FC' ERROR IF EXPRESSION > 255
                JMP         GETCCH          ; GET CURRENT INPUT CHARACTER FROM BASIC
; VAL
VAL             JSR         >LB686          ; POINT X TO STRING ADDRESS
                LBEQ        LBA39           ; IF NULL STRING SET FPA0
                LDU         CHARAD          ; SAVE INPUT POINTER IN REGISTER U
                STX         CHARAD          ; POINT INPUT POINTER TO ADDRESS OF STRING
                ABX                         ;  MOVE POINTER TO END OF STRING TERMINATOR
                LDA         ,X              ; GET LAST BYTE OF STRING
                PSHS        U,X,A           ; SAVE INPUT POINTER, STRING TERMINATOR ADDRESS AND CHARACTER
                CLR         ,X              ; CLEAR STRING TERMINATOR : FOR ASCII - FP CONVERSION
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                JSR         >LBD12          ; CONVERT AN ASCII STRING TO FLOATING POINT
                PULS        A,X,U           ; RESTORE CHARACTERS AND POINTERS
                STA         ,X              ; REPLACE STRING TERMINATOR
                STU         CHARAD          ; RESTORE INPUT CHARACTER
                RTS
LB734           BSR         LB73D           ; EVALUATE AN EXPRESSION, RETURN
                STX         BINVAL          ; THE VALUE IN X; STORE IT IN BINVAL
LB738           JSR         >LB26D          ; SYNTAX CHECK FOR A COMMA
                BRA         LB70B           ; EVALUATE EXPRESSION IN RANGE 0 <= X < 256
; EVALUATE EXPRESSION : RETURN INTEGER PORTION IN X - 'FC' ERROR IF
; EXPRESSION IS NEGATIVE OR > 32767, I.E. NOT A LEGAL POSITIVE INTEGER.
LB73D           JSR         >LB141          ; EVALUATE NUMERIC EXPRESSION
LB740           LDA         FP0SGN          ; GET SIGN OF FPA0 MANTISSA
                BMI         LB706           ; 'ILLEGAL FUNCTION CALL' IF NEGATIVE
                LDA         FP0EXP          ; GET EXPONENT OF FPA0
                CMPA        #$90            ; COMPARE TO LARGEST POSITIVE INTEGER
                BHI         LB706           ; 'ILLEGAL FUNCTION CALL' IF TOO LARGE
                JSR         >LBCC8          ; SHIFT BINARY POINT TO EXTREME RIGHT OF FPA0
                LDX         FPA0+2          ; LOAD X WITH LOWER TWO BYTES OF FPA0
                RTS
; PEEK
PEEK            BSR         LB740           ; CONVERT FPA0 TO INTEGER IN REGISTER X
                LDB         ,X              ; GET THE VALUE BEING 'PEEK'ED
                JMP         >LB4F3          ; CONVERT ACCB INTO A FP NUMBER
; POKE
POKE            BSR         LB734           ; EVALUATE 2 EXPRESSIONS
                LDX         BINVAL          ; GET THE ADDRESS TO BE 'POKE'ED
                STB         ,X              ; STORE THE DATA IN THAT ADDRESS
                RTS
; LLIST
LLIST           LDB         #-2             ; SET DEVICE NUMBER TO
                STB         DEVNUM          ; PRINTER
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
; LIST
LIST            PSHS        CC              ; SAVE ZERO FLAG ON STACK
                JSR         >LAF67          ; CONVERT DECIMAL LINE NUMBER TO BINARY
                JSR         >LAD01          ; FIND RAM ADDRESS OF THAT LINE NUMBER AND
                STX         LSTTXT          ; SAVE IT IN LSTTXT
                PULS        CC              ; GET ZERO FLAG FROM STACK
                BEQ         LB784           ; BRANCH IF END OF LINE
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                BEQ         LB789           ; BRANCH IF END OF LINE
                CMPA        #$AC            ; MINUS TOKEN (IS IT A RANGE OF LINE NUMBERS?)
                BNE         LB783           ; NO - RETURN
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                BEQ         LB784           ; BRANCH IF END OF LINE
                JSR         >LAF67          ; GET ENDING LINE NUMBER
                BEQ         LB789           ; BRANCH IF LEGAL LINE NUMBER
LB783           RTS
; LIST THE ENTIRE PROGRAM
LB784           LDU         #$FFFF          ; SET THE DEFAULT ENDING LINE NUMBER
                STU         BINVAL          ; TO $FFFF
LB789           LEAS        2,S             ; PURGE RETURN ADDRESS FROM THE STACK
                LDX         LSTTXT          ; POINT X TO STARTING LINE ADDRESS
LB78D           JSR         >LB95C          ; MOVE CURSOR TO START OF A NEW LINE
                JSR         >LA549          ; CHECK FOR A BREAK OR PAUSE
                LDD         ,X              ; GET ADDRESS OF NEXT BASIC LINE
                BNE         LB79F           ; BRANCH IF NOT END OF PROGRAM
LB797           JSR         >LA42D          ; CHECK CLOSE FILE HANDLER
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JMP         >LAC73          ; RETURN TO BASICS MAIN INPUT LOOP
LB79F           STX         LSTTXT          ; SAVE NEW STARTING LINE ADDRESS
                LDD         2,X             ; GET THE LINE NUMBER OF THIS LINE AND
                CMPD        BINVAL          ; COMPARE IT TO ENDING LINE NUMBER
                BHI         LB797           ; EXIT IF LINE NUMBER > ENDING LINE NUMBER
                JSR         >LBDCC          ; PRINT THE NUMBER IN ACCD ON SCREEN IN DECIMAL
                JSR         >LB9AC          ; SEND A SPACE TO CONSOLE OUT
                LDX         LSTTXT          ; GET RAM ADDRESS OF THIS LINE
                BSR         LB7C2           ; UNCRUNCH A LINE
                LDX         [LSTTXT]        ; POINT X TO START OF NEXT LINE
                LDU         #LINBUF+1       ; POINT U TO BUFFER FULL OF UNCRUNCHED LINE
LB7B9           LDA         ,U+             ; GET A BYTE FROM THE BUFFER
                BEQ         LB78D           ; BRANCH IF END OF BUFFER
                JSR         >LB9B1          ; SEND CHARACTER TO CONSOLE OUT
                BRA         LB7B9           ; GET ANOTHER CHARACTER
; UNCRUNCH A LINE INTO BASICS LINE INPUT BUFFER
LB7C2           JSR         >RVEC24         ; HOOK INTO RAM
                LEAX        4,X             ; MOVE POINTER PAST ADDRESS OF NEXT LINE AND LINE NUMBER
                LDY         #LINBUF+1       ; UNCRUNCH LINE INTO LINE INPUT BUFFER
LB7CB           LDA         ,X+             ; GET A CHARACTER
                BEQ         LB820           ; BRANCH IF END OF LINE
                BMI         LB7E6           ; BRANCH IF ITS A TOKEN
                CMPA        #':             ; CHECK FOR END OF SUB LINE
                BNE         LB7E2           ; BRNCH IF NOT END OF SUB LINE
                LDB         ,X              ; GET CHARACTER FOLLOWING COLON
                CMPB        #$84            ; TOKEN FOR ELSE?
                BEQ         LB7CB           ; YES - DONT PUT IT IN BUFFER
                CMPB        #$83            ; TOKEN FOR REMARK?
                BEQ         LB7CB           ; YES - DONT PUT IT IN BUFFER
                FCB         SKP2            ; SKIP TWO BYTES
LB7E0           LDA         #'!             ; EXCLAMATION POINT
LB7E2           BSR         LB814           ; PUT CHARACTER IN BUFFER
                BRA         LB7CB           ; GET ANOTHER CHARACTER
; UNCRUNCH A TOKEN
LB7E6           LDU         #COMVEC-10      ; FIRST DO COMMANDS
                CMPA        #$FF            ; CHECK FOR SECONDARY TOKEN
                BNE         LB7F1           ; BRANCH IF NON SECONDARY TOKEN
                LDA         ,X+             ; GET SECONDARY TOKEN
                LEAU        5,U             ; BUMP IT UP TO SECONDARY FUNCTIONS
LB7F1           ANDA        #$7F            ; MASK OFF BIT 7 OF TOKEN
LB7F3           LEAU        10,U            ; MOVE TO NEXT COMMAND TABLE
                TST         ,U              ; IS THIS TABLE ENABLED?
                BEQ         LB7E0           ; NO - ILLEGAL TOKEN
LB7F9           SUBA        ,U              ; SUBTRACT THE NUMBER OF TOKENS FROM THE CURRENT TOKEN NUMBER
                BPL         LB7F3           ; BRANCH IF TOKEN NOT IN THIS TABLE
                ADDA        ,U              ; RESTORE TOKEN NUMBER RELATIVE TO THIS TABLE
                LDU         1,U             ; POINT U TO COMMAND DICTIONARY TABLE
LB801           DECA                        ;  DECREMENT TOKEN NUMBER
                BMI         LB80A           ; BRANCH IF THIS IS THE CORRECT TOKEN
; SKIP THROUGH DICTIONARY TABLE TO START OF NEXT TOKEN
LB804           TST         ,U+             ; GRAB A BYTE
                BPL         LB804           ; BRANCH IF BIT 7 NOT SET
                BRA         LB801           ; GO SEE IF THIS IS THE CORRECT TOKEN
LB80A           LDA         ,U              ; GET A CHARACTER FROM DICTIONARY TABLE
                BSR         LB814           ; PUT CHARACTER IN BUFFER
                TST         ,U+             ; CHECK FOR START OF NEXT TOKEN
                BPL         LB80A           ; BRANCH IF NOT DONE WITH THIS TOKEN
                BRA         LB7CB           ; GO GET ANOTHER CHARACTER
LB814           CMPY        #LINBUF+LBUFMX  ; TEST FOR END OF LINE INPUT BUFFER
                BCC         LB820           ; BRANCH IF AT END OF BUFFER
                ANDA        #$7F            ; MASK OFF BIT 7
                STA         ,Y+             ; SAVE CHARACTER IN BUFFER AND
                CLR         ,Y              ; CLEAR NEXT CHARACTER SLOT IN BUFFER
LB820           RTS

; CRUNCH THE LINE THAT THE INPUT POINTER IS
; POINTING TO INTO THE LINE INPUT BUFFER
; RETURN LENGTH OF CRUNCHED LINE IN ACCD

LB821           JSR         >RVEC23         ; HOOK INTO RAM
                LDX         CHARAD          ; GET BASIC'S INPUT POINTER ADDRESS
                LDU         #LINBUF         ; POINT X TO LINE INPUT BUFFER
LB829           CLR         V43             ; CLEAR ILLEGAL TOKEN FLAG
                CLR         V44             ; CLEAR DATA FLAG
LB82D           LDA         ,X+             ; GET INPUT CHAR
                BEQ         LB852           ; BRANCH IF END OF LINE
                TST         V43             ; CHECK ILLEGAL TOKEN FLAG & BRANCH IF NOT
                BEQ         LB844           ; PROCESSING AN ILLEGAL TOKEN
                JSR         >LB3A2          ; SET CARRY IF NOT UPPER CASE ALPHA
                BCC         LB852           ; BRANCH IF UPPER CASE ALPHA
                CMPA        #'0             ; DONT CRUNCH ASCII NUMERIC CHARACTERS
                BLO         LB842           ; BRANCH IF NOT NUMERIC
                CMPA        #'9
                BLS         LB852           ; BRANCH IF NUMERIC
; END UP HERE IF NOT UPPER CASE ALPHA OR NUMERIC
LB842           CLR         V43             ; CLEAR ILLEGAL TOKEN FLAG
LB844           CMPA        #SPACE          ; SPACE?
                BEQ         LB852           ; DO NOT REMOVE SPACES
                STA         V42             ; SAVE INPUT CHARACTER AS SCAN DELIMITER
                CMPA        #'"             ; CHECK FOR STRING DELIMITER
                BEQ         LB886           ; BRANCH IF STRING
                TST         V44             ; CHECK DATA FLAG AND BRANCH IF CLEAR
                BEQ         LB86B           ; DO NOT CRUNCH DATA
LB852           STA         ,U+             ; SAVE CHARACTER IN BUFFER
                BEQ         LB85C           ; BRANCH IF END OF LINE
                CMPA        #':             ; CHECK FOR END OF SUBLINE
                BEQ         LB829           ; AND RESET FLAGS IF END OF SUBLINE
LB85A           BRA         LB82D           ; GO GET ANOTHER CHARACTER
LB85C           CLR         ,U+             ; DOUBLE ZERO AT END OF LINE
                CLR         ,U+
                TFR         U,D             ; SAVE ADDRESS OF END OF LINE IN ACCD
                SUBD        #LINHDR         ; LENGTH OF LINE IN ACCD
                LDX         #LINBUF-1       ; SET THE INPUT POINTER TO ONE BEFORE
                STX         CHARAD          ; THE START OF THE CRUNCHED LINE
                RTS         EXIT            ; 'CRUNCH'
LB86B           CMPA        #'?             ; CHECK FOR "?" - PRINT ABBREVIATION
                BNE         LB873           ; BRANCH IF NOT PRINT ABBREVIATION
                LDA         #$87            ; GET THE PRINT TOKEN AND SAVE IT
                BRA         LB852           ; IN BUFFER
LB873           CMPA        #''             ; APOSTROPHE IS SAME AS REM
                BNE         LB88A           ; BRANCH IF NOT REMARK
                LDD         #$3A83          ; COLON, REM TOKEN
                STD         ,U++            ; SAVE IN BUFFER
LB87C           CLR         V42             ; SET DELIMITER = 0 (END OF LINE)
LB87E           LDA         ,X+             ; SCAN TILL WE MATCH [V42]
                BEQ         LB852           ; BRANCH IF END OF LINE
                CMPA        V42             ; DELIMITER?
                BEQ         LB852           ; BRANCH OUT IF SO
LB886           STA         ,U+             ; DONT CRUNCH REMARKS OR STRINGS
                BRA         LB87E           ; GO GET MORE STRING OR REMARK
LB88A           CMPA        #'0             ; LESS THAN ASCII ZERO?
                BCS         LB892           ; BRANCH IF SO
                CMPA        #';+1           ; CHECK FOR NUMERIC VALUE, COLON OR SEMICOLON
                BCS         LB852           ; AND INSERT IN BUFFER IF SO
LB892           LEAX        -1,X            ; MOVE INPUT POINTER BACK ONE
                PSHS        U,X             ; SAVE POINTERS TO INPUT STRING, OUTPUT STRING
                CLR         V41             ; TOKEN FLAG 0 = COMMAND, FF = SECONDARY
                LDU         #COMVEC-10      ; POINT U TO COMMAND INTERPRETATION
; TABLE FOR BASIC - 10
LB89B           CLR         V42             ; INITIALIZE V42 AS TOKEN COUNTER
LB89D           LEAU        10,U            ; MOVE TO NEXT COMMAND INTERPRETATION TABLE
                LDA         ,U              ; GET NUMBER OF COMMANDS
                BEQ         LB8D4           ; GO DO SECONDARY FUNCTIONS IF NO COMMAND TABLE
                LDY         1,U             ; POINT Y TO COMMAND DICTIONARY TABLE
LB8A6           LDX         ,S              ; GET POINTER TO INPUT STRING
LB8A8           LDB         ,Y+             ; GET A BYTE FROM DICTIONARY TABLE
                SUBB        ,X+             ; SUBTRACT INPUT CHARACTER
                BEQ         LB8A8           ; LOOP IF SAME
                CMPB        #$80            ; LAST CHAR IN RESERVED WORD TABLE HAD
; BIT 7 SET, SO IF WE HAVE $80 HERE
; THEN IT IS A GOOD COMPARE
                BNE         LB8EA           ; BRANCH IF NO MATCH - CHECK ANOTHER COMMAND
                LEAS        2,S             ; DELETE OLD INPUT POINTER FROM STACK
                PULS        U               ; GET POINTER TO OUTPUT STRING
                ORB         V42             ; OR IN THE TABLE POSITION TO MAKE THE TOKEN
; - NOTE THAT B ALREADY HAD $80 IN IT -
                LDA         V41             ; CHECK TOKEN FLAG AND BRANCH
                BNE         LB8C2           ; IF SECONDARY
                CMPB        #$84            ; IS IT ELSE TOKEN?
                BNE         LB8C6           ; NO
                LDA         #':             ; PUT A COLON (SUBLINE) BEFORE ELSE TOKEN
LB8C2           STD         ,U++            ; SECONDARY TOKENS PRECEEDED BY $FF
                BRA         LB85A           ; GO PROCESS MORE INPUT CHARACTERS
LB8C6           STB         ,U+             ; SAVE THIS TOKEN
                CMPB        #$86            ; DATA TOKEN?
                BNE         LB8CE           ; NO
                INC         V44             ; SET DATA FLAG
LB8CE           CMPB        #$82            ; REM TOKEN?
                BEQ         LB87C           ; YES
LB8D2           BRA         LB85A           ; GO PROCESS MORE INPUT CHARACTERS
; CHECK FOR A SECONDARY TOKEN
LB8D4           LDU         #COMVEC-5       ; NOW DO SECONDARY FUNCTIONS
LB8D7           COM         V41             ; TOGGLE THE TOKEN FLAG
                BNE         LB89B           ; BRANCH IF NOW CHECKING SECONDARY COMMANDS
; THIS CODE WILL PROCESS INPUT DATA WHICH CANNOT BE CRUNCHED AND SO
; IS ASSUMED TO BE ILLEGAL DATA OR AN ILLEGAL TOKEN
                PULS        X,U             ; RESTORE INPUT AND OUTPUT POINTERS
                LDA         ,X+             ; MOVE THE FIRST CHARACTER OF AN
                STA         ,U+             ; ILLEGAL TOKEN
                JSR         >LB3A2          ; SET CARRY IF NOT ALPHA
                BCS         LB8D2           ; BRANCH IF NOT ALPHA
                COM         V43             ; SET ILLEGAL TOKEN FLAG IF UPPER CASE ALPHA
                BRA         LB8D2           ; PROCESS MORE INPUT CHARACTERS
LB8EA           INC         V42             ; INCREMENT TOKEN COUNTER
                DECA                        ;  DECR COMMAND COUNTER
                BEQ         LB89D           ; GET ANOTHER COMMAND TABLE IF DONE W/THIS ONE
                LEAY        -1,Y            ; MOVE POINTER BACK ONE
LB8F1           LDB         ,Y+             ; GET TO NEXT
                BPL         LB8F1           ; RESERVED WORD
                BRA         LB8A6           ; GO SEE IF THIS WORD IS A MATCH
; PRINT
PRINT           BEQ         LB958           ; BRANCH IF NO ARGUMENT
                BSR         LB8FE           ; CHECK FOR ALL PRINT OPTIONS
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                RTS
LB8FE           CMPA        #'@             ; CHECK FOR PRINT @
                BNE         LB907           ; NOT PRINT @
LB902           JSR         >LA554          ; MOVE CURSOR TO PROPER PRINT LOCATION
LB905           BRA         LB911           ; GO PRINT THE DATA
LB907           CMPA        #'#             ; CHECK FOR PRINT NUMBER
                BNE         LB918           ; NOT PRINT#
                JSR         >LA5A5          ; CHECK FOR A VALID DEVICE NUMBER
                JSR         >LA406          ; CHECK FOR A VALID OUTPUT FILE
LB911           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         LB958           ; BRANCH IF END OF LINE
                JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
LB918           JSR         >RVEC9          ; HOOK INTO RAM
LB91B           BEQ         LB965           ; RETURN IF END OF LINE
LB91D           CMPA        #$A4            ; TOKEN FOR TAB( ?
                BEQ         LB97E           ; YES
                CMPA        #',             ; COMMA?
                BEQ         LB966           ; YES - ADVANCE TO NEXT TAB FIELD
                CMPA        #';             ; SEMICOLON?
                BEQ         LB997           ; YES - DO NOT ADVANCE CURSOR
                JSR         >LB156          ; EVALUATE EXPRESSION
                LDA         VALTYP          ; GET VARIABLE TYPE AND
                PSHS        A               ; SAVE IT ON THE STACK
                BNE         LB938           ; BRANCH IF STRING VARIABLE
                JSR         >LBDD9          ; CONVERT FP NUMBER TO AN ASCII STRING
                JSR         >LB516          ; PARSE A STRING FROM (X-1) AND PUT
; DESCRIPTOR ON STRING STACK
LB938           BSR         LB99F           ; PRINT STRING POINTED TO BY X
                PULS        B               ; GET VARIABLE TYPE BACK
                JSR         >LA35F          ; SET UP TAB WIDTH ZONE, ETC
                TST         PRTDEV          ; CHECK THE PRINT DEVICE
                BEQ         LB949           ; AND BRANCH IF NOT CASSETTE
                BSR         LB958           ; SEND A CARRIAGE RETURN TO CONSOLE OUT
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BRA         LB91B           ; CHECK FOR MORE PRINT DATA
LB949           TSTB                        ;  CHECK CURRENT PRINT POSITION
                BNE         LB954           ; BRANCH IF NOT AT START OF LINE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #',             ; COMMA?
                BEQ         LB966           ; SKIP TO NEXT TAB FIELD
                BSR         LB9AC           ; SEND A SPACE TO CONSOLE OUT
LB954           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BNE         LB91D           ; BRANCH IF NOT END OF LINE
LB958           LDA         #CR             ; SEND A CR TO
                BRA         LB9B1           ; CONSOLE OUT
LB95C           JSR         >LA35F          ; SET UP TAB WIDTH, ZONE ETC
LB95F           BEQ         LB958           ; BRANCH IF WIDTH = ZERO
                LDA         DEVPOS          ; GET PRINT POSITION
                BNE         LB958           ; BRANCH IF NOT AT START OF LINE
LB965           RTS
; SKIP TO NEXT TAB FIELD
LB966           JSR         >LA35F          ; SET UP TAB WIDTH, ZONE ETC
                BEQ         LB975           ; BRANCH IF LINE WIDTH = 0 (CASSETTE)
                LDB         DEVPOS          ; GET CURRENT POSITION
                CMPB        DEVLCF          ; COMPARE TO LAST TAB ZONE
                BCS         LB977           ; BRANCH IF < LAST TAB ZONE
                BSR         LB958           ; SEND A CARRIAGE RETURN TO CONSOLE OUT
                BRA         LB997           ; GET MORE DATA
LB975           LDB         DEVPOS
LB977           SUBB        DEVCFW          ; SUBTRACT TAB FIELD WIDTH FROM CURRENT
                BCC         LB977           ; POSITION UNTIL CARRY SET - NEGATING THE
                NEGB                        ;  REMAINDER LEAVES THE NUMBER OF SPACES TO NEXT
; TAB ZONE IN ACCB
                BRA         LB98E           ; GO ADVANCE TO NEXT TAB ZONE
; PRINT TAB(
LB97E           JSR         >LB709          ; EVALUATE EXPRESSION - RETURN VALUE IN B
                CMPA        #') ; 'SYNTAX' ERROR IF NOT ')'
                LBNE        LB277
                JSR         >LA35F          ; SET UP TAB WIDTH, ZONE ETC
                SUBB        DEVPOS          ; GET DIFFERENCE OF PRINT POSITION & TAB POSITION
                BLS         LB997           ; BRANCH IF TAB POSITION < CURRENT POSITION
LB98E           TST         PRTDEV          ; GET PRINT DEVICE NUMBER AND
                BNE         LB997           ; BRANCH IF CASSETTE
LB992           BSR         LB9AC           ; SEND A SPACE TO CONSOLE OUT
                DECB                        ;  DECREMENT DIFFERENCE COUNT
                BNE         LB992           ; BRANCH UNTIL CURRENT POSITION = TAB POSITION
LB997           JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                JMP         >LB91B          ; LOOK FOR MORE PRINT DATA
; COPY A STRING FROM (X) TO CONSOLE OUT
STRINOUT        EQU         *
LB99C           JSR         >LB518          ; PARSE A STRING FROM X AND PUT
; DESCRIPTOR ON STRING STACK
LB99F           JSR         >LB657          ; GET LENGTH OF STRING AND REMOVE
; DESCRIPTOR FROM STRING STACK
LB9A2           INCB                        ;  COMPENSATE FOR DECB BELOW
LB9A3           DECB                        ;  DECREMENT COUNTER
                BEQ         LB965           ; EXIT ROUTINE
                LDA         ,X+             ; GET A CHARACTER FROM X
                BSR         LB9B1           ; SEND TO CONSOLE OUT
                BRA         LB9A3           ; KEEP LOOPING
LB9AC           LDA         #SPACE          ; SPACE TO CONSOLE OUT
                FCB         SKP2            ; SKIP NEXT TWO BYTES
LB9AF           LDA         #'?             ; QUESTION MARK TO CONSOLE OUT
LB9B1           JMP         >PUTCHR         ; JUMP TO CONSOLE OUT
; FLOATING POINT MATH PACKAGE
; ADD .5 TO FPA0
LB9B4           LDX         #LBEC0          ; FLOATING POINT CONSTANT (.5)
                BRA         LB9C2           ; ADD .5 TO FPA0
; SUBTRACT FPA0 FROM FP NUMBER POINTED
; TO BY (X), LEAVE RESULT IN FPA0
LB9B9           JSR         >LBB2F          ; COPY PACKED FP DATA FROM (X) TO FPA1
; ARITHMETIC OPERATION (-) JUMPS HERE - SUBTRACT FPA0 FROM FPA1 (ENTER
; WITH EXPONENT OF FPA0 IN ACCB AND EXPONENT OF FPA1 IN ACCA)
LB9BC           COM         FP0SGN          ; CHANGE MANTISSA SIGN OF FPA0
                COM         RESSGN          ; REVERSE RESULT SIGN FLAG
                BRA         LB9C5           ; GO ADD FPA1 AND FPA0
; ADD FP NUMBER POINTED TO BY
; (X) TO FPA0 - LEAVE RESULT IN FPA0
LB9C2           JSR         >LBB2F          ; UNPACK PACKED FP DATA FROM (X) TO
; FPA1; RETURN EXPONENT OF FPA1 IN ACCA
; ARITHMETIC OPERATION (+) JUMPS HERE - ADD FPA0 TO
; FPA1 (ENTER WITH EXPONENT OF FPA0 IN ACCB AND EXPONENT OF FPA1 IN ACCA
LB9C5           TSTB                        ;  CHECK EXPONENT OF FPA0
                LBEQ        LBC4A           ; COPY FPA1 TO FPA0 IF FPA0 = 0
                LDX         #FP1EXP         ; POINT X TO FPA1
LB9CD           TFR         A,B             ; PUT EXPONENT OF FPA1 INTO ACCB
                TSTB                        ;  CHECK EXPONENT
                BEQ         LBA3E           ; RETURN IF EXPONENT = 0 (ADDING 0 TO FPA0)
                SUBB        FP0EXP          ; SUBTRACT EXPONENT OF FPA0 FROM EXPONENT OF FPA1
                BEQ         LBA3F           ; BRANCH IF EXPONENTS ARE EQUAL
; -----------------------------------------------------------------------------
                if          VERBAS<12
; -----------------------------------------------------------------------------
                BMI         LB9E2           ; BRANCH IF EXPONENT FPA0 > FPA1
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                BCS         LB9E2           ; BRANCH IF EXPONENT FPA0 > FPA1
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                STA         FP0EXP          ; REPLACE FPA0 EXPONENT WITH FPA1 EXPONENT
                LDA         FP1SGN          ; REPLACE FPA0 MANTISSA SIGN
                STA         FP0SGN          ; WITH FPA1 MANTISSA SIGN
                LDX         #FP0EXP         ; POINT X TO FPA0
                NEGB                        ;  NEGATE DIFFERENCE OF EXPONENTS
LB9E2           CMPB        #-8             ; TEST DIFFERENCE OF EXPONENTS
                BLE         LBA3F           ; BRANCH IF DIFFERENCE OF EXPONENTS <= 8
                CLRA                        ;  CLEAR OVERFLOW BYTE
                LSR         1,X             ; SHIFT MS BYTE OF MANTISSA; BIT 7 = 0
                JSR         >LBABA          ; GO SHIFT MANTISSA OF (X) TO THE RIGHT (B) TIMES
LB9EC           LDB         RESSGN          ; GET SIGN FLAG
                BPL         LB9FB           ; BRANCH IF FPA0 AND FPA1 SIGNS ARE THE SAME
                COM         1,X             ; COMPLEMENT MANTISSA POINTED
                COM         2,X             ; TO BY (X) THE
                COM         3,X             ; ADCA BELOW WILL
                COM         4,X             ; CONVERT THIS OPERATION
                COMA                        ;  INTO A NEG (MANTISSA)
                ADCA        #0              ; ADD ONE TO ACCA - COMA ALWAYS SETS THE CARRY FLAG
; THE PREVIOUS TWO BYTES MAY BE REPLACED BY A NEGA

; ADD MANTISSAS OF FPA0 AND FPA1, PUT RESULT IN FPA0
LB9FB           STA         FPSBYT          ; SAVE FPA SUB BYTE
                LDA         FPA0+3          ; ADD LS BYTE
                ADCA        FPA1+3          ; OF MANTISSA
                STA         FPA0+3          ; SAVE IN FPA0 LSB
                LDA         FPA0+2          ; ADD NEXT BYTE
                ADCA        FPA1+2          ; OF MANTISSA
                STA         FPA0+2          ; SAVE IN FPA0
                LDA         FPA0+1          ; ADD NEXT BYTE
                ADCA        FPA1+1          ; OF MANTISSA
                STA         FPA0+1          ; SAVE IN FPA0
                LDA         FPA0            ; ADD MS BYTE
                ADCA        FPA1            ; OF MANTISSA
                STA         FPA0            ; SAVE IN FPA0
                TSTB                        ;  TEST SIGN FLAG
                BPL         LBA5C           ; BRANCH IF FPA0 & FPA1 SIGNS WERE ALIKE
LBA18           BCS         LBA1C           ; BRANCH IF POSITIVE MANTISSA
                BSR         LBA79           ; NEGATE FPA0 MANTISSA
; NORMALIZE FPA0
LBA1C           CLRB                        ;  CLEAR TEMPORARY EXPONENT ACCUMULATOR
LBA1D           LDA         FPA0            ; TEST MSB OF MANTISSA
                BNE         LBA4F           ; BRANCH IF <> 0
                LDA         FPA0+1          ; IF THE MSB IS
                STA         FPA0            ; 0, THEN SHIFT THE
                LDA         FPA0+2          ; MANTISSA A WHOLE BYTE
                STA         FPA0+1          ; AT A TIME. THIS
                LDA         FPA0+3          ; IS FASTER THAN ONE
                STA         FPA0+2          ; BIT AT A TIME
                LDA         FPSBYT          ; BUT USES MORE MEMORY.
                STA         FPA0+3          ; FPSBYT, THE CARRY IN
                CLR         FPSBYT          ; BYTE, REPLACES THE MATISSA LSB.
                ADDB        #8              ; SHIFTING ONE BYTE = 8 BIT SHIFTS; ADD 8 TO EXPONENT
                CMPB        #5*8            ; CHECK FOR 5 SHIFTS
                BLT         LBA1D           ; BRANCH IF < 5 SHIFTS, IF > 5, THEN MANTISSA = 0
LBA39           CLRA                        ;  A ZERO EXPONENT = 0 FLOATING POINT
LBA3A           STA         FP0EXP          ; ZERO OUT THE EXPONENT
                STA         FP0SGN          ; ZERO OUT THE MANTISSA SIGN
LBA3E           RTS
LBA3F           BSR         LBAAE           ; SHIFT FPA0 MANTISSA TO RIGHT
                CLRB                        ;  CLEAR CARRY FLAG
                BRA         LB9EC
; SHIFT FPA0 LEFT ONE BIT UNTIL BIT 7
; OF MATISSA MS BYTE = 1
LBA44           INCB                        ;  ADD ONE TO EXPONENT ACCUMULATOR
                ASL         FPSBYT          ; SHIFT SUB BYTE ONE LEFT
                ROL         FPA0+3          ; SHIFT LS BYTE
                ROL         FPA0+2          ; SHIFT NS BYTE
                ROL         FPA0+1          ; SHIFT NS BYTE
                ROL         FPA0            ; SHIFT MS BYTE
LBA4F           BPL         LBA44           ; BRANCH IF NOT YET NORMALIZED
                LDA         FP0EXP          ; GET CURRENT EXPONENT
                PSHS        B               ; SAVE EXPONENT MODIFIER CAUSED BY NORMALIZATION
                SUBA        ,S+             ; SUBTRACT ACCUMULATED EXPONENT MODIFIER
                STA         FP0EXP          ; SAVE AS NEW EXPONENT
                BLS         LBA39           ; SET FPA0 = 0 IF THE NORMALIZATION CAUSED
; MORE OR EQUAL NUMBER OF LEFT SHIFTS THAN THE
; SIZE OF THE EXPONENT
                FCB         SKP2            ; SKIP 2 BYTES
LBA5C           BCS         LBA66           ; BRANCH IF MANTISSA OVERFLOW
                ASL         FPSBYT          ; SUB BYTE BIT 7 TO CARRY - USE AS ROUND-OFF
; FLAG (TRUNCATE THE REST OF SUB BYTE)
                LDA         #0              ; CLRA, BUT DO NOT CHANGE CARRY FLAG
                STA         FPSBYT          ; CLEAR THE SUB BYTE
                BRA         LBA72           ; GO ROUND-OFF RESULT
LBA66           INC         FP0EXP          ; INCREMENT EXPONENT - MULTIPLY BY 2
                BEQ         LBA92           ; OVERFLOW ERROR IF CARRY PAST $FF
                ROR         FPA0            ; SHIFT MANTISSA
                ROR         FPA0+1          ; ONE TO
                ROR         FPA0+2          ; THE RIGHT -
                ROR         FPA0+3          ; DIVIDE BY TWO
LBA72           BCC         LBA78           ; BRANCH IF NO ROUND-OFF NEEDED
                BSR         LBA83           ; ADD ONE TO MANTISSA - ROUND OFF
                BEQ         LBA66           ; BRANCH iF OVERFLOW - MANTISSA = 0
LBA78           RTS
; NEGATE FPA0 MANTISSA
LBA79           COM         FP0SGN          ; TOGGLE SIGN OF MANTISSA
LBA7B           COM         FPA0            ; COMPLEMENT ALL 4 MANTISSA BYTES
                COM         FPA0+1
                COM         FPA0+2
                COM         FPA0+3
; ADD ONE TO FPA0 MANTISSA
LBA83           LDX         FPA0+2          ; GET BOTTOM 2 MANTISSA
                LEAX        1,X             ; BYTES, ADD ONE TO
                STX         FPA0+2          ; THEM AND SAVE THEM
                BNE         LBA91           ; BRANCH IF NO OVERFLOW
                LDX         FPA0            ; IF OVERFLOW ADD ONE
                LEAX        1,X             ; TO TOP 2 MANTISSA
                STX         FPA0            ; BYTES AND SAVE THEM
LBA91           RTS
LBA92           LDB         #2*5            ; 'OV' OVERFLOW ERROR
                JMP         >LAC46          ; PROCESS AN ERROR
LBA97           LDX         #FPA2-1         ; POINT X TO FPA2
; SHIFT FPA POINTED TO BY (X) TO
; THE RIGHT -(B) TIMES. EXIT WITH
; ACCA CONTAINING DATA SHIFTED OUT
; TO THE RIGHT (SUB BYTE) AND THE DATA
; SHIFTED IN FROM THE LEFT WILL COME FROM FPCARY
LBA9A           LDA         4,X             ; GET LS BYTE OF MANTISSA (X)
                STA         FPSBYT          ; SAVE IN FPA SUB BYTE
                LDA         3,X             ; SHIFT THE NEXT THREE BYTES OF THE
                STA         4,X             ; MANTISSA RIGHT ONE COMPLETE BYTE.
                LDA         2,X
                STA         3,X
                LDA         1,X
                STA         2,X
                LDA         FPCARY          ; GET THE CARRY IN BYTE
                STA         1,X             ; STORE AS THE MS MANTISSA BYTE OF (X)
LBAAE           ADDB        #8              ; ADD 8 TO DIFFERENCE OF EXPONENTS
                BLE         LBA9A           ; BRANCH IF EXPONENT DIFFERENCE < -8
                LDA         FPSBYT          ; GET FPA SUB BYTE
                SUBB        #8              ; CAST OUT THE 8 ADDED IN ABOVE
                BEQ         LBAC4           ; BRANCH IF EXPONENT DIFFERENCE = 0
; SHIFT MANTISSA POINTED TO BY (X) TO THE RIGHT (B) TIMES. OVERFLOW RETAINED IN ACCA.
LBAB8           ASR         1,X             ; SHIFT MANTISSA AND SUB BYTE ONE BIT TO THE RIGHT
LBABA           ROR         2,X
                ROR         3,X
                ROR         4,X
                RORA                        ;
                INCB                        ;  ADD ONE TO EXPONENT DIFFERENCE
                BNE         LBAB8           ; BRANCH IF EXPONENTS NOT =
LBAC4           RTS
LBAC5           FCB         $81,$00,$00,$00,$00 ; FLOATING POINT CONSTANT 1.0
; ARITHMETIC OPERATION (*) JUMPS HERE - MULTIPLY
; FPA0 BY (X) - RETURN PRODUCT IN FPA0
LBACA           BSR         LBB2F           ; MOVE PACKED FPA FROM (X) TO FPA1
                BEQ         LBB2E           ; BRANCH IF EXPONENT OF FPA0 = 0
                BSR         LBB48           ; CALCULATE EXPONENT OF PRODUCT
; MULTIPLY FPA0 MANTISSA BY FPA1. NORMALIZE
; HIGH ORDER BYTES OF PRODUCT IN FPA0. THE
; LOW ORDER FOUR BYTES OF THE PRODUCT WILL
; BE STORED IN VAB-VAE.
LBAD0           LDA         #0              ; ZERO OUT MANTISSA OF FPA2
                STA         FPA2
                STA         FPA2+1
                STA         FPA2+2
                STA         FPA2+3
                LDB         FPA0+3          ; GET LS BYTE OF FPA0
                BSR         LBB00           ; MULTIPLY BY FPA1
                LDB         FPSBYT          ; TEMPORARILY SAVE SUB BYTE 4
                STB         VAE
                LDB         FPA0+2          ; GET NUMBER 3 MANTISSA BYTE OF FPA0
                BSR         LBB00           ; MULTIPLY BY FPA1
                LDB         FPSBYT          ; TEMPORARILY SAVE SUB BYTE 3
                STB         VAD
                LDB         FPA0+1          ; GET NUMBER 2 MANTISSA BYTE OF FPA0
                BSR         LBB00           ; MULTIPLY BY FPA1
                LDB         FPSBYT          ; TEMPORARILY SAVE SUB BYTE 2
                STB         VAC
                LDB         FPA0            ; GET MS BYTE OF FPA0 MANTISSA
                BSR         LBB02           ; MULTIPLY BY FPA1
                LDB         FPSBYT          ; TEMPORARILY SAVE SUB BYTE 1
                STB         VAB
                JSR         >LBC0B          ; COPY MANTISSA FROM FPA2 TO FPA0
                JMP         >LBA1C          ; NORMALIZE FPA0
LBB00           BEQ         LBA97           ; SHIFT FPA2 ONE BYTE TO RIGHT
LBB02           COMA                        ;  SET CARRY FLAG
; MULTIPLY FPA1 MANTISSA BY ACCB AND
; ADD PRODUCT TO FPA2 MANTISSA
LBB03           LDA         FPA2            ; GET FPA2 MS BYTE
                RORB                        ;  ROTATE CARRY FLAG INTO SHIFT COUNTER
; DATA BIT INTO CARRY
                BEQ         LBB2E           ; BRANCH WHEN 8 SHIFTS DONE
                BCC         LBB20           ; DO NOT ADD FPA1 IF DATA BIT = 0
                LDA         FPA2+3          ; ADD MANTISSA LS BYTE
                ADDA        FPA1+3
                STA         FPA2+3
                LDA         FPA2+2          ; ADD MANTISSA NUMBER 3 BYTE
                ADCA        FPA1+2
                STA         FPA2+2
                LDA         FPA2+1          ; ADD MANTISSA NUMBER 2 BYTE
                ADCA        FPA1+1
                STA         FPA2+1
                LDA         FPA2            ; ADD MANTISSA MS BYTE
                ADCA        FPA1
LBB20           RORA                        ;  ROTATE CARRY INTO MS BYTE
                STA         FPA2
                ROR         FPA2+1          ; ROTATE FPA2 ONE BIT TO THE RIGHT
                ROR         FPA2+2
                ROR         FPA2+3
                ROR         FPSBYT
                CLRA                        ;  CLEAR CARRY FLAG
                BRA         LBB03           ; KEEP LOOPING
LBB2E           RTS
; UNPACK A FP NUMBER FROM (X) TO FPA1
LBB2F           LDD         1,X             ; GET TWO MSB BYTES OF MANTISSA FROM FPA POINTED TO BY X
                STA         FP1SGN          ; SAVE PACKED MANTISSA SIGN BYTE
                ORA         #$80            ; FORCE BIT 7 OF MSB MANTISSA = 1
                STD         FPA1            ; SAVE 2 MSB BYTES IN FPA1
                LDB         FP1SGN          ; GET PACKED MANTISSA SIGN BYTE. EOR W/FPA0
                EORB        FP0SGN          ; SIGN - NEW SIGN POSITION IF BOTH OLD SIGNS ALIKE,
                STB         RESSGN          ; NEG IF BOTH OLD SIGNS DIFF. SAVE ADJUSTED
; MANTISSA SIGN BYTE
                LDD         3,X             ; GET 2 LSB BYTES OF MANTISSA
                STD         FPA1+2          ; AND PUT IN FPA1
                LDA         ,X              ; GET EXPONENT FROM (X) AND
                STA         FP1EXP          ; PUT IN EXPONENT OF FPA1
                LDB         FP0EXP          ; GET EXPONENT OF FPA0
                RTS
; CALCULATE EXPONENT FOR PRODUCT OF FPA0 & FPA1
; ENTER WITH EXPONENT OF FPA1 IN ACCA
LBB48           TSTA                        ;  TEST EXPONENT OF FPA1
                BEQ         LBB61           ; PURGE RETURN ADDRESS & SET FPA0 = 0
                ADDA        FP0EXP          ; ADD FPA1 EXPONENT TO FPA0 EXPONENT
                RORA                        ;  ROTATE CARRY INTO BIT 7 BIT 0 INTO CARRY
                ROLA                        ;  SET OVERFLOW FLAG
                BVC         LBB61           ; BRANCH IF EXPONENT TOO LARGE OR SMALL
                ADDA        #$80            ; ADD $80 BIAS TO EXPONENT
                STA         FP0EXP          ; SAVE NEW EXPONENT
                BEQ         LBB63           ; SET FPA0
                LDA         RESSGN          ; GET MANTISSA SIGN
                STA         FP0SGN          ; SAVE AS MANTISSA SIGN OF FPA0
                RTS
; IF FPA0 = POSITIVE THEN 'OV' ERROR IF FPA0
; IS NEGATIVE THEN FPA0 = 0
LBB5C           LDA         FP0SGN          ; GET MANTISSA SIGN OF FPA0
                COMA                        ;  CHANGE SIGN OF FPA0 MANTISSA
                BRA         LBB63
LBB61           LEAS        2,S             ; PURGE RETURN ADDRESS FROM STACK
LBB63           LBPL        LBA39           ; ZERO FPA0 MANTISSA SIGN & EXPONENT
LBB67           JMP         >LBA92          ; 'OV' OVERFLOW ERROR
; FAST MULTIPLY BY 10 AND LEAVE RESULT IN FPA0
LBB6A           JSR         >LBC5F          ; TRANSFER FPA0 TO FPA1
                BEQ         LBB7C           ; BRANCH IF EXPONENT = 0
                ADDA        #2              ; ADD 2 TO EXPONENT (TIMES 4)
                BCS         LBB67           ; 'OV' ERROR IF EXPONENT > $FF
                CLR         RESSGN          ; CLEAR RESULT SIGN BYTE
                JSR         >LB9CD          ; ADD FPA1 TO FPA0 (TIMES 5)
                INC         FP0EXP          ; ADD ONE TO EXPONENT (TIMES 10)
                BEQ         LBB67           ; 'OV' ERROR IF EXPONENT > $FF
LBB7C           RTS
LBB7D           FCB         $84,$20,$00,$00,$00 ; FLOATING POINT CONSTANT 10
; DIVIDE FPA0 BY 10
LBB82           JSR         >LBC5F          ; MOVE FPA0 TO FPA1
                LDX         #LBB7D          ; POINT TO FLOATING POINT CONSTANT 10
                CLRB                        ;  ZERO MANTISSA SIGN BYTE
LBB89           STB         RESSGN          ; STORE THE QUOTIENT MANTISSA SIGN BYTE
                JSR         >LBC14          ; UNPACK AN FP NUMBER FROM (X) INTO FPA0
                FCB         SKP2            ; SKIP TWO BYTES
; DIVIDE (X) BY FPA0-LEAVE NORMALIZED QUOTIENT IN FPA0
LBB8F           BSR         LBB2F           ; GET FP NUMBER FROM (X) TO FPA1
; ARITHMETIC OPERATION (/) JUMPS HERE. DIVIDE FPA1 BY FPA0 (ENTER WITH
; EXPONENT OF FPA1 IN ACCA AND FLAGS SET BY TSTA)
; DIVIDE FPA1 BY FPA0
LBB91           BEQ         LBC06           ; '/0' DIVIDE BY ZERO ERROR
                NEG         FP0EXP          ; GET EXPONENT OF RECIPROCAL OF DIVISOR
                BSR         LBB48           ; CALCULATE EXPONENT OF QUOTIENT
                INC         FP0EXP          ; INCREMENT EXPONENT
                BEQ         LBB67           ; 'OV' OVERFLOW ERROR
                LDX         #FPA2           ; POINT X TO MANTISSA OF FPA2 - HOLD
; TEMPORARY QUOTIENT IN FPA2
                LDB         #4              ; 5 BYTE DIVIDE
                STB         TMPLOC          ; SAVE BYTE COUNTER
                LDB         #1              ; SHIFT COUNTER-AND TEMPORARY QUOTIENT BYTE
; COMPARE FPA0 MANTISSA TO FPA1 MANTISSA -
; SET CARRY FLAG IF FPA1 >= FPA0
LBBA4           LDA         FPA0            ; COMPARE THE TWO MS BYTES
                CMPA        FPA1            ; OF FPA0 AND FPA1 AND
                BNE         LBBBD           ; BRANCH IF <>
                LDA         FPA0+1          ; COMPARE THE NUMBER 2
                CMPA        FPA1+1          ; BYTES AND
                BNE         LBBBD           ; BRANCH IF <>
                LDA         FPA0+2          ; COMPARE THE NUMBER 3
                CMPA        FPA1+2          ; BYTES AND
                BNE         LBBBD           ; BRANCH IF <>
                LDA         FPA0+3          ; COMPARE THE LS BYTES
                CMPA        FPA1+3          ; AND BRANCH
                BNE         LBBBD           ; IF <>
                COMA                        ;  SET CARRY FLAG IF FPA0 = FPA1
LBBBD           TFR         CC,A            ; SAVE CARRY FLAG STATUS IN ACCA; CARRY
; CLEAR IF FPA0 > FPA1
                ROLB                        ;  ROTATE CARRY INTO TEMPORARY QUOTIENT BYTE
                BCC         LBBCC           ; CARRY WILL BE SET AFTER 8 SHIFTS
                STB         ,X+             ; SAVE TEMPORARY QUOTIENT
                DEC         TMPLOC          ; DECREMENT BYTE COUNTER
                BMI         LBBFC           ; BRANCH IF DONE
                BEQ         LBBF8           ; BRANCH IF LAST BYTE
                LDB         #1              ; RESET SHIFT COUNTER AND TEMPORARY QUOTIENT BYTE
LBBCC           TFR         A,CC            ; RESTORE CARRY FLAG AND
                BCS         LBBDE           ; BRANCH IF FPA0 =< FPA1
LBBD0           ASL         FPA1+3          ; SHIFT FPA1 MANTISSA 1 BIT TO LEFT
                ROL         FPA1+2
                ROL         FPA1+1
                ROL         FPA1
                BCS         LBBBD           ; BRANCH IF CARRY - ADD ONE TO PARTIAL QUOTIENT
                BMI         LBBA4           ; IF MSB OF HIGH ORDER MANTISSA BYTE IS
; SET, CHECK THE MAGNITUDES OF FPA0, FPA1
                BRA         LBBBD           ; CARRY IS CLEAR, CHECK ANOTHER BIT
; SUBTRACT FPA0 FROM FPA1 - LEAVE RESULT IN FPA1
LBBDE           LDA         FPA1+3          ; SUBTRACT THE LS BYTES OF MANTISSA
                SUBA        FPA0+3
                STA         FPA1+3
                LDA         FPA1+2          ; THEN THE NEXT BYTE
                SBCA        FPA0+2
                STA         FPA1+2
                LDA         FPA1+1          ; AND THE NEXT
                SBCA        FPA0+1
                STA         FPA1+1
                LDA         FPA1            ; AND FINALLY, THE MS BYTE OF MANTISSA
                SBCA        FPA0
                STA         FPA1
                BRA         LBBD0           ; GO SHIFT FPA1
LBBF8           LDB         #$40            ; USE ONLY TWO BITS OF THE LAST BYTE (FIFTH)
                BRA         LBBCC           ; GO SHIFT THE LAST BYTE
LBBFC           RORB                        ;  SHIFT CARRY (ALWAYS SET HERE) INTO
                RORB                        ;  BIT 5 AND MOVE
                RORB                        ;  BITS 1,0 TO BITS 7,6
                STB         FPSBYT          ; SAVE SUB BYTE
                BSR         LBC0B           ; MOVE MANTISSA OF FPA2 TO FPA0
                JMP         >LBA1C          ; NORMALIZE FPA0
LBC06           LDB         #2*10           ; '/0' ERROR
                JMP         >LAC46          ; PROCESS THE ERROR
; COPY MANTISSA FROM FPA2 TO FPA0
LBC0B           LDX         FPA2            ; MOVE TOP 2 BYTES
                STX         FPA0
                LDX         FPA2+2          ; MOVE BOTTOM 2 BYTES
                STX         FPA0+2
                RTS
; COPY A PACKED FP NUMBER FROM (X) TO FPA0
LBC14           PSHS        A               ; SAVE ACCA
                LDD         1,X             ; GET TOP TWO MANTISSA BYTES
                STA         FP0SGN          ; SAVE MS BYTE OF MANTISSA AS MANTISSA SIGN
                ORA         #$80            ; UNPACK MS BYTE
                STD         FPA0            ; SAVE UNPACKED TOP 2 MANTISSA BYTES
                CLR         FPSBYT          ; CLEAR MANTISSA SUB BYTE
                LDB         ,X              ; GET EXPONENT TO ACCB
                LDX         3,X             ; MOVE LAST 2
                STX         FPA0+2          ; MANTISSA BYTES
                STB         FP0EXP          ; SAVE EXPONENT
                PULS        A,PC            ; RESTORE ACCA AND RETURN
LBC2A           LDX         #V45            ; POINT X TO MANTISSA OF FPA4
                BRA         LBC35           ; MOVE FPA0 TO FPA4
LBC2F           LDX         #V40            ; POINT X TO MANTISSA OF FPA3
                FCB         SKP2            ; SKIP TWO BYTES
LBC33           LDX         VARDES          ; POINT X TO VARIABLE DESCRIPTOR IN VARDES
; PACK FPA0 AND MOVE IT TO ADDRESS IN X
LBC35           LDA         FP0EXP          ; COPY EXPONENT
                STA         ,X
                LDA         FP0SGN          ; GET MANTISSA SIGN BIT
                ORA         #$7F            ; MASK THE BOTTOM 7 BITS
                ANDA        FPA0            ; AND BIT 7 OF MANTISSA SIGN INTO BIT 7 OF MS BYTE
                STA         1,X             ; SAVE MS BYTE
                LDA         FPA0+1          ; MOVE 2ND MANTISSA BYTE
                STA         2,X
                LDU         FPA0+2          ; MOVE BOTTOM 2 MANTISSA BYTES
                STU         3,X
                RTS
; MOVE FPA1 TO FPA0 RETURN W/MANTISSA SIGN IN ACCA
LBC4A           LDA         FP1SGN          ; COPY MANTISSA SIGN FROM
LBC4C           STA         FP0SGN          ; FPA1 TO FPA0
                LDX         FP1EXP          ; COPY EXPONENT + MS BYTE FROM
                STX         FP0EXP          ; FPA1 TO FPA0
                CLR         FPSBYT          ; CLEAR MANTISSA SUB BYTE
                LDA         FPA1+1          ; COPY 2ND MANTISSA BYTE
                STA         FPA0+1          ; FROM FPA1 TO FPA0
                LDA         FP0SGN          ; GET MANTISSA SIGN
                LDX         FPA1+2          ; COPY 3RD AND 4TH MANTISSA BYTE
                STX         FPA0+2          ; FROM FPA1 TO FPA0
                RTS
; TRANSFER FPA0 TO FPA1
LBC5F           LDD         FP0EXP          ; TRANSFER EXPONENT & MS BYTE
                STD         FP1EXP
                LDX         FPA0+1          ; TRANSFER MIDDLE TWO BYTES
                STX         FPA1+1
                LDX         FPA0+3          ; TRANSFER BOTTOM TWO BYTES
                STX         FPA1+3
                TSTA                        ;  SET FLAGS ACCORDING TO EXPONENT
                RTS
; CHECK FPA0; RETURN ACCB = 0 IF FPA0 = 0,
; ACCB = $FF IF FPA0 = NEGATIVE, ACCB = 1 IF FPA0 = POSITIVE
LBC6D           LDB         FP0EXP          ; GET EXPONENT
                BEQ         LBC79           ; BRANCH IF FPA0 = 0
LBC71           LDB         FP0SGN          ; GET SIGN OF MANTISSA
LBC73           ROLB                        ;  BIT 7 TO CARRY
                LDB         #$FF            ; NEGATIVE FLAG
                BCS         LBC79           ; BRANCH IF NEGATIVE MANTISSA
                NEGB                        ;  ACCB 1 IF POSITIVE MANTISSA
LBC79           RTS
; SGN
SGN             BSR         LBC6D           ; SET ACCB ACCORDING TO SIGN OF FPA0
; CONVERT A SIGNED NUMBER IN ACCB INTO A FLOATING POINT NUMBER
LBC7C           STB         FPA0            ; SAVE ACCB IN FPA0
                CLR         FPA0+1          ; CLEAR NUMBER 2 MANTISSA BYTE OF FPA0
                LDB         #$88            ; EXPONENT REQUIRED IF FPA0 IS TO BE AN INTEGER
LBC82           LDA         FPA0            ; GET MS BYTE OF MANTISSA
                SUBA        #$80            ; SET CARRY IF POSITIVE MANTISSA
LBC86           STB         FP0EXP          ; SAVE EXPONENT
                LDD         ZERO            ; ZERO OUT ACCD AND
                STD         FPA0+2          ; BOTTOM HALF OF FPA0
                STA         FPSBYT          ; CLEAR SUB BYTE
                STA         FP0SGN          ; CLEAR SIGN OF FPA0 MANTISSA
                JMP         >LBA18          ; GO NORMALIZE FPA0
; ABS
ABS             CLR         FP0SGN          ; FORCE MANTISSA SIGN OF FPA0 POSITIVE
                RTS
; COMPARE A PACKED FLOATING POINT NUMBER POINTED TO
; BY (X) TO AN UNPACKED FP NUMBER IN FPA0. RETURN
; ZERO FLAG SET AND ACCB = 0, IF EQUAL; ACCB = 1 IF
; FPA0 > (X); ACCB = $FF IF FPA0 < (X)
LBC96           LDB         ,X              ; CHECK EXPONENT OF (X)
                BEQ         LBC6D           ; BRANCH IF FPA = 0
                LDB         1,X             ; GET MS BYTE OF MANTISSA OF (X)
                EORB        FP0SGN          ; EOR WITH SIGN OF FPA0
                BMI         LBC71           ; BRANCH IF SIGNS NOT =
; COMPARE FPA0 WITH FP NUMBER POINTED TO BY (X).
; FPA0 IS NORMALIZED, (X) IS PACKED.
LBCA0           LDB         FP0EXP          ; GET EXPONENT OF
                CMPB        ,X              ; FPA0, COMPARE TO EXPONENT OF
                BNE         LBCC3           ; (X) AND BRANCH IF <>.
                LDB         1,X             ; GET MS BYTE OF (X), KEEP ONLY
                ORB         #$7F            ; THE SIGN BIT - 'AND' THE BOTTOM 7
                ANDB        FPA0            ; BITS OF FPA0 INTO ACCB
                CMPB        1,X             ; COMPARE THE BOTTOM 7 BITS OF THE MANTISSA
                BNE         LBCC3           ; MS BYTE AND BRANCH IF <>
                LDB         FPA0+1          ; COMPARE 2ND BYTE
                CMPB        2,X             ; OF MANTISSA,
                BNE         LBCC3           ; BRANCH IF <>
                LDB         FPA0+2          ; COMPARE 3RD BYTE
                CMPB        3,X             ; OF MANTISSA,
                BNE         LBCC3           ; BRANCH IF <>
                LDB         FPA0+3          ; SUBTRACT LS BYTE
                SUBB        4,X             ; OF (X) FROM LS BYTE OF
                BNE         LBCC3           ; FPA0, BRANCH IF <>
                RTS         RETURN          ; IF FP (X) = FPA0
LBCC3           RORB                        ;  SHIFT CARRY TO BIT 7 CARRY SET IF FPA0 < (X)
                EORB        FP0SGN          ; TOGGLE SIZE COMPARISON BIT IF FPA0 IS NEGATIVE
                BRA         LBC73           ; GO SET ACCB ACCORDING TO COMPARISON
; DE-NORMALIZE FPA0 : SHIFT THE MANTISSA UNTIL THE BINARY POINT IS TO THE RIGHT
; OF THE LEAST SIGNIFICANT BYTE OF THE MANTISSA
LBCC8           LDB         FP0EXP          ; GET EXPONENT OF FPA0
                BEQ         LBD09           ; ZERO MANTISSA IF FPA0 = 0
                SUBB        #$A0            ; SUBTRACT $A0 FROM FPA0 EXPONENT T THIS WILL YIELD
; THE NUMBER OF SHIFTS REQUIRED TO DENORMALIZE FPA0. WHEN
; THE EXPONENT OF FPA0 IS = ZERO, THEN THE BINARY POINT
; WILL BE TO THE RIGHT OF THE MANTISSA
                LDA         FP0SGN          ; TEST SIGN OF FPA0 MANTISSA
                BPL         LBCD7           ; BRANCH IF POSITIVE
                COM         FPCARY          ; COMPLEMENT CARRY IN BYTE
                JSR         >LBA7B          ; NEGATE MANTISSA OF FPA0
LBCD7           LDX         #FP0EXP         ; POINT X TO FPA0
                CMPB        #-8             ; EXPONENT DIFFERENCE < -8?
                BGT         LBCE4           ; YES
                JSR         >LBAAE          ; SHIFT FPA0 RIGHT UNTIL FPA0 EXPONENT = $A0
                CLR         FPCARY          ; CLEAR CARRY IN BYTE
                RTS
LBCE4           CLR         FPCARY          ; CLEAR CARRY IN BYTE
                LDA         FP0SGN          ; GET SIGN OF FPA0 MANTISSA
                ROLA                        ;  ROTATE IT INTO THE CARRY FLAG
                ROR         FPA0            ; ROTATE CARRY (MANTISSA SIGN) INTO BIT 7
; OF LS BYTE OF MANTISSA
                JMP         >LBABA          ; DE-NORMALIZE FPA0
; INT
; THE INT STATEMENT WILL "DENORMALIZE" FPA0 - THAT IS IT WILL SHIFT THE BINARY POINT
; TO THE EXTREME RIGHT OF THE MANTISSA TO FORCE ITS EXPONENT TO BE $AO. ONCE
; THIS IS DONE THE MANTISSA OF FPA0 WILL CONTAIN THE FOUR LEAST SIGNIFICANT
; BYTES OF THE INTEGER PORTION OF FPA0. AT THE CONCLUSION OF THE DE-NORMALIZATION
; ONLY THE INTEGER PORTION OF FPA0 WILL REMAIN.

INT             LDB         FP0EXP          ; GET EXPONENT OF FPA0
                CMPB        #$A0            ; LARGEST POSSIBLE INTEGER EXPONENT
                BCC         LBD11           ; RETURN IF FPA0 >= 32768
                BSR         LBCC8           ; SHIFT THE BINARY POINT ONE TO THE RIGHT OF THE
; LS BYTE OF THE FPA0 MANTISSA
                STB         FPSBYT          ; ACCB = 0: ZERO OUT THE SUB BYTE
                LDA         FP0SGN          ; GET MANTISSA SIGN
                STB         FP0SGN          ; FORCE MANTISSA SIGN TO BE POSITIVE
                SUBA        #$80            ; SET CARRY IF MANTISSA
                LDA         #$A0            ; GET DENORMALIZED EXPONENT AND
                STA         FP0EXP          ; SAVE IT IN FPA0 EXPONENT
                LDA         FPA0+3          ; GET LS BYTE OF FPA0 AND
                STA         CHARAC          ; SAVE IT IN CHARAC
                JMP         >LBA18          ; NORMALIZE FPA0
LBD09           STB         FPA0            ; LOAD MANTISSA OF FPA0 WITH CONTENTS OF ACCB
                STB         FPA0+1
                STB         FPA0+2
                STB         FPA0+3
LBD11           RTS         *
; CONVERT ASCII STRING TO FLOATING POINT
LBD12           LDX         ZERO            ; (X) = 0
                STX         FP0SGN          ; ZERO OUT FPA0 & THE SIGN FLAG (COEFCT)
                STX         FP0EXP
                STX         FPA0+1
                STX         FPA0+2
                STX         V47             ; INITIALIZE EXPONENT & EXPONENT SIGN FLAG TO ZERO
                STX         V45             ; INITIALIZE RIGHT DECIMAL CTR & DECIMAL PT FLAG TO 0
                BCS         LBD86           ; IF CARRY SET (NUMERIC CHARACTER), ASSUME ACCA CONTAINS FIRST
; NUMERIC CHAR, SIGN IS POSITIVE AND SKIP THE RAM HOOK
                JSR         >RVEC19         ; HOOK INTO RAM
                CMPA        #'-             ; CHECK FOR A LEADING MINUS SIGN AND BRANCH
                BNE         LBD2D           ; IF NO MINUS SIGN
                COM         COEFCT          ; TOGGLE SIGN; 0 = +; FF = -
                BRA         LBD31           ; INTERPRET THE REST OF THE STRING
LBD2D           CMPA        #'+             ; CHECK FOR LEADING PLUS SlGN AND BRANCH
                BNE         LBD35           ; IF NOT A PLUS SIGN
LBD31           JSR         GETNCH          ; GET NEXT INPUT CHARACTER FROM BASIC
                BCS         LBD86           ; BRANCH IF NUMERIC CHARACTER
LBD35           CMPA        #'.             ; DECIMAL POlNT?
                BEQ         LBD61           ; YES
                CMPA        #'E             ; "E" SHORTHAND FORM (SCIENTIFIC NOTATION)?
                BNE         LBD65           ; NO
; EVALUATE EXPONENT OF EXPONENTIAL FORMAT
                JSR         GETNCH          ; GET NEXT INPUT CHARACTER FROM BASIC
                BCS         LBDA5           ; BRANCH IF NUMERIC
                CMPA        #$AC            ; MINUS TOKEN?
                BEQ         LBD53           ; YES
                CMPA        #'-             ; ASCII MINUS?
                BEQ         LBD53           ; YES
                CMPA        #$AB            ; PLUS TOKEN?
                BEQ         LBD55           ; YES
                CMPA        #'+             ; ASCII PLUS?
                BEQ         LBD55           ; YES
                BRA         LBD59           ; BRANCH IF NO SIGN FOUND
LBD53           COM         V48             ; SET EXPONENT SIGN FLAG TO NEGATIVE
; STRIP A DECIMAL NUMBER FROM BASIC LINE, CONVERT IT TO BINARY IN V47
LBD55           JSR         GETNCH          ; GET NEXT INPUT CHARACTER FROM BASIC
                BCS         LBDA5           ; IF NUMERIC CHARACTER, CONVERT TO BINARY
LBD59           TST         V48             ; CHECK EXPONENT SIGN FLAG
                BEQ         LBD65           ; AND BRANCH IF POSITIVE
                NEG         V47             ; NEGATE VALUE OF EXPONENT
                BRA         LBD65
LBD61           COM         V46             ; TOGGLE DECIMAL PT FLAG AND INTERPRET ANOTHER
                BNE         LBD31           ; CHARACTER IF <> 0 - TERMINATE INTERPRETATION
; IF SECOND DECIMAL POINT
; ADJUST FPA0 FOR THE DECIMAL EXPONENT IN V47
LBD65           LDA         V47             ; GET EXPONENT, SUBTRACT THE NUMBER OF
                SUBA        V45             ; PLACES TO THE RIGHT OF DECIMAL POINT
                STA         V47             ; AND RESAVE IT.
                BEQ         LBD7F           ; EXIT ROUTINE IF ADJUSTED EXPONENT = ZERO
                BPL         LBD78           ; BRANCH IF POSITIVE EXPONENT
LBD6F           JSR         >LBB82          ; DIVIDE FPA0 BY 10
                INC         V47             ; INCREMENT EXPONENT COUNTER (MULTIPLY BY 10)
                BNE         LBD6F           ; KEEP MULTIPLYING
                BRA         LBD7F           ; EXIT ROUTINE
LBD78           JSR         >LBB6A          ; MULTIPLY FPA0 BY 10
                DEC         V47             ; DECREMENT EXPONENT COUNTER (DIVIDE BY 10)
                BNE         LBD78           ; KEEP MULTIPLYING
LBD7F           LDA         COEFCT          ; GET THE SIGN FLAG
                BPL         LBD11           ; RETURN IF POSITIVE
                JMP         >LBEE9          ; TOGGLE MANTISSA SIGN OF FPA0, IF NEGATIVE
; FPA0 BY TEN AND ADD ACCA TO THE RESULT
LBD86           LDB         V45             ; GET THE RIGHT DECIMAL COUNTER AND SUBTRACT
                SUBB        V46             ; THE DECIMAL POINT FLAG FROM IT. IF DECIMAL POINT
                STB         V45             ; FLAG=0, NOTHING HAPPENS. IF DECIMAL POINT FLAG IS
; -1, THEN RIGHT DECIMAL COUNTER IS INCREMENTED BY ONE
                PSHS        A               ; SAVE NEW DIGIT ON STACK
                JSR         >LBB6A          ; MULTIPLY FPA0 BY 10
                PULS        B               ; GET NEW DIGIT BACK
                SUBB        #'0             ; MASK OFF ASCII
                BSR         LBD99           ; ADD ACCB TO FPA0
                BRA         LBD31           ; GET ANOTHER CHARACTER FROM BASIC
LBD99           JSR         >LBC2F          ; PACK FPA0 AND SAVE IT IN FPA3
                JSR         >LBC7C          ; CONVERT ACCB TO FP NUMBER IN FPA0
                LDX         #V40            ; ADD FPA0 TO
                JMP         >LB9C2          ; FPA3
; MULTIPLY V47 BY 10 AND ADD TO ASCII NUMBER IN
; ACCA - SAVE BINARY RESULT IN V47
LBDA5           LDB         V47
                ASLB                        ;  TIMES 2
                ASLB                        ;  TIMES 4
                ADDB        V47             ; ADD 1 = TIMES 5
                ASLB                        ;  TIMES 10
                SUBA        #'0             ; MASK OFF ASCII FROM ACCA, PUSH
                PSHS        B               ; RESULT ONTO THE STACK AND
                ADDA        ,S+             ; ADD lT TO ACCB
                STA         V47             ; SAVE IN V47
                BRA         LBD55           ; INTERPRET ANOTHER CHARACTER

LBDB6           FCB         $9B,$3E,$BC,$1F,$FD ; 99999999.9
LBDBB           FCB         $9E,$6E,$6B,$27,$FD ; 999999999
LBDC0           FCB         $9E,$6E,$6B,$28,$00 ; 1E + 09

LBDC5           LDX         #LABE8-1        ; POINT X TO " IN " MESSAGE
                BSR         LBDD6           ; COPY A STRING FROM (X) TO CONSOLE OUT
                LDD         CURLIN          ; GET CURRENT BASIC LINE NUMBER TO ACCD
; CONVERT VALUE IN ACCD INTO A DECIMAL NUMBER
; AND PRINT IT TO CONSOLE OUT
LBDCC           STD         FPA0            ; SAVE ACCD IN TOP HALF OF FPA0
                LDB         #$90            ; REQD EXPONENT IF TOP HALF OF ACCD = INTEGER
                COMA                        ;  SET CARRY FLAG - FORCE POSITIVE MANTISSA
                JSR         >LBC86          ; ZERO BOTTOM HALF AND SIGN OF FPA0, THEN
; SAVE EXPONENT AND NORMALIZE IT
                BSR         LBDD9           ; CONVERT FP NUMBER TO ASCII STRING
LBDD6           JMP         >LB99C          ; COPY A STRING FROM (X) TO CONSOLE OUT
; CONVERT FP NUMBER TO ASCII STRING
LBDD9           LDU         #STRBUF+3       ; POINT U TO BUFFER WHICH WILL NOT CAUSE
; THE STRING TO BE STORED IN STRING SPACE
LBDDC           LDA         #SPACE          ; SPACE = DEFAULT SIGN FOR POSITIVE #
                LDB         FP0SGN          ; GET SIGN OF FPA0
                BPL         LBDE4           ; BRANCH IF POSITIVE
                LDA         #'-             ; ASCII MINUS SIGN
LBDE4           STA         ,U+             ; STORE SIGN OF NUMBER
                STU         COEFPT          ; SAVE BUFFER POINTER
                STA         FP0SGN          ; SAVE SIGN (IN ASCII)
                LDA         #'0             ; ASCII ZERO IF EXPONENT = 0
                LDB         FP0EXP          ; GET FPA0 EXPONENT
                LBEQ        LBEB8           ; BRANCH IF FPA0 = 0
                CLRA                        ;  BASE 10 EXPONENT=0 FOR FP NUMBER > 1
                CMPB        #$80            ; CHECK EXPONENT
                BHI         LBDFF           ; BRANCH IF FP NUMBER > 1
; IF FPA0 < 1.0, MULTIPLY IT BY 1E+09 TO SPEED UP THE CONVERSION PROCESS
                LDX         #LBDC0          ; POINT X TO FP 1E+09
                JSR         >LBACA          ; MULTIPLY FPA0 BY (X)
                LDA         #-9             ; BASE 10 EXPONENT = -9
LBDFF           STA         V45             ; BASE 10 EXPONENT
; PSEUDO - NORMALIZE THE FP NUMBER TO A VALUE IN THE RANGE
; OF 999,999,999 RO 99,999,999.9 - THIS IS THE LARGEST
; NUMBER RANGE IN WHICH ALL OF THE DIGITS ARE
; SIGNIFICANT WHICH CAN BE DISPLAYED WITHOUT USING
; SCIENTIFIC NOTATION
LBE01           LDX         #LBDBB          ; POINT X TO FP 999,999,999
                JSR         >LBCA0          ; COMPARE FPA0 TO 999,999,999
                BGT         LBE18           ; BRANCH IF > 999,999,999
LBE09           LDX         #LBDB6          ; POINT X TO FP 99,999,999.9
                JSR         >LBCA0          ; COMPARE FPA0 TO 99,999,999.9
                BGT         LBE1F           ; BRANCH IF > 99,999,999.9 (IN RANGE)
                JSR         >LBB6A          ; MULTIPLY FPA0 BY 10
                DEC         V45             ; SUBTRACT ONE FROM DECIMAL OFFSET
                BRA         LBE09           ; PSEUDO - NORMALIZE SOME MORE
LBE18           JSR         >LBB82          ; DIVIDE FPA0 BY 10
                INC         V45             ; ADD ONE TO BASE 10 EXPONENT
                BRA         LBE01           ; PSEUDO - NORMALIZE SOME MORE
LBE1F           JSR         >LB9B4          ; ADD .5 TO FPA0 (ROUND OFF)
                JSR         >LBCC8          ; CONVERT FPA0 TO AN INTEGER
                LDB         #1              ; DEFAULT DECIMAL POINT FLAG (FORCE IMMED DECIMAL PT)
                LDA         V45             ; GET BASE 10 EXPONENT AND ADD TEN TO IT
                ADDA        #9+1            ; (NUMBER NORMALIZED TO 9 PLACES & DECIMAL PT)
                BMI         LBE36           ; BRANCH IF NUMBER < 1.0
                CMPA        #9+2            ; NINE PLACES MAY BE DISPLAYED WITHOUT
; USING SCIENTIFIC NOTATION
                BCC         LBE36           ; BRANCH IF SCIENTIFIC NOTATION REQUIRED
                DECA                        ;  SUBTRACT 1 FROM MODIFIED BASE 10 EXPONENT CTR
                TFR         A,B             ; AND SAVE IT IN ACCB (DECiMAL POINT FLAG)
                LDA         #2              ; FORCE EXPONENT = 0 - DON'T USE SCIENTIFIC NOTATION
LBE36           DECA                        ;  SUBTRACT TWO (WITHOUT AFFECTING CARRY)
                DECA                        ;  FROM BASE 10 EXPONENT
                STA         V47             ; SAVE EXPONENT - ZERO EXPONENT = DO NOT DISPLAY
; IN SCIENTIFIC NOTATION
                STB         V45             ; DECIMAL POINT FLAG - NUMBER OF PLACES TO
; LEFT OF DECIMAL POINT
                BGT         LBE4B           ; BRANCH IF >= 1
                LDU         COEFPT          ; POINT U TO THE STRING BUFFER
                LDA         #'.             ; STORE A PERIOD
                STA         ,U+             ; IN THE BUFFER
                TSTB                        ;  CHECK DECIMAL POINT FLAG
                BEQ         LBE4B           ; BRANCH IF NOTHING TO LEFT OF DECIMAL POINT
                LDA         #'0             ; STORE A ZERO
                STA         ,U+             ; IN THE BUFFER
; CONVERT FPA0 INTO A STRING OF ASCII DIGITS
LBE4B           LDX         #LBEC5          ; POINT X TO FP POWER OF 10 MANTISSA
                LDB         #0+$80          ; INITIALIZE DIGIT COUNTER TO 0+$80
; BIT 7 SET IS USED TO INDICATE THAT THE POWER OF 10 MANTISSA
; IS NEGATIVE. WHEN YOU 'ADD' A NEGATIVE MANTISSA, IT IS
; THE SAME AS SUBTRACTING A POSITIVE ONE AND BIT 7 OF ACCB IS HOW
; THE ROUTINE KNOWS THAT A 'SUBTRACTION' IS OCCURING.
LBE50           LDA         FPA0+3          ; ADD MANTISSA LS
                ADDA        3,X             ; BYTE OF FPA0
                STA         FPA0+3          ; AND (X)
                LDA         FPA0+2          ; ADD MANTISSA
                ADCA        2,X             ; NUMBER 3 BYTE OF
                STA         FPA0+2          ; FPA0 AND (X)
                LDA         FPA0+1          ; ADD MANTISSA
                ADCA        1,X             ; NUMBER 2 BYTE OF
                STA         FPA0+1          ; FPA0 AND (X)
                LDA         FPA0            ; ADD MANTISSA
                ADCA        ,X              ; MS BYTE OF
                STA         FPA0            ; FPA0 AND (X)
                INCB                        ;  ADD ONE TO DIGIT COUNTER
                RORB                        ;  ROTATE CARRY INTO BIT 7
                ROLB                        ;  SET OVERFLOW FLAG AND BRANCH IF CARRY = 1 AND
                BVC         LBE50           ; POSITIVE MANTISSA OR CARRY = 0 AND NEG MANTISSA
                BCC         LBE72           ; BRANCH IF NEGATIVE MANTISSA
                SUBB        #10+1           ; TAKE THE 9S COMPLEMENT IF
                NEGB                        ;  ADDING MANTISSA
LBE72           ADDB        #'0-1           ; ADD ASCII OFFSET TO DIGIT
                LEAX        4,X             ; MOVE TO NEXT POWER OF 10 MANTISSA
                TFR         B,A             ; SAVE DIGIT IN ACCA
                ANDA        #$7F            ; MASK OFF BIT 7 (ADD/SUBTRACT FLAG)
                STA         ,U+             ; STORE DIGIT IN STRING BUFFER
                DEC         V45             ; DECREMENT DECIMAL POINT FLAG
                BNE         LBE84           ; BRANCH IF NOT TIME FOR DECIMAL POINT
                LDA         #'.             ; STORE DECIMAL POINT IN
                STA         ,U+             ; STRING BUFFER
LBE84           COMB                        ;  TOGGLE BIT 7 (ADD/SUBTRACT FLAG)
                ANDB        #$80            ; MASK OFF ALL BUT ADD/SUBTRACT FLAG
                CMPX        #LBEC5+9*4      ; COMPARE X TO END OF MANTISSA TABLE
                BNE         LBE50           ; BRANCH IF NOT AT END OF TABLE
; BLANK TRAILING ZEROS AND STORE EXPONENT IF ANY
LBE8C           LDA         ,-U             ; GET THE LAST CHARACTER; MOVE POINTER BACK
                CMPA        #'0             ; WAS IT A ZERO?
                BEQ         LBE8C           ; IGNORE TRAILING ZEROS IF SO
                CMPA        #'.             ; CHECK FOR DECIMAL POINT
                BNE         LBE98           ; BRANCH IF NOT DECIMAL POINT
                LEAU        -1,U            ; STEP OVER THE DECIMAL POINT
LBE98           LDA         #'+             ; ASCII PLUS SIGN
                LDB         V47             ; GET SCIENTIFIC NOTATION EXPONENT
                BEQ         LBEBA           ; BRANCH IF NOT SCIENTIFIC NOTATION
                BPL         LBEA3           ; BRANCH IF POSITIVE EXPONENT
                LDA         #'-             ; ASCII MINUS SIGN
                NEGB                        ;  NEGATE EXPONENT IF NEGATIVE
LBEA3           STA         2,U             ; STORE EXPONENT SIGN IN STRING
                LDA         #'E             ; GET ASCII E (SCIENTIFIC NOTATION
                STA         1,U             ; FLAG) AND SAVE IT IN THE STRING
                LDA         #'0-1           ; INITIALIZE ACCA TO ASCII ZERO
; CONVERT BINARY VALUE IN ACCB TO DECIMAL
; ASCII NUMBER (< 100) IN ACCD
LBEAB           INCA                        ;  ADD ONE TO 10S DIGIT OF EXPONENT
                SUBB        #10             ; SUBTRACT 10 FROM ACCB
                BCC         LBEAB           ; ADD 1 TO 10S DIGIT IF NO CARRY
                ADDB        #'9+1           ; CONVERT UNITS DIGIT TO ASCII
                STD         3,U             ; SAVE EXPONENT IN STRING
                CLR         5,U             ; CLEAR LAST BYTE (TERMINATOR)
                BRA         LBEBC           ; GO RESET POINTER
LBEB8           STA         ,U              ; STORE LAST CHARACTER
LBEBA           CLR         1,U             ; CLEAR LAST BYTE (TERMINATOR - REQUIRED BY
; PRINT SUBROUTINES)
LBEBC           LDX         #STRBUF+3       ; RESET POINTER TO START OF BUFFER
                RTS

LBEC0           FCB         $80,$00,$00,$00,$00 ; FLOATING POINT .5

; TABLE OF UNNORMALIZED POWERS OF 10
LBEC5           FCB         $FA,$0A,$1F,$00 ; -100000000
LBEC9           FCB         $00,$98,$96,$80 ; 10000000
LBECD           FCB         $FF,$F0,$BD,$C0 ; -1000000
LBED1           FCB         $00,$01,$86,$A0 ; 100000
LBED5           FCB         $FF,$FF,$D8,$F0 ; -10000
LBED9           FCB         $00,$00,$03,$E8 ; 1000
LBEDD           FCB         $FF,$FF,$FF,$9C ; -100
LBEE1           FCB         $00,$00,$00,$0A ; 10
LBEE5           FCB         $FF,$FF,$FF,$FF ; -1


LBEE9           LDA         FP0EXP          ; GET EXPONENT OF FPA0
                BEQ         LBEEF           ; BRANCH IF FPA0 = 0
                COM         FP0SGN          ; TOGGLE MANTISSA SIGN OF FPA0
LBEEF           RTS
; EXPAND A POLYNOMIAL OF THE FORM
; AQ+BQ**3+CQ**5+DQ**7.... WHERE Q = FPA0
; AND THE X REGISTER POINTS TO A TABLE OF
; COEFFICIENTS A,B,C,D....
LBEF0           STX         COEFPT          ; SAVE COEFFICIENT TABLE POINTER
                JSR         >LBC2F          ; MOVE FPA0 TO FPA3
                BSR         LBEFC           ; MULTIPLY FPA3 BY FPA0
                BSR         LBF01           ; EXPAND POLYNOMIAL
                LDX         #V40            ; POINT X TO FPA3
LBEFC           JMP         >LBACA          ; MULTIPLY (X) BY FPA0
; CALCULATE THE VALUE OF AN EXPANDED POLYNOMIAL
; EXPRESSION. ENTER WITH (X) POINTING TO A TABLE
; OF COEFFICIENTS, THE FIRST BYTE OF WHICH IS THE
; NUMBER OF (COEFFICIENTS-1) FOLLOWED BY THAT NUMBER
; OF PACKED FLOATING POINT NUMBERS. THE
; POLYNOMIAL IS EVALUATED AS FOLLOWS: VALUE =
; (((FPA0*Y0+Y1)*FPA0+Y2)*FPA0YN)
LBEFF           STX         COEFPT          ; SAVE COEFFICIENT TABLE POINTER
LBF01           JSR         >LBC2A          ; MOVE FPA0 TO FPA4
                LDX         COEFPT          ; GET THE COEFFICIENT POINTER
                LDB         ,X+             ; GET THE TOP OF COEFFICIENT TABLE TO
                STB         COEFCT          ; USE AND STORE IT IN TEMPORARY COUNTER
                STX         COEFPT          ; SAVE NEW COEFFICIENT POINTER
LBF0C           BSR         LBEFC           ; MULTIPLY (X) BY FPA0
                LDX         COEFPT          ; GET COEFFICIENT POINTER
                LEAX        5,X             ; MOVE TO NEXT FP NUMBER
                STX         COEFPT          ; SAVE NEW COEFFICIENT POINTER
                JSR         >LB9C2          ; ADD (X) AND FPA0
                LDX         #V45            ; POINT (X) TO FPA4
                DEC         COEFCT          ; DECREMENT TEMP COUNTER
                BNE         LBF0C           ; BRANCH IF MORE COEFFICIENTS LEFT
                RTS
; RND
RND             JSR         >LBC6D          ; TEST FPA0
                BMI         LBF45           ; BRANCH IF FPA0 = NEGATIVE
                BEQ         LBF3B           ; BRANCH IF FPA0 = 0
                BSR         LBF38           ; CONVERT FPA0 TO AN INTEGER
                JSR         >LBC2F          ; PACK FPA0 TO FPA3
                BSR         LBF3B           ; GET A RANDOM NUMBER: FPA0 < 1.0
                LDX         #V40            ; POINT (X) TO FPA3
                BSR         LBEFC           ; MULTIPLY (X) BY FPA0
                LDX         #LBAC5          ; POINT (X) TO FP VALUE OF 1.0
                JSR         >LB9C2          ; ADD 1.0 TO FPA0
LBF38           JMP         >INT            ; CONVERT FPA0 TO AN INTEGER
; CALCULATE A RANDOM NUMBER IN THE RANGE 0.0 < X <= 1.0
LBF3B           LDX         RVSEED+1        ; MOVE VARIABLE
                STX         FPA0            ; RANDOM NUMBER
                LDX         RVSEED+3        ; SEED TO
                STX         FPA0+2          ; FPA0
LBF45           LDX         >RSEED          ; MOVE FIXED
                STX         FPA1            ; RANDOM NUMBER
                LDX         >RSEED+2        ; SEED TO
                STX         FPA1+2          ; MANTISSA OF FPA0
                JSR         >LBAD0          ; MULTIPLY FPA0 X FPA1
                LDD         VAD             ; GET THE TWO LOWEST ORDER PRODUCT BYTES
                ADDD        #$658B          ; ADD A CONSTANT
                STD         RVSEED+3        ; SAVE NEW LOW ORDER VARIABLE RANDOM # SEED
                STD         FPA0+2          ; SAVE NEW LOW ORDER BYTES OF FPA0 MANTISSA
                LDD         VAB             ; GET 2 MORE LOW ORDER PRODUCT BYTES
                ADCB        #$B0            ; ADD A CONSTANT
                ADCA        #5              ; ADD A CONSTANT
                STD         RVSEED+1        ; SAVE NEW HIGH ORDER VARIABLE RANDOM # SEED
                STD         FPA0            ; SAVE NEW HIGH ORDER FPA0 MANTISSA
                CLR         FP0SGN          ; FORCE FPA0 MANTISSA = POSITIVE
                LDA         #$80            ; SET FPA0 BIASED EXPONENT
                STA         FP0EXP          ; TO 0 1 < FPA0 < 0
                LDA         FPA2+2          ; GET A BYTE FROM FPA2 (MORE RANDOMNESS)
                STA         FPSBYT          ; SAVE AS SUB BYTE
                JMP         >LBA1C          ; NORMALIZE FPA0

RSEED           FDB         $40E6           ; CONSTANT RANDOM NUMBER GENERATOR SEED
                FDB         $4DAB
; SIN
; THE SIN FUNCTION REQUIRES AN ARGUMENT IN RADIANS AND WILL REPEAT ITSELF EVERY
; 2*PI RADIANS. THE ARGUMENT IS DIVIDED BY 2*PI AND ONLY THE FRACTIONAL PART IS
; RETAINED. SINCE THE ARGUMENT WAS DIVIDED BY 2*P1, THE COEFFICIENTS MUST BE
; MULTIPLIED BY THE APPROPRIATE POWER OF 2*PI.
; SIN IS EVALUATED USING THE TRIGONOMETRIC IDENTITIES BELOW:
; SIN(X)=SIN(PI-X) & -SIN(PI/2-X)=SIN((3*PI)/2+X)
SIN             JSR         >LBC5F          ; COPY FPA0 TO FPA1
                LDX         #LBFBD          ; POINT (X) TO 2*PI
                LDB         FP1SGN          ; GET MANTISSA SIGN OF FPA1
                JSR         >LBB89          ; AND DIVIDE FPA0 BY 2*PI
                JSR         >LBC5F          ; COPY FPA0 TO FPA1
                BSR         LBF38           ; CONVERT FPA0 TO AN INTEGER
                CLR         RESSGN          ; SET RESULT SIGN = POSITIVE
                LDA         FP1EXP          ; GET EXPONENT OF FPA1
                LDB         FP0EXP          ; GET EXPONENT OF FPA0
                JSR         >LB9BC          ; SUBTRACT FPA0 FROM FPA1
; FPA0 CONTAINS ONLY THE FRACTIONAL PART OF ARGUMENT/2*PI
                LDX         #LBFC2          ; POINT X TO FP (.25)
                JSR         >LB9B9          ; SUBTRACT FPA0 FROM .25 (PI/2)
                LDA         FP0SGN          ; GET MANTISSA SIGN OF FPA0
                PSHS        A               ; SAVE IT ON STACK
                BPL         LBFA6           ; BRANCH IF MANTISSA POSITIVE
                JSR         >LB9B4          ; ADD .5 (PI) TO FPA0
                LDA         FP0SGN          ; GET SIGN OF FPA0
                BMI         LBFA9           ; BRANCH IF NEGATIVE
                COM         RELFLG          ; COM IF +(3*PI)/2 >= ARGUMENT >+ PI/2 (QUADRANT FLAG)
LBFA6           JSR         >LBEE9          ; TOGGLE MANTISSA SIGN OF FPA0
LBFA9           LDX         #LBFC2          ; POINT X TO FP (.25)
                JSR         >LB9C2          ; ADD .25 (PI/2) TO FPA0
                PULS        A               ; GET OLD MANTISSA SIGN
                TSTA                        ;  BRANCH IF OLD
                BPL         LBFB7           ; SIGN WAS POSITIVE
                JSR         >LBEE9          ; TOGGLE MANTISSA SIGN
LBFB7           LDX         #LBFC7          ; POINT X TO TABLE OF COEFFICIENTS
                JMP         >LBEF0          ; GO CALCULATE POLYNOMIAL VALUE
LBFBD           FCB         $83,$49,$0F,$DA,$A2 ; 6.28318531 (2*PI)
LBFC2           FCB         $7F,$00,$00,$00,$00 ; .25
; MODIFIED TAYLOR SERIES SIN COEFFICIENTS
LBFC7           FCB         6-1             ; SIX COEFFICIENTS
LBFC8           FCB         $84,$E6,$1A,$2D,$1B ; -((2*PI)**11)/11!
LBFCD           FCB         $86,$28,$07,$FB,$F8 ; ((2*PI)**9)/9!
LBFD2           FCB         $87,$99,$68,$89,$01 ; -((2*PI)**7)/7!
LBFD7           FCB         $87,$23,$35,$DF,$E1 ; ((2*PI)**5)/5!
LBFDC           FCB         $86,$A5,$5D,$E7,$28 ; -((2*PI)**3)/3!
LBFE1           FCB         $83,$49,$0F,$DA,$A2 ; 2*PI


; apparently, the following 10 bytes are remnants of an old microsoft
; easter egg present in their commodore basic. it's been suggested
; that this was inserted personally by bill gates after an argument
; with jack tramiel.
;
; basically the string "MICROSOFT!" was encoded backwards and was
; printable via some command on the commodore pet. this was
; presumably to be able to prove later that the code originated
; from microsoft in case someone copied it.
;
; the code to display it is gone, but apparently when porting to the
; coco, someone just copy pasted the extra "unknown data" which
; followed the 6 coefficients for SIN().
;
; note PET had its own "petascii" encoding - and the upper two bits
; are masked off in the display code on the pet. thus the encoding
; here is just randomizing the upper two bits.
;
; ( info was found at http://www.pagetable.com/?p=43 )

LBFE6           FCB         $a1             ; '! ^ $80
                FCB         $54             ; 'T ^ $00
                FCB         $46             ; 'F ^ $00
                FCB         $8f             ; 'O ^ $c0
                FCB         $13             ; 'S ^ $40
                FCB         $8f             ; 'O ^ $c0
                FCB         $52             ; 'R ^ $00
                FCB         $43             ; 'C ^ $00
                FCB         $89             ; 'I ^ $c0
                FCB         $cd             ; 'M ^ $80



; INTERRUPT VECTORS
LBFF0           FDB         LA681           ; RESERVED

; -----------------------------------------------------------------------------
                if          VERBAS<20
; -----------------------------------------------------------------------------
LBFF2           FDB         SW3VEC          ; SWI3
LBFF4           FDB         SW2VEC          ; SWI2
LBFF6           FDB         FRQVEC          ; FIRQ
LBFF8           FDB         IRQVEC          ; IRQ
LBFFA           FDB         SWIVEC          ; SWI
LBFFC           FDB         NMIVEC          ; NMI
LBFFE           FDB         RESVEC          ; RESET
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FDB         $FEEE           ; INT.SWI3
                FDB         $FEF1           ; INT.SWI2
                FDB         $FEF4           ; INT.FIRQ
                FDB         $FEF7           ; INT.IRQ
                FDB         $FEFA           ; INT.SWI
                FDB         $FEFD           ; INT.NMI
                FDB         $8C1B           ; DLDBUG
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                END
