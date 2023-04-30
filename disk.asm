
; TODO disk basic 1.0 and 1.1 haven't been merged at all yet.
; TODO (they are just here back to back in a big if.. else..)

; -----------------------------------------------------------------------------
                if          VERDISK>10
; -----------------------------------------------------------------------------

; Disk Color BASIC 1.1
; Copied from the PDF version of Disk Color BASIC Unravelled.
; Fixed up to assemble in Mamou

; Revision History

; $Id: $

DHITOK          EQU         $E1             ; HIGHEST 1.1 DISK TOKEN
CYEAR           EQU         '2




; FILE ALLOCATION TABLE FORMAT


; THE FILE ALLOCATION TABLE (FAT) CONTAINS THE STATUS OF THE GRANULES ON A DISKETTE.
; THE FAT CONTAINS 6 CONTROL BYTES FOLLOWED BY 68 DATA BYTES (ONE PER GRANULE). ONLY THE
; FIRST TWO OF THE SIX CONTROL BYTES ARE USED. A VALUE OF $FF IS SAVED IN UNALLOCATED
; GRANULES. IF BITS 6 & 7 OF THE DATA BYTE ARE SET, THE GRANULE IS THE LAST GRANULE
; IN A FILE AND BITS 0-5 ARE THE NUMBER OF USED SECTORS IN THAT GRANULE. IF BITS 6 & 7
; ARE NOT SET, THE DATA BYTE CONTAINS THE NUMBER OF THE NEXT GRANULE IN THE FILE.

; OFFSETS TO FAT CONTROL BYTES
FAT0            EQU         0               ; ACTIVE FILE COUNTER : DISK TO RAM FAT IMAGE DISABLE
FAT1            EQU         1               ; VALID DATA FLAG: 0=DISK DATA VALID, <> 0 = NEW FAT
; DATA - DISK DATA INVALID
; 2 TO 5 NOT USED
FATCON          EQU         6               ; OFFSET TO START OF FAT DATA (68 BYTES)


; DIRECTORY ENTRY FORMAT


; THE DIRECTORY IS USED TO KEEP TRACK OF HOW MANY FILES ARE STORED ON A DISKETTE
; AND WHERE THE FILE IS STORED ON THE DISK. THE FIRST GRANULE USED BY THE FILE WILL
; ALLOW THE FAT TO TRACK DOWN ALL OF THE GRANULES USED BY THE FILE. IF THE FIRST
; BYTE OF THE DIRECTORY ENTRY IS ZERO, THE FILE HAS BEEN KILLED;
; IF THE FIRST BYTE IS $FF THEN THE DIRECTORY ENTRY HAS NEVER BEEN USED.

; BYTE DESCRIPTION
DIRNAM          EQU         0               ; FILE NAME
DIREXT          EQU         8               ; FILE EXTENSION
DIRTYP          EQU         11              ; FILE TYPE
DIRASC          EQU         12              ; ASCII FLAG
DIRGRN          EQU         13              ; FIRST GRANULE IN FILE
DIRLST          EQU         14              ; NUMBER OF BYTES IN LAST SECTOR
; 16 TO 31 UNUSED


; FILE CONTROL BLOCK FORMAT


; THE FILE STRUCTURE OF COLOR TRS DOS IS CONTROLLED BY A FILE CONTROL BLOCK (FCB)
; THE FCB CONTAINS 25 CONTROL BYTES AND A SECTOR LONG (256 BYTES) DATA BUFFER.
; THE CONTROL BYTES CONTROL THE ORDERLY FLOW OF DATA FROM THE COMPUTER'S RAM TO
; THE DISKETTE AND VICE VERSA. THE OPEN COMMAND INITIALIZES THE FCB; THE INPUT,
; OUTPUT, WRITE, PRINT, GET AND PUT COMMANDS TRANSFER DATA THROUGH THE FCB AND
; THE CLOSE COMMAND TURNS OFF THE FCB.

; TABLES OF OFFSETS TO FCB CONTROL BYTES

; RANDOM FILE
; BYTE DESCRIPTION
FCBTYP          EQU         0               ; FILE TYPE: $40=RANDOM/DIRECT, 0=CLOSED
FCBDRV          EQU         1               ; DRIVE NUMBER
FCBFGR          EQU         2               ; FIRST GRANULE IN FILE
FCBCGR          EQU         3               ; CURRENT GRANULE BEING USED
FCBSEC          EQU         4               ; CURRENT SECTOR BEING USED (1-9)
; 5 UNUSED
FCBPOS          EQU         6               ; CURRENT PRINT POSITION - ALWAYS ZERO IN RANDOM FILES
FCBREC          EQU         7               ; CURRENT RECORD NUMBER
FCBRLN          EQU         9               ; RANDOM FILE RECORD LENGTH
FCBBUF          EQU         11              ; POINTER TO START OF THIS FILE'S RANDOM ACCESS BUFFER
FCBSOF          EQU         13              ; SECTOR OFFSET TO CURRENT POSITION IN RECORD
FCBFLG          EQU         15              ; GET/PUT FLAG: 0=PUT, 1=PUT
; 16,17 NOT USED
FCBDIR          EQU         18              ; DIRECTORY ENTRY NUMBER (0-71)
FCBLST          EQU         19              ; NUMBER OF BYTES IN LAST SECTOR OF FILE
FCBGET          EQU         21              ; 'GET' RECORD COUNTER: HOW MANY CHARACTERS HAVE BEEN PULLED OUT OF THE CURRENT RECORD
FCBPUT          EQU         23              ; 'PUT' RECORD COUNTER: POINTER TO WHERE IN THE RECORD THE NEXT BYTE WILL BE 'PUT'
FCBCON          EQU         25              ; OFFSET TO START OF FCB DATA BUFFER (256 BYTES)

; SEQUENTIAL FILE
; BYTE DESCRIPTION
FCBCPT          EQU         5               ; INPUT FILE: CHARACTER POINTER - POINTS TO NEXT CHARACTER IN
; FILE TO BE PROCESSED.
; OUTPUT FILE: FULL SECTOR FLAG - IF IT IS 1 WHEN THE FILE IS
; CLOSED IT MEANS 256 BYTES OF THE LAST SECTOR HAVE BEEN USED.
; INPUT OR OUTPUT TO A FILE.
; 9 TO 15 UNUSED
FCBCFL          EQU         16              ; CACHE FLAG: 00=CACHE EMPTY, $FF=CACHE FULL
FCBCDT          EQU         17              ; CACHE DATA BYTE
; 21,22 UNUSED
FCBDFL          EQU         23              ; INPUT FILE ONLY: DATA LEFT FLAG: 0=DATA LEFT, $FF=NO DATA (EMPTY)
FCBLFT          EQU         24              ; NUMBER OF CHARACTERS LEFT IN BUFFER (INPUT FILE)
; NUMBER OF CHARS STORED IN BUFFER (OUTPUT FILE)

                ORG         $C000

                FCC         'DK'
                BRA         LRC00C
DCNVEC          FDB         DSKCON          ; DSKCON POINTER
DSKVAR          FDB         DCOPC           ; ADDRESS OF DSKCON VARIABLES
DSINIT          FDB         DOSINI          ; DISK INITIALIZATION VECTOR
DOSVEC          FDB         DOSCOM          ; DOS COMMAND VECTOR
; ZERO OUT THE RAM USED BY DISK BASIC
LRC00C          LDX         #DBUF0          ; POINT X TO START OF DISK RAM
LRC00F          CLR         ,X+             ; CLEAR A BYTE
                CMPX        #DFLBUF         ; END OF DISK'S RAM?
                BNE         LRC00F          ; NO - KEEP CLEARING
                LDX         #LC109          ; POINT X TO ROM IMAGE OF COMMAND INTERPRETATION TABLE
                LDU         #COMVEC+20      ; POINT U TO RAM ADDRESS OF SAME
                LDB         #10             ; 10 BYTES PER TABLE
                JSR         >LA59A          ; MOVE (B) BYTES FROM (X) TO (U)
                LDD         #LB277          ; SYNTAX ERROR ADDRESS
                STD         $03,U           ; SET JUMP TABLE ADDRESSES OF THE USER COMMAND
                STD         $08,U           ; INTERPRETATION TABLE TO POINT TO SYNTAX ERROR
                CLR         ,U              ; CLEAR BYTE 0 OF USER TABLE (DOESN'T EXIST FLAG)
                CLR         $05,U           ; SET NUMBER OF SECONDARY USER TOKENS TO ZERO
                LDD         #DXCVEC         ; SAVE NEW
                STD         COMVEC+13       ; POINTERS TO EXBAS
                LDD         #DXIVEC         ; COMMAND AND SECONDARY
                STD         COMVEC+18       ; COMMAND INTERPRETATION ROUTINES
; MOVE THE NEW RAM VECTORS FROM ROM TO RAM
                LDU         #RVEC0          ; POINT U TO 1ST RAM VECTOR
LC03B           LDA         #$7E            ; OP CODE OF JMP INSTRUCTION
                STA         RVEC22          ; SET 1ST BYTE OF 'GET'/'PUT' RAM VECTOR TO 'JMP'
                STA         ,U+             ; SET 1ST BYTE OF RAM VECTOR TO 'JMP'
                LDD         ,X++            ; GET RAM VECTOR FROM ROM
                STD         ,U++            ; STORE IT IN RAM
                CMPX        #LC139          ; COMPARE TO END OF ROM VALUES
                BNE         LC03B           ; BRANCH IF NOT ALL VECTORS MOVED
                LDX         #DVEC22         ; GET ROM VALUE OF 'GET'/'PUT' RAM VECTOR
                STX         RVEC22+1        ; SAVE IT IN RAM
                LDX         #DVEC20         ; GET DISK COMMAND INTERPRETATION LOOP RAM VECTOR
                STX         RVEC20+1        ; SAVE IN RAM VECTOR TABLE
; INITIALIZE DISK BASIC'S USR VECTORS
                LDX         #DUSRVC         ; POINT X TO START OF DISK BASIC USR VECTORS
                STX         USRADR          ; SAVE START ADDRESS IN USRADR
                LDU         #LB44A          ; POINT U TO ADDRESS OF 'FUNCTION CALL' ERROR
                LDB         #$0A            ; 10 USER VECTORS TO INITIALIZE
LC061           STU         ,X++            ; SET USR VECTOR TO 'FC' ERROR
                DECB                        ;  DECREMENT USR VECTOR COUNTER
                BNE         LC061           ; BRANCH IN NOT DONE WITH ALL 10 VECTORS
                LDX         #DNMISV         ; GET ADDRESS OF NMI SERVICING ROUTINE
                STX         NMIVEC+1        ; SAVE IT IN NMI VECTOR
                LDA         #$7E            ; OP CODE OF JMP
                STA         NMIVEC          ; MAKE THE NMI VECTOR A JMP
                LDX         #DIRQSV         ; GET ADDRESS OF DISK BASIC IRQ SERVICING ROUTINE
                STX         IRQVEC+1        ; SAVE IT IN IRQVEC
                LDA         #$13            ; INITIALIZE WRITE FAT
                STA         WFATVL          ; TO DISK TRIGGER VALUE
                CLR         FATBL0          ;
                CLR         FATBL1          ; INITIALIZE THE ACTIVE FILE COUNTER OF
                CLR         FATBL2          ; EACH FAT TO ZERO. THIS WILL CAUSE THE FATS
                CLR         FATBL3          ; TO THINK THERE ARE NO ACTIVE FILES
                LDX         #DFLBUF         ; GET THE STARTING ADDRESS OF THE
                STX         RNBFAD          ; RANDOM FILE BUFFER FREE AREA AND DAVE IT AS THE
; START ADDRESS OF FREE RAM FOR RANDOM FILE BUFFERS
                LEAX        $0100,X         ; SAVE 256 BYTES FOR RANDOM FILE BUFFERS INITIALLY
                STX         FCBADR          ; SAVE START ADDRESS OF FCBS
                LEAX        $01,X           ; ADD ONE AND SAVE THE STARTING
                STX         FCBV1           ; ADDRESS OF FCB1
                CLR         FCBTYP,X        ; CLEAR THE FIRST BYTE OF FCB 1 (CLOSE FCB)
                LEAX        FCBLEN,X        ; POINT X TO FCB 2
                STX         FCBV1+2         ; SAVE ITS STARTING ADDRESS IN FCB VECTOR TABLE
                CLR         FCBTYP,X        ; CLEAR THE FIRST BYTE OF FCB 2 (CLOSE FCB)
                LEAX        FCBLEN,X        ; POINT X TO SYSTEM FCB - THIS FCB WILL ONLY
; BE USED TO COPY, LOAD, SAVE, MERGE, ETC
                STX         FCBV1+4         ; SAVE ITS ADDRESS IN THE FCB VECTOR TABLE
                CLR         FCBTYP,X        ; CLEAR THE FIRST BYTE OF SYSTEM FCB (CLOSE FCB)
                LDA         #$02            ; SET THE NUMBER OF ACTIVE RESERVED
                STA         FCBACT          ; FILE BUFFERS TO 2 (1,2)
                LEAX        FCBLEN,X        ; POINT X TO ONE PAST THE END OF SYSTEM FCB
                TFR         X,D             ; SAVE THE ADDRESS IN ACCD
                TSTB                        ;  ON AN EVEN 256 BYTE BOUNDARY?
                BEQ         LC0BD           ; YES
                INCA                        ;  NO - ADD 256 TO ADDRESS
LC0BD           BITA        #$01            ; CHECK TO SEE IF ACCD IS ON AN EVEN
                BEQ         LC0C2           ; 512 BYTE (ONE GRAPHIC PAGE) BOUNDARY - ADD
                INCA                        ;  256 (INCA) TO IT IF NOT
LC0C2           TFR         A,B             ; COPY ACCA TO ACCB
                ADDB        #$18            ; SAVE ENOUGH ROOM FOR 4 GRAPHICS PAGES (PCLEAR 4)
                STB         TXTTAB          ; SAVE NEW START OF BASIC ADDRESS
                JSR         >L96EC          ; INITIALIZE EXBAS VARIABLES & DO A NEW
                LDA         BEGGRP          ; GET THE START OF CURRENT GRAPHICS PAGE
                ADDA        #$06            ; ADD 1.5K (6 X 256 = ONE GRAPHICS PAGE)
                STA         ENDGRP          ; SAVE NEW END OF GRAPHICS PAGE
                JSR         [DSINIT]        ; INITIALIZE SWI2,3 JUMP ADDRESSES
                BSR         LC0F0           ; GO INITIALIZE THE FLOPPY DISK CONTROLLER
                ANDCC       #$AF            ; TURN ON IRQ AND FIRQ
                LDX         #LC139-1        ; POINT X TO DISK BASIC COPYRIGHT MESSAGE
LC0DC           JSR         STRINOUT        ; PRINT COPYRIGHT MESSAGE TO SCREEN
                LDX         #DKWMST         ; GET DISK BASIC WARM START ADDRESS
                STX         RSTVEC          ; SAVE IT IN RESET VECTOR
                JMP         >LA0E2          ; JUMP BACK TO BASIC
DKWMST          NOP                         ;  WARM START INDICATOR
                BSR         LC0F0           ; INITIALIZE THE FLOPPY DISK CONTROLLER
                JSR         >LD2D2          ; CLOSE FILES AND DO MORE INITIALIZATION
                JMP         XBWMST          ; JUMP TO EXBAS' WARM START
LC0F0           CLR         NMIFLG          ; RESET NMI FLAG
                CLR         RDYTMR          ; RESET DRIVE NOT READY TIMER
                CLR         DRGRAM          ; RESET RAM IMAGE OF DSKREG (MOTORS OFF)
                CLR         DSKREG          ; RESET DISK CONTROL REGISTER
                LDA         #$D0            ; FORCE INTERRUPT COMMAND OF 1793
                STA         FDCREG          ; SEND IT TO 1793
                EXG         A,A             ; DELAY
                EXG         A,A             ; DELAY SOME MORE
                LDA         FDCREG          ; GET 1793 STATUS (CLEAR REGISTER)
                RTS

; DISK BASIC COMMAND INTERP TABLES
LC109           FCB         20              ; 20 DISK BASIC 1.1 COMMANDS
                FDB         LC192           ; DISK BASIC'S COMMAND DICTIONARY
                FDB         LC238           ; COMMAND JUMP TABLE
                FCB         06              ; 6 DISK BASIC SECONDARY FUNCTIONS
                FDB         LC219           ; SECONDARY FUNCTION TABLE
                FDB         LC24E           ; SECONDARY FUNCTION JUMP TABLE

; RAM HOOKS FOR DISK BASIC
LC113           FDB         DVEC0,DVEC1,DVEC2
                FDB         DVEC3,DVEC4,DVEC5
                FDB         DVEC6,DVEC7,DVEC8
                FDB         XVEC9,DVEC10,DVEC11
                FDB         DVEC12,DVEC13,DVEC14
                FDB         DVEC15,DVEC12
                FDB         DVEC17,DVEC18

; DISK BASIC COPYRIGHT MESSAGE
LC139           FCC         'DISK EXTENDED COLOR BASIC 1.1'
                FCB         CR
                FCC         'COPYRIGHT (C) 198'
                FCB         CYEAR
                FCC         ' BY TANDY'
                FCB         CR
                FCC         'UNDER LICENSE FROM MICROSOFT'
                FCB         CR,CR,0

; DISK BASIC COMMAND DICTIONARY TABLE
; TOKEN #
LC192           FCS         'DIR'           ; CE
                FCS         'DRIVE'         ; CF
                FCS         'FIELD'         ; D0
                FCS         'FILES'         ; D1
                FCS         'KILL'          ; D2
                FCS         'LOAD'          ; D3
                FCS         'LSET'          ; D4
                FCS         'MERGE'         ; D5
                FCS         'RENAME'        ; D6
                FCS         'RSET'          ; D7
                FCS         'SAVE'          ; D8
                FCS         'WRITE'         ; D9
                FCS         'VERIFY'        ; DA
                FCS         'UNLOAD'        ; DB
                FCS         'DSKINI'        ; DC
                FCS         'BACKUP'        ; DD
                FCS         'COPY'          ; DE
                FCS         'DSKI$'         ; DF
                FCS         'DSKO$'         ; E0

                FCS         'DOS'           ; E1

; DISK BASIC COMMAND JUMP TABLE
; COMMAND / TOKEN #
LC1F1           FDB         DIR             ; DIR / CE
                FDB         DRIVE           ; DRIVE / CF
                FDB         FIELD           ; FIELD / D0
                FDB         FILES           ; FILES / D1
                FDB         KILL            ; KILL / D2
                FDB         LOAD            ; LOAD / D3
                FDB         LSET            ; LSET / D4
                FDB         MERGE           ; MERGE / D5
                FDB         RENAME          ; RENAME / D6
                FDB         RSET            ; RSET / D7
                FDB         SAVE            ; SAVE / D8
                FDB         WRITE           ; WRITE / D9
                FDB         VERIFY          ; VERIFY / DA
                FDB         UNLOAD          ; UNLOAD / DB
                FDB         DSKINI          ; DSKINI /DC
                FDB         BACKUP          ; BACKUP / DD
                FDB         COPY            ; COPY / DE
                FDB         DSKI            ; DSKI$ / DF
                FDB         DSKO            ; DSKO$ / E0

                FDB         DOS             ; DOS / E1

; SECONDARY FUNCTION DICTIONARY TABLE
; TOKEN #
LC219           FCS         'CVN'           ; A2
                FCS         'FREE'          ; A3
                FCS         'LOC'           ; A4
                FCS         'LOF'           ; A5
                FCS         'MKN$'          ; A6
                FCS         'AS'            ; A7

; DISK BASIC SECONDARY FUNCTION JUMP TABLE
; FUNCTION / TOKEN #
LC22C           FDB         CVN             ; CVN / A2
                FDB         FREE            ; FREE / A3
                FDB         LOC             ; LOC / A4
                FDB         LOF             ; LOF / A5
                FDB         MKN             ; MKN$ / A6
                FDB         LB277           ; AS / A7
; DISK BASIC COMMAND INTERPRETATION HANDLER
LC238           CMPA        #DHITOK         ; COMPARE TO HIGHEST DISK BASIC TOKEN
                BHI         LC244           ; AND BRANCH IF HIGHER
                LDX         #LC1F1          ; POINT X TO DISK BASIC COMMAND JUMP TABLE
                SUBA        #$CE            ; SUBTRACT OUT LOWEST DISK BASIC COMMAND TOKEN
                JMP         >LADD4          ; JUMP TO BASIC'S COMMAND HANDLER
LC244           CMPA        #DHITOK         ; COMPARE TO HIGHEST DISK BASIC TOKEN
                LBLS        LB277           ; 'SYNTAX' ERROR IF < DISK BASIC COMMAND TOKEN
                JMP         [COMVEC+33]     ; PROCESS A USER COMMAND TOKEN
; DISK BASIC SECONDARY COMMAND INTERPRETATION HANDLER
LC24E           CMPB        #($A7-$80)*2    ; COMPARE MODIFIED SECONDARY TOKEN TO
                BLS         LC256           ; HIGHEST DISK BASIC TOKEN & BRANCH IF HIGHER
                JMP         [COMVEC+38]     ; JUMP TO USER SECONDARY COMMAND HANDLER
LC256           SUBB        #($A2-$80)*2    ; SUBTRACT OUT THE SMALLEST SECONDARY
                PSHS        B               ; DISK TOKEN & SAVE MODIFIED TOKEN ON THE STACK
                JSR         >LB262          ; SYNTAX CHECK FOR '(' AND EVALUATE EXPRESSION
                PULS        B               ; RESTORE MODIFIED TOKEN
                LDX         #LC22C          ; POINT X TO SECONDARY COMMAND JUMP TABLE
                JMP         >LB2CE          ; JUMP TO BASIC'S SECONDARY COMMAND HANDLER

; ERROR DRIVER RAM VECTOR
DVEC17          PULS        Y               ; PUT THE RETURN ADDRESS INTO Y
                JSR         >LAD33          ; RESET THE CONT FLAG, ETC
                JSR         >LD2D2          ; INITIALIZE SOME DISK VARIABLES AND CLOSE FILES
                PSHS        Y,B             ; PUT RETURN ADDRESS AND ERROR NUMBER ON THE STACK
                JSR         >DVEC7          ; CLOSE ALL FILES
                PULS        B               ; GET THE ERROR NUMBER BACK
                CMPB        #2*27           ; COMPARE TO THE LOWEST DISK ERROR NUMBER
                LBCS        XVEC17          ; BRANCH TO EXBAS ERROR HANDLER IF NOT DISK ERROR NUMBER
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF THE STACK
                JSR         >LA7E9          ; TURN OFF THE CASSETTE MOTOR
                JSR         >LA974          ; DISABLE THE ANALOG MULTIPLEXER
                CLR         DEVNUM          ; SET DEVICE NUMBER TO THE SCREEN
                JSR         >LB95C          ; SEND A CR TO THE SCREEN
                JSR         >LB9AF          ; SEND A '?' TO THE SCREEN
                LDX         #LR_DBERTAB-2*27     ; POINT X TO DISK BASIC'S ERROR TABLE (MINUS LOWEST DISK ERROR NUMBER)
                JMP         >LAC60          ; JUMP TO BASIC'S ERROR HANDLER

; DISK BASIC ERROR MESSAGES
LR_DBERTAB      FCC         'BR'            ; 27 BAD RECORD
                FCC         'DF'            ; 28 DISK FULL
                FCC         'OB'            ; 29 OUT OF BUFFER SPACE
                FCC         'WP'            ; 30 WRITE PROTECTED
                FCC         'FN'            ; 31 BAD FILE NAME
                FCC         'FS'            ; 32 BAD FILE STRUCTURE
                FCC         'AE'            ; 33 FILE ALREADY EXISTS
                FCC         'FO'            ; 34 FIELD OVERFLOW
                FCC         'SE'            ; 35 SET TO NON-FIELDED STRING
                FCC         'VF'            ; 36 VERIFICATION ERROR
                FCC         'ER'            ; 37 WRITE OR INPUT PAST END OF RECORD

; DISK FILE EXTENSIONS
BASEXT          FCC         'BAS'           ; BASIC FILE EXTENSION
DEFEXT          FCC         "   "           ; BLANK (DEFAULT) FILE EXTENSION
DATEXT          FCC         'DAT'           ; DATA FILE EXTENSION
BINEXT          FCC         'BIN'           ; BINARY FILE EXTENSION

; CLS RAM VECTOR
DVEC22          PSHS        X,CC            ; SAVE X REG AND STATUS
                LDX         $03,S           ; LOAD X WITH CALLING ADDRESS
                CMPX        #L975F          ; COMING FROM EXBAS' GET/PUT?
                BNE         LC2BF           ; NO
                CMPA        #'#             ; NUMBER SIGN (GET#, PUT#)?
                BEQ         LC2C1           ; BRANCH IF GET OR PUT TO RANDOM FILE
LC2BF           PULS        CC,X,PC         ; RESTORE X REG, STATUS AND RETURN

; GET/PUT TO A DIRECT/RANDOM FILE
LC2C1           LEAS        $05,S           ; PURGE RETURN ADDRESS AND REGISTERS OFF OF THE STACK
                JSR         >LC82E          ; EVALUATE DEVICE NUMBER & SET FCB POINTER
                STX         FCBTMP          ; SAVE FCB POINTER
                CLR         FCBGET,X        ; RESET THE GET
                CLR         FCBGET+1,X      ; DATA POINTER
                CLR         FCBPUT,X        ; RESET THE PUT
                CLR         FCBPUT+1,X      ; DATA POINTER
                CLR         FCBPOS,X        ; RESET PRINT POSITION COUNTER
                LDA         FCBDRV,X        ; GET THE FCB DRIVE NUMBER AND
                STA         DCDRV           ; SAVE IT IN DSKCON VARIABLE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER FROM BASIC
                BEQ         LC2EA           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVALUATE EXPRESSION - RETURN IN (X)
                TFR         X,D             ; SAVE RECORD NUMBER IN ACCD
LC2E6           LDX         FCBTMP          ; POINT X TO FCB
                STD         FCBREC,X        ; SAVE RECORD NUMBER IN FCB
LC2EA           LDD         FCBREC,X        ; GET RECORD NUMBER
                BEQ         LC30B           ; 'BAD RECORD' ERROR IF RECORD NUMBER = 0
                JSR         >LC685          ; INCREMENT RECORD NUMBER
                LDD         FCBRLN,X        ; GET RANDOM FILE RECORD LENGTH AND RANDOM FILE
                LDX         FCBBUF,X        ; BUFFER POINTER AND SAVE THEM ON THE STACK -
                PSHS        X,B,A           ; THESE ARE THE INITIAL VALUES OF A TEMPORARY
; RECORD LENGTH COUNTER AND RANDOM BUFFER
; POINTER WHICH ARE MAINTAINED ON THE STACK
                LEAX        -2,U            ; POINT X TO (RECORD NUMBER -1)
                JSR         >L9FB5          ; MULT (UNSIGNED) RECORD LENGTH X (RECORD NUMBER -1)
                PSHS        U,Y             ; SAVE PRODUCT ON THE STACK
                LDA         ,S+             ; CHECK MS BYTE OF PRODUCT
                BNE         LC30B           ; 'BR' ERROR IF NOT ZERO (RECORD NUMBER TOO BIG)
                PULS        X               ; PULL THE BOTTOM 3 PRODUCT BYTES OFF THE STACK;
                PULS        B               ; TOP TWO IN X, BOTTOM IN ACCB; ACCB POINTS TO
; THE FIRST BYTE OF THE SECTOR USED BY THIS RECORD,
; (X) CONTAINS THE SECTOR OFFSET (IN WHICH SECTOR
; FROM THE START THE BYTE IS LOCATED)
LC306           CMPX        #(TRKMAX-1)*18  ; 612 SECTORS MAX IN A RANDOM FILE
                BLO         LC310           ; BRANCH IF RECORD LENGTH O.K.
LC30B           LDB         #2*27           ; 'BAD RECORD' ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
LC310           LDU         FCBTMP          ; POINT U TO FCB
                CMPX        FCBSOF,U        ; COMPARE SAVED SECTOR OFFSET TO THE CURRENT SECTOR OFFSET
                LBEQ        LC3CF           ; BEING PROCESSED - DO NOT PROCESS A NEW SECTOR IF THEY ARE EQUAL
                PSHS        X,B             ; SAVE BYTE AND SECTOR OFFSET TO RECORD START ON STACK
                LDA         FCBFLG,U        ; CHECK FCB GET/PUT FLAG AND
                BEQ         LC324           ; BRANCH IF IT WAS A GET
                CLR         FCBFLG,U        ; FORCE GET/PUT TO 'PUT'
                LDB         #$03            ; DSKCON WRITE OP CODE
                BSR         LC357           ; GO WRITE A SECTOR - SAVE 'PUT' DATA ON DISK
; CONVERT THE SECTOR OFFSET TO A GRANULE AND SECTOR NUMBER
LC324           LDD         $01,S           ; GET THE NUMBER OF SECTORS TO THE START OF
                JSR         >LC784          ; THIS RECORD NUMBER AND CONVERT THEM TO A GRANULE OFFSET
                PSHS        B               ; SAVE GRANULE OFFSET ON THE STACK
                JSR         >LC779          ; MULTIPLY GRANULE NUMBER X 9 - CONVERT TO NUMBER OF SECTORS
                NEGB                        ;  NEGATE LS BYTE OF GRANULE OFFSET AND ADD THE
                ADDB        $03,S           ; LS BYTE OF SECTOR OFFSET - ACCB = SECTOR
; NUMBER (0-8) CORRESPONDING TO THE SECTOR NUMBER WITHIN A
; GRANULE OF THE LAST SECTOR OF THE SECTOR OFFSET
                INCB                        ;  = ADD ONE - SECTORS SAVED IN THE FCB START
                STB         FCBSEC,U        ; AT 1 NOT 0 - SAVE IT IN THE FCB
                LDB         FCBFGR,U        ; GET FIRST GRANULE IN FILE
                JSR         >LC755          ; POINT X TO FAT
                LEAU        FATCON,X        ; POINT U TO FAT DATA
                LDA         ,S              ; GET NUMBER OF GRANULES OFFSET TO RECORD
                INCA                        ;  ADD ONE (COMPENSATE FOR DECA BELOW)
LC33E           LEAX        ,U              ; POINT X TO FAT DATA
                ABX                         ;  POINT X TO CORRECT GRANULE
                DECA                        ;  DECREMENT GRANULE COUNTER
                BEQ         LC37B           ; BRANCH IF CORRECT GRANULE FOUND
                STB         ,S              ; SAVE GRANULE ADDRESS ON STACK
                LDB         ,X              ; GET NEXT GRANULE IN FILE
                CMPB        #$C0            ; LAST GRANULE IN FILE?
                BLO         LC33E           ; NO - KEEP LOOKING
; THE GRANULE BEING SEARCHED FOR IS NOT PRESENTLY DEFINED IN THIS RANDOM FILE
                LDB         ,S              ; GET OFFSET TO LAST GRANULE
                TST         VD8             ; CHECK GET/PUT FLAG
                BNE         LC366           ; AND BRANCH IF PUT
LC352           LDB         #2*23           ; 'INPUT PAST END OF FILE' ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
LC357           LEAX        FCBCON,U        ; POINT X TO FCB DATA BUFFER
; READ/WRITE A SECTOR. ENTER WITH OP CODE IN ACCB, BUFFER PTR IN X
LC35A           STB         DCOPC           ; SAVE DSKCON OPERATION CODE VARIABLE
                STX         DCBPT           ; SAVE DSKCON LOAD BUFFER VARIABLE
                LEAX        ,U              ; POINT X TO FCB
                JSR         >LC763          ; CONVERT FCB TRACK AND SECTOR TO DSKCON VARIABLES
                JMP         >LD6F2          ; READ/WRITE A TRACK OR SECTOR
; 'PUT' DATA INTO A GRANULE NOT PRESENTLY INCLUDED IN THIS FILE
LC366           PSHS        X,A             ; SAVE GRANULE COUNTER AND POINTER TO LAST USED GRANULE
                JSR         >LC7BF          ; FIND FIRST FREE GRANULE IN FAT
                TFR         A,B             ; SAVE FREE GRANULE NUMBER IN ACCB
                PULS        A,U             ; PULL LAST GRANULE POINTER AND COUNTER OFF OF STACK
                STB         ,U              ; SAVE NEWLY FOUND GRANULE NUMBER IN ADDRESS OF LAST GRANULE
                DECA                        ;  DECREMENT GRANULE COUNTER
                BNE         LC366           ; GET ANOTHER GRANULE IF NOT DONE
                PSHS        X,B             ; SAVE POINTER TO LAST GRANULE AND OFFSET
                JSR         >LC71E          ; WRITE FAT TO DISK
                PULS        B,X             ; RESTORE POINTER AND OFFSET
; WHEN CORRECT GRANULE IS FOUND, FIND THE RIGHT SECTOR
LC37B           LEAS        $01,S           ; REMOVE GRAN NUMBER FROM STACK
                LDU         FCBTMP          ; POINT U TO FCB
                STB         FCBCGR,U        ; SAVE CURRENT GRANULE IN FCB
                LDA         #$FF            ; SET FCBSOF,U TO ILLEGAL SECTOR OFFSET WHICH WILL
                STA         FCBSOF,U        ; FORCE NEW SECTOR DATA TO BE READ IN
                LDA         ,X              ; GET CURRENT GRANULE
                CMPA        #$C0            ; IS IT THE LAST GRANULE?
                BLO         LC3B2           ; NO
                ANDA        #$3F            ; MASK OFF LAST GRANULE FLAG BITS
                CMPA        FCBSEC,U        ; COMPARE CALCULATED SECTOR TO CURRENT SECTOR IN FCB
                BHS         LC3B2           ; AND BRANCH IF CALCULATED SECTOR IS > LAST SECTOR IN FILE
                LDA         VD8             ; CHECK GET/PUT FLAG: IF 'GET' THEN 'INPUT
                BEQ         LC352           ; PAST END OF FILE' ERROR
                LDA         FCBSEC,U        ; GET CURRENT SECTOR NUMBER FROM FCB,
                ORA         #$C0            ; OR IN THE LAST GRANULE FLAG BITS
                STA         ,X              ; AND SAVE IN FAT
                JSR         >LC5A9          ; WRITE FAT TO DISK IF NECESSARY
                LDX         FCBRLN,U        ; GET RECORD LENGTH AND CHECK TO
                CMPX        #SECLEN         ; SEE IF IT IS SECLEN (EXACTLY ONE SECTOR)
                BNE         LC3AD           ; BRANCH IF IT IS NOT EXACTLY ONE SECTOR
                CMPX        FCBLST,U        ; BRANCH IF THE NUMBER OF BYTES IN THE LAST SECTOR
                BEQ         LC3B2           ; IS SET TO ONE SECTOR (SECLEN)
                LDA         #$81            ; SET THE PRESAVED FLAG (BIT15) AND FORCE
LC3AC           FCB         $21             ; THE NUMBER OF BYTES IN LAST SECTOR TO 256 (THROWN AWAY BRN INSTRUCTION)
LC3AD           CLRA                        ; SET THE NUMBER OF BYTES IN LAST SECTOR TO ZERO
                CLRB                        ; CLEAR LS BYTE OF ACCD
                STD         FCBLST,U        ; SAVE THE NUMBER OF BYTES IN LAST SECTOR
LC3B2           LDB         #$02            ; DSKCON READ OP CODE
                LDX         FCBRLN,U        ; GET RECORD LENGTH AND COMPARE
                CMPX        #SECLEN         ; IT TO SECLEN - EXACTLY ONE SECTOR
                BNE         LC3C8           ; BRANCH IF NOT EXACTLY ONE SECTOR LONG
                LEAS        $07,S           ; CLEAN UP STACK
                LDX         FCBBUF,U        ; POINT X TO START OF RANDOM FILE BUFFER
                LDA         VD8             ; CHECK GET/PUT FLAG AND
                BEQ         LC3C5           ; BRANCH IF GET
                LDB         #$03            ; DSKCON WRITE OP CODE
LC3C5           JMP         >LC35A          ; READ/WRITE A SECTOR
LC3C8           JSR         >LC357          ; READ A SECTOR INTO FCB DATA BUFFER
                PULS        B,X             ; GET BACK THE BYTE OFFSET TO RECORD: X = NUMBER OF
; SECTORS; ACCB = BYTE POINTER IN SECTOR
                STX         FCBSOF,U        ; SAVE SECTOR OFFSET IN FCB
LC3CF           PSHS        B               ; SAVE BYTE OFFSET ON STACK
                JSR         >LC755          ; POINT X TO FILE ALLOCATION TABLE
                LEAX        FATCON,X        ; MOVE X TO FAT DATA
                LDB         FCBCGR,U        ; GET CURRENT GRANULE NUMBER
                ABX                         ; POINT X TO PROPER GRANULE IN FAT
                LDA         ,X              ; GET CURRENT GRANULE AND CHECK TO
                CMPA        #$C0            ; SEE IF IT IS LAST GRANULE
                BLO         LC40A           ; BRANCH IF THIS GRANULE IS < LAST GRANULE
                ANDA        #$3F            ; MASK OFF LAST GRANULE FLAG BITS
                CMPA        FCBSEC,U        ; COMPARE LAST SECTOR USED IN GRANULE TO
                BNE         LC40A           ; CALCULATED SECTOR; BRANCH IF NOT EQUAL
                LDD         FCBLST,U        ; GET NUMBER OF BYTES IN LAST SECTOR
                ANDA        #$7F            ; MASK OFF PRESAVED FLAG (BIT 15)
                PSHS        B,A             ; SAVE NUMBER OF BYTES IN LAST SECTOR ON STACK
                CLRA                        ; LOAD ACCB WITH THE BYTE OFFSET TO CURRENT
                LDB         $02,S           ; RECORD AND ADD THE REMAINING RECORD LENGTH
                ADDD        $03,S           ; TO IT - ACCD = END OF RECORD OFFSET
                CMPD        ,S++            ; COMPARE THE END OF RECORD OFFSET TO THE NUMBER OF
                BLS         LC40A           ; BYTES USED IN THE LAST SECTOR
                TST         VD8             ; CHECK GET/PUT FLAG AND BRANCH IF 'GET'
                LBEQ        LC352           ; TO 'INPUT PAST END OF FILE' ERROR
; IF LAST USED SECTOR, CALCULATE HOW MANY BYTES ARE USED
; IF DATA IS BEING 'PUT' PASTH THE CURRENT END OF FILE
                CMPD        #SECLEN         ; COMPARE TO ONE SECTOR'S LENGTH
                BLS         LC405           ; BRANCH IF REMAINDER OF RECORD LENGTH WILL FIT IN THIS SECTOR
                LDD         #SECLEN         ; FORCE NUMBER OF BYTES = ONE SECTOR LENGTH
LC405           ORA         #$80            ; SET PRE-SAVED FLAG BIT - ALL PUT RECORDS ARE
; WRITTEN TO DISK BEFORE LEAVING 'PUT'
                STD         FCBLST,U        ; SAVE NUMBER OF BYTES USED IN LAST SECTOR
LC40A           PULS        B               ; PULL BYTE OFFSET OFF OF THE STACK
                LEAX        FCBCON,U        ; POINT X TO FCB DATA BUFFER
                ABX                         ; MOVE X TO START OF RECORD
                LDU         $02,S           ; POINT U TO CURRENT POSITION IN RANDOM FILE BUFFER
                PSHS        B               ; SAVE BYTE OFFSET ON STACK
                LDA         #-1             ; CONVERT ACCD INTO A NEGATIVE 2 BYTE NUMBER
; REPRESENTING THE REMAINING UNUSED BYTES IN THE SECTOR
                ADDD        $01,S           ; ADD TEMPORARY RECORD LENGTH COUNTER (SUBTRACT
; REMAINING BYTES FROM TEMPORARY RECORD LENGTH)
                BHS         LC421           ; BRANCH IF THERE ARE ENOUGH UNUSED BYTES TO FINISH THE RECORD
                STD         $01,S           ; SAVE NEW TEMPORARY RECORD LENGTH COUNTER
                PULS        B               ; RESTORE BYTE COUNTER
                NEGB                        ; NEGATE IT - ACCB = THE NUMBER OF BYTES
; AVAILABLE TO A RECORD IN THIS SECTOR
                BRA         LC429           ; MOVE THE DATA
; BRANCH HERE IF REMAINING RECORD LENGTH WILL FIT IN
; WHAT'S LEFT OF THE CURRENTLY SELECTED SECTOR
LC421           LDB         $02,S           ; GET REMAINING RECORD LENGTH
                CLR         $01,S           ; CLEAR THE TEMPORARY RECORD LENGTH
                CLR         $02,S           ; COUNTER ON THE STACK
                LEAS        $01,S           ; PURGE BYTE OFFSET FROM STACK
LC429           LDA         VD8             ; CHECK GET/PUT FLAG AND
                BEQ         LC42F           ; BRANCH IF GET
                EXG         X,U             ; SWAP SOURCE AND DESTINATION POINTERS
LC42F           JSR         >LA59A          ; TRANSFER DATA FROM SOURCE TO DESTINATION BUFFERS
                STU         $02,S           ; SAVE NEW TEMP RECORD POINTER ON THE STACK (GET)
; MOVE DATA FROM FCB DATA BUFFER TO THE RANDOM FILE BUFFER IF 'GET'
; OR FROM RANDOM FILE BUFFER TO FCB DATA BUFFER IF 'PUT'
                LDU         FCBTMP          ; POINT U TO FCB
                LDA         VD8             ; CHECK GET/PUT FLAG AND
                BEQ         LC43E           ; BRANCH IF GET
                STA         FCBFLG,U        ; SAVE 'PUT' FLAG IN THE FCB
                STX         $02,S           ; SAVE NEW TEMPORARY RECORD POINTER ON STACK (PUT)
LC43E           LDX         FCBSOF,U        ; GET SECTOR OFFSET COUNTER AND
                LEAX        $01,X           ; ADD ONE TO IT
                CLRB                        ; SET BYTE OFFSET = 0
                LDU         ,S              ; CHECK THE LENGTH OF THE TEMPORARY RECORD LENGTH
                LBNE        LC306           ; COUNTER AND KEEP MOVING DATA IF <> 0
                PULS        A,B,X,PC        ; PULL TEMPORARY RECORD LENGTH AND
; BUFFER ADDRESS OFF STACK AND RETURN
; OPEN RAM HOOK
DVEC0           LEAS        $02,S           ; PULL RETURN ADDRESS OFF OF THE STACK
                JSR         >LB156          ; EVALUATE AN EXPRESSION
                JSR         >LB6A4          ; GET MODE(I,O,R) - FIRST BYTE OF STRING EXPRESSION
                PSHS        B               ; AND SAVE IT ON STACK
                JSR         >LA5A2          ; GET DEVICE NUMBER
                TSTB                        ; SET FLAGS
                LBLE        LA603           ; BRANCH IF NOT A DISK FILE
                PULS        A               ; GET MODE
                PSHS        B,A             ; SAVE MODE AND DEVICE NUMBER (FILE NUMBER)
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDX         #DATEXT         ; POINT TO 'DAT' FOR EXTENSION
                JSR         >LC938          ; GET FILENAME FROM BASIC
                LDD         #$01FF          ; DEFAULT DISK FILE TYPE AND ASCII FLAG
                STD         DFLTYP          ; SAVE DEFAULT VALUES: DATA, ASCII
                LDX         #SECLEN         ; DEFAULT RECORD LENGTH - 1 PAGE
                JSR         GETCCH          ; GET CHAR FROM BASIC
                BEQ         LC481           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB3E6          ; EVALUATE EXPRESSION
                LDX         FPA0+2          ; GET EVALUATED EXPRESSION
LC481           STX         DFFLEN          ; RECORD LENGTH
                LBEQ        LB44A           ; IF = 0, THEN 'ILLEGAL FUNCTION CALL'
                JSR         >LA5C7          ; ERROR IF ANY FURTHER CHARACTERS ON LINE
                PULS        A,B             ; GET MODE AND FILE NUMBER
; OPEN DISK FILE FOR READ OR WRITE
LC48D           PSHS        A               ; SAVE MODE ON STACK
                JSR         >LC749          ; POINT X TO FCB FOR THIS FILE
                LBNE        LA61C           ; 'FILE ALREADY OPEN' ERROR IF FILE OPEN
                STX         FCBTMP          ; SAVE FILE BUFFER POINTER
                JSR         >LC79D          ; MAKE SURE FILE ALLOC TABLE IS VALID
                JSR         >LC68C          ; SCAN DIRECTORY FOR 'FILENAME.EXT'
                PULS        B               ; GET MODE
                LDA         #INPFIL         ; INPUT TYPE FILE
                PSHS        A               ; SAVE FILE TYPE ON STACK
                CMPB        #'I             ; INPUT MODE?
                BNE         LC4C7           ; BRANCH IF NOT
; OPEN A SEQUENTIAL FILE FOR INPUT
                JSR         >LC6E5          ; CHECK TO SEE IF DIRECTORY MATCH IS FOUND
                JSR         >LC807          ; CHECK TO SEE IF FILE ALREADY OPEN
                LDX         V974            ; GET RAM DIRECTORY BUFFER
                LDD         DIRTYP,X        ; GET FILE TYPE AND ASCII FLAG
                STD         DFLTYP          ; SAVE IN RAM IMAGE
                BSR         LC52D           ; INITIALIZE FILE BUFFER CONTROL BLOCK
                JSR         >LC627          ; GO FILL DATA BUFFER
LC4BB           JSR         >LC755          ; POINT X TO PROPER FILE ALLOCATION TABLE
                INC         FAT0,X          ; ADD ONE TO FAT ACTIVE FILE COUNTER
                LDX         FCBTMP          ; GET FILE BUFFER POINTER
                PULS        A               ; GET FILE TYPE
                STA         FCBTYP,X        ; SAVE IT IN FCB
                RTS
LC4C7           ASL         ,S              ; SET FILE TYPE TO OUTPUT
                CMPB        #'O             ; FILE MODE = OUTPUT?
                BNE         LC4E8           ; BRANCH IF NOT

; OPEN A SEQUENTIAL FILE FOR OUTPUT
                TST         V973            ; DOES FILE EXIST ON DIRECTORY?
                BEQ         LC4E1           ; BRANCH IF NOT
                JSR         >LC6FC          ; KILL THE OLD FILE
                LDA         V973            ; GET DIRECTORY SECTOR NUMBER OF OLD FILE AND
                STA         V977            ; SAVE IT AS FIRST FREE DIRECTORY ENTRY
                LDX         V974            ; GET RAM DIRECTORY IMAGE OF OLD FILE AND
                STX         V978            ; SAVE IT AS FIRST FREE DIRECTORY ENTRY

LC4E1           JSR         >LC567          ; SET UP NEW DIRECTORY ENTRY ON DISK
                BSR         LC538           ; INITIALIZE FILE BUFFER
                BRA         LC4BB           ; FLAG AND MAP FCB AS BEING USED
LC4E8           CMPB        #'R             ; FILE MODE = R (RANDOM)?
                BEQ         LC4F2           ; BRANCH IF SO
                CMPB        #'D             ; FILE MODE = D (DIRECT)?
                LBNE        LA616           ; 'BAD FILE MODE' ERROR IF NOT

; OPEN A RANDOM/DIRECT FILE
LC4F2           ASL         ,S              ; SET FILE TYPE TO DIRECT
                LDD         RNBFAD          ; GET ADDRESS OF RANDOM FILE BUFFER AREA
                PSHS        B,A             ; AND SAVE IT ON THE STACK
                ADDD        DFFLEN          ; ADD THE RECORD LENGTH
                BLO         LC504           ; 'OB' ERROR IF SUM > $FFFF
                CMPD        FCBADR          ; IS IT > THAN FCB DATA AREA?
                BLS         LC509           ; BRANCH IF NOT
LC504           LDB         #2*29           ; 'OUT OF BUFFER SPACE' ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
LC509           PSHS        B,A             ; SAVE END OF RANDOM BUFFER ON STACK
                TST         V973            ; DID THIS FILE EXIST
                BNE         LC514           ; BRANCH IF SO
                BSR         LC567           ; SET UP NEW FILE IN DIRECTORY

                BRA         LC519           ; INITIALIZE FCB
LC514           LDA         #$FF            ; SET FILE TYPE MATCH = $FF (ILLEGAL VALUE) -
                JSR         >LC807          ; THIS WILL FORCE ANY OPEN MATCHED FILE TO CAUSE
; A 'FILE ALREADY OPEN' ERROR

LC519           BSR         LC52D           ; INITIALIZE FCB
                COM         FCBSOF,X        ; SET FCBSOF,X TO $FF (ILLEGAL SECTOR OFFSET) WHICH WILL
; FORCE NEW SECTOR DATA TO BE READ IN DURING GET/PUT
                INC         FCBREC+1,X      ; INITIALIZE RECORD NUMBER = 1
                PULS        A,B,U           ; U = START OF RANDOM FILE BUFFER AREA, ACCD = END
                STD         RNBFAD          ; SAVE NEW START OF RANDOM FILE BUFFER AREA
                STU         FCBBUF,X        ; SAVE BUFFER START IN FCB
                LDU         DFFLEN          ; GET RANDOM FILE RECORD LENGTH
                STU         FCBRLN,X        ; AND SAVE IT IN FCB
                BRA         LC4BB           ; SET FAT FLAG, SAVE FILE TYPE IN FCB

; INITIALIZE FCB DATA FOR INPUT
LC52D           BSR         LC538           ; INITIALIZE FCB
                LDU         V974            ; GET RAM DIRECTORY IMAGE
                LDU         DIRLST,U        ; GET NUMBER OF BYTES IN LAST SECTOR OF FILE
                STU         FCBLST,X        ; SAVE IT IN FCB
                RTS
; INITIALIZE FILE CONTROL BLOCK
LC538           LDX         FCBTMP          ; GET CURRENT FILE BUFFER
                LDB         #FCBCON         ; CLEAR FCB CONTROL BYTES
LC53C           CLR         ,X+             ; CLEAR A BYTE
                DECB                        ; DECREMENT COUNTER
                BNE         LC53C           ; BRANCH IF NOT DONE
                LDX         FCBTMP          ; GET CURRENT FILE BUFFER ADDRESS BACK
                LDA         DCDRV           ; GET CURRENT DRIVE NUMBER AND
                STA         FCBDRV,X        ; SAVE IT IN FCB
                LDA         V976            ; GET FIRST GRANULE -
                STA         FCBFGR,X        ; SAVE IT AS THE STARTING GRANULE NUMBER AND
                STA         FCBCGR,X        ; SAVE IT AS CURRENT GRANULE NUMBER
                LDB         V973            ; GET DIRECTORY SECTOR NUMBER
                SUBB        #$03            ; SUBTRACT 3 - DIRECTORY SECTORS START AT 3
                ASLB                        ; MULTIPLY SECTORS
                ASLB                        ; BY 8 (8 DIRECTORY
                ASLB                        ; ENTRIES PER SECTOR)
                PSHS        B               ; SAVE SECTOR OFFSET
                LDD         V974            ; GET RAM DIRECTORY IMAGE
                SUBD        #DBUF0          ; SUBTRACT RAM OFFSET
                LDA         #$08            ; 8 DIRECTORY ENTRIES/SECTOR
                MUL                         ; NOW ACCA CONTAINS 0-7
                ADDA        ,S+             ; ACCA CONTAINS DIRECTORY ENTRY (0-71)
                STA         FCBDIR,X        ; SAVE DIRECTORY ENTRY NUMBER
                RTS
; SET UP DIRECTORY AND UPDATE FILE ALLOCATION TABLE ENTRY IN FIRST UNUSED SECTOR
LC567           LDB         #28*2           ; 'DISK FULL' ERROR
                LDA         V977            ; GET SECTOR NUMBER OF FIRST EMPTY DIRECTORY ENTRY
                LBEQ        LAC46           ; 'DISK FULL' ERROR IF NO EMPTY DIRECTORY ENTRIES
                STA         V973            ; SAVE SECTOR NUMBER OF FIRST EMPTY DIRECTORY ENTRY
                STA         DSEC            ; SAVE SECTOR NUMBER IN DSKCON REGISTER
                LDB         #$02            ; READ OP CODE
                STB         DCOPC           ; SAVE IN DSKCON REGISTER
                JSR         >LD6F2          ; READ SECTOR
                LDX         V978            ; GET ADDRESS OF RAM IMAGE OF UNUSED DIRECTORY
                STX         V974            ; ENTRY AND SAVE AS CURRENT USED RAM IMAGE
                LEAU        ,X              ; (TFR X,U) POINT U TO DIRECTORY RAM IMAGE
                LDB         #DIRLEN         ; SET COUNTER TO CLEAR 32 BYTES (DIRECTORY ENTRY)
LC586           CLR         ,X+             ; CLEAR BYTE
                DECB                        ; DECREMENT COUNTER
                BNE         LC586           ; CONTINUE IF NOT DONE
                LDX         #DNAMBF         ; POINT TO FILENAME AND EXTENSION RAM IMAGE
                LDB         #11             ; 11 BYTES IN FILENAME AND EXTENSION
                JSR         >LA59A          ; MOVE B BYTES FROM X TO U
                LDD         DFLTYP          ; GET FILE TYPE AND ASCII FLAG
                STD         $00,U           ; SAVE IN RAM IMAGE
                LDB         #33             ; FIRST GRANULE TO CHECK
                JSR         >LC7BF          ; FIND THE FIRST FREE GRANULE
                STA         V976            ; SAVE IN RAM
                STA         $02,U           ; SAVE IN RAM IMAGE OF DIRECTORY TRACK
                LDB         #$03            ; GET WRITE OPERATION CODE AND SAVE
                STB         DCOPC           ; IT IN DSKCON REGISTER
                JSR         >LD6F2          ; GO WRITE A SECTOR IN DIRECTORY
LC5A9           PSHS        U,X,B,A         ; SAVE REGISTERS
                JSR         >LC755          ; POINT X TO FILE ALLOCATION TABLE
                INC         FAT1,X          ; INDICATE NEW DATA IN FILE ALLOC TABLE
                LDA         FAT1,X          ; GET NEW DATA FLAG
                CMPA        WFATVL          ; HAVE ENOUGH GRANULES BEEN REMOVED FROM THE FAT TO
; CAUSE THE FAT TO BE WRITTEN TO THE DISK
                BLO         LC5BA           ; RETURN IF NO NEED TO WRITE OUT ALLOCATION TABLE
                JSR         >LC71E          ; WRITE FILE ALLOCATION SECTOR TO DISK
LC5BA           PULS        A,B,X,U,PC      ; RESTORE REGISTERS
; CONSOLE IN RAM VECTOR
DVEC4           LDA         DEVNUM          ; GET DEVICE NUMBER
                LBLE        XVEC4           ; BRANCH IF NOT DISK FILE
                LEAS        $02,S           ; GET RID OF RETURN ADDRESS
LC5C4           PSHS        X,B             ; SAVE REGISTERS
                CLR         CINBFL          ; CLEAR BUFFER NOT EMPTY FLAG
                LDX         #FCBV1-2        ; POINT TO FILE BUFFER VECTOR TABLE
                LDB         DEVNUM          ; GET ACTIVE DISK FILE NUMBER
                ASLB                        ; TIMES 2 - TWO BYTES PER FCB ADDRESS
                LDX         B,X             ; NOW X POINTS TO FILE BUFFER
                LDB         ,X              ; GET FILE TYPE (FCBTYP,X)
                CMPB        #RANFIL         ; IS THIS A RANDOM (DIRECT) FILE?
                BNE         LC5EC           ; BRANCH IF NOT
; GET A BYTE FROM A RANDOM FILE - RETURN CHAR IN ACCA
                LDD         FCBGET,X        ; GET THE RECORD COUNTER
                CMPD        FCBRLN,X        ; COMPARE TO RECORD LENGTH AND
                BHS         LC5FE           ; BRANCH TO BUFFER EMPTY IF >= RECORD LENGTH
                ADDD        #$0001          ; ADD ONE TO RECORD POINTER AND
                STD         FCBGET,X        ; SAVE IT IN FCB
                LDX         FCBBUF,X        ; POINT X TO START OF RANDOM FILE BUFFER AND
                LEAX        D,X             ; ADD THE RECORD COUNTER TO IT
                LDA         -1,X            ; GET A CHARACTER FROM THE BUFFER
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
; GET A BYTE FROM A SEQUENTIAL FILE
LC5EC           LDB         FCBCFL,X        ; TEST THE CACHE FLAG AND BRANCH IF AN
                BEQ         LC5F9           ; EXTRA CHARACTER HAS NOT BEEN READ FROM FILE
                LDA         FCBCDT,X        ; GET THE CACHE CHARACTER
                CLR         FCBCFL,X        ; CLEAR THE CACHE FLAG
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
LC5F9           LDB         FCBDFL,X        ; IS ANY DATA LEFT?
                BEQ         LC602           ; BRANCH IF SO
LC5FE           COM         CINBFL          ; SET FLAG TO BUFFER EMPTY
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
LC602           LDB         FCBCPT,X        ; GET CHARACTER POINTER
                INC         FCBCPT,X        ; ADD ONE TO CHARACTER POINTER
                DEC         FCBLFT,X        ; DECREMENT NUMBER OF CHARACTERS LEFT IN FILE BUFFER
                BEQ         LC611           ; IF LAST CHARACTER, GO GET SOME MORE
                ABX                         ; ADD CHARACTER COUNTER TO X
                LDA         FCBCON,X        ; GET DATA CHARACTER (SKIP PAST 25 FCB CONTROL BYTES
                PULS        B,X,PC
; GET A CHARACTER FROM FCB DATA BUFFER - RETURN CHAR IN ACCA
LC611           PSHS        U,Y             ; SAVE REGISTERS
                CLRA                        ;
                LEAU        D,X             ; POINT U TO CORRECT CHARACTER
                LDA         FCBCON,U        ; GET DATA CHAR (SKIP PAST 25 CONTROL BYTES)
                PSHS        A               ; AND SAVE DATA CHARACTER ON STACK
                CLR         FCBCPT,X        ; RESET CHAR POINTER TO START OF BUFFER
                LDA         FCBDRV,X        ; GET DRIVE NUMBER AND SAVE IT IN
                STA         DCDRV           ; DSKCON VARIABLE
                BSR         LC627           ; GO READ A SECTOR - FILL THE BUFFER
                PULS        A,Y,U           ; RESTORE REGISTERS AND DATA CHARACTER
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
; REFILL THE FCB INPUT DATA BUFFER FOR SEQUENTIAL FILES
LC627           LDA         FCBSEC,X        ; GET CURRENT SECTOR NUMBER
LC629           INCA                        ; ADD ONE
                PSHS        A               ; SAVE NEW SECTOR NUMBER ON THE STACK
                CMPA        #$09            ; NINE SECTORS PER GRANULE
                BLS         LC631           ; BRANCH IF <= 9
                CLRA                        ; SET TO SECTOR ZERO
LC631           STA         FCBSEC,X        ; SAVE SECTOR NUMBER
                LDB         FCBCGR,X        ; GET GRANULE NUMBET TO FAT POINTER
                LEAU        ,X              ; POINT U TO FCB (TFR X,U)
                JSR         >LC755          ; POINT X TO PROPER FILE ALLOCATION TABLE
                ABX                         ; ADD OLD GRANULE NUMBER TO FAT POINTER
                LDB         FATCON,X        ; GET GRANULE NUMBER (6 CONTROL BYTES AT FRONT OF FAT)
                LEAX        ,U              ; POINT X TO FCB
                CMPB        #$C0            ; IS CURRENT GRANULE LAST ONE IN FILE?
                BHS         LC64D           ; YES
                PULS        A               ; GET SECTOR NUMBER
                SUBA        #10             ; WAS IT 10? - OVERFLOW TO NEXT GRANULE IF SO
                BNE         LC65E           ; BRANCH IF NOT
                STB         FCBCGR,X        ; SAVE NEW GRANULE NUMBER
                BRA         LC629           ; SET VARIABLES FOR NEW GRANULE
LC64D           ANDB        #$3F            ; GET NUMBER OF SECTORS USED IN THIS GRANULE
                CMPB        #$09            ; 9 SECTORS / GRANULE
                BLS         LC658           ; BRANCH IF OK
LC653           LDB         #2*32           ; 'BAD FILE STRUCTURE' ERROR
                JMP         >LAC46          ; ERROR DRIVER
LC658           SUBB        ,S+             ; SUBTRACT CURRENT SECTOR NUMBER AND PULS A
                BLO         LC67D           ; BRANCH IF PAST LAST SECTOR
                TFR         B,A             ; SECTOR NUMBER TO ACCA
LC65E           PSHS        A               ; SAVE SECTOR NUMBER DIFFERENCE
                BSR         LC685           ; INCREMENT RECORD NUMBER
                LDA         #$02            ; GET READ OPERATION CODE
                STA         DCOPC           ; AND SAVE IT IN DSKCON VARIABLE
                JSR         >LC763          ; GET PROPER TRACK AND SECTOR TO DSKCON VARIABLES
                LEAU        FCBCON,X        ; POINT U TO START OF FCB DATA BUFFER
                STU         DCBPT           ; AND SAVE IT IN DSKCON VARIABLE
                JSR         >LD6F2          ; GO READ A SECTOR INTO FCB BUFFER
                CLR         FCBLFT,X        ; NUMBER OF CHARS LEFT IN BUFFER = 256
                LDB         ,S+             ; GET SECTOR NUMBER OFF STACK
                BNE         LC684           ; RETURN IF DATA LEFT; FALL THRU IF LAST SECTOR
                LDD         FCBLST,X        ; GET NUMBER OF BYTES IN THE LAST SECTOR
                BNE         LC681           ; BRANCH IF SOME BYTES IN LAST SECTOR
LC67D           CLRB                        ; SET NUMBER OF REMAINING BYTES = 256
                COM         FCBDFL,X        ; SET DATA LEFT FLAG TO $FF
LC681           STB         FCBLFT,X        ; SAVE THE NUMBER OF CHARS LEFT IN BUFFER
LC684           RTS
LC685           LDU         FCBREC,X        ; GET CURRENT RECORD NUMBER
                LEAU        $01,U           ; BUMP IT
                STU         FCBREC,X        ; PUT IT BACK
                RTS
; SCAN DIRECTORY FOR FILENAME.EXT FOUND IN DNAMBF. IF FILENAME FOUND,
; RETURN WITH SECTOR NUMBER IN V973, GRANULE IN V976 AND RAM BUFFER
; CONTAINING DIRECTORY DATA IN V974. IF DISK IS FULL THEN V973,
; V977 = 0. THE FIRST UNUSED SECTOR RETURNED IN V977, RAM IMAGE IN V978
LC68C           CLR         V973            ; CLEAR SECTOR NUMBER
                CLR         V977            ; CLEAR TEMP SECTOR COUNTER
                LDD         #$1102          ; TRACK 17 (DIRECTORY), READ OPERATION CODE
                STA         DCTRK           ; SAVE TRACK NUMBER
                STB         DCOPC           ; SAVE OPERATION CODE (READ)
                LDB         #$03            ; READ SECTOR 3 (FIRST DIRECTORY SECTOR)
LC69B           STB         DSEC            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                LDU         #DBUF0          ; BUFFER AREA NUMBER 0 AS DATA BUFFER - SAVE
                STU         DCBPT           ; IN DSKCON VARIABLE
                JSR         >LD6F2          ; GO READ A SECTOR
LC6A5           STU         V974            ; SAVE RAM DIRECTORY BUFFER ADDRESS
                LEAY        ,U              ; POINT Y TO DIRECTORY BUFFER
                LDA         ,U              ; GET A BYTE FROM BUFFER
                BNE         LC6D6           ; BRANCH IF NOT ZERO - FILE IS ACTIVE
                BSR         LC6D9           ; SET UNUSED FILE POINTERS IF ENTRY HAS BEEN KILLED
LC6B0           LDX         #DNAMBF         ; POINT TO DISK FILE NAME BUFFER
LC6B3           LDA         ,X+             ; COMPARE THE FILENAME AND EXTENSION
                CMPA        ,U+             ; STORED IN RAM AT DNAMBF TO THE DIRECTORY
                BNE         LC6C7           ; ENTRY STORED AT ,U (BRANCH IF MISMATCH)
                CMPX        #DNAMBF+11      ; AT END OF FILE NAME BUFFER?
                BNE         LC6B3           ; BRANCH IF NOT DONE CHECKING FILENAME
                STB         V973            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                LDA         FCBFGR,U        ; GET NUMBER OF FIRST GRANULE IN FILE
                STA         V976            ; AND SAVE IT IN V976
                RTS
LC6C7           LEAU        DIRLEN,Y        ; GET NEXT DIRECTORY ENTRY (DIRLEN BYTES PER ENTRY)
                CMPU        #DBUF0+SECLEN   ; AT END OF BUFFER?
                BNE         LC6A5           ; CHECK NEXT ENTRY IF NOT AT END
                INCB                        ; NEXT SECTOR
                CMPB        #11             ; 11 SECTORS MAX IN DIRECTORY
                BLS         LC69B           ; BRANCH IF MORE SECTORS
                RTS
LC6D6           COMA                        ; COMPLEMENT FIRST BYTE IN DIRECTORY EMTRY
                BNE         LC6B0           ; BRANCH IF FILE IS ACTIVE - FALL THRU IF NOT USED
; SET POINTERS FOR FIRST UNUSED DIRECTORY ENTRY
LC6D9           LDA         V977            ; UNUSED ENTRY ALREADY FOUND?
                BNE         DVEC12          ; RETURN IF UNUSED ENTRY ALREADY FOUND
                STB         V977            ; SECTOR CONTAINING THIS DIRECTORY ENTRY
                STU         V978            ; POINTS TO RAM AREA WHERE DIRECTORY DATA IS STORED
DVEC12          RTS
LC6E5           LDB         #2*26           ; 'NE' ERROR
                TST         V973            ; WAS A DIRECTORY MATCH FOUND?
                BNE         DVEC12          ; RETURN IF FOUND
                JMP         >LAC46          ; JUMP TO ERROR HANDLER IF NOT FOUND
; KILL COMMAND
KILL            JSR         >LC935          ; GET FILENAME.EXT FROM BASIC
                JSR         >LA5C7          ; 'SYNTAX' ERROR IF MORE CHARACTERS ON LINE
                JSR         >LC79D          ; GET VALID FAT DATA
                BSR         LC68C           ; TEST FOR FILE NAME MATCH IN DIRECTORY
                BSR         LC6E5           ; MAKE SURE THE FILE EXISTED
LC6FC           LDA         #$FF            ; MATCH FILE TYPE = $FF; THIS WILL CAUSE AN 'AO'
; ERROR TO BE GENERATED IF ANY FILE TYPE IS OPEN
                JSR         >LC807          ; CHECK TO MAKE SURE FILE IS NOT OPEN
                LDX         V974            ; GET RAM IMAGE OF DIRECTORY
                CLR         ,X              ; AND ZERO FIRST BYTE - KILL FILE (DIRNAM,X)
                LDB         #$03            ; WRITE OPERATION CODE - SAVE
                STB         DCOPC           ; IT IN DSKCON VARIABLE
                JSR         >LD6F2          ; WRITE A SECTOR
                LDB         DIRGRN,X        ; GET NUMBER OF FIRST GRANULE IN FILE
LC70F           BSR         LC755           ; POINT X TO PROPER FILE ALLOCATION TABLE
                LEAX        FATCON,X        ; SKIP 6 CONTROL BYTES
                ABX                         ; POINT TO CORRECT ENTRY
                LDB         ,X              ; GET NEXT GRANULE
                LDA         #$FF            ; GET FREE GRANULE FLAG AND
                STA         ,X              ; MARK GRANULE AS FREE
                CMPB        #$C0            ; WAS THIS THE LAST GRANULE?
                BLO         LC70F           ; KEEP FREEING GRANULES IF NOT LAST ONE
; WRITE FILE ALLOCATION SECTOR TO DIRECTORY - DO NOT WRITE
; THE SIX CONTROL BYTES AT THE START OF THE FAT TO THE DISK
LC71E           LDU         #DBUF0          ; POINT U TO DISK BUFFER 0 AND
                STU         DCBPT           ; SAVE IT AS DSKCON VARIABLE
                LDD         #$1103          ; WRITE DIRECTORY TRACK - SAVE
                STA         DCTRK           ; TRACK AND WRITE OPERATION CODE IN
                STB         DCOPC           ; DSKCON VARIABLES
                LDB         #$02            ; GET FILE ALLOCATION SECTOR AND
                STB         DSEC            ; SAVE IN DSKCON VARIABLE
                BSR         LC755           ; POINT X TO PROPER FILE ALLOCATION TABLE
                CLR         FAT1,X          ; RESET FLAG INDICATING VALID FAT DATA HAS BEEN STORED ON DISK
                LEAX        FATCON,X        ; MOVE (X) TO START OF GRANULE DATA
                LDB         #GRANMX         ; 68 BYTES IN FAT
                JSR         >LA59A          ; MOVE ACCB BYTES FROM FAT RAM IMAGE TO DBUF0
; ZERO OUT ALL OF THE BYTES IN THE FAT SECTOR WHICH DO NOT CONTAIN THE GRANULE DATA
LC739           CLR         ,U+             ; CLEAR A BYTE
                CMPU        #DBUF0+SECLEN   ; FINISHED THE WHOLE SECTOR?
                BNE         LC739           ; NO
                JMP         >LD6F2          ; WRITE A SECTOR

; ENTER WITH ACCB CONTAINING FILE NUMBER (1-15); EXIT WITH X POINTING
; TO CORRECT FILE BUFFER; FLAGS SET ACCORDING TO FILE TYPE.

LC744           PSHS        B               ; SAVE FILE NUMBER ON STACK
                LDB         DEVNUM          ; GET DEVICE NUMBER (FILE NUMBER)
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
LC749           PSHS        B               ; SAVE FILE NUMBER ON STACK
                ASLB                        ; X2: 2 BYTES PER POINTER
                LDX         #FCBV1-2        ; POINT X TO START OF FCB POINTERS
                LDX         B,X             ; POINT X TO PROPER FCB
                LDB         FCBTYP,X        ; SET FLAGS ACCORDING TO FILE TYPE
                PULS        B,PC            ; RESTORE FILE NUMBER
; POINT X TO DRIVE ALLOCATION TABLE
LC755           PSHS        B,A             ; SAVE ACCD ON STACK
                LDA         DCDRV           ; GET DRIVE NUMBER
                LDB         #FATLEN         ; GET LENGTH OF FILE ALLOCATION TABLE
                MUL                         ; MULTIPLY BY DRIVE NUMBER TO GET OFFSET
                LDX         #FATBL0         ; START OF FILE ALLOCATION TABLE
                LEAX        D,X             ; POINT TO RIGHT TABLE
                PULS        A,B,PC          ; RESTORE ACCD
; CONVERT GRANULE NUMBER TO TRACK & SECTOR NUMBER - X MUST BE POINTING TO CORRECT
; FCB; THE TRACK AND SECTOR NUMBER WILL BE STORED IN DSKCON REGISTERS
LC763           LDB         FCBCGR,X        ; GET GRANULE NUMBER
                LSRB                        ;  DIVIDE BY 2 - 2 GRANULES / TRACK
                STB         DCTRK           ; TRACK NUMBER
                CMPB        #17             ; TRACK 17 = DIRECTORY TRACK
                BLO         LC76E           ; BRANCH IF < DIRECTORY TRACK
                INC         DCTRK           ; INCR TRACK NUMBER IF > DIRECTORY TRACK
LC76E           ASLB                        ;  MULTIPLY TRACK NUMBER BY 2
                NEGB                        ;  NEGATE GRANULE NUMBER
                ADDB        FCBCGR,X        ; B=0 IF EVEN GRANULE; 1 IF ODD
                BSR         LC779           ; RETURN B=0 FOR EVEN GRANULE NUMBER, B=9 FOR ODD GRANULE NUMBER
                ADDB        FCBSEC,X        ; ADD SECTOR NUMBER
                STB         DSEC            ; SAVE SECTOR NUMBER
                RTS
; MULTIPLY ACCD BY 9
LC779           PSHS        B,A             ; TEMP STORE ACCD ON STACK
                ASLB                        ;
                ROLA                        ;  MULTIPLY BY 2
                ASLB                        ;  =
                ROLA                        ;  = MULTIPLY BY FOUR
                ASLB                        ;
                ROLA                        ;  MULTIPLY BY EIGHT
                ADDD        ,S++            ; ADD ONE = MULTIPLY BY NINE
                RTS
; CONVERT ACCD INTO A GRANULE NUMBER - RETURN RESULT IN ACCB;
; ENTER WITH ACCD CONTAINING A NUMBER OF SECTORS. RETURN IN ACCB
; THE NUMBER (0-67) CORRESPONDING TO THE NUMBER OF COMPLETE
; GRANULES CONTAINED IN THAT MANY SECTORS.
; DIVIDE BY 90, MULTIPLY BY 10 IS FASTER THAN DIVIDE BY 9
LC784           CLR         ,-S             ; CLEAR A TEMPORARY SLOT ON THE STACK
LC786           INC         ,S              ; DIVIDE ACCD BY 90 - SAVE THE
                SUBD        #9*10           ; QUOTIENT+1 ON THE STACK - REMAINDER
                BPL         LC786           ; IN ACCB
                LDA         ,S              ; PUT THE QUOTIENT+1 IN ACCA AND
                STB         ,S              ; SAVE REMAINDER ON STACK
                LDB         #10             ; MULTIPLY (QUOTIENT+1)
                MUL                         ;  BY 10
                PULS        A               ; PUT THE REMAINDER IN ACCA
LC796           DECB                        ;  DECREMENT THE GRANULE COUNT BY ONE FOR
                ADDA        #$09            ; EVERY NINE SECTORS (1 GRANULE) IN THE
                BMI         LC796           ; REMAINDER - COMPENSATE FOR THE + 1 IN QUOTIENT+1
                CLRA                        ;  CLEAR MS BYTE OF ACCD
LC79C           RTS
; MAKE SURE RAM FILE ALLOCATION TABLE DATA IS VALID
LC79D           BSR         LC755           ; POINT X TO FAT FOR THE CORRECT DRIVE NUMBER
                TST         FAT0,X          ; CHECK TO SEE IF ANY FILES ARE ACTIVE
                BNE         LC79C           ; RETURN IF ANY FILES ACTIVE IN THIS FAT
                CLR         FAT1,X          ; RESET FAT DATA VALID FLAG
                LEAU        FATCON,X        ; LOAD U WITH START OF GRANULE DATA BUFFER
                LDX         #DBUF0          ; BUFFER FOR DISK TRANSFER
                STX         DCBPT           ; PUT IN DSKCON PARAMETER
                LDD         #$1102          ; DIRECTORY TRACK, READ SECTOR
                STA         DCTRK           ; STORE IN DSKCON TRACK NUMBER
                STB         DCOPC           ; STORE IN DSKCON OP CODE
                LDB         #$02            ; GET SECTOR NUMBER 2 (FILE ALLOCATION TABLE)
                STB         DSEC            ; STORE IN DSKCON PARAMETER
                JSR         >LD6F2          ; GO READ SECTOR
                LDB         #GRANMX         ; TRANSFER FILE ALLOCATION TABLE TO FILE ALLOC TABLE BUFFER
                JMP         >LA59A          ; MOVE B BYTES FROM (X) TO (U)
; FIND FIRST FREE GRANULE - ENTER WITH ACCB CONTAINING
; GRANULE FROM WHICH TO START SEARCHING. THE FOUND GRANULE
; IS MARKED BY STORING A $C0 IN THE GRANULE'S DATA BYTE
; TO INDICATE THAT IT IS THE LAST GRANULE IN THE FILE.
; RETURN WITH FIRST FREE GRANULE FOUND IN ACCA
LC7BF           BSR         LC755           ; POINT X TO FILE ALLOC TABLE
                LEAX        FATCON,X        ; SKIP CONTROL BYTES
                CLRA                        ;  USE ACCA AS GRANULE COUNTER
                ANDB        #$FE            ; MASK OFF BIT ZERO OF SEARCH GRANULE
                CLR         ,-S             ; INITIALIZE AND SAVE A BYTE ON STACK (DIRECTION FLAG)
LC7C8           COM         B,X             ; IS THIS GRANULE FREE? ($FF=FREE)
                BEQ         LC7FD           ; BRANCH IF IT IS
                COM         B,X             ; RESTORE GRANULE DATA
                INCA                        ;  ADD ONE TO GRANULE COUNTER
                CMPA        #GRANMX         ; GRANMX GEANULES PER DISK
                BHS         LC7F8           ; BRANCH IF ALL GRANULES CHECKED (DISK FULL)
                INCB                        ;  INCR TO NEXT GRANULE
                BITB        #$01            ; IS BIT 0 SET?
                BNE         LC7C8           ; BRANCH IF ODD GRANULE NUMBER (SAME TRACK)
                PSHS        B,A             ; SAVE GRANULE COUNTER AND CURRENT GRANULE NUMBER
                SUBB        #$02            ; SUBTRACT ONE TRACK (2 GRANULES)
                COM         $02,S           ; COMPLEMENT DIRECTION FLAG
                BNE         LC7EC           ; BRANCH EVERY OTHER TIME
                SUBB        ,S+             ; SUBTRACT THE GRANULE COUNTER FROM THE CURRENT GRANULE NUMBER
                BPL         LC7E8           ; BRANCH IF LOWER BOUND NOT EXCEEDED
                LDB         ,S              ; RESTORE CURRENT GRANULE NUMBER IF LOWER BOUND EXCEEDED
LC7E6           COM         $01,S           ; COMPLEMENT FLAG - IF GRANULE NUMBER HAS EXCEEDED
; BOUNDS ON EITHER THE HI OR LO SIDE, FORCE IT TO GO IN
; THE DIRECTION OPPOSITE THE EXCEEDED BOUND
LC7E8           LEAS        $01,S           ; CLEAN UP STACK
                BRA         LC7C8           ; CHECK FOR ANOTHER FREE GRANULE
LC7EC           ADDB        ,S+             ; ADD THE GRANULE COUNTER TO THE CURRENT GRANULE NUMBER
                CMPB        #GRANMX         ; GRANMX GRANULES PER DISK
                BLO         LC7E8           ; BRANCH IF UPPER BOUND NOT EXCEEDED
                LDB         ,S              ; RESTORE CURRENT GRANULE COUNT AND GO TWICE
                SUBB        #$04            ; AS FAR AS USUAL IN OPPOSITE DIRECTION IF UPPER BOUND EXCEEDED
                BRA         LC7E6           ; KEEP SEARCHING
LC7F8           LDB         #2*28           ; 'DISK FULL' ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
; POINT X TO FIRST FREE GRANULE POSITION IN THE FILE ALLOCATION
; TABLE AND MARK THE POSITION WITH A LAST GRANULE IN FILE MARKER
LC7FD           LEAS        $01,S           ; CLEAR UP STACK - REMOVE DIRECTION FLAG
                TFR         B,A             ; GRANULE NUMBER TO ACCA
                ABX                         ;  POINT X TO FIRST FOUND GRANULE
                LDB         #$C0            ; LAST GRANULE FLAG
                STB         ,X              ; MARK THE FIRST FOUND GRANULE AS THE LAST GRANULE
LC806           RTS
; CHECK ALL ACTIVE FILES TO MAKE SURE A FILE IS NOT ALREADY OPEN - TO BE OPEN
; A FILE BUFFER MUST MATCH THE DRIVE NUMBER AND FIRST GRANULE NUMBER
; IN RAM DIRECTORY ENTRY AND THE FCB TYPE MUST NOT MATCH THE FILE TYPE IN ACCA
; AN 'AO' ERROR WILL NOT BE GENERATED IF A FILE IS BEING OPENED FOR
; THE SAME MODE THAT IT HAS ALREADY BEEN OPENED UNDER.
LC807           PSHS        A               ; SAVE FILE TYPE ON STACK
                LDB         FCBACT          ; NUMBER OF CURRENTLY OPEN FILES
                INCB                        ;  ADD ONE MORE TO FILE COUNTER
LC80D           JSR         >LC749          ; POINT X TO FCB OF THIS FILE
                BEQ         LC829           ; BRANCH IF BUFFER NOT BEING USED
                LDA         DCDRV           ; GET DRIVE NUMBER AND CHECK TO SEE IF IT
                CMPA        FCBDRV,X        ; MATCHES THE DRIVE NUMBER FOR THIS BUFFER
                BNE         LC829           ; FILE EXISTS ON ANOTHER DRIVE
                LDU         V974            ; GET RAM DIRECTORY AREA
                LDA         DIRGRN,U        ; GET FIRST GRANULE IN FILE
                CMPA        FCBFGR,X        ; DOES IT MATCH THIS FILE BUFFER?
                BNE         LC829           ; NO
                LDA         FCBTYP,X        ; GET FILE TYPE OF THIS BUFFER
                CMPA        ,S              ; DOES IT MATCH THE ONE WE ARE LOOKING FOR?
                LBNE        LA61C           ; 'FILE ALREADY OPEN' ERROR IF NOT
LC829           DECB                        ;  DECR FILE COUNTER
                BNE         LC80D           ; BRANCH IF HAVEN'T CHECKED ALL ACTIVE FILES
                PULS        A,PC            ; RESTORE FILE TYPE AND RETURN
LC82E           JSR         >LA5A5          ; EVALUATE AN EXPRESSION (DEVICE NUMBER)
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                TSTB                        ;  TEST NEW DEVICE NUMBER
                LBLE        LB44A           ; 'FC' ERROR IF DEVICE NUMBER NOT A DISK FILE
                JSR         >LC749          ; POINT X TO FCB
                LDA         FCBTYP,X        ; TEST IF BUFFER IS IN USE
                LBEQ        LA3FB           ; 'FILE NOT OPEN' ERROR
                CMPA        #RANFIL         ; DIRECT/RANDOM FILE?
                BEQ         LC806           ; RETURN IF RANDOM
LC845           JMP         >LA616          ; BAD FILE MODE ERROR IF NOT RANDOM

; INPUT DEVICE NUMBER CHECK RAM HOOK
DVEC5           LDA         #INPFIL         ; INPUT FILE TYPE
LC84A           FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)

; PRINT DEVICE NUMBER CHECK RAM HOOK
DVEC6           LDA         #OUTFIL         ; OUTPUT FILE TYPE
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN IF
                BLE         LC806           ; NOT A DISK FILE
                STX         ,S              ; REPLACE SUBROUTINE RETURN ADDRESS WITH X REGISTER -
; THIS IS THE SAME AS LEAS 2,S AND PSHS X
                JSR         >LC744          ; POINT X TO FCB
                PSHS        B,A             ; SAVE ACCB AND FILE TYPE ON STACK
                LDA         FCBTYP,X        ; GET FILE TYPE
                LBEQ        LA3FB           ; 'FILE NOT OPEN' ERROR
                CMPA        #RANFIL         ; RANDOM FILE?
                BEQ         LC868           ; BRANCH IF RANDOM FILE
                CMPA        ,S              ; IS THIS FCB OF THE PROPER TYPE?
                BNE         LC845           ; 'FILE MODE' ERROR IF NOT
LC866           PULS        A,B,X,PC        ; RESTORE ACCB,X,ACCA (FILE TYPE) AND RETURN
LC868           LDX         $04,S           ; GET CALLING ADDRESS FROM THE STACK AND
                CMPX        #LB00C          ; RETURN UNLESS COMING FROM
                BNE         LC866           ; BASIC'S 'INPUT' STATEMENT
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR A COMMA
                CMPA        #'"             ; CHECK FOR A DOUBLE QUOTE
                BNE         LC881           ; RETURN TO BASIC'S 'INPUT' COMMAND
                JSR         >LB244          ; STRIP PROMPT STRING FROM BASIC AND PUT IT ON THE STRING STACK
                JSR         >LB657          ; PURGE THE STRING PUT ON THE STRING STACK
                LDB         #';             ; SEMICOLON
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR SEMICOLON
LC881           LDX         #LB01E          ; GET MODIFIED REENTRY POINT INTO BASIC
                STX         $04,S           ; AND PUT IT INTO THE RETURN ADDRESS ON THE STACK
                PULS        A,B,X,PC        ; RETURN TO BASIC
; DEVICE NUMBER VALIDITY CHECK RAM HOOK
DVEC1           BLE         LC8AF           ; RETURN IF NOT A DISK FILE
                CMPB        FCBACT          ; COMPARE DEVICE NUMBER TO HIGHEST POSSIBLE
                LBHI        LA61F           ; 'DEVICE NUMBER' ERROR IF TOO BIG
                PULS        X,PC            ; RETURN
; SET PRINT PARAMETERS RAM HOOK
DVEC2           TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                BLE         LC8AF           ; RETURN IF NOT DISK FILE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
                PSHS        X,B,A           ; SAVE REGISTERS
                CLR         PRTDEV          ; SET PRINT DEVICE NUMBER TO NON-CASSETTE
                JSR         >LC744          ; POINT X TO FCB
                LDB         FCBPOS,X        ; GET PRINT POSITION
                CLRA                        ;  PRINT WIDTH (256)
                LDX         #$1000          ; TAB FIELD WIDTH AND TAB ZONE
                JMP         >LA37C          ; SAVE THE PRINT PARAMETERS
; BREAK CHECK RAM HOOK
DVEC11          TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN
                BLE         LC8AF           ; IF NOT A DISK FILE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK - DON'T
LC8AF           RTS                         ; = DO A BREAK CHECK IF DISK FILE
; COMMAND INTERPRETATION RAM HOOK
DVEC20          LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
LC8B2           ANDCC       #$AF            ; ENABLE IRQ & FIRQ
                CLR         PIA0+2          ; STROBE ALL KEYS (COLUMN STROBE)
                LDA         PIA0            ; READ KEYBOARD ROWS
                COMA                        ;  INVERT KEYBOARD ROW DATA
                ANDA        #$7F            ; MASK OFF JOYSTICK INPUT BIT
                BEQ         LC8C2           ; BRANCH IF NO KEY DOWN
                JSR         >LADEB          ; GO DO A BREAK CHECK IF A KEY IS DOWN
LC8C2           LDX         CHARAD          ; GET INPUT POINTER INTO X
                STX         TINPTR          ; TEMP SAVE IT
                LDA         ,X+             ; SEARCH FOR THE END OF CURRENT LINE
                BEQ         LC8D1           ; BRANCH IF END OF LINE
                CMPA        #':             ; CHECK FOR END OF SUB LINE, TOO
                BEQ         LC8F3           ; BRANCH IF END OF SUB LINE
                JMP         >LB277          ; 'SYNTAX' ERROR IF NOT END OF LINE
LC8D1           LDA         ,X++            ; GET MS BYTE OF ADDRESS OF NEXT BASIC LINE
                STA         ENDFLG          ; AND SAVE IT IN CURLIN
                BNE         LC8DA           ; BRANCH IF NOT END OF PROGRAM
                JMP         >LAE15          ; GO 'STOP' THE SYSTEM
LC8DA           LDD         ,X+             ; GET LINE NUMBER OF THIS LINE AND
                STD         CURLIN          ; SAVE IT IN CURLIN
                STX         CHARAD          ; RESET BASIC'S INPUT POINTER
                LDA         TRCFLG          ; CHECK THE TRACE FLAG AND
                BEQ         LC8F3           ; BRANCH IF TRACE OFF
                LDA         #'[             ; [ LEFT DELIMITER OF TRON
                JSR         PUTCHR          ; SEND CHARACTER TO CONSOLE OUT
                LDA         CURLIN          ; GET NUMBER OF CURRENT LINE NUMBER
                JSR         >LBDCC          ; CONVERT ACCD TO DECIMAL & PRINT IT ON SCREEN
                LDA         #']             ; ] RIGHT DELIMITER OF TRON
                JSR         PUTCHR          ; SEND A CHARACTER TO CONSOLE OUT
LC8F3           JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                TFR         CC,B            ; SAVE STATUS REGISTER IN ACCB
                CMPA        #$98            ; CSAVE TOKEN?
                BNE         LC8FE           ; NO
                JMP         >L8316          ; GO CHECK FOR CSAVEM
LC8FE           CMPA        #$97            ; CLOAD TOKEN?
                BNE         LC905           ; NO
                JMP         >L8311          ; JUMP TO EXBAS' CLOAD ROUTINE
LC905           TFR         B,CC            ; RESTORE STATUS REGISTER
                JSR         >LADC6          ; LOOP THROUGH BASIC'S MAIN INTERPRETATION LOOP
                BRA         LC8B2
; EOF RAM HOOK
DVEC14          LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
                LDA         DEVNUM          ; GET DEVICE NUMBER AND SAVE
                PSHS        A               ; IT ON THE STACK
                JSR         >LA5AE          ; STRIP DEVICE NUMBER OFF OF INPUT LINE
                JSR         >LA3ED          ; VERIFY THAT THE FILE TYPE WAS 'INPUT'
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                LBLE        LA5DA           ; BRANCH BACK TO BASIC'S EOF IF NOT DISK FILE
                JSR         >LC744          ; POINT X TO FCB
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #RANFIL         ; RANDOM FILE?
                LBEQ        LA616           ; 'FM' BAD FILE MODE ERROR IF RANDOM
                CLRB                        ;  FILE NOT EMPTY FLAG - SET TO NOT EMPTY
                LDA         FCBCFL,X        ; CHECK THE CACHE FLAG - BRANCH IF
                BNE         LC932           ; THERE IS A CHARACTER WHICH HAS BEEN CACHED
                LDB         FCBDFL,X        ; GET SEQUENTIAL INPUT FILE STATUS
LC932           JMP         >LA5E4          ; LINK BACK TO BASIC'S EOF STATEMENT
; GET FILENAME/EXTENSION: DRIVE NUMBER FROM BASIC
LC935           LDX         #DEFEXT         ; POINT TO ' ' BLANK (DEFAULT) EXTENSION
LC938           CLR         ,-S             ; CLEAR A BYTE ON STACK FOR USE AS A DRIVES FLAG
                LDA         DEFDRV          ; GET DEFAULT DISK NUMBER
                STA         DCDRV           ; STORE IN DSKCON PARAMETER
                LDU         #DNAMBF         ; DISK FILENAME BUFFER
                LDD         #$2008          ; STORE 8 BLANKS IN RAM (DEFAULT FILE NAME)
LC945           STA         ,U+             ; STORE A BLANK IN FILE NAME
                DECB                        ;  DECREMENT COUNTER
                BNE         LC945           ; BRANCH IF NOT DONE
                LDB         #$03            ; 3 BYTES IN EXTENSION
                JSR         >LA59A          ; MOVE B BYTES FROM (X) TO (U)
                JSR         >L8748          ; EVALUATE A STRING EXPRESSION
                LEAU        ,X              ; POINT U TO START OF STRING
                CMPB        #$02            ; CHECK LENGTH OF STRING AND
                BLO         LC96A           ; BRANCH IF < 2
                LDA         $01,U           ; GET 2ND CHARACTER IN STRING AND
                CMPA        #':             ; = CHECK FOR COLON
                BNE         LC96A           ; BRANCH IF NO DRIVE NUMBER
                LDA         ,U              ; GET 1ST CHARACTER
                CMPA        #'0             ; IN STRING AND
                BLO         LC96A           ; CHECK TO SEE
                CMPA        #'3             ; IF IT IS IN
                BHI         LC96A           ; THE RANGE 0-3
                BSR         LC99D           ; GET DRIVE NUMBER
LC96A           LDX         #DNAMBF         ; POINT X TO FILE NAME BUFFER
                INCB                        ;  COMPENSATE FOR DECB BELOW
LC96E           DECB                        ;  DECREMENT STRING LENGTH
                BNE         LC97D           ; BRANCH IF MORE CHARACTERS IN STRING
                LEAS        $01,S           ; CLEAN UP STACK - REMOVE DRIVE FLAG
LC973           CMPX        #DNAMBF         ; POINTER STILL AT START OF BUFFER?
                BNE         LC9DF           ; RETURN IF NOT
LC978           LDB         #2*31           ; 'BAD FILENAME' ERROR IF NULL FILENAME
                JMP         >LAC46          ; ERROR HANDLER
LC97D           LDA         ,U+             ; GET A CHARACTER FROM STRING
                CMPA        #'.             ; LOOK FOR PERIOD?
                BEQ         LC9B0           ; YES
                CMPA        #'/             ; SLASH?
                BEQ         LC9B0           ; YES
                CMPA        #':             ; COLON?
                BEQ         LC994           ; YES
                CMPX        #DEXTBF         ; COMPARE POINTER TO END OF FILENAME BUFFER
                BEQ         LC978           ; 'BAD FILENAME' ERROR - FILENAME TOO LONG
                BSR         LC9D0           ; PUT A CHARACTER IN FILENAME
                BRA         LC96E           ; GET ANOTHER CHARACTER FROM STRING
LC994           BSR         LC973           ; 'BAD FILENAME' ERROR IF NO FILENAME YET
                BSR         LC99D           ; GET DRIVE NUMBER
                TSTB                        ;  CHECK LENGTH OF STRING
                BNE         LC978           ; ''BAD FILENAME' ERROR IF MORE CHARACTERS LEFT
LC99B           PULS        A,PC            ; REMOVE DRIVES FLAG FROM STACK AND RETURN
; GRAB DRIVE NUMBER
LC99D           COM         $02,S           ; TOGGLE DRIVE FLAG
                BEQ         LC978           ; 'BAD FILENAME' ERROR IF DRIVE NUMBER DEFINED TWICE
                LDA         ,U++            ; ASCII VALUE OF DRIVE NUMBER TO ACCA
                SUBB        #$02            ; DECREMENT STRING LENGTH BY 2 FOR DRIVE (:X)
                SUBA        #'0             ; SUBTRACT ASCII BIAS
                BLO         LC978           ; DRIVE NUMBER TOO LOW - 'BAD FILENAME' ERROR
                CMPA        #$03            ; MAX OF 4 DRIVES
                BHI         LC978           ; DRIVE NUMBER TOO HIGH - 'BAD FILENAME' ERROR
                STA         DCDRV           ; STORE IN DSKCON DRIVE NUMBER
                RTS
; GRAB EXTENSION
LC9B0           BSR         LC973           ; 'BAD FILENAME' ERROR IF NO FILENAME YET
                LDX         #DFLTYP         ; POINT X TO END OF EXTENSION BUFFER
                LDA         #SPACE          ; BLANK
LC9B7           STA         ,-X             ;
                CMPX        #DEXTBF         ; FILL EXTENSION WITH
                BNE         LC9B7           ; BLANKS (DEFAULT)
LC9BE           DECB                        ;  DECREMENT STRING COUNTER
                BEQ         LC99B           ; RETURN IF ZERO
                LDA         ,U+             ; GET A CHARACTER FROM STRING
                CMPA        #':             ; CHECK FOR DRIVE SEPARATOR
                BEQ         LC994           ;
                CMPX        #DFLTYP         ; CHECK FOR END OF ESTENSION RAM BUFFER &
                BEQ         LC978           ; 'BAD FILENAME' ERROR IF EXTENSION TOO LONG
                BSR         LC9D0           ; PUT A CHARACTER IN EXTENSION BUFFER
                BRA         LC9BE           ; GET ANOTHER EXTENSION CHARACTER
; INSERT CHARACTER INTO FILENAME OR EXTENSION
LC9D0           STA         ,X+             ; STORE CHARACTER IN FILENAME BUFFER
                BEQ         LC978           ; 'BAD FILENAME' ERROR; ZEROES ARE ILLEGAL
                CMPA        #'.             ; PERIOD?
                BEQ         LC978           ; 'BAD FILENAME' ERROR IF PERIOD
                CMPA        #'/             ; SLASH?
                BEQ         LC978           ; 'BAD FILENAME' ERROR IF SLASH
                INCA                        ;  CHECK FOR $FF
                BEQ         LC978           ; 'BAD FILENAME' ERROR IF $FF
LC9DF           RTS
; SAVE COMMAND
SAVE            CMPA        #'M             ;
                LBEQ        LCF68           ; BRANCH IF SAVEM
                BSR         LCA33           ; GO GET FILENAME, ETC. FROM BASIC
                LDX         ZERO            ; ZERO OUT X REG
                STX         DFLTYP          ; SET FILE TYPE AND ASCII FLAG TO ZERO
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER FROM BASIC
                BEQ         LCA12           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDB         #'A             ; ASCII FILE?
                JSR         >LB26F          ; SYNTAX CHECK ON CONTENTS OF ACCB
                BNE         LC9DF           ; RETURN IF NO MORE CHARACTERS ON LINE
                COM         DASCFL          ; SET CRUNCHED/ASCII FLAG TO ASCII
                BSR         LCA04           ; OPEN A SEQUENTIAL FILE FOR OUTPUT
                CLRA                        ;  SET ZERO FLAG - CAUSE ENTIRE FILE TO BE LISTED
                JMP         LIST            ; 'LIST' THE FILE TO CONSOLE OUT
; OPEN A SEQUENTIAL FILE FOR INPUT/OUTPUT - USE THE SYSTEM
; FCB LOCATED AT THE TOP OF FCBS
LCA04           LDA         #'O             ; OUTPUT FILE TYPE
LCA06           FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
LCA07           LDA         #'I             ; INPUT FILE TYPE
                LDB         FCBACT          ; GET NUMBER OF RESERVED FILES CURRENTLY RESERVED
                INCB                        ; ADD ONE - USE ONE ABOVE HIGHEST RESERVED FCB
                STB         DEVNUM          ; SAVE IT IN DEVICE NUMBER
                JMP         >LC48D          ; OPEN A FILE & INITIALIZE FCB
; SAVE A CRUNCHED FILE - A PREAMBLE OF THREE BYTES WILL PRECEED CRUNCHED
; FILES: BYTE 1 = $FF, 2,3 = LENGTH OF BASIC PROGRAM
LCA12           BSR         LCA04           ; OPEN A SEQUENTIAL FILE FOR OUTPUT
                LDA         #$FF            ; BASIC FILE FLAG
                JSR         >LCC24          ; CONSOLE OUT
                LDD         VARTAB          ; LOAD ACCD WITH START OF VARIABLES
                SUBD        TXTTAB          ; SUBTRACT START OF BASIC
                JSR         >LCC24          ; CONSOLE OUT FILE LENGTH MS BYTE
                TFR         B,A             ; PULL LS BYTE INTO ACCA
                JSR         >LCC24          ; CONSOLE OUT FILE LENGTH LS BYTE
                LDX         TXTTAB          ; POINT X TO START OF BASIC
LCA27           LDA         ,X+             ; GET BYTE FROM BASIC
                JSR         >LCC24          ; SEND TO CONSOLE OUT
                CMPX        VARTAB          ; COMPARE TO END OF BASIC
                BNE         LCA27           ; KEEP GOING IF NOT AT END
                JMP         >LA42D          ; CLOSE FILE
LCA33           LDX         #BASEXT         ; POINT TO 'BAS' EXTENSION (DEFAULT)
                JMP         >LC938          ; GET FILENAME.EXT FROM BASIC
; MERGE COMMAND
MERGE           CLRA                        ;  RUN FLAG (0 = DON'T RUN)
                LDB         #$FF            ; MERGE FLAG ($FF = MERGE)
                BRA         LCA50           ; GO LOAD THE FILE
; RUN RAM VECTOR
DVEC18          CMPA        #'"             ; CHECK FOR FILENAME DELIMITER (DOUBLE QUOTE)
                LBNE        XVEC18          ; NONE - JUMP TO EXBAS RUN RAM HOOK
                LDA         #$02            ; RUN FLAG - DON'T CLOSE ALL FILES BEFORE RUN
                BRA         LCA4F           ; LOAD THE FILE
; LOAD COMMAND
LOAD            CMPA        #'M             ;
                LBEQ        LCFC1           ; BRANCH IF LOADM
                CLRA                        ;  RUN FLAG = ZERO (DON'T RUN)
LCA4F           CLRB                        ;  CLEAR MERGE FLAG
LCA50           STA         DRUNFL          ; RUN FLAG (0 = DON'T RUN, 2 = RUN)
                STB         DMRGFL          ; MERGE FLAG (0 = NO MERGE, $FF = MERGE)
                BSR         LCA33           ; GO GET FILENAME, ETC. FROM BASIC
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                BEQ         LCA6C           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDB         #'R             ;
                JSR         >LB26F          ; IS NEXT CHAR 'R'? RUN AFTER LOAD
                JSR         >LA5C7          ; SYNTAX ERROR IF ANY MORE CHARS ON LINE
                LDA         #$03            ; SET FLAGS TO RUN AND CLOSE ALL FILES
                STA         DRUNFL          ; BEFORE THE FILE IS RUN
LCA6C           BSR         LCA07           ; GRAB FCB FOR INPUT FILE
                LDA         DASCFL          ; CHECK ASCII FLAG AND BRANCH
                BEQ         LCA7E           ; IF CRUNCHED BASIC FILE
                TST         DMRGFL          ; IS THIS A MERGE?
                BNE         LCA7B           ; BRANCH IF MERGE
                JSR         >LAD19          ; DO A 'NEW' - ERASE VARIABLES, RESET VARIABLES
LCA7B           JMP         >LAC7C          ; GO TO BASIC'S MAIN LOOP, IT WILL LOAD PROGRAM
; LOAD IN A CRUNCHED BASIC FILE
LCA7E           LDA         DFLTYP          ; CHECK FILE TYPE (MUST BE BASIC:0) & CHECK
                ORA         DMRGFL          ; MERGE FLAG (MUST BE NO MERGE: 0)
                LBNE        LA616           ; 'BAD FILE MODE' ERROR IF MERGE OR NON-BASIC
                JSR         >LAD19          ; DO A 'NEW' - RESET POINTERS, ERASE VARIABLES
                COM         DLODFL          ; SET THE LOAD FLAG TO $FF - THIS WILL CAUSE A NEW TO
; OCCUR IF AN ERROR OCCURS WHILE THE PROGRAM IS BEING LOADED
                JSR         >LCDBC          ; GET CHAR FROM BUFFER - SHOULD BE $FF
                JSR         >LCDBC          ; GET ANOTHER - MS BYTE OF LENGTH
                PSHS        A               ; SAVE MS BYTE ON STACK
                JSR         >LCDBC          ; LS BYTE OF LENGTH OF PROGRAM
                TFR         A,B             ; PUT LS BYTE INTO ACCB
                PULS        A               ; NOW ACCD CONTAINS LENGTH OF PROGRAM
                ADDD        TXTTAB          ; ADD BEGINNING OF BASIC
                JSR         >LAC37          ; SEE OF ENOUGH ROOM IN RAM FOR THIS FILE
                LDX         TXTTAB          ; GET START OF BASIC
LCAA4           JSR         >LC5C4          ; READ A CHAR FROM CONSOLE IN
                LDB         CINBFL          ; BUFFER EMPTY?
                BNE         LCAAF           ; BRANCH IF SO
                STA         ,X+             ; STORE CHAR
                BRA         LCAA4           ; GET ANOTHER CHARACTER
LCAAF           CLR         DLODFL          ; CLEAR LOAD FLAG - LOAD WAS ERROR FREE
                STX         VARTAB          ; SAVE NEW START OF VARIABLES
; MAKE SURE LAST THREE BYTES LOADED WERE ZERO
                LDB         #$03            ; CHECK THREE BYTES
LCAB6           LDA         ,-X             ; CHECK A BYTE
                BNE         LCABD           ; BRANCH IF NON-ZERO
                DECB                        ;  DECREMENT COUNTER
                BNE         LCAB6           ; KEEP CHECKING IF NOT DONE
LCABD           LDX         VARTAB          ; GET START OF VARIABLES
LCABF           STX         VARTAB          ; SAVE START OF VARIABLES
                CLR         ,X+             ; CLEAR A BYTE
                DECB                        ;  DECREMRNT COUNTER
                BPL         LCABF           ; KEEP CLEARING BYTES IF NOT DONE
LCAC6           JSR         >LA42D          ; CLOSE SELECTED FILE
                JSR         >LAD21          ; DO PART OF NEW - ERASE VARIABLES, RESET INPUT PTR
                JSR         XVEC18          ; INITIALIZE EXBAS GRAPHICS VARIABLES
                JSR         >LACEF          ; RELOCATE ALL THE BASIC NEXT LINE POINTERS
                ASR         DRUNFL          ; CHECK LSB OF RUN FLAG
                BLO         LCADA           ; BRANCH IF DON'T CLOSE ALL FILES
                JSR         >LA426          ; CLOSE ALL FILES
LCADA           ASR         DRUNFL          ; TEST BIT 1 OF RUN FLAG
                LBCS        LAD9E           ; BRANCH TO COMM INTERPRETATION LOOP IF BIT 1 SET
                JMP         >LAC73          ; RETURN TO DIRECT MODE
DVEC13          TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                BGT         LCAC6           ; TRY TO RUN FILE IF IT IS A DISK FILE
                RTS
; CLOSE ALL FILE BUFFERS RAM VECTOR
DVEC7           LDB         FCBACT          ; GET THE NUMBER OF RESERVED FILE BUFFERS
                INCB                        ;  ADD ONE
LCAED           PSHS        B               ; SAVE IT
                STB         DEVNUM          ; STORE IT IN DEVICE NUMBER
                BSR         LCB01           ; CLOSE FILE
                PULS        B               ; GET BACK NUMBER OF FILE BUFFERS
                DECB                        ;  DECREMENT FILE BUFFER COUNTER
                BNE         LCAED           ; BRANCH IF ALL FILES NOT CLOSED
LCAF8           RTS
; CLOSE FILE RAM HOOK
DVEC8           TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN
                LBLE        XVEC8           ; IF NOT A DISK FILE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
LCB01           JSR         >LC744          ; POINT X TO CORRECT FCB
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
LCB06           STX         FCBTMP          ; SAVE FILE BUFFER POINTER
                LDA         FCBTYP,X        ; GET THE TYPE OF THIS FILE
                BEQ         LCAF8           ; RETURN IF FILE NOT OPEN
                PSHS        A               ; SAVE FILE TYPE
                CLR         FCBTYP,X        ; CLOSE THE FILE - ZERO OUT THE FILE TYPE
                LDB         FCBDRV,X        ; GET DRIVE NUMBER AND
                STB         DCDRV           ; SAVE IT IN DSKCON VARIABLE
                CMPA        #OUTFIL         ; CHECK FOR OUTPUT TYPE AND
                BNE         LCB31           ; BRANCH IF NOT OUTPUT TYPE FILE
; CLOSE A SEQUENTIAL OUTPUT FILE
                LDB         FCBLFT,X        ; GET THE NUMBER OF CHARACTERS IN BUFFER
                LDA         #$80            ; SET THE PRE-SAVED BIT TO INDICATE THAT THE DATA
; HAS ALREADY BEEN SAVED ON DISK
                ORA         FCBCPT,X        ; 'OR' IN THE FULL SECTOR FLAG
                STD         FCBLST,X        ; SAVE THE NUMBER OF BYTES USED IN THE LAST SECTOR
                INC         FCBSEC,X        ; INCREMENT THE SECTOR NUMBER
                LDB         FCBCGR,X        ; GET THE CURRENT GRANULE NUMBER
                JSR         >LC755          ; POINT X TO FILE ALLOCATION TABLE
                STA         FAT1,X          ; SET FAT DATA NOT VALID FLAG (ACCA <> 0)
                ABX                         ;  ADD GRANULE OFFSET TO FAT POINTER
                INC         FATCON,X        ; INCREMENT GRANULE DATA (ADD ONE SECTOR TO LAST
; GRANULE) SKIP PAST THE SIX FAT CONTROL BYTES
LCB2E           JMP         >LCBC3          ; UPDATE FAT AND DIRECTORY
LCB31           CMPA        #RANFIL         ; RANDOM FILE?
                BNE         LCB2E           ; NO - UPDATE FAT AND DIRECTORY IF SEQUENTIAL INPUT FILE
; CLOSE A RANDOM FILE
                LDD         FCBRLN,X        ; GET RECORD LENGTH
                LDX         FCBBUF,X        ; POINT X TO RANDOM FILE BUFFER
                LEAY        D,X             ; POINT Y TO END OF RANDOM FILE BUFFER
                PSHS        Y,X,B,A         ; SAVE POINTERS ON STACK
                LEAY        ,S              ; POINT Y CURRENT STACK POINTER
                LDU         VARTAB          ; GET START OF VARIABLES
LCB41           CMPU        ARYTAB          ; COMPARE TO START OF ARRAYS
                BEQ         LCB54           ; BRANCH IF ALL VARIABLES CHECKED
                LDA         $01,U           ; GET 2ND BYTE OF VARIABLE NAME
                LEAU        $02,U           ; MOVE POINTER TO START OF DESCRIPTOR
                BPL         LCB4E           ; BRANCH IF VARIABLE - NUMERIC
                BSR         LCB76           ; ADJUST STRING VARIABLE IF IN RANDOM FILE BUFFER
LCB4E           LEAU        $05,U           ; MOVE POINTER TO NEXT VARIABLE
                BRA         LCB41           ; PROCESS ANOTHER VARIABLE
LCB52           PULS        U               ; GET ADDRESS OF NEXT ARRAY TO U
LCB54           CMPU        ARYEND          ; COMPARE TO END OF ARRAYS
                BEQ         LCB93           ; BRANCH IF END OF ARRAYS
                TFR         U,D             ; SAVE ARRAY START IN ACCD, ADD OFFSET
                ADDD        $02,U           ; TO NEXT ARRAY AND SAVE ADDRESS OF
                PSHS        B,A             ; NEXT ARRAY ON THE STACK
                LDA         $01,U           ; GET 2ND LETTER OF VARIABLE NAME
                BPL         LCB52           ; BRANCH IF NUMERIC
                LDB         $04,U           ; GET THE NUMBER OF DIMENSIONS
                ASLB                        ;  X2:2 BYTES PER DIMENSION
                ADDB        #$05            ; 5 BYTES CONSTANT PER ARRAY DESCRIPTOR
                CLRA                        ;  CLEAR MSB OF OFFSET - (ONLY 125 DIMENSIONS ALLOWED)
                LEAU        D,U             ; POINT U TO START OF THIS ARRAY'S VARIABLES
LCB6B           CMPU        ,S              ; AT END OF THIS ARRAY?
                BEQ         LCB52           ; YES
                BSR         LCB76           ; ADJUST STRING VARIABLE IF IN RANDOM FILE BUFFER
                LEAU        $05,U           ; MOVE POINTER TO NEXT DESCRIPTOR
                BRA         LCB6B           ; CHECK NEXT VARIABLE

; CHECK TO SEE IF A STRING IS LOCATED IN THE RANDOM FILE BUFFER AREA. IF IT IS
; THE RANDOM FILE BUFFER IN QUESTION, IT WILL BE DELETED. IF IT IS HIGHER IN THE RANDOM
; FILE BUFFER SPACE THAN THE BUFFER IN QUESTION, THE LENGTH OF THE CURRENT
; BUFFER WILL BE SUBTRACTED FROM THE ADDRESS OF THE STRING BECAUSE THE CURRENT
; BUFFER IS BEING DELETED (CLOSED).
LCB76           LDX         $02,U           ; POINT X TO START OF STRING
                CMPX        RNBFAD          ; COMPARE TO START OF FREE RANDOM FILE BUFFER AREA
                BHS         LCB8B           ; RETURN IF > START OF FREE RANDOM FILE BUFFER AREA
                CMPX        $02,Y           ; COMPARE TO START OF THIS FILE'S RANDOM BUFFER
                BLO         LCB8B           ; RETURN IF < START OF THIS FILE'S RANDOM BUFFER
                CMPX        $04,Y           ; COMPARE TO END OF THIS FILE'S RANDOM BUFFER
                BLO         LCB8C           ; RETURN IF < END OF THIS FILE'S RANDOM BUFFER
                TFR         X,D             ; SAVE POINTER IN ACCD
                SUBD        ,Y              ; SUBTRACT RECORD LENGTH FROM START OF STRING ADDRESS
                STD         $02,U           ; SAVE NEW START OF STRING ADDRESS
LCB8B           RTS
LCB8C           CLR         ,U              ; CLEAR THE LENGTH OF THE STRING
                CLR         $02,U           ; CLEAR THE ADDRESS
                CLR         $03,U           ; OF THE STRING
                RTS
; REMOVE RESERVED SPACE IN RANDOM FILE BUFFER FOR A 'CLOSED' RANDOM FILE
; ADJUST THE START OF RANDOM FILE BUFFER POINTER IN ALL RANDOM FCBS
LCB93           LDB         FCBACT          ; GET THE NUMBER OF ACTIVE FILES
                INCB                        ;  ADD ONE
LCB97           PSHS        B               ; SAVE FILES COUNT ON THE STACK
                JSR         >LC749          ; POINT X TO FCB
                LDA         FCBTYP,X        ; GET FILE TYPE
                CMPA        #RANFIL         ; IS IT A RANDOM FILE?
                BNE         LCBAD           ; BRANCH IF NOT
                LDD         FCBBUF,X        ; GET START OF THIS FILE'S RANDOM FILE BUFFER
                CMPD        $04,Y           ; COMPARE TO END OF RANDOM FILE BUFFER AREA AND
                BLO         LCBAD           ; BRANCH IF < END OF RANDOM FILE BUFFER AREA
                SUBD        ,Y              ; SUBTRACT RECORD LENGTH OF SELECTED FILE
                STD         FCBBUF,X        ; SAVE NEW START OF RANDOM FILE BUFFER
LCBAD           PULS        B               ; GET THE FILES COUNTER
                DECB                        ;  DECREMENT FILES COUNTER
                BNE         LCB97           ; BRANCH IF ALL FILES NOT DONE
                PULS        A,B,X,U         ; U = END OF RANDOM FILE BUFFER, X = START OF RANDOM
; FILE BUFFER, ACCD = RECORD LENGTH
; THIS WOULD PROBABLY BE THE MOST CONVENIENT PLACE TO FIX THE BUG WHICH
; CAUSES THE SYSTEM TO HANG IF AN ERROR IS ENCOUNTERED DURING 'COPY'
; CMPU FCBADR * IS THE END OF THIS FCB'S BUFFER ABOVE THE END
; OF THE START OF THE FCB AREA
; BLO LCBB4 NO - FREE UP THE SPACE USED BY THIS FILE IN RANDOM BUFFER
; LDX #DFLBUF YES - DOING A 'COPY'; RESET START OF RANDOM BUFFER
; BRA LCBC0
; RANDOM FILE BUFFER AREA
; REMOVE RESERVED SPACE FOR CLOSED FILE FROM RANDOM FILE BUFFER SPACE
LCBB4           CMPU        RNBFAD          ; AT THE BOTTOM OF FREE RANDOM BUFFER AREA?
                BEQ         LCBC0           ; BRANCH IF THERE
                LDA         ,U+             ; GRAB A SOURCE BYTE AND
                STA         ,X+             ; MOVE IT TO DESTINATION
                BRA         LCBB4           ; KEEP MOVING BYTES
LCBC0           STX         RNBFAD          ; SAVE NEW START OF FREE RANDOM BUFFER AREA
LCBC3           JSR         >LC755          ; POINT X TO PROPER FILE ALLOCATION TABLE
                DEC         FAT0,X          ; REMOVE ONE ACTIVE FILE
                TST         FAT1,X          ; NEW DATA IN FAT RAM IMAGE?
                BEQ         LCBCF           ; NO
                JSR         >LC71E          ; WRITE OUT FILE ALLOCATION TABLE TO DISK
LCBCF           LDX         FCBTMP          ; GET FILE BUFFER POINTER
                PULS        A               ; GET FILE TYPE
                CMPA        #OUTFIL         ; IS IT A SEQUENTIAL OUTPUT FILE?
                BEQ         LCBDF           ; YES
                CMPA        #RANFIL         ; IS IT A RANDOM FILE?
                BNE         LCB8B           ; RETURN IF NOT A RANDOM FILE (SEQUENTIAL INPUT)
                LDA         FCBFLG,X        ; TEST THE GET/PUT FLAG AND
                BEQ         LCBE9           ; BRANCH IF 'GET'
; WRITE CONTENTS OF FILE BUFFER TO DISK
LCBDF           JSR         >LC763          ; GET PROPER TRACK & SECTOR NUMBERS
                LEAU        FCBCON,X        ; POINT U TO START OF FCB DATA
                STU         DCBPT           ; SET UP FILE BUFFER POINTER FOR DSKCON
                BSR         LCC15           ; GO WRITE A SECTOR
LCBE9           LDA         FCBLST,X        ; CHECK THE PRE-SAVED FLAG
                BPL         LCB8B           ; RETURN IF RECORD HAS ALREADY BEEN SAVED ON DISK
                LDB         FCBDIR,X        ; GET DIRECTORY NUMBER OF THIS FILE
                ANDB        #$07            ; 8 ENTRIES PER SECTOR
                LDA         #DIRLEN         ; DIRLEN BYTES PER DIRECTORY ENTRY
                MUL                         ;  GET SECTOR OFFSET FOR THIS ENTRY
                LDU         #DBUF0          ; GET READ/WRITE BUFFER 0 AND
                STU         DCBPT           ; SAVE IT IN DSKCON REGISTER
                LEAY        D,U             ; Y POINTS TO CORRECT DIRECTORY ENTRY
                LDB         FCBDIR,X        ; GET DIRECTORY ENTRY NUMBER
                LSRB                        ;
                LSRB                        ;
                LSRB                        ;  DIVIDE BY 8 EIGHT DIRECTORY ENTRIES PER SECTOR
                ADDB        #$03            ; ADD BIAS; FIRST 3 SECTORS NOT DIRECTORY
                STB         DSEC            ; STORE SECTOR NUMBER
                LDD         #$1102          ; DIRECTORY TRACK - READ OP CODE
                STA         DCTRK           ; STORE TRACK NUMBER
                BSR         LCC17           ; GO READ DIRECTORY
                LDD         FCBLST,X        ; GET NUMBER OF BYTES IN THE LAST SECTOR
                ANDA        #$7F            ; MASK OFF THE PRE-SAVED FLAG
                STD         DIRLST,Y        ; SAVE NUMBER OF BYTES IN LAST SECTOR OF FILE IN DIRECTORY
LCC15           LDB         #$03            ; WRITE OP CODE
LCC17           STB         DCOPC           ; SAVE DSKCON OP CODE VARIABLE
                JMP         >LD6F2          ; GO READ/WRITE SECTOR
; CONSOLE OUT RAM HOOK
DVEC3           TST         DEVNUM          ; CHECK DEVICE NUMBER
                LBLE        XVEC3           ; BRANCH TO EX BASIC IF NOT A DISK FILE
                LEAS        $02,S           ; POP RETURN OFF STACK
; SEND A CHARACTER IN ACCA TO A DISK FILE. A CARRIAGE RETURN WILL RESET THE
; PRINT POSITION AND CONTROL CODES WILL NOT INCREMENT THE PRINT POSITION.
LCC24           PSHS        X,B,A           ; SAVE REGISTERS
                LDX         #FCBV1-2        ; POINT X TO TABLE OF FILE NUMBER VECTORS
                LDB         DEVNUM          ; GET CURRENT FILE NUMBER
                ASLB                        ;  2 BYTES PER FCB ADDRESS
                LDX         B,X             ; POINT X TO PROPER FCB
                LDB         ,X              ; GET FILE TYPE (FCBTYP,X)
                CMPB        #INPFIL         ; IS IT AN INPUT FILE?
                BEQ         LCC6A           ; RETURN IF SO
                CMPA        #CR             ; CARRIAGE RETURN (ENTER)
                BNE         LCC3A           ; NO
                CLR         FCBPOS,X        ; CLEAR PRINT POSITION IF CARRIAGE RETURN
LCC3A           CMPA        #SPACE          ;
                BLO         LCC40           ; BRANCH IF CONTROL CHAR
                INC         FCBPOS,X        ; INCREMENT PRINT POSITION
LCC40           CMPB        #RANFIL         ; IS IT RANDOM FILE?
                BNE         LCC5E           ; BRANCH IF NOT RANDOM
; PUT A BYTE INTO A RANDOM FILE
                LDD         FCBPUT,X        ; GET 'PUT' BYTE COUNTER
                ADDD        #$0001          ; ADD ONE
                CMPD        FCBRLN,X        ; COMPARE TO RECORD LENGTH
                LBHI        LCDCB           ; 'FR' ERROR IF 'PUT' BYTE COUNTER > RECORD LENGTH
                STD         FCBPUT,X        ; SAVE NEW 'PUT' BYTE COUNTER
                LDX         FCBBUF,X        ; POINT TO RANDOM FILE BUFFER POINTER
                LEAX        D,X             ; POINT TO ONE PAST END OF CURRENT RECORD DATA
                PULS        A               ; PULL DATA FROM STACK
                STA         -1,X            ; STORE IN DATA BUFFER
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
; WRITE A BYTE TO SEQUENTIAL OUTPUT FILE
LCC5E           INC         FCBLFT,X        ; INCREMENT CHARACTER COUNT
                LDB         FCBLFT,X        ; GET CHARACTER COUNT AND BRANCH
                BEQ         LCC6C           ; IF THE BUFFER IS FULL
                ABX                         ;  ADD CHARACTER COUNT TO FCB ADDRESS
                STA         FCBCON-1,X      ; STORE NEW CHARACTER (SKIP PAST 25 CONTROL BYTES AT FCB START)
LCC6A           PULS        A,B,X,PC
; WRITE OUT A FULL BUFFER AND RESET BUFFER
LCC6C           PSHS        U,Y             ; SAVE REGISTERS
                STA         SECLEN+FCBCON-1,X ; STORE LAST CHARACTER IN BUFFER
                LDB         FCBDRV,X        ; GET DRIVE NUMBER AND SAVE
                STB         DCDRV           ; IT IN DSKCON CONTROL TABLE
                INC         FCBSEC,X        ; INCREMENT SECTOR NUMBER
                JSR         >LCBDF          ; WRITE THE FILE BUFFER TO DISK
                LEAY        ,X              ; SAVE FCB POINTER IN Y
                LDB         FCBCGR,X        ; GET GRANULE NUMBER
                JSR         >LC755          ; POINT X TO PROPER ALLOCATION TABLE
                ABX                         ;  ADD THE GRANULE NUMBER TO FAT POINTER
                LEAU        FATCON,X        ; POINT U TO THE CORRECT GRANULE IN FAT - SKIP PAST THE SIX FAT CONTROL BYTES
                LDA         FCBSEC,Y        ; GET CURRENT SECTOR FOR THIS GRANULE
                CMPA        #$09            ; MAX SECTOR NUMBER (9 SECTORS/GRANULE)
                BLO         LCC99           ; BRANCH IF NOT AT END OF GRANULE


                DEC         FCBSEC,Y        ; DECREMENT SECTOR NUMBER AND INCREMENT ERROR FLAG IN
                INC         FCBCPT,Y        ; CASE ERROR FOUND WHILE LOOKING FOR NEXT GRANULE
; THE ERROR FLAG IS USED TO INDICATE THAT ANOTHER SECTOR
; MUST BE ADDED TO THE LENGTH OF FILE FOLLOWING ERROR PROCESSING.
                JSR         >LC7BF          ; GET NEXT FREE GRANULE
                CLR         FCBSEC,Y        ; CLEAR SECTOR NUMBER AND
                CLR         FCBCPT,Y        ; ERROR FLAG - DISK WAS NOT FULL
                STA         FCBCGR,Y        ; SAVE NEW GRANULE IN FCB



                FCB         $8C             ; SKIP TWO BYTES NO DATA STORED IN NEW SECTOR YET (THROWN AWAY CMPX INSTRUCTION)
LCC99           ORA         #$C0            ; FORCE GRANULE NUMBER TO BE FINAL GRANULE IN FILE
                STA         ,U              ; STORE IN MAP
                LEAX        ,Y              ; POINT X TO FCB
                JSR         >LC685          ; INCREMENT RECORD NUMBER
                JSR         >LC5A9          ; UPDATE FILE ALLOCATION TABLE
                PULS        Y,U             ; RESTORE REGISTERS
                PULS        A,B,X,PC        ; RESTORE REGISTERS AND RETURN

; DIR COMMAND
DIR             JSR         >LD24F          ; SCAN DRIVE NUMBER FROM INPUT LINE
                JSR         >LC79D          ; GET FAT FOR THIS DRIVE
                JSR         >LB958          ; PRINT CARRIAGE RETURN TO CONSOLE OUT
                LDD         #$1102          ; GET TRACK 17 AND
                STA         DCTRK           ; READ OP CODE AND
                STB         DCOPC           ; SAVE IN DSKCON VARIABLES
                LDB         #$03            ; START WITH SECTOR 3 (FIRST DIRECTORY SECTOR)
                
; READ A DIRECTORY SECTOR INTO THE I/O BUFFER
LCCBB           STB         DSEC            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                LDX         #DBUF0          ; USE I/O BUFFER 0 FOR DATA TRANSFER
                STX         DCBPT           ; SAVE IN DSKCON VARIABLE
                JSR         >LD6F2          ; READ A SECTOR

; SEND DIRECTORY INFORMATION TO CONSOLE OUT
LCCC5           PULS        U               ; SAVE TOP OF STACK
                JSR         >LA549          ; GO DO A BREAK CHECK
                PSHS        U               ; RESTORE STACK
                LDA         ,X              ; TEST FILE NAME FIRST BYTE (DIRNAM,X)
                BEQ         LCD08           ; BRANCH IF KILLED
                COMA                        ;  FF END OF DIRECTORY
                BEQ         LCD17           ; RETURN IF END OF DIRECTORY
                PSHS        X               ; SAVE DIRECTORY POINTER ON STACK
                LDB         #$08            ; NUMBER CHARACTERS TO PRINT
                JSR         >LB9A2          ; SEND FILENAME TO CONSOLE OUT
                BSR         LCD1B           ; SEND BLANK TO CONSOLE OUT
                LDB         #$03            ; NUMBER CHARACTERS TO PRINT
                JSR         >LB9A2          ; SEND EXTENSION TO CONSOLE OUT
                BSR         LCD1B           ; SEND BLANK TO CONSOLE OUT
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #10             ; CHECK THE NUMBER OF DECIMAL DIGITS IN
                BHS         LCCEB           ; ACCB: IF THERE IS ONLY ONE DIGIT,
                BSR         LCD1B           ; SEND BLANK TO CONSOLE OUT
LCCEB           CLRA                        ;  CLEAR MS BYTE OF ACCO
                JSR         >LBDCC          ; PRINT ACCD IN DECIMAL TO CONSOLE OUT
                BSR         LCD1B           ; SEND BLANK TO CONSOLE OUT
                LDX         ,S              ; X NOW POINTS TO DIRECTORY ENTRY
                LDA         #'A+1           ; ASCII BIAS
                ADDA        DIRASC,X        ; ADD TO ASCII FLAG
                BSR         LCD18           ; PRINT CHARACTER AND BLANK TO CONSOLE OUT
                LDB         DIRGRN,X        ; GET FIRST GRANULE IN FILE
                BSR         LCD1E           ; COUNT GRANULES
                TFR         A,B             ; SAVE COUNT IN ACCB
                CLRA                        ;  CLEAR MS BYTE OF ACCD
                JSR         >LBDCC          ; PRINT ACCO IN DECIMAL TO CONSOLE OUT
                JSR         >LB958          ; SEND CARRIAGE RETURN TO CONSOLE OUT
                PULS        X               ; PULL DIRECTORY POINTER OFF OF THE STACK
LCD08           LEAX        DIRLEN,X        ; MOVE X TO NEXT DIRECTORY ENTRY
                CMPX        #DBUF0+SECLEN   ; END OF I/O BUFFER?
                BLO         LCCC5           ; BRANCH IF MORE DIRECTORY ENTRIES IN BUFFER
                LDB         DSEC            ; GET CURRENT SECTOR
                INCB                        ;  BUMP COUNT
                CMPB        #SECMAX         ; SECMAX SECTORS IN DIRECTORY TRACK
                BLS         LCCBB           ; GET NEXT SECTOR
LCD17           RTS                         ; FINISHED
LCD18           JSR         PUTCHR          ; SEND CHARACTER TO CONSOLE OUT
LCD1B           JMP         >LB9AC          ; SEND BLANK TO CONSOLE OUT
; ENTER WITH ACCB POINTING TO FIRST GRANULE IN A FILE; RETURN THE NUMBER OF
; GRANULES IN THE FILE IN ACCA, THE GRANULE DATA FOR THE LAST SECTOR IN ACCB
LCD1E           JSR         >LC755          ; POINT X TO FILE ALLOCATION BUFFER
                LEAU        FATCON,X        ; POINT U TO START OF GRANULE DATA
                CLRA                        ;  RESET GRANULE COUNTER
LCD24           INCA                        ;  INCREMENT GRANULE COUNTER
                CMPA        #GRANMX         ; CHECKED ALL 68 GRANULES?
                LBHI        LC653           ; YES - 'BAD FILE STRUCTURE' ERROR
                LEAX        ,U              ; POINT U TO START OF GRANULE DATA
                ABX                         ;  ADD POINTER TO FIRST GRANULE
                LDB         ,X              ; GET THIS GRANULE'S CONTROL BYTE
                CMPB        #$C0            ; IS THIS THE LAST GRANULE IN FILE?
                BLO         LCD24           ; NO - KEEP GOING
                RTS
; INPUT RAM HOOK
DVEC10          TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN
                BLE         LCD97           ; IF NOT A DISK FILE
                LDX         #LB069          ; CHANGE THE RETURN ADDRESS ON THE STACK TO RE-ENTER BASIC'S INPUT
                STX         ,S              ; ROUTINE AT A DIFFERENT PLACE THAN THE CALLING ROUTINE
                LDX         #LINBUF+1       ; POINT X TO THE LINE INPUT BUFFER
                LDB         #',             ; =
                STB         CHARAC          ; COMMA IS READ ITEM SEPARATOR (TEMPORARY STRING SEARCH FLAG)
                LDA         VALTYP          ; GET VARIABLE TYPE AND BRANCH IF
                BNE         LCD4B           ; IT IS A STRING
                LDB         #SPACE          ; SPACE = NUMERIC SEARCH DELIMITER
LCD4B           BSR         LCDBC           ; GET AN INPUT CHARACTER
                CMPA        #SPACE          ; SPACE?
                BEQ         LCD4B           ; YES - GET ANOTHER CHARACTER
                CMPA        #'"             ; QUOTE?
                BNE         LCD5F           ; NO
                CMPB        #',             ; SEARCH CHARACTER = COMMA?
                BNE         LCD5F           ; NO - NUMERIC SEARCH
                TFR         A,B             ; SAVE DOUBLE QUOTE AS
                STB         CHARAC          ; THE SEARCH FLAG
                BRA         LCD81           ; SAVE DOUBLE QUOTES AS FIRST ITEM IN BUFFER
LCD5F           CMPB        #'"             ;
                BEQ         LCD74           ; BRANCH IF INPUTTING A STRING VARIABLE
                CMPA        #CR             ; IS THE INPUT CHARACTER A CARRIAGE RETURN
                BNE         LCD74           ; NO
                CMPX        #LINBUF+1       ; IF AT THE START OF INPUTBUFFER, CHECK FOR A
                BEQ         LCDB0           ; FOLLOWING LINE FEED AND EXIT ROUTINE
                LDA         -1,X            ; IF THE INPUT CHARACTER PRECEEDING THE CR WAS A LINE FEED,
                CMPA        #LF             ; THEN INSERT THE CR IN THE INPUT STRING, OTHERWISE
                BNE         LCDB0           ; CHECK FOR A FOLLOWING LINE FEED AND EXIT THE ROUTINE
                LDA         #CR             ; RESTORE CARRIAGE RETURN AS THE INPUT CHARACTER
LCD74           TSTA                        ;  CHECK FOR A NULL (ZERO) INPUT CHARACTER AND
                BEQ         LCD8E           ; IGNORE IT IF lT IS A NULL
                CMPA        CHARAC          ;
                BEQ         LCD98           ; CHECK TO SEE IF THE INPUT CHARACTER MATCHES
                PSHS        B               ; EITHER ACCB OR CHARAC AND IF IT DOES, THEN
                CMPA        ,S+             ; BRANCH TO CHECK FOR ITEM SEPARATOR OR
                BEQ         LCD98           ; TERMINATOR SEQUENCE AND EXIT ROUTINE
LCD81           STA         ,X+             ; STORE NEW CHARACTER IN BUFFER
                CMPX        #LINBUF+LBUFMX  ; END OF INPUT BUFFER
                BNE         LCD8E           ; NO
                BSR         LCDD0           ; GET A CHARACTER FROM CONSOLE IN
                BNE         LCD92           ; EXIT ROUTINE IF BUFFER EMPTY
                BRA         LCDAC           ; CHECK FOR CR OR CR/LF AND EXIT ROUTINE
LCD8E           BSR         LCDD0           ; GET A CHARACTER FROM CONSOLE IN
                BEQ         LCD5F           ; BRANCH IF BUFFER NOT EMPTY
LCD92           CLR         ,X              ; PUT A ZERO AT END OF BUFFER WHEN DONE
                LDX         #LINBUF         ; POINT (X) TO LINBUF - RESET POINTER
LCD97           RTS
; CHECK FOR ITEM SEPARATOR OR TERMINATOR AND EXIT THE INPUT ROUTINE
LCD98           CMPA        #'"             ; QUOTE?
                BEQ         LCDA0           ; YES
                CMPA        #SPACE          ; SPACE?
                BNE         LCD92           ; NO - EXIT ROUTINE
LCDA0           BSR         LCDD0           ; GET A CHARACTER FROM CONSOLE IN
                BNE         LCD92           ; EXIT ROUTINE IF BUFFER EMPTY
                CMPA        #SPACE          ; SPACE?
                BEQ         LCDA0           ; YES - GET ANOTHER CHARACTER
                CMPA        #',             ; COMMA (ITEM SEPARATOR)?
                BEQ         LCD92           ; YES - EXIT ROUTINE
LCDAC           CMPA        #CR             ; CARRIAGE RETURN?
                BNE         LCDB8           ; NO
LCDB0           BSR         LCDD0           ; GET A CHARACTER FROM CONSOLE IN
                BNE         LCD92           ; EXIT ROUTINE IF BUFFER EMPTY
                CMPA        #LF             ; LINE FEED? TREAT CR,LF AS A CR
                BEQ         LCD92           ; YES - EXIT ROUTINE
LCDB8           BSR         LCDD6           ; BACK UP PTR INPUT POINTER ONE
                BRA         LCD92           ; EXIT ROUTINE
LCDBC           BSR         LCDD0           ; GET A CHAR FROM INPUT BUFFER - RETURN IN ACCA
                BEQ         LCDD5           ; RETURN IF BUFFER NOT EMPTY
                JSR         >LC744          ; POINT X TO START OF FILE BUFFER
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #RANFIL         ; IS IT RANDOM FILE TYPE?
                LBNE        LC352           ; 'INPUT PAST END OF FILE ERROR IF NOT RANDOM
LCDCB           LDB         #2*37           ; 'WRITE/INPUT PAST END OF RECORD ERROR IF RANDOM
                JMP         >LAC46          ; JUMP TO THE ERROR HANDLER
LCDD0           JSR         >LA176          ; GET A CHAR FROM INPUT BUFFER
                TST         CINBFL          ; SET FLAGS ACCORDING TO CONSOLE INPUT FLAG
LCDD5           RTS
; MOVE THE INPUT POINTER BACK ONE (DISK FILE)
LCDD6           PSHS        X,B             ; SAVE REGISTERS ON STACK
                JSR         >LC744          ; POINT X TO PROPER FCB
                LDB         FCBTYP,X        ; GET FILE TYPE OF THIS FCB
                CMPB        #RANFIL         ; IS IT A RANDOM FILE?
                BNE         LCDEC           ; BRANCH IF NOT A RANDOM FILE
                LDD         FCBGET,X        ; GRAB THE RANDOM FILE 'GET' POINTER,
                SUBD        #$0001          ; MOVE IT BACK ONE AND RESTORE IT
                STD         FCBGET,X        ;
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
LCDEC           STA         FCBCDT,X        ; SAVE THE CHARACTER IN THE CACHE
                COM         FCBCFL,X        ; SET THE CACHE FLAG TO $FF - DATA IN CACHE
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
; CVN COMMAND
CVN             JSR         >LB654          ; GET LENGTH AND ADDRESS OF STRING
                CMPB        #$05            ; FIVE BYTES IN A FLOATING POINT NUMBER
                LBCS        LB44A           ; 'FC' ERROR IF <> 5 BYTES
                CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                JMP         >LBC14          ; COPY A PACKED FP NUMBER FROM (X) TO FPA0
; MKN$ COMMAND
MKN             JSR         >LB143          ; 'TM' ERROR IF VALTYP=STRING
                LDB         #$05            ; FIVE BYTES IN A FLOATING POINT NUMBER
                JSR         >LB50F          ; RESERVE FIVE BYTES IN STRING SPACE
                JSR         >LBC35          ; PACK FPA0 AND STORE IT IN STRING SPACE
                JMP         >LB69B          ; SAVE STRING DESCRIPTOR ON STRING STACK
; LOC COMMAND
LOC             BSR         LCE19           ; POINT X TO FILE BUFFER
                LDD         FCBREC,X        ; GET RECORD NUMBER (RANDOM FILE) OR SECTOR CTR (SEQUENTIAL)
LCE14           STD         FPA0+2          ; SAVE ACCD IN BOTTOM 2 BYTES OF FPA0 AND
                JMP         >L880E          ; CONVERT TO FLOATING POINT NUMBER
; STRIP A DEVICE NUMBER FROM A BASIC STATEMENT, SET PRINT
; PARAMETERS ACCORDING TO IT - ERROR IF FILE NOT
; OPEN. RETURN WITH (X) POINTING TO THAT FILE'S FCB
LCE19           LDA         DEVNUM          ; GET CURRENT DEVICE NUMBER AND
                PSHS        A               ; SAVE IT ON THE STACK
                JSR         >LB143          ; 'TM' ERROR IF VALTYP=STRING
                JSR         >LA5AE          ; CHECK FOR VALID DEVICE NUMBER/SET PRINT PARAMETERS
                TST         DEVNUM          ; CHECK DEVICE NUMBER
                LBLE        LB44A           ; BRANCH IF NOT DISK FILE 'ILLEGAL FUNCTION CALL'
                JSR         >LC744          ; POINT (X) TO FILE BUFFER
                PULS        A               ; GET OLD DEVICE NUMBER OFF OF THE STACK AND
                STA         DEVNUM          ; SAVE IT AS DEVICE NUMBER
                TST         FCBTYP,X        ; IS FILE OPEN?
                LBEQ        LA3FB           ; 'FILE NOT OPEN' ERROR IF NOT OPEN
                RTS
; LOF
LOF             BSR         LCE19           ; POINT X TO FILE BUFFER
                LDA         FCBDRV,X        ; GET DRIVE NUMBER AND SAVE IT
                STA         DCDRV           ; IN DSKCON VARIABLE
                LDB         FCBFGR,X        ; GET FIRST GRANULE OF FILE
                PSHS        X               ; SAVE FCB POINTER ON STACK
                JSR         >LCD1E          ; FIND TOTAL NUMBER OF GRANULES IN THIS FILE
                DECA                        ;  SUBTRACT THE LAST GRANULE IN THE FILE
                ANDB        #$3F            ; GET NUMBER OF SECTORS USED IN LAST GRANULE
                PSHS        B               ; SAVE NUMBER OF SECTORS IN LAST GRANULE ON STACK
                TFR         A,B             ; CONVERT ACCA TO POSITIVE
                CLRA                        ;  2 BYTE VALUE IN ACCD
                JSR         >LC779          ; MULT NUMBER OF FULL GRANULES BY 9
                ADDB        ,S+             ; ADD NUMBER SECTORS IN LAST TRACK
                ADCA        #$00            ; PROPAGATE CARRY TO MS BYTE OF ACCD
                PULS        X               ; GET FCB POINTER BACK
                PSHS        A               ; SAVE ACCA ON STACK
                LDA         FCBTYP,X        ; GET FILE TYPE OF THIS FCB AND
                CMPA        #RANFIL         ; CHECK TO SEE IF IT'S A RANDOM FILE
                PULS        A               ; RESTORE ACCA
                BNE         LCE14           ; IF NOT A RANDOM FILE, THEN THE TOTAL NUMBER OF SECTORS IN THE FILE
; IS THE LENGTH OF THE FILE
; CALCULATE LOF FOR A RANDOM FILE - THE LENGTH OF A RANDOM FILE IS THE
; NUMBER OF RECORDS IN THE FILE.
                PSHS        X               ; SAVE FCB POINTER ON STACK
                SUBD        ZERO            ; SUBTRACT ZERO FROM ACCD (NUMBER OF SECTORS)
                BEQ         LCE68           ; BRANCH IF ZERO SECTORS
                SUBD        #$0001          ; SUBTRACT ONE SECTOR - THE LAST SECTOR MAY NOT BE IOOZ USED
LCE68           BSR         LCE14           ; PUT ACCD INTO FPA0
                LDB         FP0EXP          ; GET EXPONENT OF FPA0
                BEQ         LCE72           ; BRANCH IF FPA0 = 0
                ADDB        #$08            ; ADD 8 TO EXPONENT (MULTIPLY FPA0 BY
                STB         FP0EXP          ; 256 BYTES/SECTOR) AND SAVE NEW EXPONENT
LCE72           JSR         >LBC5F          ; SAVE NUMBER OF BYTES IN FULL SECTORS IN FPA1
                LDX         ,S              ; POINT X TO FCB
                LDD         FCBLST,X        ; GET NUMBER OF BYTES IN LAST SECTOR
                ANDA        #$7F            ; MASK OFF THE PRE-SAVED BYTE
                BSR         LCE14           ; PUT NUMBER BYTES IN LAST SECTOR INTO FPA0
                CLR         RESSGN          ; FORCE SUM SIGN = POSITIVE
                LDA         FP1EXP          ; GET EXPONENTS OF FPA0 AND
                LDB         FP0EXP          ; FPA1 PRIOR TO ADDITION
                JSR         >LB9C5          ; ADD NUMBER BYTES IN LAST SECTOR TO NUMBER OF BYTES IN FULL SECTORS
                JSR         >LBC5F          ; SAVE TOTAL NUMBER OF BYTES IN FPA1
                PULS        X               ; POINT X TO FCB
                LDD         FCBRLN,X        ; GET RECORD LENGTH
                BSR         LCE14           ; PUT IT INTO FPA0
                CLR         RESSGN          ; FORCE QUOTIENT SIGN = POSITIVE
                LDA         FP1EXP          ; GET EXPONENTS OF FPA0 AND
                LDB         FP0EXP          ; FPA1 PRIOR TO DIVISION
                JSR         >LBB91          ; DIVIDE TOTAL NUMBER OF BYTES BY NUMBER OF BYTES IN A RECORD
                JMP         INT             ; CONVERT FPA0 TO AN INTEGER
; FREE COMMAND
FREE            JSR         >LB143          ; NUMBER TYPE CHECK
                JSR         >LB70E          ; EVALUATE NUMERIC EXPRESSION AND RETURN VALUE IN ACCB
                CMPB        #$03            ; ONLY 4 LEGAL DRIVES
                LBHI        LA61F           ; 'DEVICE NUMBER' ERROR IF DRIVE NUMBER IS > 3
                STB         DCDRV           ; SAVE IN DRIVE NUMBER
                JSR         >LC79D          ; GET FILE ALLOCATION TABLE AND STORE IN BUFFER
                JSR         >LC755          ; POINT X TO START OF FILE ALLOCATION TABLE BUFFER
                LEAX        FATCON,X        ; MOVE TO FIRST GRANULE DATA BYTE
                CLR         ,-S             ; SPACE FOR FREE GRANULE COUNTER
                LDB         #GRANMX         ; GET MAXIMUM NUMBER OF GRANULES
LCEB6           LDA         ,X+             ; GET GRANULE DATA
                COMA                        ;  FREE GRANULES $FF
                BNE         LCEBD           ; BRANCH IF NOT FREE
                INC         ,S              ; INCREMENT FREE GRANULE COUNTER
LCEBD           DECB                        ;  DECREMENT GRANULE COUNTER
                BNE         LCEB6           ; BRANCH IF NOT DONE
                PULS        B               ; GET FREE GRANULE COUNTER TO ACCB
                JMP         >LB4F3          ; LOAD ACCB INTO FPA0
; DRIVE COMMAND
DRIVE           JSR         EVALEXPB        ; EVALUATE EXPR; RETURN VALUE IN ACCB
                CMPB        #$03            ; MAX DRIVE NUMBER = 3
                LBHI        LA61F           ; 'DEVICE #' ERROR IF DRIVE NUMBER > 3
                STB         DEFDRV          ; SAVE DEFAULT DRIVE NUMBER
                RTS
; EVALUATE EXPRESSION RAM VECTOR
DVEC15          LDA         $04,S           ; CHECK STACKED PRECEDENCE FLAG AND IF IT IS NOT AN END
                BNE         LCEE9           ; OF OPERATION, BRANCH TO EXTENDED BASIC'S EXPRESSION EVALUATION ROUTINE
                LDX         $05,S           ;
                CMPX        #LAF9A          ;
                BNE         LCEE9           ; CHECK TWO RETURN ADDRESSES BACK ON THE STACK
                LDX         $02,S           ; TO SEE IF THE CALL TO EVALUATE EXPRESSION IS
                CMPX        #LB166          ; COMING FROM THE 'LET' COMMAND - BRANCH OUT IF
                BNE         LCEE9           ; NOT COMING FROM 'LET'
                LDX         #LCEEC          ; IF COMING FROM 'LET', REPLACE THE RETURN ADDR
                STX         $05,S           ; WITH THE DISK BASIC 'LET' MODIFIER ADDRESS
LCEE9           JMP         XVEC15          ; EXTENDED BASIC EXPRESSION EVALUATION
; LET MODIFIER
LCEEC           PULS        A               ; PULL VARIABLE TYPE OFF OF THE STACK
                RORA                        ;  SET CARRY IF SIRING, CLEAR CARRY IF NUMERIC
                JSR         >LB148          ; DO A 'TM' CHECK
                LBEQ        LBC33           ; IF NUMERIC VARIABLE, PACK FPA0 INTO VARDES
                LDX         FPA0+2          ; POINT X TO STRING DESCRIPTOR
                LDD         $02,X           ; GET ADDRESS OF SIRING
                CMPD        #DFLBUF         ; COMPARE TO START OF RANDOM FILE BUFFERS
                BLO         LCF07           ; AND BRANCH IF LOWER
                SUBD        FCBADR          ; SUBTRACT OUT THE END OF RANDOM FILE BUFFERS
                LBCS        LAFB1           ; BRANCH IF STRING STORED IN RANDOM FILE BUFFER - MOVE IT INTO THE STRING SPACE
LCF07           JMP         >LAFA4          ; BRANCH BACK TO BASICS 'LET' COMMAND
; FOR EXBAS COMMAND INTERPRETATION HANDLER
DXCVEC          CMPA        #$CA            ; TOKEN FOR DLOAD?
                BEQ         LCF2A           ; YES
                CMPA        #$C8            ; TOKEN FOR PMODE?
                LBNE        L813C           ; NO
; DISK BASIC MODIFIER FOR PMODE - ALLOWS FOR THE RAM THE DOS USES
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                CMPA        #',             ; CHECK FOR COMMA
                LBEQ        L9650           ; BRANCH IF COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION; RETURN VALUE IN ACCB
                CMPB        #$04            ; CHECK FOR PMODE 4
                LBHI        LB44A           ; 'FC' ERROR IF PMODE > 4
                LDA         GRPRAM          ; NUMBER BLOCKS BEFORE GRAPHICS PAGES
                JMP         >L962E          ; JUMP TO EXEAS' PMODE COMMAND
; DISK BASIC DLOAD MODIFIER
LCF2A           JSR         >LA429          ; CLOSE FILES
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                JMP         >DLDBUG         ; JUMP TO EXEAS' DLOAD
DXIVEC          CMPB        #($9A-$80)*2    ; MODIFIED TOKEN FOR POS
                LBNE        L8168           ; IF NOT POS, GO TO EXBAS SECONDARY COMM HANDLER
                JSR         >LB262          ; SYNTAX CHECK FOR '(' AND EVALUATE EXPRESSION
                LDA         DEVNUM          ; GET DEVICE NUMBER AND
                PSHS        A               ; SAVE IT ON STACK
                JSR         >LA5AE          ; EVALUATE DEVICE NUMBER
                JSR         >LA406          ; TEST DEVICE NUMBER
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND BRANCH
                BLE         LCF5C           ; IF NOT A DISK FILE
                JSR         >LC744          ; POINT X TO FCB
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #RANFIL         ; DIRECT/RANDOM FILE?
                BNE         LCF5C           ; BRANCH IF NOT A RANDOM FILE
                PULS        A               ; RESTORE DEVICE NUMBER
                STA         DEVNUM          ;
                LDD         FCBPUT,X        ; GRAB THE 'PUT' DATA ITEM COUNTER AND CONVERT
                JMP         GIVABF          ; IT TO A FLOATING POINT NUMBER
LCF5C           JSR         >LA35F          ; SET PRINT PARAMETERS
                PULS        A               ; RESTORE DEVICE NUMBER
                STA         DEVNUM          ;
                LDB         DEVPOS          ; GET PRINT POSITION AND
                JMP         >LB4F3          ; CONVERT IT TO FLOATING POINT NUMBER IN FPA0
; SAVEM COMMAND
LCF68           JSR         GETNCH          ; GET NEXT INPUT CHARACTER
                BSR         LCFBB           ; GET FILENAME, ETC.
                JSR         >L836C          ; EVALUATE EXPRESSION, PUT II (2 BYTES) ON STACK
                JSR         >L836C          ; DITTO
                CMPX        $02,S           ; COMPARE END ADDRESS TO START ADDRESS
                LBCS        LB44A           ; IF START > END, THEN 'ILLEGAL FUNCTION CALL'
                JSR         >L836C          ; EVAL EXPRESSION (TRANSFER ADDRESS), PUT ON STACK
                JSR         >LA5C7          ; SYNTAX ERROR IF ANY MORE CHARS ON THIS LINE
                LDD         #$0200          ; FILE TYPE=2, ASCII FLAG = CRUNCHED (0)
                STD         DFLTYP          ;
                JSR         >LCA04          ; GET NEXT UNOPEN FILE AND INITIALIZE FCB
                CLRA                        ;  ZERO FLAG - FIRST BYTE OF PREAMBLE
                BSR         LCFB5           ; WRITE A BYTE TO BUFFER
                LDD         $02,S           ; GET END ADDRESS
                SUBD        $04,S           ; SUBTRACT THE START ADDRESS
                ADDD        #$0001          ; THE SAVED DATA BLOCK WILL INCLUDE BOTH THE FIRST AND LAST BYTES
                TFR         D,Y             ; SAVE LENGTH IN Y
                BSR         LCFB3           ; WRITE FILE LENGTH TO BUFFER - FIRST ARGUMENT OF PREAMBLE
                LDD         $04,S           ; GET THE START ADDRESS
                BSR         LCFB3           ; WRITE OUT THE START ADDRESS - SECOND PREAMBLE ARGUMENT
                LDX         $04,S           ; GET START ADDRESS
LCF9B           LDA         ,X+             ; GRAB A BYTE
                JSR         >LCC24          ; WRITE IT OUT
                LEAY        -1,Y            ; DECREMENT BYTE COUNTER
                BNE         LCF9B           ; BRANCH IF ALL BYTES NOT DONE
                LDA         #$FF            ; FIRST BYTE OF POSTAMBLE
                BSR         LCFB5           ; WRITE IT OUT - EOF RECORD
                CLRA                        ;  FIRST ARGUMENT OF POSTAMBLE IS
                CLRB                        ;  A DUMMY - ZERO VALUE
                BSR         LCFB3           ; WRITE OUT POSTAMBLE FIRST ARGUMENT
                PULS        A,B,X,Y         ; GET CONTROL ADDRESSES FROM THE STACK
                BSR         LCFB3           ; WRITE OUT THE TRANSFER ADDRESS - 2ND ARGUMENT
                JMP         >LA42D          ; GO CLOSE ALL FILES
; WRITE ACCD TO THE BUFFER
LCFB3           BSR         LCFB5           ; WRITE ACCA TO BUFFER, THEN SWAP ACCA,ACCB
LCFB5           JSR         >LCC24          ; WRITE ACCA TO BUFFER
                EXG         A,B             ; SWAP ACCA,ACCB
                RTS
LCFBB           LDX         #BINEXT         ; POINT TO .BIN EXTENSION
                JMP         >LC938          ; GET FILENAME, ETC.
; LOADM COMMAND
LCFC1           JSR         GETNCH          ; GET NEXT INPUT CHARACTER
                BSR         LCFBB           ; GET FILENAME, ETC.
                JSR         >LCA07          ; OPEN NEXT AVAILABLE FILE FOR INPUT
                LDD         DFLTYP          ; GET FILE TYPE AND ASCII FLAG
                SUBD        #$0200          ; FOR LOADM FILE: TYPE=2, ASCII FLAG=0
                LBNE        LA616           ; 'BAD FILE MODE' ERROR
                LDX         ZERO            ; ZERO OUT X REG - DEFAULT VALUE OF OFFSET
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                BEQ         LCFDE           ; BRANCH IF END OF LINE - NO OFFSET
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVALUATE EXPRESSION
LCFDE           STX         VD3             ; STORE OFFSET IN VD3
                JSR         >LA5C7          ; SYNTAX ERROR IF OTHER CHARACTERS ON LINE
; GET PREAMBLE/POSTAMBLE
LCFE3           JSR         >LCDBC          ; GET FIRST BYTE
                PSHS        A               ; SAVE IT ON THE STACK
                BSR         LD013           ; GET FIRST ARGUMENT
                TFR         D,Y             ; SAVE IT IN Y
                BSR         LD013           ; GET THE SECOND ARGUMENT
                ADDD        VD3             ; ADD IT TO THE OFFSET
                STD         EXECJP          ; STORE IT IN THE JUMP ADDRESS OF THE EXEC COMMAND
                TFR         D,X             ; SAVE IT IN X
                LDA         ,S+             ; GET THE FIRST BYTE OFF OF THE STACK
                LBNE        LA42D           ; CLOSE FILE IF POSTAMBLE (EOF)
; GET RECORD BYTE(S)
LCFFA           JSR         >LC5C4          ; GET BYTE FROM BUFFER
                LDB         CINBFL          ; GET STATUS OF CONSOLE IN BUFFER
                BEQ         LD004           ; BRANCH IF BUFFER NOT EMPTY
                JMP         >LC352          ; 'INPUT PAST END OF FILE' ERROR
LD004           STA         ,X              ; STORE BYTE IN MEMORY
                CMPA        ,X+             ; TEST TO SEE IF IT STORED PROPERLY AND
                BEQ         LD00D           ; BRANCH IF PROPER STORE (NOT IN ROM OR BAD RAM)
                JMP         >LD709          ; 'I/O ERROR' IF BAD STORE
LD00D           LEAY        -1,Y            ; DECREMENT BYTE COUNT
                BNE         LCFFA           ; GET NEXT BYTE IF NOT DONE
                BRA         LCFE3           ; READ ANOTHER PRE/POST AMBLE
; READ TWO BYTES FROM BUFFER - RETURN THEM IN ACCD
LD013           BSR         LD015           ; READ A BYTE, SAVE IT IN ACCB
LD015           JSR         >LCDBC          ; GET A CHARACTER FROM INPUT BUFFER, RETURN IT IN ACCA
                EXG         A,B             ; SWAP ACCA,ACCB
                RTS
; RENAME COMMAND
RENAME          LDX         CHARAD          ; SAVE CURRENT INPUT POINTER
                PSHS        X               ; ON THE STACK
                BSR         LD056           ; GET FILENAME OF SOURCE FILE
                LDA         DCDRV           ; SAVE DRIVE NUMBER
                PSHS        A               ; ON THE STACK
                BSR         LD051           ; SYNTAX CHECK FOR 'TO' AND GET NEW FILENAME
                PULS        A               ; GET SOURCE DRIVE NUMBER
                CMPA        DCDRV           ; COMPARE TO NEW FILE DRIVE NUMBER
                LBNE        LB44A           ; 'FC' ERROR IF FlIES ON DIFFERENT DRIVES
                BSR         LD059           ; VERIFY THAT NEW FILE DOES NOT ALREADY EXIST
                PULS        X               ; RESTORE INPUT POINTER
                STX         CHARAD          ;
                BSR         LD056           ; GET SOURCE FILENAME AGAIN
                JSR         >LC68C          ; SCAN DIRECTORY FOR SOURCE FILENAME
                JSR         >LC6E5          ; 'NE' ERROR IF NOT FOUND
                BSR         LD051           ; SYNTAX CHECK FOR 'TO' AND GET NEW FILENAME
                LDX         #DNAMBF         ; POINT X TO FILENAME
                LDU         V974            ; POINT U TO DIRECTORY ENTRY OF SOURCE FILE
                LDB         #$0B            ; 11 CHARACTERS IN FILENAME AND EXTENSION
                JSR         >LA59A          ; COPY NEW FILENAME TO SOURCE FILE DIRECTORY RAM IMAGE
                LDB         #$03            ; GET WRITE OP CODE AND
                STB         DCOPC           ; SAVE IN DSKCON VARIABLE
                JMP         >LD6F2          ; WRITE NEW DIRECTORY SECTOR
; DO A SYNTAX CHECK FOR 'TO AND STRIP A FILENAME FROM BASIC
LD051           LDB         #$A5            ; 'TO' TOKEN
                JSR         >LB26F          ; SYNTAX CHECK FOR 'TO'
LD056           JMP         >LC935          ; GET FILENAME FROM BASIC
LD059           JSR         >LC68C          ; SCAN DIRECTORY FOR FILENAME
                LDB         #33*2           ; 'FILE ALREADY EXISTS' ERROR
                TST         V973            ; CHECK FOR A MATCH
                LBNE        LAC46           ; 'AE' ERROR IF FILE IN DIRECTORY
                RTS
; WRITE COMMAND
WRITE           LBEQ        LB958           ; PRINT CARRIAGE RETURN TO CONSOLE OUT IF END OF LINE
                BSR         LD06F           ; GO WRITE AN ITEM LIST
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
LD06E           RTS
LD06F           CMPA        #'#             ; CHECK FOR DEVICE NUMBER FLAG
                BNE         LD082           ; DEFAULT TO CURRENT DEVICE NUMBER IF NONE GIVEN
                JSR         >LA5A5          ; SET DEVICE NUMBER; CHECK VALIDITY
                JSR         >LA406          ; MAKE SURE SELECTED FILE IS AN OUTPUT FILE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                LBEQ        LB958           ; PRINT CR TO CONSOLE OUT IF END OF LINE
LD07F           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
LD082           JSR         >LB156          ; EVALUATE EXPRESSION
                LDA         VALTYP          ; GET VARIABLE TYPE
                BNE         LD0A7           ; BRANCH IF STRING
                JSR         >LBDD9          ; CONVERT FP NUMBER TO ASCII STRING
                JSR         >LB516          ; PUT ON TEMPORARY STRING STACK
                JSR         >LB99F          ; PRINT STRING TO CONSOLE OUT
; PRINT ITEM SEPARATOR TO CONSOLE OUT
LD092           JSR         GETCCH          ; GET CURRENT CHARACTER
                LBEQ        LB958           ; PUT CR TO CONSOLE OUT IF END OF LINE
                LDA         #',             ; COMMA: NON-CASSETTE SEPARATOR
                JSR         >LA35F          ; SET PRINT PARAMETERS
                TST         PRTDEV          ; GET CONSOLE PRINT DEVICE AND
                BEQ         LD0A3           ; BRANCH IF NOT CASSETTE
                LDA         #CR             ; GET CARRIAGE RETURN - CASSETTE ITEM SEPARATOR
LD0A3           BSR         LD0B9           ; SEND SEPARATOR TO CONSOLE OUT
                BRA         LD07F           ; GET NEXT ITEM
; PRINT A STRING TO CONSOLE OUT
LD0A7           BSR         LD0B0           ; PRINT LEADING STRING DELIMITER (")
                JSR         >LB99F          ; PRINT STRING TO CONSOLE OUT
                BSR         LD0B0           ; PRINT ENDING STRING DELIMITER (")
                BRA         LD092           ; GO PRINT SEPARATOR
; PRINT STRING DELIMITER (") TO CONSOLE OUT
LD0B0           JSR         >LA35F          ; SET PRINT PARAMETERS
                TST         PRTDEV          ; GET CONSOLE PRINT DEVICE AND
                BNE         LD06E           ; RETURN IF CASSETTE
                LDA         #'"             ; QUOTE: NON-CASSETTE STRING DELIMITER
LD0B9           JMP         PUTCHR          ; SEND TO CONSOLE OUT
; FIELD COMMAND
FIELD           JSR         >LC82E          ; EVALUATE DEVICE NUMBER & VERIFY RANDOM FILE OPEN
                CLRA                        ;
                CLRB                        ;  CLEAR TOTAL FIELD LENGTH COUNTER
                PSHS        X,B,A           ; SAVE FCB POINTER & INITIALIZE TOTAL FIELD LENGTH TO ZERO
LD0C3           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BNE         LD0C9           ; BRANCH IF NOT END OF LINE
                PULS        A,B,X,PC        ; CLEAN UP STACK AND RETURN
LD0C9           JSR         >LB738          ; SYNTAX CHECK FOR COMMA, EVALUATE EXPRESSION
                PSHS        X,B             ; SAVE FIELD LENGTH (ACCB) ON STACK, X IS A DUMMY WHICH WILL RESERVE 2 BYTES FOR THE ADDRESS WHICH WILL BE CALCULATED BELOW
; AT THIS POINT THE STACK WILL HAVE THE FOLLOWING INFORMATION ON IT:
; ,S = FIELD LENGTH 1 2,S = RANDOM FILE BUFFER ADDRESS
; 3 4,S = TOTAL FIELD LENGTH 5 6,S = FCD POINTER
                CLRA                        ;  CLEAR MS BYTE
                ADDD        $03,S           ; ADD FIELD LENGTH TO TOTAL FIELD LENGTH COUNTER
                BLO         LD0DA           ; 'FO' ERROR IF SUM > $FFFF
                LDX         $05,S           ; POINT X TO FCB
                CMPD        FCBRLN,X        ; COMPARE TO RECORD LENGTH & BRANCH IF
                BLS         LD0DF           ; TOTAL FIELD LENGTH < RECORD LENGTH
LD0DA           LDB         #34*2           ; 'FIELD OVERFLOW' ERROR
                JMP         >LAC46          ; JUMP TO ERROR DRIVER
LD0DF           LDU         $03,S           ; LOAD U WITH OLD TOTAL LENGTH OF ALL FIELDS
                STD         $03,S           ; SAVE NEW TOTAL FIELD LENGTH
                LDD         FCBBUF,X        ; POINT ACCD TO START OF RANDOM FILE BUFFER
                LEAU        D,U             ; POINT U TO THIS FIELD'S SLOT IN THE RANDOM
                STU         $01,S           ; FILE BUFFER AND SAVE IT ON THE STACK
                LDB         #$FF            ; SECONDARY TOKEN
                JSR         >LB26F          ; SYNTAX CHECK FOR SECONDARY TOKEN
                LDB         #$A7            ; 'AS' TOKEN
                JSR         >LB26F          ; SYNTAX CHECK FOR 'AS' TOKEN
                JSR         >LB357          ; EVALUATE VARIABLE
                JSR         >LB146          ; 'TM' ERROR IF NUMERIC VARIABLE
                PULS        B,U             ; PULL STRING ADDRESS AND LENGTH
                STB         ,X              ; OFF OF THE STACK AND SAVE THEM
                STU         $02,X           ; IN STRING DESCRIPTOR
                BRA         LD0C3           ; CHECK FOR ANOTHER FIELD SPECIFICATION
; RSET COMMAND
RSET            FCB         $86             ; SKIP ONE BYTE (THROWN AWAY LDA INSTRUCTION)
; LSET COMMAND
LSET            CLRA                        ;  LSET FLAG = 0
                PSHS        A               ; SAVE RSET($4F),LSET(00) FLAG ON THE STACK
                JSR         >LB357          ; EVALUATE FIELD STRING VARIABLE
                JSR         >LB146          ; 'TM' ERROR IF NUMERIC VARIABLE
                PSHS        X               ; SAVE STRING DESCRIPTOR ON STACK
                LDX         $02,X           ; POINT X TO ADDRESS OF STRING
                CMPX        #DFLBUF         ; COMPARE STRING ADDRESS TO START OF RANDOM
                BLO         LD119           ; FILE BUFFER; 'SE' ERROR IF < RANDOM FILE BUFFER
                CMPX        FCBADR          ; COMPARE STRING ADDRESS TO TOP OF RANDOM FILE BUFFER
                BLO         LD11E           ; AREA - BRANCH IF STRING IN RANDOM FILE BUFFER
LD119           LDB         #2*35           ; 'SET TO NON-FIELDED STRING' ERROR
                JMP         >LAC46          ; JUMP TO ERROR HANDLER
LD11E           LDB         #$B3            ;
                JSR         >LB26F          ; SYNTAX CHECK FOR '=' TOKEN
                JSR         >L8748          ; EVALUATE DATA STRING EXPRESSION; RETURN WITH X
; POINTING TO STRING; ACCB = LENGTH
                PULS        Y               ; POINT Y TO FIELD STRING DESCRIPTOR
                LDA         ,Y              ; GET LENGTH OF FIELD STRING
                BEQ         LD15A           ; RETURN IF NULL STRING
                PSHS        B               ; SAVE LENGTH OF DATA STRING ON STACK
                LDB         #SPACE          ; PREPARE TO FILL DATA STRING WITH BLANKS
                LDU         $02,Y           ; POINT U TO FIELD STRING ADDRESS
; FILL THE FIELDED STRING WITH BLANKS
LD132           STB         ,U+             ; STORE A SPACE IN FIELDED STRING
                DECA                        ;  DECREMENT LENGTH COUNTER
                BNE         LD132           ; KEEP FILLING W/SPACES IF NOT DONE
                LDB         ,S+             ; GET THE LENGTH OF THE DATA STRING AND
                BEQ         LD15A           ; RETURN IF IT IS NULL (ZERO)
                CMPB        ,Y              ; COMPARE LENGTH OF DATA STRING TO LENGTH OF FIELD
                BLO         LD143           ; STRING, BRANCH IF FIELD STRING > DATA STRING
                LDB         ,Y              ; GET THE LENGTH OF THE FIELD STRING AND FORCE THE
                CLR         ,S              ; RSET/LSET FLAG TO LSET (0) IF DATA STRING LENGTH IS
; >= THE FIELD STRING LENGTH. THIS WILL CAUSE THE RIGHT
; SIDE OF THE DATA STRING TO BE TRUNCATED
LD143           LDU         $02,Y           ; LOAD U WITH THE ADDRESS OF THE FIELD STRING
                TST         ,S+             ; GET THE RSET/LSET FLAG FROM THE STACK
                BEQ         LD157           ; AND BRANCH IF LSET
; RSET ROUTINE
                PSHS        B               ; SAVE THE NUMBER OF BYTES TO MOVE INTO THE FIELD STRING
                CLRA                        ;  = TAKE THE 2'S COMPLEMENT OF AN UNSIGNED
                NEGB                        ;  = NUMBER IN ACCB - LEAVE THE DOUBLE BYTE SIGNED
                SBCA        #$00            ; RESULT IN ACCD
                ADDB        ,Y              ; ADD THE LENGTH OF THE FIELD STRING TO THE INVERSE
                ADCA        #$00            ; OF THE NUMBER OF BYTES TO BE MOVED
                LEAU        D,U             ; ADD RESULT TO START OF FIELD STRING. NOW U
; WILL POINT TO (-NUMBER OF BYTES TO MOVE)
; FROM THE RIGHT SIDE OF THE FIELD STRING
                PULS        B               ; GET THE NUMBER OF BYTES TO MOVE
LD157           JMP         >LA59A          ; MOVE ACCB BYTES FROM X TO U (DATA TO FIELD STRING)
LD15A           PULS        A,PC            ; PULL LSET/RSET FLAG OFF OF STACK AND RETURN
; FILES COMMAND
FILES           JSR         >L95AC          ; RESET SAM DISPLAY PAGE AND VDG MODE
                LDD         FCBADR          ; GET START OF FILE BUFFERS
                SUBD        #DFLBUF         ; SUBTRACT THE START OF RANDOM FILE BUFFER SPACE
                PSHS        B,A             ; SAVE DEFAULT VALUE OF RANDOM FILE BUFFER SPACE ON STACK
                LDB         FCBACT          ; GET CURRENT NUMBER OF FCBS
                PSHS        B               ; AND SAVE ON THE STACK (DEFAULT VALUE)
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                CMPA        #',             ; CHECK FOR COMMA
                BEQ         LD181           ; BRANCH IF COMMA - NO BUFFER NUMBER PARAMETER GIVEN
                JSR         EVALEXPB        ; EVALUATE EXPRESSION (BUFFER NUMBER)
                CMPB        #15             ; 15 FCBS MAX
                LBHI        LB44A           ; BRANCH IF > 15 - 'ILLEGAL FUNCTION CALL'
                STB         ,S              ; SAVE NUMBER OF FCBS ON STACK
                JSR         GETCCH          ; CHECK CURRENT INPUT CHAR
                BEQ         LD189           ; BRANCH IF END OF LINE
LD181           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB3E6          ; EVALUATE EXPRESSION, RETURN VALUE IN ACCD
                STD         $01,S           ; SAVE RANDOM FILE BUFFER SIZE ON STACK
LD189           JSR         DVEC7           ; CLOSE FILES
                LDB         ,S              ; GET THE NUMBER OF BUFFERS TO MAKE AND
                PSHS        B               ; INITIALIZE A BUFFER COUNTER ON THE STACK
                LDD         #DFLBUF         ; GET START OF RANDOM FILE BUFFERS
                ADDD        $02,S           ; ADD THE NEWLY SPECIFIED RANDOM FILE BUFFER SPACE
                BLO         LD208           ; 'OUT OF MEMORY' ERROR IF > $FFFF
                STD         $02,S           ; SAVE START OF FCBS
; RESERVE SPACE FOR FCBS
LD199           ADDD        #FCBLEN         ; FCBLEN REQUIRED FOR EACH BUFFER
                BLO         LD208           ; 'OUT OF MEMORY' ERROR IF > $FFFF
                DEC         ,S              ; DECREMENT BUFFER COUNTER
                BPL         LD199           ; BRANCH IF NOT DONE - THE BPL WILL SET UP ONE MORE BUFFER
; THAN THE NUMBER REQUESTED. THIS EXTRA BUFFER IS THE SYSTEM BUFFER
; AND IS LOCATED AT THE END OF THE NORMAL FCBS. ONLY SYSTEM ROUTINES
; (COPY, BACKUP, MERGE ETC.) MAY ACCESS THIS BUFFER.
                TSTB                        ;  AT AN EXACT 256 BYTE BOUNDARY?
                BEQ         LD1A8           ; YES
                INCA                        ;  NO - ADD 256
                BEQ         LD208           ; 'OUT OF MEMORY' ERROR IF PAST $FFFF
LD1A8           BITA        #$01            ; ON A 512 BYTE BOUNDARY?
                BEQ         LD1AF           ; YES
                INCA                        ;  NO - ADD 256
                BEQ         LD208           ; 'OM' ERROR IF PAST $FFFF
LD1AF           STA         ,S              ; SAVE MS BYTE OF NEW GRAPHIC RAM START
                LDD         VARTAB          ; GET START OF VARIABLES
                SUBA        GRPRAM          ; SUBTRACT THE OLD GRAPHIC RAM START - ACCD CONTAINS LENGTH
; OF PROGRAM PLUS RESERVED GRAPHIC RAM
                ADDA        ,S              ; ADD IN THE AMOUNT OF RAM CALCULATED ABOVE
                BLO         LD208           ; 'OUT OF MEMORY' ERROR IF > $FFFF
                TFR         D,X             ; SAVE NEW VARTAB IN X
                INCA                        ;  ADD 256 - TO GUARANTEE ENOUGH ROOM SINCE ALL CALCULATIONS USE
; ONLY THE MSB OF THE ADDRESS
                BEQ         LD208           ; 'OUT OF MEMORY' ERROR IF PAST $FFFF
                CMPD        FRETOP          ; IS IT GREATER THAN THE START OF STRING SPACE
                BHS         LD208           ; 'OUT OF MEMORY' IF > START OF STRING SPACE
                DECA                        ;  SUBTRACT 256 - COMPENSATE FOR INCA ABOVE
                SUBD        VARTAB          ; SUBTRACT START OF VARIABLES
                ADDD        TXTTAB          ; ADD START OF BASIC
                TFR         D,Y             ; Y HAS NEW START OF BASIC
                LDA         ,S              ; GET THE GRAPHIC RAM START, SUBTRACT
                SUBA        GRPRAM          ; THE OLD GRAPHIC RAN START AND SAVE
                TFR         A,B             ; THE DIFFERENCE IN ACCA AND ACCB
                ADDA        BEGGRP          ; ADD THE OLD GRAPHIC PAGE START AND
                STA         BEGGRP          ; STORE THE NEW START OF GRAPHICS RAM
                ADDB        ENDGRP          ; ADD THE OLD GRAPHIC RAM END ADDRESS AND
                STB         ENDGRP          ; STORE THE NEW END OF GRAPHICS RAM
                PULS        A,B,U           ; ACCA=MSB OF START OF GRAPHIC RAM; ACCB=NUMBER OF FILE BUFFERS
; U=START OF FILE BUFFERS
                STA         GRPRAM          ; SAVE NEW START OF GRAPHIC RAM
                STB         FCBACT          ; NUMBER OF FILE BUFFERS
                STU         FCBADR          ; START OF FILE BUFFERS
                LDA         CURLIN          ; GET CURRENT LINE NUMBER
                INCA                        ;  ARE WE IN DIRECT MODE?
                BEQ         LD1EF           ; YES - MOVE BASIC PROGRAM
                TFR         Y,D             ; MOVE NEW START OF BASIC TO ACCD
                SUBD        TXTTAB          ; SUBTRACT OLD START OF BASIC
                ADDD        CHARAD          ; ADD OLD INPUT POINTER
                STD         CHARAD          ; SAVE NEW INPUT POINTER
LD1EF           LDU         VARTAB          ; POINT U TO OLD START OF VARIABLES
                STX         VARTAB          ; SAVE NEW START OF VARIBLES
                CMPU        VARTAB          ; COMPARE OLD START OF VARIABLES TO NEW START OF
                BHI         LD20B           ; VARIABLES & BRANCH IF OLD > NEW
; MOVE BASIC PROGRAM IF OLD START ADDRESS <= NEW START ADDRESS
LD1F8           LDA         ,-U             ; GET A BYTE
                STA         ,-X             ; MOVE lT
                CMPU        TXTTAB          ; AT START OF BASIC PROGRAM?
                BNE         LD1F8           ; NO
                STY         TXTTAB          ; STORE NEW START OF BASIC PROGRAM
                CLR         -1,Y            ; RESET START OF PROGRAM FLAG
                BRA         LD21B           ; CLOSE ALL FILES
LD208           JMP         >LAC44          ; 'OUT OF MEMORY' ERROR
; MOVE BASIC PROGRAM IF OLD START ADDRESS > NEW START ADDRESS
LD20B           LDU         TXTTAB          ; POINT U TO OLD START OF BASIC
                STY         TXTTAB          ; SAVE NEW START OF BASIC
                CLR         -1,Y            ; RESET START OF BASIC FLAG
LD212           LDA         ,U+             ; GET A BYTE
                STA         ,Y+             ; MOVE IT
                CMPY        VARTAB          ; AT START OF VARIABLES
                BNE         LD212           ; NO - MOVE ANOTHER BYTE
; CLOSE ALL FCBS AND RECALCULATE FCB START ADDRESSES
LD21B           LDU         #FCBV1          ; POINT U TO FILE BUFFER POINTERS
                LDX         FCBADR          ; POINT X TO START OF BUFFERS
                CLRB                        ;  RESET FILE COUNTER
LD222           STX         ,U++            ; STORE FILE ADDRESS IN VECTOR TABLE
                CLR         FCBTYP,X        ; RESET FILE TYPE TO CLOSED
                LEAX        FCBLEN,X        ; GO TO NEXT FCB
                INCB                        ;  INCREMENT FILE COUNTER
                CMPB        FCBACT          ; CLOSE ALL ACTIVE BUFFERS AND SYSTEM FCB
                BLS         LD222           ; BRANCH IF NOT DONE
                JMP         >L96CB          ; READJUST LINE NUMBERS, ETC.
; UNLOAD COMMAND
UNLOAD          BSR         LD24F           ; GET DRIVE NUMBER
                CLRB                        ;  CLEAR FILE COUNTER
LD236           INCB                        ;  INCREMENT FILE COUNTER
                JSR         >LC749          ; POINT X TO FCB
                BEQ         LD249           ; BRANCH IF FILE NOT OPEN
                LDA         FCBDRV,X        ; CHECK DRIVE NUMBER
                CMPA        DCDRV           ; DOES IT MATCH THE 'UNLOAD' DRIVE NUMBER?
                BNE         LD249           ; NO MATCH - DO NOT CLOSE THE FILE
                PSHS        B               ; SAVE FILE COUNTER ON THE STACK
                JSR         >LCB06          ; CLOSE FCB
                PULS        B               ; RESTORE FILE COUNTER
LD249           CMPB        FCBACT          ; CHECKED ALL FILES?
                BLS         LD236           ; NO
                RTS
; GET DRIVE NUMBER FROM BASIC - USE THE DEFAULT DRIVE IF NONE GIVEN
LD24F           LDB         DEFDRV          ; GET DEFAULT DRIVE NUMBER
                JSR         GETCCH          ; GET NEXT INPUT CHAR
                BEQ         LD25F           ; USE DEFAULT DRIVE NUMBER IF NONE GIVEN
LD256           JSR         EVALEXPB        ; EVALUATE EXPRESSION
                CMPB        #$03            ; 4 DRIVES MAX
                LBHI        LA61F           ; 'DEVICE NUMBER ERROR' IF > 3
LD25F           STB         DCDRV           ; STORE IN DSKCON VARIABLE
                RTS
; BACKUP COMMAND
BACKUP          LBEQ        LA61F           ; DEVICE NUMBER ERROR IF NO DRIVE NUMBERS GIVEN
                JSR         >L95AC          ; RESET SAM DISPLAY PAGE AND VOG MODE
                JSR         >LD256          ; GET SOURCE DRIVE NUMBER AND SAVE
                STB         DBUF0+255       ; IT AT TOP OF DBUF0 (TOP OF NEW STACK)
                JSR         GETCCH          ; GET A CHARACTER FROM BASIC
                BEQ         LD27B           ; BRANCH IF END OF LINE
                LDB         #$A5            ; TOKEN FOR 'TO'
                JSR         >LB26F          ; SYNTAX CHECK FOR 'TO'
                JSR         >LD256          ; GET DESTINATION DRIVE NUMBER
LD27B           LDS         #DBUF0+255      ; PUT STACK AT TOP OF DBUF0
                PSHS        B               ; SAVE DESTINATION DRIVE NUMBER ON STACK
                JSR         >LA5C7          ; SYNTAX ERROR IF NOT END OF LINE
                JSR         DVEC7           ; CLOSE ALL FILES
                CLR         ,-S             ; CLEAR A TRACK COUNTER ON STACK
                LDX         #DFLBUF-1       ; POINT X TO TOP OF DISK RAM VARIABLES
LD28C           INC         ,S              ; INCREMENT TRACK COUNTER
                LEAX        SECMAX*SECLEN,X ; INCREMENT X BY ONE TRACK
                CMPX        MEMSIZ          ; COMPARE TO TOP OF NON RESERVED RAN
                BLS         LD28C           ; KEEP GOING IF MORE FREE RAM LEFT
                DEC         ,S              ; DECREMENT TRACK COUNTER
                LBEQ        LAC44           ; 'OM' ERROR IF < 1 TRACK OF FREE RAM
                LDA         #TRKMAX         ; GET MAXIMUM NUMBER OF TRACKS INITIALIZE REMAINING TRACKS CTR
                CLRB                        ;  INITIALIZE TRACKS WRITTEN COUNTER TO ZERO
                PSHS        B,A             ; SAVE TRACKS WRITTEN AND REMAINING COUNTERS ON STACK
; AT THIS POINT THE STACK HAS THE FOLLOWING DATA ON IT:
; ,S = TRACKS REMAINING COUNTER; 1,S = TRACKS WRITTEN COUNTER
; 2,S = NUMBER OF TRACKS WHICH FIT IN RAM; 3,S = DESTINATION DRIVE NUMBER
; 4,S = SOURCE DRIVE NUMBER
                COM         DRESFL          ; SET THE DISK RESET FLAG TO CAUSE A RESET
LD2A4           CLRB                        ;  INITIALIZE WRITE TRACK COUNTER TO ZERO
LD2A5           INCB                        ;  ADD ONE TO WRITE TRACK COUNTER
                DEC         ,S              ; DECREMENT REMAINING TRACKS COUNTER
                BEQ         LD2AE           ; AND BRANCH IF NO TRACKS LEFT
                CMPB        $02,S           ; COMPARE WRITE TRACK COUNTER TO NUMBER OF TRACKS THAT
                BNE         LD2A5           ; WILL FIT IN RAM AND BRANCH IF ROOM FOR MORE TRACKS IN RAM
LD2AE           STB         TMPLOC          ; SAVE THE NUMBER OF TRACKS TO BE TRANSFERRED
                LDB         $04,S           ; GET SOURCE DRIVE NUMBER
                BSR         LD2FC           ; FILL RAM BUFFER WITH TMPLOC TRACKS OF DATA
                LDA         #$FF            ; SET SOURCE/DESTINATION FLAG TO DESTINATION
                JSR         >LD322          ; PRINT PROMPT MESSAGE IF NEEDED
                LDB         $03,S           ; GET DESTINATION DRIVE NUMBER
                BSR         LD2FF           ; WRITE TMPLOC TRACKS FROM BUFFER
                TST         ,S              ; TEST TRACKS REMAINING FLAG
                BEQ         LD2CD           ; BRANCH IF BACKUP DONE
                CLRA                        ;  SET SOURCE/DESTINATION FLAG TO SOURCE
                JSR         >LD322          ; PRINT PROMPT MESSAGE IF NEEDED
                LDB         $01,S           ; GET THE TRACKS WRITTEN COUNTER, ADD THE NUMBER OF
                ADDB        TMPLOC          ; TRACKS MOVED THIS TIME THROUGH LOOP AND
                STB         $01,S           ; SAVE THE NEW TRACKS WRITTEN COUNTER
                BRA         LD2A4           ; COPY SOME MORE TRACKS
LD2CD           BSR         LD2D2           ; CHECK FOR DOS INITIALIZATION
                JMP         >LAC73          ; JUMP BACK TO BASICS MAIN LOOP
LD2D2           PULS        U               ; PUT THE RETURN ADDRESS IN U
                LDA         DRESFL          ; TEST DISK RESET FLAG
                BEQ         LD2EF           ; DONT RESET THE DOS IF FLAG NOT SET
                LDX         #FCBV1          ; POINT X TO TABLE OF FCB ADDRESSES
                CLRA                        ;  SET FILE COUNTER TO ZERO
LD2DD           CLR         [,X++]          ; MARK FCB AS CLOSED
                INCA                        ;  ADD ONE TO FILE COUNTER
                CMPA        FCBACT          ; COMPARE TO NUMBER OF RESERVED FILES
                BLS         LD2DD           ; BRANCH IF ANY FILES NOT SHUT DOWN
                LDX         TXTTAB          ; LOAD X WITH THE START OF BASIC
                CLR         -1,X            ; SET FIRST BYTE OF BASIC PROGRAM TO ZERO
                JSR         >LAD19          ; GO DO A 'NEW'
                CLR         DRESFL          ; RESET THE DOS RESET FLAG
LD2EF           LDA         DLODFL          ; CHECK THE LOAD RESET FLAG AND
                BEQ         LD2FA           ; BRANCH IF NOT SET
                CLR         DLODFL          ; CLEAR THE LOAD RESET FLAG
                JSR         >LAD19          ; GO DO A 'NEW'
LD2FA           JMP         ,U              ; JUMP BACK TO RETURN ADDRESS SAVED IN U ABOVE
LD2FC           LDA         #$02            ; READ OP CODE
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
LD2FF           LDA         #$03            ; WRITE OP CODE
                STD         DCOPC           ; SAVE IN DSKCON VARIABLE
                LDA         $03,S           ; GET THE NUMBER OF THE TRACK BEING CURRENTLY
                STA         DCTRK           ; WRITTEN AND SAVE IT IN DSKCON VARIABLE
                LDX         #DFLBUF         ; TRACK BUFFER STARTS AT DFLBUF
                STX         DCBPT           ; SAVE IT IN DSKCON VARIABLE
                LDA         TMPLOC          ; GET NUMBER OF TRACKS TO MOVE
LD30E           LDB         #$01            ; INITIALIZE SECTOR COUNTER TO ONE
LD310           STB         DSEC            ; SAVE DSKCON SECTOR VARIABLE
                JSR         >LD6F2          ; READ/WRITE A SECTOR
                INC         DCBPT           ; MOVE BUFFER POINTER UP ONE SECTOR (256 BYTES)
                INCB                        ;  INCREMENT SECTOR COUNTER
                CMPB        #SECMAX         ; COMPARE TO MAXIMUM NUMBER OF SECTORS PER TRACK
                BLS         LD310           ; BRANCH IF ANY SECTORS LEFT
                INC         DCTRK           ; INCREMENT TRACK COUNTER VARIABLE TO NEXT TRACK
                DECA                        ;  DECREMENT TRACKS TO MOVE COUNTER
                BNE         LD30E           ; READ MORE TRACKS IF ANY LEFT
                RTS
LD322           LDB         $05,S           ; GET THE DESTINATlON DRIVE NUMBER AND
                CMPB        $06,S           ; COMPARE IT TO THE SOURCE DRIVE NUMBER
; PRINT SOURCE/DESTINATION DISK SWITCH PROMPT MESSAGE
LD326           BNE         LD35E           ; RETURN IF DRIVE NUMBERS NOT EQUAL
                CLR         RDYTMR          ; RESET THE READY TIMER
                CLR         DSKREG          ; CLEAR DSKREG - TURN OFF ALL DISK MOTORS
                CLR         DRGRAM          ; CLEAR DSKREG RAM IMAGE
                PSHS        A               ; SAVE SOURCE/DESTINATION FLAG ON STACK
                JSR         >CLRSCRN        ; CLEAR SCREEN
                LDX         #LD35F          ; POINT X TO 'INSERT SOURCE' MESSAGE
                LDB         #13             ; 13 BYTES IN MESSAGE
                LDA         ,S+             ; GET SOURCE/DESTINATION FLAG FROM THE STACK
                BEQ         LD344           ; BRANCH IF SOURCE
                LDX         #LD36C          ; POINT X TO 'INSERT DESTINATION' MESSAGE
                LDB         #18             ; 18 BYTES IN MESSAGE
LD344           JSR         >LB9A2          ; SEND MESSAGE TO CONSOLE OUT
                LDX         #LD37E          ; POINT X TO 'DISKETTE AND' MESSAGE
                LDB         #27             ; 27 BYTES IN MESSAGE
                JSR         >LB9A2          ; SEND MESSAGE TO CONSOLE OUT
                LDD         #$6405          ; SET UP 'SOUND' PARAMETERS
                STA         SNDTON          ; FOR A BEEP
                JSR         >LA951          ; JUMP TO 'SOUND' - DO A BEEP
LD357           JSR         >LA171          ; GET A CHARACTER FROM CONSOLE IN
                CMPA        #CR             ; KEEP LOOKING AT CONSOLE IN UNTIL
                BNE         LD357           ; YOU GET A CARRIAGE RETURN
LD35E           RTS
LD35F           FCC         'INSERT SOURCE'
LD36C           FCC         'INSERT DESTINATION'
LD37E           FCC         ' DISKETTE AND'
                FCB         CR
                FCC         "PRESS 'ENTER'"
; PUSH FILENAME.EXT AND DRIVE NUMBER ONTO THE STACK
LD399           PULS        Y               ; SAVE RETURN ADDRESS IN Y
                LDB         #11             ; 11 CHARACTERS IN FILENAME AND EXTENSION
                LDX         #DNAMBF+11      ; POINT X TO TOP OF DISK NAME/EXT BUFFER
LD3A0           LDA         ,-X             ; GET A CHARACTER FROM FILENAME.
                PSHS        A               ; EXT BUFFER AND PUSH IT ONTO THE
                DECB                        ;  STACK - DECREMENT COUNTER AND
                BNE         LD3A0           ; KEEP LOOPING UNTIL DONE
                LDA         DCDRV           ; GET DRIVE NUMBER AND PUSH
                PSHS        A               ; IT ONTO THE STACK
                JMP         ,Y              ; PSEUDO - RETURN TO CALLING ROUTINE
; PULL FILENAME.EXT AND DRIVE NUMBER FROM (X) TO RAM
LD3AD           LDA         ,X+             ; GET DRIVE NUMBER AND SAVE
                STA         DCDRV           ; IT IN DSKCON VARIABLE
                LDB         #11             ; 11 BYTES IN FILENAME AND EXTENSION
                LDU         #DNAMBF         ; POINT U TO DISK NAME BUFFER
                JMP         >LA59A          ; MOVE FILENANE.EXT FROM (X) TO DNAMBF
; COPY
; THE COPY PROCESS IS PERFORMED BY COPYING DATA FROM THE SOURCE FILE
; TO RAM AND THEN COPYING IT TO THE DESTINATION FILE. THE SOURCE AND
; DESTINATION FILES ARE OPENED AS RANDOM FILES AND BOTH USE THE SYSTEM
; FCB ABOVE THE RESERVED FCBS. ALL OF AVAILABLE FREE RAM ABOVE THE
; VARIABLES IS USED AS A COPY BUFFER WHICH SPEEDS UP THE COPYING PROCESS
; BUT UNFORTUNATELY THE METHOD USED WILL ALLOW AN ERROR ENCOUNTERED DURING
; THE COPY PROCESS TO 'HANG' THE SYSTEM. THIS IS CAUSED BY POINTING THE FCB'S
; RANDOM FILE BUFFER POINTER (FCBBUF,X) TO THE FREE RAM BUFFER. AN ERROR
; WILL THEN CAUSE THE OPEN FILE TO BE CLOSED WITH FCBBUF,X POINTING TO AN
; AREA IN RAM WHERE THE RANDOM FILE BUFFER CLOSE ROUTINE (LCAE2) WILL NEVER
; LOOK FOR IT
COPY            JSR         >LC935          ; GET SOURCE FILENAME.EXT & DRIVE NUMBER FROM BASIC
                BSR         LD399           ; AND SAVE THEM ON THE STACK
                CLR         ,-S             ; CLEAR A BYTE ON STACK - SINGLE DISK COPY (SDC) FLAG
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         LD3CE           ; BRANCH IF END OF LINE - SINGLE DISK COPY
                COM         ,S              ; SET SOC FLAG TO $FF (NO SINGLE DISK COPY)
                LDB         #$A5            ; TOKEN FOR 'TO'
                JSR         >LB26F          ; SYNTAX CHECK FOR 'TO'
                JSR         >LC935          ; GET DESTINATION FILENAME.EXT AND DRIVE NUMBER
LD3CE           BSR         LD399           ; SAVE DESTINATION FILENAME.EXT & DRIVE NUMBER ON STACK
                JSR         >LA5C7          ; SYNTAX ERROR IF MORE CHARACTERS ON LINE
                JSR         DVEC7           ; CLOSE ALL FILES
; COUNT THE NUMBER OF SECTORS WORTH OF FREE RAM AVAILABLE
                CLR         ,-S             ; CLEAR A SECTOR COUNTER ON THE STACK
                LEAX        -SECLEN,S       ; POINT X ONE SECTOR LENGTH DOWN FROM THE TOP OF STACK
LD3DC           INC         ,S              ; INCREMENT SECTOR COUNTER
                LEAX        -SECLEN,X       ; DECREMENT X BY ONE SECTOR
                CMPX        ARYEND          ; COMPARE TO TOP OF ARRAYS
                BHS         LD3DC           ; BRANCH IF NOT AT BOTTOM OF FREE RAM
                DEC         ,S              ; DECREMENT SECTOR COUNTER
                LBEQ        LAC44           ; 'OM' ERROR IF NOT AT LEAST ONE FULL SECTOR OF FREE RAM
                LEAX        14,S            ; POINT X TO START OF SOURCE DATA
                BSR         LD3AD           ; PUT SOURCE DATA INTO DNAMBF AND DSKCON
                JSR         >LC68C          ; SCAN DIRECTORY FOR A MATCH
                JSR         >LC6E5          ; 'NE' ERROR IF MATCH NOT FOUND
                LDX         V974            ; POINT X TO DIRECTORY RAM IMAGE OF FOUND FILE
                LDU         DIRLST,X        ; GET NUMBER OF BYTES IN LAST SECTOR AND
                LDX         DIRTYP,X        ; SOURCE FILE TYPE AND ASCII FLAG
                PSHS        U,X             ; AND SAVE THEM ON THE STACK
                JSR         >LC79D          ; GET VALID FAT DATA
                LDB         V976            ; GET NUMBER OF FIRST GRANULE IN FILE
                JSR         >LCD1E          ; GET THE NUMBER OF GRANULES IN FILE
                PSHS        A               ; AND SAVE IT ON THE STACK
                DECA                        ;  SUBTRACT OFF THE LAST GRANULE
                ANDB        #$3F            ; MASK OFF LAST GRANULE FLAG BITS AND SAVE THE
                PSHS        B               ; NUMBER OF SECTORS IN LAST GRANULE ON STACK
                TFR         A,B             ; SAVE THE NUMBER OF GRANULES IN ACCB
                CLRA                        ;  CLEAR THE MS BYTE OF ACCD
                JSR         >LC779          ; MULTIPLY ACCD BY NINE
                ADDB        ,S              ; ADD THE NUMBER OF SECTORS IN THE LAST
                ADCA        #$00            ; GRANULE TO ACCD
                LDX         #$0001          ; INITIALIZE RECORD COUNTER TO ONE
                PSHS        X,B,A           ; INITIALIZE SECTOR AND RECORD COUNTERS ON THE STACK
; AT THIS POINT THE CONTROL VARIABLES FOR COPY ARE STORED ON THE STACK.
; 0 1,S = REMAINING SECTORS COUNTER; 2 3,S = RECORD COUNTER
; 4,S = NUMBER OF SECTORS TO BE COPIED. INITIALLY SET TO NUMBER OF
; SECTORS IN THE LAST GRANULE.
; 5,S = GRAN TEST FLAG. INITIALLY SET TO NUMBER OF GRANS IN FILE
; 6,S = FILE TYPE; 7,S = ASCII FLAG; 8 9,S = NUMBER OF BYTES IN LAST SECTOR
; 10,S = NUMBER OF SECTORS WHICH WILL FIT IN THE CURRENTLY AVAILABLE FREE RAM
; 11-22,S = DESTINATION FILENAME.EXT AND DRIVE NUMBER
; 23,S = SINGLE DISK COPY FLAG; 24-35,S = SOURCE FILENAME.EXT AND DRIVE NUMBER
LD41E           CLRB                        ;  SET SECTOR COUNTER TO ZERO
                LDX         ,S              ; GET THE NUMBER OF SECTORS REMAINING IN THE FILE
                BEQ         LD42C           ; BRANCH IF NO SECTORS LEFT
LD423           INCB                        ;  ADD A SECTOR TO TEMPORARY SECTOR COUNTER
                LEAX        -1,X            ; DECREMENT REMAINING SECTORS COUNTER
                BEQ         LD42C           ; BRANCH IF NO SECTORS LEFT
                CMPB        10,S            ; COMPARE TEMPORARY COUNTER TO NUMBER OF SECTORS WHICH MAY
; BE STORED IN FREE RAM
                BNE         LD423           ; BRANCH IF STILL ROOM FOR MORE SECTORS
LD42C           STX         ,S              ; SAVE THE NUMBER OF UNCOPIED SECTORS REMAINING IN THE FILE
                STB         $04,S           ; SAVE THE NUMBER OF SECTORS TO BE COPIED THIS TIME THROUGH LOOP
                BSR         LD482           ; 'GET' ACCB SECTORS TO RAM BUFFER
                LDA         #$FF            ; SET SOURCE/DESTINATION FLAG TO DESTINATION
                BSR         LD476           ; PRINT PROMPT MESSAGE IF REQUIRED
                TST         $05,S           ; CHECK THE GRAN TEST FLAG. IF <> 0, IT CONTAINS THE
                BEQ         LD45F           ; NUMBER OF GRANS IN THE FILE AND THE DESTINATION DISK
; MUST BE CHECKED FOR ENOUGH ROOM. IF IT IS =0
; THEN THE CHECK HAS ALREADY BEEN DONE
                LEAX        11,S            ; POINT TO DESTINATION FILE PARAMETERS
                JSR         >LD3AD          ; GET DESTINATION FILE PARAMETERS FROM STACK
                JSR         >LD059          ; SCAN DIRECTORY FOR FILE - 'AE' ERROR IF IT EXISTS
                JSR         >LC79D          ; GET VALID FAT DATA
; MAKE SURE THERE ARE ENOUGH FREE GRANULES ON THE DESTINATION DISK
                JSR         >LC755          ; POINT X TO FAT
                LEAX        FATCON,X        ; SKIP PAST THE FAT CONTROL BYTES
                LDA         $05,S           ; GET THE NUMBER OF GRANS IN THE FILE
                LDB         #GRANMX         ; SET GRAN COUNTER TO MAXIMUM
LD44E           COM         ,X              ; CHECK TO SEE IF A BRAN IS FREE
                BNE         LD455           ; AND BRANCH IF IT IS NOT FREE
                DECA                        ;  = DECREMENT COUNTER AND BRANCH IF
                BEQ         LD45D           ; THERE ARE ENOUGH FREE GRANULES
LD455           COM         ,X+             ; RESTORE FAT BYTE AND INCREMENT POINTER
                DECB                        ;  DECREMENT GRAN COUNTER
                BNE         LD44E           ; BRANCH IF ALL GRANS NOT CHECKED
                JMP         >LC7F8          ; 'DISK FULL' ERROR
LD45D           COM         ,X              ; RESTORE FAT BYTE
LD45F           BSR         LD47C           ; 'PUT' DATA FROM RAM BUFFER TO DESTINATION FILE
                LDX         ,S              ; GET THE NUMBER OF REMAINING SECTORS
                BEQ         LD472           ; EXIT ROUTINE IF NO SECTORS LEFT
                LDD         $02,S           ;
                ADDB        $04,S           ; GET THE CURRENT RECORD COUNTER, ADD
                ADCA        #$00            ; THE NUMBER OF SECTORS (RECORDS) MOVED
                STD         $02,S           ; AND SAVE THE NEW RECORD COUNTER
                CLRA                        ;  SET SOURCE/DESTINATION FLAG TO SOURCE
                BSR         LD476           ; PRINT PROMPT MESSAGE IF REQUIRED
                BRA         LD41E           ; KEEP COPYING SECTORS
LD472           LEAS        36,S            ; REMOVE TEMPORARY STORAGE VARIABLES FROM STACK
                RTS         ****            ; COPY DONE ****
LD476           TST         25,S            ; CHECK SINGLE DISK COPY FLAG - IF <> ZERO, THEN DON'T
; PRINT THE PROMPT MESSAGE
                JMP         >LD326          ; PRINT THE PROMPT MESSAGE IF REQUIRED
; 'PUT'.'GET' DATA FROM THE DESTINATION/SOURCE FILES
LD47C           LDA         #$FF            ; 'PUT' FLAG
                LEAX        13,S            ; POINT X TO DESTINATION FILENAME DATA
                BRA         LD486           ; GO 'PUT' SOME DATA
LD482           CLRA                        ;  ZERO IS THE 'GET' FLAG
                LEAX        26,S            ; POINT X TO THE SOURCE FILENAME DATA
LD486           STA         VD8             ; SAVE THE 'GET'/'PUT' FLAG
                JSR         >LD3AD          ; GET FILENAME AND DRIVE DATA FROM THE STACK
                LDX         $08,S           ; GET ASCII FLAG AND FILE TYPE AND SAVE
                STX         DFLTYP          ; THEM IN THE DISK RAM VARIABLES
                LDX         #SECLEN         ; SAVE ONE SECTOR LENGTH IN
                STX         DFFLEN          ; RAM RECORD LENGTH VARIABLE
                LDA         #'R             ; RANDOM FILE TYPE FLAG
                LDB         FCBACT          ; GET THE HIGHEST RESERVED FCB NUMBER, ADD ONE
                INCB                        ;  AND OPEN A RANDOM FILE WHOSE FCB WILL BE ONE ABOVE
                JSR         >LC48D          ; THE HIGHEST RESERVED FCB (THE SYSTEM FCB)
                LDX         FCBTMP          ; POINT X TO THE 'SYSTEM' FCB
                LDD         #SECLEN         ; SET THE NUMBER OF BYTES IN THE LAST SECTOR
                STD         FCBLST,X        ; OF THE FILE EQUAL TO ONE SECTOR LENGTH
                LDB         $06,S           ; GET THE NUMBER OF SECTORS TO MOVE AND
                BEQ         LD4D4           ; BRANCH IF NONE LEFT
                LDB         VD8             ; GRAB THE 'GET'/'PUT' FLAG, 'AND' IT WITH THE
                ANDB        $07,S           ; GRAN TEST FLAG - BRANCH IF 'GET'ING DATA OR THIS IS
                BEQ         LD4BA           ; NOT THE FIRST TIME THROUGH THE LOOP
                LDD         $02,S           ; GET THE NUMBER OF SECTORS REMAINING TO BE COPIED AND
                ADDB        $06,S           ; ADD THE NUMBER TO BE COPIED THIS TIME THROUGH LOOP
                ADCA        #$00            ;
                JSR         >LC2E6          ; 'PUT' THE LAST RECORD IN THE FILE TO THE SYSTEM FCB.
; THE RECORD NUMBER IS IN ACCD.
LD4BA           LDX         FCBTMP          ; POINT X TO THE SYSTEM FCB
                LDU         $04,S           ; GET THE CURRENT RECORD NUMBER
                STU         FCBREC,X        ; AND SAVE IT IN THE FCB
                LDB         $06,S           ; GET THE NUMBER OF THE RECORD (SECTOR) TO MOVE
                LDU         ARYEND          ; END OF ARRAYS IS THE START OF THE COPY FREE RAM BUFFER
LD4C4           PSHS        U,B             ; SAVE SECTOR COUNTER AND BUFFER POINTER ON THE STACK
                LDX         FCBTMP          ; POINT X TO SYSTEM FCB
                STU         FCBBUF,X        ; SET THE RANDOM FILE BUFFER POINTER TO THE 'COPY' RAM BUFFER
; THIS WILL CAUSE THE SYSTEM TO 'HANG' IF AN ERROR OCCURS DURING COPY.
                JSR         >LC2EA          ; GO 'GET' OR 'PUT' DATA TO THE SYSTEM FCB
                INC         $01,S           ; ADD 256 (ONE SECTOR) TO THE BUFFER POINTER
                PULS        B,U             ; GET THE SECTOR COUNTER AND BUFFER POINER
                DECB                        ;  DECREMENT SECTOR COUNTER
                BNE         LD4C4           ; BRANCH IF ALL SECTORS NOT DONE
LD4D4           LDX         FCBTMP          ; POINT X TO SYSTEM FCB
                LDU         #DFLBUF         ; RESET THE RANDOM FILE BUFFER POINTER FOR THE SYSTEM
                STU         FCBBUF,X        ; FCB TO THE BOTTOM OF RANDOM FILE BUFFER AREA
                LDB         VD8             ; GRAB THE 'GET'/'PUT' FLAG, 'AND' IT WITH THE GRAN
                ANDB        $07,S           ; TEST FLAG - CLOSE THE FILE IF 'GET'ING DATA AND
                BEQ         LD4EA           ; THIS IS NOT THE FIRST TIME THROUGH THE LOOP
                CLR         $07,S           ; RESET THE GRAN TEST FLAG IF FIRST TIME THROUGH LOOP
                LDD         10,S            ; GET THE NUMBER OF BYTES IN THE LAST SECTOR,
                ORA         #$80            ; 'OR' IN THE PRE-SAVED FLAG AND
                STD         FCBLST,X        ; SAVE THE NUMBER OF BYTES IN THE LAST SECTOR IN THE FCB
LD4EA           JMP         >LCB06          ; CLOSE THE FILE
; DSKI$ COMMAND
DSKI            BSR         LD527           ; GET THE DRIVE, TRACK AND SECTOR NUMBERS
                BSR         LD51C           ; EVALUATE STRING VARIABLE 1 AND SAVE
                PSHS        X               ; THE DESCRIPTOR ADDRESS ON THE STACK
                BSR         LD51C           ; EVALUATE STRING VARIABLE 2 AND SAVE
                PSHS        X               ; THE DESCRiPTOR ADDRESS ON THE STACK
                LDB         #$02            ; DSKCON READ OP CODE
                JSR         >LD58F          ; REAO A SECTOR INTO DBUF0
                LDU         #DBUF0+128      ; POINT U TO TOP HALF OF DBUF0
                PULS        X               ; GET STRING 2 DESCRIPTOR ADDRESS
                BSR         LD508           ; PUT STRING 2 INTO STRING SPACE
                LDU         #DBUF0          ; POINT U TO BOTTOM HALF OF DBUF0
                PULS        X               ; GET STRING 1 DESCRIPTOR ADDRESS
LD508           PSHS        U,X             ; PUT STRING DESCRIPTOR & SOURCE POINTER ON THE STACK
                LDB         #128            ;
                JSR         >LB50F          ; RESERVE 128 BYTES IN STRING SPACE
                LEAU        ,X              ; POINT U TO RESERVED STRING SPACE
                PULS        X               ; GET STRING DESCRIPTOR ADDRESS
                STB         ,X              ; SAVE DESCRIPTOR DATA (LENGTH AND ADDRESS)
                STU         $02,X           ; OF THE NEW STRING
                PULS        X               ; GET THE SOURCE (DBUF0) POINTER
LD519           JMP         >LA59A          ; MOVE SECTOR DATA FROM DBUF0 TO STRING SPACE
LD51C           JSR         SYNCOMMA        ; SYNTAX CHECK FOR A COMMA
                LDX         #LB357          ; POINT X TO EVALUATE VARIABLE ROUTINE
                BSR         LD553           ; EVALUATE A VARIABLE
LD524           JMP         >LB146          ; 'TM' ERROR IF NUMERIC VARIABLE
; EVALUATE DRIVE, TRACK AND SECTOR NUMBERS
LD527           JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                CMPB        #$03            ; COMPARE TO 3 (HIGHEST DRIVE NUMBER) -
                BHI         LD54A           ; 'FC' ERROR IF ITS > 3
                PSHS        B               ; SAVE DRIVE NUMBER ON THE STACK
                JSR         >LB738          ; SYNTAX CHECK FOR COMMA. EVALUATE EXPRESSION (TRACK NUMBER)
                CMPB        #TRKMAX-1       ; CHECK FOR MAXIMUM TRACK NUMBER
                BHI         LD54A           ; 'FC' ERROR IF TRACK NUMBER > 34
                PSHS        B               ; SAVE TRACK NUMBER ON THE STACK
                JSR         >LB738          ; SYNTAX CHECK FOR COMMA, EVALUATE EXPRESSION (SECTOR NUMBER)
                STB         DSEC            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                DECB                        ;  USELESS INSTRUCTION. NEXT INSTRUCTION SHOULD JUST
                CMPB        #SECMAX-1       ; CHECK FOR MAXIMUM SECTOR NUMBER (SECMAX)
                BHI         LD54A           ; 'FC' ERROR IF SECTOR NUMBER TOO BIG
                PULS        A,B             ; GET TRACK AND DRIVE NUMBER OFF OF
                STA         DCTRK           ; THE STACK AND SAVE IN DSKCON
                STB         DCDRV           ; VARIABLES
                RTS
LD54A           JMP         >LB44A          ; JUMP TO 'FC' ERROR
LD54D           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDX         #LB156          ; POINT X TO 'EVALUATE EXPRESSION' ROUTINE ADDRESS
LD553           LDB         DCDRV           ; GET THE DSKCON DRIVE, TRACK AND
                LDU         DCTRK           ; SECTOR VALUES AND SAVE THEM ON THE STACK
                PSHS        U,B             ;
                JSR         ,X              ; GO EVALUATE AN EXPRESSION OR A VARIABLE
                PULS        B,U             ; GET THE DRIVE, TRACK AND SECTOR
                STB         DCDRV           ; NUMBERS OFF OF THE STACK AND PUT
                STU         DCTRK           ; THEM BACK INTO THE DSKCON VARIABLES
                RTS
; DSKO$ COMMAND
DSKO            BSR         LD527           ; GET THE DRIVE, TRACK AND SECTOR NUMBERS
                BSR         LD54D           ; GET THE DESCRIPTOR OF STRING 1
                BSR         LD524           ; 'TM' ERROR IF NUMERIC EXPRESSION
                LDX         FPA0+2          ; GET STRING 1 DESCRIPTOR ADDRESS
                PSHS        X               ; AND SAVE IT ON THE STACK
                BSR         LD54D           ; GET THE DESCRIPTOR OF STRING 2
                JSR         >LB654          ; GET LENGTH AND ADDRESS OF STRING 2 AND
                PSHS        X,B             ; SAVE THEM ON THE STACK
                CLRB                        ;  SET CLEAR COUNTER TO 256 (FULL SECTOR BUFFER)
                LDX         #DBUF0          ; USE DBUF0 AS THE DSKO$ I/O BUFFER
LD577           CLR         ,X+             ; CLEAR A BYTE IN I/O BUFFER
                DECB                        ;  DECREMENT CLEAR COUNTER
                BNE         LD577           ; BRANCH IF ALL 256 BYTES NOT CLEARED
                PULS        B,X             ; GET THE LENGTH AND ADDRESS OF STRING 2
                LDU         #DBUF0+128      ; POINT X TO STRING 2 DESTINATION
                BSR         LD519           ; MOVE STRING 2 DATA INTO DBUF0
                PULS        X               ; POINT X TO STRING 1 DESCRIPTOR
                JSR         >LB659          ; GET THE LENGTH AND ADDRESS OF STRING 1
                LDU         #DBUF0          ; POINT U TO STRING 1 DESTINATION
                BSR         LD519           ; MOVE STRING 1 DATA INTO DBUF0
                LDB         #$03            ; DSKCON WRITE OP CODE
LD58F           LDX         #DBUF0          ; POINT X TO I/O BUFFER (DBUF0)
                STX         DCBPT           ;
                STB         DCOPC           ; SAVE NEW DSKCON BUFFER POINTER AND OP CODE VARIABLES
                JMP         >LD6F2          ; GO WRITE OUT A SECTOR
; DSKINI COMMAND
DSKINI          LBEQ        LA61F           ; BRANCH TO 'DN' ERROR IF NO DRIVE NUMBER SPECIFIED
                JSR         >LD256          ; CALCULATE DRIVE NUMBER
                LDB         #$04            ; SKIP FACTOR DEFAULT VALUE
                JSR         GETCCH          ; GET CURRENT INPUT CHAR FROM BASiC
                BEQ         LD5B2           ; BRANCH IF END OF LINE
                JSR         >LB738          ; SYNTAX CHECK FOR COMMA AND EVALUATE EXPRESSION
                CMPB        #17             ; MAX VALUE OF SKIP FACTOR = 16
                LBHS        LB44A           ; 'ILLEGAL FUNCTION CALL' IF BAD SKIP FACTOR
                JSR         >LA5C7          ; SYNTAX ERROR IF MORE CHARACTERS ON THE LINE
LD5B2           PSHS        B               ; SAVE SKIP FACTOR ON THE STACK
                LDX         #DBUF1+SECMAX   ; POINT TO END OF LOGICAL SECTOR NUMBER STORAGE AREA
                LDB         #SECMAX         ; 18 SECTORS PER TRACK
LD5B9           CLR         ,-X             ; CLEAR A BYTE IN THE BUFFER
                DECB                        ;  CLEARED ALL 18?
                BNE         LD5B9           ; KEEP GOING IF NOT
                CLRA                        ;  RESET PHYSICAL SECTOR COUNTER
                BRA         LD5CE           ; START WITH FIRST PHYSICAL SECTOR = 1
; CALCULATE LOGICAL SECTOR NUMBERS
LD5C1           ADDB        ,S              ; ADD SKIP FACTOR TO LOGICAL SECTOR COUNTER
LD5C3           INCB                        ;  ADD ONE TO LOGICAL SECTOR COUNTER
LD5C4           SUBB        #SECMAX         ; SUBTRACT MAX NUMBER OF SECTORS
                BHS         LD5C4           ; BRANCH UNTIL 0 > ACCB >= -18
                ADDB        #SECMAX         ; ADD 18, NOW ACCB IS 0-17
                TST         B,X             ; IS ANYTHING STORED HERE ALREADY?
                BNE         LD5C3           ; YES - GET ANOTHER SECTOR
LD5CE           INCA                        ;  INCREMENT PHYSICAL SECTOR NUMBER AND
                STA         B,X             ; SAVE IT IN THE RAM BUFFER
                CMPA        #SECMAX         ; FINISHED WITH ALL SECTORS?
                BLO         LD5C1           ; NO - KEEP GOING
                LEAS        $01,S           ; REMOVE SKIP FACTOR FROM STACK
                LDX         #DFLBUF+$1888-2 ; GET TOP OF RAM USED BY DSKINI
                CMPX        MEMSIZ          ; IS IT > CLEARED AREA?
                LBHI        LAC44           ; 'OUT OF MEMORY' ERROR IF > CLEARED AREA
                JSR         DVEC7           ; CLOSE ALL FILES
                COM         DRESFL          ; SET RESET FLAG TO $FF - THIS WILL CAUSE A DOS RESET
                LDS         #DBUF1+SECLEN   ; SET STACK TO TOP OF DBUF1
                JSR         >L95AC          ; RESET SAM TO DISPLAY PAGE ZERO AND ALPHA GRAPHICS
                LDA         #$00            ; YOU COULD DELETE THIS INSTRUCTION AND CHANGE FOLLOWING STA TO CLR
                STA         DCOPC           ; RESTORE HEAD TO TRACK ZERO DSKCON OP CODE
                CLR         DCTRK           ; SET DSKCON TRACK VARIABLE TO TRACK ZERO
                JSR         >LD6F2          ; RESTORE HEAD TO TRACK ZERO
                CLR         RDYTMR          ; RESET THE READY TIMER
                LDA         #$C0            ; FOC READ ADDRESS CODE
                STA         FDCREG          ;
                JSR         >LD7D1          ; CHECK DRIVE READY - WAIT UNTIL READY
                BEQ         LD620           ; BRANCH IF DRIVES READY
                JMP         >LD688          ; ERROR IF DRIVES NOT READY
LD606           CMPA        #22             ; CHECK FOR TRACK 22 (PRECOMPENSATION)
                BLO         LD612           ; AND BRANCH IF < TRACK 22 - NO PRECOMP
                LDA         DRGRAM          ; GET THE RAM IMAGE OF DSKREG, 'OR'
                ORA         #$10            ; IN THE PRECOMPENSATION FLAG AND
                STA         DSKREG          ; SEND IT TO DSKREG
LD612           LDA         #$53            ; GET STEP IN COMMAND
                STA         FDCREG          ; AND SEND IT TO THE 1793
                EXG         A,A             ; DELAY AFTER ISSUING COMMAND TO 1793
                EXG         A,A             ;
                JSR         >LD7D1          ; CHECK DRIVE READY
                BNE         LD688           ; BRANCH IF NOT READY - ISSUE AN ERROR
LD620           JSR         >LD7F0          ; WAIT A WHILE
                BSR         LD691           ; BUILD A FORMATTED TRACK IN RAM
                LDY         #FDCREG+3       ; Y POINTS TO 1793 DATA REGISTER
                ORCC        #$50            ; DISABLE INTERRUPTS
                LDX         #LD64F          ; GET RETURN ADDRESS AND STORE
                STX         DNMIVC          ; IT IN THE NON MASKABLE INTERRUPT VECTOR
                LDX         #DFLBUF         ; POINT X TO THE FORMATTED TRACK RAM IMAGE
                LDA         FDCREG          ; RESET STATUS OF THE 1793
                LDA         #$FF            ; ENABLE THE NMI FLAG TO VECTOR
                STA         NMIFLG          ; OUT OF AN I/O LOOP UPON AN NMI INTERRUPT
                LDB         #$F4            ; GET WRITE TRACK COMMAND AND
                STB         FDCREG          ; SEND TO 1793
                LDA         DRGRAM          ; GET THE DSKREG RAM IMAGE AND 'OR' IN THE
                ORA         #$80            ; FLAG WHICH WILL ENABLE THE 1793 TO HALT
                STA         DSKREG          ; THE 6809. SEND RESULT TO DSKREG
LD649           LDB         ,X+             ; GET A BYTE FROM THE FORMATTED TRACK
                STB         ,Y              ; RAM IMAGE, SEND IT TO THE 1793 AND
                BRA         LD649           ; LOOP BACK TO GET ANOTHER BYTE
LD64F           LDA         FDCREG          ; GET STATUS
                ANDCC       #$AF            ; ENABLE INTERRUPTS
                ANDA        #$44            ; KEEP ONLY WRITE PROTECT & LOST DATA
                STA         DCSTA           ; AND SAVE IT IN THE DSKCON STATUS BYTE
                BNE         LD688           ; BRANCH IF ERROR
                INC         DCTRK           ; SKIP TO THE NEXT TRACK
                LDA         DCTRK           ; GET THE TRACK NUMBER
                CMPA        #TRKMAX         ; WAS IT THE LAST TRACK
                BNE         LD606           ; NO - KEEP GOING
; VERIFY THAT ALL SECTORS ARE READABLE
                LDA         #$02            ; GET THE DSKCON READ OP CODE
                STA         DCOPC           ; AND SAVE IT IN THE DSKCON VARIABLE
                LDX         #DBUF0          ; POINT THE DSKCON BUFFER POINTER
                STX         DCBPT           ; TO DBUF0
                LDU         #DBUF1          ; POINT U TO THE LOGICAL SECTOR NUMBERS
                CLRA                        ;  RESET THE TRACK COUNTER TO ZERO
LD66F           STA         DCTRK           ; SET THE DSKCON TRACK VARIABLE
                CLRB                        ;  RESET THE SECTOR COUNTER
LD672           LDA         B,U             ; GET THE PHYSICAL SECTOR NUMBER
                STA         DSEC            ; SAVE DSKCON SECTOR VARIABLE
                JSR         >LD6F2          ; READ A SECTOR
                INCB                        ;  INCREMENT THE SECTOR COUNTER
                CMPB        #SECMAX         ; AND COMPARE IT TO MAXIMUM SECTOR NUMBER
                BLO         LD672           ; AND KEEP LOOPING IF MORE SECTORS LEFT
                LDA         DCTRK           ; GET THE CURRENT TRACK NUMBER
                INCA                        ;  = ADD ONE TO IT, COMPARE TO THE MAXIMUM TRACK
                CMPA        #TRKMAX         ; NUMBER AND KEEP LOOPING IF
                BLO         LD66F           ; THERE ARE STILL TRACKS TO DO
                JMP         >LD2CD          ; GO CHECK FOR A DOS RESET
LD688           CLR         DRGRAM          ; CLEAR RAM IMAGE OF DSKREG
                CLR         DSKREG          ; CLEAR DSKREG - TURN DISK MOTORS OFF
                JMP         >LD701          ; PROCESS DRIVES NOT READY ERROR

; BUILD A FORMATTED TRACK OF DATA IN RAM STARTING AT DFLBUF.
LD691           LDX         #DFLBUF         ; START TRACK BUFFER AT DFLBUF
                LDD         #$204E          ; GET SET TO WRITE 32 BYTES OF $4E
                BSR         LD6C2           ; GO WRITE GAP IV
                CLRB                        ;  RESET SECTOR COUNTER
LD69A           PSHS        B               ; SAVE SECTOR COUNTER
                LDU         #DBUF1          ; POINT U TO THE TABLE OF LOGICAL SECTORS
                LDB         B,U             ; GET LOGICAL SECTOR NUMBER FROM TABLE AND
                STB         DSEC            ; SAVE IT IN THE DSKCON VARIABLE
                LDU         #LD6D4          ; POINT U TO TABLE OF SECTOR FORMATTING DATA
                LDB         #$03            ; GET FIRST 3 DATA BLOCKS AND
                BSR         LD6C8           ; WRITE THEM TO BUFFER
                LDA         DCTRK           ; GET TRACK NUMBER AND STORE lT
                STA         ,X+             ; IN THE RAM BUFFER
                CLR         ,X+             ; CLEAR A BYTE (SIDE NUMBER) IN BUFFER
                LDA         DSEC            ; GET SECTOR NUMBER AND
                STA         ,X+             ; STORE IT IN THE BUFFER
                LDB         #$09            ; GET THE LAST NINE DATA BLOCKS AND
                BSR         LD6C8           ; WRITE THEM TO THE BUFFER
                PULS        B               ; GET SECTOR COUNTER
                INCB                        ;  NEXT SECTOR
                CMPB        #SECMAX         ; 18 SECTORS PER TRACK
                BLO         LD69A           ; BRANCH IF ALL SECTORS NOT DONE
                LDD         #$C84E          ; WRITE 200 BYTES OF $4E AT END OF TRACK

; WRITE ACCA BYTES OF ACCB INTO BUFFER
LD6C2           STB         ,X+             ; STORE A BYTE IN THE BUFFER
                DECA                        ;  DECREMENT COUNTER
                BNE         LD6C2           ; BRANCH IF ALL BYTES NOT MOVED
                RTS
LD6C8           PSHS        B               ; SAVE THE COUNTER ON THE STACK
                LDD         ,U++            ; GET TWO BYTES OF DATA FROM THE TABLE
                BSR         LD6C2           ; WRITE ACCA BYTES OF ACCB INTO THE BUFFER
                PULS        B               ; GET THE COUNTER BACK, DECREMENT
                DECB                        ;  IT AND BRANCH IF ALL DATA BLOCKS
                BNE         LD6C8           ; NOT DONE
                RTS

; DATA USED TO FORMAT A SECTOR ON THE DISK

; THESE DATA ARE CLOSE TO THE IBM SYSTEM 34 FORMAT FOR 256 BYTE SECTORS.
; DOUBLE DENSITY. THE FORMAT GENERALLY CONFORMS TO THAT SPECIFIED ON THE
; 1793 DATA SHEET. THE GAP SIZES HAVE BEEN REDUCED TO THE MINIMUM
; ALLOWABLE. THE IBM FORMAT USES $40 AS THE FILL CHARACTER FOR THE DATA
; BLOCKS WHILE COLOR DOS USES AN $FF AS THE FILL CHARACTER.
LD6D4           FCB         8,0             ; SYNC FIELD
                FCB         3,$F5
                FCB         1,$FE           ; ID ADDRESS MARK (AM1)
; TRACK, SIDE, AND SECTOR NUMBERS ARE INSERTED HERE
                FCB         1,1             ; SECTOR SIZE (256 BYTE SECTORS)
                FCB         1,$F7           ; CRC REQUEST
                FCB         22,$4E          ; GAP II (POST-ID GAP)
                FCB         12,0            ; SYNC FIELD
                FCB         3,$F5
                FCB         1,$FB           ; DATA ADDRESS MARK (AM2)
                FCB         0,$FF           ; DATA FIELD (256 BYTES)
                FCB         1,$F7           ; CRC REQUEST
                FCB         24,$4E          ; GAP III (POST DATA GAP)
; DOS COMMAND
DOS             BNE         LD742           ; RETURN IF ARGUMENT GIVEN
                JMP         [DOSVEC]        ; JUMP TO THE DOS COMMAND
LD6F2           PSHS        B               ; SAVE ACCB
                LDB         #$05            ; 5 RETRIES
                STB         ATTCTR          ; SAVE RETRY COUNT
                PULS        B               ; RESTORE ACCB
LD6FB           BSR         DSKCON          ; GO EXECUTE COMMAND
                TST         DCSTA           ; CHECK STATUS
                BEQ         LD70E           ; BRANCH IF NO ERRORS
LD701           LDA         DCSTA           ; GET DSKCON ERROR STATUS
                LDB         #2*30           ; 'WRITE PROTECTED' ERROR
                BITA        #$40            ; CHECK BIT 6 OF STATUS
                BNE         LD70B           ; BRANCH IF WRITE PROTECT ERROR
LD709           LDB         #2*20           ; 'I/O ERROR'
LD70B           JMP         >LAC46          ; JUMP TO ERROR DRIVER
LD70E           PSHS        A               ; SAVE ACCA
                LDA         DCOPC           ; GET OPERATION CODE
                CMPA        #$03            ; CHECK FOR WRITE SECTOR COMMAND
                PULS        A               ; RESTORE ACCA
                BNE         LD742           ; RETURN IF NOT WRITE SECTOR
                TST         DVERFL          ; CHECK VERIFY FLAG
                BEQ         LD742           ; RETURN IF NO VERIFY
                PSHS        U,X,B,A         ; SAVE REGISTERS
                LDA         #$02            ; READ OPERATION CODE
                STA         DCOPC           ; STORE TO DSKCON PARAMETER
                LDU         DCBPT           ; POINT U TO WRITE BUFFER ADDRESS
                LDX         #DBUF1          ; ADDRESS OF VERIFY BUFFER
                STX         DCBPT           ; TO DSKCON VARIABLE
                BSR         DSKCON          ; GO READ SECTOR
                STU         DCBPT           ; RESTORE WRITE BUFFER
                LDA         #$03            ; WRITE OP CODE
                STA         DCOPC           ; SAVE IN DSKCON VARIABLE
                LDA         DCSTA           ; CHECK STATUS FOR THE READ OPERATION
                BNE         LD743           ; BRANCH IF ERROR
                CLRB                        ;  CHECK 256 BYTES
LD737           LDA         ,X+             ; GET BYTE FROM WRITE BUFFER
                CMPA        ,U+             ; COMPARE TO READ BUFFER
                BNE         LD743           ; BRANCH IF NOT EQUAL
                DECB                        ;  DECREMENT BYTE COUNTER AND
                BNE         LD737           ; BRANCH IF NOT DONE
                PULS        A,B,X,U         ; RESTORE REGISTERS
LD742           RTS
LD743           PULS        A,B,X,U         ; RESTORE REGISTERS
                DEC         ATTCTR          ; DECREMENT THE VERIFY COUNTER
                BNE         LD6FB           ; BRANCH IF MORE TRIES LEFT
                LDB         #2*36           ; 'VERIFY ERROR'
                BRA         LD70B           ; JUMP TO ERROR HANDLER
; VERIFY COMMAND
VERIFY          CLRB                        ;  OFF FLAG = 0
                CMPA        #$AA            ; OFF TOKEN ?
                BEQ         LD75A           ; YES
                COMB                        ;  ON FLAG = $FF
                CMPA        #$88            ; ON TOKEN
                LBNE        LB277           ; BRANCH TO 'SYNTAX ERROR' IF NOT ON OR OFF
LD75A           STB         DVERFL          ; SET VERIFY FLAG
                JMP         GETNCH          ; GET NEXT CHARACTER FROM BASIC
; DSKCON ROUTINE
DSKCON          PSHS        U,Y,X,B,A       ; SAVE REGISTERS
                LDA         #$05            ; GET RETRY COUNT AND
                PSHS        A               ; SAVE IT ON THE STACK
LD765           CLR         RDYTMR          ; RESET DRIVE NOT READY TIMER
                LDB         DCDRV           ; GET DRIVE NUMBER
                LDX         #LD89D          ; POINT X TO DRIVE ENABLE MASKS
                LDA         DRGRAM          ; GET DSKREG IMAGE
                ANDA        #$A8            ; KEEP MOTOR STATUS, DOUBLE DENSITY. HALT ENABLE
                ORA         B,X             ; 'OR' IN DRIVE SELECT DATA
                ORA         #$20            ; 'OR' IN DOUBLE DENSITY
                LDB         DCTRK           ; GET TRACK NUMBER
                CMPB        #22             ; PRECOMPENSATION STARTS AT TRACK 22
                BLO         LD77E           ; BRANCH IF LESS THAN 22
                ORA         #$10            ; TURN ON WRITE PRECOMPENSATION IF >= 22
LD77E           TFR         A,B             ; SAVE PARTIAL IMAGE IN ACCB
                ORA         #$08            ; 'OR' IN MOTOR ON CONTROL BIT
                STA         DRGRAM          ; SAVE IMAGE IN RAM
                STA         DSKREG          ; PROGRAM THE 1793 CONTROL REGISTER
                BITB        #$08            ; WERE MOTORS ALREADY ON?
                BNE         LD792           ; DON'T WAIT FOR IT TO COME UP TO SPEED IF ALREADY ON
                JSR         >LA7D1          ; WAIT A WHILE
                JSR         >LA7D1          ; WAIT SOME MORE FOR MOTOR TO COME UP TO SPEED
LD792           BSR         LD7D1           ; WAIT UNTIL NOT BUSY OR TIME OUT
                BNE         LD7A0           ; BRANCH IF TIMED OUT (DOOR OPEN. NO DISK, NO POWER. ETC.)
                CLR         DCSTA           ; CLEAR STATUS REGISTER
                LDX         #LD895          ; POINT TO COMMAND JUMP VECTORS
                LDB         DCOPC           ; GET COMMAND
                ASLB                        ;  2 BYTES PER COMMAND JUMP ADDRESS
                JSR         [B,X]           ; GO DO IT
LD7A0           PULS        A               ; GET RETRY COUNT
                LDB         DCSTA           ; GET STATUS
                BEQ         LD7B1           ; BRANCH IF NO ERRORS
                DECA                        ;  DECREMENT RETRIES COUNTER
                BEQ         LD7B1           ; BRANCH IF NO RETRIES LEFT
                PSHS        A               ; SAVE RETRY COUNT ON STACK
                BSR         LD7B8           ; RESTORE HEAD TO TRACK 0
                BNE         LD7A0           ; BRANCH IF SEEK ERROR
                BRA         LD765           ; GO TRY COMMAND AGAIN IF NO ERROR
LD7B1           LDA         #120            ; 120*1/60 = 2 SECONDS (1/60 SECOND FOR EACH IRQ INTERRUPT)
                STA         RDYTMR          ; WAIT 2 SECONDS BEFORE TURNING OFF MOTOR
                PULS        A,B,X,Y,U,PC    ; RESTORE REGISTERS - EXIT DSKCON
; RESTORE HEAD TO TRACK 0
LD7B8           LDX         #DR0TRK         ; POINT TO TRACK TABLE
                LDB         DCDRV           ; GET DRIVE NUMBER
                CLR         B,X             ; ZERO TRACK NUMBER
                LDA         #$03            ; RESTORE HEAD TO TRACK 0, UNLOAD THE HEAD
                STA         FDCREG          ; AT START, 30 MS STEPPING RATE
                EXG         A,A             ;
                EXG         A,A             ; WAIT FOR 1793 TO RESPOND TO COMMAND
                BSR         LD7D1           ; WAIT TILL DRIVE NOT BUSY
                BSR         LD7F0           ; WAIT SOME MORE
                ANDA        #$10            ; 1793 STATUS : KEEP ONLY SEEK ERROR
                STA         DCSTA           ; SAVE IN DSKCON STATUS
LD7D0           RTS
; WAIT FOR THE 1793 TO BECOME UNBUSY. IF IT DOES NOT BECOME UNBUSY,
; FORCE AN INTERRUPT AND ISSUE A DRIVE NOT READY 1793 ERROR.
LD7D1           LDX         ZERO            ; GET ZERO TO X REGISTER - LONG WAIT
LD7D3           LEAX        -1,X            ; DECREMENT LONG WAIT COUNTER
                BEQ         LD7DF           ; lF NOT READY BY NOW, FORCE INTERRUPT
                LDA         FDCREG          ; GET 1793 STATUS AND TEST
                BITA        #$01            ; BUSY STATUS BIT
                BNE         LD7D3           ; BRANCH IF BUSY
                RTS
LD7DF           LDA         #$D0            ; FORCE INTERRUPT COMMAND - TERMINATE ANY COMMAND
                STA         FDCREG          ; IN PROCESS. DO NOT GENERATE A 1793 INTERRUPT REQUEST
                EXG         A,A             ; WAIT BEFORE READING 1793
                EXG         A,A             ;
                LDA         FDCREG          ; RESET INTRQ (FDC INTERRUPT REQUEST)
                LDA         #$80            ; RETURN DRIVE NOT READY STATUS IF THE DRIVE DID NOT BECOME UNBUSY
                STA         DCSTA           ; SAVE DSKCON STATUS BYTE
                RTS
; MEDIUM DELAY
LD7F0           LDX         #8750           ; DELAY FOR A WHILE
LD7F3           LEAX        -1,X            ; DECREMENT DELAY COUNTER AND
                BNE         LD7F3           ; BRANCH IF NOT DONE
                RTS
; READ ONE SECTOR
LD7F8           LDA         #$80            ; $80 IS READ FLAG (1793 READ SECTOR)
LD7FA           FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
; WRITE ONE SECTOR
LD7FB           LDA         #$A0            ; $A0 IS WRITE FLAG (1793 WRITE SECTOR)
                PSHS        A               ; SAVE READ/WRITE FLAG ON STACK
                LDX         #DR0TRK         ; POINT X TO TRACK NUMBER TABLE IN RAM
                LDB         DCDRV           ; GET DRIVE NUMBER
                ABX                         ;  POINT X TO CORRECT DRIVE'S TRACK BYTE
                LDB         ,X              ; GET TRACK NUMBER OF CURRENT HEAD POSITION
                STB         FDCREG+1        ; SEND TO 1793 TRACK REGISTER
                CMPB        DCTRK           ; COMPARE TO DESIRED TRACK
                BEQ         LD82C           ; BRANCH IF ON CORRECT TRACK
                LDA         DCTRK           ; GET TRACK DESIRED
                STA         FDCREG+3        ; SEND TO 1793 DATA REGiSTER
                STA         ,X              ; SAVE IN RAM TRACK IMAGE
                LDA         #$17            ; SEEK COMMAND FOR 1793: DO NOT LOAD THE
                STA         FDCREG          ; HEAD AT START, VERIFY DESTINATION TRACK,
                EXG         A,A             ; 30 MS STEPPING RATE - WAIT FOR
                EXG         A,A             ; VALID STATUS FROM 1793
                BSR         LD7D1           ; WAIT TILL NOT BUSY
                BNE         LD82A           ; RETURN IF TIMED OUT
                BSR         LD7F0           ; WAIT SOME MORE
                ANDA        #$18            ; KEEP ONLY SEEK ERROR OR CRC ERROR IN ID FIELD
                BEQ         LD82C           ; BRANCH IF NO ERRORS - HEAD ON CORRECT TRACK
                STA         DCSTA           ; SAVE IN DSKCON STATUS
LD82A           PULS        A,PC
; HEAD POSITIONED ON CORRECT TRACK
LD82C           LDA         DSEC            ; GET SECTOR NUMBER DESIRED
                STA         FDCREG+2        ; SEND TO 1793 SECTOR REGISTER
                LDX         #LD88B          ; POINT X TO ROUTINE TO BE VECTORED
                STX         DNMIVC          ; TO BY NMI UPON COMPLETION OF DISK I/O AND SAVE VECTOR
                LDX         DCBPT           ; POINT X TO I/O BUFFER
                LDA         FDCREG          ; RESET INTRQ (FDC INTERRUPT REQUEST)
                LDA         DRGRAM          ; GET DSKREG IMAGE
                ORA         #$80            ; SET FLAG TO ENABLE 1793 TO HALT 6809
                PULS        B               ; GET READ/WRITE COMMAND FROM STACK
                LDY         ZERO            ; ZERO OUT Y - TIMEOUT INITIAL VALUE
                LDU         #FDCREG         ; U POINTS TO 1793 INTERFACE REGISTERS
                COM         NMIFLG          ; NMI FLAG = $FF: ENABLE NMI VECTOR
                ORCC        #$50            ; DISABLE FIRQ,IRQ
                STB         FDCREG          ; SEND READ/WRITE COMMAND TO 1793: SINGLE RECORD, COMPARE
                EXG         A,A             ; FOR SIDE 0, NO 15 MS DELAY, DISABLE SIDE SELECT
                EXG         A,A             ; COMPARE, WRITE DATA ADDRESS MARK (FB) - WAIT FOR STATUS
                CMPB        #$80            ; WAS THIS A READ?
                BEQ         LD875           ; IF SO, GO LOOK FOR DATA
; WAIT FOR THE 1793 TO ACKNOWLEDGE READY TO WRITE DATA
                LDB         #$02            ; DRQ MASK BIT
LD85B           BITB        ,U              ; IS 1793 READY FOR A BYTE? (DRQ SET IN STATUS BYTE)
                BNE         LD86B           ; BRANCH IF SO
                LEAY        -1,Y            ; DECREMENT WAIT TIMER
                BNE         LD85B           ; KEEP WAITING FOR THE 1793 DRQ
LD863           CLR         NMIFLG          ; RESET NMI FLAG
                ANDCC       #$AF            ; ENABLE FIRQ,IRQ
                JMP         >LD7DF          ; FORCE INTERRUPT, SET DRIVE NOT READY ERROR
; WRITE A SECTOR
LD86B           LDB         ,X+             ; GET A BYTE FROM RAM
                STB         FDCREG+3        ; SEND IT TO 1793 DATA REGISTER
                STA         DSKREG          ; REPROGRAM FDC CONTROL REGISTER
                BRA         LD86B           ; SEND MORE DATA
; WAIT FOR THE 17933 TO ACKNOWLEDGE READY TO READ DATA
LD875           LDB         #$02            ; DRQ MASK BIT
LD877           BITB        ,U              ; DOES THE 1793 HAVE A BYTE? (DRQ SET IN STATUS BYTE)
                BNE         LD881           ; YES, GO READ A SECTOR
                LEAY        -1,Y            ; DECREMENT WAIT TIMER
                BNE         LD877           ; KEEP WAITING FOR 1793 DRQ
                BRA         LD863           ; GENERATE DRIVE NOT READY ERROR
; READ A SECTOR
LD881           LDB         FDCREG+3        ; GET DATA BYTE FROM 1793 DATA REGISTER
                STB         ,X+             ; PUT IT IN RAM
                STA         DSKREG          ; REPROGRAM FDC CONTROL REGISTER
                BRA         LD881           ; KEEP GETTING DATA
; BRANCH HERE ON COMPLETION OF SECTOR READ/WRITE
LD88B           ANDCC       #$AF            ; ENABLE IRQ, FIRO
                LDA         FDCREG          ; GET STATUS & KEEP WRITE PROTECT, RECORD TYPE/WRITE
                ANDA        #$7C            ; FAULT, RECORD NOT FOUND, CRC ERROR OR LOST DATA
                STA         DCSTA           ; SAVE IN DSKCON STATUS
                RTS
; DSKCON OPERATION CODE JUMP VECTORS
LD895           FDB         LD7B8           ; RESTORE HEAD TO TRACK ZERO
                FDB         LD7D0           ; NO OP - RETURN
                FDB         LD7F8           ; READ SECTOR
                FDB         LD7FB           ; WRITE SECTOR
; DSKREG MASKS FOR DISK DRIVE SELECT
LD89D           FCB         1               ; DRIVE SEL 0
                FCB         2               ; DRIVE SEL 1
                FCB         4               ; DRIVE SEL 2
                FCB         $40             ; DRIVE SEL 3
; NMI SERVICE
DNMISV          LDA         NMIFLG          ; GET NMI FLAG
                BEQ         LD8AE           ; RETURN IF NOT ACTIVE
                LDX         DNMIVC          ; GET NEW RETURN VECTOR
                STX         10,S            ; STORE AT STACKED PC SLOT ON STACK
                CLR         NMIFLG          ; RESET NMI FLAG
LD8AE           RTI
; IRQ SERVICE
DIRQSV          LDA         PIA0+3          ; 63.5 MICRO SECOND OR 60 HZ INTERRUPT?
                BPL         LD8AE           ; RETURN IF 63.5 MICROSECOND
                LDA         PIA0+2          ; RESET 60 HZ PIA INTERRUPT FLAG
                LDA         RDYTMR          ; GET TIMER
                BEQ         LD8CD           ; BRANCH IF NOT ACTIVE
                DECA                        ;  DECREMENT THE TIMER
                STA         RDYTMR          ; SAVE IT
                BNE         LD8CD           ; BRANCH IF NOT TIME TO TURN OFF DISK MOTORS
                LDA         DRGRAM          ; GET DSKREG IMAGE
                ANDA        #$B0            ; TURN ALL MOTORS AND DRIVE SELECTS OFF
                STA         DRGRAM          ; PUT IT BACK IN RAM IMAGE
                STA         DSKREG          ; SEND TO CONTROL REGISTER (MOTORS OFF)
LD8CD           JMP         >L8955          ; JUMP TO EXTENDED BASIC'S IRQ HANDLER
; THIS IS THE END OF DISK BASIC (EXCEPT FOR THE DOS COMMAND AT $DF00).
; THE CODE FROM THIS POINT TO $DF00 IS GARBAGE.
; DOSBAS 1.1 = 1584 WASTED BYTES
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $13,$34,$06,$9D,$A5,$27,$03,$BD,$95,$81,$96,$B5,$97,$D8,$35,$06
                FCB         $DD,$B4,$4F,$34,$56,$BD,$95,$22,$BD,$92,$8F,$DF,$D9,$BD,$99,$DF
                FCB         $27,$0F,$BD,$99,$CB,$86,$01,$97,$D7,$BD,$99,$BA,$00,$D7,$BD,$99
                FCB         $BA,$10,$DF,$DC,$0D,$DB,$26,$03,$10,$DE,$DC,$35,$56,$0F,$DB,$10
                FCB         $DF,$DC,$30,$01,$9F,$BD,$DF,$D1,$97,$D7,$27,$9F,$2B,$06,$5C,$D1
                FCB         $D6,$23,$05,$5F,$5D,$27,$DD,$5A,$D7,$C0,$BD,$99,$DF,$27,$0F,$10
                FCB         $83,$00,$03,$25,$04,$30,$1E,$8D,$38,$BD,$99,$CB,$8D,$4C,$43,$53
                FCB         $D3,$D1,$DD,$D1,$2F,$16,$BD,$95,$06,$BD,$9A,$12,$26,$05,$CC,$FF
                FCB         $FF,$20,$ED,$BD,$95,$14,$8D,$3E,$8D,$5E,$20,$E0,$BD,$95,$06,$30
                FCB         $8B,$9F,$BD,$43,$53,$83,$00,$01,$2F,$04,$1F,$01,$8D,$03,$7E,$99
                FCB         $34,$DD,$CB,$35,$20,$DC,$BD,$34,$16,$96,$D7,$40,$D6,$C0,$34,$06
                FCB         $34,$20,$C6,$02,$BD,$AC,$33,$DC,$CB,$39,$DD,$CB,$35,$20,$DC,$C3
                FCB         $34,$16,$96,$D7,$20,$E6,$9E,$BD,$9F,$C3,$39,$DD,$CD,$10,$9E,$C3
                FCB         $8D,$F4,$10,$9F,$BD,$8D,$11,$9E,$CD,$30,$8B,$C3,$00,$01,$39,$BD
                FCB         $99,$C6,$10,$8E,$95,$14,$20,$06,$10,$8E,$95,$06,$AD,$A4,$DE,$8A
                FCB         $9E,$BD,$2B,$17,$9C,$D3,$22,$13,$34,$60,$8D,$16,$27,$0B,$BD,$93
                FCB         $77,$35,$60,$33,$41,$AD,$A4,$20,$E9,$35,$60,$1F,$30,$1F,$01,$93
                FCB         $8A,$39,$AD,$9F,$00,$D9,$1F,$89,$D4,$D8,$34,$06,$A4,$84,$A1,$61
                FCB         $35,$86,$9E,$8A,$C6,$01,$34,$14,$BD,$B1,$56,$5F,$BD,$A9,$A2,$BD
                FCB         $A9,$76,$BD,$B6,$54,$20,$02,$35,$14,$D7,$D8,$27,$FA,$9F,$D9,$10
                FCB         $27,$0F,$31,$0D,$D8,$27,$F0,$BD,$9B,$98,$81,$3B,$27,$F5,$81,$27
                FCB         $27,$F1,$81,$58,$10,$27,$01,$B2,$8D,$02,$20,$E7,$81,$4F,$26,$0D
                FCB         $D6,$DE,$5C,$8D,$5B,$5A,$C1,$04,$22,$63,$D7,$DE,$39,$81,$56,$26
                FCB         $1A,$D6,$DF,$54,$54,$C0,$1F,$8D,$47,$C1,$1F,$22,$50,$58,$58,$34
                FCB         $04,$CC,$7E,$7E,$AB,$E4,$E0,$E0,$DD,$DF,$39,$81,$4C,$26,$23,$D6
                FCB         $E1,$8D,$2D,$5D,$27,$37,$D7,$E1,$0F,$E5,$8D,$03,$24,$FC,$39,$0D
                FCB         $D8,$27,$0A,$BD,$9B,$98,$81,$2E,$27,$05,$BD,$9B,$E2,$43,$39,$0C
                FCB         $E5,$39,$81,$54,$26,$0D,$D6,$E2,$8D,$06,$5D,$27,$10,$D7,$E2,$39
                FCB         $7E,$9B,$AC,$81,$50,$26,$24,$BD,$9C,$CB,$5D,$26,$03,$7E,$B4,$4A
                FCB         $96,$E5,$9E,$DF,$34,$12,$86,$7E,$97,$DF,$97,$E0,$0F,$E5,$8D,$07
                FCB         $35,$12,$97,$E5,$9F,$DF,$39,$6F,$E2,$20,$40,$81,$4E,$26,$03,$BD
                FCB         $9B,$98,$81,$41,$25,$04,$81,$47,$23,$05,$BD,$9B,$BE,$20,$23,$80
                FCB         $41,$8E,$9C,$5B,$E6,$86,$0D,$D8,$27,$18,$BD,$9B,$98,$81,$23,$27
                FCB         $04,$81,$2B,$26,$03,$5C,$20,$0A,$81,$2D,$26,$03,$5A,$20,$03,$BD
                FCB         $9B,$E2,$5A,$C1,$0B,$22,$A6,$34,$04,$D6,$E1,$96,$E2,$3D,$DD,$D5
                FCB         $33,$61,$96,$DE,$81,$01,$22,$2C,$8E,$9C,$62,$C6,$18,$3D,$3A,$35
                FCB         $04,$58,$3A,$31,$84,$8D,$45,$DD,$E3,$8D,$0C,$96,$DF,$8D,$0B,$8D
                FCB         $06,$96,$E0,$8D,$05,$20,$F2,$86,$7E,$12,$B7,$FF,$20,$AE,$A4,$30
                FCB         $1F,$26,$FC,$39,$8E,$9C,$7A,$C6,$0C,$3D,$3A,$35,$04,$3A,$8D,$1C
                FCB         $DD,$E3,$8D,$0C,$96,$DF,$8D,$0B,$8D,$06,$96,$E0,$8D,$05,$20,$F2
                FCB         $86,$7E,$12,$B7,$FF,$20,$A6,$84,$4A,$26,$FD,$39,$C6,$FF,$96,$E5
                FCB         $27,$05,$8B,$02,$3D,$44,$56,$39,$34,$10,$0D,$D8,$27,$4D,$9E,$D9
                FCB         $A6,$80,$9F,$D9,$0A,$D8,$81,$20,$27,$F0,$35,$90,$8D,$EA,$81,$2B
                FCB         $27,$3C,$81,$2D,$27,$3C,$81,$3E,$27,$42,$81,$3C,$27,$39,$81,$3D
                FCB         $27,$3F,$BD,$90,$AA,$25,$24,$5F,$80,$30,$97,$D7,$86,$0A,$3D,$4D
                FCB         $26,$19,$DB,$D7,$25,$15,$0D,$D8,$27,$17,$BD,$9B,$98,$BD,$90,$AA
                FCB         $24,$E6,$0C,$D8,$9E,$D9,$30,$1F,$9F,$D9,$39,$7E,$B4,$4A,$5C,$27
                FCB         $FA,$39,$5D,$27,$F6,$5A,$39,$5D,$27,$F1,$54,$39,$5D,$2B,$EC,$58
                FCB         $39,$34,$60,$8D,$16,$BD,$B7,$0E,$35,$E0,$BD,$9C,$1B,$C6,$02,$BD
                FCB         $AC,$33,$D6,$D8,$9E,$D9,$34,$14,$7E,$9A,$32,$9E,$D9,$34,$10,$BD
                FCB         $9B,$98,$BD,$B3,$A2,$25,$C4,$BD,$9B,$98,$81,$3B,$26,$F9,$35,$10
                FCB         $DE,$A6,$34,$40,$9F,$A6,$BD,$B2,$84,$35,$10,$9F,$A6,$39,$4F,$1F
                FCB         $8B,$DC,$E3,$10,$27,$0D,$74,$93,$D5,$DD,$E3,$22,$0D,$0F,$E3,$0F
                FCB         $E4,$35,$02,$10,$EE,$67,$84,$7F,$34,$02,$3B,$0A,$0C,$01,$03,$05
                FCB         $06,$08,$01,$A8,$01,$90,$01,$7A,$01,$64,$01,$50,$01,$3D,$01,$2B
                FCB         $01,$1A,$01,$0A,$00,$FB,$00,$ED,$00,$DF,$00,$D3,$00,$C7,$00,$BB
                FCB         $00,$B1,$00,$A6,$00,$9D,$00,$94,$00,$8B,$00,$83,$00,$7C,$00,$75
                FCB         $00,$6E,$A6,$9C,$93,$8B,$83,$7B,$74,$6D,$67,$61,$5B,$56,$51,$4C
                FCB         $47,$43,$3F,$3B,$37,$34,$31,$2E,$2B,$28,$26,$23,$21,$1F,$1D,$1B
                FCB         $19,$18,$16,$14,$13,$12,$9E,$8A,$C6,$01,$34,$14,$D7,$C2,$9F,$D5
                FCB         $BD,$95,$9A,$BD,$B1,$56,$BD,$B6,$54,$20,$08,$BD,$9B,$98,$7E,$9B
                FCB         $BE,$35,$14,$D7,$D8,$27,$FA,$9F,$D9,$10,$27,$00,$EA,$0D,$D8,$27
                FCB         $F0,$BD,$9B,$98,$81,$3B,$27,$F5,$81,$27,$27,$F1,$81,$4E,$26,$04
                FCB         $03,$D5,$20,$E9,$81,$42,$26,$04,$03,$D6,$20,$E1,$81,$58,$10,$27
                FCB         $00,$96,$81,$4D,$10,$27,$01,$2A,$34,$02,$C6,$01,$0D,$D8,$27,$11
                FCB         $BD,$9B,$98,$BD,$B3,$A2,$34,$01,$BD,$9B,$E2,$35,$01,$24,$02,$8D
                FCB         $AA,$35,$02,$81,$43,$27,$28,$81,$41,$27,$2E,$81,$53,$27,$32,$81
                FCB         $55,$27,$5C,$81,$44,$27,$55,$81,$4C,$27,$4C,$81,$52,$27,$43,$80
                FCB         $45,$27,$2F,$4A,$27,$27,$4A,$27,$32,$4A,$27,$1D,$7E,$B4,$4A,$BD
                FCB         $95,$5D,$D7,$B2,$BD,$95,$9A,$20,$84,$C1,$04,$24,$EF,$D7,$E8,$20
                FCB         $F6,$C1,$3F,$24,$E7,$D7,$E9,$20,$EE,$4F,$8D,$58,$21,$4F,$1F,$01
                FCB         $20,$59,$4F,$1F,$01,$8D,$4D,$1E,$01,$20,$50,$4F,$1F,$01,$8D,$44
                FCB         $20,$49,$4F,$9E,$8A,$20,$44,$4F,$8D,$3A,$20,$F7,$4F,$20,$03,$4F
                FCB         $8D,$32,$9E,$8A,$1E,$10,$20,$33,$BD,$9C,$1B,$C6,$02,$BD,$AC,$33
                FCB         $D6,$D8,$9E,$D9,$34,$14,$7E,$9C,$C6,$D6,$E9,$27,$1B,$4F,$1E,$01
                FCB         $A7,$E2,$2A,$02,$8D,$0D,$BD,$9F,$B5,$1F,$30,$44,$56,$44,$56,$6D
                FCB         $E0,$2A,$04,$40,$50,$82,$00,$39,$1F,$10,$39,$34,$06,$8D,$DA,$35
                FCB         $10,$34,$06,$8D,$D4,$35,$10,$10,$9E,$E8,$34,$20,$6D,$E4,$27,$08
                FCB         $1E,$10,$8D,$DF,$6A,$E4,$20,$F4,$35,$20,$DE,$8A,$D3,$C7,$2B,$02
                FCB         $1F,$03,$1F,$10,$9E,$8A,$D3,$C9,$2B,$02,$1F,$01,$11,$83,$01,$00
                FCB         $25,$03,$CE,$00,$FF,$8C,$00,$C0,$25,$03,$8E,$00,$BF,$DC,$C7,$DD
                FCB         $BD,$DC,$C9,$DD,$BF,$9F,$C5,$DF,$C3,$0D,$D5,$26,$04,$9F,$C9,$DF
                FCB         $C7,$BD,$94,$20,$0D,$D6,$26,$03,$BD,$94,$A1,$0F,$D5,$0F,$D6,$7E
                FCB         $9C,$DD,$BD,$9B,$98,$34,$02,$BD,$9E,$5E,$34,$06,$BD,$9B,$98,$81
                FCB         $2C,$10,$26,$FF,$07,$BD,$9E,$5B,$1F,$01,$35,$40,$35,$02,$81,$2B
                FCB         $27,$04,$81,$2D,$26,$A6,$1F,$30,$7E,$9D,$CB,$BD,$9B,$98,$81,$2B
                FCB         $27,$07,$81,$2D,$27,$04,$BD,$9B,$E2,$4F,$34,$02,$BD,$9C,$CB,$35
                FCB         $02,$4D,$27,$04,$4F,$50,$82,$00,$39,$00,$00,$00,$01,$FE,$C5,$19
                FCB         $19,$FB,$16,$31,$F2,$F4,$FB,$4A,$51,$EC,$84,$61,$F9,$E1,$C7,$78
                FCB         $AE,$D4,$DC,$8E,$3B,$C5,$E5,$A2,$69,$B5,$06,$B5,$06,$81,$40,$26
                FCB         $02,$9D,$9F,$BD,$95,$22,$BD,$93,$B2,$BD,$93,$1D,$AE,$C4,$9F,$CB
                FCB         $AE,$42,$9F,$CD,$BD,$B2,$6D,$BD,$B7,$3D,$CE,$00,$CF,$AF,$C4,$BD
                FCB         $93,$20,$86,$01,$97,$C2,$BD,$95,$81,$8E,$01,$00,$9D,$A5,$27,$0F
                FCB         $BD,$B2,$6D,$BD,$B1,$41,$96,$4F,$8B,$08,$97,$4F,$BD,$B7,$40,$96
                FCB         $B6,$85,$02,$27,$04,$1F,$10,$30,$8B,$9F,$D1,$C6,$01,$D7,$C2,$D7
                FCB         $D8,$BD,$9F,$E2,$34,$06,$BD,$9F,$E2,$DD,$D9,$35,$06,$34,$06,$9E

; THIS IS THE CODE FOR THE DOS COMMAND
DOSCOM          SWI3                        ; DO A SOFTWARE INTERRUPT (#3)
                CLR         TMPLOC          ; RESET SECTOR COUNTER
                LDD         #DOSBUF         ; RAM LOAD ADDRESS FOR SECTOR DATA
                PSHS        B,A             ; SAVE RAM LOAD ADDRESS
LDF09           LDX         DSKVAR          ; POINT X TO DSKCON VARIABLES
                INC         TMPLOC          ; INCREMENT SECTOR COUNTER
                LDA         TMPLOC          ; GET THE SECTOR COUNTER
                CMPA        #SECMAX         ; LOADED IN 18 SECTORS? (ONE TRACK)
                BHI         LDF36           ; YES - EXIT
                STA         $03,X           ; NO - SAVE SECTOR NUMBER IN DSEC
                LDD         #$0200          ; GET FDC OP CODE (READ) AND DRIVE NUMBER (0)
                STA         ,X              ; SAVE THEM IN DSKCON VARIABLES (BUG - SHOULD BE STD ,X)
                LDA         #34             ; GET TRACK NUMBER (34)
                STA         $02,X           ; SAVE IT IN DSKCON VARIABLES TOO
                PULS        A,B             ; GET RAM LOAD ADDRESS
                STD         $04,X           ; AND SAVE IT IN THE DSKCON VARIABLES
                ADDA        #$01            ; ADD 256 (ONE SECTOR) TO RAM LOAD ADDRESS (SHOULD BE INCA)
                PSHS        B,A             ; SAVE NEW RAM LOAD ADDRESS
                JSR         [DCNVEC]        ; GO READ A SECTOR
                TST         $06,X           ; CHECK FOR ERRORS
                BEQ         LDF09           ; KEEP READING IF NONE
                PULS        A,B             ; PULL LOAD ADDRESS OFF OF THE STACK
                LDB         #2*20           ; 'IO' ERROR
                JMP         >LAC46          ; JUMP TO ERROR SERVICING ROUTINE
LDF36           PULS        A,B             ; PULL RAM LOAD ADDRESS OFF OF THE STACK
                LDD         DOSBUF          ; GET FIRST TWO BYTES OF RAM DATA
                CMPD        #$4F53          ; LOOK FOR 'OS' (OS9) AT START OF BUFFER
                LBEQ        DOSBUF+2        ; IF 'OS' THEN BRANCH TO DATA LOADED IN RAM
                CLR         DOSBUF          ; OTHERWISE CLEAR THE FIRST TWO
                CLR         DOSBUF+1        ; BYTES OF RAM DATA
                JMP         BAWMST          ; JUMP TO BASIC'S WARM START
DOSINI          LDD         #$3B3B          ; TWO RTI INSTRUCTIONS
                STD         SW3VEC          ;
                STD         SW3VEC+2        ; LOAD THE SWI2 AND SWI3 JUMP
                STD         SW2VEC+1        ; VECTORS WITH RTIS
END             RTS

; END OF THE DOS AND DOSINI COMMANDS - THE REST OF THE CODE
; TO THE END OF THE DISK ROM ($DFFF) IS GARBAGE.
; DOSBAS 1.1 = 167 WASTED BYTES
                FCB         $00,$00,$00,$00,$00,$00,$00,$05,$10,$93,$D3,$25,$02,$DC,$D3,$DD
                FCB         $C3,$A6,$E4,$81,$04,$25,$0A,$DC,$CD,$93,$C5,$24,$11,$4F,$5F,$20
                FCB         $0D,$DC,$CD,$D3,$C5,$25,$05,$10,$93,$D5,$25,$02,$DC,$D5,$DD,$C5
                FCB         $0D,$D8,$26,$02,$8D,$50,$35,$06,$04,$D8,$25,$05,$10,$93,$D9,$27
                FCB         $0C,$5C,$C1,$08,$26,$04,$4C,$5F,$84,$07,$7E,$9E,$FD,$39,$9E,$CF
                FCB         $EC,$C4,$27,$07,$83,$00,$01,$8D,$03,$1F,$21,$39,$34,$76,$6F,$64
                FCB         $A6,$63,$3D,$ED,$66,$EC,$61,$3D,$EB,$66,$89,$00,$ED,$65,$E6,$E4
                FCB         $A6,$63,$3D,$E3,$65,$ED,$65,$24,$02,$6C,$64,$A6,$E4,$E6,$62,$3D
                FCB         $E3,$64,$ED,$64,$35,$F6,$7E,$94,$A1,$5F,$9D,$A5,$27,$11,$BD,$B2
                FCB         $6D,$BD,$B1,$41,$96,$4F,$8B,$06,$97,$4F,$BD,$B7,$0E,$C4,$3F,$1F
                FCB         $98,$C4,$07,$44,$44,$44,$39

; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------

; Disk Color BASIC 1.0
; Copied from the PDF version of Disk Color BASIC Unravelled.
; Fixed up to assemble in Mamou

; Revision History

; $Id: $

DHITOK          EQU         $E0             ; HIGHEST 1.0 DISK TOKEN
CYEAR           EQU         '1
;
;
;
;
; FILE ALLOCATION TABLE FORMAT
;
;
; THE FILE ALLOCATION TABLE (FAT) CONTAINS THE STATUS OF THE GRANULES ON A DISKETTE.
; THE FAT CONTAINS 6 CONTROL BYTES FOLLOWED BY 68 DATA BYTES (ONE PER GRANULE). ONLY THE
; FIRST TWO OF THE SIX CONTROL BYTES ARE USED. A VALUE OF $FF IS SAVED IN UNALLOCATED
; GRANULES. IF BITS 6 & 7 OF THE DATA BYTE ARE SET, THE GRANULE IS THE LAST GRANULE
; IN A FILE AND BITS 0-5 ARE THE NUMBER OF USED SECTORS IN THAT GRANULE. IF BITS 6 & 7
; ARE NOT SET, THE DATA BYTE CONTAINS THE NUMBER OF THE NEXT GRANULE IN THE FILE.

; OFFSETS TO FAT CONTROL BYTES
FAT0            EQU         0               ; ACTIVE FILE COUNTER : DISK TO RAM FAT IMAGE DISABLE
FAT1            EQU         1               ; VALID DATA FLAG: 0=DISK DATA VALID, <> 0 = NEW FAT
; DATA - DISK DATA INVALID
; 2 TO 5 NOT USED
FATCON          EQU         6               ; OFFSET TO START OF FAT DATA (68 BYTES)
;
;
; DIRECTORY ENTRY FORMAT
;
;
; THE DIRECTORY IS USED TO KEEP TRACK OF HOW MANY FILES ARE STORED ON A DISKETTE
; AND WHERE THE FILE IS STORED ON THE DISK. THE FIRST GRANULE USED BY THE FILE WILL
; ALLOW THE FAT TO TRACK DOWN ALL OF THE GRANULES USED BY THE FILE. IF THE FIRST
; BYTE OF THE DIRECTORY ENTRY IS ZERO, THE FILE HAS BEEN KILLED;
; IF THE FIRST BYTE IS $FF THEN THE DIRECTORY ENTRY HAS NEVER BEEN USED.

; BYTE DESCRIPTION
DIRNAM          EQU         0               ; FILE NAME
DIREXT          EQU         8               ; FILE EXTENSION
DIRTYP          EQU         11              ; FILE TYPE
DIRASC          EQU         12              ; ASCII FLAG
DIRGRN          EQU         13              ; FIRST GRANULE IN FILE
DIRLST          EQU         14              ; NUMBER OF BYTES IN LAST SECTOR
; 16 TO 31 UNUSED
;
;
; FILE CONTROL BLOCK FORMAT
;
;
; THE FILE STRUCTURE OF COLOR TRS DOS IS CONTROLLED BY A FILE CONTROL BLOCK (FCB)
; THE FCB CONTAINS 25 CONTROL BYTES AND A SECTOR LONG (256 BYTES) DATA BUFFER.
; THE CONTROL BYTES CONTROL THE ORDERLY FLOW OF DATA FROM THE COMPUTER'S RAM TO
; THE DISKETTE AND VICE VERSA. THE OPEN COMMAND INITIALIZES THE FCB; THE INPUT,
; OUTPUT, WRITE, PRINT, GET AND PUT COMMANDS TRANSFER DATA THROUGH THE FCB AND
; THE CLOSE COMMAND TURNS OFF THE FCB.

; TABLES OF OFFSETS TO FCB CONTROL BYTES

; RANDOM FILE
; BYTE DESCRIPTION
FCBTYP          EQU         0               ; FILE TYPE: $40=RANDOM/DIRECT, 0=CLOSED
FCBDRV          EQU         1               ; DRIVE NUMBER
FCBFGR          EQU         2               ; FIRST GRANULE IN FILE
FCBCGR          EQU         3               ; CURRENT GRANULE BEING USED
FCBSEC          EQU         4               ; CURRENT SECTOR BEING USED (1-9)
; 5 UNUSED
FCBPOS          EQU         6               ; CURRENT PRINT POSITION - ALWAYS ZERO IN RANDOM FILES
FCBREC          EQU         7               ; CURRENT RECORD NUMBER
FCBRLN          EQU         9               ; RANDOM FILE RECORD LENGTH
FCBBUF          EQU         11              ; POINTER TO START OF THIS FILE'S RANDOM ACCESS BUFFER
FCBSOF          EQU         13              ; SECTOR OFFSET TO CURRENT POSITION IN RECORD
FCBFLG          EQU         15              ; GET/PUT FLAG: 0=PUT, 1=PUT
; 16,17 NOT USED
FCBDIR          EQU         18              ; DIRECTORY ENTRY NUMBER (0-71)
FCBLST          EQU         19              ; NUMBER OF BYTES IN LAST SECTOR OF FILE
FCBGET          EQU         21              ; 'GET' RECORD COUNTER: HOW MANY CHARACTERS HAVE BEEN PULLED OUT OF THE CURRENT RECORD
FCBPUT          EQU         23              ; 'PUT' RECORD COUNTER: POINTER TO WHERE IN THE RECORD THE NEXT BYTE WILL BE 'PUT'
FCBCON          EQU         25              ; OFFSET TO START OF FCB DATA BUFFER (256 BYTES)

; SEQUENTIAL FILE
; BYTE DESCRIPTION
FCBCPT          EQU         5               ; INPUT FILE: CHARACTER POINTER - POINTS TO NEXT CHARACTER IN
; FILE TO BE PROCESSED.
; OUTPUT FILE: FULL SECTOR FLAG - IF IT IS 1 WHEN THE FILE IS
; CLOSED IT MEANS 256 BYTES OF THE LAST SECTOR HAVE BEEN USED.
; INPUT OR OUTPUT TO A FILE.
; 9 TO 15 UNUSED
FCBCFL          EQU         16              ; CACHE FLAG: 00=CACHE EMPTY, $FF=CACHE FULL
FCBCDT          EQU         17              ; CACHE DATA BYTE
; 21,22 UNUSED
FCBDFL          EQU         23              ; INPUT FILE ONLY: DATA LEFT FLAG: 0=DATA LEFT, $FF=NO DATA (EMPTY)
FCBLFT          EQU         24              ; NUMBER OF CHARACTERS LEFT IN BUFFER (INPUT FILE)
; NUMBER OF CHARS STORED IN BUFFER (OUTPUT FILE)

                ORG         $C000

                FCC         'DK'
                BRA         LRC00C

DCNVEC          FDB         DSKCON          ; DSKCON POINTER
DSKVAR          FDB         $00EA           ; ADDRESS OF DSKCON VARIABLES

; ZERO OUT THE RAM USED BY DISK BASIC
LRC00C          LDX         #DBUF0          ; POINT X TO START OF DISK RAM
LRC00F          CLR         ,X+             ; CLEAR A BYTE
                CMPX        #DFLBUF         ; END OF DISK'S RAM?
                BNE         LRC00F          ; NO - KEEP CLEARING
                LDX         #LC0F6          ; POINT X TO ROM IMAGE OF COMMAND INTERPRETATION TABLE
                LDU         #COMVEC+20      ; POINT U TO RAM ADDRESS OF SAME
                LDB         #10             ; 10 BYTES PER TABLE
                JSR         LA59A           ; MOVE (B) BYTES FROM (X) TO (U)
                LDD         #LB277          ; SYNTAX ERROR ADDRESS
                STD         $03,U           ; SET JUMP TABLE ADDRESSES OF THE USER COMMAND
                STD         $08,U           ; INTERPRETATION TABLE TO POINT TO SYNTAX ERROR
                CLR         ,U              ; CLEAR BYTE 0 OF USER TABLE (DOESN'T EXIST FLAG)
                CLR         $05,U           ; SET NUMBER OF SECONDARY USER TOKENS TO ZERO
                LDD         #DXCVEC         ; SAVE NEW
                STD         COMVEC+13       ; POINTERS TO EXBAS
                LDD         #DXIVEC         ; COMMAND AND SECONDARY
                STD         COMVEC+18       ; COMMAND INTERPRETATION ROUTINES
; MOVE THE NEW RAM VECTORS FROM ROM TO RAM
                LDU         #RVEC0          ; POINT U TO 1ST RAM VECTOR
LC037           LDA         #$7E            ; OP CODE OF JMP INSTRUCTION
                STA         RVEC22          ; SET 1ST BYTE OF 'GET'/'PUT' RAM VECTOR TO 'JMP'
                STA         ,U+             ; SET 1ST BYTE OF RAM VECTOR TO 'JMP'
                LDD         ,X++            ; GET RAM VECTOR FROM ROM
                STD         ,U++            ; STORE IT IN RAM
                CMPX        #LC126          ; COMPARE TO END OF ROM VALUES
                BNE         LC037           ; BRANCH IF NOT ALL VECTORS MOVED
                LDX         #DVEC22         ; GET ROM VALUE OF 'GET'/'PUT' RAM VECTOR
                STX         RVEC22+1        ; SAVE IT IN RAM
; INITIALIZE DISK BASIC'S USR VECTORS
                LDX         #DUSRVC         ; POINT X TO START OF DISK BASIC USR VECTORS
                STX         USRADR          ; SAVE START ADDRESS IN USRADR
                LDU         #LB44A          ; POINT U TO ADDRESS OF 'FUNCTION CALL' ERROR
                LDB         #10             ; 10 USER VECTORS TO INITIALIZE
LC057           STU         ,X++            ; SET USR VECTOR TO 'FC' ERROR
                DECB                        ; DECREMENT USR VECTOR COUNTER
                BNE         LC057           ; BRANCH IN NOT DONE WITH ALL 10 VECTORS
                LDX         #DNMISV         ; GET ADDRESS OF NMI SERVICING ROUTINE
                STX         NMIVEC+1        ; SAVE IT IN NMI VECTOR
                LDA         #$7E            ; OP CODE OF JMP
                STA         NMIVEC          ; MAKE THE NMI VECTOR A JMP
                LDX         #DIRQSV         ; GET ADDRESS OF DISK BASIC IRQ SERVICING ROUTINE
                STX         IRQVEC+1        ; SAVE IT IN IRQVEC
                LDA         #19             ; = INITIALIZE WRITE FAT
                STA         WFATVL          ; = TO DISK TRIGGER VALUE
                CLR         FATBL0          ;
                CLR         FATBL1          ; INITIALIZE THE ACTIVE FILE COUNTER OF
                CLR         FATBL2          ; EACH FAT TO ZERO. THIS WILL CAUSE THE FATS
                CLR         FATBL3          ; TO THINK THERE ARE NO ACTIVE FILES
                LDX         #DFLBUF         ; = GET THE STARTING ADDRESS OF THE
                STX         RNBFAD          ; = RANDOM FILE BUFFER FREE AREA AND DAVE IT AS THE
                LEAX        256,X           ; = START ADDRESS OF FREE RAM FOR RANDOM FILE BUFFERS
                STX         FCBADR          ; SAVE 256 BYTES FOR RANDOM FILE BUFFERS INITIALLY
; SAVE START ADDRESS OF FCBS
                LEAX        $01,X           ; ADD ONE AND SAVE THE STARTING
                STX         FCBV1           ; ADDRESS OF FCB1
                CLR         FCBTYP,X        ; CLEAR THE FIRST BYTE OF FCB 1 (CLOSE FCB)
                LEAX        FCBLEN,X        ; POINT X TO FCB 2
                STX         FCBV1+2         ; SAVE ITS STARTING ADDRESS IN FCB VECTOR TABLE
                CLR         FCBTYP,X        ; CLEAR THE FIRST BYTE OF FCB 2 (CLOSE FCB)
                LEAX        FCBLEN,X        ; POINT X TO SYSTEM FCB - THIS FCB WILL ONLY
; BE USED TO COPY, LOAD, SAVE, MERGE, ETC
                STX         FCBV1+4         ; SAVE ITS ADDRESS IN THE FCB VECTOR TABLE
                CLR         FCBTYP,X        ; CLEAR THE FIRST BYTE OF SYSTEM FCB (CLOSE FCB)
                LDA         #$02            ; SET THE NUMBER OF ACTIVE RESERVED
                STA         FCBACT          ; FILE BUFFERS TO 2 (1,2)
                LEAX        FCBLEN,X        ; POINT X TO ONE PAST THE END OF SYSTEM FCB
                TFR         X,D             ; SAVE THE ADDRESS IN ACCD
                TSTB                        ; ON AN EVEN 256 BYTE BOUNDARY?
                BEQ         LC0B3           ; YES
                INCA                        ; NO - ADD 256 TO ADDRESS
LC0B3           TFR         A,B             ; COPY ACCA TO ACCB
                ADDB        #24             ; SAVE ENOUGH ROOM FOR 4 GRAPHICS PAGES (PCLEAR 4)
                STB         TXTTAB          ; SAVE NEW START OF BASIC ADDRESS
                JSR         L96EC           ; INITIALIZE EXBAS VARIABLES & DO A NEW
                LDA         BEGGRP          ; GET THE START OF CURRENT GRAPHICS PAGE
                ADDA        #$06            ; ADD 1.5K (6 X 256 = ONE GRAPHICS PAGE)
                STA         ENDGRP          ; SAVE NEW END OF GRAPHICS PAGE
                BSR         LC0DD           ; GO INITIALIZE THE FLOPPY DISK CONTROLLER
                ANDCC       #$AF            ; TURN ON IRQ AND FIRQ
                LDX         #LC126-1        ; POINT X TO DISK BASIC COPYRIGHT MESSAGE
                JSR         STRINOUT        ; PRINT COPYRIGHT MESSAGE TO SCREEN
                LDX         #DKWMST         ; GET DISK BASIC WARM START ADDRESS
                STX         RSTVEC          ; SAVE IT IN RESET VECTOR
                JMP         LA0E2           ; JUMP BACK TO BASIC

DKWMST          NOP                         ; WARM START INDICATOR
                BSR         LC0DD           ; INITIALIZE THE FLOPPY DISK CONTROLLER
                JSR         LD1E5           ; CLOSE FILES AND DO MORE INITIALIZATION
                JMP         XBWMST          ; JUMP TO EXBAS' WARM START
LC0DD           CLR         NMIFLG          ; RESET NMI FLAG
                CLR         RDYTMR          ; RESET DRIVE NOT READY TIMER
                CLR         DRGRAM          ; RESET RAM IMAGE OF DSKREG (MOTORS OFF)
                CLR         DSKREG          ; RESET DISK CONTROL REGISTER
                LDA         #$D0            ; FORCE INTERRUPT COMMAND OF 1793
                STA         FDCREG          ; SEND IT TO 1793
                EXG         A,A             ; DELAY
                EXG         A,A             ; DELAY SOME MORE
                LDA         FDCREG          ; GET 1793 STATUS (CLEAR REGISTER)
                RTS

; DISK BASIC COMMAND INTERP TABLES
LC0F6           FCB         19              ; 19 DISK BASIC 1.0 COMMANDS
                FDB         LC17F           ; DISK BASIC'S COMMAND DICTIONARY
                FDB         LC220           ; COMMAND JUMP TABLE
                FCB         6               ; 6 DISK BASIC SECONDARY FUNCTIONS
                FDB         LC201           ; SECONDARY FUNCTION TABLE
                FDB         LC236           ; SECONDARY FUNCTION JUMP TABLE

; RAM HOOKS FOR DISK BASIC
LC100           FDB         DVEC0,DVEC1,DVEC2
                FDB         DVEC3,DVEC4,DVEC5
                FDB         DVEC6,DVEC7,DVEC8
                FDB         XVEC9,DVEC10,DVEC11
                FDB         DVEC12,DVEC13,DVEC14
                FDB         DVEC15,DVEC12,DVEC17
                FDB         DVEC18

; DISK BASIC COPYRIGHT MESSAGE
LC126           FCC         'DISK EXTENDED COLOR BASIC 1.0'
                FCB         CR
                FCC         'COPYRIGHT (C) 198'
                FCB         CYEAR
                FCC         ' BY TANDY'
                FCB         CR
                FCC         'UNDER LICENSE FROM MICROSOFT'
                FCB         CR,CR,0

; DISK BASIC COMMAND DICTIONARY TABLE
; TOKEN #
LC17F           FCS         'DIR'
                FCS         'DRIVE'
                FCS         'FIELD'
                FCS         'FILES'
                FCS         'KILL'
                FCS         'LOAD'
                FCS         'LSET'
                FCS         'MERGE'
                FCS         'RENAME'
                FCS         'RSET'
                FCS         'SAVE'
                FCS         'WRITE'
                FCS         'VERIFY'
                FCS         'UNLOAD'
                FCS         'DSKINI'
                FCS         'BACKUP'
                FCS         'COPY'
                FCS         'DSKI$'
                FCS         'DSKO$'


; DISK BASIC COMMAND JUMP TABLE
; COMMAND / TOKEN #
LC1DB           FDB         DIR             ; DIR / CE
                FDB         DRIVE           ; DRIVE / CF
                FDB         FIELD           ; FIELD / D0
                FDB         FILES           ; FILES / D1
                FDB         KILL            ; KILL / D2
                FDB         LOAD            ; LOAD / D3
                FDB         LSET            ; LSET / D4
                FDB         MERGE           ; MERGE / D5
                FDB         RENAME          ; RENAME / D6
                FDB         RSET            ; RSET / D7
                FDB         SAVE            ; SAVE / D8
                FDB         WRITE           ; WRITE / D9
                FDB         VERIFY          ; VERIFY / DA
                FDB         UNLOAD          ; UNLOAD / DB
                FDB         DSKINI          ; DSKINI /DC
                FDB         BACKUP          ; BACKUP / DD
                FDB         COPY            ; COPY / DE
                FDB         DSKI            ; DSKI$ / DF
                FDB         DSKO            ; DSKO$ / E0



; SECONDARY FUNCTION DICTIONARY TABLE
; TOKEN #
LC201           FCS         'CVN'
                FCS         'FREE'
                FCS         'LOC'
                FCS         'LOF'
                FCS         'MKN$'
                FCS         'AS'

; DISK BASIC SECONDARY FUNCTION JUMP TABLE
; FUNCTION / TOKEN #
LC214           FDB         CVN             ; CVN / A2
                FDB         FREE            ; FREE / A3
                FDB         LOC             ; LOC / A4
                FDB         LOF             ; LOF / A5
                FDB         MKN             ; MKN$ / A6
                FDB         LB277           ; AS / A7 (SYNTAX ERROR)
; DISK BASIC COMMAND INTERPRETATION HANDLER
LC220           CMPA        #DHITOK         ; COMPARE TO HIGHEST DISK BASIC TOKEN
                BHI         LC22C           ; AND BRANCH IF HIGHER
                LDX         #LC1DB          ; POINT X TO DISK BASIC COMMAND JUMP TABLE
                SUBA        #$CE            ; SUBTRACT OUT LOWEST DISK BASIC COMMAND TOKEN
                JMP         LADD4           ; JUMP TO BASIC'S COMMAND HANDLER
LC22C           CMPA        #DHITOK         ; COMPARE TO HIGHEST DISK BASIC TOKEN
                LBLS        LB277           ; 'SYNTAX' ERROR IF < DISK BASIC COMMAND TOKEN
                JMP         [COMVEC+33]     ; PROCESS A USER COMMAND TOKEN
; DISK BASIC SECONDARY COMMAND INTERPRETATION HANDLER
LC236           CMPB        #($A7-$80)*2    ; COMPARE MODIFIED SECONDARY TOKEN TO
                BLS         LC23E           ; HIGHEST DISK BASIC TOKEN & BRANCH IF HIGHER
                JMP         [COMVEC+38]     ; JUMP TO USER SECONDARY COMMAND HANDLER
LC23E           SUBB        #($A2-$80)*2    ; SUBTRACT OUT THE SMALLEST SECONDARY
                PSHS        B               ; DISK TOKEN & SAVE MODIFIED TOKEN ON THE STACK
                JSR         LB262           ; SYNTAX CHECK FOR '(' AND EVALUATE EXPRESSION
                PULS        B               ; RESTORE MODIFIED TOKEN
                LDX         #LC214          ; POINT X TO SECONDARY COMMAND JUMP TABLE
                JMP         LB2CE           ; JUMP TO BASIC'S SECONDARY COMMAND HANDLER

; ERROR DRIVER RAM VECTOR
DVEC17          PULS        Y               ; PUT THE RETURN ADDRESS INTO Y
                JSR         LAD33           ; RESET THE CONT FLAG, ETC
                JSR         LD1E5           ; INITIALIZE SOME DISK VARIABLES AND CLOSE FILES
                PSHS        Y,B             ; PUT RETURN ADDRESS AND ERROR NUMBER ON THE STACK
                JSR         DVEC7           ; CLOSE ALL FILES
                PULS        B               ; GET THE ERROR NUMBER BACK
                CMPB        #2*27           ; COMPARE TO THE LOWEST DISK ERROR NUMBER
                LBCS        XVEC17          ; BRANCH TO EXBAS ERROR HANDLER IF NOT DISK ERROR NUMBER
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF THE STACK
                JSR         LA7E9           ; TURN OFF THE CASSETTE MOTOR
                JSR         LA974           ; DISABLE THE ANALOG MULTIPLEXER
                CLR         DEVNUM          ; SET DEVICE NUMBER TO THE SCREEN
                JSR         LB95C           ; SEND A CR TO THE SCREEN
                JSR         LB9AF           ; SEND A '?' TO THE SCREEN
                LDX         #LC290-2*27     ; POINT X TO DISK BASIC'S ERROR TABLE
                JMP         LAC60           ; JUMP TO BASIC'S ERROR HANDLER

; DISK BASIC ERROR MESSAGES
LC290           FCC         'BR'            ; 27 BAD RECORD
                FCC         'DF'            ; 28 DISK FULL
                FCC         'OB'            ; 29 OUT OF BUFFER SPACE
                FCC         'WP'            ; 30 WRITE PROTECTED
                FCC         'FN'            ; 31 BAD FILE NAME
                FCC         'FS'            ; 32 BAD FILE STRUCTURE
                FCC         'AE'            ; 33 FILE ALREADY EXISTS
                FCC         'FO'            ; 34 FIELD OVERFLOW
                FCC         'SE'            ; 35 SET TO NON-FIELDED STRING
                FCC         'VF'            ; 36 VERIFICATION ERROR
                FCC         'ER'            ; 37 WRITE OR INPUT PAST END OF RECORD

; DISK FILE EXTENSIONS
BASEXT          FCC         'BAS'           ; BASIC FILE EXTENSION
DEFEXT          FCC         '   '           ; BLANK (DEFAULT) FILE EXTENSION
DATEXT          FCC         'DAT'           ; DATA FILE EXTENSION
BINEXT          FCC         'BIN'           ; BINARY FILE EXTENSION

; CLS RAM VECTOR
DVEC22          PSHS        X,CC            ; SAVE X REG AND STATUS
                LDX         $03,S           ; LOAD X WITH CALLING ADDRESS
                CMPX        #L975F          ; COMING FROM EXBAS' GET/PUT?
                BNE         LC2A7           ; NO
                CMPA        #'#'            ; NUMBER SIGN (GET#, PUT#)?
                BEQ         LC2A9           ; BRANCH IF GET OR PUT TO RANDOM FILE
LC2A7           PULS        CC,X,PC         ; RESTORE X REG, STATUS AND RETURN

; GET/PUT TO A DIRECT/RANDOM FILE
LC2A9           LEAS        $05,S           ; PURGE RETURN ADDRESS AND REGISTERS OFF OF THE STACK
                JSR         LC7FE           ; EVALUATE DEVICE NUMBER & SET FCB POINTER
                STX         FCBTMP          ; SAVE FCB POINTER
                CLR         FCBGET,X        ; RESET THE GET
                CLR         FCBGET+1,X      ; DATA POINTER
                CLR         FCBPUT,X        ; = RESET THE PUT
                CLR         FCBPUT+1,X      ; = DATA POINTER
                CLR         FCBPOS,X        ; RESET PRINT POSITION COUNTER
                LDA         FCBDRV,X        ; GET THE FCB DRIVE NUMBER AND
                STA         DCDRV           ; SAVE IT IN DSKCON VARIABLE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER FROM BASIC
                BEQ         LC2D0           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         LB3E6           ; EVALUATE EXPRESSION - RETURN IN ACCD

LC2CC           LDX         FCBTMP          ; POINT X TO FCB
                STD         FCBREC,X        ; SAVE RECORD NUMBER IN FCB
LC2D0           JSR         LC658           ; INCREMENT RECORD NUMBER
                LDD         FCBRLN,X        ; GET RANDOM FILE RECORD LENGTH AND RANDOM FILE
                LDX         FCBBUF,X        ; BUFFER POINTER AND SAVE THEM ON THE STACK -
                PSHS        X,B,A           ; THESE ARE THE INITIAL VALUES OF A TEMPORARY
; RECORD LENGTH COUNTER AND RANDOM BUFFER
; POINTER WHICH ARE MAINTAINED ON THE STACK
                LEAX        -2,U            ; POINT X TO (RECORD NUMBER -1)
                JSR         L9FB5           ; MULT (UNSIGNED) RECORD LENGTH X (RECORD NUMBER -1)
                PSHS        U,Y             ; SAVE PRODUCT ON THE STACK
                LDA         ,S+             ; CHECK MS BYTE OF PRODUCT
                BNE         LC2ED           ; 'BR' ERROR IF NOT ZERO (RECORD NUMBER TOO BIG)
                PULS        X               ; PULL THE BOTTOM 3 PRODUCT BYTES OFF THE STACK;
                PULS        B               ; TOP TWO IN X, BOTTOM IN ACCB; ACCB POINTS TO
; THE FIRST BYTE OF THE SECTOR USED BY THIS RECORD,
; (X) CONTAINS THE SECTOR OFFSET (IN WHICH SECTOR
; FROM THE START THE BYTE IS LOCATED)
LC2E8           CMPX        #(TRKMAX-1)*18  ; 612 SECTORS MAX IN A RANDOM FILE
                BLO         LC2F2           ; BRANCH IF RECORD LENGTH O.K.
LC2ED           LDB         #2*27           ; 'BAD RECORD' ERROR
                JMP         LAC46           ; JUMP TO ERROR HANDLER
LC2F2           LDU         FCBTMP          ; POINT U TO FCB
                CMPX        FCBSOF,U        ; COMPARE SAVED SECTOR OFFSET TO THE CURRENT SECTOR OFFSET
                LBEQ        LC3B1           ; BEING PROCESSED - DO NOT PROCESS A NEW SECTOR IF THEY ARE EQUAL
                PSHS        X,B             ; SAVE BYTE AND SECTOR OFFSET TO RECORD START ON STACK
                LDA         FCBFLG,U        ; CHECK FCB GET/PUT FLAG AND
                BEQ         LC306           ; BRANCH IF IT WAS A GET
                CLR         FCBFLG,U        ; FORCE GET/PUT TO 'PUT'
                LDB         #$03            ; DSKCON WRITE OP CODE
                BSR         LC339           ; GO WRITE A SECTOR - SAVE 'PUT' DATA ON DISK
; CONVERT THE SECTOR OFFSET TO A GRANULE AND SECTOR NUMBER
LC306           LDD         $01,S           ; GET THE NUMBER OF SECTORS TO THE START OF
                JSR         LC754           ; THIS RECORD NUMBER AND CONVERT THEM TO A GRANULE OFFSET
                PSHS        B               ; SAVE GRANULE OFFSET ON THE STACK
                JSR         LC749           ; MULTIPLY GRANULE NUMBER X 9 - CONVERT TO NUMBER OF SECTORS
                NEGB                        ;  NEGATE LS BYTE OF GRANULE OFFSET AND ADD THE
                ADDB        $03,S           ; LS BYTE OF SECTOR OFFSET - ACCB = SECTOR
; NUMBER (0-8) CORRESPONDING TO THE SECTOR NUMBER WITHIN A
; GRANULE OF THE LAST SECTOR OF THE SECTOR OFFSET
                INCB                        ; = ADD ONE - SECTORS SAVED IN THE FCB START
                STB         FCBSEC,U        ; = AT 1 NOT 0 - SAVE IT IN THE FCB
                LDB         FCBFGR,U        ; GET FIRST GRANULE IN FILE
                JSR         LC725           ; POINT X TO FAT
                LEAU        FATCON,X        ; POINT U TO FAT DATA
                LDA         ,S              ; GET NUMBER OF GRANULES OFFSET TO RECORD
                INCA                        ; ADD ONE (COMPENSATE FOR DECA BELOW)
LC320           LEAX        ,U              ; POINT X TO FAT DATA
                ABX                         ; POINT X TO CORRECT GRANULE
                DECA                        ; DECREMENT GRANULE COUNTER
                BEQ         LC35D           ; BRANCH IF CORRECT GRANULE FOUND
                STB         ,S              ; SAVE GRANULE ADDRESS ON STACK
                LDB         ,X              ; GET NEXT GRANULE IN FILE
                CMPB        #$C0            ; LAST GRANULE IN FILE?
                BLO         LC320           ; NO - KEEP LOOKING

; THE GRANULE BEING SEARCHED FOR IS NOT PRESENTLY DEFINED IN THIS RANDOM FILE
                LDB         ,S              ; GET OFFSET TO LAST GRANULE
                TST         VD8             ; CHECK GET/PUT FLAG
                BNE         LC348           ; AND BRANCH IF PUT
LC334           LDB         #2*23           ; 'INPUT PAST END OF FILE' ERROR
                JMP         LAC46           ; JUMP TO ERROR HANDLER
LC339           LEAX        FCBCON,U        ; POINT X TO FCB DATA BUFFER

; READ/WRITE A SECTOR. ENTER WITH OP CODE IN ACCB, BUFFER PTR IN X
LC33C           STB         DCOPC           ; SAVE DSKCON OPERATION CODE VARIABLE
                STX         DCBPT           ; SAVE DSKCON LOAD BUFFER VARIABLE
                LEAX        ,U              ; POINT X TO FCB
                JSR         LC733           ; CONVERT FCB TRACK AND SECTOR TO DSKCON VARIABLES
                JMP         LD5FF           ; READ/WRITE A TRACK OR SECTOR

; 'PUT' DATA INTO A GRANULE NOT PRESENTLY INCLUDED IN THIS FILE
LC348           PSHS        X,A             ; SAVE GRANULE COUNTER AND POINTER TO LAST USED GRANULE
                JSR         LC78F           ; FIND FIRST FREE GRANULE IN FAT
                TFR         A,B             ; SAVE FREE GRANULE NUMBER IN ACCB
                PULS        A,U             ; PULL LAST GRANULE POINTER AND COUNTER OFF OF STACK
                STB         ,U              ; SAVE NEWLY FOUND GRANULE NUMBER IN ADDRESS OF LAST GRANULE
                DECA                        ; DECREMENT GRANULE COUNTER
                BNE         LC348           ; GET ANOTHER GRANULE IF NOT DONE
                PSHS        X,B             ; SAVE POINTER TO LAST GRANULE AND OFFSET
                JSR         LC6F1           ; WRITE FAT TO DISK
                PULS        B,X             ; RESTORE POINTER AND OFFSET

; WHEN CORRECT GRANULE IS FOUND, FIND THE RIGHT SECTOR
LC35D           LEAS        $01,S           ; REMOVE GRAN NUMBER FROM STACK
                LDU         FCBTMP          ; POINT U TO FCB
                STB         FCBCGR,U        ; SAVE CURRENT GRANULE IN FCB
                LDA         #$FF            ; SET FCBSOF,U TO ILLEGAL SECTOR OFFSET WHICH WILL
                STA         FCBSOF,U        ; FORCE NEW SECTOR DATA TO BE READ IN
                LDA         ,X              ; GET CURRENT GRANULE
                CMPA        #$C0            ; IS IT THE LAST GRANULE?
                BLO         LC394           ; NO
                ANDA        #$3F            ; MASK OFF LAST GRANULE FLAG BITS
                CMPA        FCBSEC,U        ; COMPARE CALCULATED SECTOR TO CURRENT SECTOR IN FCB
                BHS         LC394           ; AND BRANCH IF CALCULATED SECTOR IS > LAST SECTOR IN FILE
                LDA         VD8             ; = CHECK GET/PUT FLAG: IF 'GET' THEN 'INPUT
                BEQ         LC334           ; = PAST END OF FILE' ERROR
                LDA         FCBSEC,U        ; GET CURRENT SECTOR NUMBER FROM FCB,
                ORA         #$C0            ; OR IN THE LAST GRANULE FLAG BITS
                STA         ,X              ; AND SAVE IN FAT
                JSR         LC57C           ; WRITE FAT TO DISK IF NECESSARY
                LDX         FCBRLN,U        ; GET RECORD LENGTH AND CHECK TO
                CMPX        #SECLEN         ; SEE IF IT IS SECLEN (EXACTLY ONE SECTOR)
                BNE         LC38F           ; BRANCH IF IT IS NOT EXACTLY ONE SECTOR
                CMPX        FCBLST,U        ; =BRANCH IF THE NUMBER OF BYTES IN THE LAST SECTOR
                BEQ         LC394           ; =IS SET TO ONE SECTOR (SECLEN)
                LDA         #$81            ; SET THE PRESAVED FLAG (BIT15) AND FORCE
LC38E           FCB         $21             ; THE NUMBER OF BYTES IN LAST SECTOR TO 256
LC38F           CLRA                        ; SET THE NUMBER OF BYTES IN LAST SECTOR TO ZERO
                CLRB                        ; CLEAR LS BYTE OF ACCD
                STD         FCBLST,U        ; SAVE THE NUMBER OF BYTES IN LAST SECTOR
LC394           LDB         #$02            ; DSKCON READ OP CODE
                LDX         FCBRLN,U        ; GET RECORD LENGTH AND COMPARE
                CMPX        #SECLEN         ; IT TO SECLEN - EXACTLY ONE SECTOR
                BNE         LC3AA           ; BRANCH IF NOT EXACTLY ONE SECTOR LONG
                LEAS        $07,S           ; CLEAN UP STACK
                LDX         FCBBUF,U        ; POINT X TO START OF RANDOM FILE BUFFER
                LDA         VD8             ; CHECK GET/PUT FLAG AND
                BEQ         LC3A7           ; BRANCH IF GET
                LDB         #$03            ; DSKCON WRITE OP CODE
LC3A7           JMP         LC33C           ; READ/WRITE A SECTOR
LC3AA           JSR         LC339           ; READ A SECTOR INTO FCB DATA BUFFER
                PULS        B,X             ; GET BACK THE BYTE OFFSET TO RECORD: X = NUMBER OF
; SECTORS; ACCB = BYTE POINTER IN SECTOR
                STX         FCBSOF,U        ; SAVE SECTOR OFFSET IN FCB
LC3B1           PSHS        B               ; SAVE BYTE OFFSET ON STACK
                JSR         LC725           ; POINT X TO FILE ALLOCATION TABLE
                LEAX        FATCON,X        ; MOVE X TO FAT DATA
                LDB         FCBCGR,U        ; GET CURRENT GRANULE NUMBER
                ABX                         ; POINT X TO PROPER GRANULE IN FAT
                LDA         ,X              ; GET CURRENT GRANULE AND CHECK TO
                CMPA        #$C0            ; SEE IF IT IS LAST GRANULE
                BLO         LC3E5           ; BRANCH IF THIS GRANULE IS < LAST GRANULE
                ANDA        #$3F            ; MASK OFF LAST GRANULE FLAG BITS
                CMPA        FCBSEC,U        ; COMPARE LAST SECTOR USED IN GRANULE TO
                BNE         LC3E5           ; CALCULATED SECTOR; BRANCH IF NOT EQUAL
                LDD         FCBLST,U        ; GET NUMBER OF BYTES IN LAST SECTOR
                ANDA        #$7F            ; MASK OFF PRESAVED FLAG (BIT 15)
                PSHS        B,A             ; SAVE NUMBER OF BYTES IN LAST SECTOR ON STACK
                CLRA                        ;  LOAD ACCB WITH THE BYTE OFFSET TO CURRENT
                LDB         $02,S           ; RECORD AND ADD THE REMAINING RECORD LENGTH
                ADDD        $03,S           ; TO IT - ACCD = END OF RECORD OFFSET
                CMPD        ,S++            ; =COMPARE THE END OF RECORD OFFSET TO THE NUMBER OF
                BLS         LC3E5           ; =BYTES USED IN THE LAST SECTOR
                TST         VD8             ; CHECK GET/PUT FLAG AND BRANCH IF 'GET'
                LBEQ        LC334           ; TO 'INPUT PAST END OF FILE' ERROR

; IF LAST USED SECTOR, CALCULATE HOW MANY BYTES ARE USED
; IF DATA IS BEING 'PUT' PASTH THE CURRENT END OF FILE
                ANDA        #$01
                ORA         #$80            ; SET PRE-SAVED FLAG BIT - ALL PUT RECORDS ARE
; WRITTEN TO DISK BEFORE LEAVING 'PUT'
                STD         FCBLST,U        ; SAVE NUMBER OF BYTES USED IN LAST SECTOR
LC3E5           PULS        B               ; PULL BYTE OFFSET OFF OF THE STACK
                LEAX        FCBCON,U        ; POINT X TO FCB DATA BUFFER
                ABX                         ; MOVE X TO START OF RECORD
                LDU         $02,S           ; POINT U TO CURRENT POSITION IN RANDOM FILE BUFFER
                PSHS        B               ; SAVE BYTE OFFSET ON STACK
                LDA         #-1             ; CONVERT ACCD INTO A NEGATIVE 2 BYTE NUMBER
; REPRESENTING THE REMAINING UNUSED BYTES IN THE SECTOR
                ADDD        $01,S           ; ADD TEMPORARY RECORD LENGTH COUNTER (SUBTRACT
; REMAINING BYTES FROM TEMPORARY RECORD LENGTH)
                BHS         LC3FC           ; BRANCH IF THERE ARE ENOUGH UNUSED BYTES TO FINISH THE RECORD
                STD         $01,S           ; SAVE NEW TEMPORARY RECORD LENGTH COUNTER
                PULS        B               ; RESTORE BYTE COUNTER
                NEGB                        ;  NEGATE IT - ACCB = THE NUMBER OF BYTES
; AVAILABLE TO A RECORD IN THIS SECTOR
                BRA         LC404           ; MOVE THE DATA

; BRANCH HERE IF REMAINING RECORD LENGTH WILL FIT IN
; WHAT'S LEFT OF THE CURRENTLY SELECTED SECTOR
LC3FC           LDB         $02,S           ; GET REMAINING RECORD LENGTH
                CLR         $01,S           ; CLEAR THE TEMPORARY RECORD LENGTH
                CLR         $02,S           ; COUNTER ON THE STACK
                LEAS        $01,S           ; PURGE BYTE OFFSET FROM STACK
LC404           LDA         VD8             ; CHECK GET/PUT FLAG AND
                BEQ         LC40A           ; BRANCH IF GET
                EXG         X,U             ; SWAP SOURCE AND DESTINATION POINTERS
LC40A           JSR         LA59A           ; TRANSFER DATA FROM SOURCE TO DESTINATION BUFFERS
                STU         $02,S           ; SAVE NEW TEMP RECORD POINTER ON THE STACK (GET)

; MOVE DATA FROM FCB DATA BUFFER TO THE RANDOM FILE BUFFER IF 'GET'
; OR FROM RANDOM FILE BUFFER TO FCB DATA BUFFER IF 'PUT'
                LDU         FCBTMP          ; POINT U TO FCB
                LDA         VD8             ; CHECK GET/PUT FLAG AND
                BEQ         LC419           ; BRANCH IF GET
                STA         FCBFLG,U        ; SAVE 'PUT' FLAG IN THE FCB
                STX         $02,S           ; SAVE NEW TEMPORARY RECORD POINTER ON STACK (PUT)
LC419           LDX         FCBSOF,U        ; GET SECTOR OFFSET COUNTER AND
                LEAX        $01,X           ; ADD ONE TO IT
                CLRB                        ; SET BYTE OFFSET = 0
                LDU         ,S              ; CHECK THE LENGTH OF THE TEMPORARY RECORD LENGTH
                LBNE        LC2E8           ; COUNTER AND KEEP MOVING DATA IF <> 0
                PULS        A,B,X,PC        ; PULL TEMPORARY RECORD LENGTH AND
; BUFFER ADDRESS OFF STACK AND RETURN

; OPEN RAM HOOK
DVEC0           LEAS        $02,S           ; PULL RETURN ADDRESS OFF OF THE STACK
                JSR         LB156           ; EVALUATE AN EXPRESSION
                JSR         LB6A4           ; GET MODE(I,O,R) - FIRST BYTE OF STRING EXPRESSION
                PSHS        B               ; AND SAVE IT ON STACK
                JSR         LA5A2           ; GET DEVICE NUMBER
                TSTB                        ; SET FLAGS
                LBLE        LA603           ; BRANCH IF NOT A DISK FILE
                PULS        A               ; GET MODE
                PSHS        B,A             ; SAVE MODE AND DEVICE NUMBER (FILE NUMBER)
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDX         #DATEXT         ; POINT TO 'DAT' FOR EXTENSION
                JSR         LC88A           ; GET FILENAME FROM BASIC
                LDD         #$01FF          ; DEFAULT DISK FILE TYPE AND ASCII FLAG
                STD         DFLTYP          ; SAVE DEFAULT VALUES: DATA, ASCII
                LDX         #SECLEN         ; DEFAULT RECORD LENGTH - 1 PAGE
                JSR         GETCCH          ; GET CHAR FROM BASIC
                BEQ         LC45C           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         LB3E6           ; EVALUATE EXPRESSION
                LDX         FPA0+2          ; GET EVALUATED EXPRESSION
LC45C           STX         DFFLEN          ; RECORD LENGTH
                LBEQ        LB44A           ; IF = 0, THEN 'ILLEGAL FUNCTION CALL'
                JSR         LA5C7           ; ERROR IF ANY FURTHER CHARACTERS ON LINE
                PULS        A,B             ; GET MODE AND FILE NUMBER

; OPEN DISK FILE FOR READ OR WRITE
LC468           PSHS        A               ; SAVE MODE ON STACK
                JSR         LC719           ; POINT X TO FCB FOR THIS FILE
                LBNE        LA61C           ; 'FILE ALREADY OPEN' ERROR IF FILE OPEN
                STX         FCBTMP          ; SAVE FILE BUFFER POINTER
                JSR         LC76D           ; MAKE SURE FILE ALLOC TABLE IS VALID
                JSR         LC65F           ; SCAN DIRECTORY FOR 'FILENAME.EXT'
                PULS        B               ; GET MODE
                LDA         #INPFIL         ; INPUT TYPE FILE
                PSHS        A               ; SAVE FILE TYPE ON STACK
                CMPB        #'I'            ; INPUT MODE?
                BNE         LC4A2           ; BRANCH IF NOT

; OPEN A SEQUENTIAL FILE FOR INPUT
                JSR         LC6B8           ; CHECK TO SEE IF DIRECTORY MATCH IS FOUND
                JSR         LC7D7           ; CHECK TO SEE IF FILE ALREADY OPEN
                LDX         V974            ; GET RAM DIRECTORY BUFFER
                LDD         DIRTYP,X        ; GET FILE TYPE AND ASCII FLAG
                STD         DFLTYP          ; SAVE IN RAM IMAGE
                BSR         LC500           ; INITIALIZE FILE BUFFER CONTROL BLOCK
                JSR         LC5FA           ; GO FILL DATA BUFFER
LC496           JSR         LC725           ; POINT X TO PROPER FILE ALLOCATION TABLE
                INC         FAT0,X          ; ADD ONE TO FAT ACTIVE FILE COUNTER
                LDX         FCBTMP          ; GET FILE BUFFER POINTER
                PULS        A               ; GET FILE TYPE
                STA         FCBTYP,X        ; SAVE IT IN FCB
                RTS
LC4A2           ASL         ,S              ; SET FILE TYPE TO OUTPUT
                CMPB        #'O'            ; FILE MODE = OUTPUT?
                BNE         LC4C2           ; BRANCH IF NOT

; OPEN A SEQUENTIAL FILE FOR OUTPUT
                TST         V973            ; DOES FILE EXIST ON DIRECTORY?
                BEQ         LC4BC           ; BRANCH IF NOT
                JSR         LC6CF           ; KILL THE OLD FILE
                LDA         V973            ; GET DIRECTORY SECTOR NUMBER OF OLD FILE AND
                STA         V977            ; SAVE IT AS FIRST FREE DIRECTORY ENTRY
                LDX         V974            ; =GET RAM DIRECTORY IMAGE OF OLD FILE AND
                STX         V978            ; =SAVE IT AS FIRST FREE DIRECTORY ENTRY

LC4BC           BSR         LC53A           ; SET UP NEW DIRECTORY ENTRY ON DISK
                BSR         LC50B           ; INITIALIZE FILE BUFFER
                BRA         LC496           ; FLAG AND MAP FCB AS BEING USED
LC4C2           CMPB        #'R'            ; FILE MODE = R (RANDOM)?
                BEQ         LC4CC           ; BRANCH IF SO
                CMPB        #'D'            ; FILE MODE = D (DIRECT)?
                LBNE        LA616           ; 'BAD FILE MODE' ERROR IF NOT

; OPEN A RANDOM/DIRECT FILE
LC4CC           ASL         ,S              ; SET FILE TYPE TO DIRECT
                LDD         RNBFAD          ; GET ADDRESS OF RANDOM FILE BUFFER AREA
                PSHS        B,A             ; AND SAVE IT ON THE STACK
                ADDD        DFFLEN          ; ADD THE RECORD LENGTH
                BLO         LC4DE           ; 'OB' ERROR IF SUM > $FFFF
                CMPD        FCBADR          ; IS IT > THAN FCB DATA AREA?
                BLS         LC4E3           ; BRANCH IF NOT
LC4DE           LDB         #2*29           ; 'OUT OF BUFFER SPACE' ERROR
                JMP         LAC46           ; JUMP TO ERROR HANDLER
LC4E3           PSHS        B,A             ; SAVE END OF RANDOM BUFFER ON STACK
                TST         V973            ; DID THIS FILE EXIST
                BNE         LC4EC           ; BRANCH IF SO
                BSR         LC53A           ; SET UP NEW FILE IN DIRECTORY






LC4EC           BSR         LC500           ; INITIALIZE FILE BUFFER
                COM         FCBSOF,X        ; SET FCBSOF,X TO $FF (ILLEGAL SECTOR OFFSET) WHICH WILL
; FORCE NEW SECTOR DATA TO BE READ IN DURING GET/PUT
                INC         FCBREC+1,X      ; INITIALIZE RECORD NUMBER = 1
                PULS        A,B,U           ; U = START OF RANDOM FILE BUFFER AREA, ACCD = END
                STD         RNBFAD          ; SAVE NEW START OF RANDOM FILE BUFFER AREA
                STU         FCBBUF,X        ; SAVE BUFFER START IN FCB
                LDU         DFFLEN          ; GET RANDOM FILE RECORD LENGTH
                STU         FCBRLN,X        ; AND SAVE IT IN FCB
                BRA         LC496           ; SET FAT FLAG, SAVE FILE TYPE IN FCB

; INITIALIZE FCB DATA FOR INPUT
LC500           BSR         LC50B           ; INITIALIZE FCB
                LDU         V974            ; GET RAM DIRECTORY IMAGE
                LDU         DIRLST,U        ; GET NUMBER OF BYTES IN LAST SECTOR OF FILE
                STU         FCBLST,X        ; SAVE IT IN FCB
                RTS
; INITIALIZE FILE CONTROL BLOCK
LC50B           LDX         FCBTMP          ; GET CURRENT FILE BUFFER
                LDB         #FCBCON         ; CLEAR FCB CONTROL BYTES
LC50F           CLR         ,X+             ; CLEAR A BYTE
                DECB                        ; DECREMENT COUNTER
                BNE         LC50F           ; BRANCH IF NOT DONE
                LDX         FCBTMP          ; GET CURRENT FILE BUFFER ADDRESS BACK
                LDA         DCDRV           ; GET CURRENT DRIVE NUMBER AND
                STA         FCBDRV,X        ; SAVE IT IN FCB
                LDA         V976            ; =GET FIRST GRANULE -
                STA         FCBFGR,X        ; =SAVE IT AS THE STARTING GRANULE NUMBER AND
                STA         FCBCGR,X        ; =SAVE IT AS CURRENT GRANULE NUMBER
                LDB         V973            ; GET DIRECTORY SECTOR NUMBER
                SUBB        #$03            ; SUBTRACT 3 - DIRECTORY SECTORS START AT 3
                ASLB                        ;  MULTIPLY SECTORS
                ASLB                        ;  BY 8 (8 DIRECTORY
                ASLB                        ;  ENTRIES PER SECTOR)
                PSHS        B               ; SAVE SECTOR OFFSET
                LDD         V974            ; GET RAM DIRECTORY IMAGE
                SUBD        #DBUF0          ; SUBTRACT RAM OFFSET
                LDA         #$08            ; 8 DIRECTORY ENTRIES/SECTOR
                MUL                         ; NOW ACCA CONTAINS 0-7
                ADDA        ,S+             ; ACCA CONTAINS DIRECTORY ENTRY (0-71)
                STA         FCBDIR,X        ; SAVE DIRECTORY ENTRY NUMBER
                RTS

; SET UP DIRECTORY AND UPDATE FILE ALLOCATION TABLE ENTRY IN FIRST UNUSED SECTOR
LC53A           LDB         #28*2           ; 'DISK FULL' ERROR
                LDA         V977            ; GET SECTOR NUMBER OF FIRST EMPTY DIRECTORY ENTRY
                LBEQ        LAC46           ; 'DISK FULL' ERROR IF NO EMPTY DIRECTORY ENTRIES
                STA         V973            ; SAVE SECTOR NUMBER OF FIRST EMPTY DIRECTORY ENTRY
                STA         DSEC            ; SAVE SECTOR NUMBER IN DSKCON REGISTER
                LDB         #$02            ; READ OP CODE
                STB         DCOPC           ; SAVE IN DSKCON REGISTER
                JSR         LD5FF           ; READ SECTOR
                LDX         V978            ; GET ADDRESS OF RAM IMAGE OF UNUSED DIRECTORY
                STX         V974            ; ENTRY AND SAVE AS CURRENT USED RAM IMAGE
                LEAU        ,X              ; (TFR X,U) POINT U TO DIRECTORY RAM IMAGE
                LDB         #DIRLEN         ; SET COUNTER TO CLEAR 32 BYTES (DIRECTORY ENTRY)
LC559           CLR         ,X+             ; CLEAR BYTE
                DECB                        ; DECREMENT COUNTER
                BNE         LC559           ; CONTINUE IF NOT DONE
                LDX         #DNAMBF         ; POINT TO FILENAME AND EXTENSION RAM IMAGE
                LDB         #11             ; 11 BYTES IN FILENAME AND EXTENSION
                JSR         LA59A           ; MOVE B BYTES FROM X TO U
                LDD         DFLTYP          ; GET FILE TYPE AND ASCII FLAG
                STD         $00,U           ; SAVE IN RAM IMAGE
                LDB         #33             ; FIRST GRANULE TO CHECK
                JSR         LC78F           ; FIND THE FIRST FREE GRANULE
                STA         V976            ; SAVE IN RAM
                STA         $02,U           ; SAVE IN RAM IMAGE OF DIRECTORY TRACK
                LDB         #$03            ; GET WRITE OPERATION CODE AND SAVE
                STB         DCOPC           ; IT IN DSKCON REGISTER
                JSR         LD5FF           ; GO WRITE A SECTOR IN DIRECTORY
LC57C           PSHS        U,X,B,A         ; SAVE REGISTERS
                JSR         LC725           ; POINT X TO FILE ALLOCATION TABLE
                INC         FAT1,X          ; INDICATE NEW DATA IN FILE ALLOC TABLE
                LDA         FAT1,X          ; GET NEW DATA FLAG
                CMPA        WFATVL          ; HAVE ENOUGH GRANULES BEEN REMOVED FROM THE FAT TO
; CAUSE THE FAT TO BE WRITTEN TO THE DISK
                BLO         LC58D           ; RETURN IF NO NEED TO WRITE OUT ALLOCATION TABLE
                JSR         LC6F1           ; WRITE FILE ALLOCATION SECTOR TO DISK
LC58D           PULS        A,B,X,U,PC      ; RESTORE REGISTERS

; CONSOLE IN RAM VECTOR
DVEC4           LDA         DEVNUM          ; GET DEVICE NUMBER
                LBLE        XVEC4           ; BRANCH IF NOT DISK FILE
                LEAS        $02,S           ; GET RID OF RETURN ADDRESS
LC597           PSHS        X,B             ; SAVE REGISTERS
                CLR         CINBFL          ; CLEAR BUFFER NOT EMPTY FLAG
                LDX         #FCBV1-2        ; POINT TO FILE BUFFER VECTOR TABLE
                LDB         DEVNUM          ; GET ACTIVE DISK FILE NUMBER
                ASLB                        ; TIMES 2 - TWO BYTES PER FCB ADDRESS
                LDX         B,X             ; NOW X POINTS TO FILE BUFFER
                LDB         ,X              ; GET FILE TYPE (FCBTYP,X)
                CMPB        #RANFIL         ; IS THIS A RANDOM (DIRECT) FILE?
                BNE         LC5BF           ; BRANCH IF NOT

; GET A BYTE FROM A RANDOM FILE - RETURN CHAR IN ACCA
                LDD         FCBGET,X        ; GET THE RECORD COUNTER
                CMPD        FCBRLN,X        ; COMPARE TO RECORD LENGTH AND
                BHS         LC5D1           ; BRANCH TO BUFFER EMPTY IF >= RECORD LENGTH
                ADDD        #$0001          ; = ADD ONE TO RECORD POINTER AND
                STD         FCBGET,X        ; = SAVE IT IN FCB
                LDX         FCBBUF,X        ; POINT X TO START OF RANDOM FILE BUFFER AND
                LEAX        D,X             ; ADD THE RECORD COUNTER TO IT
                LDA         -1,X            ; GET A CHARACTER FROM THE BUFFER
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
; GET A BYTE FROM A SEQUENTIAL FILE
LC5BF           LDB         FCBCFL,X        ; TEST THE CACHE FLAG AND BRANCH IF AN
                BEQ         LC5CC           ; EXTRA CHARACTER HAS NOT BEEN READ FROM FILE
                LDA         FCBCDT,X        ; GET THE CACHE CHARACTER
                CLR         FCBCFL,X        ; CLEAR THE CACHE FLAG
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN

LC5CC           LDB         FCBDFL,X        ; IS ANY DATA LEFT?
                BEQ         LC5D5           ; BRANCH IF SO
LC5D1           COM         CINBFL          ; SET FLAG TO BUFFER EMPTY
                PULS        B,X,PC

LC5D5           LDB         FCBCPT,X        ; GET CHARACTER POINTER
                INC         FCBCPT,X        ; ADD ONE TO CHARACTER POINTER
                DEC         FCBLFT,X        ; DECREMENT NUMBER OF CHARACTERS LEFT IN FILE BUFFER
                BEQ         LC5E4           ; IF LAST CHARACTER, GO GET SOME MORE
                ABX                         ; ADD CHARACTER COUNTER TO X
                LDA         FCBCON,X        ; GET DATA CHARACTER (SKIP PAST 25 FCB CONTROL BYTES
                PULS        B,X,PC
; GET A CHARACTER FROM FCB DATA BUFFER - RETURN CHAR IN ACCA
LC5E4           PSHS        U,Y             ; SAVE REGISTERS
                CLRA                        ;
                LEAU        D,X             ; POINT U TO CORRECT CHARACTER
                LDA         FCBCON,U        ; =GET DATA CHAR (SKIP PAST 25 CONTROL BYTES)
                PSHS        A               ; =AND SAVE DATA CHARACTER ON STACK
                CLR         FCBCPT,X        ; RESET CHAR POINTER TO START OF BUFFER
                LDA         FCBDRV,X        ; GET DRIVE NUMBER AND SAVE IT IN
                STA         DCDRV           ; DSKCON VARIABLE
                BSR         LC5FA           ; GO READ A SECTOR - FILL THE BUFFER
                PULS        A,Y,U           ; RESTORE REGISTERS AND DATA CHARACTER
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
; REFILL THE FCB INPUT DATA BUFFER FOR SEQUENTIAL FILES
LC5FA           LDA         FCBSEC,X        ; GET CURRENT SECTOR NUMBER
LC5FC           INCA                        ; ADD ONE
                PSHS        A               ; SAVE NEW SECTOR NUMBER ON THE STACK
                CMPA        #$09            ; NINE SECTORS PER GRANULE
                BLS         LC604           ; BRANCH IF <= 9
                CLRA                        ; SET TO SECTOR ZERO
LC604           STA         FCBSEC,X        ; SAVE SECTOR NUMBER
                LDB         FCBCGR,X        ; GET GRANULE NUMBET TO FAT POINTER
                LEAU        ,X              ; POINT U TO FCB (TFR X,U)
                JSR         LC725           ; POINT X TO PROPER FILE ALLOCATION TABLE
                ABX                         ; ADD OLD GRANULE NUMBER TO FAT POINTER
                LDB         FATCON,X        ; GET GRANULE NUMBER (6 CONTROL BYTES AT FRONT OF FAT)
                LEAX        ,U              ; POINT X TO FCB
                CMPB        #$C0            ; IS CURRENT GRANULE LAST ONE IN FILE?
                BHS         LC620           ; YES
                PULS        A               ; GET SECTOR NUMBER
                SUBA        #10             ; WAS IT 10? - OVERFLOW TO NEXT GRANULE IF SO
                BNE         LC631           ; BRANCH IF NOT
                STB         FCBCGR,X        ; SAVE NEW GRANULE NUMBER
                BRA         LC5FC           ; SET VARIABLES FOR NEW GRANULE
LC620           ANDB        #$3F            ; GET NUMBER OF SECTORS USED IN THIS GRANULE
                CMPB        #$09            ; 9 SECTORS / GRANULE
                BLS         LC62B           ; BRANCH IF OK
LC626           LDB         #2*32           ; 'BAD FILE STRUCTURE' ERROR
                JMP         LAC46           ; ERROR DRIVER
LC62B           SUBB        ,S+             ; SUBTRACT CURRENT SECTOR NUMBER AND PULS A
                BLO         LC650           ; BRANCH IF PAST LAST SECTOR
                TFR         B,A             ; SECTOR NUMBER TO ACCA
LC631           PSHS        A               ; SAVE SECTOR NUMBER DIFFERENCE
                BSR         LC658           ; INCREMENT RECORD NUMBER
                LDA         #$02            ; GET READ OPERATION CODE
                STA         DCOPC           ; AND SAVE IT IN DSKCON VARIABLE
                JSR         LC733           ; GET PROPER TRACK AND SECTOR TO DSKCON VARIABLES
                LEAU        FCBCON,X        ; POINT U TO START OF FCB DATA BUFFER
                STU         DCBPT           ; AND SAVE IT IN DSKCON VARIABLE
                JSR         LD5FF           ; GO READ A SECTOR INTO FCB BUFFER
                CLR         FCBLFT,X        ; NUMBER OF CHARS LEFT IN BUFFER = 256
                LDB         ,S+             ; GET SECTOR NUMBER OFF STACK
                BNE         LC657           ; RETURN IF DATA LEFT; FALL THRU IF LAST SECTOR
                LDD         FCBLST,X        ; GET NUMBER OF BYTES IN THE LAST SECTOR
                BNE         LC654           ; BRANCH IF SOME BYTES IN LAST SECTOR
LC650           CLRB                        ; SET NUMBER OF REMAINING BYTES = 256
                COM         FCBDFL,X        ; SET DATA LEFT FLAG TO $FF
LC654           STB         FCBLFT,X        ; SAVE THE NUMBER OF CHARS LEFT IN BUFFER
LC657           RTS

LC658           LDU         FCBREC,X        ; GET CURRENT RECORD NUMBER
                LEAU        $01,U           ; BUMP IT
                STU         FCBREC,X        ; PUT IT BACK
                RTS

; SCAN DIRECTORY FOR FILENAME.EXT FOUND IN DNAMBF. IF FILENAME FOUND,
; RETURN WITH SECTOR NUMBER IN V973, GRANULE IN V976 AND RAM BUFFER
; CONTAINING DIRECTORY DATA IN V974. IF DISK IS FULL THEN V973,
; V977 = 0. THE FIRST UNUSED SECTOR RETURNED IN V977, RAM IMAGE IN V978
LC65F           CLR         V973            ; CLEAR SECTOR NUMBER
                CLR         V977            ; CLEAR TEMP SECTOR COUNTER
                LDD         #$1102          ; TRACK 17 (DIRECTORY), READ OPERATION CODE
                STA         DCTRK           ; SAVE TRACK NUMBER
                STB         DCOPC           ; SAVE OPERATION CODE (READ)
                LDB         #$03            ; READ SECTOR 3 (FIRST DIRECTORY SECTOR)
LC66E           STB         DSEC            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                LDU         #DBUF0          ; BUFFER AREA NUMBER 0 AS DATA BUFFER - SAVE
                STU         DCBPT           ; IN DSKCON VARIABLE
                JSR         LD5FF           ; GO READ A SECTOR
LC678           STU         V974            ; SAVE RAM DIRECTORY BUFFER ADDRESS
                LEAY        ,U              ; POINT Y TO DIRECTORY BUFFER
                LDA         ,U              ; GET A BYTE FROM BUFFER
                BNE         LC6A9           ; BRANCH IF NOT ZERO - FILE IS ACTIVE
                BSR         LC6AC           ; SET UNUSED FILE POINTERS IF ENTRY HAS BEEN KILLED
LC683           LDX         #DNAMBF         ; POINT TO DISK FILE NAME BUFFER
LC686           LDA         ,X+             ; COMPARE THE FILENAME AND EXTENSION
                CMPA        ,U+             ; STORED IN RAM AT DNAMBF TO THE DIRECTORY
                BNE         LC69A           ; ENTRY STORED AT ,U (BRANCH IF MISMATCH)
                CMPX        #DNAMBF+11      ; AT END OF FILE NAME BUFFER?
                BNE         LC686           ; BRANCH IF NOT DONE CHECKING FILENAME
                STB         V973            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                LDA         FCBFGR,U        ; GET NUMBER OF FIRST GRANULE IN FILE
                STA         V976            ; AND SAVE IT IN V976
                RTS

LC69A           LEAU        DIRLEN,Y        ; GET NEXT DIRECTORY ENTRY (DIRLEN BYTES PER ENTRY)
                CMPU        #DBUF0+SECLEN   ; AT END OF BUFFER?
                BNE         LC678           ; CHECK NEXT ENTRY IF NOT AT END
                INCB                        ; NEXT SECTOR
                CMPB        #11             ; 11 SECTORS MAX IN DIRECTORY
                BLS         LC66E           ; BRANCH IF MORE SECTORS
                RTS

LC6A9           COMA                        ; COMPLEMENT FIRST BYTE IN DIRECTORY EMTRY
                BNE         LC683           ; BRANCH IF FILE IS ACTIVE - FALL THRU IF NOT USED

; SET POINTERS FOR FIRST UNUSED DIRECTORY ENTRY
LC6AC           LDA         V977            ; UNUSED ENTRY ALREADY FOUND?
                BNE         DVEC12          ; RETURN IF UNUSED ENTRY ALREADY FOUND
                STB         V977            ; SECTOR CONTAINING THIS DIRECTORY ENTRY
                STU         V978            ; POINTS TO RAM AREA WHERE DIRECTORY DATA IS STORED
DVEC12          RTS

LC6B8           LDB         #2*26           ; 'NE' ERROR
                TST         V973            ; WAS A DIRECTORY MATCH FOUND?
                BNE         DVEC12          ; RETURN IF FOUND
                JMP         LAC46           ; JUMP TO ERROR HANDLER IF NOT FOUND

; KILL COMMAND
KILL            JSR         LC887           ; GET FILENAME.EXT FROM BASIC
                JSR         LA5C7           ; 'SYNTAX' ERROR IF MORE CHARACTERS ON LINE
                JSR         LC76D           ; GET VALID FAT DATA
                BSR         LC65F           ; TEST FOR FILE NAME MATCH IN DIRECTORY
                BSR         LC6B8           ; MAKE SURE THE FILE EXISTED
LC6CF           LDA         #$FF            ; MATCH FILE TYPE = $FF; THIS WILL CAUSE AN 'AO'
; ERROR TO BE GENERATED IF ANY FILE TYPE IS OPEN
                JSR         LC7D7           ; CHECK TO MAKE SURE FILE IS NOT OPEN
                LDX         V974            ; GET RAM IMAGE OF DIRECTORY
                CLR         ,X              ; AND ZERO FIRST BYTE - KILL FILE (DIRNAM,X)
                LDB         #$03            ; =WRITE OPERATION CODE - SAVE
                STB         DCOPC           ; =IT IN DSKCON VARIABLE
                JSR         LD5FF           ; WRITE A SECTOR
                LDB         DIRGRN,X        ; GET NUMBER OF FIRST GRANULE IN FILE
LC6E2           BSR         LC725           ; POINT X TO PROPER FILE ALLOCATION TABLE
                LEAX        FATCON,X        ; SKIP 6 CONTROL BYTES
                ABX                         ; POINT TO CORRECT ENTRY
                LDB         ,X              ; GET NEXT GRANULE
                LDA         #$FF            ; GET FREE GRANULE FLAG AND
                STA         ,X              ; MARK GRANULE AS FREE
                CMPB        #$C0            ; WAS THIS THE LAST GRANULE?
                BLO         LC6E2           ; KEEP FREEING GRANULES IF NOT LAST ONE
; WRITE FILE ALLOCATION SECTOR TO DIRECTORY - DO NOT WRITE
; THE SIX CONTROL BYTES AT THE START OF THE FAT TO THE DISK
LC6F1           LDU         #DBUF0          ; =POINT U TO DISK BUFFER 0 AND
                STU         DCBPT           ; =SAVE IT AS DSKCON VARIABLE
                LDD         #$1103          ; WRITE DIRECTORY TRACK - SAVE
                STA         DCTRK           ; TRACK AND WRITE OPERATION CODE IN
                STB         DCOPC           ; DSKCON VARIABLES
                LDB         #$02            ; = GET FILE ALLOCATION SECTOR AND
                STB         DSEC            ; = SAVE IN DSKCON VARIABLE
                BSR         LC725           ; POINT X TO PROPER FILE ALLOCATION TABLE
                CLR         FAT1,X          ; RESET FLAG INDICATING VALID FAT DATA HAS BEEN STORED ON DISK
                LEAX        FATCON,X        ; MOVE (X) TO START OF GRANULE DATA
                LDB         #GRANMX         ; 68 BYTES IN FAT
                JSR         LA59A           ; MOVE ACCB BYTES FROM FAT RAM IMAGE TO DBUF0

; ZERO OUT ALL OF THE BYTES IN THE FAT SECTOR WHICH DO NOT CONTAIN THE GRANULE DATA
; ZERO OUT THE REMAINDER OF THE SECTOR BUFFER
LC70C           CLR         ,X+             ; THIS IS A BUG; SHOULD BE CLR ,U+
                CMPX        #DBUF0+SECLEN   ; MORE OF THE SAME BUG; SHOULD BE CMPU
; BNE LC70C THIS INSTRUCTION HAS BEEN LEFT OUT
                JMP         LD5FF           ; WRITE A SECTOR

; ENTER WITH ACCB CONTAINING FILE NUMBER (1-15); EXIT WITH X POINTING
; TO CORRECT FILE BUFFER; FLAGS SET ACCORDING TO FILE TYPE.

LC714           PSHS        B               ; SAVE FILE NUMBER ON STACK
                LDB         DEVNUM          ; GET DEVICE NUMBER (FILE NUMBER)
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
LC719           PSHS        B               ; SAVE FILE NUMBER ON STACK
                ASLB                        ; X2: 2 BYTES PER POINTER
                LDX         #FCBV1-2        ; POINT X TO START OF FCB POINTERS
                LDX         B,X             ; POINT X TO PROPER FCB
                LDB         FCBTYP,X        ; SET FLAGS ACCORDING TO FILE TYPE
                PULS        B,PC            ; RESTORE FILE NUMBER

; POINT X TO DRIVE ALLOCATION TABLE

LC725           PSHS        B,A             ; SAVE ACCD ON STACK
                LDA         DCDRV           ; GET DRIVE NUMBER
                LDB         #FATLEN         ; GET LENGTH OF FILE ALLOCATION TABLE
                MUL                         ; MULTIPLY BY DRIVE NUMBER TO GET OFFSET
                LDX         #FATBL0         ; START OF FILE ALLOCATION TABLE
                LEAX        D,X             ; POINT TO RIGHT TABLE
                PULS        A,B,PC          ; RESTORE ACCD

; CONVERT GRANULE NUMBER TO TRACK & SECTOR NUMBER - X MUST BE POINTING TO CORRECT
; FCB; THE TRACK AND SECTOR NUMBER WILL BE STORED IN DSKCON REGISTERS
LC733           LDB         FCBCGR,X        ; GET GRANULE NUMBER
                LSRB                        ; DIVIDE BY 2 - 2 GRANULES / TRACK
                STB         DCTRK           ; TRACK NUMBER
                CMPB        #17             ; TRACK 17 = DIRECTORY TRACK
                BLO         LC73E           ; BRANCH IF < DIRECTORY TRACK
                INC         DCTRK           ; INCR TRACK NUMBER IF > DIRECTORY TRACK
LC73E           ASLB                        ; MULTIPLY TRACK NUMBER BY 2
                NEGB                        ; NEGATE GRANULE NUMBER
                ADDB        FCBCGR,X        ; B=0 IF EVEN GRANULE; 1 IF ODD
                BSR         LC749           ; RETURN B=0 FOR EVEN GRANULE NUMBER, B=9 FOR ODD GRANULE NUMBER
                ADDB        FCBSEC,X        ; ADD SECTOR NUMBER
                STB         DSEC            ; SAVE SECTOR NUMBER
                RTS
; MULTIPLY ACCD BY 9
LC749           PSHS        B,A             ; TEMP STORE ACCD ON STACK
                ASLB                        ;
                ROLA                        ;  MULTIPLY BY 2
                ASLB                        ; =
                ROLA                        ; = MULTIPLY BY FOUR
                ASLB                        ;
                ROLA                        ;  MULTIPLY BY EIGHT
                ADDD        ,S++            ; ADD ONE = MULTIPLY BY NINE
                RTS

; CONVERT ACCD INTO A GRANULE NUMBER - RETURN RESULT IN ACCB;
; ENTER WITH ACCD CONTAINING A NUMBER OF SECTORS. RETURN IN ACCB
; THE NUMBER (0-67) CORRESPONDING TO THE NUMBER OF COMPLETE
; GRANULES CONTAINED IN THAT MANY SECTORS.
; DIVIDE BY 90, MULTIPLY BY 10 IS FASTER THAN DIVIDE BY 9
LC754           CLR         ,-S             ; CLEAR A TEMPORARY SLOT ON THE STACK
LC756           INC         ,S              ; DIVIDE ACCD BY 90 - SAVE THE
                SUBD        #9*10           ; QUOTIENT+1 ON THE STACK - REMAINDER
                BPL         LC756           ; IN ACCB
                LDA         ,S              ; = PUT THE QUOTIENT+1 IN ACCA AND
                STB         ,S              ; = SAVE REMAINDER ON STACK
                LDB         #10             ; MULTIPLY (QUOTIENT+1)
                MUL                         ;  BY 10
                PULS        A               ; PUT THE REMAINDER IN ACCA
LC766           DECB                        ;  DECREMENT THE GRANULE COUNT BY ONE FOR
                ADDA        #$09            ; EVERY NINE SECTORS (1 GRANULE) IN THE
                BMI         LC766           ; REMAINDER - COMPENSATE FOR THE + 1 IN QUOTIENT+1
                CLRA                        ; CLEAR MS BYTE OF ACCD
LC76C           RTS

; MAKE SURE RAM FILE ALLOCATION TABLE DATA IS VALID
LC76D           BSR         LC725           ; POINT X TO FAT FOR THE CORRECT DRIVE NUMBER
                TST         FAT0,X          ; CHECK TO SEE IF ANY FILES ARE ACTIVE
                BNE         LC76C           ; RETURN IF ANY FILES ACTIVE IN THIS FAT
                CLR         FAT1,X          ; RESET FAT DATA VALID FLAG
                LEAU        FATCON,X        ; LOAD U WITH START OF GRANULE DATA BUFFER
                LDX         #DBUF0          ; BUFFER FOR DISK TRANSFER
                STX         DCBPT           ; PUT IN DSKCON PARAMETER
                LDD         #$1102          ; DIRECTORY TRACK, READ SECTOR
                STA         DCTRK           ; STORE IN DSKCON TRACK NUMBER
                STB         DCOPC           ; STORE IN DSKCON OP CODE
                LDB         #$02            ; GET SECTOR NUMBER 2 (FILE ALLOCATION TABLE)
                STB         DSEC            ; STORE IN DSKCON PARAMETER
                JSR         LD5FF           ; GO READ SECTOR
                LDB         #GRANMX         ; TRANSFER FILE ALLOCATION TABLE TO FILE ALLOC TABLE BUFFER
                JMP         LA59A           ; MOVE B BYTES FROM (X) TO (U)

; FIND FIRST FREE GRANULE - ENTER WITH ACCB CONTAINING
; GRANULE FROM WHICH TO START SEARCHING. THE FOUND GRANULE
; IS MARKED BY STORING A $C0 IN THE GRANULE'S DATA BYTE
; TO INDICATE THAT IT IS THE LAST GRANULE IN THE FILE.
; RETURN WITH FIRST FREE GRANULE FOUND IN ACCA
LC78F           BSR         LC725           ; POINT X TO FILE ALLOC TABLE
                LEAX        FATCON,X        ; SKIP CONTROL BYTES
                CLRA                        ; USE ACCA AS GRANULE COUNTER
                ANDB        #$FE            ; MASK OFF BIT ZERO OF SEARCH GRANULE
                CLR         ,-S             ; INITIALIZE AND SAVE A BYTE ON STACK (DIRECTION FLAG)
LC798           COM         B,X             ; IS THIS GRANULE FREE? ($FF=FREE)
                BEQ         LC7CD           ; BRANCH IF IT IS
                COM         B,X             ; RESTORE GRANULE DATA
                INCA                        ; ADD ONE TO GRANULE COUNTER
                CMPA        #GRANMX         ; GRANMX GEANULES PER DISK
                BHS         LC7C8           ; BRANCH IF ALL GRANULES CHECKED (DISK FULL)
                INCB                        ; INCR TO NEXT GRANULE
                BITB        #$01            ; IS BIT 0 SET?
                BNE         LC798           ; BRANCH IF ODD GRANULE NUMBER (SAME TRACK)
                PSHS        B,A             ; SAVE GRANULE COUNTER AND CURRENT GRANULE NUMBER
                SUBB        #$02            ; SUBTRACT ONE TRACK (2 GRANULES)
                COM         $02,S           ; COMPLEMENT DIRECTION FLAG
                BNE         LC7BC           ; BRANCH EVERY OTHER TIME
                SUBB        ,S+             ; SUBTRACT THE GRANULE COUNTER FROM THE CURRENT GRANULE NUMBER
                BPL         LC7B8           ; BRANCH IF LOWER BOUND NOT EXCEEDED
                LDB         ,S              ; RESTORE CURRENT GRANULE NUMBER IF LOWER BOUND EXCEEDED
LC7B6           COM         $01,S           ; COMPLEMENT FLAG - IF GRANULE NUMBER HAS EXCEEDED
; BOUNDS ON EITHER THE HI OR LO SIDE, FORCE IT TO GO IN
; THE DIRECTION OPPOSITE THE EXCEEDED BOUND
LC7B8           LEAS        $01,S           ; CLEAN UP STACK
                BRA         LC798           ; CHECK FOR ANOTHER FREE GRANULE

LC7BC           ADDB        ,S+             ; ADD THE GRANULE COUNTER TO THE CURRENT GRANULE NUMBER
                CMPB        #GRANMX         ; GRANMX GRANULES PER DISK
                BLO         LC7B8           ; BRANCH IF UPPER BOUND NOT EXCEEDED
                LDB         ,S              ; RESTORE CURRENT GRANULE COUNT AND GO TWICE
                SUBB        #$04            ; AS FAR AS USUAL IN OPPOSITE DIRECTION IF UPPER BOUND EXCEEDED
                BRA         LC7B6           ; KEEP SEARCHING
LC7C8           LDB         #2*28           ; 'DISK FULL' ERROR
                JMP         LAC46           ; JUMP TO ERROR HANDLER

; POINT X TO FIRST FREE GRANULE POSITION IN THE FILE ALLOCATION
; TABLE AND MARK THE POSITION WITH A LAST GRANULE IN FILE MARKER
LC7CD           LEAS        $01,S           ; CLEAR UP STACK - REMOVE DIRECTION FLAG
                TFR         B,A             ; GRANULE NUMBER TO ACCA
                ABX                         ; POINT X TO FIRST FOUND GRANULE
                LDB         #$C0            ; LAST GRANULE FLAG
                STB         ,X              ; MARK THE FIRST FOUND GRANULE AS THE LAST GRANULE
LC7D6           RTS

; CHECK ALL ACTIVE FILES TO MAKE SURE A FILE IS NOT ALREADY OPEN - TO BE OPEN
; A FILE BUFFER MUST MATCH THE DRIVE NUMBER AND FIRST GRANULE NUMBER
; IN RAM DIRECTORY ENTRY AND THE FCB TYPE MUST NOT MATCH THE FILE TYPE IN ACCA
; AN 'AO' ERROR WILL NOT BE GENERATED IF A FILE IS BEING OPENED FOR
; THE SAME MODE THAT IT HAS ALREADY BEEN OPENED UNDER.

LC7D7           PSHS        A               ; SAVE FILE TYPE ON STACK
                LDB         FCBACT          ; NUMBER OF CURRENTLY OPEN FILES
                INCB                        ; ADD ONE MORE TO FILE COUNTER
LC7DD           JSR         LC719           ; POINT X TO FCB OF THIS FILE
                BEQ         LC7F9           ; BRANCH IF BUFFER NOT BEING USED
                LDA         DCDRV           ; GET DRIVE NUMBER AND CHECK TO SEE IF IT
                CMPA        FCBDRV,X        ; MATCHES THE DRIVE NUMBER FOR THIS BUFFER
                BNE         LC7F9           ; FILE EXISTS ON ANOTHER DRIVE
                LDU         V974            ; GET RAM DIRECTORY AREA
                LDA         DIRGRN,U        ; GET FIRST GRANULE IN FILE
                CMPA        FCBFGR,X        ; DOES IT MATCH THIS FILE BUFFER?
                BNE         LC7F9           ; NO
                LDA         FCBTYP,X        ; GET FILE TYPE OF THIS BUFFER
                CMPA        ,S              ; DOES IT MATCH THE ONE WE ARE LOOKING FOR?
                LBNE        LA61C           ; 'FILE ALREADY OPEN' ERROR IF NOT
LC7F9           DECB                        ; DECR FILE COUNTER
                BNE         LC7DD           ; BRANCH IF HAVEN'T CHECKED ALL ACTIVE FILES
                PULS        A,PC            ; RESTORE FILE TYPE AND RETURN

LC7FE           JSR         LA5A5           ; EVALUATE AN EXPRESSION (DEVICE NUMBER)
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                TSTB                        ; TEST NEW DEVICE NUMBER
                LBLE        LB44A           ; 'FC' ERROR IF DEVICE NUMBER NOT A DISK FILE
                JSR         LC719           ; POINT X TO FCB
                LDA         FCBTYP,X        ; TEST IF BUFFER IS IN USE
                LBEQ        LA3FB           ; 'FILE NOT OPEN' ERROR
                CMPA        #RANFIL         ; DIRECT/RANDOM FILE?
                BEQ         LC7D6           ; RETURN IF RANDOM
LC815           JMP         LA616           ; BAD FILE MODE ERROR IF NOT RANDOM

; INPUT DEVICE NUMBER CHECK RAM HOOK
DVEC5           LDA         #INPFIL         ; INPUT FILE TYPE
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)

; PRINT DEVICE NUMBER CHECK RAM HOOK
DVEC6           LDA         #OUTFIL         ; OUTPUT FILE TYPE
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN IF
                BLE         LC7D6           ; NOT A DISK FILE
                STX         ,S              ; = REPLACE SUBROUTINE RETURN ADDRESS WITH X REGISTER -
; = THIS IS THE SAME AS LEAS 2,S AND PSHS X
                JSR         LC714           ; POINT X TO FCB
                PSHS        A               ; SAVE FILE TYPE ON STACK
                LDA         FCBTYP,X        ; GET FILE TYPE
                LBEQ        LA3FB           ; 'FILE NOT OPEN' ERROR
                CMPA        #RANFIL         ; RANDOM FILE?
                BEQ         LC836           ; BRANCH IF RANDOM FILE
                CMPA        ,S              ; IS THIS FCB OF THE PROPER TYPE?
                BNE         LC815           ; 'FILE MODE' ERROR IF NOT
LC836           PULS        A,X,PC          ; RETURN
; DEVICE NUMBER VALIDITY CHECK RAM HOOK
DVEC1           BLE         LC7D6           ; RETURN IF NOT A DISK FILE
                CMPB        FCBACT          ; COMPARE DEVICE NUMBER TO HIGHEST POSSIBLE
                LBHI        LA61F           ; 'DEVICE NUMBER' ERROR IF TOO BIG
                PULS        X,PC            ; RETURN

; SET PRINT PARAMETERS RAM HOOK
DVEC2           TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                BLE         LC7D6           ; RETURN IF NOT DISK FILE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
                PSHS        X,B,A           ; SAVE REGISTERS
                CLR         PRTDEV          ; SET PRINT DEVICE NUMBER TO NON-CASSETTE
                JSR         LC714           ; POINT X TO FCB
                LDB         FCBPOS,X        ; GET PRINT POSITION
                CLRA                        ; PRINT WIDTH (256)
                LDX         #$1000          ; TAB FIELD WIDTH AND TAB ZONE
                JMP         LA37C           ; SAVE THE PRINT PARAMETERS

; BREAK CHECK RAM HOOK
DVEC11          TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN
                BLE         LC85F           ; IF NOT A DISK FILE
                LEAS        $02,S           ; = PURGE RETURN ADDRESS OFF OF THE STACK - DON'T
LC85F           RTS                         ; = DO A BREAK CHECK IF DISK FILE

; EOF RAM HOOK
DVEC14          LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
                LDA         DEVNUM          ; GET DEVICE NUMBER AND SAVE
                PSHS        A               ; IT ON THE STACK
                JSR         LA5AE           ; STRIP DEVICE NUMBER OFF OF INPUT LINE
                JSR         LA3ED           ; VERIFY THAT THE FILE TYPE WAS 'INPUT'
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                LBLE        LA5DA           ; BRANCH BACK TO BASIC'S EOF IF NOT DISK FILE
                JSR         LC714           ; POINT X TO FCB
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #RANFIL         ; RANDOM FILE?
                BEQ         LC815           ; 'BAD FILE MODE' ERROR IF RANDOM
                CLRB                        ; FILE NOT EMPTY FLAG - SET TO NOT EMPTY
                LDA         FCBCFL,X        ; CHECK THE CACHE FLAG - BRANCH IF
                BNE         LC884           ; THERE IS A CHARACTER WHICH HAS BEEN CACHED
                LDB         FCBDFL,X        ; GET SEQUENTIAL INPUT FILE STATUS
LC884           JMP         LA5E4           ; LINK BACK TO BASIC'S EOF STATEMENT

; GET FILENAME/EXTENSION: DRIVE NUMBER FROM BASIC
LC887           LDX         #DEFEXT         ; POINT TO ' ' BLANK (DEFAULT) EXTENSION
LC88A           CLR         ,-S             ; CLEAR A BYTE ON STACK FOR USE AS A DRIVES FLAG
                LDA         DEFDRV          ; GET DEFAULT DISK NUMBER
                STA         DCDRV           ; STORE IN DSKCON PARAMETER
                LDU         #DNAMBF         ; DISK FILENAME BUFFER
                LDD         #$2008          ; STORE 8 BLANKS IN RAM (DEFAULT FILE NAME)
LC897           STA         ,U+             ; STORE A BLANK IN FILE NAME
                DECB                        ; DECREMENT COUNTER
                BNE         LC897           ; BRANCH IF NOT DONE
                LDB         #$03            ; 3 BYTES IN EXTENSION
                JSR         LA59A           ; MOVE B BYTES FROM (X) TO (U)
                JSR         L8748           ; EVALUATE A STRING EXPRESSION
                LEAU        ,X              ; POINT U TO START OF STRING
                CMPB        #$02            ; CHECK LENGTH OF STRING AND
                BLO         LC8BC           ; BRANCH IF < 2
                LDA         $01,U           ; = GET 2ND CHARACTER IN STRING AND
                CMPA        #':'            ; = CHECK FOR COLON
                BNE         LC8BC           ; BRANCH IF NO DRIVE NUMBER
                LDA         ,U              ; GET 1ST CHARACTER
                CMPA        #'0'            ; IN STRING AND
                BLO         LC8BC           ; CHECK TO SEE
                CMPA        #'3'            ; IF IT IS IN
                BHI         LC8BC           ; THE RANGE 0-3
                BSR         LC8EF           ; GET DRIVE NUMBER
LC8BC           LDX         #DNAMBF         ; POINT X TO FILE NAME BUFFER
                INCB                        ; COMPENSATE FOR DECB BELOW
LC8C0           DECB                        ; DECREMENT STRING LENGTH
                BNE         LC8CF           ; BRANCH IF MORE CHARACTERS IN STRING
                LEAS        $01,S           ; CLEAN UP STACK - REMOVE DRIVE FLAG
LC8C5           CMPX        #DNAMBF         ; POINTER STILL AT START OF BUFFER?
                BNE         LC931           ; RETURN IF NOT
LC8CA           LDB         #2*31           ; 'BAD FILENAME' ERROR IF NULL FILENAME
                JMP         LAC46           ; ERROR HANDLER
LC8CF           LDA         ,U+             ; GET A CHARACTER FROM STRING
                CMPA        #'.'            ; LOOK FOR PERIOD?
                BEQ         LC902           ; YES
                CMPA        #'/'            ; SLASH?
                BEQ         LC902           ; YES
                CMPA        #':'            ; COLON?
                BEQ         LC8E6           ; YES
                CMPX        #DEXTBF         ; COMPARE POINTER TO END OF FILENAME BUFFER
                BEQ         LC8CA           ; 'BAD FILENAME' ERROR - FILENAME TOO LONG
                BSR         LC922           ; PUT A CHARACTER IN FILENAME
                BRA         LC8C0           ; GET ANOTHER CHARACTER FROM STRING
LC8E6           BSR         LC8C5           ; 'BAD FILENAME' ERROR IF NO FILENAME YET
                BSR         LC8EF           ; GET DRIVE NUMBER
                TSTB                        ;  CHECK LENGTH OF STRING
                BNE         LC8CA           ; 'BAD FILENAME' ERROR IF MORE CHARACTERS LEFT
LC8ED           PULS        A,PC            ; REMOVE DRIVES FLAG FROM STACK AND RETURN

; GRAB DRIVE NUMBER
LC8EF           COM         $02,S           ; TOGGLE DRIVE FLAG
                BEQ         LC8CA           ; 'BAD FILENAME' ERROR IF DRIVE NUMBER DEFINED TWICE
                LDA         ,U++            ; ASCII VALUE OF DRIVE NUMBER TO ACCA
                SUBB        #$02            ; DECREMENT STRING LENGTH BY 2 FOR DRIVE (:X)
                SUBA        #'0'            ; SUBTRACT ASCII BIAS
                BLO         LC8CA           ; DRIVE NUMBER TOO LOW - 'BAD FILENAME' ERROR
                CMPA        #$03            ; MAX OF 4 DRIVES
                BHI         LC8CA           ; DRIVE NUMBER TOO HIGH - 'BAD FILENAME' ERROR
                STA         DCDRV           ; STORE IN DSKCON DRIVE NUMBER
                RTS

; GRAB EXTENSION
LC902           BSR         LC8C5           ; 'BAD FILENAME' ERROR IF NO FILENAME YET
                LDX         #DNAMBF+11      ; POINT X TO END OF EXTENSION BUFFER
                LDA         #SPACE          ; BLANK
LC909           STA         ,-X             ;
                CMPX        #DEXTBF         ; FILL EXTENSION WITH
                BNE         LC909           ; BLANKS (DEFAULT)
LC910           DECB                        ; DECREMENT STRING COUNTER
                BEQ         LC8ED           ; RETURN IF ZERO
                LDA         ,U+             ; GET A CHARACTER FROM STRING
                CMPA        #':'            ; CHECK FOR DRIVE SEPARATOR
                BEQ         LC8E6           ;
                CMPX        #DNAMBF+11      ; =CHECK FOR END OF ESTENSION RAM BUFFER &
                BEQ         LC8CA           ; ='BAD FILENAME' ERROR IF EXTENSION TOO LONG
                BSR         LC922           ; PUT A CHARACTER IN EXTENSION BUFFER
                BRA         LC910           ; GET ANOTHER EXTENSION CHARACTER

; INSERT CHARACTER INTO FILENAME OR EXTENSION
LC922           STA         ,X+             ; STORE CHARACTER IN FILENAME BUFFER
                BEQ         LC8CA           ; 'BAD FILENAME' ERROR; ZEROES ARE ILLEGAL
                CMPA        #'.'            ; PERIOD?
                BEQ         LC8CA           ; 'BAD FILENAME' ERROR IF PERIOD
                CMPA        #'/'            ; SLASH?
                BEQ         LC8CA           ; 'BAD FILENAME' ERROR IF SLASH
                INCA                        ; CHECK FOR $FF
                BEQ         LC8CA           ; 'BAD FILENAME' ERROR IF $FF
LC931           RTS

; SAVE COMMAND
SAVE            CMPA        #'M'            ;
                LBEQ        LCE8C           ; BRANCH IF SAVEM
                BSR         LC985           ; GO GET FILENAME, ETC. FROM BASIC
                LDX         ZERO            ; ZERO OUT X REG
                STX         DFLTYP          ; SET FILE TYPE AND ASCII FLAG TO ZERO
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER FROM BASIC
                BEQ         LC964           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDB         #'A'            ; ASCII FILE?
                JSR         LB26F           ; SYNTAX CHECK ON CONTENTS OF ACCB
                BNE         LC931           ; RETURN IF NO MORE CHARACTERS ON LINE
                COM         DASCFL          ; SET CRUNCHED/ASCII FLAG TO ASCII
                BSR         LC956           ; OPEN A SEQUENTIAL FILE FOR OUTPUT
                CLRA                        ; SET ZERO FLAG - CAUSE ENTIRE FILE TO BE LISTED
                JMP         LIST            ; 'LIST' THE FILE TO CONSOLE OUT

; OPEN A SEQUENTIAL FILE FOR INPUT/OUTPUT - USE THE SYSTEM
; FCB LOCATED AT THE TOP OF FCBS
LC956           LDA         #'O'            ; OUTPUT FILE TYPE
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
LC959           LDA         #'I'            ; INPUT FILE TYPE
                LDB         FCBACT          ; GET NUMBER OF RESERVED FILES CURRENTLY RESERVED
                INCB                        ; ADD ONE - USE ONE ABOVE HIGHEST RESERVED FCB
                STB         DEVNUM          ; SAVE IT IN DEVICE NUMBER
                JMP         LC468           ; OPEN A FILE & INITIALIZE FCB
; SAVE A CRUNCHED FILE - A PREAMBLE OF THREE BYTES WILL PRECEED CRUNCHED
; FILES: BYTE 1 = $FF, 2,3 = LENGTH OF BASIC PROGRAM
LC964           BSR         LC956           ; OPEN A SEQUENTIAL FILE FOR OUTPUT
                LDA         #$FF            ; BASIC FILE FLAG
                JSR         LCB52           ; CONSOLE OUT
                LDD         VARTAB          ; LOAD ACCD WITH START OF VARIABLES
                SUBD        TXTTAB          ; SUBTRACT START OF BASIC
                JSR         LCB52           ; CONSOLE OUT FILE LENGTH MS BYTE
                TFR         B,A             ; PULL LS BYTE INTO ACCA
                JSR         LCB52           ; CONSOLE OUT FILE LENGTH LS BYTE
                LDX         TXTTAB          ; POINT X TO START OF BASIC
LC979           LDA         ,X+             ; GET BYTE FROM BASIC
                JSR         LCB52           ; SEND TO CONSOLE OUT
                CMPX        VARTAB          ; COMPARE TO END OF BASIC
                BNE         LC979           ; KEEP GOING IF NOT AT END
                JMP         LA42D           ; CLOSE FILE
LC985           LDX         #BASEXT         ; POINT TO 'BAS' EXTENSION (DEFAULT)
                JMP         LC88A           ; GET FILENAME.EXT FROM BASIC

; MERGE COMMAND
MERGE           CLRA                        ; RUN FLAG (0 = DON'T RUN)
                LDB         #$FF            ; MERGE FLAG ($FF = MERGE)
                BRA         LC9A2           ; GO LOAD THE FILE

; RUN RAM VECTOR
DVEC18          CMPA        #'"'            ; CHECK FOR FILENAME DELIMITER (DOUBLE QUOTE)
                LBNE        XVEC18          ; NONE - JUMP TO EXBAS RUN RAM HOOK
                LDA         #$02            ; RUN FLAG - DON'T CLOSE ALL FILES BEFORE RUN
                BRA         LC9A1           ; LOAD THE FILE

; LOAD COMMAND
LOAD            CMPA        #'M'            ;
                LBEQ        LCEE5           ; BRANCH IF LOADM
                CLRA                        ; RUN FLAG = ZERO (DON'T RUN)
LC9A1           CLRB                        ; CLEAR MERGE FLAG
LC9A2           STA         DRUNFL          ; RUN FLAG (0 = DON'T RUN, 2 = RUN)
                STB         DMRGFL          ; MERGE FLAG (0 = NO MERGE, $FF = MERGE)
                BSR         LC985           ; GO GET FILENAME, ETC. FROM BASIC
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                BEQ         LC9BE           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDB         #'R'            ;
                JSR         LB26F           ; IS NEXT CHAR 'R'? RUN AFTER LOAD
                JSR         LA5C7           ; SYNTAX ERROR IF ANY MORE CHARS ON LINE
                LDA         #$03            ; SET FLAGS TO RUN AND CLOSE ALL FILES
                STA         DRUNFL          ; BEFORE THE FILE IS RUN
LC9BE           BSR         LC959           ; GRAB FCB FOR INPUT FILE
                LDA         DASCFL          ; CHECK ASCII FLAG AND BRANCH
                BEQ         LC9D0           ; IF CRUNCHED BASIC FILE
                TST         DMRGFL          ; IS THIS A MERGE?
                BNE         LC9CD           ; BRANCH IF MERGE
                JSR         LAD19           ; DO A 'NEW' - ERASE VARIABLES, RESET VARIABLES
LC9CD           JMP         LAC7C           ; GO TO BASIC'S MAIN LOOP, IT WILL LOAD PROGRAM

; LOAD IN A CRUNCHED BASIC FILE
LC9D0           LDA         DFLTYP          ; CHECK FILE TYPE (MUST BE BASIC:0) & CHECK
                ORA         DMRGFL          ; MERGE FLAG (MUST BE NO MERGE: 0)
                LBNE        LA616           ; 'BAD FILE MODE' ERROR IF MERGE OR NON-BASIC
                JSR         LAD19           ; DO A 'NEW' - RESET POINTERS, ERASE VARIABLES
                COM         DLODFL          ; SET THE LOAD FLAG TO $FF - THIS WILL CAUSE A NEW TO
; OCCUR IF AN ERROR OCCURS WHILE THE PROGRAM IS BEING LOADED
                JSR         LCCE2           ; GET CHAR FROM BUFFER - SHOULD BE $FF
                JSR         LCCE2           ; GET ANOTHER - MS BYTE OF LENGTH
                PSHS        A               ; SAVE MS BYTE ON STACK
                JSR         LCCE2           ; LS BYTE OF LENGTH OF PROGRAM
                TFR         A,B             ; PUT LS BYTE INTO ACCB
                PULS        A               ; NOW ACCD CONTAINS LENGTH OF PROGRAM
                ADDD        TXTTAB          ; ADD BEGINNING OF BASIC
                JSR         LAC37           ; SEE OF ENOUGH ROOM IN RAM FOR THIS FILE
                LDX         TXTTAB          ; GET START OF BASIC
LC9F6           JSR         LC597           ; READ A CHAR FROM CONSOLE IN
                LDB         CINBFL          ; BUFFER EMPTY?
                BNE         LCA01           ; BRANCH IF SO
                STA         ,X+             ; STORE CHAR
                BRA         LC9F6           ; GET ANOTHER CHARACTER

LCA01           CLR         DLODFL          ; CLEAR LOAD FLAG - LOAD WAS ERROR FREE
                STX         VARTAB          ; SAVE NEW START OF VARIABLES
; MAKE SURE LAST THREE BYTES LOADED WERE ZERO
                LDB         #$03            ; CHECK THREE BYTES
LCA08           LDA         ,-X             ; CHECK A BYTE
                BNE         LCA0F           ; BRANCH IF NON-ZERO
                DECB                        ; DECREMENT COUNTER
                BNE         LCA08           ; KEEP CHECKING IF NOT DONE
LCA0F           LDX         VARTAB          ; GET START OF VARIABLES
LCA11           STX         VARTAB          ; SAVE START OF VARIABLES
                CLR         ,X+             ; CLEAR A BYTE
                DECB                        ; DECREMRNT COUNTER
                BPL         LCA11           ; KEEP CLEARING BYTES IF NOT DONE
LCA18           JSR         LA42D           ; CLOSE SELECTED FILE
                JSR         LAD21           ; DO PART OF NEW - ERASE VARIABLES, RESET INPUT PTR
                JSR         XVEC18          ; INITIALIZE EXBAS GRAPHICS VARIABLES
                JSR         LACEF           ; RELOCATE ALL THE BASIC NEXT LINE POINTERS
                ASR         DRUNFL          ; CHECK LSB OF RUN FLAG
                BLO         LCA2C           ; BRANCH IF DON'T CLOSE ALL FILES
                JSR         LA426           ; CLOSE ALL FILES
LCA2C           ASR         DRUNFL          ; TEST BIT 1 OF RUN FLAG
                LBCS        LAD9E           ; BRANCH TO COMM INTERPRETATION LOOP IF BIT 1 SET
                JMP         LAC73           ; RETURN TO DIRECT MODE

DVEC13          TST         DEVNUM          ; CHECK DEVICE NUMBER AND
                BGT         LCA18           ; TRY TO RUN FILE IF IT IS A DISK FILE
                RTS

; CLOSE ALL FILE BUFFERS RAM VECTOR
DVEC7           LDB         FCBACT          ; GET THE NUMBER OF RESERVED FILE BUFFERS
                INCB                        ; ADD ONE
LCA3F           PSHS        B               ; SAVE IT
                STB         DEVNUM          ; STORE IT IN DEVICE NUMBER
                BSR         LCA53           ; CLOSE FILE
                PULS        B               ; GET BACK NUMBER OF FILE BUFFERS
                DECB                        ; DECREMENT FILE BUFFER COUNTER
                BNE         LCA3F           ; BRANCH IF ALL FILES NOT CLOSED
LCA4A           RTS

; CLOSE FILE RAM HOOK
DVEC8           TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN
                LBLE        XVEC8           ; IF NOT A DISK FILE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
LCA53           JSR         LC714           ; POINT X TO CORRECT FCB
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
LCA58           STX         FCBTMP          ; SAVE FILE BUFFER POINTER
                LDA         FCBTYP,X        ; GET THE TYPE OF THIS FILE
                BEQ         LCA4A           ; RETURN IF FILE NOT OPEN
                PSHS        A               ; SAVE FILE TYPE
                CLR         FCBTYP,X        ; CLOSE THE FILE - ZERO OUT THE FILE TYPE
                LDB         FCBDRV,X        ; GET DRIVE NUMBER AND
                STB         DCDRV           ; SAVE IT IN DSKCON VARIABLE
                CMPA        #OUTFIL         ; = CHECK FOR OUTPUT TYPE AND
                BNE         LCA80           ; = BRANCH IF NOT OUTPUT TYPE FILE

; CLOSE A SEQUENTIAL OUTPUT FILE
                LDB         FCBLFT,X        ; GET THE NUMBER OF CHARACTERS IN BUFFER
                LDA         #$80            ; SET THE PRE-SAVED BIT TO INDICATE THAT THE DATA
; HAS ALREADY BEEN SAVED ON DISK
                STD         FCBLST,X        ; SAVE THE NUMBER OF BYTES USED IN THE LAST SECTOR
                INC         FCBSEC,X        ; INCREMENT THE SECTOR NUMBER
                LDB         FCBCGR,X        ; GET THE CURRENT GRANULE NUMBER
                JSR         LC725           ; POINT X TO FILE ALLOCATION TABLE
                STA         FAT1,X          ; SET FAT DATA NOT VALID FLAG (ACCA <> 0)
                ABX                         ; ADD GRANULE OFFSET TO FAT POINTER
                INC         FATCON,X        ; INCREMENT GRANULE DATA (ADD ONE SECTOR TO LAST
; GRANULE) SKIP PAST THE SIX FAT CONTROL BYTES
                BRA         LCAF1           ; UPDATE FAT AND DIRECTORY
LCA80           CMPA        #RANFIL         ; RANDOM FILE?
                BNE         LCAF1           ; NO - UPDATE FAT AND DIRECTORY

; CLOSE A RANDOM FILE
                LDD         FCBRLN,X        ; GET RECORD LENGTH
                LDX         FCBBUF,X        ; POINT X TO RANDOM FILE BUFFER
                LEAY        D,X             ; POINT Y TO END OF RANDOM FILE BUFFER
                PSHS        Y,X,B,A         ; SAVE POINTERS ON STACK
                LDU         VARTAB          ; GET START OF VARIABLES
LCA8E           CMPU        ARYTAB          ; COMPARE TO START OF ARRAYS
                BEQ         LCAA1           ; BRANCH IF ALL VARIABLES CHECKED
                LDA         $01,U           ; GET 2ND BYTE OF VARIABLE NAME
                LEAU        $02,U           ; MOVE POINTER TO START OF DESCRIPTOR
                BPL         LCA9B           ; BRANCH IF VARIABLE - NUMERIC
                BSR         LCAC3           ; ADJUST STRING VARIABLE IF IN RANDOM FILE BUFFER
LCA9B           LEAU        $05,U           ; MOVE POINTER TO NEXT VARIABLE
                BRA         LCA8E           ; PROCESS ANOTHER VARIABLE
LCA9F           PULS        U               ; GET ADDRESS OF NEXT ARRAY TO U
LCAA1           CMPU        ARYEND          ; COMPARE TO END OF ARRAYS
                BEQ         LCAE0           ; BRANCH IF END OF ARRAYS
                TFR         U,D             ; SAVE ARRAY START IN ACCD, ADD OFFSET
                ADDD        $02,U           ; TO NEXT ARRAY AND SAVE ADDRESS OF
                PSHS        B,A             ; NEXT ARRAY ON THE STACK
                LDA         $01,U           ; GET 2ND LETTER OF VARIABLE NAME
                BPL         LCA9F           ; BRANCH IF NUMERIC
                LDB         $04,U           ; GET THE NUMBER OF DIMENSIONS
                ASLB                        ; X2:2 BYTES PER DIMENSION
                ADDB        #$05            ; 5 BYTES CONSTANT PER ARRAY DESCRIPTOR
                CLRA                        ; CLEAR MSB OF OFFSET - (ONLY 125 DIMENSIONS ALLOWED)
                LEAU        D,U             ; POINT U TO START OF THIS ARRAY'S VARIABLES
LCAB8           CMPU        ,S              ; AT END OF THIS ARRAY?
                BEQ         LCA9F           ; YES
                BSR         LCAC3           ; ADJUST STRING VARIABLE IF IN RANDOM FILE BUFFER
                LEAU        $05,U           ; MOVE POINTER TO NEXT DESCRIPTOR
                BRA         LCAB8           ; CHECK NEXT VARIABLE
;
; CHECK TO SEE IF A STRING IS LOCATED IN THE RANDOM FILE BUFFER AREA. IF IT IS
; THE RANDOM FILE BUFFER IN QUESTION, IT WILL BE DELETED. IF IT IS HIGHER IN THE RANDOM
; FILE BUFFER SPACE THAN THE BUFFER IN QUESTION, THE LENGTH OF THE CURRENT
; BUFFER WILL BE SUBTRACTED FROM THE ADDRESS OF THE STRING BECAUSE THE CURRENT
; BUFFER IS BEING DELETED (CLOSED).
LCAC3           LDX         $02,U           ; POINT X TO START OF STRING
                CMPX        RNBFAD          ; COMPARE TO START OF FREE RANDOM FILE BUFFER AREA
                BHS         LCAD8           ; RETURN IF > START OF FREE RANDOM FILE BUFFER AREA
                CMPX        $04,S           ; COMPARE TO START OF THIS FILE'S RANDOM BUFFER
                BLO         LCAD9           ; BRANCH IF < START OF THIS FILE'S RANDOM BUFFER
; BUG ; * THIS SHOULD BE A BRANCH TO A RETURN
                CMPX        $06,S           ; COMPARE TO END OF THIS FILE'S RANDOM BUFFER
                BLO         LCAD9           ; BRANCH IF < END OF THIS FILE'S RANDOM BUFFER
                TFR         X,D             ; SAVE POINTER IN ACCD
                SUBD        $02,S           ; SUBTRACT RECORD LENGTH FROM START OF STRING ADDRESS
                STD         $02,U           ; SAVE NEW START OF STRING ADDRESS
LCAD8           RTS
LCAD9           CLR         ,U              ; CLEAR THE LENGTH OF THE STRING
                CLR         $02,U           ; CLEAR THE ADDRESS
                CLR         $03,U           ; OF THE STRING
                RTS
; REMOVE RESERVED SPACE IN RANDOM FILE BUFFER FOR A 'CLOSED' RANDOM FILE
LCAE0           PULS        A,B,X,U         ; U = END OF RANDOM FILE BUFFER, X = START OF RANDOM
; FILE BUFFER, ACCD = RECORD LENGTH

; THIS WOULD PROBABLY BE THE MOST CONVENIENT PLACE TO FIX THE BUG WHICH
; CAUSES THE SYSTEM TO HANG IF AN ERROR IS ENCOUNTERED DURING 'COPY'

; CMPU FCBADR ; IS THE END OF THIS FCB'S BUFFER ABOVE THE END
; OF THE START OF THE FCB AREA
; BLO LCAE2 NO - FREE UP THE SPACE USED BY THIS FILE IN RANDOM BUFFER
; LDX #DFLBUF YES - DOING A 'COPY'; RESET START OF RANDOM BUFFER
; BRA LCAEE
; RANDOM FILE BUFFER AREA

; REMOVE RESERVED SPACE FOR CLOSED FILE FROM RANDOM FILE BUFFER SPACE
LCAE2           CMPU        RNBFAD          ; AT THE BOTTOM OF FREE RANDOM BUFFER AREA?
                BEQ         LCAEE           ; BRANCH IF THERE
                LDA         ,U+             ; = GRAB A SOURCE BYTE AND
                STA         ,X+             ; = MOVE IT TO DESTINATION
                BRA         LCAE2           ; KEEP MOVING BYTES
LCAEE           STX         RNBFAD          ; SAVE NEW START OF FREE RANDOM BUFFER AREA
LCAF1           JSR         LC725           ; POINT X TO PROPER FILE ALLOCATION TABLE
                DEC         FAT0,X          ; REMOVE ONE ACTIVE FILE
                TST         FAT1,X          ; NEW DATA IN FAT RAM IMAGE?
                BEQ         LCAFD           ; NO
                JSR         LC6F1           ; WRITE OUT FILE ALLOCATION TABLE TO DISK
LCAFD           LDX         FCBTMP          ; GET FILE BUFFER POINTER
                PULS        A               ; GET FILE TYPE
                CMPA        #OUTFIL         ; IS IT A SEQUENTIAL OUTPUT FILE?
                BEQ         LCB0D           ; YES
                CMPA        #RANFIL         ; IS IT A RANDOM FILE?
                BNE         LCAD8           ; RETURN IF NOT A RANDOM FILE (SEQUENTIAL INPUT)
                LDA         FCBFLG,X        ; TEST THE GET/PUT FLAG AND
                BEQ         LCB17           ; BRANCH IF 'GET'

; WRITE CONTENTS OF FILE BUFFER TO DISK
LCB0D           JSR         LC733           ; GET PROPER TRACK & SECTOR NUMBERS
                LEAU        FCBCON,X        ; POINT U TO START OF FCB DATA
                STU         DCBPT           ; SET UP FILE BUFFER POINTER FOR DSKCON
                BSR         LCB43           ; GO WRITE A SECTOR
LCB17           LDA         FCBLST,X        ; CHECK THE PRE-SAVED FLAG
                BPL         LCAD8           ; RETURN IF RECORD HAS ALREADY BEEN SAVED ON DISK
                LDB         FCBDIR,X        ; GET DIRECTORY NUMBER OF THIS FILE
                ANDB        #$07            ; 8 ENTRIES PER SECTOR
                LDA         #DIRLEN         ; DIRLEN BYTES PER DIRECTORY ENTRY
                MUL                         ; GET SECTOR OFFSET FOR THIS ENTRY
                LDU         #DBUF0          ; GET READ/WRITE BUFFER 0 AND
                STU         DCBPT           ; SAVE IT IN DSKCON REGISTER
                LEAY        D,U             ; Y POINTS TO CORRECT DIRECTORY ENTRY
                LDB         FCBDIR,X        ; GET DIRECTORY ENTRY NUMBER
                LSRB                        ;
                LSRB                        ;
                LSRB                        ;  DIVIDE BY 8 EIGHT DIRECTORY ENTRIES PER SECTOR
                ADDB        #$03            ; ADD BIAS; FIRST 3 SECTORS NOT DIRECTORY
                STB         DSEC            ; STORE SECTOR NUMBER
                LDD         #$1102          ; DIRECTORY TRACK - READ OP CODE
                STA         DCTRK           ; STORE TRACK NUMBER
                BSR         LCB45           ; GO READ DIRECTORY
                LDD         FCBLST,X        ; GET NUMBER OF BYTES IN THE LAST SECTOR
                ANDA        #$7F            ; MASK OFF THE PRE-SAVED FLAG
                STD         DIRLST,Y        ; SAVE NUMBER OF BYTES IN LAST SECTOR OF FILE IN DIRECTORY
LCB43           LDB         #$03            ; WRITE OP CODE
LCB45           STB         DCOPC           ; SAVE DSKCON OP CODE VARIABLE
                JMP         LD5FF           ; GO READ/WRITE SECTOR

; CONSOLE OUT RAM HOOK
DVEC3           TST         DEVNUM          ; CHECK DEVICE NUMBER
                LBLE        XVEC3           ; BRANCH TO EX BASIC IF NOT A DISK FILE
                LEAS        $02,S           ; POP RETURN OFF STACK
; SEND A CHARACTER IN ACCA TO A DISK FILE. A CARRIAGE RETURN WILL RESET THE
; PRINT POSITION AND CONTROL CODES WILL NOT INCREMENT THE PRINT POSITION.
LCB52           PSHS        X,B,A           ; SAVE REGISTERS
                LDX         #FCBV1-2        ; POINT X TO TABLE OF FILE NUMBER VECTORS
                LDB         DEVNUM          ; GET CURRENT FILE NUMBER
                ASLB                        ; 2 BYTES PER FCB ADDRESS
                LDX         B,X             ; POINT X TO PROPER FCB
                LDB         ,X              ; GET FILE TYPE (FCBTYP,X)
                CMPB        #INPFIL         ; IS IT AN INPUT FILE?
                BEQ         LCB98           ; RETURN IF SO
                CMPA        #CR             ; CARRIAGE RETURN (ENTER)
                BNE         LCB68           ; NO
                CLR         FCBPOS,X        ; CLEAR PRINT POSITION IF CARRIAGE RETURN
LCB68           CMPA        #SPACE          ;
                BLO         LCB6E           ; BRANCH IF CONTROL CHAR
                INC         FCBPOS,X        ; INCREMENT PRINT POSITION
LCB6E           CMPB        #RANFIL         ; IS IT RANDOM FILE?
                BNE         LCB8C           ; BRANCH IF NOT RANDOM
; PUT A BYTE INTO A RANDOM FILE
                LDD         FCBPUT,X        ; GET 'PUT' BYTE COUNTER
                ADDD        #$0001          ; ADD ONE
                CMPD        FCBRLN,X        ; COMPARE TO RECORD LENGTH
                LBHI        LCCF1           ; 'FR' ERROR IF 'PUT' BYTE COUNTER > RECORD LENGTH
                STD         FCBPUT,X        ; SAVE NEW 'PUT' BYTE COUNTER
                LDX         FCBBUF,X        ; POINT TO RANDOM FILE BUFFER POINTER
                LEAX        D,X             ; POINT TO ONE PAST END OF CURRENT RECORD DATA
                PULS        A               ; PULL DATA FROM STACK
                STA         -1,X            ; STORE IN DATA BUFFER
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN

; WRITE A BYTE TO SEQUENTIAL OUTPUT FILE
LCB8C           INC         FCBLFT,X        ; INCREMENT CHARACTER COUNT
                LDB         FCBLFT,X        ; GET CHARACTER COUNT AND BRANCH
                BEQ         LCB9A           ; IF THE BUFFER IS FULL
                ABX                         ; ADD CHARACTER COUNT TO FCB ADDRESS
                STA         FCBCON-1,X      ; STORE NEW CHARACTER (SKIP PAST 25 CONTROL BYTES AT FCB START)
LCB98           PULS        A,B,X,PC

; WRITE OUT A FULL BUFFER AND RESET BUFFER
LCB9A           PSHS        U,Y             ; SAVE REGISTERS
                STA         SECLEN+FCBCON-1,X ; STORE LAST CHARACTER IN BUFFER
                JSR         LC658           ; INCREMENT RECORD NUMBER
                LDB         FCBDRV,X        ; GET DRIVE NUMBER AND SAVE
                STB         DCDRV           ; IT IN DSKCON CONTROL TABLE
                INC         FCBSEC,X        ; INCREMENT SECTOR NUMBER
                JSR         LCB0D           ; WRITE THE FILE BUFFER TO DISK
                LEAY        ,X              ; SAVE FCB POINTER IN Y
                LDB         FCBCGR,X        ; GET GRANULE NUMBER
                JSR         LC725           ; POINT X TO PROPER ALLOCATION TABLE
                ABX                         ; ADD THE GRANULE NUMBER TO FAT POINTER
                LEAU        FATCON,X        ; POINT U TO THE CORRECT GRANULE IN FAT - SKIP PAST THE SIX FAT CONTROL BYTES
                LDA         FCBSEC,Y        ; GET CURRENT SECTOR FOR THIS GRANULE
                CMPA        #$09            ; MAX SECTOR NUMBER (9 SECTORS/GRANULE)
                BLO         LCBC4           ; BRANCH IF NOT AT END OF GRANULE







                CLR         FCBSEC,Y        ; CLEAR SECTOR NUMBER
                JSR         LC78F           ; GET NEXT FREE GRANULE
                STA         FCBCGR,Y        ; SAVE NEW GRANULE IN FCB



                FCB         $8C             ; SKIP TWO BYTES NO DATA STORED IN NEW SECTOR YET (THROWN AWAY CMPX INSTRUCTION)
LCBC4           ORA         #$C0            ; FORCE GRANULE NUMBER TO BE FINAL GRANULE IN FILE
                STA         ,U              ; STORE IN MAP

                JSR         LC57C           ; UPDATE FILE ALLOCATION TABLE

                PULS        Y,U             ; RESTORE REGISTERS
                PULS        A,B,X,PC        ; RESTORE REGISTERS AND RETURN

; DIR COMMAND
DIR             JSR         LD162           ; SCAN DRIVE NUMBER FROM INPUT LINE
                JSR         LC76D           ; GET FAT FOR THIS DRIVE
                JSR         LB958           ; PRINT CARRIAGE RETURN TO CONSOLE OUT
                LDD         #$1102          ; GET TRACK 17 AND
                STA         DCTRK           ; READ OP CODE AND
                STB         DCOPC           ; SAVE IN DSKCON VARIABLES
                LDB         #$03            ; START WITH SECTOR 3 (FIRST DIRECTORY SECTOR)

; READ A DIRECTORY SECTOR INTO THE I/O BUFFER
LCBE1           STB         DSEC            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                LDX         #DBUF0          ; USE I/O BUFFER 0 FOR DATA TRANSFER
                STX         DCBPT           ; SAVE IN DSKCON VARIABLE
                JSR         LD5FF           ; READ A SECTOR

; SEND DIRECTORY INFORMATION TO CONSOLE OUT
LCBEB           PULS        U               ; SAVE TOP OF STACK
                JSR         LA549           ; GO DO A BREAK CHECK
                PSHS        U               ; RESTORE STACK
                LDA         ,X              ; TEST FILE NAME FIRST BYTE (DIRNAM,X)
                BEQ         LCC2E           ; BRANCH IF KILLED
                COMA                        ; FF = END OF DIRECTORY
                BEQ         LCC3D           ; RETURN IF END OF DIRECTORY
                PSHS        X               ; SAVE DIRECTORY POINTER ON STACK
                LDB         #$08            ; NUMBER CHARACTERS TO PRINT
                JSR         LB9A2           ; SEND FILENAME TO CONSOLE OUT
                BSR         LCC41           ; SEND BLANK TO CONSOLE OUT
                LDB         #$03            ; NUMBER CHARACTERS TO PRINT
                JSR         LB9A2           ; SEND EXTENSION TO CONSOLE OUT
                BSR         LCC41           ; SEND BLANK TO CONSOLE OUT
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #10             ; CHECK THE NUMBER OF DECIMAL DIGITS IN
                BHS         LCC11           ; ACCB: IF THERE IS ONLY ONE DIGIT,
                BSR         LCC41           ; SEND BLANK TO CONSOLE OUT
LCC11           CLRA                        ; CLEAR MS BYTE OF ACCO
                JSR         LBDCC           ; PRINT ACCD IN DECIMAL TO CONSOLE OUT
                BSR         LCC41           ; SEND BLANK TO CONSOLE OUT
                LDX         ,S              ; X NOW POINTS TO DIRECTORY ENTRY
                LDA         #'A'+1          ; ASCII BIAS
                ADDA        DIRASC,X        ; ADD TO ASCII FLAG
                BSR         LCC3E           ; PRINT CHARACTER AND BLANK TO CONSOLE OUT
                LDB         DIRGRN,X        ; GET FIRST GRANULE IN FILE
                BSR         LCC44           ; COUNT GRANULES
                TFR         A,B             ; SAVE COUNT IN ACCB
                CLRA                        ; CLEAR MS BYTE OF ACCD
                JSR         LBDCC           ; PRINT ACCD IN DECIMAL TO CONSOLE OUT
                JSR         LB958           ; SEND CARRIAGE RETURN TO CONSOLE OUT
                PULS        X               ; PULL DIRECTORY POINTER OFF OF THE STACK
LCC2E           LEAX        DIRLEN,X        ; MOVE X TO NEXT DIRECTORY ENTRY
                CMPX        #DBUF0+SECLEN   ; END OF I/O BUFFER?
                BLO         LCBEB           ; BRANCH IF MORE DIRECTORY ENTRIES IN BUFFER
                LDB         DSEC            ; GET CURRENT SECTOR
                INCB                        ; BUMP COUNT
                CMPB        #SECMAX         ; SECMAX SECTORS IN DIRECTORY TRACK
                BLS         LCBE1           ; GET NEXT SECTOR
LCC3D           RTS                         ; FINISHED
LCC3E           JSR         PUTCHR          ; SEND CHARACTER TO CONSOLE OUT
LCC41           JMP         LB9AC           ; SEND BLANK TO CONSOLE OUT

; ENTER WITH ACCB POINTING TO FIRST GRANULE IN A FILE; RETURN THE NUMBER OF
; GRANULES IN THE FILE IN ACCA, THE GRANULE DATA FOR THE LAST SECTOR IN ACCB
LCC44           JSR         LC725           ; POINT X TO FILE ALLOCATION BUFFER
                LEAU        FATCON,X        ; POINT U TO START OF GRANULE DATA
                CLRA                        ; RESET GRANULE COUNTER
LCC4A           INCA                        ; INCREMENT GRANULE COUNTER
                CMPA        #GRANMX         ; CHECKED ALL 68 GRANULES?
                LBHI        LC626           ; YES - 'BAD FILE STRUCTURE' ERROR
                LEAX        ,U              ; POINT U TO START OF GRANULE DATA
                ABX                         ; ADD POINTER TO FIRST GRANULE
                LDB         ,X              ; GET THIS GRANULE'S CONTROL BYTE
                CMPB        #$C0            ; IS THIS THE LAST GRANULE IN FILE?
                BLO         LCC4A           ; NO - KEEP GOING
                RTS

; INPUT RAM HOOK
DVEC10          TST         DEVNUM          ; CHECK DEVICE NUMBER AND RETURN
                BLE         LCCBD           ; IF NOT A DISK FILE
                LDX         #LB069          ; = CHANGE THE RETURN ADDRESS ON THE STACK TO RE-ENTER BASIC'S INPUT
                STX         ,S              ; = ROUTINE AT A DIFFERENT PLACE THAN THE CALLING ROUTINE
                LDX         #LINBUF+1       ; POINT X TO THE LINE INPUT BUFFER
                LDB         #','            ; =
                STB         CHARAC          ; =COMMA IS READ ITEM SEPARATOR (TEMPORARY STRING SEARCH FLAG)
                LDA         VALTYP          ; GET VARIABLE TYPE AND BRANCH IF
                BNE         LCC71           ; IT IS A STRING
                LDB         #SPACE          ; SPACE = NUMERIC SEARCH DELIMITER
LCC71           BSR         LCCE2           ; GET AN INPUT CHARACTER
                CMPA        #SPACE          ; SPACE?
                BEQ         LCC71           ; YES - GET ANOTHER CHARACTER
                CMPA        #'"'            ; QUOTE?
                BNE         LCC85           ; NO
                CMPB        #','            ; SEARCH CHARACTER = COMMA?
                BNE         LCC85           ; NO - NUMERIC SEARCH
                TFR         A,B             ; SAVE DOUBLE QUOTE AS
                STB         CHARAC          ; THE SEARCH FLAG
                BRA         LCCA7           ; SAVE DOUBLE QUOTES AS FIRST ITEM IN BUFFER

LCC85           CMPB        #'"'            ;
                BEQ         LCC9A           ; BRANCH IF INPUTTING A STRING VARIABLE
                CMPA        #CR             ; IS THE INPUT CHARACTER A CARRIAGE RETURN
                BNE         LCC9A           ; NO
                CMPX        #LINBUF+1       ; IF AT THE START OF INPUTBUFFER, CHECK FOR A
                BEQ         LCCD6           ; FOLLOWING LINE FEED AND EXIT ROUTINE
                LDA         -1,X            ; =IF THE INPUT CHARACTER PRECEEDING THE CR WAS A LINE FEED,
                CMPA        #LF             ; =THEN INSERT THE CR IN THE INPUT STRING, OTHERWISE
                BNE         LCCD6           ; =CHECK FOR A FOLLOWING LINE FEED AND EXIT THE ROUTINE
                LDA         #CR             ; RESTORE CARRIAGE RETURN AS THE INPUT CHARACTER
LCC9A           TSTA                        ;  CHECK FOR A NULL (ZERO) INPUT CHARACTER AND
                BEQ         LCCB4           ; IGNORE IT IF lT IS A NULL
                CMPA        CHARAC          ; =
                BEQ         LCCBE           ; =CHECK TO SEE IF THE INPUT CHARACTER MATCHES
                PSHS        B               ; =EITHER ACCB OR CHARAC AND IF IT DOES, THEN
                CMPA        ,S+             ; =BRANCH TO CHECK FOR ITEM SEPARATOR OR
                BEQ         LCCBE           ; =TERMINATOR SEQUENCE AND EXIT ROUTINE
LCCA7           STA         ,X+             ; STORE NEW CHARACTER IN BUFFER
                CMPX        #LINBUF+LBUFMX  ; END OF INPUT BUFFER
                BNE         LCCB4           ; NO
                BSR         LCCF6           ; GET A CHARACTER FROM CONSOLE IN
                BNE         LCCB8           ; EXIT ROUTINE IF BUFFER EMPTY
                BRA         LCCD2           ; CHECK FOR CR OR CR/LF AND EXIT ROUTINE

LCCB4           BSR         LCCF6           ; GET A CHARACTER FROM CONSOLE IN
                BEQ         LCC85           ; BRANCH IF BUFFER NOT EMPTY
LCCB8           CLR         ,X              ; PUT A ZERO AT END OF BUFFER WHEN DONE
                LDX         #LINBUF         ; POINT (X) TO LINBUF - RESET POINTER
LCCBD           RTS

; CHECK FOR ITEM SEPARATOR OR TERMINATOR AND EXIT THE INPUT ROUTINE
LCCBE           CMPA        #'"'            ; QUOTE?
                BEQ         LCCC6           ; YES
                CMPA        #SPACE          ; SPACE?
                BNE         LCCB8           ; NO - EXIT ROUTINE
LCCC6           BSR         LCCF6           ; GET A CHARACTER FROM CONSOLE IN
                BNE         LCCB8           ; EXIT ROUTINE IF BUFFER EMPTY
                CMPA        #SPACE          ; SPACE?
                BEQ         LCCC6           ; YES - GET ANOTHER CHARACTER
                CMPA        #','            ; COMMA (ITEM SEPARATOR)?
                BEQ         LCCB8           ; YES - EXIT ROUTINE
LCCD2           CMPA        #CR             ; CARRIAGE RETURN?
                BNE         LCCDE           ; NO
LCCD6           BSR         LCCF6           ; GET A CHARACTER FROM CONSOLE IN
                BNE         LCCB8           ; EXIT ROUTINE IF BUFFER EMPTY
                CMPA        #LF             ; LINE FEED? TREAT CR,LF AS A CR
                BEQ         LCCB8           ; YES - EXIT ROUTINE
LCCDE           BSR         LCCFC           ; BACK UP PTR INPUT POINTER ONE
                BRA         LCCB8           ; EXIT ROUTINE

LCCE2           BSR         LCCF6           ; GET A CHAR FROM INPUT BUFFER - RETURN IN ACCA
                BEQ         LCCFB           ; RETURN IF BUFFER NOT EMPTY
                JSR         LC714           ; POINT X TO START OF FILE BUFFER
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #RANFIL         ; IS IT RANDOM FILE TYPE?
                LBNE        LC334           ; 'INPUT PAST END OF FILE ERROR IF NOT RANDOM
LCCF1           LDB         #2*37           ; 'WRITE/INPUT PAST END OF RECORD ERROR IF RANDOM
                JMP         LAC46           ; JUMP TO THE ERROR HANDLER

LCCF6           JSR         LA176           ; GET A CHAR FROM INPUT BUFFER
                TST         CINBFL          ; SET FLAGS ACCORDING TO CONSOLE INPUT FLAG
LCCFB           RTS

; MOVE THE INPUT POINTER BACK ONE (DISK FILE)
LCCFC           PSHS        X,B             ; SAVE REGISTERS ON STACK
                JSR         LC714           ; POINT X TO PROPER FCB
                LDB         FCBTYP,X        ; GET FILE TYPE OF THIS FCB
                CMPB        #RANFIL         ; IS IT A RANDOM FILE?
                BNE         LCD12           ; BRANCH IF NOT A RANDOM FILE
                LDD         FCBGET,X        ; GRAB THE RANDOM FILE 'GET' POINTER,
                SUBD        #$0001          ; MOVE IT BACK ONE AND RESTORE IT
                STD         FCBGET,X        ;
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN
LCD12           STA         FCBCDT,X        ; SAVE THE CHARACTER IN THE CACHE
                COM         FCBCFL,X        ; SET THE CACHE FLAG TO $FF - DATA IN CACHE
                PULS        B,X,PC          ; RESTORE REGISTERS AND RETURN

; CVN COMMAND
CVN             JSR         LB654           ; GET LENGTH AND ADDRESS OF STRING
                CMPB        #$05            ; FIVE BYTES IN A FLOATING POINT NUMBER
                LBCS        LB44A           ; 'FC' ERROR IF <> 5 BYTES
                CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                JMP         LBC14           ; COPY A PACKED FP NUMBER FROM (X) TO FPA0

; MKN$ COMMAND
MKN             JSR         LB143           ; 'TM' ERROR IF VALTYP=STRING
                LDB         #$05            ; FIVE BYTES IN A FLOATING POINT NUMBER
                JSR         LB50F           ; RESERVE FIVE BYTES IN STRING SPACE
                JSR         LBC35           ; PACK FPA0 AND STORE IT IN STRING SPACE
                JMP         LB69B           ; SAVE STRING DESCRIPTOR ON STRING STACK

; LOC COMMAND
LOC             BSR         LCD3D           ; POINT X TO FILE BUFFER
                LDD         FCBREC,X        ; GET RECORD NUMBER (RANDOM FILE) OR SECTOR CTR (SEQUENTIAL)
LCD3A           JMP         GIVABF          ; PUT ACCD IN FPA0

; STRIP A DEVICE NUMBER FROM A BASIC STATEMENT, SET PRINT
; PARAMETERS ACCORDING TO IT - ERROR IF FILE NOT
; OPEN. RETURN WITH (X) POINTING TO THAT FILE'S FCB
LCD3D           LDA         DEVNUM          ; GET CURRENT DEVICE NUMBER AND
                PSHS        A               ; SAVE IT ON THE STACK
                JSR         LB143           ; 'TM' ERROR IF VALTYP=STRING
                JSR         LA5AE           ; CHECK FOR VALID DEVICE NUMBER/SET PRINT PARAMETERS
                TST         DEVNUM          ; CHECK DEVICE NUMBER
                LBLE        LB44A           ; BRANCH IF NOT DISK FILE 'ILLEGAL FUNCTION CALL'
                JSR         LC714           ; POINT (X) TO FILE BUFFER
                PULS        A               ; GET OLD DEVICE NUMBER OFF OF THE STACK AND
                STA         DEVNUM          ; SAVE IT AS DEVICE NUMBER
                TST         FCBTYP,X        ; IS FILE OPEN?
                LBEQ        LA3FB           ; 'FILE NOT OPEN' ERROR IF NOT OPEN
                RTS

; LOF COMMAND
LOF             BSR         LCD3D           ; POINT X TO FILE BUFFER
                LDA         FCBDRV,X        ; GET DRIVE NUMBER AND SAVE IT
                STA         DCDRV           ; IN DSKCON VARIABLE
                LDB         FCBFGR,X        ; GET FIRST GRANULE OF FILE
                PSHS        X               ; SAVE FCB POINTER ON STACK
                JSR         LCC44           ; FIND TOTAL NUMBER OF GRANULES IN THIS FILE
                DECA                        ; SUBTRACT THE LAST GRANULE IN THE FILE
                ANDB        #$3F            ; GET NUMBER OF SECTORS USED IN LAST GRANULE
                PSHS        B               ; SAVE NUMBER OF SECTORS IN LAST GRANULE ON STACK
                TFR         A,B             ; CONVERT ACCA TO POSITIVE
                CLRA                        ;  2 BYTE VALUE IN ACCD
                JSR         LC749           ; MULT NUMBER OF FULL GRANULES BY 9
                ADDB        ,S+             ; ADD NUMBER SECTORS IN LAST TRACK
                ADCA        #$00            ; PROPAGATE CARRY TO MS BYTE OF ACCD
                PULS        X               ; GET FCB POINTER BACK
                PSHS        A               ; SAVE ACCA ON STACK
                LDA         FCBTYP,X        ; GET FILE TYPE OF THIS FCB AND
                CMPA        #RANFIL         ; CHECK TO SEE IF IT'S A RANDOM FILE
                PULS        A               ; RESTORE ACCA
                BNE         LCD3A           ; IF NOT A RANDOM FILE, THEN THE TOTAL NUMBER OF SECTORS IN THE FILE
; IS THE LENGTH OF THE FILE

; CALCULATE LOF FOR A RANDOM FILE - THE LENGTH OF A RANDOM FILE IS THE
; NUMBER OF RECORDS IN THE FILE.
                PSHS        X               ; SAVE FCB POINTER ON STACK
                SUBD        ZERO            ; SUBTRACT ZERO FROM ACCD (NUMBER OF SECTORS)
                BEQ         LCD8C           ; BRANCH IF ZERO SECTORS
                SUBD        #$0001          ; SUBTRACT ONE SECTOR - THE LAST SECTOR MAY NOT BE IOOZ USED
LCD8C           BSR         LCD3A           ; PUT ACCD INTO FPA0
                LDB         FP0EXP          ; GET EXPONENT OF FPA0
                BEQ         LCD96           ; BRANCH IF FPA0 = 0
                ADDB        #$08            ; ADD 8 TO EXPONENT (MULTIPLY FPA0 BY
                STB         FP0EXP          ; 256 BYTES/SECTOR) AND SAVE NEW EXPONENT
LCD96           JSR         LBC5F           ; SAVE NUMBER OF BYTES IN FULL SECTORS IN FPA1
                LDX         ,S              ; POINT X TO FCB
                LDD         FCBLST,X        ; GET NUMBER OF BYTES IN LAST SECTOR
                ANDA        #$7F            ; MASK OFF THE PRE-SAVED BYTE
                BSR         LCD3A           ; PUT NUMBER BYTES IN LAST SECTOR INTO FPA0
                CLR         RESSGN          ; FORCE SUM SIGN = POSITIVE
                LDA         FP1EXP          ; GET EXPONENTS OF FPA0 AND
                LDB         FP0EXP          ; FPA1 PRIOR TO ADDITION
                JSR         LB9C5           ; =ADD NUMBER BYTES IN LAST SECTOR TO NUMBER OF
; =BYTES IN FULL SECTORS
                JSR         LBC5F           ; SAVE TOTAL NUMBER OF BYTES IN FPA1
                PULS        X               ; POINT X TO FCB
                LDD         FCBRLN,X        ; GET RECORD LENGTH
                BSR         LCD3A           ; PUT IT INTO FPA0
                CLR         RESSGN          ; FORCE QUOTIENT SIGN = POSITIVE
                LDA         FP1EXP          ; GET EXPONENTS OF FPA0 AND
                LDB         FP0EXP          ; FPA1 PRIOR TO DIVISION
                JSR         LBB91           ; DIVIDE TOTAL NUMBER OF BYTES BY NUMBER OF BYTES IN A RECORD
                JMP         INT             ; CONVERT FPA0 TO AN INTEGER

; FREE COMMAND
FREE            JSR         LB143           ; NUMBER TYPE CHECK
                JSR         LB70E           ; EVALUATE NUMERIC EXPRESSION AND RETURN VALUE IN ACCB
                CMPB        #$03            ; ONLY 4 LEGAL DRIVES
                LBHI        LA61F           ; 'DEVICE NUMBER' ERROR IF DRIVE NUMBER IS > 3
                STB         DCDRV           ; SAVE IN DRIVE NUMBER
                JSR         LC76D           ; GET FILE ALLOCATION TABLE AND STORE IN BUFFER
                JSR         LC725           ; POINT X TO START OF FILE ALLOCATION TABLE BUFFER
                LEAX        FATCON,X        ; MOVE TO FIRST GRANULE DATA BYTE
                CLR         ,-S             ; SPACE FOR FREE GRANULE COUNTER
                LDB         #GRANMX         ; GET MAXIMUM NUMBER OF GRANULES
LCDDA           LDA         ,X+             ; GET GRANULE DATA
                COMA                        ;  FREE GRANULES $FF
                BNE         LCDE1           ; BRANCH IF NOT FREE
                INC         ,S              ; INCREMENT FREE GRANULE COUNTER
LCDE1           DECB                        ; DECREMENT GRANULE COUNTER
                BNE         LCDDA           ; BRANCH IF NOT DONE
                PULS        B               ; GET FREE GRANULE COUNTER TO ACCB
                JMP         LB4F3           ; LOAD ACCB INTO FPA0

; DRIVE COMMAND
DRIVE           JSR         EVALEXPB        ; EVALUATE EXPR; RETURN VALUE IN ACCB
                CMPB        #$03            ; MAX DRIVE NUMBER = 3
                LBHI        LA61F           ; 'DEVICE #' ERROR IF DRIVE NUMBER > 3
                STB         DEFDRV          ; SAVE DEFAULT DRIVE NUMBER
                RTS

; EVALUATE EXPRESSION RAM VECTOR
DVEC15          LDA         $04,S           ; = CHECK STACKED PRECEDENCE FLAG AND IF IT IS NOT AN END
                BNE         LCE0D           ; = OF OPERATION, BRANCH TO EXTENDED BASIC'S EXPRESSION
; = EVALUATION ROUTINE
                LDX         $05,S           ;
                CMPX        #LAF9A          ;
                BNE         LCE0D           ; CHECK TWO RETURN ADDRESSES BACK ON THE STACK
                LDX         $02,S           ; TO SEE IF THE CALL TO EVALUATE EXPRESSION IS
                CMPX        #LB166          ; COMING FROM THE 'LET' COMMAND - BRANCH OUT IF
                BNE         LCE0D           ; NOT COMING FROM 'LET'
                LDX         #LCE10          ; = IF COMING FROM 'LET', REPLACE THE RETURN ADDR
                STX         $05,S           ; = WITH THE DISK BASIC 'LET' MODIFIER ADDRESS
LCE0D           JMP         XVEC15          ; EXTENDED BASIC EXPRESSION EVALUATION

; LET MODIFIER
LCE10           PULS        A               ; PULL VARIABLE TYPE OFF OF THE STACK
                RORA                        ; SET CARRY IF SIRING, CLEAR CARRY IF NUMERIC
                JSR         LB148           ; DO A 'TM' CHECK
                LBEQ        LBC33           ; IF NUMERIC VARIABLE, PACK FPA0 INTO VARDES
                LDX         FPA0+2          ; POINT X TO STRING DESCRIPTOR
                LDD         $02,X           ; GET ADDRESS OF SIRING
                CMPD        #DFLBUF         ; COMPARE TO START OF RANDOM FILE BUFFERS
                BLO         LCE2B           ; AND BRANCH IF LOWER
                SUBD        FCBADR          ; SUBTRACT OUT THE END OF RANDOM FILE BUFFERS
                LBCS        LAFB1           ; BRANCH IF STRING STORED IN RANDOM FILE BUFFER -
; MOVE IT INTO THE STRING SPACE
LCE2B           JMP         LAFA4           ; BRANCH BACK TO BASICS 'LET' COMMAND

; MODIFIER FOR EXBAS COMMAND INTERPRETATION HANDLER
DXCVEC          CMPA        #$CA            ; TOKEN FOR DLOAD?
                BEQ         LCE4E           ; YES
                CMPA        #$C8            ; TOKEN FOR PMODE?
                LBNE        L813C           ; NO
; DISK BASIC MODIFIER FOR PMODE - ALLOWS FOR THE RAM THE DOS USES
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                CMPA        #','            ; CHECK FOR COMMA
                LBEQ        L9650           ; BRANCH IF COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION; RETURN VALUE IN ACCB
                CMPB        #$04            ; CHECK FOR PMODE 4
                LBHI        LB44A           ; 'FC' ERROR IF PMODE > 4
                LDA         GRPRAM          ; NUMBER BLOCKS BEFORE GRAPHICS PAGES
                JMP         L962E           ; JUMP TO EXEAS' PMODE COMMAND

; DISK BASIC DLOAD MODIFIER
LCE4E           JSR         LA429           ; CLOSE FILES
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
                JMP         >DLDBUG         ; JUMP TO EXEAS' DLOAD

DXIVEC          CMPB        #($9A-$80)*2    ; MODIFIED TOKEN FOR POS
                LBNE        L8168           ; IF NOT POS, GO TO EXBAS SECONDARY COMM HANDLER
                JSR         LB262           ; SYNTAX CHECK FOR '(' AND EVALUATE EXPRESSION
                LDA         DEVNUM          ; GET DEVICE NUMBER AND
                PSHS        A               ; SAVE IT ON STACK
                JSR         LA5AE           ; EVALUATE DEVICE NUMBER
                JSR         LA406           ; TEST DEVICE NUMBER
                TST         DEVNUM          ; CHECK DEVICE NUMBER AND BRANCH
                BLE         LCE80           ; IF NOT A DISK FILE
                JSR         LC714           ; POINT X TO FCB
                LDB         FCBTYP,X        ; GET FILE TYPE
                CMPB        #RANFIL         ; DIRECT/RANDOM FILE?
                BNE         LCE80           ; BRANCH IF NOT A RANDOM FILE
                PULS        A               ; RESTORE DEVICE NUMBER
                STA         DEVNUM          ;
                LDD         FCBPUT,X        ; =GRAB THE 'PUT' DATA ITEM COUNTER AND CONVERT
                JMP         GIVABF          ; =IT TO A FLOATING POINT NUMBER
LCE80           JSR         LA35F           ; SET PRINT PARAMETERS
                PULS        A               ; RESTORE DEVICE NUMBER
                STA         DEVNUM          ;
                LDB         DEVPOS          ; =GET PRINT POSITION AND
                JMP         LB4F3           ; =CONVERT IT TO FLOATING POINT NUMBER IN FPA0

; SAVEM COMMAND
LCE8C           JSR         GETNCH          ; GET NEXT INPUT CHARACTER
                BSR         LCEDF           ; GET FILENAME, ETC.
                JSR         L836C           ; EVALUATE EXPRESSION, PUT II (2 BYTES) ON STACK
                JSR         L836C           ; DITTO
                CMPX        $02,S           ; COMPARE END ADDRESS TO START ADDRESS
                LBCS        LB44A           ; IF START > END, THEN 'ILLEGAL FUNCTION CALL'
                JSR         L836C           ; EVAL EXPRESSION (TRANSFER ADDRESS), PUT ON STACK
                JSR         LA5C7           ; SYNTAX ERROR IF ANY MORE CHARS ON THIS LINE
                LDD         #$0200          ; FILE TYPE=2, ASCII FLAG = CRUNCHED (0)
                STD         DFLTYP          ;
                JSR         LC956           ; GET NEXT UNOPEN FILE AND INITIALIZE FCB
                CLRA                        ;  ZERO FLAG - FIRST BYTE OF PREAMBLE
                BSR         LCED9           ; WRITE A BYTE TO BUFFER
                LDD         $02,S           ; GET END ADDRESS
                SUBD        $04,S           ; SUBTRACT THE START ADDRESS
                ADDD        #$0001          ; THE SAVED DATA BLOCK WILL INCLUDE BOTH THE FIRST AND LAST BYTES
                TFR         D,Y             ; SAVE LENGTH IN Y
                BSR         LCED7           ; WRITE FILE LENGTH TO BUFFER - FIRST ARGUMENT OF PREAMBLE
                LDD         $04,S           ; GET THE START ADDRESS
                BSR         LCED7           ; WRITE OUT THE START ADDRESS - SECOND PREAMBLE ARGUMENT
                LDX         $04,S           ; GET START ADDRESS
LCEBF           LDA         ,X+             ; GRAB A BYTE
                JSR         LCB52           ; WRITE IT OUT
                LEAY        -1,Y            ; DECREMENT BYTE COUNTER
                BNE         LCEBF           ; BRANCH IF ALL BYTES NOT DONE
                LDA         #$FF            ; FIRST BYTE OF POSTAMBLE
                BSR         LCED9           ; WRITE IT OUT - EOF RECORD
                CLRA                        ;  FIRST ARGUMENT OF POSTAMBLE IS
                CLRB                        ;  A DUMMY - ZERO VALUE
                BSR         LCED7           ; WRITE OUT POSTAMBLE FIRST ARGUMENT
                PULS        A,B,X,Y         ; GET CONTROL ADDRESSES FROM THE STACK
                BSR         LCED7           ; WRITE OUT THE TRANSFER ADDRESS - 2ND ARGUMENT
                JMP         LA42D           ; GO CLOSE ALL FILES

; WRITE ACCD TO THE BUFFER
LCED7           BSR         LCED9           ; WRITE ACCA TO BUFFER, THEN SWAP ACCA,ACCB
LCED9           JSR         LCB52           ; WRITE ACCA TO BUFFER
                EXG         A,B             ; SWAP ACCA,ACCB
                RTS
LCEDF           LDX         #BINEXT         ; POINT TO .BIN EXTENSION
                JMP         LC88A           ; GET FILENAME, ETC.

; LOADM COMMAND
LCEE5           JSR         GETNCH          ; GET NEXT INPUT CHARACTER
                BSR         LCEDF           ; GET FILENAME, ETC.
                JSR         LC959           ; OPEN NEXT AVAILABLE FILE FOR INPUT
                LDD         DFLTYP          ; GET FILE TYPE AND ASCII FLAG
                SUBD        #$0200          ; FOR LOADM FILE: TYPE=2, ASCII FLAG=0
                LBNE        LA616           ; 'BAD FILE MODE' ERROR
                LDX         ZERO            ; ZERO OUT X REG - DEFAULT VALUE OF OFFSET
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                BEQ         LCF02           ; BRANCH IF END OF LINE - NO OFFSET
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         LB73D           ; EVALUATE EXPRESSION
LCF02           STX         VD3             ; STORE OFFSET IN VD3
                JSR         LA5C7           ; SYNTAX ERROR IF OTHER CHARACTERS ON LINE

; GET PREAMBLE/POSTAMBLE
LCF07           JSR         LCCE2           ; GET FIRST BYTE
                PSHS        A               ; SAVE IT ON THE STACK
                BSR         LCF37           ; GET FIRST ARGUMENT
                TFR         D,Y             ; SAVE IT IN Y
                BSR         LCF37           ; GET THE SECOND ARGUMENT
                ADDD        VD3             ; ADD IT TO THE OFFSET
                STD         EXECJP          ; STORE IT IN THE JUMP ADDRESS OF THE EXEC COMMAND
                TFR         D,X             ; SAVE IT IN X
                LDA         ,S+             ; GET THE FIRST BYTE OFF OF THE STACK
                LBNE        LA42D           ; CLOSE FILE IF POSTAMBLE (EOF)

; GET RECORD BYTE(S)
LCF1E           JSR         LC597           ; GET BYTE FROM BUFFER
                LDB         CINBFL          ; GET STATUS OF CONSOLE IN BUFFER
                BEQ         LCF28           ; BRANCH IF BUFFER NOT EMPTY
                JMP         LC334           ; 'INPUT PAST END OF FILE' ERROR
LCF28           STA         ,X              ; STORE BYTE IN MEMORY
                CMPA        ,X+             ; TEST TO SEE IF IT STORED PROPERLY AND
                BEQ         LCF31           ; BRANCH IF PROPER STORE (NOT IN ROM OR BAD RAM)
                JMP         LD616           ; 'I/O ERROR' IF BAD STORE
LCF31           LEAY        -1,Y            ; DECREMENT BYTE COUNT
                BNE         LCF1E           ; GET NEXT BYTE IF NOT DONE
                BRA         LCF07           ; READ ANOTHER PRE/POST AMBLE
; READ TWO BYTES FROM BUFFER - RETURN THEM IN ACCD
LCF37           BSR         LCF39           ; READ A BYTE, SAVE IT IN ACCB
LCF39           JSR         LCCE2           ; GET A CHARACTER FROM INPUT BUFFER, RETURN IT IN ACCA
                EXG         A,B             ; SWAP ACCA,ACCB
                RTS

; RENAME COMMAND
RENAME          LDX         CHARAD          ; SAVE CURRENT INPUT POINTER
                PSHS        X               ; ON THE STACK
                BSR         LCF7A           ; GET FILENAME OF SOURCE FILE
                LDA         DCDRV           ; SAVE DRIVE NUMBER
                PSHS        A               ; ON THE STACK
                BSR         LCF75           ; SYNTAX CHECK FOR 'TO' AND GET NEW FILENAME
                PULS        A               ; GET SOURCE DRIVE NUMBER
                CMPA        DCDRV           ; COMPARE TO NEW FILE DRIVE NUMBER
                LBNE        LB44A           ; 'FC' ERROR IF FlIES ON DIFFERENT DRIVES
                BSR         LCF7D           ; VERIFY THAT NEW FILE DOES NOT ALREADY EXIST
                PULS        X               ; RESTORE INPUT POINTER
                STX         CHARAD          ;
                BSR         LCF7A           ; GET SOURCE FILENAME AGAIN
                JSR         LC65F           ; SCAN DIRECTORY FOR SOURCE FILENAME
                JSR         LC6B8           ; 'NE' ERROR IF NOT FOUND
                BSR         LCF75           ; SYNTAX CHECK FOR 'TO' AND GET NEW FILENAME
                LDX         #DNAMBF         ; POINT X TO FILENAME
                LDU         V974            ; POINT U TO DIRECTORY ENTRY OF SOURCE FILE
                LDB         #11             ; 11 CHARACTERS IN FILENAME AND EXTENSION
                JSR         LA59A           ; COPY NEW FILENAME TO SOURCE FILE DIRECTORY RAM IMAGE
                LDB         #$03            ; GET WRITE OP CODE AND
                STB         DCOPC           ; SAVE IN DSKCON VARIABLE
                JMP         LD5FF           ; WRITE NEW DIRECTORY SECTOR

; DO A SYNTAX CHECK FOR 'TO AND STRIP A FILENAME FROM BASIC
LCF75           LDB         #$A5            ; 'TO' TOKEN
                JSR         LB26F           ; SYNTAX CHECK FOR 'TO'
LCF7A           JMP         LC887           ; GET FILENAME FROM BASIC
LCF7D           JSR         LC65F           ; SCAN DIRECTORY FOR FILENAME
                LDB         #33*2           ; 'FILE ALREADY EXISTS' ERROR
                TST         V973            ; CHECK FOR A MATCH
                LBNE        LAC46           ; 'AE' ERROR IF FILE IN DIRECTORY
                RTS

; WRITE COMMAND
WRITE           LBEQ        LB958           ; PRINT CARRIAGE RETURN TO CONSOLE OUT IF END OF LINE
                BSR         LCF93           ; GO WRITE AN ITEM LIST
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
LCF92           RTS
LCF93           CMPA        #'#'            ; CHECK FOR DEVICE NUMBER FLAG
                BNE         LCFA6           ; DEFAULT TO CURRENT DEVICE NUMBER IF NONE GIVEN
                JSR         LA5A5           ; SET DEVICE NUMBER; CHECK VALIDITY
                JSR         LA406           ; MAKE SURE SELECTED FILE IS AN OUTPUT FILE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                LBEQ        LB958           ; PRINT CR TO CONSOLE OUT IF END OF LINE
LCFA3           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
LCFA6           JSR         LB156           ; EVALUATE EXPRESSION
                LDA         VALTYP          ; GET VARIABLE TYPE
                BNE         LCFCB           ; BRANCH IF STRING
                JSR         LBDD9           ; CONVERT FP NUMBER TO ASCII STRING
                JSR         LB516           ; PUT ON TEMPORARY STRING STACK
                JSR         LB99F           ; PRINT STRING TO CONSOLE OUT

; PRINT ITEM SEPARATOR TO CONSOLE OUT
LCFB6           JSR         GETCCH          ; GET CURRENT CHARACTER
                LBEQ        LB958           ; PUT CR TO CONSOLE OUT IF END OF LINE
                LDA         #','            ; COMMA: NON-CASSETTE SEPARATOR
                JSR         LA35F           ; SET PRINT PARAMETERS
                TST         PRTDEV          ; GET CONSOLE PRINT DEVICE AND
                BEQ         LCFC7           ; BRANCH IF NOT CASSETTE
                LDA         #CR             ; GET CARRIAGE RETURN - CASSETTE ITEM SEPARATOR
LCFC7           BSR         LCFDD           ; SEND SEPARATOR TO CONSOLE OUT
                BRA         LCFA3           ; GET NEXT ITEM

; PRINT A STRING TO CONSOLE OUT
LCFCB           BSR         LCFD4           ; PRINT LEADING STRING DELIMITER (")
                JSR         LB99F           ; PRINT STRING TO CONSOLE OUT
                BSR         LCFD4           ; PRINT ENDING STRING DELIMITER (")
                BRA         LCFB6           ; GO PRINT SEPARATOR

; PRINT STRING DELIMITER (") TO CONSOLE OUT
LCFD4           JSR         LA35F           ; SET PRINT PARAMETERS
                TST         PRTDEV          ; GET CONSOLE PRINT DEVICE AND
                BNE         LCF92           ; RETURN IF CASSETTE
                LDA         #'"'            ; QUOTE: NON-CASSETTE STRING DELIMITER
LCFDD           JMP         PUTCHR          ; SEND TO CONSOLE OUT

; FIELD COMMAND
FIELD           JSR         LC7FE           ; EVALUATE DEVICE NUMBER & VERIFY RANDOM FILE OPEN
                CLRA                        ;
                CLRB                        ;  CLEAR TOTAL FIELD LENGTH COUNTER
                PSHS        X,B,A           ; SAVE FCB POINTER & INITIALIZE TOTAL FIELD LENGTH TO ZERO
LCFE7           JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BNE         LCFED           ; BRANCH IF NOT END OF LINE
                PULS        A,B,X,PC        ; CLEAN UP STACK AND RETURN
LCFED           JSR         LB738           ; SYNTAX CHECK FOR COMMA, EVALUATE EXPRESSION
                PSHS        X,B             ; SAVE FIELD LENGTH (ACCB) ON STACK, X IS A DUMMY WHICH WILL
; RESERVE 2 BYTES FOR THE ADDRESS WHICH WILL BE CALCULATED BELOW
; AT THIS POINT THE STACK WILL HAVE THE FOLLOWING INFORMATION ON IT:
; ,S = FIELD LENGTH 1 2,S = RANDOM FILE BUFFER ADDRESS
; 3 4,S = TOTAL FIELD LENGTH 5 6,S = FCD POINTER
                CLRA                        ; CLEAR MS BYTE
                ADDD        $03,S           ; ADD FIELD LENGTH TO TOTAL FIELD LENGTH COUNTER
                BLO         LCFFE           ; 'FO' ERROR IF SUM > $FFFF
                LDX         $05,S           ; POINT X TO FCB
                CMPD        FCBRLN,X        ; COMPARE TO RECORD LENGTH & BRANCH IF
                BLS         LD003           ; TOTAL FIELD LENGTH < RECORD LENGTH
LCFFE           LDB         #34*2           ; 'FIELD OVERFLOW' ERROR
                JMP         LAC46           ; JUMP TO ERROR DRIVER
LD003           LDU         $03,S           ; LOAD U WITH OLD TOTAL LENGTH OF ALL FIELDS
                STD         $03,S           ; SAVE NEW TOTAL FIELD LENGTH
                LDD         FCBBUF,X        ; POINT ACCD TO START OF RANDOM FILE BUFFER
                LEAU        D,U             ; POINT U TO THIS FIELD'S SLOT IN THE RANDOM
                STU         $01,S           ; FILE BUFFER AND SAVE IT ON THE STACK
                LDB         #$FF            ; SECONDARY TOKEN
                JSR         LB26F           ; SYNTAX CHECK FOR SECONDARY TOKEN
                LDB         #$A7            ; 'AS' TOKEN
                JSR         LB26F           ; SYNTAX CHECK FOR 'AS' TOKEN
                JSR         LB357           ; EVALUATE VARIABLE
                JSR         LB146           ; 'TM' ERROR IF NUMERIC VARIABLE
                PULS        B,U             ; PULL STRING ADDRESS AND LENGTH
                STB         ,X              ; OFF OF THE STACK AND SAVE THEM
                STU         $02,X           ; IN STRING DESCRIPTOR
                BRA         LCFE7           ; CHECK FOR ANOTHER FIELD SPECIFICATION

; RSET COMMAND
RSET            FCB         $86             ; SKIP ONE BYTE (THROWN AWAY LDA INSTRUCTION)

; LSET COMMAND
LSET            CLRA                        ; LSET FLAG = 0
                PSHS        A               ; SAVE RSET($4F),LSET(00) FLAG ON THE STACK
                JSR         LB357           ; EVALUATE FIELD STRING VARIABLE
                JSR         LB146           ; 'TM' ERROR IF NUMERIC VARIABLE
                PSHS        X               ; SAVE STRING DESCRIPTOR ON STACK
                LDX         $02,X           ; POINT X TO ADDRESS OF STRING
                CMPX        #DFLBUF         ; COMPARE STRING ADDRESS TO START OF RANDOM
                BLO         LD03D           ; FILE BUFFER; 'SE' ERROR IF < RANDOM FILE BUFFER
                CMPX        FCBADR          ; = COMPARE STRING ADDRESS TO TOP OF RANDOM FILE BUFFER
                BLO         LD042           ; = AREA - BRANCH IF STRING IN RANDOM FILE BUFFER
LD03D           LDB         #2*35           ; 'SET TO NON-FIELDED STRING' ERROR
                JMP         LAC46           ; JUMP TO ERROR HANDLER
LD042           LDB         #$B3            ;
                JSR         LB26F           ; SYNTAX CHECK FOR '=' TOKEN
                JSR         L8748           ; =EVALUATE DATA STRING EXPRESSION; RETURN WITH X
; =POINTING TO STRING; ACCB = LENGTH
                PULS        Y               ; POINT Y TO FIELD STRING DESCRIPTOR
                LDA         ,Y              ; GET LENGTH OF FIELD STRING
                BEQ         LD07E           ; RETURN IF NULL STRING
                PSHS        B               ; SAVE LENGTH OF DATA STRING ON STACK
                LDB         #SPACE          ; PREPARE TO FILL DATA STRING WITH BLANKS
                LDU         $02,Y           ; POINT U TO FIELD STRING ADDRESS
; FILL THE FIELDED STRING WITH BLANKS
LD056           STB         ,U+             ; STORE A SPACE IN FIELDED STRING
                DECA                        ; DECREMENT LENGTH COUNTER
                BNE         LD056           ; KEEP FILLING W/SPACES IF NOT DONE
                LDB         ,S+             ; GET THE LENGTH OF THE DATA STRING AND
                BEQ         LD07E           ; RETURN IF IT IS NULL (ZERO)
                CMPB        ,Y              ; =COMPARE LENGTH OF DATA STRING TO LENGTH OF FIELD
                BLO         LD067           ; =STRING, BRANCH IF FIELD STRING > DATA STRING
                LDB         ,Y              ; GET THE LENGTH OF THE FIELD STRING AND FORCE THE
                CLR         ,S              ; RSET/LSET FLAG TO LSET (0) IF DATA STRING LENGTH IS
; >= THE FIELD STRING LENGTH. THIS WILL CAUSE THE RIGHT
; SIDE OF THE DATA STRING TO BE TRUNCATED
LD067           LDU         $02,Y           ; LOAD U WITH THE ADDRESS OF THE FIELD STRING
                TST         ,S+             ; GET THE RSET/LSET FLAG FROM THE STACK
                BEQ         LD07B           ; AND BRANCH IF LSET
; RSET ROUTINE
                PSHS        B               ; SAVE THE NUMBER OF BYTES TO MOVE INTO THE FIELD STRING
                CLRA                        ; = TAKE THE 2'S COMPLEMENT OF AN UNSIGNED
                NEGB                        ; = NUMBER IN ACCB - LEAVE THE DOUBLE BYTE SIGNED
                SBCA        #$00            ; = RESULT IN ACCD
                ADDB        ,Y              ; ADD THE LENGTH OF THE FIELD STRING TO THE INVERSE
                ADCA        #$00            ; OF THE NUMBER OF BYTES TO BE MOVED
                LEAU        D,U             ; =ADD RESULT TO START OF FIELD STRING. NOW U
                                            ; =WILL POINT TO (-NUMBER OF BYTES TO MOVE)
                                            ; =FROM THE RIGHT SIDE OF THE FIELD STRING
                PULS        B               ; GET THE NUMBER OF BYTES TO MOVE
LD07B           JMP         LA59A           ; MOVE ACCB BYTES FROM X TO U (DATA TO FIELD STRING)
LD07E           PULS        A,PC            ; PULL LSET/RSET FLAG OFF OF STACK AND RETURN

; FILES COMMAND
FILES           JSR         L95AC           ; RESET SAM DISPLAY PAGE AND VDG MODE
                LDD         FCBADR          ; GET START OF FILE BUFFERS
                SUBD        #DFLBUF         ; SUBTRACT THE START OF RANDOM FILE BUFFER SPACE
                PSHS        B,A             ; SAVE DEFAULT VALUE OF RANDOM FILE BUFFER SPACE ON STACK
                LDB         FCBACT          ; GET CURRENT NUMBER OF FCBS
                PSHS        B               ; AND SAVE ON THE STACK (DEFAULT VALUE)
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                CMPA        #','            ; CHECK FOR COMMA
                BEQ         LD0A5           ; BRANCH IF COMMA - NO BUFFER NUMBER PARAMETER GIVEN
                JSR         EVALEXPB        ; EVALUATE EXPRESSION (BUFFER NUMBER)
                CMPB        #15             ; 15 FCBS MAX
                LBHI        LB44A           ; BRANCH IF > 15 - 'ILLEGAL FUNCTION CALL'
                STB         ,S              ; SAVE NUMBER OF FCBS ON STACK
                JSR         GETCCH          ; CHECK CURRENT INPUT CHAR
                BEQ         LD0B0           ; BRANCH IF END OF LINE
LD0A5           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         LB3E6           ; EVALUATE EXPRESSION, RETURN VALUE IN ACCD
                ADDD        #$0001          ; ADD ONE BYTE
                STD         $01,S           ; SAVE RANDOM FILE BUFFER SIZE ON STACK
LD0B0           JSR         DVEC7           ; CLOSE FILES
                LDB         ,S              ; GET THE NUMBER OF BUFFERS TO MAKE AND
                PSHS        B               ; INITIALIZE A BUFFER COUNTER ON THE STACK
                LDD         #DFLBUF         ; GET START OF RANDOM FILE BUFFERS
                ADDD        $02,S           ; ADD THE NEWLY SPECIFIED RANDOM FILE BUFFER SPACE
                BLO         LD11B           ; 'OUT OF MEMORY' ERROR IF > $FFFF
                STD         $02,S           ; SAVE START OF FCBS
; RESERVE SPACE FOR FCBS
LD0C0           ADDD        #FCBLEN         ; FCBLEN REQUIRED FOR EACH BUFFER
                BLO         LD11B           ; 'OUT OF MEMORY' ERROR IF > $FFFF
                DEC         ,S              ; DECREMENT BUFFER COUNTER
                BPL         LD0C0           ; BRANCH IF NOT DONE - THE BPL WILL SET UP ONE MORE BUFFER
; THAN THE NUMBER REQUESTED. THIS EXTRA BUFFER IS THE SYSTEM BUFFER
; AND IS LOCATED AT THE END OF THE NORMAL FCBS. ONLY SYSTEM ROUTINES
; (COPY, BACKUP, MERGE ETC.) MAY ACCESS THIS BUFFER.
                TSTB                        ; AT AN EXACT 256 BYTE BOUNDARY?
                BEQ         LD0CF           ; YES
                INCA                        ; NO - ADD 256
                BEQ         LD11B           ; 'OUT OF MEMORY' ERROR IF PAST $FFFF
LD0CF           STA         ,S              ; SAVE MS BYTE OF NEW GRAPHIC RAM START
                LDD         VARTAB          ; GET START OF VARIABLES
                SUBA        GRPRAM          ; SUBTRACT THE OLD GRAPHIC RAM START - ACCD CONTAINS LENGTH
; OF PROGRAM PLUS RESERVED GRAPHIC RAM
                ADDA        ,S              ; ADD IN THE AMOUNT OF RAM CALCULATED ABOVE
                BLO         LD11B           ; 'OUT OF MEMORY' ERROR IF > $FFFF
                TFR         D,X             ; SAVE NEW VARTAB IN X
                INCA                        ;  ADD 256 - TO GUARANTEE ENOUGH ROOM SINCE ALL CALCULATIONS USE
; ONLY THE MSB OF THE ADDRESS
                BEQ         LD11B           ; 'OUT OF MEMORY' ERROR IF PAST $FFFF
                CMPD        FRETOP          ; IS IT GREATER THAN THE START OF STRING SPACE
                BHS         LD11B           ; 'OUT OF MEMORY' IF > START OF STRING SPACE
                DECA                        ; SUBTRACT 256 - COMPENSATE FOR INCA ABOVE
                SUBD        VARTAB          ; SUBTRACT START OF VARIABLES
                ADDD        TXTTAB          ; ADD START OF BASIC
                TFR         D,Y             ; Y HAS NEW START OF BASIC
                LDA         ,S              ; GET THE GRAPHIC RAM START, SUBTRACT
                SUBA        GRPRAM          ; THE OLD GRAPHIC RAN START AND SAVE
                TFR         A,B             ; THE DIFFERENCE IN ACCA AND ACCB
                ADDA        BEGGRP          ; = ADD THE OLD GRAPHIC PAGE START AND
                STA         BEGGRP          ; = STORE THE NEW START OF GRAPHICS RAM
                ADDB        ENDGRP          ; ADD THE OLD GRAPHIC RAM END ADDRESS AND
                STB         ENDGRP          ; STORE THE NEW END OF GRAPHICS RAM
                PULS        A,B,U           ; = ACCA=MSB OF START OF GRAPHIC RAM; ACCB=NUMBER OF FILE BUFFERS
; = U=START OF FILE BUFFERS
                STA         GRPRAM          ; SAVE NEW START OF GRAPHIC RAM
                STB         FCBACT          ; NUMBER OF FILE BUFFERS
                STU         FCBADR          ; START OF FILE BUFFERS
LD102           LDU         VARTAB          ; POINT U TO OLD START OF VARIABLES
                STX         VARTAB          ; SAVE NEW START OF VARIBLES
                CMPU        VARTAB          ; COMPARE OLD START OF VARIABLES TO NEW START OF
                BHI         LD11E           ; VARIABLES & BRANCH IF OLD > NEW
; MOVE BASIC PROGRAM IF OLD START ADDRESS <= NEW START ADDRESS
LD10B           LDA         ,-U             ; GET A BYTE
                STA         ,-X             ; MOVE lT
                CMPU        TXTTAB          ; AT START OF BASIC PROGRAM?
                BNE         LD10B           ; NO
                STY         TXTTAB          ; STORE NEW START OF BASIC PROGRAM
                CLR         -1,Y            ; RESET START OF PROGRAM FLAG
                BRA         LD12E           ; CLOSE ALL FILES
LD11B           JMP         LAC44           ; 'OUT OF MEMORY' ERROR
; MOVE BASIC PROGRAM IF OLD START ADDRESS > NEW START ADDRESS
LD11E           LDU         TXTTAB          ; POINT U TO OLD START OF BASIC
                STY         TXTTAB          ; SAVE NEW START OF BASIC
                CLR         -1,Y            ; RESET START OF BASIC FLAG
LD125           LDA         ,U+             ; GET A BYTE
                STA         ,Y+             ; MOVE IT
                CMPY        VARTAB          ; AT START OF VARIABLES
                BNE         LD125           ; NO - MOVE ANOTHER BYTE

; CLOSE ALL FCBS AND RECALCULATE FCB START ADDRESSES
LD12E           LDU         #FCBV1          ; POINT U TO FILE BUFFER POINTERS
                LDX         FCBADR          ; POINT X TO START OF BUFFERS
                CLRB                        ; RESET FILE COUNTER
LD135           STX         ,U++            ; STORE FILE ADDRESS IN VECTOR TABLE
                CLR         FCBTYP,X        ; RESET FILE TYPE TO CLOSED
                LEAX        FCBLEN,X        ; GO TO NEXT FCB
                INCB                        ; INCREMENT FILE COUNTER
                CMPB        FCBACT          ; CLOSE ALL ACTIVE BUFFERS AND SYSTEM FCB
                BLS         LD135           ; BRANCH IF NOT DONE
                JMP         L96CB           ; READJUST LINE NUMBERS, ETC.

; UNLOAD COMMAND
UNLOAD          BSR         LD162           ; GET DRIVE NUMBER
                CLRB                        ; CLEAR FILE COUNTER
LD149           INCB                        ; INCREMENT FILE COUNTER
                JSR         LC719           ; POINT X TO FCB
                BEQ         LD15C           ; BRANCH IF FILE NOT OPEN
                LDA         FCBDRV,X        ; CHECK DRIVE NUMBER
                CMPA        DCDRV           ; DOES IT MATCH THE 'UNLOAD' DRIVE NUMBER?
                BNE         LD15C           ; NO MATCH - DO NOT CLOSE THE FILE
                PSHS        B               ; SAVE FILE COUNTER ON THE STACK
                JSR         LCA58           ; CLOSE FCB
                PULS        B               ; RESTORE FILE COUNTER
LD15C           CMPB        FCBACT          ; CHECKED ALL FILES?
                BLS         LD149           ; NO
                RTS
; GET DRIVE NUMBER FROM BASIC - USE THE DEFAULT DRIVE IF NONE GIVEN
LD162           LDB         DEFDRV          ; GET DEFAULT DRIVE NUMBER
                JSR         GETCCH          ; GET NEXT INPUT CHAR
                BEQ         LD172           ; USE DEFAULT DRIVE NUMBER IF NONE GIVEN
LD169           JSR         EVALEXPB        ; EVALUATE EXPRESSION
                CMPB        #$03            ; 4 DRIVES MAX
                LBHI        LA61F           ; 'DEVICE NUMBER ERROR' IF > 3
LD172           STB         DCDRV           ; STORE IN DSKCON VARIABLE
                RTS

; BACKUP COMMAND
BACKUP          LBEQ        LA61F           ; DEVICE NUMBER ERROR IF NO DRIVE NUMBERS GIVEN
                JSR         L95AC           ; RESET SAM DISPLAY PAGE AND VOG MODE
D17C            JSR         LD169           ; GET SOURCE DRIVE NUMBER AND SAVE
                STB         DBUF0+255       ; IT AT TOP OF DBUF0 (TOP OF NEW STACK)
                JSR         GETCCH          ; GET A CHARACTER FROM BASIC
                BEQ         LD18E           ; BRANCH IF END OF LINE
                LDB         #$A5            ; TOKEN FOR 'TO'
                JSR         LB26F           ; SYNTAX CHECK FOR 'TO'
D18B            JSR         LD169           ; GET DESTINATION DRIVE NUMBER
LD18E           LDS         #DBUF0+255      ; PUT STACK AT TOP OF DBUF0
                PSHS        B               ; SAVE DESTINATION DRIVE NUMBER ON STACK
                JSR         LA5C7           ; SYNTAX ERROR IF NOT END OF LINE
                JSR         DVEC7           ; CLOSE ALL FILES
                CLR         ,-S             ; CLEAR A TRACK COUNTER ON STACK
                LDX         #DFLBUF-1       ; POINT X TO TOP OF DISK RAM VARIABLES
LD19F           INC         ,S              ; INCREMENT TRACK COUNTER
                LEAX        SECMAX*SECLEN,X ; INCREMENT X BY ONE TRACK
                CMPX        MEMSIZ          ; COMPARE TO TOP OF NON RESERVED RAN
                BLS         LD19F           ; KEEP GOING IF MORE FREE RAM LEFT
                DEC         ,S              ; DECREMENT TRACK COUNTER
                LBEQ        LAC44           ; 'OM' ERROR IF < 1 TRACK OF FREE RAM
                LDA         #TRKMAX         ; GET MAXIMUM NUMBER OF TRACKS INITIALIZE REMAINING TRACKS CTR
                CLRB                        ; INITIALIZE TRACKS WRITTEN COUNTER TO ZERO
                PSHS        B,A             ; SAVE TRACKS WRITTEN AND REMAINING COUNTERS ON STACK

; AT THIS POINT THE STACK HAS THE FOLLOWING DATA ON IT:
; ,S = TRACKS REMAINING COUNTER; 1,S = TRACKS WRITTEN COUNTER
; 2,S = NUMBER OF TRACKS WHICH FIT IN RAM; 3,S = DESTINATION DRIVE NUMBER
; 4,S = SOURCE DRIVE NUMBER
                COM         DRESFL          ; SET THE DISK RESET FLAG TO CAUSE A RESET
LD1B7           CLRB                        ; INITIALIZE WRITE TRACK COUNTER TO ZERO
LD1B8           INCB                        ; ADD ONE TO WRITE TRACK COUNTER
                DEC         ,S              ; DECREMENT REMAINING TRACKS COUNTER
                BEQ         LD1C1           ; AND BRANCH IF NO TRACKS LEFT
                CMPB        $02,S           ; = COMPARE WRITE TRACK COUNTER TO NUMBER OF TRACKS THAT
                BNE         LD1B8           ; = WILL FIT IN RAM AND BRANCH IF ROOM FOR MORE TRACKS IN RAM
LD1C1           STB         TMPLOC          ; SAVE THE NUMBER OF TRACKS TO BE TRANSFERRED
                LDB         $04,S           ; GET SOURCE DRIVE NUMBER
                BSR         LD20F           ; FILL RAM BUFFER WITH TMPLOC TRACKS OF DATA
                LDA         #$FF            ; SET SOURCE/DESTINATION FLAG TO DESTINATION
D1C9            JSR         LD235           ; PRINT PROMPT MESSAGE IF NEEDED
                LDB         $03,S           ; GET DESTINATION DRIVE NUMBER
                BSR         LD212           ; WRITE TMPLOC TRACKS FROM BUFFER
                TST         ,S              ; TEST TRACKS REMAINING FLAG
                BEQ         LD1E0           ; BRANCH IF BACKUP DONE
                CLRA                        ; SET SOURCE/DESTINATION FLAG TO SOURCE
D1D5            JSR         LD235           ; PRINT PROMPT MESSAGE IF NEEDED
                LDB         $01,S           ; GET THE TRACKS WRITTEN COUNTER, ADD THE NUMBER OF
                ADDB        TMPLOC          ; TRACKS MOVED THIS TIME THROUGH LOOP AND
                STB         $01,S           ; SAVE THE NEW TRACKS WRITTEN COUNTER
                BRA         LD1B7           ; COPY SOME MORE TRACKS

LD1E0           BSR         LD1E5           ; CHECK FOR DOS INITIALIZATION
                JMP         LAC73           ; JUMP BACK TO BASICS MAIN LOOP

LD1E5           PULS        U               ; PUT THE RETURN ADDRESS IN U
                LDA         DRESFL          ; TEST DISK RESET FLAG
                BEQ         LD202           ; DONT RESET THE DOS IF FLAG NOT SET
                LDX         #FCBV1          ; POINT X TO TABLE OF FCB ADDRESSES
                CLRA                        ; SET FILE COUNTER TO ZERO
LD1F0           CLR         [,X++]          ; MARK FCB AS CLOSED
                INCA                        ; ADD ONE TO FILE COUNTER
                CMPA        FCBACT          ; COMPARE TO NUMBER OF RESERVED FILES
                BLS         LD1F0           ; BRANCH IF ANY FILES NOT SHUT DOWN
                LDX         TXTTAB          ; LOAD X WITH THE START OF BASIC
                CLR         -1,X            ; SET FIRST BYTE OF BASIC PROGRAM TO ZERO
                JSR         LAD19           ; GO DO A 'NEW'
                CLR         DRESFL          ; RESET THE DOS RESET FLAG
LD202           LDA         DLODFL          ; CHECK THE LOAD RESET FLAG AND
                BEQ         LD20D           ; BRANCH IF NOT SET
                CLR         DLODFL          ; CLEAR THE LOAD RESET FLAG
                JSR         LAD19           ; GO DO A 'NEW'
LD20D           JMP         ,U              ; JUMP BACK TO RETURN ADDRESS SAVED IN U ABOVE

LD20F           LDA         #$02            ; READ OP CODE
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
LD212           LDA         #$03            ; WRITE OP CODE
                STD         DCOPC           ; SAVE IN DSKCON VARIABLE
                LDA         $03,S           ; GET THE NUMBER OF THE TRACK BEING CURRENTLY
                STA         DCTRK           ; WRITTEN AND SAVE IT IN DSKCON VARIABLE
                LDX         #DFLBUF         ; = TRACK BUFFER STARTS AT DFLBUF
                STX         DCBPT           ; = SAVE IT IN DSKCON VARIABLE
                LDA         TMPLOC          ; GET NUMBER OF TRACKS TO MOVE
LD221           LDB         #$01            ; INITIALIZE SECTOR COUNTER TO ONE
LD223           STB         DSEC            ; SAVE DSKCON SECTOR VARIABLE
                JSR         LD5FF           ; READ/WRITE A SECTOR
                INC         DCBPT           ; MOVE BUFFER POINTER UP ONE SECTOR (256 BYTES)
                INCB                        ; INCREMENT SECTOR COUNTER
                CMPB        #SECMAX         ; COMPARE TO MAXIMUM NUMBER OF SECTORS PER TRACK
                BLS         LD223           ; BRANCH IF ANY SECTORS LEFT
                INC         DCTRK           ; INCREMENT TRACK COUNTER VARIABLE TO NEXT TRACK
                DECA                        ; DECREMENT TRACKS TO MOVE COUNTER
                BNE         LD221           ; READ MORE TRACKS IF ANY LEFT
                RTS

LD235           LDB         $05,S           ; GET THE DESTINATlON DRIVE NUMBER AND
                CMPB        $06,S           ; COMPARE IT TO THE SOURCE DRIVE NUMBER

; PRINT SOURCE/DESTINATION DISK SWITCH PROMPT MESSAGE
LD239           BNE         LD271           ; RETURN IF DRIVE NUMBERS NOT EQUAL
                CLR         RDYTMR          ; RESET THE READY TIMER
                CLR         DSKREG          ; CLEAR DSKREG - TURN OFF ALL DISK MOTORS
                CLR         DRGRAM          ; CLEAR DSKREG RAM IMAGE
                PSHS        A               ; SAVE SOURCE/DESTINATION FLAG ON STACK
                JSR         >CLRSCRN        ; CLEAR SCREEN
                LDX         #LD272          ; POINT X TO 'INSERT SOURCE' MESSAGE
                LDB         #13             ; 13 BYTES IN MESSAGE
                LDA         ,S+             ; GET SOURCE/DESTINATION FLAG FROM THE STACK
                BEQ         LD257           ; BRANCH IF SOURCE
                LDX         #LD27F          ; POINT X TO 'INSERT DESTINATION' MESSAGE
                LDB         #18             ; 18 BYTES IN MESSAGE
LD257           JSR         LB9A2           ; SEND MESSAGE TO CONSOLE OUT
                LDX         #LD291          ; POINT X TO 'DISKETTE AND' MESSAGE
                LDB         #27             ; 27 BYTES IN MESSAGE
                JSR         LB9A2           ; SEND MESSAGE TO CONSOLE OUT
                LDD         #$6405          ; SET UP 'SOUND' PARAMETERS
                STA         SNDTON          ; FOR A BEEP
                JSR         LA951           ; JUMP TO 'SOUND' - DO A BEEP
LD26A           JSR         LA171           ; GET A CHARACTER FROM CONSOLE IN
                CMPA        #CR             ; KEEP LOOKING AT CONSOLE IN UNTIL
                BNE         LD26A           ; YOU GET A CARRIAGE RETURN
LD271           RTS

LD272           FCC         'INSERT SOURCE'


LD27F           FCC         'INSERT DESTINATION'


LD291           FCC         ' DISKETTE AND'


                FCB         CR
                FCC         "PRESS 'ENTER'"



; PUSH FILENAME.EXT AND DRIVE NUMBER ONTO THE STACK
LD2AC           PULS        Y               ; SAVE RETURN ADDRESS IN Y
                LDB         #11             ; 11 CHARACTERS IN FILENAME AND EXTENSION
                LDX         #DNAMBF+11      ; POINT X TO TOP OF DISK NAME/EXT BUFFER
LD2B3           LDA         ,-X             ; GET A CHARACTER FROM FILENAME.
                PSHS        A               ; EXT BUFFER AND PUSH IT ONTO THE
                DECB                        ;  STACK - DECREMENT COUNTER AND
                BNE         LD2B3           ; KEEP LOOPING UNTIL DONE
                LDA         DCDRV           ; = GET DRIVE NUMBER AND PUSH
                PSHS        A               ; = IT ONTO THE STACK
                JMP         ,Y              ; PSEUDO - RETURN TO CALLING ROUTINE

; PULL FILENAME.EXT AND DRIVE NUMBER FROM (X) TO RAM
LD2C0           LDA         ,X+             ; GET DRIVE NUMBER AND SAVE
                STA         DCDRV           ; IT IN DSKCON VARIABLE
                LDB         #11             ; 11 BYTES IN FILENAME AND EXTENSION
                LDU         #DNAMBF         ; POINT U TO DISK NAME BUFFER
                JMP         LA59A           ; MOVE FILENANE.EXT FROM (X) TO DNAMBF

; COPY
; THE COPY PROCESS IS PERFORMED BY COPYING DATA FROM THE SOURCE FILE
; TO RAM AND THEN COPYING IT TO THE DESTINATION FILE. THE SOURCE AND
; DESTINATION FILES ARE OPENED AS RANDOM FILES AND BOTH USE THE SYSTEM
; FCB ABOVE THE RESERVED FCBS. ALL OF AVAILABLE FREE RAM ABOVE THE
; VARIABLES IS USED AS A COPY BUFFER WHICH SPEEDS UP THE COPYING PROCESS
; BUT UNFORTUNATELY THE METHOD USED WILL ALLOW AN ERROR ENCOUNTERED DURING
; THE COPY PROCESS TO 'HANG' THE SYSTEM. THIS IS CAUSED BY POINTING THE FCB'S
; RANDOM FILE BUFFER POINTER (FCBBUF,X) TO THE FREE RAM BUFFER. AN ERROR
; WILL THEN CAUSE THE OPEN FILE TO BE CLOSED WITH FCBBUF,X POINTING TO AN
; AREA IN RAM WHERE THE RANDOM FILE BUFFER CLOSE ROUTINE (LCAE2) WILL NEVER
; LOOK FOR IT
COPY            JSR         LC887           ; GET SOURCE FILENAME.EXT & DRIVE NUMBER FROM BASIC
                BSR         LD2AC           ; AND SAVE THEM ON THE STACK
                CLR         ,-S             ; CLEAR A BYTE ON STACK - SINGLE DISK COPY (SDC) FLAG
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         LD2E1           ; BRANCH IF END OF LINE - SINGLE DISK COPY
                COM         ,S              ; SET SOC FLAG TO $FF (NO SINGLE DISK COPY)
                LDB         #$A5            ; TOKEN FOR 'TO'
                JSR         LB26F           ; SYNTAX CHECK FOR 'TO'
                JSR         LC887           ; GET DESTINATION FILENAME.EXT AND DRIVE NUMBER
LD2E1           BSR         LD2AC           ; SAVE DESTINATION FILENAME.EXT & DRIVE NUMBER ON STACK
                JSR         LA5C7           ; SYNTAX ERROR IF MORE CHARACTERS ON LINE
                JSR         DVEC7           ; CLOSE ALL FILES

; COUNT THE NUMBER OF SECTORS WORTH OF FREE RAM AVAILABLE
                CLR         ,-S             ; CLEAR A SECTOR COUNTER ON THE STACK
                LEAX        -100,S          ; BUG ; * THIS SHOULD BE $100 TO POINT X ONE SECTOR LENGTH BELOW STACK
LD2EE           INC         ,S              ; INCREMENT SECTOR COUNTER
                LEAX        -SECLEN,X       ; DECREMENT X BY ONE SECTOR
                CMPX        ARYEND          ; COMPARE TO TOP OF ARRAYS
                BHS         LD2EE           ; BRANCH IF NOT AT BOTTOM OF FREE RAM
                DEC         ,S              ; DECREMENT SECTOR COUNTER
                LBEQ        LAC44           ; 'OM' ERROR IF NOT AT LEAST ONE FULL SECTOR OF FREE RAM
                LEAX        14,S            ; POINT X TO START OF SOURCE DATA
                BSR         LD2C0           ; PUT SOURCE DATA INTO DNAMBF AND DSKCON
                JSR         LC65F           ; SCAN DIRECTORY FOR A MATCH
                JSR         LC6B8           ; 'NE' ERROR IF MATCH NOT FOUND
                LDX         V974            ; POINT X TO DIRECTORY RAM IMAGE OF FOUND FILE
                LDU         DIRLST,X        ; GET NUMBER OF BYTES IN LAST SECTOR AND
                LDX         DIRTYP,X        ; SOURCE FILE TYPE AND ASCII FLAG
                PSHS        U,X             ; AND SAVE THEM ON THE STACK
                JSR         LC76D           ; GET VALID FAT DATA
                LDB         V976            ; GET NUMBER OF FIRST GRANULE IN FILE
                JSR         LCC44           ; GET THE NUMBER OF GRANULES IN FILE
                PSHS        A               ; AND SAVE IT ON THE STACK
                DECA                        ; SUBTRACT OFF THE LAST GRANULE
                ANDB        #$3F            ; MASK OFF LAST GRANULE FLAG BITS AND SAVE THE
                PSHS        B               ; NUMBER OF SECTORS IN LAST GRANULE ON STACK
                TFR         A,B             ; SAVE THE NUMBER OF GRANULES IN ACCB
                CLRA                        ; CLEAR THE MS BYTE OF ACCD
                JSR         LC749           ; MULTIPLY ACCD BY NINE
                ADDB        ,S              ; ADD THE NUMBER OF SECTORS IN THE LAST
                ADCA        #$00            ; GRANULE TO ACCD
                LDX         #$0001          ; INITIALIZE RECORD COUNTER TO ONE
                PSHS        X,B,A           ; INITIALIZE SECTOR AND RECORD COUNTERS ON THE STACK

; AT THIS POINT THE CONTROL VARIABLES FOR COPY ARE STORED ON THE STACK.
; 0 1,S = REMAINING SECTORS COUNTER; 2 3,S = RECORD COUNTER
; 4,S = NUMBER OF SECTORS TO BE COPIED. INITIALLY SET TO NUMBER OF
; SECTORS IN THE LAST GRANULE.
; 5,S = GRAN TEST FLAG. INITIALLY SET TO NUMBER OF GRANS IN FILE
; 6,S = FILE TYPE; 7,S = ASCII FLAG; 8 9,S = NUMBER OF BYTES IN LAST SECTOR
; 10,S = NUMBER OF SECTORS WHICH WILL FIT IN THE CURRENTLY AVAILABLE FREE RAM
; 11-22,S = DESTINATION FILENAME.EXT AND DRIVE NUMBER
; 23,S = SINGLE DISK COPY FLAG; 24-35,S = SOURCE FILENAME.EXT AND DRIVE NUMBER
LD330           CLRB                        ; SET SECTOR COUNTER TO ZERO
                LDX         ,S              ; GET THE NUMBER OF SECTORS REMAINING IN THE FILE
                BEQ         LD33E           ; BRANCH IF NO SECTORS LEFT
LD335           INCB                        ; ADD A SECTOR TO TEMPORARY SECTOR COUNTER
                LEAX        -1,X            ; DECREMENT REMAINING SECTORS COUNTER
                BEQ         LD33E           ; BRANCH IF NO SECTORS LEFT
                CMPB        10,S            ; COMPARE TEMPORARY COUNTER TO NUMBER OF SECTORS WHICH MAY
; BE STORED IN FREE RAM
                BNE         LD335           ; BRANCH IF STILL ROOM FOR MORE SECTORS
LD33E           STX         ,S              ; SAVE THE NUMBER OF UNCOPIED SECTORS REMAINING IN THE FILE
                STB         $04,S           ; SAVE THE NUMBER OF SECTORS TO BE COPIED THIS TIME THROUGH LOOP
                BSR         LD394           ; 'GET' ACCB SECTORS TO RAM BUFFER
                LDA         #$FF            ; SET SOURCE/DESTINATION FLAG TO DESTINATION
                BSR         LD388           ; PRINT PROMPT MESSAGE IF REQUIRED
                TST         $05,S           ; CHECK THE GRAN TEST FLAG. IF <> 0, IT CONTAINS THE
                BEQ         LD371           ; NUMBER OF GRANS IN THE FILE AND THE DESTINATION DISK
; MUST BE CHECKED FOR ENOUGH ROOM. IF IT IS = 0
; THEN THE CHECK HAS ALREADY BEEN DONE
                LEAX        11,S            ; POINT TO DESTINATION FILE PARAMETERS
                JSR         LD2C0           ; GET DESTINATION FILE PARAMETERS FROM STACK
                JSR         LCF7D           ; SCAN DIRECTORY FOR FILE - 'AE' ERROR IF IT EXISTS
                JSR         LC76D           ; GET VALID FAT DATA

; MAKE SURE THERE ARE ENOUGH FREE GRANULES ON THE DESTINATION DISK
                JSR         LC725           ; POINT X TO FAT
                LEAX        FATCON,X        ; SKIP PAST THE FAT CONTROL BYTES
                LDA         $05,S           ; GET THE NUMBER OF GRANS IN THE FILE
                LDB         #GRANMX         ; SET GRAN COUNTER TO MAXIMUM
LD360           COM         ,X              ; CHECK TO SEE IF A BRAN IS FREE
                BNE         LD367           ; AND BRANCH IF IT IS NOT FREE
                DECA                        ; = DECREMENT COUNTER AND BRANCH IF
                BEQ         LD36F           ; = THERE ARE ENOUGH FREE GRANULES
LD367           COM         ,X+             ; RESTORE FAT BYTE AND INCREMENT POINTER
                DECB                        ; DECREMENT GRAN COUNTER
                BNE         LD360           ; BRANCH IF ALL GRANS NOT CHECKED
                JMP         LC7C8           ; 'DISK FULL' ERROR
LD36F           COM         ,X              ; RESTORE FAT BYTE
LD371           BSR         LD38E           ; 'PUT' DATA FROM RAM BUFFER TO DESTINATION FILE
                LDX         ,S              ; GET THE NUMBER OF REMAINING SECTORS
                BEQ         LD384           ; EXIT ROUTINE IF NO SECTORS LEFT
                LDD         $02,S           ;
                ADDB        $04,S           ; GET THE CURRENT RECORD COUNTER, ADD
                ADCA        #$00            ; THE NUMBER OF SECTORS (RECORDS) MOVED
                STD         $02,S           ; AND SAVE THE NEW RECORD COUNTER
                CLRA                        ; SET SOURCE/DESTINATION FLAG TO SOURCE
                BSR         LD388           ; PRINT PROMPT MESSAGE IF REQUIRED
                BRA         LD330           ; KEEP COPYING SECTORS

LD384           LEAS        36,S            ; REMOVE TEMPORARY STORAGE VARIABLES FROM STACK
                RTS                         ; COPY DONE ; ***

LD388           TST         25,S            ; CHECK SINGLE DISK COPY FLAG - IF <> ZERO, THEN DON'T
; PRINT THE PROMPT MESSAGE
                JMP         LD239           ; PRINT THE PROMPT MESSAGE IF REQUIRED
LD38E           LDA         #$FF

; 'PUT'.'GET' DATA FROM THE DESTINATION/SOURCE FILES'PUT' FLAG
                LEAX        13,S            ; POINT X TO DESTINATION FILENAME DATA
                BRA         LD398           ; GO 'PUT' SOME DATA
LD394           CLRA                        ; ZERO IS THE 'GET' FLAG
                LEAX        26,S            ; POINT X TO THE SOURCE FILENAME DATA
LD398           STA         VD8             ; SAVE THE 'GET'/'PUT' FLAG
                JSR         LD2C0           ; GET FILENAME AND DRIVE DATA FROM THE STACK
                LDX         8,S             ; GET ASCII FLAG AND FILE TYPE AND SAVE
                STX         DFLTYP          ; THEM IN THE DISK RAM VARIABLES
                LDX         #SECLEN         ; = SAVE ONE SECTOR LENGTH IN
                STX         DFFLEN          ; = RAM RECORD LENGTH VARIABLE
                LDA         #'R'            ; RANDOM FILE TYPE FLAG
                LDB         FCBACT          ; GET THE HIGHEST RESERVED FCB NUMBER, ADD ONE
                INCB                        ;  AND OPEN A RANDOM FILE WHOSE FCB WILL BE ONE ABOVE
                JSR         LC468           ; THE HIGHEST RESERVED FCB (THE SYSTEM FCB)
                LDX         FCBTMP          ; POINT X TO THE 'SYSTEM' FCB
                LDD         #SECLEN         ; SET THE NUMBER OF BYTES IN THE LAST SECTOR
                STD         FCBLST,X        ; OF THE FILE EQUAL TO ONE SECTOR LENGTH
                LDB         $06,S           ; =GET THE NUMBER OF SECTORS TO MOVE AND
                BEQ         LD3E6           ; =BRANCH IF NONE LEFT
                LDB         VD8             ; GRAB THE 'GET'/'PUT' FLAG, 'AND' IT WITH THE
                ANDB        $07,S           ; GRAN TEST FLAG - BRANCH IF 'GET'ING DATA OR THIS IS
                BEQ         LD3CC           ; NOT THE FIRST TIME THROUGH THE LOOP
                LDD         $02,S           ; =GET THE NUMBER OF SECTORS REMAINING TO BE COPIED AND
                ADDB        $06,S           ; =ADD THE NUMBER TO BE COPIED THIS TIME THROUGH LOOP
                ADCA        #$00            ; =
                JSR         LC2CC           ; 'PUT' THE LAST RECORD IN THE FILE TO THE SYSTEM FCB.
LD3CC           LDX         FCBTMP          ; THE RECORD NUMBER IS IN ACCD.
; POINT X TO THE SYSTEM FCB
                LDU         $04,S           ; GET THE CURRENT RECORD NUMBER
                STU         FCBREC,X        ; AND SAVE IT IN THE FCB
                LDB         $06,S           ; GET THE NUMBER OF THE RECORD (SECTOR) TO MOVE
                LDU         ARYEND          ; END OF ARRAYS IS THE START OF THE COPY FREE RAM BUFFER
LD3D6           PSHS        U,B             ; SAVE SECTOR COUNTER AND BUFFER POINTER ON THE STACK
                LDX         FCBTMP          ; POINT X TO SYSTEM FCB
                STU         FCBBUF,X        ; SET THE RANDOM FILE BUFFER POINTER TO THE 'COPY' RAM BUFFER
                JSR         LC2D0           ; THIS WILL CAUSE THE SYSTEM TO 'HANG' IF AN ERROR OCCURS DURING COPY.
; GO 'GET' OR 'PUT' DATA TO THE SYSTEM FCB
                INC         $01,S           ; ADD 256 (ONE SECTOR) TO THE BUFFER POINTER
                PULS        B,U             ; GET THE SECTOR COUNTER AND BUFFER POINER
                DECB                        ; DECREMENT SECTOR COUNTER
                BNE         LD3D6           ; BRANCH IF ALL SECTORS NOT DONE
LD3E6           LDX         FCBTMP          ; POINT X TO SYSTEM FCB
                LDU         #DFLBUF         ; RESET THE RANDOM FILE BUFFER POINTER FOR THE SYSTEM
                STU         FCBBUF,X        ; FCB TO THE BOTTOM OF RANDOM FILE BUFFER AREA
                LDB         VD8             ; =GRAB THE 'GET'/'PUT' FLAG, 'AND' IT WITH THE GRAN
                ANDB        $07,S           ; =TEST FLAG - CLOSE THE FILE IF 'GET'ING DATA AND
                BEQ         LD3FC           ; =THIS IS NOT THE FIRST TIME THROUGH THE LOOP
                CLR         $07,S           ; RESET THE GRAN TEST FLAG IF FIRST TIME THROUGH LOOP
                LDD         10,S            ; GET THE NUMBER OF BYTES IN THE LAST SECTOR,
                ORA         #$80            ; 'OR' IN THE PRE-SAVED FLAG AND
                STD         FCBLST,X        ; SAVE THE NUMBER OF BYTES IN THE LAST SECTOR IN THE FCB
LD3FC           JMP         LCA58           ; CLOSE THE FILE

; DSKI$ COMMAND
DSKI            BSR         LD439           ; GET THE DRIVE, TRACK AND SECTOR NUMBERS
                BSR         LD42E           ; EVALUATE STRING VARIABLE 1 AND SAVE
                PSHS        X               ; THE DESCRIPTOR ADDRESS ON THE STACK
                BSR         LD42E           ; = EVALUATE STRING VARIABLE 2 AND SAVE
                PSHS        X               ; = THE DESCRiPTOR ADDRESS ON THE STACK
                LDB         #$02            ; DSKCON READ OP CODE
                JSR         LD4A1           ; REAO A SECTOR INTO DBUF0
                LDU         #DBUF0+128      ; POINT U TO TOP HALF OF DBUF0
                PULS        X               ; GET STRING 2 DESCRIPTOR ADDRESS
                BSR         LD41A           ; PUT STRING 2 INTO STRING SPACE
                LDU         #DBUF0          ; POINT U TO BOTTOM HALF OF DBUF0
                PULS        X               ; GET STRING 1 DESCRIPTOR ADDRESS
LD41A           PSHS        U,X             ; PUT STRING DESCRIPTOR & SOURCE POINTER ON THE STACK
                LDB         #128            ;
                JSR         LB50F           ; RESERVE 128 BYTES IN STRING SPACE
                LEAU        ,X              ; POINT U TO RESERVED STRING SPACE
                PULS        X               ; GET STRING DESCRIPTOR ADDRESS
                STB         ,X              ; SAVE DESCRIPTOR DATA (LENGTH AND ADDRESS)
                STU         $02,X           ; OF THE NEW STRING
                PULS        X               ; GET THE SOURCE (DBUF0) POINTER
LD42B           JMP         LA59A           ; MOVE SECTOR DATA FROM DBUF0 TO STRING SPACE

LD42E           JSR         SYNCOMMA        ; SYNTAX CHECK FOR A COMMA
                LDX         #LB357          ; POINT X TO EVALUATE VARIABLE ROUTINE
                BSR         LD465           ; EVALUATE A VARIABLE
LD436           JMP         LB146           ; 'TM' ERROR IF NUMERIC VARIABLE

; EVALUATE DRIVE, TRACK AND SECTOR NUMBERS
LD439           JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                CMPB        #$03            ; COMPARE TO 3 (HIGHEST DRIVE NUMBER) -
                BHI         LD45C           ; 'FC' ERROR IF ITS > 3
                PSHS        B               ; SAVE DRIVE NUMBER ON THE STACK
                JSR         LB738           ; SYNTAX CHECK FOR COMMA. EVALUATE EXPRESSION (TRACK NUMBER)
                CMPB        #TRKMAX-1       ; CHECK FOR MAXIMUM TRACK NUMBER
                BHI         LD45C           ; 'FC' ERROR IF TRACK NUMBER > 34
                PSHS        B               ; SAVE TRACK NUMBER ON THE STACK
                JSR         LB738           ; SYNTAX CHECK FOR COMMA, EVALUATE EXPRESSION (SECTOR NUMBER)
                STB         DSEC            ; SAVE SECTOR NUMBER IN DSKCON VARIABLE
                DECB                        ;  USELESS INSTRUCTION. NEXT INSTRUCTION SHOULD JUST
                CMPB        #SECMAX-1       ; CHECK FOR MAXIMUM SECTOR NUMBER (SECMAX)
                BHI         LD45C           ; 'FC' ERROR IF SECTOR NUMBER TOO BIG
                PULS        A,B             ; GET TRACK AND DRIVE NUMBER OFF OF
                STA         DCTRK           ; THE STACK AND SAVE IN DSKCON
                STB         DCDRV           ; VARIABLES
                RTS
LD45C           JMP         LB44A           ; JUMP TO 'FC' ERROR

LD45F           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDX         #LB156          ; POINT X TO 'EVALUATE EXPRESSION' ROUTINE ADDRESS
LD465           LDB         DCDRV           ; GET THE DSKCON DRIVE, TRACK AND
                LDU         DCTRK           ; SECTOR VALUES AND SAVE THEM ON THE STACK
                PSHS        U,B             ;
                JSR         ,X              ; GO EVALUATE AN EXPRESSION OR A VARIABLE
                PULS        B,U             ; GET THE DRIVE, TRACK AND SECTOR
                STB         DCDRV           ; NUMBERS OFF OF THE STACK AND PUT
                STU         DCTRK           ; THEM BACK INTO THE DSKCON VARIABLES
                RTS

; DSKO$ COMMAND
DSKO            BSR         LD439           ; GET THE DRIVE, TRACK AND SECTOR NUMBERS
                BSR         LD45F           ; GET THE DESCRIPTOR OF STRING 1
                BSR         LD436           ; 'TM' ERROR IF NUMERIC EXPRESSION
                LDX         FPA0+2          ; GET STRING 1 DESCRIPTOR ADDRESS
                PSHS        X               ; AND SAVE IT ON THE STACK
                BSR         LD45F           ; GET THE DESCRIPTOR OF STRING 2
                JSR         LB654           ; GET LENGTH AND ADDRESS OF STRING 2 AND
                PSHS        X,B             ; SAVE THEM ON THE STACK
                CLRB                        ; SET CLEAR COUNTER TO 256 (FULL SECTOR BUFFER)
                LDX         #DBUF0          ; USE DBUF0 AS THE DSKO$ I/O BUFFER
LD489           CLR         ,X+             ; CLEAR A BYTE IN I/O BUFFER
                DECB                        ; DECREMENT CLEAR COUNTER
                BNE         LD489           ; BRANCH IF ALL 256 BYTES NOT CLEARED
                PULS        B,X             ; GET THE LENGTH AND ADDRESS OF STRING 2
                LDU         #DBUF0+128      ; POINT X TO STRING 2 DESTINATION
                BSR         LD42B           ; MOVE STRING 2 DATA INTO DBUF0
                PULS        X               ; POINT X TO STRING 1 DESCRIPTOR
                JSR         LB659           ; GET THE LENGTH AND ADDRESS OF STRING 1
                LDU         #DBUF0          ; POINT U TO STRING 1 DESTINATION
                BSR         LD42B           ; MOVE STRING 1 DATA INTO DBUF0
                LDB         #$03            ; DSKCON WRITE OP CODE
LD4A1           LDX         #DBUF0          ; POINT X TO I/O BUFFER (DBUF0)
                STX         DCBPT           ;
                STB         DCOPC           ; SAVE NEW DSKCON BUFFER POINTER AND OP CODE VARIABLES
                JMP         LD5FF           ; GO WRITE OUT A SECTOR

; DSKINI COMMAND
DSKINI          LBEQ        LA61F           ; BRANCH TO 'DN' ERROR IF NO DRIVE NUMBER SPECIFIED
                JSR         LD169           ; CALCULATE DRIVE NUMBER
                LDB         #$04            ; SKIP FACTOR DEFAULT VALUE
                JSR         GETCCH          ; GET CURRENT INPUT CHAR FROM BASiC
                BEQ         LD4C4           ; BRANCH IF END OF LINE
                JSR         LB738           ; SYNTAX CHECK FOR COMMA AND EVALUATE EXPRESSION
                CMPB        #17             ; MAX VALUE OF SKIP FACTOR = 16
                LBHS        LB44A           ; 'ILLEGAL FUNCTION CALL' IF BAD SKIP FACTOR
                JSR         LA5C7           ; SYNTAX ERROR IF MORE CHARACTERS ON THE LINE
LD4C4           PSHS        B               ; SAVE SKIP FACTOR ON THE STACK
                LDX         #DBUF1+SECMAX   ; POINT TO END OF LOGICAL SECTOR NUMBER STORAGE AREA
                LDB         #SECMAX         ; 18 SECTORS PER TRACK
LD4CB           CLR         ,-X             ; CLEAR A BYTE IN THE BUFFER
                DECB                        ; CLEARED ALL 18?
                BNE         LD4CB           ; KEEP GOING IF NOT
                CLRA                        ; RESET PHYSICAL SECTOR COUNTER
                BRA         LD4E0           ; START WITH FIRST PHYSICAL SECTOR = 1

; CALCULATE LOGICAL SECTOR NUMBERS
LD4D3           ADDB        ,S              ; ADD SKIP FACTOR TO LOGICAL SECTOR COUNTER
LD4D5           INCB                        ; ADD ONE TO LOGICAL SECTOR COUNTER
LD4D6           SUBB        #SECMAX         ; SUBTRACT MAX NUMBER OF SECTORS
                BHS         LD4D6           ; BRANCH UNTIL 0 > ACCB >= -18
                ADDB        #SECMAX         ; ADD 18, NOW ACCB IS 0-17
                TST         B,X             ; IS ANYTHING STORED HERE ALREADY?
                BNE         LD4D5           ; YES - GET ANOTHER SECTOR
LD4E0           INCA                        ;  INCREMENT PHYSICAL SECTOR NUMBER AND
                STA         B,X             ; SAVE IT IN THE RAM BUFFER
                CMPA        #SECMAX         ; FINISHED WITH ALL SECTORS?
                BLO         LD4D3           ; NO - KEEP GOING
                LEAS        $01,S           ; REMOVE SKIP FACTOR FROM STACK
                LDX         #DFLBUF+$1888-2 ; GET TOP OF RAM USED BY DSKINI
                CMPX        MEMSIZ          ; IS IT > CLEARED AREA?
                LBHI        LAC44           ; 'OUT OF MEMORY' ERROR IF > CLEARED AREA
                JSR         DVEC7           ; CLOSE ALL FILES
                COM         DRESFL          ; SET RESET FLAG TO $FF - THIS WILL CAUSE A DOS RESET
                LDS         #DBUF1+SECLEN   ; SET STACK TO TOP OF DBUF1
                JSR         L95AC           ; RESET SAM TO DISPLAY PAGE ZERO AND ALPHA GRAPHICS
                LDA         #$00            ; YOU COULD DELETE THIS INSTRUCTION AND CHANGE FOLLOWING STA TO CLR
                STA         DCOPC           ; RESTORE HEAD TO TRACK ZERO DSKCON OP CODE
                JSR         LD5FF           ; RESTORE HEAD TO TRACK ZERO
                CLR         RDYTMR          ; RESET THE READY TIMER
                LDA         #$C0            ; FOC READ ADDRESS CODE
                STA         FDCREG          ;
                JSR         LD6DE           ; CHECK DRIVE READY - WAIT UNTIL READY
                LBNE        LD59B           ; BRANCH IF NOT READY - ISSUE AN ERROR
                CLR         DCTRK           ; RESET TRACK NUMBER
                BRA         LD533           ; START THE FORMATTING PROCESS
LD519           CMPA        #22             ; = CHECK FOR TRACK 22 (PRECOMPENSATION)
                BLO         LD525           ; = AND BRANCH IF < TRACK 22 - NO PRECOMP
                LDA         DRGRAM          ; GET THE RAM IMAGE OF DSKREG, 'OR'
                ORA         #$10            ; IN THE PRECOMPENSATION FLAG AND
                STA         DSKREG          ; SEND IT TO DSKREG
LD525           LDA         #$53            ; = GET STEP IN COMMAND
                STA         FDCREG          ; = AND SEND IT TO THE 1793
                EXG         A,A             ; DELAY AFTER ISSUING COMMAND TO 1793
                EXG         A,A             ;
                JSR         LD6DE           ; CHECK DRIVE READY
                BNE         LD59B           ; BRANCH IF NOT READY - ISSUE AN ERROR
LD533           JSR         LD6FD           ; WAIT A WHILE
                BSR         LD5A4           ; BUILD A FORMATTED TRACK IN RAM
                LDY         #FDCREG+3       ; Y POINTS TO 1793 DATA REGISTER
                ORCC        #$50            ; DISABLE INTERRUPTS
                LDX         #LD562          ; GET RETURN ADDRESS AND STORE
                STX         DNMIVC          ; IT IN THE NON MASKABLE INTERRUPT VECTOR
                LDX         #DFLBUF         ; POINT X TO THE FORMATTED TRACK RAM IMAGE
                LDA         FDCREG          ; RESET STATUS OF THE 1793
                LDA         #$FF            ; ENABLE THE NMI FLAG TO VECTOR
                STA         NMIFLG          ; OUT OF AN I/O LOOP UPON AN NMI INTERRUPT
                LDB         #$F4            ; = GET WRITE TRACK COMMAND AND
                STB         FDCREG          ; = SEND TO 1793
                LDA         DRGRAM          ; GET THE DSKREG RAM IMAGE AND 'OR' IN THE
                ORA         #$80            ; FLAG WHICH WILL ENABLE THE 1793 TO HALT
                STA         DSKREG          ; THE 6809. SEND RESULT TO DSKREG
LD55C           LDB         ,X+             ; = GET A BYTE FROM THE FORMATTED TRACK
                STB         ,Y              ; = RAM IMAGE, SEND IT TO THE 1793 AND
                BRA         LD55C           ; = LOOP BACK TO GET ANOTHER BYTE

LD562           LDA         FDCREG          ; GET STATUS
                ANDCC       #$AF            ; ENABLE INTERRUPTS
                ANDA        #$44            ; KEEP ONLY WRITE PROTECT & LOST DATA
                STA         DCSTA           ; AND SAVE IT IN THE DSKCON STATUS BYTE
                BNE         LD59B           ; BRANCH IF ERROR
                INC         DCTRK           ; SKIP TO THE NEXT TRACK
                LDA         DCTRK           ; GET THE TRACK NUMBER
                CMPA        #TRKMAX         ; WAS IT THE LAST TRACK
                BNE         LD519           ; NO - KEEP GOING

; VERIFY THAT ALL SECTORS ARE READABLE
                LDA         #$02            ; = GET THE DSKCON READ OP CODE
                STA         DCOPC           ; = AND SAVE IT IN THE DSKCON VARIABLE
                LDX         #DBUF0          ; POINT THE DSKCON BUFFER POINTER
                STX         DCBPT           ; TO DBUF0
                LDU         #DBUF1          ; POINT U TO THE LOGICAL SECTOR NUMBERS
                CLRA                        ; RESET THE TRACK COUNTER TO ZERO
LD582           STA         DCTRK           ; SET THE DSKCON TRACK VARIABLE
                CLRB                        ; RESET THE SECTOR COUNTER
LD585           LDA         B,U             ; GET THE PHYSICAL SECTOR NUMBER
                STA         DSEC            ; SAVE DSKCON SECTOR VARIABLE
D589            JSR         LD5FF           ; READ A SECTOR
                INCB                        ;  INCREMENT THE SECTOR COUNTER
                CMPB        #SECMAX         ; AND COMPARE IT TO MAXIMUM SECTOR NUMBER
                BLO         LD585           ; AND KEEP LOOPING IF MORE SECTORS LEFT
                LDA         DCTRK           ; = GET THE CURRENT TRACK NUMBER
                INCA                        ; = ADD ONE TO IT, COMPARE TO THE MAXIMUM TRACK
                CMPA        #TRKMAX         ; = NUMBER AND KEEP LOOPING IF
                BLO         LD582           ; = THERE ARE STILL TRACKS TO DO
                JMP         LD1E0           ; GO CHECK FOR A DOS RESET
LD59B           CLR         DRGRAM          ; CLEAR RAM IMAGE OF DSKREG
                CLR         DSKREG          ; CLEAR DSKREG - TURN DISK MOTORS OFF
D5A1            JMP         LD60E           ; PROCESS DRIVES NOT READY ERROR

; BUILD A FORMATTED TRACK OF DATA IN RAM STARTING AT DFLBUF.
LD5A4           LDX         #DFLBUF         ; START TRACK BUFFER AT DFLBUF
                LDD         #$204E          ; GET SET TO WRITE 32 BYTES OF $4E
                BSR         LD5D5           ; GO WRITE GAP IV
                CLRB                        ; RESET SECTOR COUNTER
LD5AD           PSHS        B               ; SAVE SECTOR COUNTER
                LDU         #DBUF1          ; POINT U TO THE TABLE OF LOGICAL SECTORS
                LDB         B,U             ; GET LOGICAL SECTOR NUMBER FROM TABLE AND
                STB         DSEC            ; SAVE IT IN THE DSKCON VARIABLE
                LDU         #LD5E7          ; POINT U TO TABLE OF SECTOR FORMATTING DATA
                LDB         #$03            ; GET FIRST 3 DATA BLOCKS AND
                BSR         LD5DB           ; WRITE THEM TO BUFFER
                LDA         DCTRK           ; = GET TRACK NUMBER AND STORE lT
                STA         ,X+             ; = IN THE RAM BUFFER
                CLR         ,X+             ; CLEAR A BYTE (SIDE NUMBER) IN BUFFER
                LDA         DSEC            ; GET SECTOR NUMBER AND
                STA         ,X+             ; STORE IT IN THE BUFFER
                LDB         #$09            ; = GET THE LAST NINE DATA BLOCKS AND
                BSR         LD5DB           ; = WRITE THEM TO THE BUFFER
                PULS        B               ; GET SECTOR COUNTER
                INCB                        ; NEXT SECTOR
                CMPB        #SECMAX         ; 18 SECTORS PER TRACK
                BLO         LD5AD           ; BRANCH IF ALL SECTORS NOT DONE
                LDD         #$C84E          ; WRITE 200 BYTES OF $4E AT END OF TRACK

; WRITE ACCA BYTES OF ACCB INTO BUFFER
LD5D5           STB         ,X+             ; STORE A BYTE IN THE BUFFER
                DECA                        ; DECREMENT COUNTER
                BNE         LD5D5           ; BRANCH IF ALL BYTES NOT MOVED
                RTS
LD5DB           PSHS        B               ; SAVE THE COUNTER ON THE STACK
                LDD         ,U++            ; GET TWO BYTES OF DATA FROM THE TABLE
                BSR         LD5D5           ; WRITE ACCA BYTES OF ACCB INTO THE BUFFER
                PULS        B               ; GET THE COUNTER BACK, DECREMENT
                DECB                        ;  IT AND BRANCH IF ALL DATA BLOCKS
                BNE         LD5DB           ; NOT DONE
                RTS

; DATA USED TO FORMAT A SECTOR ON THE DISK

; THESE DATA ARE CLOSE TO THE IBM SYSTEM 34 FORMAT FOR 256 BYTE SECTORS.
; DOUBLE DENSITY. THE FORMAT GENERALLY CONFORMS TO THAT SPECIFIED ON THE
; 1793 DATA SHEET. THE GAP SIZES HAVE BEEN REDUCED TO THE MINIMUM
; ALLOWABLE. THE IBM FORMAT USES $40 AS THE FILL CHARACTER FOR THE DATA
; BLOCKS WHILE COLOR DOS USES AN $FF AS THE FILL CHARACTER.
LD5E7           FCB         8,0             ; SYNC FIELD
                FCB         3,$F5
                FCB         1,$FE           ; ID ADDRESS MARK (AM1)
; TRACK, SIDE, AND SECTOR NUMBERS ARE INSERTED HERE
                FCB         1,1             ; SECTOR SIZE (256 BYTE SECTORS)
                FCB         1,$F7           ; CRC REQUEST
                FCB         22,$4E          ; GAP II (POST-ID GAP)
                FCB         12,0            ; SYNC FIELD
                FCB         3,$F5
                FCB         1,$FB           ; DATA ADDRESS MARK (AM2)
                FCB         0,$FF           ; DATA FIELD (256 BYTES)
                FCB         1,$F7           ; CRC REQUEST
                FCB         24,$4E          ; GAP III (POST DATA GAP)


LD5FF           PSHS        B               ; SAVE ACCB
                LDB         #$05            ; 5 RETRIES
                STB         ATTCTR          ; SAVE RETRY COUNT
                PULS        B               ; RESTORE ACCB
LD608           BSR         DSKCON          ; GO EXECUTE COMMAND
                TST         DCSTA           ; CHECK STATUS
                BEQ         LD61B           ; BRANCH IF NO ERRORS
LD60E           LDA         DCSTA           ; GET DSKCON ERROR STATUS
                LDB         #2*30           ; 'WRITE PROTECTED' ERROR
                BITA        #$40            ; CHECK BIT 6 OF STATUS
                BNE         LD618           ; BRANCH IF WRITE PROTECT ERROR
LD616           LDB         #2*20           ; 'I/O ERROR'
LD618           JMP         LAC46           ; JUMP TO ERROR DRIVER
LD61B           PSHS        A               ; SAVE ACCA
                LDA         DCOPC           ; GET OPERATION CODE
                CMPA        #$03            ; CHECK FOR WRITE SECTOR COMMAND
                PULS        A               ; RESTORE ACCA
                BNE         LD64F           ; RETURN IF NOT WRITE SECTOR
                TST         DVERFL          ; CHECK VERIFY FLAG
                BEQ         LD64F           ; RETURN IF NO VERIFY
                PSHS        U,X,B,A         ; SAVE REGISTERS
                LDA         #$02            ; READ OPERATION CODE
                STA         DCOPC           ; STORE TO DSKCON PARAMETER
                LDU         DCBPT           ; POINT U TO WRITE BUFFER ADDRESS
                LDX         #DBUF1          ; ADDRESS OF VERIFY BUFFER
                STX         DCBPT           ; TO DSKCON VARIABLE
                BSR         DSKCON          ; GO READ SECTOR
                STU         DCBPT           ; RESTORE WRITE BUFFER
                LDA         #$03            ; WRITE OP CODE
                STA         DCOPC           ; SAVE IN DSKCON VARIABLE
                LDA         DCSTA           ; CHECK STATUS FOR THE READ OPERATION
                BNE         LD650           ; BRANCH IF ERROR
                CLRB                        ; CHECK 256 BYTES
LD644           LDA         ,X+             ; GET BYTE FROM WRITE BUFFER
                CMPA        ,U+             ; COMPARE TO READ BUFFER
                BNE         LD650           ; BRANCH IF NOT EQUAL
                DECB                        ;  DECREMENT BYTE COUNTER AND
                BNE         LD644           ; BRANCH IF NOT DONE
                PULS        A,B,X,U         ; RESTORE REGISTERS
LD64F           RTS
LD650           PULS        A,B,X,U         ; RESTORE REGISTERS
                DEC         ATTCTR          ; DECREMENT THE VERIFY COUNTER
                BNE         LD608           ; BRANCH IF MORE TRIES LEFT
                LDB         #2*36           ; 'VERIFY ERROR'
                BRA         LD618           ; JUMP TO ERROR HANDLER

; VERIFY COMMAND
VERIFY          CLRB                        ; OFF FLAG = 0
                CMPA        #$AA            ; OFF TOKEN ?
                BEQ         LD667           ; YES
                COMB                        ; ON FLAG = $FF
                CMPA        #$88            ; ON TOKEN
                LBNE        LB277           ; BRANCH TO 'SYNTAX ERROR' IF NOT ON OR OFF
LD667           STB         DVERFL          ; SET VERIFY FLAG
                JMP         GETNCH          ; GET NEXT CHARACTER FROM BASIC

; DSKCON ROUTINE
DSKCON          PSHS        U,Y,X,B,A       ; SAVE REGISTERS
                LDA         #$05            ; GET RETRY COUNT AND
                PSHS        A               ; SAVE IT ON THE STACK
LD672           CLR         RDYTMR          ; RESET DRIVE NOT READY TIMER
                LDB         DCDRV           ; GET DRIVE NUMBER
                LDX         #LD7AA          ; POINT X TO DRIVE ENABLE MASKS
                LDA         DRGRAM          ; GET DSKREG IMAGE
                ANDA        #$A8            ; KEEP MOTOR STATUS, DOUBLE DENSITY. HALT ENABLE
                ORA         B,X             ; 'OR' IN DRIVE SELECT DATA
                ORA         #$20            ; 'OR' IN DOUBLE DENSITY
                LDB         DCTRK           ; GET TRACK NUMBER
                CMPB        #22             ; PRECOMPENSATION STARTS AT TRACK 22
                BLO         LD68B           ; BRANCH IF LESS THAN 22
                ORA         #$10            ; TURN ON WRITE PRECOMPENSATION IF >= 22
LD68B           TFR         A,B             ; SAVE PARTIAL IMAGE IN ACCB
                ORA         #$08            ; 'OR' IN MOTOR ON CONTROL BIT
                STA         DRGRAM          ; SAVE IMAGE IN RAM
                STA         DSKREG          ; PROGRAM THE 1793 CONTROL REGISTER
                BITB        #$08            ; = WERE MOTORS ALREADY ON?
                BNE         LD69F           ; = DON'T WAIT FOR IT TO COME UP TO SPEED IF ALREADY ON
                JSR         LA7D1           ; WAIT A WHILE
                JSR         LA7D1           ; WAIT SOME MORE FOR MOTOR TO COME UP TO SPEED
LD69F           BSR         LD6DE           ; WAIT UNTIL NOT BUSY OR TIME OUT
                BNE         LD6AD           ; BRANCH IF TIMED OUT (DOOR OPEN. NO DISK, NO POWER. ETC.)
                CLR         DCSTA           ; CLEAR STATUS REGISTER
                LDX         #LD7A2          ; POINT TO COMMAND JUMP VECTORS
                LDB         DCOPC           ; GET COMMAND
                ASLB                        ; 2 BYTES PER COMMAND JUMP ADDRESS
                JSR         [B,X]           ; GO DO IT
LD6AD           PULS        A               ; GET RETRY COUNT
                LDB         DCSTA           ; GET STATUS
                BEQ         LD6BE           ; BRANCH IF NO ERRORS
                DECA                        ; DECREMENT RETRIES COUNTER
                BEQ         LD6BE           ; BRANCH IF NO RETRIES LEFT
                PSHS        A               ; SAVE RETRY COUNT ON STACK
                BSR         LD6C5           ; RESTORE HEAD TO TRACK 0
                BNE         LD6AD           ; BRANCH IF SEEK ERROR
                BRA         LD672           ; GO TRY COMMAND AGAIN IF NO ERROR
LD6BE           LDA         #120            ; 120*1/60 = 2 SECONDS (1/60 SECOND FOR EACH IRQ INTERRUPT)
                STA         RDYTMR          ; WAIT 2 SECONDS BEFORE TURNING OFF MOTOR
                PULS        A,B,X,Y,U,PC    ; RESTORE REGISTERS - EXIT DSKCON
; RESTORE HEAD TO TRACK 0
LD6C5           LDX         #DR0TRK         ; POINT TO TRACK TABLE
                LDB         DCDRV           ; GET DRIVE NUMBER
                CLR         B,X             ; ZERO TRACK NUMBER
                LDA         #$03            ; RESTORE HEAD TO TRACK 0, UNLOAD THE HEAD
                STA         FDCREG          ; AT START, 30 MS STEPPING RATE
                EXG         A,A             ; =
                EXG         A,A             ; = WAIT FOR 1793 TO RESPOND TO COMMAND
                BSR         LD6DE           ; WAIT TILL DRIVE NOT BUSY
                BSR         LD6FD           ; WAIT SOME MORE
                ANDA        #$10            ; 1793 STATUS : KEEP ONLY SEEK ERROR
                STA         DCSTA           ; SAVE IN DSKCON STATUS
LD6DD           RTS
; WAIT FOR THE 1793 TO BECOME UNBUSY. IF IT DOES NOT BECOME UNBUSY,
; FORCE AN INTERRUPT AND ISSUE A DRIVE NOT READY 1793 ERROR.
LD6DE           LDX         ZERO            ; GET ZERO TO X REGISTER - LONG WAIT
LD6E0           LEAX        -1,X            ; DECREMENT LONG WAIT COUNTER
                BEQ         LD6EC           ; lF NOT READY BY NOW, FORCE INTERRUPT
                LDA         FDCREG          ; GET 1793 STATUS AND TEST
                BITA        #$01            ; BUSY STATUS BIT
                BNE         LD6E0           ; BRANCH IF BUSY
                RTS
LD6EC           LDA         #$D0            ; FORCE INTERRUPT COMMAND - TERMINATE ANY COMMAND
                STA         FDCREG          ; IN PROCESS. DO NOT GENERATE A 1793 INTERRUPT REQUEST
                EXG         A,A             ; WAIT BEFORE READING 1793
                EXG         A,A             ;
                LDA         FDCREG          ; RESET INTRQ (FDC INTERRUPT REQUEST)
                LDA         #$80            ; RETURN DRIVE NOT READY STATUS IF THE DRIVE DID NOT BECOME UNBUSY
                STA         DCSTA           ; SAVE DSKCON STATUS BYTE
                RTS
; MEDIUM DELAY
LD6FD           LDX         #8750           ; DELAY FOR A WHILE
LD700           LEAX        -1,X            ; DECREMENT DELAY COUNTER AND
                BNE         LD700           ; BRANCH IF NOT DONE
                RTS
; READ ONE SECTOR
LD705           LDA         #$80            ; $80 IS READ FLAG (1793 READ SECTOR)
                FCB         $8C             ; SKIP TWO BYTES (THROWN AWAY CMPX INSTRUCTION)
; WRITE ONE SECTOR
LD708           LDA         #$A0            ; $A0 IS WRITE FLAG (1793 WRITE SECTOR)
                PSHS        A               ; SAVE READ/WRITE FLAG ON STACK
                LDX         #DR0TRK         ; POINT X TO TRACK NUMBER TABLE IN RAM
                LDB         DCDRV           ; GET DRIVE NUMBER
                ABX                         ; POINT X TO CORRECT DRIVE'S TRACK BYTE
                LDB         ,X              ; GET TRACK NUMBER OF CURRENT HEAD POSITION
                STB         FDCREG+1        ; SEND TO 1793 TRACK REGISTER
                CMPB        DCTRK           ; COMPARE TO DESIRED TRACK
                BEQ         LD739           ; BRANCH IF ON CORRECT TRACK
                LDA         DCTRK           ; GET TRACK DESIRED
                STA         FDCREG+3        ; SEND TO 1793 DATA REGiSTER
                STA         ,X              ; SAVE IN RAM TRACK IMAGE
                LDA         #$17            ; SEEK COMMAND FOR 1793: DO NOT LOAD THE
                STA         FDCREG          ; HEAD AT START, VERIFY DESTINATION TRACK,
                EXG         A,A             ; 30 MS STEPPING RATE - WAIT FOR
                EXG         A,A             ; VALID STATUS FROM 1793
                BSR         LD6DE           ; WAIT TILL NOT BUSY
                BNE         LD737           ; RETURN IF TIMED OUT
                BSR         LD6FD           ; WAIT SOME MORE
                ANDA        #$18            ; KEEP ONLY SEEK ERROR OR CRC ERROR IN ID FIELD
                BEQ         LD739           ; BRANCH IF NO ERRORS - HEAD ON CORRECT TRACK
                STA         DCSTA           ; SAVE IN DSKCON STATUS
LD737           PULS        A,PC
; HEAD POSITIONED ON CORRECT TRACK
LD739           LDA         DSEC            ; GET SECTOR NUMBER DESIRED
                STA         FDCREG+2        ; SEND TO 1793 SECTOR REGISTER
                LDX         #LD798          ; POINT X TO ROUTINE TO BE VECTORED
                STX         DNMIVC          ; TO BY NMI UPON COMPLETION OF DISK I/O AND SAVE VECTOR
                LDX         DCBPT           ; POINT X TO I/O BUFFER
                LDA         FDCREG          ; RESET INTRQ (FDC INTERRUPT REQUEST)
                LDA         DRGRAM          ; GET DSKREG IMAGE
                ORA         #$80            ; SET FLAG TO ENABLE 1793 TO HALT 6809
                PULS        B               ; GET READ/WRITE COMMAND FROM STACK
                LDY         ZERO            ; ZERO OUT Y - TIMEOUT INITIAL VALUE
                LDU         #FDCREG         ; U POINTS TO 1793 INTERFACE REGISTERS
                COM         NMIFLG          ; NMI FLAG = $FF: ENABLE NMI VECTOR
                ORCC        #$50            ; DISABLE FIRQ,IRQ
                STB         FDCREG          ; SEND READ/WRITE COMMAND TO 1793: SINGLE RECORD, COMPARE
                EXG         A,A             ; FOR SIDE 0, NO 15 MS DELAY, DISABLE SIDE SELECT
                EXG         A,A             ; COMPARE, WRITE DATA ADDRESS MARK (FB) - WAIT FOR STATUS
                CMPB        #$80            ; WAS THIS A READ?
                BEQ         LD782           ; IF SO, GO LOOK FOR DATA
; WAIT FOR THE 1793 TO ACKNOWLEDGE READY TO WRITE DATA
                LDB         #$02            ; DRQ MASK BIT
LD768           BITB        ,U              ; IS 1793 READY FOR A BYTE? (DRQ SET IN STATUS BYTE)
                BNE         LD778           ; BRANCH IF SO
                LEAY        -1,Y            ; DECREMENT WAIT TIMER
                BNE         LD768           ; KEEP WAITING FOR THE 1793 DRQ
LD770           CLR         NMIFLG          ; RESET NMI FLAG
                ANDCC       #$AF            ; ENABLE FIRQ,IRQ
                JMP         LD6EC           ; FORCE INTERRUPT, SET DRIVE NOT READY ERROR

; WRITE A SECTOR
LD778           LDB         ,X+             ; GET A BYTE FROM RAM
                STB         FDCREG+3        ; SEND IT TO 1793 DATA REGISTER
                STA         DSKREG          ; REPROGRAM FDC CONTROL REGISTER
                BRA         LD778           ; SEND MORE DATA
; WAIT FOR THE 17933 TO ACKNOWLEDGE READY TO READ DATA
LD782           LDB         #$02            ; DRQ MASK BIT
LD784           BITB        ,U              ; DOES THE 1793 HAVE A BYTE? (DRQ SET IN STATUS BYTE)
                BNE         LD78E           ; YES, GO READ A SECTOR
                LEAY        -1,Y            ; DECREMENT WAIT TIMER
                BNE         LD784           ; KEEP WAITING FOR 1793 DRQ
                BRA         LD770           ; GENERATE DRIVE NOT READY ERROR

; READ A SECTOR
LD78E           LDB         FDCREG+3        ; GET DATA BYTE FROM 1793 DATA REGISTER
                STB         ,X+             ; PUT IT IN RAM
                STA         DSKREG          ; REPROGRAM FDC CONTROL REGISTER
                BRA         LD78E           ; KEEP GETTING DATA
; BRANCH HERE ON COMPLETION OF SECTOR READ/WRITE
LD798           ANDCC       #$AF            ; ENABLE IRQ, FIRO
                LDA         FDCREG          ; GET STATUS & KEEP WRITE PROTECT, RECORD TYPE/WRITE
                ANDA        #$7C            ; FAULT, RECORD NOT FOUND, CRC ERROR OR LOST DATA
                STA         DCSTA           ; SAVE IN DSKCON STATUS
                RTS

; DSKCON OPERATION CODE JUMP VECTORS
LD7A2           FDB         LD6C5           ; RESTORE HEAD TO TRACK ZERO
                FDB         LD6DD           ; NO OP - RETURN
                FDB         LD705           ; READ SECTOR
                FDB         $D708           ; WRITE SECTOR

; DSKREG MASKS FOR DISK DRIVE SELECT
LD7AA           FCB         1               ; DRIVE SEL 0
                FCB         2               ; DRIVE SEL 1
                FCB         4               ; DRIVE SEL 2
                FCB         $40             ; DRIVE SEL 3

; NMI SERVICE
DNMISV          LDA         NMIFLG          ; GET NMI FLAG
                BEQ         LD7BB           ; RETURN IF NOT ACTIVE
                LDX         DNMIVC          ; GET NEW RETURN VECTOR
                STX         10,S            ; STORE AT STACKED PC SLOT ON STACK
                CLR         NMIFLG          ; RESET NMI FLAG
LD7BB           RTI

; IRQ SERVICE
DIRQSV          LDA         PIA0+3          ; 63.5 MICRO SECOND OR 60 HZ INTERRUPT?
                BPL         LD7BB           ; RETURN IF 63.5 MICROSECOND
                LDA         PIA0+2          ; RESET 60 HZ PIA INTERRUPT FLAG
                LDA         RDYTMR          ; GET TIMER
                BEQ         LD7DA           ; BRANCH IF NOT ACTIVE
                DECA                        ; DECREMENT THE TIMER
                STA         RDYTMR          ; SAVE IT
                BNE         LD7DA           ; BRANCH IF NOT TIME TO TURN OFF DISK MOTORS
                LDA         DRGRAM          ; = GET DSKREG IMAGE
                ANDA        #$B0            ; = TURN ALL MOTORS AND DRIVE SELECTS OFF
                STA         DRGRAM          ; = PUT IT BACK IN RAM IMAGE
                STA         DSKREG          ; SEND TO CONTROL REGISTER (MOTORS OFF)
LD7DA           JMP         L8955           ; JUMP TO EXTENDED BASIC'S IRQ HANDLER

; THIS IS THE END OF DISK BASIC.
; THE CODE FROM THIS POINT TO $DFFF IS GARBAGE.
; DOSBAS 1.0 = 2083 WASTED BYTES

			FCB		$00,$00,$00
			FCB		$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			FCB		$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
			FCB		$A7,$FC,$8F,$EC,$94,$BC,$8C,$F5,$BC,$F8,$D1,$FA,$FD,$FD,$AC,$E5
			FCB		$EC,$F8,$8A,$EA,$AD,$EC,$B9,$FF,$BC,$FD,$CC,$ED,$8C,$E5,$9C,$F4
			FCB		$A2,$A6,$E8,$FC,$AE,$E5,$94,$EC,$BE,$EE,$8C,$ED,$EA,$E5,$ED,$7C
			FCB		$BE,$FC,$8E,$ED,$FE,$EC,$9C,$AC,$DC,$FD,$FB,$F5,$AD,$FD,$AF,$E9
			FCB		$A0,$30,$24,$8A,$14,$84,$2C,$C4,$24,$08,$E0,$21,$20,$80,$28,$00
			FCB		$20,$88,$A4,$A0,$A4,$80,$AC,$11,$30,$90,$24,$D4,$01,$A1,$A0,$84
			FCB		$64,$80,$34,$04,$34,$80,$20,$21,$A1,$81,$A4,$80,$20,$90,$88,$C0
			FCB		$00,$9A,$A0,$A0,$E0,$80,$B6,$A4,$20,$A2,$20,$98,$20,$88,$7C,$AE
			FCB		$BD,$EE,$EE,$FE,$F1,$EC,$AD,$BC,$ED,$EC,$DD,$F9,$EC,$75,$B5,$BD
			FCB		$EF,$FD,$A9,$E4,$96,$FC,$94,$E4,$AE,$AD,$DA,$B4,$AC,$7C,$C3,$EC
			FCB		$BD,$FC,$AE,$FD,$EE,$FD,$85,$F4,$AD,$EF,$76,$FC,$D6,$6E,$EF,$ED
			FCB		$F6,$BD,$ED,$FE,$58,$ED,$F7,$FD,$85,$EC,$FC,$E8,$F2,$E4,$BF,$A8
			FCB		$30,$80,$A0,$05,$A4,$20,$B0,$80,$20,$84,$04,$80,$30,$8E,$E0,$A4
			FCB		$B0,$02,$04,$80,$61,$84,$20,$00,$A0,$80,$20,$88,$24,$94,$24,$A4
			FCB		$80,$04,$00,$84,$24,$84,$28,$9C,$44,$28,$A0,$80,$80,$90,$30,$80
			FCB		$E8,$80,$A4,$88,$30,$94,$20,$80,$00,$8C,$68,$AC,$A4,$84,$30,$06
			FCB		$86,$E4,$BC,$FC,$AE,$EC,$87,$BD,$9C,$FD,$EC,$BC,$FD,$B5,$A9,$ED
			FCB		$A7,$AC,$8B,$7C,$A5,$FC,$CC,$BC,$AA,$B6,$C9,$FC,$A5,$FC,$47,$FC
			FCB		$DD,$6D,$84,$FE,$AC,$EE,$20,$F4,$C5,$FF,$F4,$EE,$F7,$F4,$AA,$FC
			FCB		$EF,$FF,$9C,$FD,$EE,$E4,$AF,$FC,$BE,$BC,$DF,$A5,$EB,$EC,$EE,$A4
			FCB		$34,$86,$AC,$84,$A4,$21,$90,$88,$A0,$A0,$24,$C0,$E0,$80,$70,$E5
			FCB		$3C,$A5,$A1,$8C,$34,$A4,$E4,$84,$24,$80,$F8,$81,$A0,$8D,$EC,$4A
			FCB		$2C,$E2,$A4,$93,$20,$88,$B0,$C0,$34,$10,$20,$91,$A4,$84,$A4,$AE
			FCB		$0C,$92,$B0,$CD,$B0,$AA,$74,$E8,$A0,$12,$B9,$85,$21,$F5,$E0,$8E
			FCB		$9E,$BD,$81,$F5,$CF,$E4,$CC,$66,$A7,$AD,$98,$BD,$BC,$BC,$88,$BC
			FCB		$8E,$ED,$A4,$A8,$AD,$88,$EE,$FE,$CD,$FC,$AC,$FD,$8C,$FC,$9A,$F5
			FCB		$B4,$E4,$8E,$FC,$FE,$E4,$CC,$AC,$9D,$FC,$97,$E4,$8B,$EC,$EA,$EC
			FCB		$DB,$EC,$DF,$EC,$AC,$E4,$AE,$FC,$D4,$F4,$CD,$EC,$B4,$EC,$EF,$EE
			FCB		$B2,$24,$28,$20,$AC,$D0,$24,$1C,$30,$81,$AC,$84,$80,$01,$A4,$A4
			FCB		$70,$88,$A4,$C4,$A4,$84,$24,$60,$08,$04,$A0,$88,$20,$98,$64,$A8
			FCB		$24,$10,$D4,$90,$A0,$81,$30,$EC,$AC,$09,$64,$E0,$60,$18,$30,$B0
			FCB		$24,$80,$6C,$88,$A4,$8C,$A4,$8C,$A8,$10,$21,$A8,$68,$84,$A8,$A0
			FCB		$8D,$E5,$A4,$66,$98,$FD,$87,$FC,$DE,$AC,$9C,$F4,$8C,$F5,$B5,$F5
			FCB		$B6,$F5,$A9,$F8,$AF,$ED,$E6,$E5,$B8,$F1,$B1,$FC,$FC,$E9,$8C,$EC
			FCB		$86,$F9,$B5,$F7,$CD,$BC,$84,$E6,$8C,$FC,$81,$E4,$BC,$E4,$4F,$FC
			FCB		$8C,$FC,$AE,$E7,$BD,$FC,$DC,$FC,$F4,$E8,$9F,$EC,$EA,$FC,$EF,$6C
			FCB		$20,$82,$A4,$40,$A8,$8C,$AC,$08,$60,$82,$A1,$95,$E0,$30,$60,$80
			FCB		$64,$48,$20,$04,$20,$84,$A4,$82,$A0,$04,$20,$94,$44,$A0,$E0,$80
			FCB		$04,$C8,$2C,$82,$24,$04,$30,$B8,$24,$00,$20,$A0,$A4,$82,$A0,$80
			FCB		$84,$81,$2C,$84,$20,$80,$24,$8C,$80,$80,$24,$02,$64,$A8,$24,$88
			FCB		$ED,$BD,$D1,$FF,$B4,$E4,$AA,$EC,$80,$F1,$8C,$ED,$89,$AC,$B6,$FC
			FCB		$6E,$FD,$AE,$ED,$89,$EC,$B6,$EC,$AE,$FD,$EE,$F0,$9F,$7E,$9F,$FE
			FCB		$D9,$EC,$AE,$ED,$C6,$EC,$FE,$7D,$BC,$FC,$B9,$AC,$DA,$FC,$A5,$BD
			FCB		$CC,$FD,$AD,$E6,$AE,$F4,$CD,$F4,$F2,$ED,$84,$78,$CC,$EC,$AB,$74
			FCB		$A4,$94,$20,$84,$20,$00,$A0,$00,$A4,$05,$28,$0C,$24,$04,$24,$00
			FCB		$A0,$84,$80,$80,$18,$80,$24,$0C,$E0,$88,$04,$8A,$2C,$88,$0C,$84
			FCB		$00,$80,$E0,$04,$20,$A1,$20,$85,$20,$90,$80,$9C,$24,$C0,$E0,$8D
			FCB		$A0,$80,$20,$82,$A0,$A2,$20,$40,$80,$82,$10,$B8,$00,$84,$60,$92
			FCB		$DF,$EF,$D1,$EC,$A4,$EC,$C9,$FD,$E6,$AD,$C6,$E5,$D4,$FD,$9A,$ED
			FCB		$AE,$E4,$AC,$BC,$9F,$EC,$8A,$ED,$0F,$E5,$8F,$EC,$D7,$E4,$AC,$EC
			FCB		$CE,$EF,$9E,$F4,$D7,$E4,$CF,$F5,$CE,$F4,$E6,$6C,$81,$EC,$9B,$EE
			FCB		$9F,$7C,$BC,$F7,$CC,$6D,$FE,$EF,$8E,$6E,$EF,$BD,$BE,$BD,$8F,$E4
			FCB		$A0,$04,$20,$E0,$B4,$82,$B4,$82,$20,$8C,$B0,$90,$B0,$90,$B8,$84
			FCB		$24,$90,$6A,$86,$28,$84,$A0,$A8,$24,$CF,$B8,$88,$A0,$A0,$B1,$81
			FCB		$10,$B6,$E0,$98,$C8,$B4,$34,$AF,$34,$00,$B0,$82,$20,$90,$E4,$AC
			FCB		$28,$84,$84,$88,$24,$AC,$AC,$A4,$25,$98,$20,$A0,$20,$80,$32,$00
			FCB		$AE,$EC,$8D,$BD,$9F,$F7,$FF,$A1,$CA,$BE,$8D,$7C,$8E,$74,$EF,$EC
			FCB		$D4,$F5,$9E,$B8,$8E,$35,$C6,$E4,$90,$ED,$ED,$FC,$8C,$25,$BE,$A4
			FCB		$FC,$6C,$89,$EE,$EC,$AD,$AE,$78,$EE,$EC,$AD,$ED,$AD,$EC,$BC,$BD
			FCB		$2E,$76,$AE,$EC,$8E,$BD,$EF,$FD,$AC,$EC,$EF,$B5,$BE,$A4,$BF,$E8
			FCB		$04,$00,$24,$08,$04,$84,$24,$80,$28,$82,$60,$04,$24,$94,$28,$00
			FCB		$2C,$84,$20,$80,$B5,$86,$30,$04,$23,$84,$A0,$80,$A0,$06,$24,$AD
			FCB		$A4,$80,$A6,$86,$80,$00,$E0,$80,$66,$90,$20,$8C,$00,$8C,$04,$82
			FCB		$A4,$46,$00,$01,$20,$98,$A0,$88,$20,$2A,$24,$E0,$00,$08,$64,$02
			FCB		$8D,$A5,$B5,$E5,$AD,$F7,$CD,$F4,$A8,$29,$BC,$64,$F8,$EC,$AD,$AE
			FCB		$A8,$F5,$8C,$A7,$E7,$E5,$A9,$F4,$F4,$FD,$94,$ED,$5C,$E4,$C0,$BE
			FCB		$8C,$FD,$CD,$A4,$94,$FF,$A5,$EC,$FC,$E5,$AC,$E4,$AE,$BD,$9D,$BD
			FCB		$A8,$EC,$84,$68,$C9,$AD,$8E,$AC,$EA,$ED,$8F,$EC,$BF,$AF,$F7,$ED
			FCB		$24,$91,$A0,$04,$B4,$04,$82,$90,$AC,$08,$B0,$A0,$C0,$08,$34,$A0
			FCB		$65,$88,$73,$80,$20,$80,$2C,$04,$20,$84,$21,$26,$20,$94,$00,$20
			FCB		$A4,$96,$A4,$80,$B0,$84,$24,$82,$25,$86,$A0,$00,$7C,$30,$A2,$25
			FCB		$24,$9E,$A0,$88,$20,$80,$20,$00,$20,$A9,$20,$A2,$C0,$B2,$A1,$0E
			FCB		$AE,$EC,$A8,$EE,$AC,$F8,$FA,$B4,$A4,$FE,$FC,$F4,$1C,$ED,$AE,$F4
			FCB		$D7,$F8,$FD,$ED,$8E,$FE,$DC,$E8,$FE,$ED,$FC,$ED,$9F,$FC,$9C,$ED
			FCB		$B0,$E7,$BE,$FE,$84,$E4,$A7,$FE,$AD,$E4,$BC,$64,$8D,$BF,$AF,$FD
			FCB		$AA,$2C,$CD,$ED,$8F,$E4,$FC,$BD,$BF,$EF,$E5,$ED,$BF,$FE,$E5,$FD
			FCB		$84,$82,$A4,$83,$A4,$08,$A0,$80,$00,$84,$30,$86,$08,$20,$B0,$90
			FCB		$14,$B4,$A0,$80,$20,$90,$A4,$88,$EC,$80,$A4,$84,$A0,$84,$28,$80
			FCB		$B4,$81,$F0,$A0,$E4,$80,$00,$84,$34,$B1,$E4,$80,$24,$5D,$20,$D2
			FCB		$44,$83,$B0,$88,$20,$14,$30,$BC,$20,$A0,$C4,$9D,$68,$00,$A0,$20
			FCB		$CF,$EC,$AA,$66,$B8,$75,$9E,$FC,$FE,$B4,$BC,$FF,$AC,$FD,$9A,$BC
			FCB		$BA,$EC,$8E,$F4,$EC,$FC,$96,$FD,$D2,$EA,$84,$FC,$AE,$E4,$E5,$FD
			FCB		$BD,$EC,$DC,$AC,$A5,$B0,$CF,$EC,$7A,$B5,$E4,$EA,$EF,$EC,$A4,$FE
			FCB		$E5,$FF,$F8,$BC,$86,$E4,$FF,$E5,$AF,$6C,$EE,$EC,$EE,$6C,$2A,$6C
			FCB		$28,$A0,$64,$84,$B4,$92,$04,$94,$80,$00,$B8,$0C,$20,$88,$A0,$84
			FCB		$A0,$40,$B0,$00,$A0,$84,$A0,$CA,$00,$14,$24,$C0,$A0,$64,$24,$88
			FCB		$00,$18,$ED,$88,$28,$80,$A1,$80,$00,$85,$20,$80,$A4,$34,$00,$04
			FCB		$24,$A4,$AC,$8C,$34,$88,$2C,$04,$24,$84,$60,$88,$A0,$A0,$A0,$9C
			FCB		$B9,$F4,$97,$EC,$68,$AD,$E4,$F0,$F4,$EC,$CB,$FC,$A8,$F5,$FD,$3E
			FCB		$FC,$EE,$9D,$EC,$5E,$FC,$9C,$F6,$80,$ED,$A2,$E4,$AD,$B6,$BF,$F4
			FCB		$ED,$FE,$B3,$FC,$DC,$B5,$B3,$ED,$EF,$FC,$BF,$FC,$BC,$FE,$EA,$F4
			FCB		$C5,$E4,$89,$FC,$7C,$BD,$DE,$FD,$87,$E4,$9E,$EC,$AF,$E4,$FE,$E4
			FCB		$34,$C6,$30,$26,$24,$80,$20,$1C,$B4,$04,$B4,$28,$24,$98,$20,$00
			FCB		$A4,$A4,$A0,$80,$21,$48,$B9,$24,$20,$80,$28,$00,$20,$80,$84,$1E
			FCB		$60,$BC,$28,$88,$AC,$74,$E0,$04,$24,$A6,$00,$20,$70,$85,$A0,$89
			FCB		$30,$0B,$A0,$80,$20,$10,$A0,$14,$24,$04,$20,$82,$A0,$80,$35,$A0
			FCB		$EE,$BC,$BE,$FD,$BE,$B9,$89,$E4,$FD,$3D,$13,$E4,$9E,$EC,$8D,$EC
			FCB		$DE,$FC,$8E,$E5,$8F,$F4,$BE,$FF,$E6,$E6,$8C,$E6,$CD,$AE,$EE,$ED
			FCB		$FE,$EC,$DC,$EC,$8E,$FC,$96,$FD,$A6,$FC,$CE,$CD,$BE,$FE,$EE,$FC
			FCB		$9E,$F4,$EE,$6E,$CF,$ED,$EE,$E0,$FC,$EE,$AE,$A0,$FD,$ED,$A6,$6C
			FCB		$A8,$04,$20,$00,$30,$80,$A0,$90,$20,$80,$60,$00,$20,$82,$90,$40
			FCB		$24,$10,$04,$00,$84,$00,$24,$18,$E0,$00,$00,$02,$20,$B4,$24,$00
			FCB		$22,$80,$04,$04,$A0,$80,$84,$0A,$28,$14,$A0,$80,$00,$86,$40,$86
			FCB		$20,$90,$10,$30,$84,$84,$00,$00,$74,$80,$30,$00,$20,$08,$20,$80
			FCB		$FB,$FD,$C9,$E6,$91,$FD,$C8,$EE,$B4,$7C,$BE,$EC,$89,$EC,$BD,$FC
			FCB		$A5,$FD,$D9,$EC,$8A,$FD,$AE,$EC,$AD,$FC,$AE,$EC,$8D,$EC,$8F,$F4
			FCB		$BF,$6D,$AC,$BD,$EA,$ED,$AF,$AC,$FD,$FC,$EA,$FD,$F0,$F9,$B3,$EE
			FCB		$8E,$E4,$FD,$EC,$FF,$E0,$8E,$E4,$FD,$AD,$DC,$6C,$A0,$E4,$8E,$E4
			FCB		$28,$00,$B4,$00,$22,$00,$B4,$00,$6C,$00,$A4,$1B,$B2,$9C,$20,$84
			FCB		$24,$C4,$24,$A8,$A0,$80,$64,$88,$80,$AC,$30,$85,$20,$9C,$74,$F2
			FCB		$88,$88,$38,$80,$2C,$A0,$20,$04,$B8,$80,$24,$90,$A8,$F8,$BC,$88
			FCB		$20,$20,$A0,$E0,$6C,$80,$00,$E8,$20,$80,$24,$C0,$23,$9C,$24,$82
			FCB		$B5,$E0,$A4,$ED,$8E,$E4,$A9,$FC,$F8,$EE,$CE,$FC,$89,$E4,$98,$F5
			FCB		$A1,$EC,$A1,$E0,$B8,$EC,$D0,$FC,$CF,$F9,$AE,$EC,$8A,$FD,$6B,$6E
			FCB		$94,$A8,$B8,$E4,$CE,$A6,$8C,$F4,$EE,$BD,$8D,$EC,$ED,$F4,$CF,$6C
			FCB		$99,$6C,$C8,$F9,$F9,$A5,$B6,$79,$8C,$C1,$E4,$F5,$AA,$24,$F7,$64
			FCB		$24,$A8,$A4,$80,$A0,$96,$20,$09,$20,$18,$A8,$08,$B0,$C0,$7C,$CD
			FCB		$24,$DC,$20,$98,$64,$84,$E0,$82,$34,$A0,$EA,$88,$60,$C4,$70,$E8
			FCB		$30,$06,$E5,$1C,$20,$84,$20,$9C,$A0,$80,$A0,$1C,$A8,$80,$88,$22
			FCB		$24,$80,$24,$B0,$E4,$14,$E4,$80,$E0,$86,$00,$D6,$8C,$80,$B8,$AB
			FCB		$AF,$A2,$84,$E8,$86,$2F,$8A,$F5,$D4,$FF,$86,$EF,$8E,$F4,$C8,$6C
			FCB		$F5,$F4,$4C,$F4,$E8,$74,$04,$EE,$EE,$E0,$26,$2C,$9F,$EC,$90,$E5
			FCB		$BC,$E5,$F8,$E0,$FB,$F4,$B4,$ED,$4E,$E4,$E6,$EC,$8C,$E4,$BF,$ED
			FCB		$9F,$E0,$CC,$7C,$BF,$ED,$9D,$ED,$AD,$F8,$AF,$64,$EF,$ED,$ED,$E4
			FCB		$A0,$8C,$20,$95,$30,$38,$A0,$84,$64,$80,$E0,$76,$2C,$20,$B4,$04
			FCB		$3F,$B1,$A4,$24,$24,$80,$24,$A9,$31,$97,$AC,$28,$64,$50,$A0,$04
			FCB		$38,$80,$B4,$8E,$20,$CC,$BC,$38,$64,$8C,$A0,$90,$E4,$A0,$64,$99
			FCB		$64,$AF,$24,$E5,$60,$80,$A4,$3C,$2C,$8E,$B4,$BC,$A4,$CC,$24,$CC


; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
