
; Super Color BASIC 2.0
; Copied from the PDF version of Super Color BASIC Unravelled.
; Fixed up to assemble in Mamou

; Revision History

; $Id: $


; up when disk basic is merged:
DC0DC           EQU         $C0DC

; SUPER EXTENDED BASIC EQUATES
ROWMAX          EQU         24              ; MAXIMUM NUMBER OF ROWS IN HI-RES PRINT MODE
RAMLINK         EQU         0               ; DUMMY RAM LINK VECTOR
HRESSCRN        EQU         $2000           ; ADDRESS OF THE HI-RES SCREEN IN THE CPU'S MEMORY SPACE
HRESBUFF        EQU         $C000           ; ADDRESS OF THE GET/PUT BUFFERS IN THE CPU'S MEMORY SPACE
TMPSTACK        EQU         $DFFF           ; ADDRESS OF THE HI-RES GRAPHICS STACK IN THE CPU'S MEMORY SPACE
EBHITOK         EQU         $62             ; FIRST ENHANCED BASIC TOKEN NUMBER
EBHISTOK        EQU         $29             ; FIRST ENHANCED BASIC FUNCTION TOKEN NUMBER BUG - SHOULD BE $28
CURCHAR         EQU         SPACE           ; HI-RES CURSOR CHARACTER
; HBUFF HGET/HPUT BUFFER HEADER EQUATES
HB.ADDR         EQU         0               ; ADDRESS OF THE NEXT BUFFER - 2 BYTES
HB.NUM          EQU         2               ; NUMBER OF THIS BUFFER - 1 BYTES
HB.SIZE         EQU         3               ; NUMBER OF BYTES IN THE BUFFER - 2 BYTES
HB.LEN          EQU         5               ; NUMBER OF BYTES IN THIS HEADER
; VIDEO REGISTER EQUATES
; INIT0 BIT EQUATES
COCO            EQU         $80             ; 1 = Color Computer compatible
MMUEN           EQU         $40             ; 1 = MMU enabled
IEN             EQU         $20             ; 1 = GIME chip IRQ output enabled
FEN             EQU         $10             ; 1 = GIME chip FIRQ output enabled
MC3             EQU         8               ; 1 = RAM at XFEXX is constant
MC2             EQU         4               ; 1 = standard SCS
MC1             EQU         2               ; ROM map control
MC0             EQU         1               ; ROM map control
; INTERRUPT REQUEST ENABLED
TMR             EQU         $20             ; TIMER
HBORD           EQU         $10             ; HORIZONTAL BORDER
VBORD           EQU         8               ; VERTICAL BORDER
EI2             EQU         4               ; SERIAL DATA
EI1             EQU         2               ; KEYBOARD
EI0             EQU         1               ; CARTRIDGE
; EXPANDED MEMORY DEFINITIONS
BLOCK6.0        EQU         $30             ; BLOCKS $30-$33 ARE THE HI-RES GRAPHICS SCREEN
BLOCK6.1        EQU         $31             ; HI-RES GRAPHICS SCREEN
BLOCK6.2        EQU         $32             ; HI-RES GRAPHICS SCREEN
BLOCK6.3        EQU         $33             ; HI-RES GRAPHICS SCREEN
BLOCK6.4        EQU         $34             ; GET/PUT BUFFER
BLOCK6.5        EQU         $35             ; STACK AREA FOR HI-RES GRAPHICS COMMAND
BLOCK6.6        EQU         $36             ; CHARACTER POINTERS
BLOCK6.7        EQU         $37             ; UNUSED BY BASIC
; BLOCKS $48-$4F ARE USED FOR THE BASIC OPERATING SYSTEM
BLOCK7.0        EQU         $38
BLOCK7.1        EQU         $39
BLOCK7.2        EQU         $3A
BLOCK7.3        EQU         $3B
BLOCK7.4        EQU         $3C
BLOCK7.5        EQU         $3D
BLOCK7.6        EQU         $3E
BLOCK7.7        EQU         $3F

                ORG         DOSBAS
; New CoCo3 BASIC
SC000           ORCC        #$50            ; DISABLE IRQ, FIRQ INTERRUPTS
                LDS         #$5EFF          ; INITIALIZE STACK POINTER
                LDA         #$12            ; PALETTE COLOR: COMPOSITE-GREEN, RGB-INDIGO
; INITIALIZE ALL PALETTE REGISTERS TO GREEN (COMPOSITE)
                LDB         #16             ; 16 PALETTE REGISTERS
                LDX         #PALETREG       ; POINT X TO THE PALETTE REGISTERS
SC00D           STA         ,X+             ; SAVE THE COLOR IN THE PALETTE REGISTER
                DECB                        ;  BUMP COUNTER
                BNE         SC00D           ; LOOP UNTIL ALL PALETTE REGISTERS DONE
                LDX         #MMUREG         ; POINT X TO THE MMU REGISTERS
                LEAY        MMUIMAGE,PC     ; POINT Y TO THE MMU REGISTER IMAGES
                LDB         #16             ; 16 MMU REGISTERS
SC01B           LDA         ,Y+             ; GET A BYTE FROM THE IMAGE
                STA         ,X+             ; SAVE IT IN THE MMU REGISTER
                DECB                        ;  BUMP COUNTER
                BNE         SC01B           ; LOOP UNTIL DONE
                LDA         #COCO+MMUEN+MC3+MC2+MC1 ; ENABLE COCO COMPATIBLE MODE; ENABLE MMU
                STA         INIT0           ; AND TURN ON THE NORMAL SPARE CHIP SELECT
; MOVE THE INITIALIZATION CODE FROM ROM TO RAM($4000); THIS IS DONE IN
; PREPARATION FOR MOVING BASIC FROM ROM TO RAM.
                LEAX        BEGMOVE,PC      ; POINT TO START OF ROM CODE
                LDY         #$4000          ; RAM LOAD ADDRESS
SC02F           LDD         ,X++            ; GRAB TWO BYTES
                LDU         ,X++            ; GRAB TWO MORE BYTES
                STD         ,Y++            ; MOVE FIRST SET OF BYTES
                STU         ,Y++            ; AND THEN THE SECOND
                CMPX        #ENDMOVE        ; ARE ALL BYTES MOVED?
                BCS         SC02F           ; KEEP GOING UNTIL DONE
                JMP         $4000           ; JUMP INTO THE MOVED CODE
; THE REST OF THE CODE IS MOVED INTO RAM TO BE EXECUTED
BEGMOVE         LEAS        $-01,S          ; MAKE A TEMPORARY STORAGE LOCATION ON THE STACK
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;  SPACE FILLER NOPS - THEY SERVE NO PURPOSE
                LDA         #$FF
                STA         V_TIMER
                STA         V_TIMER+1       ; SET THE TIMER TO $FFFF AND START IT COUNTING
; SET UP THE VIDEO CONTROL REGISTERS
                LEAX        VIDIMAGE,PC     ; POINT X TO THE VIDEO CONTROL REGISTER IMAGE
                LDY         #VIDEOMOD       ; POINT Y TO THE VIDEO CONTROL REGISTERS
SC056           LDA         ,X+             ; GET A BYTE FROM THE IMAGE
                STA         ,Y+             ; SAVE IT IN THE VIDEO REGISTER
                CMPY        #MMUREG         ; CHECK FOR THE END OF THE VIDEO MODE REGISTERS
                BNE         SC056           ; LOOP UNTIL DONE
; INITIALIZE PIA1
                LDX         #PIA1           ; POINT X TO PIA 1
                LDD         #$FF34
                CLR         $01,X           ; CLEAR CONTROL REGISTER A
                CLR         $03,X           ; CLEAR CONTROL REGISTER B; ENABLE BOTH DATA DIRECTION REGISTERS
                DECA                        ;  SET ACCA TO $FE
                STA         ,X              ; BIT 0 INPUT, ALL OTHERS OUTPUT ON PORT A
                LDA         #$F8
                STA         $02,X           ; BITS 0-2 INPUT, 3-7 OUTPUT ON PORT B
                STB         $01,X           ; SET PORT TO PERIPHERAL REGISTER, CA1 DISABLED, CA2 ENABLED AS INPUT
                STB         $03,X           ; SET PORT TO PERIPHERAL REGISTER, CB1 DISABLED, CB2 ENABLED AS INPUT
                CLR         $02,X           ; SET THE GRAPHICS MODE TO NORMAL LO-RES COCO ALPHA
                LDA         #$02
                STA         ,X              ; SET THE DA OUTPUT TO ZERO AND THE RS232 OUTPUT TO MARKING
                LDA         #$FF
; INITIALIZE PIA0
                LDX         #PIA0           ; POINT X TO PIA 0
                CLR         $01,X           ; CLEAR CONTROL REGISTER A; ENABLE BOTH DATA DIRECTION REGISTERS
                CLR         $03,X           ; CLEAR CONTROL REGISTER B; ENABLE BOTH DATA DIRECTION REGISTERS
                CLR         ,X              ; SET PORT A TO ALL INPUTS
                STA         $02,X           ; SET PORT B TO ALL OUTPUTS
                STB         $01,X           ; SET PORT TO PERIPHERAL REGISTER, CA1 DISABLED, CA2 ENABLED AS INPUT
                STB         $03,X           ; SET PORT TO PERIPHERAL REGISTER, CB1 DISABLED, CB2 ENABLED AS INPUT
; INITIALIZE THE SAM MIRROR REGISTERS IN THE CUSTOM CHIP
                LDB         #12             ; RESET 12 SAM IMAGE REGISTERS
                LDU         #SAMREG         ; POINT U TO THE SAM REGISTERS
SC091           STA         ,U++            ; CLEAR THE BIT AND SKIP TO THE NEXT BIT
                DECB                        ;  BUMP COUNTER
                BNE         SC091           ; LOOP UNTIL ALL BITS CLEARED
                STA         SAMREG+9        ; SET THE VIDEO DISPLAY PAGE TO $400
                TFR         B,DP            ; SET THE DIRECT PAGE TO PAGE ZERO
                CLR         $02,X           ; STROBE ALL KEYBOARD COLUMNS; USELESS INSTRUCTION
                STA         $-03,U          ; SAMREG+21 (FFD5); SELECT RAM PAGE 1; USELESS IN THE COCO 3
                LDX         #PIA0           ; POINT X TO PIA 0; WHY?? IT'S ALREADY POINTED THERE
                LDB         #$DF            ; COLUMN TWO STROBE
                STB         $02,X           ; STROBE THE COLUMNS
                LDA         ,X              ; READ THE ROWS
                COMA                        ;
                ANDA        #$40            ; LOOK FOR ROW 6 ONLY (F1 KEY)
                STA         ,S              ; SAVE IN TEMPORARY STORAGE
; CHECK FOR THE CONTROL AND ALT KEYS
                LDY         #2              ; CHECK FOR TWO KEYS
SC0B1           ASRB                        ;  SHIFT THE COLUMN STROBE -- WASTED, SHOULD BE ASR 2,X
                STB         $02,X           ; SAVE THE NEW COLUMN STROBE
                LDA         ,X              ; READ THE KEYBOARD ROWS
                COMA                        ;
                ANDA        #$40            ; KEEP ONLY ROW 6
                BEQ         SC0C2           ; BRANCH IF KEY NOT DOWN
                LEAY        $-01,Y          ; LET'S CHECK FOT EH ALT KEY NOW
                BNE         SC0B1
                LBRA        SC1F0           ; GO DISPLAY THE HI-RES PICTURE IF CONTROL AND ALT KEYS ARE DOWN
SC0C2           LDA         #COCO+MMUEN+MC3+MC1 ; TURN OFF THE NORMAL SCS; THE EXTERNAL DISK CONTROLLER
                STA         INIT0           ; MAY NOT BE ACCESSED NOW
; THE FOLLOWING CODE CHECKS TO DETERMINE IF A JUMP TO WARM START RESET CODE SHOULD BE DONE.
; THE JUMP TO A WARM START RESET WILL BE DONE IF 1) INT.FLAG CONTAINS A $55 AND,
; 2) RSTFLG CONTAINS A $55 AND, 3) THE ADDRESS IN RSTVEC POINTS TO A $12 (NOP INSTRUCTION.)
; IF THE ABOVE CONDITIONS ARE MET, BASIC WILL BE WARM-STARTED. IF INT.FLAG DOES NOT CONTAIN
; A $55, BASIC WILL BE COLD-STARTED. IF INT.FLAG DOES CONTAIN A $55, BUT 2) AND 3) ABOVE
; ARE NOT MET, BLOCK 6.0 (128K SYSTEM) OR BLOCK 0.0 (512K SYSTEM) WILL BE LOADED INTO CPU
; BLOCK 0. THIS WILL GIVE THE CPU A NEW DIRECT PAGE AND CHECKS 2) AND 3) ABOVE WILL BE
; PERFORMED ON THIS NEW DIRECT PAGE TO SEE IF BASIC SHOULD BE WARM-STARTED.
                LDA         >INT.FLAG       ; GET THE INTERRUPT JUMP TABLE VALIDITY FLAG.
                CMPA        #$55            ; CHECK FOR VALID INTERRUPT JUMP TABLE FLAG
                BNE         SC0F6           ; INTERRUPT JUMP TABLE IS NOT VALID' COPY ROM TO RAM
                LDA         RSTFLG          ; GET THE SYSTEM RESET FLAG
                CMPA        #$55            ; CHECK FOR THE WARM START FLAG
                BNE         NOWARM          ; BRANCH IF NO WARM START
                LDX         RSTVEC          ; GET THE SYSTEM RESET VECTOR
                LDA         ,X              ; GET THE FIRST BYTE POINTED TO BY THE RESET VECTOR
                CMPA        #$12            ; IS IT A NOP?
                LBEQ        SC18C           ; DON'T COPY ROM TO RAM, ETC.
NOWARM          CLR         MMUREG          ; PUT BLOCK 6.0 (128K RAM) OR BLOCK 0.0 (512K RAM) INTO CPU BLOCK 0
                LDA         RSTFLG
                CMPA        #$55            ; CHECK FOT THE WARM START FLAG
                BNE         SC0F1           ; BRANCH IF NO WARM START
                LDX         RSTVEC          ; POINT X TO THE WARM START CODE
                LDA         ,X              ; GET THE FIRST BYTE OF THE WARM START CODE
                CMPA        #$12            ; IS IT A NOP?
                LBEQ        SC18C           ; DON'T COPY ROM TO RAM IF IT IS.
SC0F1           LDA         #BLOCK7.0       ; GET BACK BLOCK 7.0
                STA         MMUREG          ; PUT IT BACK INTO CPU BLOCK 0
SC0F6           LDX         #DOSBAS         ; POINT TO THE END OF THE COLOR BASIC ROM
                LDY         #EXBAS          ; POINT TO START OF EXTENDED BASIC
                LBSR        SC1AA           ; MOVE COLOR AND EXTENDED BASIC ROM TO RAM
; PATCH COLOR AND EXTENDED BASIC
                LEAY        PATCHTAB,PC     ; POINT Y TO THE PATCH TABLE
                LDA         ,Y+             ; GET THE NUMBER OF PATCHES TO BE MADE
SC106           PSHS        A               ; SAVE THE PATCH COUNTER
                LDX         ,Y++            ; GET THE ADDRESS WHERE THE PATCH IS TO BE PLACED
                LDB         ,Y+             ; GET THE NUMBER OF BYTES IN THE PATCH
SC10C           LDA         ,Y+             ; GET A BYTE
                STA         ,X+             ; PATCH THE CODE IN RAM
                DECB                        ;  BUMP THE COUNTER
                BNE         SC10C           ; LOOP UNTIL DONE
                PULS        A               ; RESTORE THE PATCH COUNTER
                DECA                        ;
                BNE         SC106           ; LOOP UNTIL ALL PATCHES DONE
                CLR         SAMREG+30       ; ENABLE THE ROM MODE
                LDA         #COCO+MMUEN+MC3 ; ENABLE 16K INTERNAL, 16K EXTERNAL ROM
                STA         INIT0
                LDD         DOSBAS          ; GET THE FIRST TWO BYTES OF AN EXTERNAL ROM, IF ANY
; CHECK FOR A 'DK' AT $C000 (DISK BASIC) - THIS SHOULD BE CMPD
                CMPA        #'D
                BNE         SC137
                CMPB        #'K
                BNE         SC137
; COPY THE DISK BASIC ROM INTO RAM
                LDX         #SUPERVAR       ; POINT TO THE END OF THE DISK BASIC ROM
                LDY         #DOSBAS         ; POINT TO THE START OF THE DISK BASIC ROM
                BSR         SC1AA           ; COPY ROM INTO RAM
                LBSR        SC322           ; PATCH DISK BASIC
SC137           CLR         SAMREG+30       ; ENABLE ROM MODE
                LDA         #COCO+MMUEN+MC3+MC1
                STA         INIT0           ; ENABLE 32K INTERNAL ROM
; COPY SUPER EXTENDED BASIC FROM ROM TO RAM
                LDX         #H.CRSLOC       ; POINT TO THE END OF ENHANCED BASIC ROM
                LDY         #SUPERVAR       ; POINT TO THE START OF ENHANCED BASIC ROM
                BSR         SC1AA           ; COPY ROM TO RAM
                LBSR        SC1DE           ; PATCH THE ENHANCEMENTS (MOVE THE AUTHORS' DECODED NAMES)
                LEAY        INTIMAGE,PC     ; POINT X TO THE INTERRUPT JUMP VECTOR IMAGES
                LDX         #INT.FLAG       ; DESTINATION FOR INTERRUPT VECTORS
                LDB         #19             ; 6 INTERRUPT JUMP ADDRESSES * 3 BYTES/JUMP ADDRESS + VALIDITY FLAG
                LBSR        MOVE.XY         ; COPY THE INTERRUPT JUMP VECTORS
                CLR         SAMREG+31       ; ENABLE THE RAM MODE
                TST         ,S              ; WAS THE F1 KEY DEPRESSED?
                BEQ         SC180           ; NO
                LDX         #IM.TEXT        ; TEXT MODE VIDEO CONTROL REGISTER IMAGES IN SUPER EXTENDED BASIC
                LDB         #$03            ; THREE SETS OF IMAGES
                LEAX        $01,X           ; SKIP PAST THE $FF90 TEXT MODE IMAGE
SC165           LDA         ,X              ; GET THE INIT0 IMAGE
                ORA         #$20            ; FORCE THE ALTERNATE COLOR SET
                STA         ,X              ; RE-SAVE THE INIT0 IMAGE
                LEAX        $09,X           ; SKIP TO NEXT SET OF IMAGES
                DECB                        ;  BUMP COUNTER
                BNE         SC165           ; LOOP UNTIL DONE
                LDB         #$02            ; TWO SETS OF GRAPHICS MODE IMAGES
                LDX         #IM.GRAPH       ; GRAPHICS MODE VIDEO CONTROL REGISTER IMAGES IN SUPER EXTENDED BASIC
SC175           LDA         ,X              ; GET THE INIT0 IMAGE
                ORA         #$20            ; FORCE THE ALTERNATE COLOR SET
                STA         ,X              ; RE-SAVE THE INIT0 IMAGE
                LEAX        $09,X           ; SKIP TO NEXT SET OF IMAGES
                DECB                        ;  BUMP COUNTER
                BNE         SC175           ; LOOP UNTIL DONE
; CLEAR THE LO-RES VIDEO SCREEN
SC180           LDX         #VIDRAM         ; POINT X TO THE START OF THE VIDEO DISPLAY
                LDA         #$60            ; GREEN SPACE
SC185           STA         ,X+             ; PUT A GREEN SPACE ON THE LO-RES SCREEN
                CMPX        #VIDRAM+512     ; AT THE END OF THE DISPLAY?
                BCS         SC185           ; NO
SC18C           LDA         #COCO+MMUEN+MC3+MC2+MC1
                STA         INIT0           ; ENABLE THE NORMAL SPARE CHIP SELECT (EXTERNAL $FF40)
                TST         ,S              ; WAS THE F1 KEY DEPRESSED?
                BEQ         SC19A           ; NO
                LDA         #$20            ; ALTERNATE COLOR SET FLAG
                STA         VIDEOMOD        ; FORCE THE ALTERNATE COLOR SET
SC19A           LDX         #PALETREG       ; POINT X TO THE PALETTE REGISTERS
                LEAY        PALIMAGE,PC     ; POINT Y TO THE PALETTE REGISTER IMAGES
                LDB         #16             ; 16 PALETTE REGISTERS
                BSR         MOVE.XY         ; FILL THE PALETTE REGISTERS FROM THEIR IMAGE
                LEAS        $01,S           ; REMOVE THE TEMPORARY STORAGE BYTE
                JMP         RESVEC          ; JUMP TO THE COCO 2 RESET ENTRY POINT
; COPY DATA POINTED TO BY (Y) FROM ROM TO RAM UNTIL THE ADDRESS IN
; (X) IS REACHED; PSHING AND PULING FROM U OR S WOULD BE MUCH MORE EFFICIENT
SC1AA           STX         $5F02           ; TEMPORARILY SAVE THE END OF COPY ADDRESS
                STS         $5F00           ; AND THE STACK POINTER
SC1B1           CLR         SAMREG+30       ; ENABLE THE ROM
                LDD         ,Y
                LDX         $02,Y
                LDU         $04,Y
                LDS         $06,Y
                CLR         SAMREG+31       ; DISABLE THE ROM
                STD         ,Y              ; NOW SAVE THE DATA FROM THE CPU REGISTERS INTO ROM
                STX         $02,Y
                STU         $04,Y
                STS         $06,Y
                LEAY        $08,Y           ; MOVE THE COPY POINTER UP 8 BYTES
                CMPY        $5F02           ; CHECK FOR END OF THE COPY RANGE
                BCS         SC1B1
                LDS         $5F00           ; RESTORE THE STACK
                RTS
; MOVE ACCB BYTES FROM (Y) TO (X)
MOVE.XY         LDA         ,Y+
                STA         ,X+
                DECB                        ;
                BNE         MOVE.XY
                RTS
; DECODE AND COPY THE AUTHOR'S NAMES INTO RAM
SC1DE           LDX         #AUTHORMS       ; POINT X TO THE DESTINATION FOR THE AUTHORS' NAMES
                LEAY        SC30D,PC        ; POINT Y TO THE CODED NAMES OF THE AUTHORS
                LDB         #21             ; 21 BYTES IN THE AUTHORS' NAMES
SC1E7           LDA         ,Y+             ; GET A CODED BYTE OF THE AUTHORS' NAMES
                COMA                        ;  DECODE THE BYTE
                STA         ,X+             ; SAVE THE UNCODED BYTE
                DECB                        ;  BUMP COUNTER DOWN ONE.
                BNE         SC1E7           ; LOOP UNTIL ALL BYTES DECODED
                RTS
; THIS IS THE CODE WHICH DISPLAYS THE HIGH RESOLUTION PICTURE OF THE
; AUTHORS OF SUPER EXTENDED BASIC
SC1F0           CLRA                        ;
                STA         >INT.FLAG       ; SET THE INTERRUPT FLAG TO NOT VALID (NOT INITIALIZED)
                STA         RSTFLG          ; FORCE THE ROMS TO BE COPIED INTO RAM
                STA         SAMREG+30       ; ENABLE THE ROMS
                LDB         #$09
                STB         PALETREG+10
                LDB         #63             ; WHITE (COMPOSITE AND RGB)
                STB         PALETREG+11
                LDX         #AUTHPIC        ; POINT X TO THE AUTHORS' PICTURE DATA
                LDY         #$0E00          ; DESTINATION OF THE AUTHORS' PICTURE DATA
SC20A           LDD         ,X++
                LDU         ,X++            ; GET FOUR BYTES OF PICTURE DATA
                STD         ,Y++
                STU         ,Y++            ; PUT THE DATA ON THE HI-RES SCREEN
                CMPX        #ENDPIC         ; AT THE END OF THE PICTURE DATA?
                BCS         SC20A           ; NO
                LDA         #$F9            ; 256x192 GREEN/BUFF COCO 2 HI-RES GRAPHICS MODE
                STA         PIA1+2          ; PROGRAM THE GRAPHICS MODE INTO THE PIA AND THE GIME CHIP
                CLRA                        ;
                LDX         #SAMREG         ; POINT X TO THE SAM REGISTERS
                STA         ,X
                STA         $03,X
                STA         $05,X           ; PROGRAM THE SAM REGISTERS FOR HI-RES MODE
                STA         $07,X
                STA         $09,X
                STA         $0B,X           ; SET THE VIDEO DISPLAY PAGE TO $E00
WAITLOOP        BRA         WAITLOOP        ; ENDLESS WAIT LOOP



; IMAGES OF THE VIDEO CONTROL REGISTERS (FF98-FF9F)
; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
VIDIMAGE        FCB         $00,$00,$00,$00,$0F,$E0,$00,$00
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
VIDIMAGE        FCB         $08,$00,$00,$00,$0F,$E0,$00,$00
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



; IMAGES OF THE PALETTE REGISTERS (FFB0-FFBF)
; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
PALIMAGE        FCB         $12,$24,$0B,$07,$3F,$1F,$09,$26 ; pal regs #0-7
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
PALIMAGE        FCB         $12,$36,$09,$24,$3F,$1B,$2D,$26 ; pal regs #0-7
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------
                FCB         $00,$12,$00,$3F,$00,$12,$00,$26 ; pal regs #8-15



; IMAGES OF THE MMU REGISTERS (FFA0-FFAF)
; TASK REGISTER 0
MMUIMAGE        FCB         BLOCK7.0,BLOCK7.1,BLOCK6.4,BLOCK7.3,BLOCK7.4,BLOCK7.5,BLOCK7.6,BLOCK7.7
; TASK REGISTER 1
                FCB         BLOCK7.0,BLOCK6.0,BLOCK6.1,BLOCK6.2,BLOCK6.3,BLOCK7.5,BLOCK6.5,BLOCK7.7
; TABLE OF PATCHES TO BE MADE TO COLOR AND EXTENDED BASIC. THE FIRST BYTE
; IS THE TOTAL NUMBER OF PATCHES TO BE MADE FOLLOWED BY THE CODE FOR ALL OF
; THE PATCHES. THE INDIVIDUAL PATCHES HAVE A THREE BYTE HEADER CONSISTING OF THE
; ADDRESS WHERE THE PATCH IS TO GO AND THE NUMBER OF BYTES IN THE PATCH.
PATCHTAB        FCB         27              ; NUMBER OF PATCHES
; PATCH 1 - ENABLE EXTENDED BASIC WARM START CODE
                FDB         XBWMST          ; PATCH1 $80C0
                FCB         $01
                NOP                         ;
; PATCH 2 - CRUNCH A TOKEN
                FDB         LB8D4           ; PATCH2 $B8D4
                FCB         $03
                JMP         >ALINK2         ; $E138
; PATCH 3 - UNCRUNCH A TOKEN
                FDB         LB7F3           ; PATCH3 $B7F3
                FCB         $03
                JMP         >ALINK3         ; $E172
; PATCH 4 - EXTENDED BASIC'S COMMAND INTERPRETATION LOOP
                FDB         L8150           ; PATCH4 $8150
                FCB         $04
                JMP         >ALINK4         ; $E192
                NOP                         ;
; PATCH 5 - EXTENDED BASIC'S SECONDARY COMMAND HANDLER
                FDB         L816C           ; PATCH5 $816C
                FCB         $04
                JMP         >ALINK5         ; $E1A6
                NOP                         ;
; PATCHES 6 - 11 MODIFY THE WAY A '&H' VARIABLE IS PROCESSED
; PATCH 6
                FDB         L8834           ; PATCH6 $8834
                FCB         $12
                JMP         >ALINK6A        ; $E3F8
                CLR         FPA0+1
                CLR         FPA0+2
                CLR         FPA0+3
                BRA         *-78
                CLR         FPA0
                BRA         *-47
                JMP         >ALINK6B
; PATCH 7
                FDB         L87EB           ; PATCH7 $87EB
                FCB         $07
                BRA         *+76
                NOP                         ;
                RTS
                LDX         #FPA0+1
; PATCH 8
                FDB         L880C           ; PATCH8 $880C
                FCB         $02
                BRA         *+55
; PATCH 9
                FDB         L8826           ; PATCH9 $8826
                FCB         $02
                BCS         *+25
; PATCH 10
                FDB         L87E7           ; PATCH10 $87E7
                FCB         $02
                BNE         *+7
; PATCH 11 - NEEDED BECAUSE PATCH 5 REMOVED AN RTS WHICH THIS ROUTINED USED
                FDB         L886A           ; PATCH11 $886A
                FCB         $02
                BNE         *-124
; PATCH 12 - EX BASIC'S COPYRIGHT MESSAGE
                FDB         L80B2           ; PATCH12 $80B2
                FCB         $03
                JMP         >ALINK12        ; $E288
; PATCH 13 - REMOVE ONE CR FROM ONE OF EX BAS COPYRIGHT MESSAGE
                FDB         L8101+$39       ; PATCH13 $813A
                FCB         $01
                FCB         $00
; PATCH 14 - ADD ONTO END OF EX BAS GRAPHICS INITIALIZATION ROUTINE
                FDB         L9703           ; PATCH14 $9703
                FCB         $03
                JMP         >ALINK14        ; $E389
; PATCH 15 - BREAK CHECK
                FDB         LADF0           ; PATCH15 $ADF0
                FCB         $04
                JMP         >ALINK15        ; $E429
                NOP                         ;
; PATCH 16 - CHECK FOR BREAK KEY ON BASIC'S LINE INPUT
                FDB         LA3C2           ; PATCH16 $A3C2
                FCB         $04
                JMP         >ALINK16        ; $E413
                NOP                         ;
; PATCH 17 - PATCH INPUT TO RESPOND TO ON BRK
                FDB         LB03C+1         ; PATCH17+1 $B03C+1
                FCB         $02
                FDB         ALINK17         ; $E532
; PATCH 18 - 'ON' COMMAND
                FDB         ON              ; PATCH18 $AF42
                FCB         $03
                JMP         >ALINK18        ; $E3B4
; PATCH 19 - END OF 'NEW' COMMAND
                FDB         LAD3F           ; PATCH19 $AD3F
                FCB         $04
                JMP         >ALINK19        ; $E4D0
                NOP                         ;
; PATCH 20 - ERROR SERVICING ROUTINE
                FDB         LAC46           ; PATCH20 $AC46
                FCB         $03
                JMP         >ALINK20        ; $E470
; PATCH 21 - BASIC'S MAIN LOOP IN THE DIRECT MODE
                FDB         LAC73           ; PATCH21 $AC73
                FCB         $03
                JMP         >ALINK21        ; $E502
; PATCH 22
                FDB         LA30A           ; PATCH22 $A30A
                FCB         $03
                JMP         L8C36+1         ; $8C37
; PATCH 23 - 'CLS' ROUTINE
                FDB         CLS             ; PATCH23 $A910
                FCB         $03
                JMP         L8C46           ; $8C46
; PATCH 24 - CURSOR BLINK
                FDB         LA1B1           ; PATCH24 $A1B1
                FCB         $08
                JMP         $A0CE           ; $A0CE
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
; PATCH 25 - PRINT @ COMMAND
                FDB         LB902           ; PATCH25 $B902
                FCB         $03
                JMP         >ALINK25        ; $F8C3
; PATCH 26
                FDB         LB95C           ; PATCH26 $B95C
                FCB         $03
                JMP         >ALINK26        ; $F8A3
; PATCH 27 - GET A BASIC INPUT LINE
                FDB         LA38D           ; PATCH27 $A38D
                FCB         $03
                JMP         >ALINK27        ; $F757
; THESE DATA ARE THE NAMES OF THE AUTHORS IN COMPLEMENTED ASCII (T.Harris & T.Earles,CR,0)
SC30D           FCB         $AB,$D1,$B7,$9E,$8D,$8D
                FCB         $96,$8C,$DF,$D9,$DF,$AB
                FCB         $D1,$BA,$9E,$8D,$93,$9A
                FCB         $8C,$F2,$FF
SC322           LDA         DOSBAS+4        ; LOOK FOR THE MS BYTE OF THE ADDRESS OF DSKCON
                CMPA        #$D6            ; IF IT IS D6, THEN WE HAVE DISK BASIC 1.0
                BNE         SC334           ; BRANCH IF DISK BASIC 1.1
                LDX         #$C0C6          ; POINT X TO DISK BASIC 1.0 PATCH ADDRESS ($C0C6)
                LEAY        SC355,PC        ; POINT Y TO THE PATCH DATA
                LDB         ,Y+             ; GET THE NUMBER OF BYTES TO PATCH
                BRA         SC349
SC334           LDX         #$C8B4          ; POINT X TO DISK BASIC 1.1 KEYBOARD PATCH ($C8B4)
                LDA         #$12            ; OP CODE OF A NOP INSTRUCTION
                LDB         #11             ; PATCH 11 BYTES
SC33B           STA         ,X+             ; STORE A NOP
                DECB                        ;  DECREMENT COUNTER
                BNE         SC33B           ; LOOP UNTIL DONE
                LDX         #$C0D9          ; POINT X TO DISK BASIC 1.1 PATCH ADDRESS ($C0D9)
                LEAY        SC351,PC        ; POINT Y TO THE PATCH DATA
                LDB         ,Y+             ; GET THE NUMBER OF BYTES TO PATCH
SC349           LDA         ,Y+             ; GET A PATCH BYTE
                STA         ,X+             ; STORE THE PATCH BYTE
                DECB                        ;  DECREMENT THE PATCH COUNTER
                BNE         SC349           ; LOOP UNTIL DONE
                RTS
; DISK BASIC ROM PATCHES (COPYRIGHT MESSAGE)
SC351           FCB         $03              ; NUMBER OF BYTES TO PATCH (JUST JMP INSTRUCTION BELOW)
                JMP         >SHOWDM21        ; SHOW STARTUP MESSAGE DISK BASIC 2.1
SC355           FCB         $03              ; NUMBER OF BYTES TO PATCH (JUST JMP INSTRUCTION BELOW)
                JMP         >SHOWDM20        ; SHOW STARTUP MESSAGE DISK BASIC 2.0
; INTERRUPT VECTOR IMAGES
; THESE LBRAs WILL LINK TO BASIC'S RAM INTERRUPT VECTORS AT $100
INTIMAGE        FCB         $55             ; VALIDITY FLAG (INTERRUPT VECTORS VALID/INVALID)
                LBRA        (INTIMAGE+1)-(INT.JUMP)+SW3VEC
                LBRA        (INTIMAGE+1)-(INT.JUMP)+SW2VEC
                LBRA        (INTIMAGE+1)-(INT.JUMP)+FRQVEC
                LBRA        (INTIMAGE+1)-(INT.JUMP)+IRQVEC
                LBRA        (INTIMAGE+1)-(INT.JUMP)+SWIVEC
                LBRA        (INTIMAGE+1)-(INT.JUMP)+NMIVEC

ENDMOVE         =           *               ; THE END OF THE DATA THAT'S COPIED TO RAM


; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
; UNUSED GARBAGE BYTES?
                FCB         $FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$55,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; UNUSED GARBAGE BYTES?
                FCB         $5E,$60,$C6,$F0
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$69,$70,$0C,$60,$0F,$E0,$52,$B0
                FCB         $00,$00,$00,$00,$00,$80,$00,$00,$03,$78,$04,$B0,$4E,$30,$14,$A0
                FCB         $80,$00,$00,$00,$00,$00,$00,$00,$0F,$BA,$0D,$A0,$6B,$CA,$16,$C0
                FCB         $80,$00,$04,$00,$00,$00,$00,$00,$0B,$52,$0C,$D0,$02,$90,$02,$30
                FCB         $00,$40,$00,$00,$08,$00,$00,$00,$1D,$84,$84,$10,$28,$60,$4E,$F3
                FCB         $00,$00,$00,$00,$02,$00,$00,$00,$8C,$F0,$47,$70,$45,$DC,$6C,$78
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$4D,$80,$29,$B4,$0F,$18,$0A,$F1
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$05,$A8,$08,$74,$2B,$72,$8A,$62
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$0B,$70,$02,$79,$22,$E0,$0A,$D0
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FCB         $00,$18,$00,$0E,$00

; COCO 2 COMPATIBLE DIGITIZED PICTURE OF THE AUTHORS (EARLES, HAWKINS, HARRIS)
AUTHPIC         FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$F7,$FF,$FB,$EE,$EF,$FB,$FF,$BB,$FF,$FF,$FF,$FB,$FF,$FF,$BB
                FCB         $BB,$BB,$BF,$BB,$BB,$FF,$BF,$FF,$FE,$EF,$FF,$FF,$FF,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$BF,$BB,$FF,$BB,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$EE,$EE,$EE,$EF,$FF,$FE,$FF,$FE,$EE,$EE,$FE,$EE
                FCB         $EE,$EE,$EF,$EE,$EE,$EE,$EE,$EE,$FE,$EE,$EE,$EE,$EE,$EE,$7F,$FF
                FCB         $FF,$F3,$BB,$FF,$BB,$BB,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FB,$BB,$BB,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F6,$AA,$AE,$AA,$AE,$AA,$EA,$BB,$BB,$FB,$FF,$BB,$FF,$BF,$BF
                FCB         $FF,$FB,$BF,$BB,$BB,$BB,$BB,$BB,$BA,$EA,$AA,$EE,$AE,$EE,$7F,$FF
                FCB         $FF,$F3,$BB,$BF,$BB,$BB,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FB,$BB,$BB,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EF,$FE,$EF,$FF,$FF,$FF
                FCB         $FF,$FE,$EF,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EE,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$BB,$BB,$BB,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$BB,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F6,$AA,$EE,$AA,$AA,$AA,$EA,$AB,$BB,$BB,$FF,$BB,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FB,$BF,$FF,$FF,$BB,$BB,$EA,$AE,$AA,$AA,$AF,$7F,$FF
                FCB         $FF,$F7,$BB,$BB,$BF,$FF,$FB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FB,$BB,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$EE,$EF,$EF,$EE,$EE,$EF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$EE,$EE,$FF,$EF,$EE,$EE,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F7,$EA,$AA,$BB,$BB,$BB,$BB,$BB,$FB,$BB,$BF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FB,$FF,$FB,$FB,$BB,$BB,$BA,$AA,$AA,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FB,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$EE,$FE,$EE,$EE,$EE,$EE,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FE,$EE,$EF,$FE,$EE,$EE,$EE,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$BB,$FB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$00,$00,$00
                FCB         $00,$00,$00,$00,$0F,$FF,$FF,$FF,$FF,$FF,$FB,$BB,$BB,$BB,$7F,$FF
                FCB         $FF,$F6,$EA,$AB,$AA,$AB,$BB,$BB,$BF,$BF,$FF,$FF,$FF,$80,$00,$7F
                FCB         $FF,$FF,$C0,$00,$3F,$FF,$FF,$BB,$BB,$BF,$AA,$AA,$AA,$EF,$7F,$FF
                FCB         $FF,$F3,$BB,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$BB,$BB,$BF,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$EE,$EF,$EE,$FF,$FF,$FF,$EE,$FF,$FF,$80,$00,$3F
                FCB         $FF,$FF,$80,$00,$3F,$FF,$FF,$EE,$FE,$EE,$EE,$EE,$EE,$EF,$7F,$FF
                FCB         $FF,$F3,$BB,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$07,$FF,$FF,$FF,$FF,$FF,$BB,$BB,$BB,$BF,$7F,$FF
                FCB         $FF,$F6,$EE,$AB,$AE,$EB,$BB,$BF,$FB,$FF,$FF,$FF,$FF,$80,$00,$1F
                FCB         $FF,$FF,$00,$00,$3F,$FF,$FF,$FF,$BB,$BB,$AA,$AA,$BE,$AB,$7F,$FF
                FCB         $FF,$F3,$BB,$BF,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FB,$FB,$BB,$BB,$BF,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$EE,$EE,$EE,$EF,$FE,$EF,$FF,$FF,$FF,$80,$00,$0F
                FCB         $FF,$FE,$00,$00,$3F,$FF,$FF,$EF,$EF,$EE,$EE,$EE,$EE,$EF,$7F,$FF
                FCB         $FF,$F3,$BB,$BF,$FB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$FB,$BF,$FF,$BB,$BF,$7F,$FF
                FCB         $FF,$F6,$EA,$AB,$BA,$AB,$BB,$FB,$FF,$BF,$FF,$FF,$FF,$80,$00,$07
                FCB         $FF,$FC,$00,$00,$3F,$FF,$FF,$FF,$FB,$AA,$FF,$FE,$EA,$AB,$7F,$FF
                FCB         $FF,$F3,$BB,$BF,$FB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$BF,$FF,$BB,$BF,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$FF,$FE,$EE,$EE,$FF,$FF,$FF,$FF,$FF,$80,$00,$03
                FCB         $FF,$F8,$00,$00,$3F,$FF,$FF,$FE,$EE,$EE,$FF,$FE,$EE,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$BB,$BF,$FF,$BB,$FF,$7F,$FF
                FCB         $FF,$F7,$AA,$FB,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$00,$01
                FCB         $FF,$F0,$00,$00,$3F,$FF,$FF,$BB,$BB,$AE,$BB,$BE,$AF,$BB,$7F,$FF
                FCB         $FF,$F7,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$FF,$FF,$BB,$FF,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$FF,$FF,$FF,$FF,$FE,$FF,$FF,$FF,$FF,$80,$08,$00
                FCB         $FF,$E0,$02,$00,$3F,$FF,$FF,$FE,$EF,$EE,$FF,$EE,$EF,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$FB,$FF,$FF,$BB,$FF,$7F,$FF
                FCB         $FF,$F7,$AA,$BB,$BB,$FB,$BB,$FF,$BF,$FF,$FF,$FF,$FF,$80,$0C,$00
                FCB         $7F,$C0,$06,$00,$3F,$FF,$FF,$FF,$BB,$FF,$FF,$BF,$EB,$BB,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F6,$EE,$EF,$FF,$EF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$0E,$00
                FCB         $3F,$80,$0E,$00,$3F,$FF,$FF,$FE,$FF,$FF,$FE,$EE,$EE,$FF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F3,$EA,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$0F,$00
                FCB         $1F,$00,$1E,$00,$3F,$FF,$FF,$FF,$BF,$FF,$FF,$FF,$FF,$BF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$EE,$EE,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$0F,$80
                FCB         $0E,$00,$3E,$00,$3F,$FF,$FF,$EE,$FF,$FF,$FF,$FE,$EF,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BF,$7F,$FF
                FCB         $FF,$F6,$EA,$BB,$BB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$0F,$C0
                FCB         $00,$00,$7E,$00,$3F,$FF,$FF,$BF,$FF,$FF,$FF,$FF,$BA,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$1F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$0F,$E0
                FCB         $00,$00,$FE,$00,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$FE,$EF,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BB,$7F,$FF
                FCB         $FF,$F6,$EB,$BF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$80,$0F,$C0
                FCB         $00,$00,$3E,$00,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$EB,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00
                FCB         $00,$00,$1F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$BA,$7F,$FF
                FCB         $FF,$F6,$EE,$EF,$FF,$FF,$FF,$C0,$1F,$FF,$FF,$FF,$FF,$80,$0C,$00
                FCB         $00,$00,$1E,$00,$3F,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$EE,$EB,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$FF,$FC,$00,$0F,$FF,$FF,$FF,$F0,$00,$00,$00
                FCB         $00,$00,$00,$00,$03,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$AA,$7F,$FF
                FCB         $FF,$F6,$AB,$BB,$FF,$FF,$00,$00,$03,$FF,$FF,$FF,$FF,$80,$0C,$00
                FCB         $00,$00,$06,$00,$3F,$FF,$FF,$FF,$FF,$FF,$79,$9F,$FB,$AB,$7F,$FF
                FCB         $FF,$F7,$BB,$FF,$FF,$F8,$00,$00,$01,$FF,$FF,$FF,$FF,$FF,$F0,$00
                FCB         $00,$38,$03,$FF,$FF,$FF,$FF,$FF,$FF,$F0,$00,$03,$FE,$EE,$7F,$FF
                FCB         $FF,$F6,$EF,$FF,$FF,$E0,$00,$00,$00,$4F,$FF,$FF,$FF,$80,$00,$00
                FCB         $01,$7C,$00,$00,$3F,$FF,$FF,$FF,$FF,$80,$00,$00,$FA,$BB,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$C0,$00,$00,$00,$01,$FF,$FF,$F0,$00,$00,$00
                FCB         $03,$FE,$00,$00,$03,$FF,$FF,$FF,$FF,$00,$00,$00,$7E,$EE,$7F,$FF
                FCB         $FF,$F3,$BB,$FF,$FF,$00,$00,$00,$00,$00,$FF,$FF,$FF,$80,$00,$00
                FCB         $0F,$FF,$00,$00,$3F,$FF,$FF,$FF,$FC,$00,$00,$00,$3B,$BB,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FE,$00,$00,$00,$00,$00,$7F,$FF,$FF,$FF,$00,$00
                FCB         $2F,$FF,$00,$7F,$FF,$FF,$FF,$FF,$F0,$00,$00,$00,$0E,$EE,$7F,$FF
                FCB         $FF,$F6,$FF,$FF,$FC,$00,$00,$00,$00,$00,$3F,$FF,$FF,$80,$00,$01
                FCB         $FF,$FF,$80,$00,$3F,$FF,$FF,$FF,$E0,$00,$00,$00,$07,$BB,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$F8,$00,$00,$00,$00,$00,$3F,$FF,$F0,$00,$00,$03
                FCB         $FF,$FF,$40,$00,$03,$FF,$FF,$FF,$C0,$00,$00,$00,$03,$EE,$7F,$FF
                FCB         $FF,$F3,$BB,$FF,$F0,$00,$00,$00,$01,$00,$1F,$FF,$FF,$80,$01,$07
                FCB         $FF,$FF,$A0,$00,$3F,$FF,$FF,$FF,$80,$00,$00,$00,$01,$BB,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$E0,$00,$00,$00,$01,$80,$1F,$FF,$FF,$FE,$0E,$2F
                FCB         $FF,$FF,$C0,$1F,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$EE,$7F,$FF
                FCB         $FF,$F6,$FF,$FF,$E0,$00,$00,$00,$65,$E0,$0F,$FF,$FF,$80,$0C,$1F
                FCB         $FF,$FF,$E0,$00,$3F,$FF,$FF,$FF,$00,$00,$00,$00,$00,$3B,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$C0,$00,$00,$0B,$FF,$F0,$0F,$FF,$FC,$00,$18,$0F
                FCB         $F8,$00,$60,$00,$07,$FF,$FF,$FE,$00,$00,$00,$00,$00,$0E,$7F,$FF
                FCB         $FF,$F3,$BB,$FF,$C0,$00,$01,$FF,$FF,$F8,$0F,$FF,$FF,$80,$00,$01
                FCB         $F8,$00,$20,$00,$3F,$FF,$FF,$FE,$00,$00,$00,$00,$00,$1B,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$80,$00,$03,$FF,$FF,$FC,$0F,$FF,$FF,$FC,$03,$03
                FCB         $FF,$7F,$F0,$03,$FF,$FF,$FF,$FC,$00,$00,$00,$00,$00,$0E,$7F,$FF
                FCB         $FF,$F7,$FE,$FF,$80,$01,$5D,$FF,$FF,$FC,$0F,$FF,$FF,$C0,$07,$F1
                FCB         $FC,$07,$FC,$00,$3F,$FF,$FF,$FC,$00,$00,$00,$00,$00,$03,$7F,$FF
                FCB         $FF,$F7,$BF,$FF,$00,$02,$EF,$FF,$FF,$FC,$0F,$FF,$FF,$00,$0E,$00
                FCB         $7C,$00,$F4,$00,$0C,$7F,$FF,$F8,$00,$00,$00,$00,$00,$0E,$7F,$FF
                FCB         $FF,$F2,$AB,$FF,$00,$05,$FF,$FF,$FF,$FE,$0F,$FF,$FF,$F8,$1C,$00
                FCB         $FC,$01,$F8,$01,$FB,$BF,$FF,$F8,$00,$00,$0F,$E1,$80,$0B,$7F,$FF
                FCB         $FF,$F7,$BF,$FF,$00,$03,$FF,$FF,$FF,$FE,$0F,$FF,$FF,$F8,$1F,$C0
                FCB         $FE,$3F,$FC,$01,$F6,$DF,$FF,$F8,$00,$3F,$FF,$FF,$80,$0E,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$00,$07,$FF,$FF,$FF,$FE,$0F,$FE,$FF,$FC,$1F,$F1
                FCB         $FF,$9F,$F8,$37,$F5,$DF,$FF,$F8,$00,$FF,$FF,$FF,$80,$0B,$7F,$FF
                FCB         $FF,$F7,$BF,$FF,$00,$07,$FF,$FF,$FF,$FF,$07,$FF,$FF,$F8,$0F,$EF
                FCB         $7F,$FF,$FC,$03,$F6,$DF,$FF,$F8,$7F,$FF,$FF,$FF,$80,$06,$7F,$FF
                FCB         $FF,$F6,$FF,$BF,$00,$03,$FF,$FF,$FF,$FF,$07,$FB,$BB,$FD,$DF,$FF
                FCB         $EF,$FF,$F9,$7F,$FB,$BF,$FF,$FC,$7F,$FF,$FF,$FF,$C0,$0B,$7F,$FF
                FCB         $FF,$F7,$BF,$FE,$00,$03,$FF,$FF,$FF,$F8,$07,$FF,$FF,$FF,$9F,$FF
                FCB         $FF,$FF,$FF,$BF,$FC,$7F,$FF,$FC,$75,$0F,$FF,$CF,$C0,$0E,$7F,$FF
                FCB         $FF,$F6,$FF,$FE,$00,$07,$F5,$47,$FF,$E0,$07,$FF,$EF,$FF,$CF,$FD
                FCB         $FF,$FF,$FF,$3F,$FF,$FF,$FF,$FC,$00,$00,$E0,$07,$C0,$0F,$7F,$FF
                FCB         $FF,$F7,$BF,$FF,$00,$0F,$F8,$07,$FF,$DC,$07,$FF,$FF,$FF,$CF,$FB
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FC,$00,$00,$00,$03,$E0,$0D,$7F,$FF
                FCB         $FF,$F6,$FF,$FE,$00,$1F,$D1,$1F,$FF,$EF,$07,$FF,$FF,$FF,$FF,$FF
                FCB         $F1,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$00,$00,$00,$7B,$F0,$0B,$7F,$FF
                FCB         $FF,$F3,$BF,$FE,$00,$1F,$FF,$EF,$FE,$81,$07,$FF,$FF,$FF,$7F,$F8
                FCB         $40,$FF,$FF,$FF,$FF,$FF,$FF,$FE,$60,$01,$FC,$1D,$F0,$0D,$7F,$FF
                FCB         $FF,$F6,$EE,$FF,$00,$1F,$FF,$01,$FE,$01,$87,$FE,$EE,$FF,$FF,$F0
                FCB         $00,$1F,$FF,$FF,$FF,$FF,$FF,$FE,$E0,$03,$F0,$67,$F0,$17,$7F,$FF
                FCB         $FF,$F3,$BF,$FE,$00,$1F,$F8,$01,$FC,$03,$87,$FF,$FF,$FF,$FF,$C0
                FCB         $00,$0F,$FC,$3F,$FF,$FF,$FF,$FF,$C0,$03,$F0,$33,$F0,$5F,$7F,$FF
                FCB         $FF,$F6,$AF,$FE,$00,$1F,$FF,$F0,$FE,$0F,$87,$FF,$BF,$FF,$FF,$80
                FCB         $FF,$07,$F8,$3F,$FF,$FF,$FF,$FF,$FF,$03,$FA,$FF,$F1,$BB,$7F,$FF
                FCB         $FF,$F3,$BF,$BF,$00,$1F,$FF,$FE,$FE,$9B,$87,$FF,$FF,$FF,$FF,$BD
                FCB         $80,$F3,$F8,$3F,$FF,$FF,$FF,$FF,$FF,$17,$FF,$FF,$F4,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$FF,$00,$1F,$FF,$FF,$FF,$03,$87,$EF,$EE,$EF,$FF,$00
                FCB         $00,$07,$F8,$3F,$FF,$FF,$FF,$FF,$FF,$77,$FF,$FF,$FE,$BB,$7F,$FF
                FCB         $FF,$F3,$BF,$BF,$00,$0F,$FF,$EF,$F8,$83,$87,$FF,$FF,$FF,$FF,$BE
                FCB         $BF,$FF,$F8,$7F,$FF,$FF,$FF,$FF,$FD,$B3,$FF,$FF,$FD,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$EF,$00,$0F,$FF,$F8,$7F,$F7,$8F,$FF,$FB,$BF,$FF,$FF
                FCB         $FF,$F7,$FC,$FF,$BF,$FF,$FF,$FF,$FF,$07,$FF,$FF,$FF,$BB,$7F,$FF
                FCB         $FF,$F3,$BB,$BB,$00,$0F,$FF,$F3,$7F,$0F,$8F,$FF,$FF,$FF,$FF,$FF
                FCB         $BF,$E3,$FC,$FF,$FF,$FF,$FF,$FF,$FF,$07,$FF,$FF,$FF,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$EF,$00,$0F,$FE,$F7,$FF,$CF,$8E,$FF,$FF,$EF,$FF,$FF
                FCB         $17,$F7,$FD,$EF,$EF,$FE,$FF,$FF,$FF,$9F,$FF,$FF,$FF,$BB,$7F,$FF
                FCB         $FF,$F3,$BB,$BB,$00,$07,$FF,$FE,$FF,$C1,$9F,$FF,$FF,$FF,$FF,$FE
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$07,$F7,$FF,$FE,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$00,$03,$FF,$C4,$1C,$03,$9B,$FB,$AF,$BF,$FF,$FF
                FCB         $FF,$FF,$FE,$FB,$FB,$BB,$FF,$BF,$FE,$02,$AB,$FF,$F8,$FB,$7F,$FF
                FCB         $FF,$F3,$BF,$BA,$04,$03,$FA,$00,$00,$01,$9F,$FF,$BB,$FF,$FB,$FF
                FCB         $FF,$FF,$FB,$BF,$FF,$FB,$FE,$EF,$FC,$00,$05,$FF,$F9,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$EC,$06,$01,$FC,$A2,$00,$03,$9F,$EE,$EE,$EF,$FE,$FF
                FCB         $FF,$FF,$FE,$FF,$FE,$EE,$EF,$BF,$FE,$00,$00,$3F,$FB,$BB,$7F,$FF
                FCB         $FF,$F3,$BB,$B0,$00,$01,$FA,$10,$00,$03,$9F,$FB,$BB,$FF,$FB,$FF
                FCB         $FF,$FF,$FB,$BF,$FF,$FB,$BE,$EF,$FC,$0F,$80,$1F,$F6,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$00,$00,$68,$5F,$FE,$03,$1B,$AE,$AE,$FB,$EE,$FD
                FCB         $FF,$FF,$FE,$AB,$BB,$EA,$FF,$BF,$F8,$0B,$FC,$1F,$FF,$BB,$7F,$FF
                FCB         $FF,$F3,$BB,$BA,$00,$00,$7B,$0F,$FE,$00,$1F,$BB,$BB,$FF,$FB,$FF
                FCB         $FF,$FF,$FB,$BB,$FF,$BB,$BE,$EF,$FC,$07,$FC,$5F,$F6,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$EE,$00,$00,$14,$40,$3C,$00,$0E,$EE,$EE,$FF,$FF,$FF
                FCB         $FF,$FF,$FE,$EE,$EE,$EE,$EF,$BF,$FE,$00,$61,$FF,$FF,$BB,$7F,$FF
                FCB         $FF,$F3,$BB,$80,$00,$00,$00,$7E,$00,$00,$3F,$BB,$FF,$FF,$BF,$FF
                FCB         $FF,$FF,$FF,$BB,$FF,$BB,$BE,$EF,$FF,$E0,$07,$FF,$FF,$EE,$7F,$FF
                FCB         $FF,$F6,$EE,$00,$00,$00,$00,$3F,$EE,$80,$2E,$AB,$BF,$BE,$FC,$FF
                FCB         $FF,$FF,$FE,$EB,$BB,$AA,$BB,$BF,$EF,$FA,$FF,$FF,$F8,$0B,$7F,$FF
                FCB         $FF,$F3,$BA,$00,$00,$C0,$00,$0F,$FE,$00,$3B,$BF,$FF,$FB,$B1,$FF
                FCB         $FF,$FF,$F3,$BB,$FF,$BB,$AE,$EF,$BF,$D9,$7F,$FF,$F8,$02,$7F,$FF
                FCB         $FF,$F6,$EC,$00,$00,$E0,$00,$00,$00,$00,$EE,$EE,$EF,$EE,$E1,$FF
                FCB         $FF,$FF,$F0,$EE,$EE,$EE,$EB,$BF,$EF,$8E,$3F,$FF,$F8,$00,$7F,$FF
                FCB         $FF,$F3,$BA,$00,$00,$F0,$00,$00,$00,$00,$BB,$BB,$BF,$BB,$B1,$FF
                FCB         $FF,$FF,$E0,$3B,$FF,$BB,$EE,$FF,$BB,$00,$5F,$FF,$F8,$00,$7F,$FF
                FCB         $FF,$F6,$E0,$04,$00,$78,$00,$00,$00,$01,$EE,$AA,$AE,$EA,$C0,$7F
                FCB         $FF,$FF,$80,$0B,$FF,$EF,$AB,$FF,$EF,$C2,$3F,$FF,$FF,$E0,$7F,$FF
                FCB         $FF,$F3,$80,$00,$00,$7C,$00,$00,$00,$00,$3B,$BB,$BB,$BB,$00,$3F
                FCB         $FF,$FF,$00,$03,$FF,$FE,$EE,$EF,$BB,$C0,$7F,$FF,$FE,$F8,$7F,$FF
                FCB         $FF,$F6,$60,$C0,$00,$7F,$00,$00,$00,$00,$2E,$EE,$EE,$EC,$00,$01
                FCB         $3F,$FC,$00,$00,$6E,$EF,$FF,$FB,$EE,$E2,$3F,$FF,$FE,$78,$7F,$FF
                FCB         $FF,$F0,$98,$60,$00,$3F,$80,$00,$00,$00,$03,$BB,$BB,$B8,$00,$00
                FCB         $00,$00,$00,$00,$01,$BB,$BB,$BA,$BB,$5F,$9F,$FF,$FC,$F8,$7F,$FF
                FCB         $FF,$F6,$00,$30,$00,$0F,$C0,$00,$00,$00,$00,$6A,$EE,$A0,$00,$00
                FCB         $00,$00,$00,$00,$00,$FF,$FF,$FA,$EE,$83,$FF,$FF,$FC,$F8,$7F,$FF
                FCB         $FF,$F4,$00,$00,$00,$07,$E0,$00,$00,$00,$05,$13,$BB,$80,$00,$00
                FCB         $00,$00,$00,$00,$00,$2E,$EE,$EA,$9B,$A3,$FF,$FF,$FD,$F0,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$01,$F0,$00,$00,$00,$00,$0E,$EE,$80,$00,$00
                FCB         $00,$00,$00,$00,$00,$2F,$FE,$AA,$0E,$ED,$FF,$FF,$F9,$F0,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$78,$00,$00,$00,$00,$03,$BB,$80,$00,$00
                FCB         $00,$00,$00,$00,$00,$0A,$BA,$A8,$0B,$FB,$7F,$FF,$F3,$F0,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$3C,$00,$00,$00,$00,$02,$8A,$80,$00,$00
                FCB         $00,$00,$00,$00,$00,$02,$FE,$A0,$06,$FE,$FF,$FF,$07,$F0,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$1E,$00,$00,$00,$00,$07,$03,$80,$00,$00
                FCB         $00,$00,$00,$00,$00,$02,$AA,$80,$07,$F9,$FF,$FC,$07,$F4,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$0F,$14,$0E,$00,$00,$02,$80,$80,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$AA,$80,$BF,$FF,$BF,$F0,$0F,$F6,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$03,$FF,$FC,$00,$00,$03,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$EA,$0F,$FE,$7C,$77,$80,$1F,$F6,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$01,$FF,$F8,$00,$00,$03,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$28,$7F,$FC,$FE,$80,$00,$1F,$F7,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$FF,$F0,$00,$00,$03,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$20,$37,$3C,$FE,$C0,$00,$BF,$F7,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$07,$7F,$E0,$00,$00,$F3,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$3F,$3E,$FF,$80,$01,$FF,$F7,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$A3,$FF,$C0,$00,$00,$63,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$3F,$FD,$EE,$80,$07,$FF,$F7,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$63,$FF,$C0,$00,$00,$73,$00,$00,$00,$00
                FCB         $FF,$00,$00,$00,$00,$00,$08,$1F,$78,$F7,$E8,$9F,$FF,$FC,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$77,$BF,$80,$00,$00,$33,$00,$00,$00,$3F
                FCB         $BB,$E0,$00,$00,$00,$00,$00,$1F,$F8,$3B,$55,$7F,$80,$70,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$01,$FF,$00,$00,$00,$3B,$00,$00,$00,$FF
                FCB         $3B,$FC,$00,$00,$00,$00,$00,$1F,$FF,$1D,$8E,$E7,$82,$F8,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$FF,$00,$00,$00,$30,$00,$00,$01,$FE
                FCB         $F9,$9F,$00,$00,$00,$00,$00,$0F,$FF,$C1,$17,$FF,$FF,$C0,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$18,$00,$00,$00,$F8
                FCB         $FF,$3F,$E0,$00,$04,$00,$00,$0F,$BF,$C2,$EF,$FF,$FF,$80,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$06,$00,$00,$00,$1C,$00,$00,$08,$FB
                FCB         $FF,$37,$E0,$00,$00,$00,$00,$0F,$BF,$C1,$1F,$FF,$FF,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$0E,$00,$00,$1D,$07
                FCB         $FF,$E3,$68,$00,$00,$00,$00,$07,$7F,$C7,$3F,$FF,$FE,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$1C,$00,$00,$00,$00,$00,$00,$00,$3F
                FCB         $FF,$FB,$F6,$00,$00,$00,$00,$07,$7F,$E1,$7F,$FF,$FC,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$0E,$00,$00,$00,$00,$00,$01,$C6,$7F
                FCB         $FF,$FF,$E8,$00,$00,$00,$00,$07,$0F,$F3,$8F,$97,$FC,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$B1,$EF
                FCB         $FF,$EF,$B7,$00,$00,$00,$00,$07,$83,$E1,$C6,$00,$78,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$21,$DF
                FCB         $FF,$D1,$A2,$00,$00,$40,$00,$03,$00,$F1,$FF,$14,$20,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$3E,$FF
                FCB         $FF,$FF,$DF,$00,$00,$00,$00,$03,$06,$70,$7C,$00,$C0,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0F,$FF
                FCB         $FF,$FF,$E7,$E0,$00,$00,$00,$03,$87,$F1,$F8,$03,$80,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1E,$AF
                FCB         $FF,$FF,$F7,$00,$00,$00,$00,$03,$8F,$FB,$F8,$20,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1F,$BF
                FCB         $FF,$FF,$BF,$E0,$00,$00,$00,$F3,$8D,$FF,$F8,$5F,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$1E,$FF
                FCB         $FF,$FF,$4F,$F0,$00,$00,$01,$E3,$87,$FF,$FC,$6E,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0D,$F7
                FCB         $BF,$FF,$EF,$E8,$00,$00,$00,$01,$1F,$FF,$FF,$FE,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$0B,$FF
                FCB         $9F,$FF,$FD,$F8,$00,$00,$00,$01,$BF,$FF,$FF,$FF,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$FF
                FCB         $FF,$FF,$8F,$E8,$00,$00,$00,$01,$BF,$FF,$FF,$FC,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$FF
                FCB         $FF,$FF,$FF,$F8,$00,$00,$00,$01,$BF,$FF,$FF,$F8,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$FF
                FCB         $FF,$9F,$FF,$F8,$00,$00,$00,$01,$3F,$FF,$FF,$F0,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FE,$00,$00,$07,$FF
                FCB         $1F,$FF,$F5,$FC,$00,$00,$00,$01,$3F,$FF,$FF,$E0,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$FC
                FCB         $3F,$FF,$BF,$F8,$00,$00,$00,$01,$3F,$85,$FF,$C0,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$FF
                FCB         $F7,$FF,$DF,$F8,$00,$00,$00,$00,$1D,$2A,$07,$80,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$0E,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$FF
                FCB         $FF,$FF,$FF,$F8,$00,$00,$00,$01,$06,$FF,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$1C,$00,$00,$00,$00,$00,$00,$10,$00,$00,$1F,$FF
                FCB         $FF,$FF,$DF,$F8,$00,$00,$00,$01,$17,$3D,$95,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$01,$FF,$80,$00,$07,$FF
                FCB         $FF,$FF,$EF,$F8,$00,$00,$00,$00,$0E,$EF,$DA,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$FF
                FCB         $FF,$FF,$F7,$F0,$00,$00,$00,$00,$15,$F7,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$FF
                FCB         $FF,$FF,$ED,$E0,$00,$00,$00,$00,$0F,$BF,$EC,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$FF
                FCB         $FF,$FF,$DC,$E0,$00,$00,$00,$00,$17,$7F,$D8,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$07,$FF
                FCB         $FF,$FF,$BC,$C0,$00,$00,$00,$00,$3F,$FF,$F8,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$FF
                FCB         $FF,$FF,$DE,$00,$00,$00,$00,$00,$7F,$FF,$F8,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$FF
                FCB         $FF,$FF,$FF,$80,$00,$00,$00,$00,$7F,$FF,$F8,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$FF
                FCB         $FF,$FB,$FF,$80,$00,$00,$00,$00,$7F,$FF,$F0,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$DF
                FCB         $FE,$C1,$F2,$00,$00,$00,$00,$18,$7F,$FF,$F0,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$83
                FCB         $00,$1F,$CC,$00,$00,$00,$00,$00,$7F,$FF,$F0,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$00
                FCB         $80,$3F,$98,$00,$00,$00,$00,$00,$7F,$FF,$F0,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$E2
                FCB         $46,$3E,$60,$00,$00,$00,$00,$00,$7F,$FF,$E0,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$80
                FCB         $33,$3C,$00,$00,$00,$00,$00,$00,$00,$C0,$60,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$A0
                FCB         $73,$10,$00,$00,$00,$00,$00,$00,$15,$3A,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$E0
                FCB         $08,$18,$00,$00,$00,$00,$00,$00,$02,$F6,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$E0
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$17,$B6,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$02,$ED,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$05,$2C,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$20,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$02,$B0,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$70,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$7F,$FF,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$78,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$40,$00,$00,$00,$7F,$FF,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$40,$00,$00,$00,$7F,$FF,$C0,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$7F,$FF,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$7F,$FF,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$7F,$FF,$80,$00,$00,$00,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$3C,$FF,$F3,$3F,$FF,$CF,$9F,$FF,$FF,$FF,$03,$FF
                FCB         $CC,$FF,$FF,$F3,$FF,$FF,$F8,$1F,$FE,$0F,$FF,$8F,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$18,$FF,$F3,$3F,$FF,$CF,$FF,$FF,$FF,$FF,$CF,$FF
                FCB         $CC,$FF,$FF,$FF,$FF,$FF,$FE,$7F,$FE,$7F,$FF,$CF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$00,$FF,$F3,$31,$9E,$49,$10,$E1,$FF,$FF,$CF,$FF
                FCB         $CC,$C6,$08,$23,$0F,$FF,$FE,$7F,$FE,$7C,$60,$CC,$70,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$24,$FF,$F0,$3C,$C0,$C3,$92,$4F,$FF,$FF,$CF,$FF
                FCB         $C0,$F2,$38,$F2,$7F,$FF,$FE,$7F,$FE,$1F,$23,$C9,$27,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$3C,$FF,$F3,$30,$C0,$C7,$92,$63,$FF,$FF,$CF,$FF
                FCB         $CC,$C2,$79,$F3,$1F,$FF,$FE,$7F,$FE,$7C,$27,$C8,$31,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$3C,$FF,$F3,$24,$E1,$C3,$92,$79,$FF,$FF,$CF,$FF
                FCB         $CC,$92,$79,$F3,$CF,$FF,$FE,$7F,$FE,$79,$27,$C9,$FC,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$3C,$9F,$F3,$30,$E1,$C9,$92,$43,$FF,$FF,$CE,$7F
                FCB         $CC,$C2,$79,$F2,$1F,$FF,$FE,$73,$FE,$0C,$27,$CC,$61,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F7,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$7F,$FF
                FCB         $FF,$F0,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$7F,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

ENDPIC          =           *               ; THE END OF THE AUTHOR'S PICTURE DATA

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
; UNUSED GARBAGE BYTES?
                FCB         $FF,$00,$00,$A0,$27,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $EF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$40,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; UNUSED GARBAGE BYTES?
                FCB         $FF,$00,$00,$A0,$27,$0D,$71,$01,$00,$02,$00
                FCB         $FF,$FF,$EF,$FA,$7F,$F7,$9F,$FD,$08,$80,$80,$80,$9C,$00,$10,$10
                FCB         $BF,$FF,$CF,$FF,$7F,$FA,$FB,$FA,$0E,$00,$04,$E0,$00,$00,$08,$00
                FCB         $FF,$EB,$FF,$F7,$FF,$FF,$DF,$F4,$08,$A0,$08,$10,$02,$00,$00,$10
                FCB         $F7,$FE,$37,$FF,$FF,$78,$BD,$F3,$0C,$40,$00,$E0,$03,$00,$00,$00
                FCB         $FF,$F7,$FF,$F7,$7F,$E7,$AF,$E1,$2E,$22,$18,$30,$81,$10,$00,$00
                FCB         $FF,$F7,$FF,$FB,$FF,$F3,$AF,$FD,$3D,$00,$80,$E5,$0B,$00,$00,$02
                FCB         $FF,$EF,$FF,$F3,$FF,$6D,$FE,$EF,$C9,$20,$84,$11,$1D,$00,$00,$00
                FCB         $FF,$F7,$FF,$F9,$FF,$BF,$E7,$F8,$0E,$24,$01,$91,$01,$00,$00,$00
                FCB         $FF,$EF,$FF,$FF,$FF,$D7,$7D,$FD,$05,$40,$00,$90,$06,$08,$00,$80
                FCB         $FF,$F7,$FF,$FF,$FF,$7B,$6F,$F2,$0B,$00,$01,$30,$01,$40,$00,$80
                FCB         $7F,$FF,$FE,$F1,$EF,$FB,$47,$FC,$84,$00,$04,$D0,$03,$00,$00,$00
                FCB         $EF,$67,$FF,$FB,$FF,$FF,$9F,$FC,$4D,$02,$41,$40,$03,$10,$00,$00
                FCB         $EF,$F7,$FF,$F3,$FF,$3F,$B1,$EF,$00,$80,$88,$B1,$C3,$00,$10,$00
                FCB         $EF,$ED,$EF,$F9,$FF,$F0,$DF,$FC,$09,$26,$40,$A1,$0B,$00,$00,$00
                FCB         $FF,$FF,$FE,$FE,$7F,$BA,$DF,$F3,$09,$00,$02,$20,$09,$00,$00,$00
                FCB         $27,$00,$04,$69,$4C,$00,$00,$20,$EF,$EF,$FF,$FF,$FF,$77,$B5,$63
                FCB         $C1,$01,$02,$10,$48,$20,$00,$00,$DF,$EF,$FF,$FB,$FF,$FB,$DF,$E1
                FCB         $09,$80,$80,$20,$03,$00,$00,$20,$FF,$FF,$FF,$FB,$FF,$FF,$3B,$F7
                FCB         $00,$22,$08,$E1,$17,$00,$00,$00,$EF,$F7,$EF,$FF,$FF,$DF,$AB,$DD
                FCB         $6D,$A1,$68,$A9,$0E,$00,$00,$00,$FF,$FF,$FF,$B7,$FF,$FF,$BD,$79
                FCB         $0F,$01,$40,$E0,$09,$00,$00,$00,$FF,$F6,$FF,$F3,$FF,$97,$E1,$77
                FCB         $01,$80,$0A,$10,$0C,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$BF,$8F,$FF
                FCB         $18,$20,$02,$91,$00,$00,$00,$00,$FF,$F7,$FF,$FF,$FF,$EB,$FF,$F9
                FCB         $0D,$80,$19,$40,$27,$00,$00,$00,$FF,$EF,$EF,$FF,$FF,$F1,$F6,$F7
                FCB         $02,$42,$03,$00,$CB,$00,$00,$00,$FF,$FF,$FF,$F3,$7F,$FE,$FF,$F9
                FCB         $49,$C0,$22,$20,$00,$00,$00,$00,$7F,$EF,$FF,$F9,$FF,$7F,$7F,$FF
                FCB         $11,$21,$00,$30,$29,$00,$00,$00,$FF,$FF,$FF,$F7,$FF,$EF,$CD,$E7
                FCB         $06,$02,$04,$C0,$0A,$00,$40,$00,$5F,$FB,$FF,$FB,$DF,$7D,$F5,$FB
                FCB         $00,$20,$20,$50,$0D,$00,$00,$00,$FD,$E7,$FF,$FF,$FF,$FC,$39,$D3
                FCB         $00,$00,$40,$C0,$0F,$00,$00,$08,$AF,$FF,$FF,$FB,$7F,$EF,$FF,$FC
                FCB         $08,$03,$30,$40,$0A,$00,$00,$00,$FF,$F7,$BF,$FF,$FF,$5C,$EE,$7D
                FCB         $9F,$1E,$CD,$F0,$35,$BC,$AF,$F7,$10,$00,$01,$00,$00,$00,$20,$00
                FCB         $97,$7E,$DB,$30,$07,$C4,$29,$B7,$00,$06,$41,$00,$00,$04,$00,$00
                FCB         $DF,$B6,$E7,$FC,$AD,$F5,$62,$F1,$00,$82,$41,$20,$00,$00,$00,$00
                FCB         $9D,$3E,$49,$F0,$BF,$C9,$7D,$F9,$00,$8E,$01,$00,$00,$00,$20,$00
                FCB         $9E,$7E,$67,$93,$1B,$55,$A5,$EF,$01,$18,$41,$00,$00,$04,$21,$20
                FCB         $BF,$BF,$73,$18,$0B,$75,$65,$F5,$00,$10,$00,$00,$00,$00,$01,$00
                FCB         $11,$5A,$FB,$60,$8D,$A6,$B7,$FD,$90,$00,$00,$00,$00,$00,$21,$20
                FCB         $B9,$DE,$5F,$E0,$5F,$3B,$A7,$FF,$00,$86,$01,$00,$00,$00,$20,$80
                FCB         $9F,$1E,$49,$11,$0F,$6F,$6D,$F4,$00,$04,$00,$00,$00,$04,$00,$00
                FCB         $3D,$5F,$43,$30,$4F,$06,$EF,$DF,$00,$06,$01,$00,$00,$00,$01,$20
                FCB         $B1,$1D,$45,$F8,$EE,$45,$31,$D7,$90,$16,$00,$00,$00,$04,$01,$40
                FCB         $BB,$3F,$67,$75,$97,$55,$EB,$F1,$00,$14,$01,$20,$00,$00,$00,$A0
                FCB         $9B,$1E,$85,$30,$4D,$45,$21,$F9,$00,$1A,$40,$00,$00,$04,$21,$80
                FCB         $9F,$1F,$41,$54,$05,$D3,$37,$7F,$80,$16,$40,$00,$00,$00,$00,$00
                FCB         $9B,$DA,$49,$50,$66,$7D,$6F,$FD,$00,$58,$40,$20,$00,$00,$00,$00
                FCB         $9B,$1F,$C1,$30,$00,$24,$25,$FF,$10,$10,$00,$00,$00,$00,$20,$81
                FCB         $00,$00,$41,$00,$08,$00,$01,$00,$5B,$FE,$5F,$A0,$49,$DF,$A3,$EB
                FCB         $00,$0C,$01,$00,$00,$00,$20,$00,$95,$1E,$45,$F4,$25,$5C,$6B,$FD
                FCB         $80,$4A,$40,$00,$00,$00,$01,$00,$16,$FE,$6F,$D0,$0F,$6C,$35,$B9
                FCB         $00,$16,$01,$00,$00,$04,$21,$00,$9B,$BF,$41,$90,$1F,$E4,$AB,$6F
                FCB         $00,$0C,$40,$00,$00,$00,$20,$00,$BE,$FE,$4F,$7C,$0F,$95,$A1,$F7
                FCB         $90,$02,$00,$10,$00,$00,$21,$84,$9C,$FE,$53,$D0,$7C,$54,$03,$ED
                FCB         $80,$86,$41,$00,$00,$00,$20,$00,$97,$9E,$53,$60,$3E,$35,$BB,$F6
                FCB         $00,$18,$41,$00,$00,$00,$20,$80,$DF,$7D,$2B,$D0,$9F,$A4,$FF,$FC
                FCB         $10,$0E,$40,$10,$00,$00,$01,$20,$1B,$9B,$59,$F0,$20,$15,$7F,$7F
                FCB         $00,$0E,$00,$00,$00,$04,$20,$80,$D8,$3F,$5D,$00,$0F,$05,$67,$F8
                FCB         $00,$06,$40,$00,$00,$04,$21,$00,$B1,$5E,$43,$33,$2E,$1D,$A5,$FD
                FCB         $10,$00,$40,$00,$00,$00,$01,$01,$95,$1E,$97,$61,$07,$94,$65,$5F
                FCB         $00,$08,$01,$10,$00,$00,$21,$00,$9B,$FF,$45,$03,$23,$B4,$BF,$FF
                FCB         $00,$0A,$01,$40,$00,$00,$00,$00,$D9,$5C,$59,$E9,$4F,$D5,$29,$FF
                FCB         $00,$18,$40,$00,$00,$00,$01,$10,$95,$1E,$05,$90,$8F,$15,$A5,$F4
                FCB         $90,$1E,$01,$00,$00,$00,$00,$40,$6F,$E1,$BE,$FF,$FF,$FB,$DE,$F7
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



; THE NEW SUPER EXTENDED BASIC CODE STARTS HERE
; THE CODE FROM THIS POINT TO $FDFF IS THE ENHANCEMENTS ADDED TO THE 'OLD' COCO BASIC
; TO SUPPORT THE NEW FEATURES AVAILABLE IN THE COCO 3.
; THESE ARE THE ONLY 'SANCTIONED BY TANDY' LEGAL ENTRY POINTS INTO THE SUPER
; EXTENDED (ENHANCED) PORTION OF THE BASIC ROM
SUPERVAR        FDB         HRMODE          ; ADDRESS OF DIRECT PAGE VARIABLES UNIQUE TO ENHANCED BASIC
PRGTEXT         FDB         SETTEXT         ; SET THE VIDEO CONTROL REGISTERS TO DISPLAY HI-RES TEXT
PRGGRAPH        FDB         SETGRAPH        ; SET THE VIDEO CONTROL REGISTERS TO DISPLAY HI-RES GRAPHICS
PRGMMU          FDB         SETMMU          ; PROGRAM THE MMU REGISTERS FROM THEIR IMAGES
GETTEXT         FDB         SELTEXT         ; PLACE THE HI-RES TEXT SCREEN INTO LOGICAL BLOCK 1
GETBLOK0        FDB         SELBLOK0        ; PLACE THE BLOCK NUMBER IN ACCB INTO LOGICAL BLOCK 0
GETTASK0        FDB         SELTASK0        ; RE-SELECT TASK REGISTER 0
GETTASK1        FDB         SELTASK1        ; SELECT TASK REGISTER 1
                JMP         LA05E           ; EXECUTE A ROM CARTRIDGE ($A05E)
SPARE0          FDB         $0000           ; UNDEFINED
SPARE1          FDB         $0000           ; UNDEFINED
SPARE2          FDB         $0000           ; UNDEFINED
; SET UP THE VIDEO CONTROL REGISTERS ACCORDING TO THE SELECTED WIDTH
SETTEXT         PSHS        Y,X,A
                LBRN        RAMLINK         ; RAM HOOK
                LDX         #IM.TEXT        ; POINT TO THE 32 COLUMN VIDEO MODE REGISTER TABLE
                LDA         HRWIDTH         ; CHECK THE HI-RES TEXT MODE
                BEQ         SETVIDEO        ; BRANCH IF 32 COLUMN MODE
                LDX         #SE03B          ; POINT TO THE 40 COLUMN VIDEO MODE REGISTER TABLE
                CMPA        #$01            ; VIDEO MODE WIDTH SET TO 40 COLUMN?
                BEQ         SETVIDEO        ; YES
                LDX         #SE044          ; POINT TO THE 80 COLUMN VIDEO MODE REGISTER TABLE
                BRA         SETVIDEO


; VIDEO MODE REGISTER IMAGES FOR THE HI-RES TEXT MODES
; INITIAL VIDEO CONTROL REGISTER DATA FOR 32 COLUMN COCO COMPATIBLE MODE
IM.TEXT         FCB         COCO+MMUEN+MC3+MC2 ; FF90

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
                FCB         $00
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCB         $00 | $08
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FCB         $00,$00,$00,$0F,$E0 ; FF98
                FCB         $00,$00
; INITIAL VIDEO CONTROL REGISTER DATA FOR 40 COLUMN HI-RES MODE
SE03B           FCB         MMUEN+MC3+MC2   ; FF90

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
                FCB         $03
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCB         $03 | $08
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FCB         $05,$12,$00,$00,$D8 ; FF98
                FCB         $00,$00
; INITIAL VIDEO CONTROL REGISTER DATA FOR 80 COLUMN HI-RES MODE
SE044           FCB         MMUEN+MC3+MC2   ; FF90

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
                FCB         $03
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCB         $03 | $08
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FCB         $15,$12,$00,$00,$D8 ; FF98
                FCB         $00,$00
SETGRAPH        PSHS        Y,X,A
                LBRN        RAMLINK         ; RAM HOOK
                LDX         #IM.GRAPH       ; POINT TO THE VIDEO MODE RAM IMAGE FOR HSCREEN MODES 1,2
                LDY         #RESTABLE       ; POINT TO THE VIDEO RESOLUTION TABLE
                LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                CMPA        #$02            ; 1 OR 2 ARE 40 COLUMN MODES
                BLS         SE063           ; BRANCH IF 40 COLUMN TEXT MODE
                LDX         #SE079          ; POINT TO THE VIDEO RAM IMAGE FOR 80 COLUMN MODE
SE063           SUBA        #$01            ; ADJUST MODE NUMBERS TO START AT ZERO
                LDA         A,Y             ; GRAB THE PROPER VIDEO RESOLUTION MODE
                STA         $02,X           ; SAVE IT IN THE PROPER IMAGE
                JMP         >SETVIDEO       ; GO SET UP THE VIDEO REGISTERS
; VIDEO RESOLUTION MODE REGISTER (FF99) DATA FOR HSCREEN MODES
RESTABLE        FCB         $15             ; 320 PIXELS, 4 COLORS
                FCB         $1E             ; 320 PIXELS, 16 COLORS
                FCB         $14             ; 640 PIXELS, 2 COLORS
                FCB         $1D             ; 640 PIXELS, 4 COLORS
; VIDEO MODE REGISTER IMAGES FOR THE HI-RES GRAPHICS MODES
; VIDEO MODE REGISTER IMAGE FOR THE 320x192 GRAPHICS MODE
IM.GRAPH        FCB         MMUEN+MC3+MC2   ; FF90

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
                FCB         $80
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCB         $80 | $08
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FCB         $00,$00,$00,$00,$C0 ; FF98
                FCB         $00,$00
; VIDEO MODE REGISTER IMAGE FOR THE 640x192 GRAPHICS MODE
SE079           FCB         MMUEN+MC3+MC2   ; FF90

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
                FCB         $80
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCB         $80 | $08
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FCB         $00,$00,$00,$00,$C0 ; FF98
                FCB         $00,$00
; PROGRAM INIT0 AND THE 8 VIDEO MODE REGISTERS
; ENTER WITH X POINTING TO THE DATA TO PUT INTO THE REGISTERS
SETVIDEO        LDA         ,X+             ; GET THE FIRST BYTE
                STA         INIT0           ; AND PUT IT INTO INIT0
                LDY         #VIDEOMOD       ; POINT TO THE VIDEO MODE REGISTERS
SE08B           LDA         ,X+             ; GET A BYTE
                STA         ,Y+             ; AND STICK IT INTO THE VIDEO MODE REGISTER
                CMPY        #MMUREG         ; END OF THE VIDEO MODE REGISTERS?
                BCS         SE08B           ; NO - KEEP STUFFING REGISTERS
                PULS        A,X,Y,PC
; PROGRAM THE MMU REGISTERS; ENTER WITH X POINTING TO THE DATA TO PLACE INTO THE MMU REGISTERS
SETMMU          PSHS        Y,X,B,A
                LEAX        IM.MMU,PC       ; POINT TO THE RAM IMAGE OF THE MMU REGISTERS
                BSR         SE0F1           ; MOVE 16 BYTES INTO THE MMU REGISTERS
                PULS        A,B,X,Y,PC
; PLACE A BLOCK INTO LOGICAL ADDRESS SPACE BLOCK 0.
; ENTER WITH ACCB CONTAINING THE BLOCK NUMBER TO BE PLACED INTO THE LOGICAL ADDRESS SPACE
; EXIT WITH BLOCK 7.0 REPLACED IN BLOCK 0 OF THE LOGICAL ADDRESS SPACE RAM IMAGE
SELBLOK0        PSHS        Y,X,B,A
                LEAX        IM.MMU,PC       ; POINT TO THE RAM IMAGE OF THE MMU REGISTERS
                PSHS        X               ; TEMP SAVE
                STB         ,X              ; SAVE THE NEW BLOCK NUMBER IN LOGICAL ADDRESS SPACE BLOCK 0 (TR0)
                BSR         SE0F1           ; COPY THE RAM IMAGE OF THE MMU REGISTERS INTO THE MMU REGISTERS
                LDB         #BLOCK7.0       ; GET BLOCK 7.0
                PULS        X               ; RESTORE THE MMU IMAGE POINTER
                STB         ,X              ; RESTORE BLOCK 7.0 TO BLOCK 0 OF MMU RAM IMAGE
                PULS        A,B,X,Y,PC
; PLACE THE HI-RES TEXT SCREEN INTO LOGICAL ADDRESS SPACE BLOCK 1
; EXIT WITH BLOCK 7.1 REPLACED INTO BLOCK 1 OF THE LOGICAL ADDRESS SPACE RAM IMAGE
SELTEXT         PSHS        Y,X,B,A
                LEAX        IM.MMU,PC       ; POINT TO THE RAM IMAGE OF THE MMU REGISTERS
                PSHS        X               ; TEMP SAVE
                LDB         #BLOCK6.6       ; GET THE BLOCK WHICH CONTAINS THE HI-RES TEXT SCREEN
                STB         $01,X           ; AND SAVE IT IN THE MMU IMAGE OF TASK REGISTER 0
                BSR         SE0F1           ; COPY THE RAM IMAGE OF THE MMU REGISTERS INTO THE MMU REGISTERS
                PULS        X               ; RESTORE THE MMU IMAGE PONTER
                LDB         #BLOCK7.1       ; GET BLOCK 7.1 (BASIC'S NORMAL LOGICAL BLOCK 1)
                STB         $01,X           ; AND SAVE IT IN THE MMU IMAGE
                PULS        A,B,X,Y,PC
SE0CB           PSHS        Y,X,B,A
                LEAX        IM.MMU,PC       ; POINT TO THE MMU RAM IMAGE
                PSHS        X               ; TEMP SAVE
                LDB         #BLOCK6.4       ; GET BLOCK 6.4
                STB         14,X            ; AND SAVE IT IN LOGICAL BLOCK 6 OF TASK REGISTER 1
                BSR         SE0F1           ; COPY THE RAM IMAGE OF THE MMU REGISTERS INTO THE MMU REGISTERS
                PULS        X               ; RESTORE MMU IMAGE POINTER
                LDB         #BLOCK6.5       ; GET THE 'NORMAL' BLOCK FOR TASK REGISTER 1, LOGICAL BLOCK 6
                STB         14,X            ; PUT IT BACK INTO TASK REGISTER 1 IMAGE
                PULS        A,B,X,Y,PC
; MASTER IMAGES USED TO PROGRAM THE CUSTOM CHIP'S MMU REGISTERS
; TASK REGISTER 0
IM.MMU          FCB         BLOCK7.0,BLOCK7.1,BLOCK7.2,BLOCK7.3,BLOCK7.4,BLOCK7.5,BLOCK7.6,BLOCK7.7
; TASK REGISTER 1
                FCB         BLOCK7.0,BLOCK6.0,BLOCK6.1,BLOCK6.2,BLOCK6.3,BLOCK7.5,BLOCK6.5,BLOCK7.7
; COPY 16 BYTES INTO THE MMU REGISTERS
; ENTER WITH X POINTING TO THE 16 BYTES
SE0F1           LDY         #MMUREG         ; POINT TO THE MMU REGISTERS
                LDB         #16             ; 16 MMU REGISTERS
SE0F7           LDA         ,X+             ; GET A BYTE
                STA         ,Y+             ; AND PUT IT INTO THE MMU REGISTER
                DECB                        ;  DECREMENT THE BYTE COUNT
                BNE         SE0F7           ; KEEP GOING UNTIL ALL REGISTERS MOVED
                RTS
; SELECT TASK REGISTER 0 AS THE ACTIVE TASK REGISTER
; ENTER WITH THE STACK POINTING TO A TEMPORARY LOCATION; THE PERMANENT
; STACK POINTER WAS SAVED ON THIS TEMPORARY STACK WHEN TASK REGISTER 1
; WAS SELECTED AS THE ACTIVE TASK REGISTER
SELTASK0        STD         V40             ; TEMPORARILY SAVE ACCD
                LDD         ,S              ; GET THE RETURN ADDRESS OFF THE STACK
                STD         V42             ; AND TEMPORARILY SAVE IT IN V42
                LDD         $02,S           ; GET THE PERMANENT STACK POINTER FROM THE STACK
                STD         V44             ; AND TEMPORARILY SAVE IT IN V44
                CLRB                        ;  TASK REGISTER 0 AND TIMER INPUT OF 63.5 MICROSECONDS
                STB         INIT1           ; PROGRAM INITIALIZATION REGISTER 1
                LDS         V44             ; RESET THE STACK POINTER
                LDD         V42             ; GET BACK THE RETURN ADDRESS
                PSHS        B,A             ; AND PUT IT ONTO THE STACK
                LDD         V40             ; RESTORE ACCD
                ANDCC       #$AF            ; TURN ON IRQ, FIRQ
                RTS
; SELECT TASK REGISTER 1 AS THE ACTIVE TASK REGISTER
; EXIT WITH THE STACK POINTER SET TO A TEMPORARY LOCATION
SELTASK1        ORCC        #$50            ; DISABLE INTERRUPTS
                STD         V40             ; TEMPORARILY SAVE ACCD IN V40
                PULS        A,B             ; GET THE RETURN ADDRESS
                STD         V42             ; AND TEMPORARILY SAVE IT IN V42
                STS         V44             ; TEMPORARILY SAVE THE STACK POINTER IN V44
                LDB         #$01            ; TASK REGISTER 1 AND TIMER INPUT AT 63.5 MICROSECONDS
                STB         INIT1           ; SETUP INITIALIZATION REGISTER 1
                LDS         #TMPSTACK       ; PUT THE STACK JUST BELOW THE START OF ENHANCED BASIC
                LDD         V44             ; GET THE OLD STACK POINTER BACK
                PSHS        B,A             ; AND STUFF IT ONTO THE STACK
                LDD         V42             ; GET THE RETURN ADDRESS BACK
                PSHS        B,A             ; AND STUFF IT ONTO THE STACK TOO
                LDD         V40             ; GET BACK ACCD
                RTS
; CRUNCH A TOKEN PATCH ENTERED FROM $B8D4
ALINK2          TST         V41             ; CHECK THE TOKEN FLAG
                BNE         SE152           ; BRANCH IF IT IS A FUNCTION TOKEN
                LDA         V42             ; GET THE TOKEN COUNTER
                CMPA        #$62            ; COMPARE TO THE FIRST ENHANCED BASIC TOKEN
                BLS         SE148           ; BRANCH IF BEFORE FIRST TOKEN
                LDU         #COMVEC-5       ; POINT U TO EXTENDED COLOR BASIC'S INTERPRETATION TABLE
                JMP         LB8D7           ; RE-ENTER THE MAIN STREAM CODE
SE148           LDA         #$62            ; FORCE THE TOKEN COUNTER TO THE FIRST ENHANCED BASIC TOKEN NUMBER
                LDU         #SE158          ; POINT TO ENHANCED BASIC'S COMMAND INTERPRETATION TABLE
SE14D           STA         V42             ; SAVE THE NEW TOKEN COUNTER
                JMP         LB89D           ; RE-ENTER THE MAIN STREAM CODE
SE152           LDA         V42             ; GET THE TOKEN COUNTER
                CMPA        #$29            ; COMPARE TO THE FIRST ENHANCED FUNCTION TOKEN NUMBER
                BLS         SE15B           ; BRANCH IF LESS THAN ENHANCED TOKEN NUMBER
SE158           JMP         LB8D7           ; RE-ENTER THE MAIN STREAM CODE
SE15B           LDA         #$29            ; FORCE COUNTER TO FIRST ENHANCED FUNCTION
SE15D           LDU         #SE15D          ; POINT TO THE ENHANCED FUNCTION INTERPRETATION TABLE
                BRA         SE14D
; BASIC 2.0 COMMAND INTERPRETATION VECTOR TABLE
EBCOMTAB        FCB         23              ; 23 BASIC 2.0 COMMANDS
                FDB         COMDIC20        ; BASIC 2.0'S COMMAND DICTIONARY
                FDB         ALINK4          ; COMMAND PROCESSING ROUTINE ENTRY POINT
                FCB         5               ; 5 BASIC 2.0 FUNCTIONS
                FDB         FUNDIC20        ; FUNCTION DICTIONARY TABLE
                FDB         ALINK5          ; FUNCTION PROCESSING ROUTINE ENTRY POINT
                FCB         $00,$00,$00,$00,$00,$00 ; DUMMY SPACE USED TO SIMULATE AN EMPTY COMMAND INTERP. VECTOR TABLE
; UNCRUNCH A TOKEN PATCH ENTERED FROM $B7F3
ALINK3          LEAU        10,U            ; SKIP TO THE NEXT COMMAND INTERPRETATION TABLE
                TST         ,U              ; IS THIS A VALID TABLE?
                LBNE        LB7F9           ; YES - RE-ENTER THE MAIN STREAM CODE
                LEAX        $-01,X          ; UNNECESSARY INSTRUCTION; NEXT ONE SHOULD JUST BE LDA -1,X
                LDA         ,X+             ; GET THE TOKEN FROM BASIC'S INPUT LINE
                ANDA        #$7F            ; STRIP OFF THE $80 COMMAND TOKEN BIAS
                CMPA        #$62            ; FIRST LEGAL BASIC 2.0 COMMAND TOKEN NUMBER
                BCS         SE18B           ; BRANCH IF LEGAL TOKEN
                SUBA        #$62            ; ADJUST BASIC 2.0 TOKENS TO START AT 0
                LDU         #SE158          ; POINT TO ENHANCED BASIC'S COMMAND INTERPRETATION TABLE
                BRA         ALINK3
SE18B           SUBA        #$29            ; SUBTRACT OUT THE FIRST ENHANCED FUNCTION TABLE
                LDU         #SE15D          ; POINT U TO BE ABLE TO SEARCH FOR AN ENHANCED FUNCTION TOKEN
                BRA         ALINK3
; BASIC 2.0 COMMAND PROCESSING ROUTINE ENTRY POINT PATCH ENTERED FROM $8150
ALINK4          CMPA        #$E2            ; TOKEN NUMBER OF FIRST ENHANCED BASIC COMMAND
                BCS         SE19A           ; BRANCH IF LESS THAN ENHANCED TOKEN
                CMPA        #$F8            ; COMPARE TO THE HIGHEST ENHANCED BASIC TOKEN
                BLS         SE19E           ; BRANCH IF ENHANCED BASIC TOKEN
SE19A           JMP         [COMVEC+23]     ; GO TO DISK BASIC'S COMMAND HANDLER
SE19E           SUBA        #$E2            ; SUBTRACT OUT THE NON-ENHANCED BASIC TOKENS
                LDX         #COMDIS20       ; POINT X TO ENHANCED BASIC'S COMMAND DISPATCH TABLE
                JMP         LADD4           ; RE-ENTER THE MAIN STREAM CODE
; BASIC 2.0 FUNCTION PROCESSING ROUTINE PATCH ENTERED FROM $816C
ALINK5          CMPB        #$52            ; COMPARE TO THE FIRST ENHANCED BASIC FUNCTION TOKEN
                BCS         SE1AE           ; BRANCH IF LESS THAN ENHANCED TOKEN
                CMPB        #$5A            ; COMPARE TO THE HIGHEST FUNCTION TOKEN
                BLS         SE1B2           ; BRANCH IF ENHANCED TOKEN
SE1AE           JMP         [COMVEC+28]     ; JUMP TO DISK BASIC'S FUNCTION HANDLER
SE1B2           SUBB        #$52            ; SUBTRACT OUT THE NON-ENHANCED BASIC TOKENS
                CMPB        #2*2            ; CHECK FOR LPEEK, BUTTON, HPOINT
                BCC         SE1BF           ; BRANCH IF ERNO, ERLIN
                PSHS        B               ; SAVE THE TOKEN COUNTER
                JSR         LB262           ; EVALUATE AN EXPRESSION IN PARENTHESIS
                PULS        B               ; RESTORE THE TOKEN COUNTER
SE1BF           LDX         #FUNDIS20       ; POINT TO ENHANCED BASIC'S FUNCTION DISPATCH TABLE
                JMP         LB2CE           ; RE-ENTER THE MAIN STREAM CODE

; BASIC 2.0 COMMAND DICTIONARY TABLE

; TOKEN #
COMDIC20        FCS         'WIDTH'         ; E2
                FCS         'PALETTE'       ; E3
                FCS         'HSCREEN'       ; E4
                FCS         'LPOKE'         ; E5
                FCS         'HCLS'          ; E6
                FCS         'HCOLOR'        ; E7
                FCS         'HPAINT'        ; E8
                FCS         'HCIRCLE'       ; E9
                FCS         'HLINE'         ; EA
                FCS         'HGET'          ; EB
                FCS         'HPUT'          ; EC
                FCS         'HBUFF'         ; ED
                FCS         'HPRINT'        ; EE
                FCS         'ERR'           ; EF
                FCS         'BRK'           ; F0
                FCS         'LOCATE'        ; F1
                FCS         'HSTAT'         ; F2
                FCS         'HSET'          ; F3
                FCS         'HRESET'        ; F4
                FCS         'HDRAW'         ; F5
                FCS         'CMP'           ; F6
                FCS         'RGB'           ; F7
                FCS         'ATTR'          ; F8

; BASIC 2.0 COMMAND DISPATCH TABLE

; TOKEN #
COMDIS20        FDB         WIDTH           ; WIDTH E2
                FDB         PALETTE         ; PALETTE E3
                FDB         HSCREEN         ; HSCREEN E4
                FDB         LPOKE           ; LPOKE E5
                FDB         HCLS            ; HCLS E6
                FDB         HCOLOR          ; HCOLOR E7
                FDB         HPAINT          ; HPAINT E8
                FDB         HCIRCLE         ; HCIRCLE E9
                FDB         HLINE           ; HLINE EA
                FDB         HGET            ; HGET EB
                FDB         HPUT            ; HPUT EC
                FDB         HBUFF           ; HBUFF ED
                FDB         HPRINT          ; HPRINT EE
                FDB         ERR             ; ERR EF
                FDB         BRK             ; BRK F0
                FDB         LOCATE          ; LOCATE F1
                FDB         HSTAT           ; HSTAT F2
                FDB         HSET            ; HSET F3
                FDB         HRESET          ; HRESET F4
                FDB         HDRAW           ; HDRAW F5
                FDB         CMP             ; CMP F6
                FDB         RGB             ; RGB F7
                FDB         ATTR            ; ATTR F8

; BASIC 2.0 FUNCTION DICTIONARY TABLE

; TOKEN #
FUNDIC20        FCS         'LPEEK'         ; A8
                FCS         'BUTTON'        ; A9
                FCS         'HPOINT'        ; AA
                FCS         'ERNO'          ; AB
                FCS         'ERLIN'         ; AC

; BASIC 2.0 FUNCTION DISPATCH TABLE

; TOKEN #
FUNDIS20        FDB         LPEEK           ; LPEEK A8
                FDB         BUTTON          ; BUTTON A9
                FDB         HPOINT          ; HPOINT AA
                FDB         ERNO            ; ERNO AB
                FDB         ERLIN           ; ERLIN AC
; PRINT THE COPYRIGHT MESSAGE PATCH ENTERED FROM $80B2
ALINK12         LDX         #L80E6+1        ; POINT TO EXTENDED BASIC'S COPYRIGHT MESSAGE
                JSR         >STRINOUT       ; COPY A STRING FROM (X) TO CONSOLE OUT
                LDX         #MWAREMS-1      ; MICROWARE'S COPYRIGHT MESSAGE
                JSR         >STRINOUT       ; COPY A STRING FROM (X) TO CONSOLE OUT
                JMP         L80B8           ; EXTENDED BASIC'S WARM START REENTRY
; PRINT THE DISK BASIC 2.0 COPYRIGHT MESSAGE PATCH ENTERED FROM $C0C6
SHOWDM20        LDX         #DISK20MS-1     ; POINT TO DISK BASIC 2.0 MESSAGE
                JMP         DC0DC-19        ; COPY MESSAGE TO SCREEN AND WARM START DISK BASIC 2.0
; PRINT THE DISK BASIC 2.1 COPYRIGHT MESSAGE PATCH ENTERED FROM $C0C6
SHOWDM21        LDX         #DISK21MS-1     ; POINT TO DISK BASIC 2.1 MESSAGE
                JMP         DC0DC           ; COPY MESSAGE TO SCREEN AND WARM START DISK BASIC 2.1
DISK20MS        FCC         'DISK EXTENDED COLOR BASIC 2.0'
                FCB         $0D
                FCC         'COPR. 1981, 1986 BY TANDY'
                FCB         $0D
                FCC         'UNDER LICENSE FROM MICROSOFT'
                FCB         $0D
MWAREMS         FCC         'AND MICROWARE SYSTEMS CORP.'
SE313           FCB         $0D,$0D,$00
DISK21MS        FCC         'DISK EXTENDED COLOR BASIC 2.1'
                FCB         $0D
                FCC         'COPR. 1982, 1986 BY TANDY'
                FCB         $0D
                FCC         'UNDER LICENSE FROM MICROSOFT'
                FCB         $0D
                FCC         'AND MICROWARE SYSTEMS CORP.'
                FCB         $0D,$0D,$00
; GRAPHICS INITIALIZATION PATCH ENTERED FROM $9703
ALINK14         CLRA                        ;
                CLRB                        ;
                LBRN        RAMLINK         ; RAM HOOK
                STB         >H.CRSATT       ; SET CURSOR ATTRIBUTES TO ZERO
                STD         HRMODE          ; SET HI-RES GRAPHICS AND TEXT MODES TO OFF
                STD         >H.ONBRK        ; RESET THE ON BRK ADDRESS TO ZERO; NON-INITIALIZED
                STD         >H.ONERR        ; RESET THE ON ERROR ADDRES TO ZERO; NON-INITIALIZED
                STA         >H.BCOLOR       ; PALETTE REGISTER ZERO IS THE DEFAULT BACKGROUND COLOR
                LDA         #$01            ; DEFAULT PALETTE REGISTER FOR THE FOREGROUND COLOR
                STA         >H.FCOLOR       ; USE PALETTE REGISTER1 AS THE FOREGROUND COLOR
                LDA         #BLOCK6.4       ; GET THE HPUT/HGET BUFFER BLOCK
                STA         >MMUREG         ; PIT IT INTO LOGICAL BLOCK 0
                LDD         #$FFFF          ; HPUT/HGET BUFFER EMPTY FLAG
                STD         $0              ; RESET THE HPUT/HGET BUFFER TO EMPTY
                LDA         #BLOCK7.0
                STA         >MMUREG         ; RESTORE BLOCK 7.0 TO LOGICAL BLOCK 0 OF TASK REGISTER 0
                JMP         LAD19           ; GO DO A COMPLETE 'NEW'
; ON COMMAND (FOR ON ERR AND ON BRK) PATCH ENTERED FROM $AF42
ALINK18         CMPA        #$EF            ; 'ERR' TOKEN
                BEQ         ERR
                CMPA        #$F0            ; 'BRK' TOKEN
                BEQ         BRK
                JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                JMP         LAF45           ; JUMP TO THE ON COMMAND($AF45)
SE3C2           JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
                CMPA        #$81            ; 'GO' TOKEN
                BNE         SE3CF           ; SYNTAX ERROR IF NOT GO
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
                CMPA        #$A5            ; 'TO' TOKEN
                BNE         SE3CF           ; SYNTAX ERROR IF NOT GOTO
                RTS
SE3CF           LEAS        $02,S           ; REMOVE ONE RETURN ADDRESS FROM THE STACK
                JMP         LB277           ; 'SYNTAX' ERROR
; ERR
ERR             BSR         SE3C2           ; CHECK FOR THE 'GO' AND 'TO' TOKENS
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
                JSR         LAF67           ; STRIP THE 'GOTO' LINE NUMBER FROM THE BASIC INPUT LINE
                LDD         BINVAL          ; GET THE 'GOTO' LINE NUMBER
                STD         >H.ONERR        ; SAVE IT
                LDD         CURLIN          ; GET THE CURRENT LINE NUMBER
                STD         >H.ONERRS       ; AND SAVE IT AS THE SOURCE LINE NUMBER
                RTS
; BRK
BRK             BSR         SE3C2           ; CHECK FOR THE 'GO' AND THE 'TO' TOKENS
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
                JSR         LAF67           ; STRIP THE 'GOTO' LINE NUMBER FROM THE BASIC INPUT LINE
                LDD         BINVAL          ; GET THE 'GOTO' LINE NUMBER
                STD         >H.ONBRK        ; SAVE IT
                LDD         CURLIN          ; GET THE CURRENT LINE NUMBER
                STD         >H.ONBRKS       ; AND SAVE IT AS THE SOURCE LINE NUMBER
                RTS
; &H TYPE VARIABLE EVALUATION PATCH ENTERED FROM $8834
ALINK6A         LSL         $02,X           ;
                ROL         $01,X           ; MULTIPLY THE TEMPORARY
                ROL         ,X              ; ACCUMULATOR BY TWO
                LBCS        LBA92           ; 'OV' OVERFLOW ERROR ($BA92)
                DECB                        ;  DECREMENT THE SHIFT COUNTER
                BNE         ALINK6A         ; LOOP UNTIL DONE
                SUBA        #'0             ; MASK OFF ASCII
                ADDA        $02,X           ; ADD DIGIT TO TEMPORARY
                STA         $02,X           ; ACCUMULATOR AND SAVE IT
                RTS
; &H TYPE VARIABLE EVALUATION PATCH ENTERED FROM $8843
ALINK6B         LBCS        L8800           ; ($8800)
                JMP         L883F           ; ($883F)
; BASIC'S LINE INPUT PATCH ENTERED FROM $A3C2
ALINK16         CMPA        #$03            ; BREAK KEY DEPRESSED?
                ORCC        #$01            ; SET THE CARRY FLAG
                BNE         SE426           ; BRANCH IF NOT THE BREAK KEY
                PSHS        A,CC            ; SAVE REGISTERS
                LDA         HRMODE          ; CHECK THE HI-RES GRAPHICS MODE
                BEQ         SE424           ; BRANCH IF IN COCO COMPATIBLE MODE
                CLR         HRMODE          ; FORCE TO COCO COMPATIBLE MODE
                JSR         SETTEXT         ; PROGRAM THE VIDEO MODE REGISTERS
SE424           PULS        CC,A            ; RESTORE REGISTERS
SE426           JMP         LA3C6           ; RE-ENTER THE MAIN STREAM OF CODE ($A3C6)
; BREAK CHECK PATCH ENTERED FROM $ADF0
ALINK15         CMPA        #$03            ; BREAK KEY DEPRESSED?
                BEQ         SE430           ; YES
                JMP         LADF4           ; RE-ENTER THE MAIN STREAM OF CODE ($ADF4)
SE430           LDA         #$01            ; 'BREAK' FLAG
                STA         >H.ERRBRK       ; SAVE IN THE ERROR/BREAK FLAG
                LDA         CURLIN          ; DIRECT MODE?
                INCA                        ;  $FF SIGNIFIES DIRECT MODE
                BEQ         SE43F           ; BRANCH IF DIRECT MODE
                LDD         >H.ONBRK        ; HAS AN ON BRK TRAP BEEN SET UP?
                BNE         SE449           ; YES
SE43F           LDA         HRMODE          ; CHECK THE HI-RES GRAPHICS MODE
                BEQ         SE446           ; BRANCH IF COCO COMPATIBLE
                JSR         SETTEXT         ; PROGRAM THE VIDEO DISPLAY REGISTERS
SE446           JMP         STOP            ; JUMP TO THE STOP COMMAND ($AE09)
SE449           STD         BINVAL          ; SAVE THE SEARCH LINE NUMBER
                TST         >H.ERRBRK       ; CHECK THE ERROR/BREAK FLAG
                BNE         SE458           ; BRANCH IF BREAK
                LDS         FRETOP          ; IF ERROR, RESET THE STACK POINTER
                LDD         #LADC4          ; GET THE ADDRESS ($ADC4) OF THE MAIN COMMAND INTERPRETATION
                PSHS        B,A             ; LOOP AND SAVE IT AS THE NEW RETURN ADDRESS
SE458           JSR         LAEEB           ; MOVE THE INPUT POINTER TO THE END OF THE LINE
                LEAX        $01,X           ; SKIP TO THE START OF THE NEXT LINE
                LDD         BINVAL          ; GET THE LINE NUMBER WE'RE LOOKING FOR
                CMPD        CURLIN          ; COMPARE TO THE CURRENT LINE NUMBER
                BHI         SE466           ; BRANCH IF SEARCH LINE NUMBER GREATER THAN CURRENT LINE NUMBER
                LDX         TXTTAB          ; POINT X TO THE BEGINNING OF THE PROGRAM
SE466           JSR         LAD05           ; SEARCH FOR THE PROGRAM LINE NUMBER IN ACCD
                LBCS        SE51E           ; BRANCH IF LINE NUMBER NOT FOUND
                JMP         LAEBB           ; RESET BASIC'S INPUT POINTER AND RETURN ($AEBB)
; ERROR SERVICING ROUTINE PATCH ENTERED FROM $AC46
ALINK20         CLR         >H.ERRBRK       ; SET THE ERROR/BREAK FLAG TO ERROR (0)
                LDA         CURLIN          ; GET THE CURRENT LINE NUMBER
                INCA                        ;  CHECK FOR DIRECT MODE
                BEQ         SE47D           ; BRANCH IF DIRECT MODE
                LDX         >H.ONERR        ; HAS AN ON ERROR TRAP BEEN SET UP?
                BNE         SE4B3           ; BRANCH IF ONE HAS
SE47D           PSHS        A               ; SAVE ACCA
                LDA         HRMODE          ; TEST THE HI-RES GRAPHICS MODE
                PULS        A               ; RESTORE ACCA
                BEQ         SE488           ; BRANCH IF HI-RES GRAPHICS NOT SET UP
                JSR         SETTEXT         ; PROGRAM THE VIDEO CONTROL REGISTERS FOR THE CURRENT MODE
SE488           CMPB        #38*2           ; HI-RES GRAPHICS ERROR
                BNE         SE49F           ; BRANCH IF NOT
                JSR         LB95C           ; SET UP PRINT PARAMETERS
                JSR         LB9AF           ; SEND A '?' TO CONSOLE OUT
                LEAX        BAS20ERR,PC     ; POINT TO ENHANCED BASIC'S ADDITIONAL ERROR CODES
SE496           JSR         LACA0           ; GET A CHARACTER FROM X AND SEND IT TO CONSOLE OUT
                JSR         LACA0           ; DO IT AGAIN
                JMP         LAC65           ; RE-ENTER THE MAIN STREAM OF CODE ($AC65)
SE49F           CMPB        #39*2           ; HI-RES TEXT MODE ERROR
                BNE         SE4B0           ; BRANCH IF NOT
                JSR         LB95C           ; SET UP THE PRINT PARAMETERS
                JSR         LB9AF           ; SEND A '?' TO CONSOLE OUT
                LEAX        SE4CE,PC        ; POINT TO ENHANCED BASIC'S ADDITIONAL ERROR CODES
                JMP         >SE496          ; GO PRINT THE ERROR CODE POINTED TO BY X
SE4B0           JMP         LAC49           ; JUMP TO THE ERROR SERVICING ROUTINE ($AC49)
SE4B3           STB         >H.ERROR        ; SAVE THE ERROR NUMBER
                PSHS        B               ; ALSO PUT IT ON THE STACK TEMPORARILY
                LDD         CURLIN          ; GET THE CURRENT LINE NUMBER
                STD         >H.ERLINE       ; SAVE THE LINE NUMBER WHERE THE ERROR OCCURRED
                PULS        B               ; GET BACK THE ERROR NUMBER
                CMPB        #3*2            ; WAS IT AN OUT OF DATA ERROR?
                BNE         SE4C7           ; BRANCH IF NOT
                LDD         BINVAL          ; THE INPUT POINTER IS SAVED IN BINVAL BY THE READ COMMAND
                STD         CHARAD          ; SAVE NEW ADDRESS FOR BASIC'S INPUT POINTER
SE4C7           TFR         X,D             ; SAVE THE ON ERROR DESTINATION LINE NUMBER IN ACCD
                LBRA        SE449           ; GO TRANSFER CONTROL TO THAT LINE NUMBER
; ENHANCED BASIC'S ERROR CODES
BAS20ERR        FCC         'HR'            ; 38 HIRES GRAHICS ERROR
SE4CE           FCC         'HP'            ; 39 HIRES TEXT ERROR
; LINE INTO 'NEW' FROM $AD3F
ALINK19         PSHS        B,A             ; SAVE THE CONTENTS OF ACCD
                CLRA                        ;
                CLRB                        ;
                STD         OLDPTR          ; RESET 'CONT' ADDRESS SO THAT YOU CAN'T CONTINUE
                STD         >H.ONBRK        ; RESET THE ON BRK ADDRESS TO ZERO: NON-INITIALIZED
                STD         >H.ONERR        ; RESET THE ON ERROR ADDRESS TO ZERO: NON-INITIALIZED
                STD         >H.ERLINE       ; RESET THE ERLIN LINE NUMBER TO ZERO: NO ERROR
                LDA         #$FF            ; INDICATES NO ERROR
                STA         >H.ERROR        ; RESET ERROR NUMBER TO NO ERROR
                PULS        A,B             ; RESTORE ACCD
                JMP         LAD43           ; JUMP TO THE END OF THE NEW COMMAND ($AD43)
; ERNO
ERNO            CLRA                        ;  CLEAR THE MS BYTE OF ACCD
                LDB         >H.ERROR        ; GET THE ERROR NUMBER
                CMPB        #$FF            ; IS IT A REAL ERROR
                BNE         SE4F4           ; BRANCH IF YES
                SEX         NOW             ; ACCD = $FFFF IF NOT A REAL ERROR
                BRA         SE4FA           ; CONVERT ACCD TO FLOATING POINT
SE4F4           CMPB        #$F1            ; CHECK FOR ERROR NUMBER $F1
                BNE         SE4F9           ; BRANCH IF NOT ERROR $F1
                COMB                        ;  CONVERT TO 7*2 (UNDEFINED LINE NUMBER)
SE4F9           ASRB                        ;  DIVIDE ERROR NUMBER BY 2
SE4FA           JMP         >GIVABF         ; CONVERT ACCD INTO A FLOATING POINT NUMBER
; ERLIN
ERLIN           LDD         >H.ERLINE       ; GET THE LINE NUMBER WHERE THE ERROR OCCURRED
                BRA         SE4FA           ; CONVERT IT INTO A FLOATING POINT NUMBER
; BASIC'S MAIN LOOP IN THE DIRECT MODE PATCH ENTERED FROM $AC73
ALINK21         JSR         SETTEXT         ; SET UP HI-RES TEXT MODE IF ENABLED
                JSR         LB95C           ; SET UP VARIOUS PRINT PARAMETERS
                ORCC        #$50            ; DISABLE IRQ, FIRQ
                LDA         #BLOCK6.4       ; GET/PUT BUFFER BLOCK
                STA         MMUREG          ; PUT IT INTO LOGICAL BLOCK 0
                LDD         #$FFFF          ; NO HGET/HPUT BUFFERS USED FLAG
                STD         0               ; SET THE HGET/HPUT BUFFER SPACE TO SHOW NO BUFFERS IN USE
                LDA         #BLOCK7.0       ; GET NORMAL LOGICAL BLOCK 0
                STA         MMUREG          ; PUT BACK INTO THE LOGICAL ADDRESS SPACE
                ANDCC       #$AF            ; ENABLE IRQ, FIRQ
                JMP         LAC76           ; RE-ENTER THE MAIN STREAM CODE ($AC76)
SE51E           TST         >H.ERRBRK       ; CHECK THE ERROR/BREAK FLAG
                BEQ         SE528           ; BRANCH IF ERROR BROUGHT US HERE
                LDD         >H.ONBRKS       ; GET THE ON BRK SOURCE LINE NUMBER IF BREAK VECTORED US HERE
                BRA         SE52B
SE528           LDD         >H.ONERRS       ; GET THE ON ERROR SOURCE LINE NUMBER
SE52B           STD         CURLIN          ; SAVE THE SOURCE LINE NUMBER AS THE CURRENT LINE NUMBER
                LDB         #7*2            ; UNDEFINED LINE NUMBER ERROR
                JMP         LAC49           ; JUMP TO THE ERROR SERVICING ROUTINE ($AC49)
; INPUT PATCH ENTERED FROM $B03D
ALINK17         LDD         >H.ONBRK        ; GET THE ON BRK SOURCE LINE NUMBER
                LBEQ        LAE11           ; BRANCH IF ON BRK NOT INITIALIZED ($AE11)
                PSHS        B,A             ; SAVE THE ON BRK SOURCE ADDRESS
                LDA         #$01            ; BREAK FLAG
                STA         >H.ERRBRK       ; SET THE ERROR/BREAK FLAG TO BREAK
                PULS        A,B             ; RESTORE SOURCE ADDRESS - INEFFICIENT, LDD H.ONBRK IS BETTER
                LBRA        SE449
; LPOKE
LPOKE           JSR         LB141           ; EVALUATE A NUMERIC EXPRESSION
                LBRN        RAMLINK         ; ROM HOOK
                BSR         SE58E           ; CONVERT FPA0 INTO AN EXTENDED ADDRESS
                CMPB        #BLOCK7.7       ; HIGHEST POSSIBLE BLOCK NUMBER
                LBHI        LB44A           ; ILLEGAL FUNCTION CALL ERROR IF BLOCK NUMBER TOO BIG
                PSHS        X,B             ; SAVE REGISTERS
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                TFR         B,A             ; SAVE THE BLOCK NUMBER IN ACCA
                PULS        B,X             ; RESTORE REGISTERS
                CMPB        #BLOCK7.7       ; COMPARE TO HIGHEST POSSIBLE BLOCK NUMBER
                LBHI        LB44A           ; ILLEGAL FUNCTION CALL ERROR
                ORCC        #$50            ; DISABLE INTERRUPTS
                LBSR        SELBLOK0        ; PUT THE INTERPRETED BLOCK INTO LOGICAL BLOCK 0
                STA         ,X              ; STORE THE VALUE BEING POKEd
                LBSR        SETMMU          ; RESTORE THE MMU REGISTERS TO WHAT BASIC EXPECTS
                ANDCC       #$AF            ; ENABLE THE IRQ AND FIRQ INTERRUPTS
                RTS
; LPEEK
LPEEK           BSR         SE58E           ; CONVERT FPA0 INTO AN EXTENDED ADDRESS
                LBRN        RAMLINK         ; RAM HOOK
                CMPB        #BLOCK7.7       ; COMPARE TO HIGHEST LEGAL BLOCK NUMBER
                LBHI        LB44A           ; ILLEGAL FUNCTINO CALL ERROR IF BLOCK NUMBER TOO BIG
                ORCC        #$50            ; DISABLE INTERRUPTS
                LBSR        SELBLOK0        ; GET THE INTERPRETED BLOCK NUMBER INTO CPU BLOCK 0
                LDB         ,X              ; GET THE VALUE BEING LPEEKed
                LBSR        SETMMU          ; RESTORE THE MMU REGISTERS TO WHAT BASIC EXPECTS
                ANDCC       #$AF            ; ENABLE THE IRQ AND FIRQ INTERRUPTS
                JMP         LB4F3           ; CONVERT THE VALUE IN ACCB INTO A FLOATING POINT NUMBER
; CONVERT FPA0 INTO A 'LONG' ADDRESS
; THE 'LONG' ADDRESS WIL BE RETURNED IN TWO PIECES: THE LOW ORDER 13 BITS
; WILL BE IN THE X REGISTER, AND THE HIGH ORDER 6 BITS, WHICH ARE THE
; BLOCK NUMBER, WILL BE IN ACCB
SE58E           PSHS        A
                LDA         FP0EXP          ; GET THE EXPONENT OF FPA0
                CMPA        #$93            ; EXPONENT OF 512K-1
                BLS         SE59A           ; BRANCH IF <= 512K-1
                LDB         #BLOCK7.7+1     ; MAKE IT ONE BLOCK BIGGER THAN THE BIGGEST ALLOWABLE
                BRA         SE5AF           ; EXIT ROUTINE
SE59A           JSR         LBCC8           ; DE-NORMALIZE FPA0
                LDD         FPA0+2          ; GET THE TWO LEAST SIGNIFICANT BITS OF FPA0
                ANDA        #$1F            ; MASK OFF THE 3 HIGH ORDER BITS
                TFR         D,X             ; SAVE THE 13 LOW ORDER BITS IN X REGISTER
                LDD         FPA0+1          ; GET THE SECOND AND THIRD BYTES IF FPA0
                ASRA                        ;
                RORB                        ;
                ASRA                        ;
                RORB                        ;
                ASRA                        ;
                RORB                        ;
                ASRA                        ;  NOT NECESSARY WITH MAXIMUM OF 512K RAM
                RORB                        ;
                ASRA                        ;  NOT NECESSARY WITH MAXIMUM OF 512K RAM
                RORB                        ;  SHIFT ACCD RIGHT 5 TIMES - THE BLOCK NUMBER IS IN ACCB
SE5AF           PULS        A,PC
; BUTTON
BUTTON          JSR         INTCNV          ; CONVERT FPA0 INTO AN INTEGER IN ACCB
                LBRN        RAMLINK         ; RAM HOOK
                CMPB        #$03            ; ONLY BUTTON NUMBERS 0-3 ALLOWD
                LBHI        LB44A           ; ILLEGAL FUNCTION ERROR
                TFR         B,A             ; SAVE BUTTON NUMBER IN ACCA
                CLRB                        ;
                COMB                        ;  NOW ACCB = $FF
                LDX         #PIA0           ; POINT TO THE KEYBOARD STROBE PIO
                STB         $02,X           ; SET THE COLUMN STROBE TO $FF - ALLOW ONLY BUTTONS TO BE CHECKED
                LDB         ,X              ; READ THE KEYBOARD ROWS
                CMPB        #$0F            ; THE BUTTONS ARE ON THE BOTTOM FOUR ROWS
                BEQ         SE5EA           ; BRANCH IF NO BUTTONS DOWN
                LEAX        SE5D5,PC        ; POINT TO THE BUTTON MASKING ROUTINES
                ASLA                        ;
                ASLA                        ;  MULT ACCA BY FOUR - FOUR BYTES/EACH MASKING ROUTINE
                JMP         A,X             ; JUMP TO THE APPROPRIATE MASKING ROUTINE
; MASK OFF ALL BUT BUTTON 1, RIGHT JOYSTICK
SE5D5           ANDB        #$01
                BRA         SE5E3
; MASK OFF ALL BUT BUTTON 1, LEFT JOYSTICK
                ANDB        #$04
                BRA         SE5E3
; MASK OFF ALL BUT BUTTON 2, RIGHT JOYSTICK
                ANDB        #$02
                BRA         SE5E3
; MASK OFF ALL BUT BUTTON 2, LEFT JOYSTICK
                ANDB        #$08
SE5E3           BNE         SE5EA           ; BRANCH IF MASKED BUTTON NOT DOWN
                LDD         #1              ; IF BUTTON DOWN, RETURN A VALUE OF ONE
                BRA         SE5EC
SE5EA           CLRA                        ;
                CLRB                        ;  RETURN A ZERO IF BUTTON IS NOT DOWN
SE5EC           JSR         GIVABF          ; CONVERT ACCD INTO A FLOATING POINT NUMBER IN FPA0
                RTS
; PALETTE
PALETTE         CMPA        #$F7            ; 'RGB' TOKEN?
                LBRN        RAMLINK         ; RAM HOOK
                BNE         SE600           ; NOT THE 'RGB' TOKEN, CHECK FOR 'CMP'
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
; RGB ENTRY POINT - SET THE PALETTE REGISTERS FOR DEFAULT RGB VALUES
SE5FA           LEAX        IM.RGB,PC       ; POINT TO THE DEFAULT RGB PALETTE COLORS
                BRA         SE634           ; PUT THE DATA POINTED TO BY X INTO THE PALETTE REGISTERS
SE600           CMPA        #$F6            ; 'CMP' TOKEN?
                BNE         SE60C           ; NO, GET A REGISTER NUMBER AND COLOR
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
; CMP ENTRY POINT - SET THE PALETTE REGISTERS FOR DEFAULT CMP VALUES

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
SE606           LEAX        IM.CMP,PC       ; POINT TO THE DEFAULT CMP PALETTE COLORS
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
SE606           LEAX        IM.RGB,PC       ; POINT TO THE DEFAULT CMP PALETTE COLORS
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                BRA         SE634           ; PUT THE DATA POINTED TO BY X INTO THE PALETTE REGISTERS
SE60C           JSR         >SE7B2          ; EVALUATE TWO EXPRESSIONS, NORMALLY A HORIZONTAL & VERTICAL COORDINATE
                LDX         #PALETREG       ; POINT TO THE GIME CHIP'S PALETTE REGISTERS
                LDY         #IM.PALET       ; POINT TO THE RAM IMAGE OF THE PALETTE REGISTERS
                LDA         BINVAL+1        ; GET THE NUMBER OF THE PALETTE REGISTER TO CHANGE
                CMPA        #16             ; 16 PALETTE REGISTERS MAXIMUM
                LBCC        LB44A           ; ILLEGAL FUNCTION CALLERROR IF PALETTE REGISTER > 15
                LEAX        A,X             ; POINT TO THE SELECTED PALETTE REGISTER
                LEAY        A,Y             ; POINT TO THE SELECTED PALETTE REGISTER RAM IMAGE
                LDB         VERBEG+1        ; GET THE NEW COLOR FOR THE PALETTE REGISTER
                CMPB        #63             ; MAXIMUM OF 64 COLORS (ZERO IS A LEGIT COLOR)
                BLS         SE62A           ; BRANCH IF LEGITIMATE COLOR SELECTED
                LDB         #63             ; USE COLOR 63 IF BAD COLOR NUMBER SELECTED
SE62A           ORCC        #$50            ; DISABLE INTERRUPTS
                SYNC                        ;  WAIT FOR AN INTERRUPT TO CHANGE PALETTE REGISTERS - THIS WILL
; PREVENT THE SCREEN FROM FLASHING WHEN THE CHANGE IS MADE.
                STB         ,X              ; SAVE THE NEW COLOR IN THE PALETTE REGISTER
                STB         ,Y              ; SAVE THE NEW COLOR IN THE PALETTE REGISTER RAM IMAGE
                ANDCC       #$AF            ; ENABLE IRQ, FIRQ INTERRUPTS
                RTS
SE634           PSHS        X               ; SAVE THE SOURCE REGISTER POINTER
                LDY         #IM.PALET       ; POINT TO THE PALETTE REGISTER RAM IMAGE
                BSR         SE648           ; COPY THE SOURCE PALETTE REGISTER TO THE RAM IMAGE
                PULS        X               ; RESTORE THE SOURCE REGISTER POINTER
                LDY         #PALETREG       ; POINT TO THE PALETTE REGISTERS
                ORCC        #$50            ; DIABLE INTERRUPTS
                SYNC                        ;  COPY IMMEDIATELY AFTER AN INTERRUPT TO PREVENT SPARKING
                BSR         SE648           ; COPY THE SOURCE REGISTER DATE INTO THE PALETTE REGISTERS
                RTS
SE648           LDB         #16-1           ; NUMBER OF BYTES TO COPY - BUG - SHOULD BE 16
SE64A           LDA         ,X+             ; GET A BYTE
                STA         ,Y+             ; MOVE IT
                DECB                        ;  BUMP COUNTER DOWN ONE
                BNE         SE64A           ; LOOP UNTIL DONE
                ANDCC       #$AF            ; ENABLE IRQ, FIRQ INTERRUPTS
                RTS

; PALETTE COLORS FOR A COMPOSITE MONITOR
IM.CMP          FCB         18,36,11,7,63,31,9,38,0,18,0,63,0,18,0,38

; PALETTE COLORS FOR AN RGB MONITOR
IM.RGB          FCB         18,54,9,36,63,27,45,38,0,18,0,63,0,18,0,38
RGB             BRA         SE5FA
CMP             BRA         SE606

; MASTER IMAGES USED TO PROGRAM THE CUSTOM CHIP'S PALETTE REGISTERS
; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
IM.PALET        FCB         18,36,11,7,63,31,9,38
                FCB         0,18,0,63,0,18,0,38
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
IM.PALET        FCB         $12,$36,$09,$24,$3f,$1b,$2d,$26
                FCB         $00,$12,$00,$3f,$00,$12,$00,$26
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

; HSCREEN
HSCREEN         CMPA        #$00            ; CHECK FOR END OF LINE
                LBRN        RAMLINK         ; RAM HOOK
                BNE         SE693           ; BRANCH IF NOT END OF LINE
                CLRB                        ;  IF END OF LINE, SET ARGUMENT TO ZERO
                BRA         SE69C           ; SET THE HSCREEN MODE
SE693           JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                CMPB        #$04            ; ONLY 4 HSCREEN MODES ALLOWED
                LBHI        LB44A           ; ILLEGAL FUNCTION CALL ERROR
SE69C           STB         HRMODE          ; SAVE THE HI-RES GRAPHICS MODE
                CMPB        #$00            ; CHECK FOR MODE 0
                BNE         SE6A5           ; BRANCH IF NOT HSCREEN 0
                JMP         SETTEXT         ; SETUP THE VIDEO MODE REGISTERS FOR COCO COMPATIBLE MODE
SE6A5           STB         HRMODE          ; SAVE THE HI-RES GRAPHICS MODE
                LDX         #SE6CB          ; POINT TO THE TABLE OF NUMBER OF BYTES/HORIZONTAL ROW
                SUBB        #$01            ; CONVERT THE HI-RES MODE FROM 1-4 TO 0-3
                LDA         B,X             ; GET THE NUMBER OF BYTES/HORIZONTAL ROW
                STA         HORBYT          ; AND SAVE IT
                CMPB        #$01            ; ONE OF THE FIRST TWO MODES?
                BGT         SE6B9           ; BRANCH IF NOT
                LDD         #160            ; HORIZONTAL CENTER OF 320 COORDINATE SCREEN
                BRA         SE6BC
SE6B9           LDD         #320            ; HORIZONTAL CENTER OF 640 COORDINATE SCREEN
SE6BC           STD         HORDEF          ; SAVE AS HORIZONTAL DEFAULT COORD
                LDD         #96             ; VERTICAL CENTER COORDINATE
                STD         VERDEF          ; SAVE AS VERTICAL DEFAULT
                LDB         >H.BCOLOR       ; GET THE BACKGROUND COLOR
                BSR         CLRHIRES        ; CLEAR THE HI-RES GRAPHICS SCREEN TO THE BACKGROUND COLOR
                JMP         SETGRAPH        ; GROGRAM THE VIDEO RESOLUTION MODE
; TABLE OF THE NUMBER OF BYTES PER HORIZONTAL ROW FOR EACH HSCREEN MODE
SE6CB           FCB         80,160,80,160
; HCLS
HCLS            BNE         SE6D6           ; BRANCH IF NOT END OF LINE
                LDB         >H.BCOLOR       ; GET THE BACKGROUND COLOR
                BRA         CLRHIRES        ; CLEAR THE SCREEN TO THE BACKGROUND COLOR
SE6D6           BSR         SE70E           ; EVALUATE AN EXPRESSION, SYNTAX CHECK FOR NOT > 16
; CLEAR THE HI-RES GRAPHICS SCREEN TO THE COLOR IN ACCB
CLRHIRES        TST         HRMODE          ; CHECK THE HI-RES MODE
                BEQ         SE6EF           ; HR' ERROR IF IN THE 32 COLUMN MODE
                BSR         PIXELFIL        ; FILL ACCB WITH THE SELECTED COLOR
                JSR         SELTASK1        ; SELECT TASK REGISTER 1 AS THE ACTIVE TASK REGISTER
; FILL MEMORY FROM HRESSCRN TO $A000 WITH ACCB; THIS IS THE HI-RES GRAPHICS SCREEN
                LDX         #HRESSCRN       ; POINT TO START OF HI-RES GRAPHICS SCREEN
SE6E4           STB         ,X+             ; 'CLEAR' A BYTE
                CMPX        #BASIC          ; CHECK FOR END OF THE HI-RES GRAPHICS SCREEN
                BNE         SE6E4           ; KEEP 'CLEARING' UNTIL DONE
                JSR         SELTASK0        ; SET TASK REGISTER 0 AS THE ACTIVE TASK REGISTER
                RTS
SE6EF           LDB         #38*2           ; 'HR' ERROR
                JMP         LAC46           ; JUMP TO THE ERROR HANDLER
; HCOLOR
HCOLOR          CMPA        #',             ; CHECK FOR COMMA, FIRST ARGUMENT NOT GIVEN
                LBRN        RAMLINK         ; RAM HOOK
                BEQ         SE705           ; BRANCH IF FIRST ARGUMENT NOT GIVEN
                BSR         SE70E           ; EVALUATE EXPRESSION, SYNTAX CHECK FOR EXPRESSION > 16
                STB         >H.FCOLOR       ; SAVE THE NEW FORGROUND COLOR
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                BEQ         SE70D           ; BRANCH IF END OF LINE, NO BACKGROUND COLOR GIVEN
SE705           JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                BSR         SE70E           ; EVALUATE EXPRESSION, SYNTAX CHECK FOR EXPRESSION > 16
                STB         >H.BCOLOR       ; SAVE THE NEW BACKGROUND COLOR
SE70D           RTS
SE70E           JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
SE711           CMPB        #16             ; MAXIMUM OF 16 DIFFERENT COLORS
                LBCC        LB44A           ; ILLEGAL FUNCTION CALL ERROR
                RTS
SE718           JSR         >SE731          ; SET THE WORKING COLOR AND ALL PIXEL BYTES TO DEFAULT VALUES
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                BEQ         SE72F           ; BRANCH IF END OF LINE
                CMPA        #') SYNTAX CHECK FOR ')'
                BEQ         SE72F           ; EXIT IF ')'
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                CMPA        #',             ; SYNTAX CHECK FOR A COMMA
                BEQ         SE72F           ; USE DEFAULT COLORS IF TWO COMMAS
                JSR         >SE70E          ; EVALUATE COLOR ARGUMENT
                BSR         SE73B           ; SET THE WORKING AND ALL COLOR BYTES TO THE COLOR ARGUMENT
SE72F           JMP         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER AND RETURN
SE731           LDB         >H.FCOLOR       ; GET THE FOREGOUND COLOR
                TST         SETFLG          ; TEST THE HSET/HRESET FLAG
                BNE         SE73B           ; BRANCH IF HSET
                LDB         >H.BCOLOR       ; GET THE BACKGROUND COLOR IF HRESET
SE73B           STB         WCOLOR          ; SAVE THE NEW WORKING COLOR
                BSR         PIXELFIL        ; FILL ALL PIXELS IN A BYTE WITH THE WORKING COLOR
                STB         ALLCOL          ; SAVE THE FILLED WITH WORKING COLOR BYTE
                RTS
; FILL ACCB WITH PIXELS OF THE COLOR CONTAINED IN ACCB
PIXELFIL        PSHS        X
                LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                SUBA        #$01            ; CONVERT 1-4 TO 0-3
                LDX         #SE759          ; POINT TO THE TABLE OF PIXEL MASKS
                ANDB        A,X             ; KEEP ONLY ONE PIXEL'S WORTH OF COLOR INFORMATION
                LDA         HRMODE          ; BOTH OF THESE INSTRUCTIONS
                SUBA        #$01            ; ARE SUPERFLUOUS
                LDX         #SE75D          ; POINT TO THE TABLE OF MULTIPLIERS
                LDA         A,X             ; GET THE APPROPRIATE MULTIPLIER
                MUL                         ;  NOW THE COLOR INFORMATION IS IN EVERY PIXEL IN THE BYTE
                PULS        X,PC
; PIXEL MASKS FOR THE HI-RES GRAPHICS MODES
SE759           FCB         $03,$0F,$01,$03
; MULTIPLIERS TO SPREAD HI-RES PIXELS THROUGH AN ENTIRE BYTE
SE75D           FCB         $55,$11,$FF,$55
; HSET
HSET            LDA         #$01            ; HSET FLAG
                BRA         SE76A
; HRESET
HRESET          CLRA                        ;  HRESET FLAG
                LBRN        RAMLINK
SE76A           TST         HRMODE          ; IS THE HI-RES GRAPHICS MODE ENABLED?
                BEQ         SE6EF           ; HR' ERROR IF HI-RES MODE NOT ENABLED
                STA         SETFLG          ; SAVE THE HSET/HRESET FLAG
                JSR         LB26A           ; SYNTAX CHECK FOR '('
                JSR         >SE7AA          ; EVALUATE TWO EXPRESSIONS
                TST         SETFLG          ; CHECK THE HSER/HRESET FLAG
                BNE         SE77F           ; BRANCH IF HSET
                JSR         >SE731          ; SET THE WORKING COLOR AND ALL PIXEL BYTE
                BRA         SE782
SE77F           JSR         >SE718          ; GET THE HSET COLOR
SE782           JSR         LB267           ; SYNTAX CHECK FOR ')'
                JSR         >HCALPOS        ; LOAD X WITH PIXEL BYTE ADDRESS; ACCA WITH PIXEL MASK
SE788           JSR         >SELTASK1       ; MAKE TASK REGISTER 1 THE ACTIVE TASK REGISTER
                JSR         >SE792          ; SET OR RESET A PIXEL
                JSR         >SELTASK0       ; RESET TASK REGISTER 0 TO BE THE ACTIVE TASK REGISTER
                RTS
; HSET/HRESET A PIXEL; ENTER W/X POINTING TO THE BYTE CONTAINING THE PIXEL AND
; ACCA POINTING TO THE MASK FOR THE PROPER PIXEL
SE792           LDB         ,X              ; GET THE BYTE WHICH CONTAINS THE PIXEL
                PSHS        B               ; AND SAVE IT ON THE STACK
                TFR         A,B             ; COPY THE MASK TO ACCB
                COMA                        ;  INVERT THE MASK
                ANDA        ,X              ; ERASE OLD PIXEL DATA
                ANDB        ALLCOL          ; FORCE THE PIXEL MASK TO BE THE CORRECT COLOR
                PSHS        B               ; AND SAVE THE 'COLORED' DATA ON THE STACK
                ORA         ,S+             ; REPLACE THE 'ERASED' PIXEL WITH THE NEW COLOR DATA
                STA         ,X              ; AND SAVE IT IN THE SCREEN MEMORY
                SUBA        ,S+             ; ACCA=0 IF OLD AND NEW PIXELS WERE IDENTICAL
                ORA         CHGFLG          ; SET CHGFLG <> 0 IF THE PIXEL WAS CHANGED
                STA         CHGFLG          ; SAVE THE 'CHANGED' STATUS
                RTS
SE7AA           JSR         >SE7B2          ; EVALUATE TWO EXPRESSIONS
SE7AD           LDU         #HORBEG         ; POINT U TO EVALUATED COORDINATES' STORAGE LOCATIONS
; THE 'NORMALIZATION' ($9320) ROUTINE FROM EXTENDED BASIC WENT HERE - IT IS NOT NEEDED
; IN ENHANCED BASIC SO IT WAS REPLACED WITH AN RTS.
SE7B0           RTS
                RTS         WASTED          ; BYTE
; EVALUATE TWO EXPRESSIONS - NORMALLY A HORIZONTAL AND VERTICAL COORDINATE
; PERFORM COORDINATE SYNTAX RANGE CHECKS ON THE EXPRESSIONS
SE7B2           JSR         LB734           ; EVALUATE TWO EXPRESSIONS; RETURN 1ST VALUE IN BINVAL, SECOND IN ACCB
                LDY         #HORBEG         ; POINT TO THE COORDINATE STORAGE VARIABLES
SE7B9           CMPB        #192            ; CHECK FOR MAXIMUM VERTICAL COORDINATE
                BCS         SE7BF           ; BRANCH IF WITHIN RANGE
                LDB         #192-1          ; FORCE TO MAXIMUM VALUE IF OUT OF RANGE
SE7BF           CLRA                        ;  CLEAR THE MOST SIGNIFICANT BYTE OF ACCD
                STD         $02,Y           ; SAVE THE VERTICAL COORDINATE
                LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                CMPA        #$02            ; IS MAXIMUM PIXEL WIDTH=320?
                BGT         SE7CD           ; NO
                LDD         #320-1          ; LOAD ACCD WITH MAXIMUM HORIZONTAL COORDINATE FORE 320 PIXEL WIDE
                BRA         SE7D0           ; DO THE HORIZONTAL RANGE CHECK
SE7CD           LDD         #640-1          ; LOAD ACCD WITH MAXIMUM HORIZONTAL COORDINATE FORE 640 PIXEL WIDE
SE7D0           CMPD        BINVAL          ; IS THE HORIZONTAL COORDINATE > MAXIMUM VALUE?
                BCS         SE7D7           ; YES, USE THE MAXIMUM HORIZONTAL COORDINATE
                LDD         BINVAL          ; GET THE NEW HORIZONTAL COORDINATE
SE7D7           STD         ,Y              ; SAVE THE HORIZONTAL COORDINATE
                RTS
; THIS ROUTINE WILL CONVERT THE X,Y COORDINATES OF A PIXEL INTO THE SCREEN ADDRESS (X REG) AND
; PIXEL OFFSET (ACCA) OF THE BYTE ON THE SCREEN CONTAINING THE PIXEL.
HCALPOS         BSR         SE7E6           ; POINT U TO THE HCALPOS SUBROUTINE FOR THE CURRENT HRMODE
                JMP         ,U              ; EXECUTE THE HCALPOS SUBROUTINE
; CALTABLE
CALTABLE        FDB         G2BITBIX,G4BITPIX,G1BITPIX
                FDB         G2BITBIX
; POINT U TO THE PROPER CALPOS SUBROUTINE
SE7E6           LDU         #CALTABLE       ; POINT U TO THE CALPOS ADDRESS TABLE
                LDA         HRMODE          ; GET THE HI-RS GRAPHICS MODE
                SUBA        #$01            ; (DECA WOULD DO) CONVERT FROM 1-4 TO 0-3
                ASLA                        ;  X2 BYTES PER ADDRESS
                LDU         A,U             ; GET THE APPROPRIATE CALPOS ADDRESS FROM THE TABLE
                RTS
; TABLE OF 1 BIT PIXEL MASKS
PIX1MASK        FCB         $80,$40,$20,$10,$08,$04
                FCB         $02,$01
; TABLE OF 2 BIT PIXEL MASKS
PIX2MASK        FCB         $C0,$30,$0C,$03
; TABLE OF 4 BIT PIXEL MASKS
PIX4MASK        FCB         $F0,$0F

; CONVERT HORIZONTAL, VERTICAL COORDINATES INTO THE ADDRESS (X) FOR THE BYTE WHICH CONTAINS THE DESIRED
; PIXEL AND A MASK (ACCA) WHICH HAS ONLY THOSE BITS WHICH CORRESPOND TO THE DESIRED PIXEL
G1BITPIX        PSHS        U,B             ; SAVE REGISTERS
                LDB         HORBYT          ; GET THE NUMBER OF BYTES PER HORIZONTAL ROW
                LDA         VERBEG+1        ; GET THE VERTICAL COORDINATE
                MUL                         ;  NOW ACCD CONTAINS THE ROW OFFSET IN BYTES FROM THE TOP OF SCREEN
                ADDD        #HRESSCRN       ; ADD THE ROW OFFSET TO THE START OF THE SCREEN
                TFR         D,X             ; X CONTAINS THE ADDRESS OF THE START OF THE ROW CONTAINING A PIXEL
                LDD         HORBEG          ; GET THE HORIZONTAL COORDINATE
                LSRA                        ;
                RORB                        ;
                LSRA                        ;
                RORB                        ;
                LSRA                        ;  DIVIDE HORIZONTAL COORDINATE BY EIGHT - THERE ARE 8 PIXELS PER BYTE
                RORB                        ;  ACCD CONTAINS THE COLUMN OFFSET TO THE PIXEL IN BYTES
                LEAX        D,X             ; ADD THE COLUMN OFFSET - X POINTS TO THE BYTE CONTAINING THE PIXEL
                LDA         HORBEG+1        ; GET THE LEAST SIGNIFICANT BYTE OF THE HORIZONTAL COORDINATE
                ANDA        #$07            ; KEEP BITS 0-2 WHICH ARE THE PIXEL POSITION IN THE BYTE
                LDU         #PIX1MASK       ; POINT TO THE TABLE OF TWO COLOR PIXEL MASKS
                LDA         A,U             ; GET THE CORRECT PIXEL MASK
                PULS        B,U,PC          ; RESTORE THE REGISTERS
G2BITBIX        PSHS        U,B             ; SAVE REGISTERS
                LDB         HORBYT          ; GET THE NUMBER OF BYTES/ROW
                LDA         VERBEG+1        ; GET THE VERTICAL COORDINATE
                MUL                         ;  NOW ACCD CONTAINS THE ROW OFFSET IN BYTES FROM THE TOP OF SCREEN
                ADDD        #HRESSCRN       ; ADD THE ROW OFFSET TO THE START OF THE SCREEN
                TFR         D,X             ; X CONTAINS THE ADDRESS OF THE START OF THE ROW CONTAINING A PIXEL
                LDD         HORBEG          ; GET THE HORIZONTAL COORDINATE
                LSRA                        ;
                RORB                        ;
                LSRA                        ;  DIVIDE HORIZONTAL COORDINATE BY FOUR - THERE ARE 4 PIXELS PER BYTE
                RORB                        ;  ACCD CONTAINS THE COLUMN OFFSET TO THE PIXEL IN BYTES
                LEAX        D,X             ; ADD THE COLUMN OFFSET - X POINTS TO THE BYTE CONTAINING THE PIXEL
                LDA         HORBEG+1        ; GET THE LEAST SIGNIFICANT BYTE OF THE HORIZONTAL COORDINATE
                ANDA        #$03            ; KEEP BITS 0,1 WHICH ARE THE PIXEL POSITION IN THE BYTE
                LDU         #PIX2MASK       ; POINT TO THE TABLE OF FOUR COLOR PIXEL MASKS
                LDA         A,U             ; GET THE CORRECT PIXEL MASK
                PULS        B,U,PC          ; RESTORE THE REGISTERS
G4BITPIX        PSHS        U,B             ; SAVE REGISTERS
                LDB         HORBYT          ; GET THE NUMBER OF BYTES/ROW
                LDA         VERBEG+1        ; GET THE VERTICAL COORDINATE
                MUL                         ;  NOW ACCD CONTAINS THE ROW OFFSET IN BYTES FROM THE TOP OF SCREEN
                ADDD        #HRESSCRN       ; ADD THE ROW OFFSET TO THE START OF THE SCREEN
                TFR         D,X             ; X CONTAINS THE ADDRESS OF THE START OF THE ROW CONTAINING A PIXEL
                LDD         HORBEG          ; GET THE HORIZONTAL COORDINATE
                LSRA                        ;  DIVIDE HORIZONTAL COORDINATE BY TWO - THERE ARE 2 PIXELS PER BYTE
                RORB                        ;  ACCD CONTAINS THE COLUMN OFFSET TO THE PIXEL IN BYTES
                LEAX        D,X             ; ADD THE COLUMN OFFSET - X POINTS TO THE BYTE CONTAINING THE PIXEL
                LDA         HORBEG+1        ; GET THE LEAST SIGNIFICANT BYTE OF THE HORIZONTAL COORDINATE
                ANDA        #$01            ; KEEP BITS 0 WHICH IS THE PIXEL POSITION IN THE BYTE
                LDU         #PIX4MASK       ; POINT TO THE TABLE OF 16 COLOR PIXEL MASKS
                LDA         A,U             ; GET THE CORRECT PIXEL MASK
                PULS        B,U,PC          ; RESTORE THE REGISTERS
; HPOINT
HPOINT          TST         HRMODE          ; CHECK FOR HI-RES GRAPHICS MODE
                LBEQ        SE6EF           ; 'HR' ERROR IF NOT GRAPHICS
                JSR         LB26A           ; SYNTAX CHECK FOR '('
                JSR         >SE7AA          ; EVALUATE TWO EXPRESSIONS (X,Y COORDS)
                JSR         LB267           ; SYNTAX CHECK FOR ')'
                JSR         SELTASK1        ; SELECT TASK REGSTER 1
                JSR         HCALPOS         ; POINT X TO PIXEL, ACCA CONTAINS MASK
                TFR         A,B             ; PUT MASK IN ACCB
                ANDB        ,X              ; MASK OFF ALL BUT DESIRED PIXEL
SE875           LSRA                        ;  SHIFT MASK TO THE RIGHT
                BCS         SE87B           ; STOP SHIFTING IF DATA IS RIGHT JUSTIFIED
                LSRB                        ;  SHIFT PIXEL TO THE RIGHT
                BRA         SE875           ; KEEP SHIFTING UNTIL DATA IS RIGHT JUSTIFIED
SE87B           JSR         LB4F3           ; CONVERT ACCB INTO A FLOATING POINT NUMBER
                JSR         SELTASK0        ; SELECT TASK REGISTER 0
                RTS
; HLINE
HLINE           TST         HRMODE          ; CHECK HI-RES GRAPHICS MODE
                LBEQ        SE6EF           ; 'HR' ERROR IF NOT GRAPHICS
                LBRN        RAMLINK         ; RAM HOOK
                CMPA        #'( CHECK FOR '('
                BEQ         SE899           ; GO LOOK FOR START AND END POINTS
                CMPA        #$AC            ; CHECK FOR MINUS SIGN TOKEN
                BEQ         SE899           ; BRANCH IF NO STARTING POINTS GIVEN
                LDB         #'@ CHECK FOR '@' ; SIGN
                JSR         LB26F           ; GO DO A SYNTAX CHECK
SE899           JSR         >SE9E1          ; GET STARTING AND ENDING COORDINATES
                LDX         HOREND          ; GET ENDING HORIZONTAL COORDINATE
                STX         HORDEF          ; PUT IN LAST USED HORIZONTAL END POINT
                LDX         VEREND          ; GET ENDING VERTICAL COORDINATE
                STX         VERDEF          ; PUT IN LAST USED VERTICAL END POINT
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                CMPA        #$BE            ; PRESET TOKEN?
                BEQ         SE8B4           ; BRANCK IF YES
                CMPA        #$BD            ; PSET TOKEN?
                LBNE        LB277           ; 'SYNTAX' ERROR IF NOT PSET OR PRESET
                LDB         #$01            ; PSET FLAG
                FCB         SKP1LD          ; OP CODE FOR LDA #; EFFECTIVELY SKIP NEXT INSTRUCTION
SE8B4           CLRB                        ;  PRESET FLAG
                PSHS        B               ; SAVE PSET/PRESET FLAG
                JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC'S INPUT LINE
                JSR         >SEA0D          ; NORMALIZE START/END COORDS
                PULS        B               ; GET PSET/PRESET FLAG
                STB         SETFLG          ; SAVE IT
                JSR         >SE731          ; SET ACTIVE COLOR BYTE
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                LBEQ        SE94E           ; BRANCH IF NO BOX TO BE DRAWN
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                LDB         #'B             ; DRAW A BOX?
                JSR         LB26F           ; GO DO A SYNTAX CHECK FOR A 'B'
                BNE         SE8EB           ; FOUND A 'B' AND SOMETHING FOLLOWS
                BSR         SE906           ; DRAW A HORIZONTAL LINE
                BSR         SE931           ; DRAW A VERTICAL LINE
                LDX         HORBEG          ; GET HORIZONTAL START COORD
                PSHS        X               ; SAVE IT ON THE STACK
                LDX         HOREND          ; GET HORIZONTAL END COORDINATE
                STX         HORBEG          ; PUT IN HORIZONTAL START COORDINATE
                BSR         SE931           ; DRAW A VERTICAL LINE
                PULS        X               ; GET THE PREVIOUS HORIZONTAL START COORDINATE
                STX         HORBEG          ; RESTORE IT
                LDX         VEREND          ; GET VERTICAL END COORDINATE
                STX         VERBEG          ; PUT INTO START COORD
                BRA         SE906           ; DRAW A HORIZONTAL LINE
SE8EB           LDB         #'F             ; CHECK FOR FILL OPTION
                JSR         LB26F           ; GO DO A SYNTAX CHECK FOR AN 'F'
                BRA         SE8F6           ; GO 'FILL' THE BOX
SE8F2           LEAX        $-01,X          ; MOVE VERTICAL COORD UP ONE
SE8F4           STX         VERBEG          ; SAVE THE NEW VERTICAL START COORDINATE
; DRAW A SERIES OF HORIZONTAL LINES FROM VERTICAL START TO VERTICAL END
SE8F6           JSR         >SE906          ; DRAW A HORIZONTAL LINE
                LDX         VERBEG          ; GET START VERTICAL COORD
                CMPX        VEREND          ; COMPARE TO END VERTICAL COORD
                BEQ         SE905           ; RETURN IF EQUAL
                BCC         SE8F2           ; BRANCH IF START HORIZONTAL > END HORIZONTAL
                LEAX        $01,X           ; MOVE HORIZONTAL COORD DOWN ONE
                BRA         SE8F4           ; KEEP DRAWING LINES
SE905           RTS
; DRAW A HORIZONTAL LINE FROM HOREND TO HORBEG AT VERTICAL COORD VERBEG; COLOR IN ALLCOL
SE906           LDX         HORBEG          ; GET STARTING COORDINATES
                PSHS        X               ; SAVE 'EM
                JSR         >SE9DB          ; GET ABSOLUTE VALUE OF HOREND-HORBEG (HORIZONTAL COORD)
                BCC         SE913           ; BRANCH IF END > START
                LDX         HOREND          ; GET END COORD
                STX         HORBEG          ; MAKE IT THE START COORD
SE913           TFR         D,Y             ; SAVE DIFFERENCE IN Y
                LEAY        $01,Y           ; ADD ONE TO DIFFERENCE - TURN ON STARTING AND ENDING COORDS
                JSR         HCALPOS         ; GET ABSOLUTE SCREEN ADDRESS IN X AND PIXEL MASK IN ACCA
                PULS        U               ; GET START COORDS
                STU         HORBEG          ; RESTORE THEM
                LBSR        SEA16           ; POINT U TO ROUTINE TO MOVE PIXEL POINTERS TO RIGHT
SE921           STA         VD7             ; SAVEL PIXEL MASK
                JSR         >SE788          ; TURN ON PIXEL
                LDA         VD7             ; GET OLD PIXEL MASK
                JSR         ,U              ; MOVE TO NEXT ONE TO RIGHT
                LEAY        $-01,Y          ; DEC COUNTER
                BNE         SE921           ; LOOP IF NOT DONE
                RTS
SE92F           PULS        A,B             ; CLEAN UP STACK
; DRAW A VERTICAL LINE FROM VEREND TO VERBEG AT HORIZONTAL COORD HORBEG
SE931           LDD         VERBEG          ; GET END VERTICAL COORDS
                PSHS        B,A             ; SAVE 'EM
                JSR         >SE9CD          ; CALCULATE ABSOLUTE VALUE OF VEREND-VERBEG
                BCC         SE93E           ; BRANCH IF END COORD > START COORD
                LDX         VEREND          ; GET VERTICAL END COORDINATE
                STX         VERBEG          ; MAKE IT THE START COORD IF END COORD WAS RIGHT OF START
SE93E           TFR         D,Y             ; LENGTH OF LINE TO Y
                LEAY        $01,Y           ; SET BOTH START AND END COORDS
                JSR         HCALPOS         ; GET ABSOLUTE SCREEN ADDRESS IN X AND PIXEL MASK IN ACCA
                PULS        U               ; GET END COORDS
                STU         VERBEG          ; RESTORE THEM
                LBSR        SEA21           ; POINT U TO ROUTINE TO MOVE DOWN ONE ROW
                BRA         SE921           ; DRAW A VERTICAL LINE
; DRAW A LINE FROM (HORBEG, VERBEG) TO (HOREND, VEREND)
SE94E           LDY         #SE9B8          ; POINT Y TO INCREMENT VERBEG (VERTICAL START COORD)
                JSR         >SE9CD          ; CALCULATE VERTICAL DIFFERENCE (VEREND-VERBEG)
                BEQ         SE906           ; DRAW A HORIZONTAL LINE IF DELTA V=0
                BCC         SE95D           ; BRANCH IF VERTICAL END COORD > VERTICAL START COORD
                LDY         #SE9C6          ; POINT Y TO DECR VERTICAL COORD
SE95D           PSHS        B,A             ; SAVE DELTA V
                LDU         #SE9B1          ; POINT U TO INCR HORIZONTAL COORD
                JSR         >SE9DB          ; CALCULATE HORIZONTAL DIFFERENCE (HOREND-HORBEG)
                BEQ         SE92F           ; DRAW A VERTICAL LINE IF DELTA H=0
                BCC         SE96C           ; BRANCH IF HORIZONTAL END COORD > HORIZONTAL START COORD
                LDU         #SE9BF          ; POINT U TO DECR HORIZONTAL COORD
SE96C           CMPD        ,S              ; COMPARE DELTA H TO DELTA V
                PULS        X               ; PUT DELTA V IN X
                BCC         SE977           ; BRANCH IF DELTA H > DELTA V
                EXG         U,Y             ; SWAP CHANGE HORIZONTAL AND CHANGE VERTICAL ADDRESS
                EXG         D,X             ; EXCHANGE DELTA HORIZONTAL AND DELTA VERTICAL
SE977           PSHS        U,B,A           ; SAVE THE LARGER OF DELTA V, DELTA H AND INCR/DECR ADDRESS
                PSHS        B,A             ; SAVE THE LARGER OF DELTA V, DELTA H
                LSRA                        ;
                RORB                        ;  DIVIDE BY 2, SHIFT ACCD RIGHT ONE BIT
                BCS         SE988           ; BRANCH IF ODD NUMBER
                CMPU        #SE9B8+1        ; SEE IF INCR OR DECR
                BCS         SE988           ; BRANCH IF INCR
                SUBD        #1              ; SUBTRACT ONE IF DECR
SE988           PSHS        X,B,A           ; SAVE SMALLEST DELTA (X) AND INITIAL MINOR COORDINATE
; INCREMENT COUNTER WHICH IS 1/2 OF LARGEST DELTA
                JSR         >SE7E6          ; POINT U TO PROPER COORDINATE TO SCREEN CONVERSION ROUTINE
; DRAW THE LINE HERE - AT THIS POINT THE STACK HAS THE DRAW DATA ON IT
; 0 1,S=MINOR COORDINATE INCREMENT COUNTER
; 2 3,S=ABSOLUTE VALUE OF THE SMALLEST DELTA COORDINATE
; 4 5,S=ABSOLUTE VALUE OF THE LARGEST DELTA COORDINATE
; 6 7,S=LARGEST COORDINATE COUNTER (HOW MANY TIMES THROUGH THE DRAW LOOP)
; INITIALLY SET TO ABSOLUTE VALUE OF LARGEST DELTA
; 8 9,S=ADDRESS OF THE ROUTINE WHICH WILL INCREMENT OR DECREMENT THE LARGEST DELTA COORDINATE
SE98D           JSR         ,U              ; CONVERT (X,Y) COORDINATES TO ABSOLUTE SCREEN ADDRESS
                JSR         >SE788          ; TURN ON A PIXEL
                LDX         $06,S           ; GET DISTANCE COUNTER
                BEQ         SE9AD           ; BRANCH IF LINE COMPLETELY DRAWN
                LEAX        $-01,X          ; DECR ONE
                STX         $06,S           ; SAVE IT
                JSR         [$08,S]         ; INCR/DECR COORDINATE WHICH HAS THE SMALLEST DELTA
                LDD         ,S              ; GET THE MINOR COORDINATE INCREMENT COUNTER
                ADDD        $02,S           ; ADD THE SMALLEST DIFFERENCE
                STD         ,S              ; SAVE NEW MINOR COORDINATE INCREMENT COUNTER
                SUBD        $04,S           ; SUBTACT OUT THE LARGEST DIFFERENCE
                BCS         SE98D           ; BRANCH IF RESULT NOT > LARGEST DIFFERENCE
                STD         ,S              ; IF >=, THEN STORE NEW MINOR COORDINATE INCREMENT
                JSR         ,Y              ; INCR/DECR COORDINATE WHICH HAS THE SMALLEST DELTA
                BRA         SE98D           ; KEEP GOING
SE9AD           PULS        X               ; CLEAN UP STACK
                PULS        A,B,X,Y,U,PC    ; CLEAN UP STACK AND RETURN
; THESE ROUTINES ARE USED TO INCREMENT OR DECREMENT THE HORIZONTAL AND VERTICAL
; COORDINATES. THEY NEED TO BE KEPT IN THIS ORDER (INCR, INCR, DECR, DECR)
SE9B1           LDX         HORBEG          ; GET HORIZONTAL COORD
                LEAX        $01,X           ; ADD ONE
                STX         HORBEG          ; SAVE NEW HORIZONTAL COORD
                RTS
SE9B8           LDX         VERBEG          ; GET VERTICAL COORD
                LEAX        $01,X           ; ADD ONE
                STX         VERBEG          ; SAVE NEW VERTICAL COORD
                RTS
SE9BF           LDX         HORBEG          ; GET HORIZONTAL COORD
                LEAX        $-01,X          ; SUBTRACT ONE
                STX         HORBEG          ; SAVE NEW HORIZONTAL COORD
                RTS
SE9C6           LDX         VERBEG          ; GET VERTICAL COORD
                LEAX        $-01,X          ; SUBTRACT ONE
                STX         VERBEG          ; SAVE NEW VERTICAL COORD
SE9CC           RTS
SE9CD           LDD         VEREND          ; GET VERTICAL ENDING ADDRESS
                SUBD        VERBEG          ; SUBTRACT OUT VERTICAL BEGINNING ADDRESS
SE9D1           BCC         SE9CC           ; RETURN IF END >= START
                PSHS        CC              ; SAVE STATUS (WHICH COORDINATE IS GREATER)
; THE NEXT THREE INSTRUCTIONS WILL NEGATE ACCD
                NEGA                        ;
                NEGB                        ;
                SBCA        #$00            ; NEGATE ACCB
                PULS        CC,PC           ; RESTORE STATUS AND RETURN
SE9DB           LDD         HOREND          ; GET HORIZONTAL END COORD
                SUBD        HORBEG          ; SUBTRACT OUT HORIZONTAL START COORD
                BRA         SE9D1           ; GET ABSOLUTE VALUE
; EVALUATE TWO SETS OF COORDINATES SEPERATED BY A MINUS
; SIGN. PUT 1ST SET OF COORDINATES AT (HORBEG,VERBEG), SECOND
; SET AT (HOREND,VEREND). IF NOTHING BEFORE MINUS SIGN, PUT
; (HORDEF,VERDEF) DEFAULTS AT (HORBEG,VERBEG).
SE9E1           LDX         HORDEF          ; GET THE LAST HORIZONTAL END POINT
                STX         HORBEG          ; PUT AS START POINT
                LDX         VERDEF          ; GET THE LAST VERTICAL END POINT
                STX         VERBEG          ; PUT AS VERTICAL START POINT
                CMPA        #$AC            ; CHECK FOR MINUS SIGN (-) TOKEN
                BEQ         SE9F0           ; BRANCH IF NO STARTING COORDINATES GIVEN
                JSR         >SEA04          ; GO GET THE STARTING COORDINATES
SE9F0           LDB         #$AC            ; TOKEN FOR THE MINUS SIGN (-)
                JSR         LB26F           ; DO A SYNTAX CHECK FOR A MINUS SIGN
                JSR         LB26A           ; SYNTAX CHECK FOR A '('
                JSR         LB734           ; EVALUATE 2 EXPRESSIONS
                LDY         #HOREND         ; TEMP STORAGE LOCS FOR END COORDS OF LINE COMMAND
                JSR         >SE7B9          ; GET END POINT COORDINATES
                BRA         SEA0A           ; SYNTAX CHECK FOR A ')'
SEA04           JSR         LB26A           ; SYNTAX CHECK FOR A '('
                JSR         >SE7B2          ; EVALUATE HORIZONTAL & VERTICAL COORDINATES WITH RANGE CHECK
SEA0A           JMP         LB267           ; SYNTAX CHECK FOR ')' AND RETURN
SEA0D           JSR         >SE7AD          ; POINT U TO HORBEG; USELESS GIVEN THE FOLLOWINF INSTRUCTION
                LDU         #HOREND         ; POINT U TO HOREND
                JMP         >SE7B0          ; JUMP TO AN RTS; ONCE WAS A JUMP TO NORMALIZATION ROUTINE
; POINT U TO ROUTINE WHICH WILL MOVE PIXEL ONE TO RIGHT
SEA16           LDU         #SEA25          ; POINT TO JUMP TABLE
                LDB         HRMODE          ; GET HI-RES GRAPHICS MODE VALUE
                SUBB        #$01            ; ADJUST OUT MODE 0 (WHY NOT DECB)
                ASLB                        ;  TWO BYTES PER ENTRY
                LDU         B,U             ; GET JUMP ADDRESS
                RTS
SEA21           LDU         #SEA45          ; POINT U TO ROUTINE TO MOVE ABSOLUTE POS DOWN ONE ROW
                RTS
; JUMP TABLE OF ADDRESSES OF ROUTINES WHICH WILL MOVE THE
; ABSOLUTE SCREEN ADDRESS POINTER ONE PIXEL TO THE RIGHT
SEA25           FDB         SEA34           ; HSCREEN 1
                FDB         SEA3D           ; HSCREEN 2
                FDB         SEA2D           ; HSCREEN 3
                FDB         SEA34           ; HSCREEN 4
; ENTER WITH ABSOLUTE SCREEN POSITION IN X, PIXEL MASK
; IN ACCA - ADJUST X AND ACCA TO THE NEXT PIXEL TO THE RIGHT FOR HSCREEN 3
SEA2D           LSRA                        ;  SHIFT ONE BIT TO THE RIGHT
                BCC         SEA33           ; BRANCH IF SAME BYTE
                RORA                        ;  SET BIT 7 OF ACCA IF JUST MOVED TO NEXT BYTE
                LEAX        $01,X           ; ADD ONE TO SCREEN POSITION
SEA33           RTS
; ENTER WITH ABSOLUTE SCREEN POSITION IN X, PIXEL MASK IN ACCA -
; ADJUST X AND ACCA TO THE NEXT PIXEL TO THE RIGHT FOR HSCREEN 1 & 4
SEA34           LSRA                        ;  SHIFT MASK ONE BIT TO THE RIGHT
                LSRA                        ;  DO IT AGAIN
                BCC         SEA33           ; BRANCH IF SAME BYTE
                LDA         #$C0            ; SET PIXEL #3 IF NEW BYTE
                LEAX        $01,X           ; ADD ONE TO SCREEN ADDRESS
                RTS
; ENTER WITH ABSOLUTE SCREEN POSITION IN X, PIXEL MASK IN ACCA -
; ADJUST X AND ACCA TO THE NEXT PIXEL TO THE RIGHT FOR HSREEN 2
SEA3D           COMA                        ;  SET TO ALTERNATE PIXEL
                CMPA        #$F0            ; SEE IF TOP HALF OF BYTE
                BNE         SEA44           ; BRANCH IF SAME BYTE
                LEAX        $01,X           ; MOVE POINTER TO NEXT SCREEN ADDRESS
SEA44           RTS
; ROUTINE TO MOVE DOWN ONE ROW
; ENTER WITH ABSOLUTE SCREEN ADDRESS IN X
SEA45           LDB         HORBYT          ; GET NUMBER OF BYTES PER HORIZONTAL GRAPHICS ROW
                ABX                         ;  ADD A ROW TO CURRENT ADDRESS (MOVE DOWN ONE ROW)
                RTS
; HCIRCLE
HCIRCLE         TST         HRMODE          ; CHECK HI-RES GRAPHICS MODE
                LBEQ        SE6EF           ; BRANCH IF NOT HI-RES GRAPHICS
                LBRN        RAMLINK         ; RAM HOOK
                CMPA        #'@             ; CHECK FOR @ SIGN (HCIRCLE@ IS LEGAL SYNTAX)
                BNE         SEA59           ; BRANCH IF NOT
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
SEA59           JSR         >SEB60          ; GET MAX HORIZONTAL & VERTICAL COORD VALUES AND PUT THEM IN VD3 & VD5
                JSR         >SEA04          ; GET HORIZONTAL & VERTICAL CENTER COORDS AND PUT THEM IN VBD AND VBF
                JSR         >SE7AD          ; NORMALIZE START COORDS FOR PROPER HI-RES GRAPHICS MODE
                LDX         ,U              ; GET HORIZONTAL COORD
                STX         VCB             ; SAVE IT
                LDX         $02,U           ; GET VERTICAL COORD
                STX         VCD             ; SAVE IT
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                JSR         LB73D           ; EVALUATE EXPRESSION, RETURN VALUE IN X
                LDU         #VCF            ; POINT U TO TEMP DATA STORAGE
                STX         ,U              ; SAVE RADIUS
                JSR         >SE7B0          ; NOW A JSR TO AN RTS; WAS A CALL TO A NORMALIZATION ROUTINE
                LDA         #$01            ; PSET FLAG
                STA         SETFLG          ; SAVE PSET/PRESET FLAG
                JSR         >SE718          ; GO EVALUATE COLOR EXPRESSION AND SAVE VALUE
                LDX         #$100           ; DEFAULT HEIGHT/WIDTH RATIO
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                BEQ         SEA95           ; BRANCH IF NONE
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                JSR         LB141           ; EVALUATE A NUMERIC EXPRESSION
                LDA         FP0EXP          ; GET FPA0 EXPONENT
                ADDA        #$08            ; ADD 8 TO IT (EFFECTIVELY MULTIPLIES BY 256)
                STA         FP0EXP          ; SAVE NEW VALUE
                JSR         LB740           ; EVALUATE EXPRESSION, RETURN VALUE IN X
SEA95           LDA         HRMODE          ; GET CURRENT HI-RES GRAPHICS MODE
                CMPA        #$02            ; SEE WHICH MODE IT IS
                BHI         SEA9F           ; BRANCH IF HSCREEN 4
                TFR         X,D             ; PREPARE TO DOUBLE THE HEIGHT/WIDTH RATIO FOR MODES 0-2
                LEAX        D,X             ; DOUBLE H/W RATIO TO COMPENSATE FOR HORIZONTAL PIXEL SIZE
SEA9F           STX         VD1             ; SAVE H/W RATIO
                LDB         #$01            ; CODE FOR PSET
                STB         SETFLG          ; SET PSET/PRESET FLAG TO PSET
                STB         VD8             ; FIRST TIME FLAG - SET TO 0 AFTER ARC DRAWN
                JSR         >SEB7B          ; EVALUATE CIRCLE START POINT (OCTANT, SUBARC)
                PSHS        B,A             ; SAVE START POINT
                JSR         >SEB7B          ; EVALUATE CIRCLE END POINT (OCTANT, SUBARC)
                STD         VD9             ; SAVE END POINT
                PULS        A,B             ; GET BACK START POINT
SEAB3           PSHS        B,A             ; STORE CURRENT CIRCLE POSITION
                LDX         HOREND          ; GET END HORIZONTAL COORD
                STX         HORBEG          ; MAKE IT THE NEW START
                LDX         VEREND          ; GET END VERTICAL COORD
                STX         VERBEG          ; MAKE IT THE NEW START
                LDU         #CIRCDATA+2     ; POINT TO TABLE OF SINES AND COSINES
                ANDA        #$01            ; TEST OCTANT NUMBER
                BEQ         SEAC7           ; BRANCH IF EVEN
                NEGB                        ;
                ADDB        #$08            ; CONVERT 0-7 TO 8-1 FOR ODD OCTANT NUMBERS
SEAC7           ASLB                        ;  MUL BY 2
                ASLB                        ;  DO IT AGAIN (FOUR BYTES PER TABLE ENTRY)
                LEAU        B,U             ; POINT TO CORRECT TABLE ENTRY
                PSHS        U               ; SAVE SIN/COS TABLE ENTRY
                JSR         >SEBBD          ; CALCULATE HORIZONTAL OFFSET
                PULS        U               ; GET BACK SIN/COS TABLE POINTER
                LEAU        $-02,U          ; MOVE TO COSINE (VERTICAL)
                PSHS        X               ; SAVE HORIZONTAL OFFSET
                JSR         >SEBBD          ; CALCULATE VERTICAL OFFSET
                PULS        Y               ; PUT HORIZONTAL OFFSET IN Y
                LDA         ,S              ; GET OCTANT NUMBER
                ANDA        #$03            ; MASK OFF BOTTOM TWO BITS
                BEQ         SEAE7           ; BRANCH IF OCTANT 0 OR 4
                CMPA        #$03            ; NOW SEE IF BOTH BITS WERE SET
                BEQ         SEAE7           ; BRANCH IF OCTANT 3 OR 7
                EXG         X,Y             ; SWAP HORIZONTAL AND VERTICAL OFFSETS
SEAE7           STX         HOREND          ; SAVE HORIZONTAL OFFSET
; H/W RATIO WILL ONLY MODIFY THE VERTICAL COORD
                TFR         Y,D             ; PUT CALCULATED VERTICAL OFFSET INTO ACCD
                LSRA                        ;
                RORB                        ;  DIVIDE OFFSET BY 2
                LDX         VD1             ; GET H/W RATIO
                JSR         >SEBCB          ; MULT VERTICAL OFFSET BY H/W RATIO
                TFR         Y,D             ; TRANSFER PRODUCT TO ACCD
                TSTA                        ;  CHECK OVERFLOW AND GET MS BYTE RESULT
                LBNE        LB44A           ; ILLEGAL FUNCTION CALL ERROR (RESULT > 255)
                STB         VEREND          ; SAVE DELTA VERTICAL MS BYTE
                TFR         U,D             ; LS BYTE RESULT TO ACCA
                STA         VEREND+1        ; SAVE DELTA VERTICAL LS BYTE
                LDA         ,S              ; GET OCTANT NUMBER
                CMPA        #$02            ; CHECK FOR OCTANT 0,1,6,7
                BCS         SEB13           ; BRANCH IF SUBARC HORIZONTAL END POINT >= HORIZONTAL CENTER
                CMPA        #$06            ; MORE CHECKS FOR OCTANT 0,1,6,7
                BCC         SEB13           ; BRANCH IF SUBARC HORIZONTAL END POINT >= HORIZONTAL CENTER
                LDD         VCB             ; GET HORIZONTAL COORD OF CENTER
                SUBD        HOREND          ; SUBTRACT HORIZONTAL DIFFERENCE
                BCC         SEB20           ; BRANCH IF NO UNDERFLOW
                CLRA                        ;
                CLRB                        ;  FORCE COORD TO 0 IF RESULT WAS LESS THAN 0
                BRA         SEB20           ; SAVE NEW COORD
SEB13           LDD         VCB             ; GET HORIZONTAL COORD OF CENTER
                ADDD        HOREND          ; ADD HORIZONTAL DIFFERENCE
                BCS         SEB1E           ; BRANCH IF OVERFLOW
                CMPD        VD3             ; COMPARE TO MAX HORIZONTAL COORDINATE
                BCS         SEB20           ; BRANCH IF < MAX HOR
SEB1E           LDD         VD3             ; GET MAX HORIZONTAL COORD
SEB20           STD         HOREND          ; SAVE NEW HORIZONTAL SUBARC END COORD
                LDA         ,S              ; GET OCTANT NUMBER
                CMPA        #$04            ; CHECK FOR OCTANT 0,1,2 OR 3
                BCS         SEB32           ; BRANCH IF SUBARC VERTICAL END POINT >= VERTICAL CENTER
                LDD         VCD             ; GET VERTICAL COORD OF CENTER
                SUBD        VEREND          ; SUBTRACT VERTICAL DIFFERENCE
                BCC         SEB3F           ; BRANCH IF NO UNDERFLOW
                CLRA                        ;
                CLRB                        ;  FORCE NEW VERTICAL TO 0 IF MINUS
                BRA         SEB3F           ; SAVE NEW COORD
SEB32           LDD         VCD             ; GET VERTICAL COORD OF CENTER
                ADDD        VEREND          ; ADD VERTICAL DIFFERENCE
                BCS         SEB3D           ; BRANCH IF OVERFLOW
                CMPD        VD5             ; COMPARE TO MAX VERTICAL COORD
                BCS         SEB3F           ; BRANCH IF < MAX VER
SEB3D           LDD         VD5             ; GET MAX VERTICAL COORD
SEB3F           STD         VEREND          ; SAVE NEW VERTICAL SUBARC END COORD
                TST         VD8             ; CHECK FIRST TIME FLAG
                BNE         SEB48           ; DO NOT DRAWE A LINE FIRST TIME THROUGH -
; BECAUSE THE FIRST TIME YOU WOULD DRAW A LINE
; FROM THE CENTER TO THE FIRST POINT ON THE CIRCLE
                LBSR        SE94E           ; DRAW A LINE
SEB48           PULS        A,B             ; GET END COORDS
                LSR         VD8             ; SHIFT FIRST TIME FLAG
                BCS         SEB53           ; DO NOT CHECK FOR END POINT AFTER DRAWING FIRST ARC
                CMPD        VD9             ; COMPARE CURRENT POSITION TO END POINT
                BEQ         SEB5F           ; BRANCH IF CIRCLE DRAWING IS FINISHED
; INCREMENT SUBARC CTR, IF . 7 THEN INC OCTANT CTR
SEB53           INCB                        ;  INC SUBARC COUNTER
                CMPB        #$08            ; > 7?
                BNE         SEB5C           ; BRANCH IF NOT
                INCA                        ;  INCR OCTANT COUNTER
                CLRB                        ;  RESET SUBARC COUNTER
                ANDA        #$07            ; KEEP IN RANGE OF 0-7; ONCE ACCA=ACCB, THIS WILL MAKE ACCA=0
; SO THE END POINT WILL BE (0,0) AND THE CIRCLE ROUTINE WILL END
SEB5C           JMP         >SEAB3          ; KEEP DRAWING THE CIRCLE
SEB5F           RTS         EXIT            ; CIRCLE ROUTINE
; GET MAXIMUM VALUE OF HORIZONTAL & VERTICAL COORDINATES NORMALIZED FOR
; PROPER GRAPHICS MODE. RETURN VALUES: HORIZONTAL IN VD3, VERTICAL IN VD5
SEB60           LDU         #VD3            ; POINT U TO STORAGE AREA
                LDX         #640-1          ; GET MAXIMUM HORIZONTAL COORD
                STX         ,U              ; SAVE IT
                LDA         HRMODE          ; GET CURRENT GRAPHICS MODE
                CMPA        #$02            ; SEE WHICH MODE
                BGT         SEB73           ; BRANCH IF MODES 3 OR 4
                LDX         #320-1          ; MAXIMUM VALUE FOR HORIZONTAL COORD IN MODES 1 AND 2
                STX         ,U              ; SAVE IT
SEB73           LDX         #192-1          ; GET THE MAXIMUM VERTICAL COORD
                STX         $02,U           ; SAVE IT
                JMP         >SE7B0          ; JUMP TO AN RTS; ONCE WAS A NORMALIZATION ROUTINE
; EVALUATE CIRCLE START POINT (OCTANT, SUBARC)
; CALCULATE START OF END POINT WHICH IS A NUMBER FROM
; 0-63 SAVED AS AN OCTANT NUMBER (0-7) AND SUBARC NUMBER (0-7)
SEB7B           CLRB                        ;  SET DEFAULT VALUE TO 0
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                BEQ         SEB91           ; BRANCH IF NONE
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                JSR         LB141           ; EVALUATE A NUMERIC EXPRESSION
                LDA         FP0EXP          ; GET EXPONENT OF FPA0
                ADDA        #$06            ; ADD 6 TO EXPONENT - MULTIPLY EXPONENT BY 64
                STA         FP0EXP          ; RESAVE IT
                JSR         LB70E           ; CONVERT FPA0 TO INTEGER IN ACCB
                ANDB        #$3F            ; FORCE MAX VALUE OF 63
SEB91           TFR         B,A             ; SAVE VALUE IN ACCA ALSO
                ANDB        #$07            ; NOW ACCB CONTAINS SUBARC NUMBER
                LSRA                        ;
                LSRA                        ;
                LSRA                        ;  DIVIDE ACCA BY 8 - OCTANT NUMBER
                RTS
CIRCDATA        FDB         $0000,$0001     ; SUBARC 0
                FDB         $FEC5,$1919     ; SUBARC 1
                FDB         $FB16,$31F2     ; SUBARC 2
                FDB         $F4FB,$4A51     ; SUBARC 3
                FDB         $EC84,$61F9     ; SUBARC 4
                FDB         $E1C7,$78AE     ; SUBARC 5
                FDB         $D4DC,$8E3B     ; SUBARC 6
                FDB         $C5E5,$A269     ; SUBARC 7
                FDB         $B506,$B506     ; SUBARC 8
; MULTIPLY RADIUS BY SIN/COS VALUE AND RETURN OFFSET IN X
SEBBD           LDX         VCF             ; GET RADIUS
                LDD         ,U              ; GET SIN/COS TABLE MODIFIER
                BEQ         SEBCA           ; BRANCH IF 0 (OFFSET = RADIUS)
                SUBD        #1              ; SUBTRACT ONE
                BSR         SEBCB           ; MULTIPLY RADIUS BY SIN/COS
                TFR         Y,X             ; RETURN RESULT IN X
SEBCA           RTS
; MULTIPLY (UNSIGNED) TWO 16 BIT NUMBERS TOGETHER -
; ENTER WITH ONE NUMBER IN ACCD, THE OTHER IN X REGISTER
; THE 4 BYTE PRODUCT WILL BE STORED IN 4,S - 7,S
; (Y, U REGISTERS ON THE STACK). I.E. (AA AB) x (XH,XL)=
; 256 * AA * XH + 16 * (AA * XL + AB * HX) + AB * XL. THE TWO BYTE
; MULTIPLIER AND THE MULTIPLICAND ARE TREATED AS A 1
; BYTE INTEGER PART (MSB) WITH A 1 BYTE FRACTIONAL PART (LSB)
SEBCB           PSHS        U,Y,X,B,A       ; SAVE REGISTERS AND RESERVE STORAGE SPACE ON THE STACK
                CLR         $04,S           ; RESET OVERFLOW FLAG
                LDA         $03,S           ;
                MUL                         ;  =
                STD         $06,S           ; CALCULATE ACCB*XL, STORE RESULT IN 6,S
                LDD         $01,S           ;
                MUL                         ;  CALCULATE ACCB*XH
                ADDB        $06,S           ;
                ADCA        #$00            ;
                STD         $05,S           ; ADD THE CARRY FROM THE 1ST MUL TO THE RESULT OF THE 2ND MUL
                LDB         ,S              ;
                LDA         $03,S           ;
                MUL                         ;  CALCULATE ACCA*XL
                ADDD        $05,S           ;
                STD         $05,S           ; ADD RESULT TO TOTAL OF 2 PREVIOUS MULTS
                BCC         SEBEA           ; BRANCH IF NO OVERFLOW
                INC         $04,S           ; SET OVERFLOW FLAG (ACCD > $FFFF)
SEBEA           LDA         ,S              ;
                LDB         $02,S           ;
                MUL                         ;  CALCULATE ACCA*XH
                ADDD        $04,S           ;
                STD         $04,S           ; ADD TO PREVIOUS RESULT
                PULS        A,B,X,Y,U,PC    ; RETURN WITH RESULT IN U AND Y
; HPAINT
HPAINT          TST         HRMODE          ; CHECK HI-RES GRAPHICS MODE
                LBEQ        SE6EF           ; 'HR' ERROR IF HI-RES GRAPHICS MODE NOT SET UP
                LBRN        RAMLINK         ; RAM HOOK
                CMPA        #'@             ; CHECK FOR @ SIGN
                BNE         SEC05           ; BRANCH IF NOT
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
SEC05           JSR         >SEA04          ; SYNTAX CHECK FOR '(', TWO EXPRESSIONS, AND ')'
                JSR         >SE7AD          ; NORMALIZE THE HORIZONTAL AND VERTICAL COORDS
                LDA         #$01            ; CODE FOR PSET
                STA         SETFLG          ; SET PSET/PRESET FLAG TO PSET
                JSR         >SE718          ; GET PAINT COLOR CODE & SET ACTIVE COLOR AND ALL PIXEL BYTES
                LDD         WCOLOR          ; GET THEM
                PSHS        B,A             ; SAVE THEM ON THE STACK
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                BEQ         SEC1D           ; BRANCH IF NONE LEFT - DEFAULT BORDER COLOR TO FOREGROUND,
; PAINT COLOR TO BACKGROUND
                JSR         >SE718          ; EVALUATE THE BORDER COLOR
SEC1D           LDA         ALLCOL          ; GET BORDER COLOR ALL PIXEL BYTE
                STA         VD8             ; TEMP SAVE IT
                PULS        A,B             ; GET PAINT ACTIVE COLORS BACK
                STD         WCOLOR          ; RESAVE THEM
                JSR         SELTASK1
                CLRA                        ;  STORE A BLOCK OF 'PAINT' DATA ON THE STACK WHICH
                PSHS        U,X,B,A         ; WILL ACT AS AN END OF 'PAINT' DATA FLAG.
; THE CLRA WILL CAUSE THE UP/DN FLAG TO BE ZERO WHICH IS USED TO EXIT THE HPAINT ROUTINE
                JSR         >SEB60          ; GET NORMALIZED MAX HOR/VERTICAL VALUES - RETURN RESULT IN VD3,VD5
                JSR         >SE7E6          ; POINT U TO THE ROUTINE WHICH WILL SELECT A PIXEL

; 'PAINT' THE FIRST HORIZONTAL LINE FROM THE START COORDINATES
                STU         VD9             ; SAVE ADDRESS
                JSR         >SECBE          ; 'PAINT' FROM THE CURRENT HORIZONTAL COORD TO ZERO
                BEQ         SEC47           ; BRANCH IF NO PAINTING DONE - HIT BORDER INSTANTLY
                JSR         >SED01          ; PAINT TOWARD MAX HORIZONTAL COORD
                LDA         #$01            ; SET UP/DN FLAG TO UP (1=UP, $FF=DOWN)
                STA         VD7             ; SAVE IT
                JSR         >SED2E          ; SAVE POSITIVE GOING LINE INFO ON STACK
                NEG         VD7             ; SET UP/DN FLAG TO $FF (DOWN)
                JSR         >SED2E          ; SAVE NEGATIVE GOING LINE INFO ON STACK
SEC47           STS         TMPSTK          ; TEMP STORE STACK POINTER
SEC4A           TST         CHGFLG          ; SEE IF PAINTED COLOR IS DIFFERENT THAN THE ORIGINAL COLOR
                BNE         SEC51           ; BRANCH IF DATA HAS BEEN MODIFIED
                LDS         TMPSTK          ; GET STACK POINTER BACK
SEC51           PULS        A,B,X,U         ; GET DATA FOR NEXT LINE SEGMENT TO CHECK FROM THE STACK
                CLR         CHGFLG          ; CLEAR THE CHANGE FLAG
                STS         TMPSTK          ; TEMP SAVE THE STACK ADDRESS
                LEAX        $01,X           ; ADD ONE TO 'START HORIZONTAL COORD -1'
                STX         HORBEG          ; PIT IT AT 'CURRENT HORIZONTAL COORD ADDRESS'
                STU         VD1             ; SAVE LENGTH OF PARENT LINE
                STA         VD7             ; SAVE UP/DN FLAG
                BEQ         SECBA           ; EXIT ROUTINE IF UP/DN FLAG = 0
                BMI         SEC6A           ; BRANCH IF UP/DN FLAG = DOWN
; CHECK ONE LINE BELOW CURRENT DATA
                INCB                        ;  INCREMENT VERTICAL COORD
                CMPB        VD6             ; COMPARE TO MAXIMUM VERTICAL COORD
                BLS         SEC6E           ; BRANCH IF NOT GREATER - PROCESS LINE
                CLRB                        ;  SET VERTICAL COORD TO ZERO TO FORCE WRAP AROUND
SEC6A           TSTB                        ;  CHECK VERTICAL COORD
                BEQ         SEC4A           ; PROCESS ANOTHER BLOCK OF PAINT DATA IF WRAP AROUND -
; DISCARD ANY LINE BELOW VERTICAL COORD = 0 OR ABOVE MAX VER COORD
                DECB                        ;  DEC VERTICAL COORD
; PROCESS A HORIZONTAL LINE THAT WAS STORED ON STACK - LIMIT CHECK HAVE BEEN DONE
SEC6E           STB         VERBEG+1        ; SAVE CURRENT VERTICAL COORD
                JSR         >SECBE          ; PAINT FROM HORIZONTAL COORD TO ZERO OR BORDER
                BEQ         SEC86           ; BRANCH IF NO PIXELS WERE PAINTED
                CMPD        #3              ; SEE IF FEWER THAN 3 PIXELS WERE PAINTED
                BCS         SEC80           ; BRANCH IF NO NEED TO CHECK FOR PAINTABLE DATA
                LEAX        $-02,X          ; MOVE HORIZONTAL COORD TWO PIXELS TO THE LEFT
                JSR         >SED15          ; SAVE A BLOCK OF PAINT DATA IN THE DIRECTION OPPOSITE TO UP/DN FLAG
SEC80           JSR         >SED01          ; CONTINUE PAINTING LINE TO THE RIGHT
SEC83           JSR         >SED2E          ; SAVE A BLOCK OF PAINT DATA IN THE SAME DIRECTION AS UP/DN FLAG
; THIS CODE WILL INSURE THAT THE CURRENT LINE IS
; EXAMINED TO THE RIGHT FOR PAINTABLE PIXELS FOR A
; LINE EQUAL TO THE LENGTH OF THE PARENT LINE
SEC86           COMA                        ;
                COMB                        ;  COMPLEMENT LENGTH OF LINE JUST PAINTED
SEC88           ADDD        VD1             ; ADD TO LENGTH OF PARENT LINE
                STD         VD1             ; SAVE DIFFERENCE OF LINE JUST PAINTED AND PARENT LINE
                BLE         SECA5           ; BRANCH IF PARENT LINE IS SHORTER
                JSR         >SE9B1          ; GO INCR HORIZONTAL COORD
                JSR         >SECF1          ; CHECK FOR BORDER COLOR
                BNE         SEC9B           ; BRANCH IF NOT BORDER COLOR
                LDD         #-1             ; GO DECREMENT ONE FROM LENGTH OF DIFFERENCE
                BRA         SEC88           ; LINE AND KEEP LOOKING FOR NON BORDER COLOR
SEC9B           JSR         >SE9BF          ; GET DECR HORIZONTAL COORD
                JSR         >SED3A          ; GET AND SAVE HORIZONTAL COORD
                BSR         SECC7           ; PAINT FORWARD TO MAX HORIZONTAL COORD OR BORDER
                BRA         SEC83           ; SAVE BLOCK OF PAINT DATA AND KEEP CHECKING

; CHECK TO SEE IF THE CURRENT LINE EXTENDS FURTHER TO
; THE RIGHT THAN THE PARENT LINE AND PUT A BLOCK OF
; PAINT DATA ON THE STACK IF IT IS MORE THAN 2 PIXELS
; PAST THE END OF THE PARENT LINE
SECA5           JSR         >SE9B1          ; INC CURRENT HORIZONTAL COORD
                LEAX        D,X             ; POINT X TO THE RIGHT END OF THE PARENT LINE
                STX         HORBEG          ; SAVE AS THE CURRENT HORIZONTAL COORDINATE
                COMA                        ;  = ACCA CONTAINS A NEGATIVE NUMBER CORRESPONDING TO THE NUMBER
                COMB                        ;  = OF PIXELS THE CURRENT LINE EXTENDS PAST THE RIGHT END
                SUBD        #1              ; OF THE PARENT LINE. CONVERT TO POSITIVE NUMBER AND BRANCH
                BLE         SECB7           ; IF THE LINE DOESN'T EXTEND PAST THE END OF THE PARENT.
                TFR         D,X             ; SAVE PORTION OF THE LINE TO THE RIGHT OF THE PARENT LINE
; AS THE LENGTH
                BSR         SED15           ; SAVE BLOCK OF PAINT DATA IN THE DIRECTION OPPOSITE THE
; CURRENT UP/DN FLAG
SECB7           JMP         >SEC4A          ; PROCESS MORE PAINT DATA BLOCKS
SECBA           JSR         SELTASK0        ; ENABLE TASK REGISTER 0
                RTS
; PAINT FROM HORIZONTAL COORD TO ZERO OR HIT BORDER; RETURN WITH Z=1 IF NO PAINTING DONE
SECBE           JSR         >SED3A          ; PUT STARTING COORD IN HOREND
                LDY         #SE9BF          ; ROUTINE TO DEC HORIZONTAL ADDRESS
                BRA         SECCD           ; GO PAINT THE LINE
; PAINT FROM HORIZONTAL COORD TO MAX HORIZONTAL COORD OR HIT BORDER; RETURN Z=1 IF NO PAINTING DONE
SECC7           LDY         #SE9B1          ; ROUTINE TO INCR HORIZONTAL COORD
                JSR         ,Y              ; INCR HORIZONTAL COORD - LEFT PAINT ROUTINE PAINTED FIRST COORD
SECCD           LDU         ZERO            ; ZERO INITIAL PIXEL PAINT COUNTER
                LDX         HORBEG          ; GET HORIZONTAL COORD
SECD1           BMI         SECEA           ; BRANCH IF HORIZONTAL COORD IS > $7F OR < 0
                CMPX        VD3             ; COMPARE CURRENT COORD TO MAX VALUE
                BHI         SECEA           ; BRANCH IF > MAX
                PSHS        U,Y             ; SAVE PAINT COUNTER AND INC/DEC POINTER
                BSR         SECF1           ; CHECK FOR BORDER PIXEL
                BEQ         SECE8           ; BRANCH IF HIT BORDER
                JSR         >SE792          ; SET PIXEL TO PAINT COLOR - PAINTING IS DONE HERE
                PULS        Y,U             ; RESTORE PAINT COUNTER AND INC/DEC POINTER
                LEAU        $01,U           ; ADD ONE TO PAINT COUNTER
                JSR         ,Y              ; INCR OR DECR HORIZONTAL COORD DEPENDING ON CONTENTS OF Y
                BRA         SECD1           ; KEEP PAINTING LINE
SECE8           PULS        Y,U             ; RESTORE PAINT COUNTER AND INC/DEC POINTER
SECEA           TFR         U,D             ; SAVE PAINT COUNTER IN ACCD
                TFR         D,X             ; ALSO SAVE IT IN X
                SUBD        ZERO            ; SET COUNTERS ACCORDING TO CONDITION OF PAINT COUNTER
                RTS
; CHECK FOR BORDER COLOR - ENTER WITH VD9 CONTAINING
; ADDRESS OF ROUTINE TO GET ABSOLUTE SCREEN ADDRESS
; AND PIXEL MASK - EXIT WITH Z=1 IF HIT BORDER COLOR PIXEL
SECF1           JSR         [VD9]           ; GET SCREEN ADDRESS AND PIXEL MASK
                TFR         A,B             ; COPY PIXEL MASK IN ACCB
                ANDB        VD8             ; AND PIXEL MASK WITH BORDER COLOR
                PSHS        B,A             ; SAVE MASK AND BORDER PIXEL
                ANDA        ,X              ; TEST THE PIXEL ON THE SCREEN
                CMPA        $01,S           ; COMPARE WITH ACCB ON THE STACK
                PULS        A,B,PC          ; EXIT WITH Z FLAG=1 IF MATCH
; GO HERE TO FINISH PAINTING TO RIGHT AFTER YOU HAVE PAINTED LEFT
SED01           STD         VCD             ; SAVE NUMBER OF PIXELS PAINTED
                LDY         HOREND          ; GET LAST HORIZONTAL START COORD
                BSR         SED3A           ; SAVE CURRENT HORIZONTAL COORD - HOREND NOW CONTAINS COORDINATE
; OF THE LEFT BORDER OF THIS HORIZONTAL LINE
                STY         HORBEG          ; START PAINTING TO RIGHT FROM THE LEFT PAINT START COORD
                BSR         SECC7           ; PAINT TOWARDS THE RIGHT
                LDX         VCD             ; GET THE NUMBER OF PIXELS PAINTED WHEN GOING TOWARDS LEFT PIXELS
                LEAX        D,X             ; ADD NUMBER OF PAINTED GOING TOWARD THE RIGHT
                ADDD        #1              ; ADD 1 TO PAINT COUNT TOWARD RIGHT - ACCD=LENGTH OF PAINTED LINE
                RTS
; BLOCKS OF DATA ARE STORED ON THE STACK SO THAT HPAINT
; CAN REMEMBER WHERE IT SHOULD GO BACK AND PAINT UP OR DOWN
; FROM THE CURRENT LINE IT IS PAINTING. THESE BLOCKS OF DATA
; REPRESENT HORIZONTAL LINES ABOVE OR BELOW THE CURRENT LINE
; BEING PAINTED AND REQUIRE SIX BYTES OF STORAGE ON THE STACK.
; THE DATA ARE AS FOLLOWS: ,S=UP/DN FLAG; 1,S=VERTICAL COORD
; OF LINE; 2 3,S=LEFT MOST HORIZONTAL COORD OF LINE; 4 5,S=LENGTH OF LINE
; SAVE A BLOCK OF PAINT DATA FOR A LINE IN THE OPPOSITE DIRECTION OF THE CURREN UP/DN FLAG
SED15           STD         VCB             ; SAVE NUMBER OF PIXELS PAINTED
                PULS        Y               ; GET RETURN ADDRESS IN Y
                LDD         HORBEG          ; GET HORIZONTAL START COORD
                PSHS        X,B,A           ; PUT ON STACK
                LDA         VD7             ; GET UP/DN FLAG
                NEGA                        ;  REVERSE IT
SED20           LDB         VERBEG+1        ; GET VERTICAL START COORDINATE
                PSHS        B,A             ; SAVE VERTICAL START COORD AND UP/DN FLAG
                PSHS        Y               ; PUT BACK RETURN ADDRESS
                LDB         #$06            ; GET NUMBER OF FREE BYTES TO CHECK FOR
                JSR         >SED3F          ; GO SEE IF THERE IS ENOUGH RAM
                LDD         VCB             ; GET LENGTH OF RIGHT PAINTED LINE
                RTS
; SAVE A BLOCK OF PAINT DATA FOR A LINE IN THE SAME DIRECTION AS THE CURRENT UP/DN FLAG
SED2E           STD         VCB             ; SAVE THE LENGTH OF RIGHT HORIZONTAL PAINTED LINE
                PULS        Y               ; SAVE RETURN ADDRESS IN Y
                LDD         HOREND          ; GET HORIZONTAL START COORD
                PSHS        X,B,A           ; SAVE START COORD AND LENGTH
                LDA         VD7             ; GET UP/DN FLAG (1 OR -1)
                BRA         SED20           ; SAVE THE PAINT DATA ON THE STACK
SED3A           LDX         HORBEG          ; GET CURRENT HORIZONTAL COORD
                STX         HOREND          ; SAVE IT
                RTS
; CHECK ACCB (ONLY 0-127) BYTES OF FREE RAM ON THE STACK
SED3F           NEGB                        ;
                LEAS        B,S             ; MOVE THE STACK POINTER DOWN ACCB BYTES
                CMPS        #TMPSTACK-($2000+14) ; COMPARE TO THE BOTTOM OF THE STACK AREA - THE 14 EXTRA BYTES ARE
; GENERATED BY THE FACT THAT THE SEVEN INTERRUPT VECTORS ARE GOTTEN FROM
; THE ROM BY THE GIME CHIP. THE 14 BYTES IN RAM ARE UNUSED BY BASIC.
                LBCS        SED4E           ; 'OM' ERROR IF PAST THE BOTTOM
                NEGB                        ;  MAKE ACCB POSITIVE AGAIN
                LEAS        B,S             ; PUT THE STACK POINTER BACK WHERE IT BELONGS
                RTS
SED4E           LDS         #TMPSTACK-2     ; PUT THE STACK POINTER AT THE TOP OF THE TEMPORARY STACK BUFFER
                JSR         SELTASK0        ; ENABLE TASK REGISTER 0
                JMP         LAC44           ; GO DO AN 'OM' ERROR
; HBUFF
; THE HBUFF COMMAND WILL RESERVE SPACE IN THE HPUT/HGET BUFFER. THERE MUST BE ENOUGH FREE RAM
; IN THE BUFFER FOR THE REQUESTED BUFFER SIZE AND A FIVE BYTE HEADER. EACH BUFFER HAS A FIVE BYTE
; HEADER WHICH IS DESCRIBED AS FOLLOWS:
; BYTES 0,1: ADDRESS OF THE NEXT HPUT/HGET BUFFER IN THE BUFFER SPACE. IF ZERO, THERE ARE
; NO MORE BUFFERS IN THE BUFFER SPACE. IF $FFFF, THEN THERE ARE NO
; BUFFERS ALLOCATED AND THE ENTIRE BUFFER SPACE IS FREE.
; BYTE 2: BUFFER NUMBER; BYTES 3,4: SIZE OF THE BUFFER
; HBUFF
HBUFF           JSR         LB73D           ; EVALUATE BUFFER NUMBER ARGUMENT; RETURN VALUE IN X
                LBRN        RAMLINK         ; RAM HOOK
                CMPX        #255            ; MAXIMUM OF 255 BUFFERS ALLOWED
                LBHI        LB44A           ; ILLEGAL FUNCTION CALL ERROR IF BUFFER NUMBER > 255
                STX         VD1             ; SAVE THE BUFFER NUMBER
                BEQ         SED72           ; DON'T GET THE SIZE OF THE BUFFER IF BUFFER 0 SELECTED
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                JSR         LB73D           ; EVALUATE THE BUFFER SIZE ARGUMENT
                STX         VD3             ; SAVE THE BUFFER SIZE
SED72           JSR         >SE0CB          ; PUT BLOCK 6.4 INTO LOGICAL BLOCK 6 ($C000) OF TASK REGISTER 1
                JSR         SELTASK1        ; ENABLE TASK REGISTER 1
                LDD         VD1             ; GET THE NEW BUFFER NUMBER
                TSTB                        ;  CHECK FOR BUFFER ZERO
                BNE         SED85           ; BRANCH IF NOT BUFFER ZERO
                LDD         #$FFFF          ; EMPTY BUFFER FLAG
                STD         HRESBUFF        ; RESET BUFFER SPACE TO EMPTY
                BRA         SEDBD           ; EXIT COMMAND
SED85           LDY         #HRESBUFF       ; POINT TO THE START OF THE BUFFER SPACE
                LDD         ,Y              ; GET THE FIRST TWO BYTES OF THE HEADER BLOCK (HB.ADDR)
                CMPD        #$FFFF          ; IS THE BUFFER EMPTY?
                BNE         SED95           ; NO; CHECK FOR FIRST EMPTY HEADER SPOT
                BSR         SEDC4           ; CHECK FOR ENOUGH FREE RAM IN THE BUFFER SPACE FOR THIS BUFFER
                BRA         SEDB0
SED95           LDB         VD1+1           ; GET THE BUFFER NUMBER
SED97           CMPB        $02,Y           ; COMPARE TO THE BUFFER NUMBER OF THE CURRENT HEADER (HB.NUM)
                BEQ         SEDD2           ; RE-DIMENSIONED ARRAY (DD) ERROR IF THE SAME
                LDU         ,Y              ; GET THE ADDRESS OF THE NEXT BUFFER
                BEQ         SEDA3           ; BRANCH IF THIS IS THE LAST HEADER
                TFR         U,Y             ; POINT Y TO THE START OF THE NEXT BUFFER HEADER
                BRA         SED97           ; KEEP SEARCHING FOR THE LAST HEADER
SEDA3           TFR         Y,U             ; SAVE THE START ADDRESS OF THE LAST HEADER IN U
                LDD         $03,Y           ; GET THE SIZE OF THE LAST HEADER (HB.SIZE)
                LEAY        $05,Y           ; SKIP PAST THE HEADER DATA (HB.LEN)
                LEAY        D,Y             ; NOW Y POINTS TOT THE START OF FREE BUFFER SPACE
                BSR         SEDC4           ; CHECK FOR ENOUGH FREE RAM IN THE BUFFER SPACE FOR THIS BUFFER
                STY         ,U              ; SAVE THE ADDRESS OF THIS HEADER IN THE PREVIOUS HEADER
SEDB0           LDD         #0              ; LAST ENTRY FLAG
                STD         ,Y              ; MAKE THIS HEADER THE LAST ENTRY
                LDB         VD1+1           ; GET THE BUFFER NUMBER AND
                STB         $02,Y           ; SAVE IT IN THE HEADER
                LDD         VD3             ; GET THE SIZE OF THE BUFFER AND
                STD         $03,Y           ; SAVE IT IN THE HEADER TOO
SEDBD           JSR         SELTASK0        ; ENABLE TASK REGISTER 0
                JSR         SETMMU          ; RESET MMU REGISTERS
                RTS
SEDC4           TFR         Y,X             ; USE X A TEMPORARY POINTER TO THE START OF BUFFER
                LEAX        $05,X           ; SKIP PAST THE HEADER
                LDD         VD3             ; GET THE SIZE OF THE BUFFER AND
                LEAX        D,X             ; ADD IT TO THE BUFFER POINTER WHICH NOW POINTS TO THE BUFFER END
                CMPX        #HRESBUFF+$1F00 ; PAST THE END OF THE BUFFER SPACE?
                BHI         SEDD6           ; 'OM' ERROR IF PAST END OF BUFFER SPACE
                RTS
SEDD2           LDB         #9*2            ; REDIMENSIONED ARRAY ERROR (DD)
                BRA         SEDD8
SEDD6           LDB         #6*2            ; OUT OF MEMORY ERROR (OM)
SEDD8           LDS         #TMPSTACK-2     ; RESET THE STACK TO TEMPORARY LOCATON
                JSR         SELTASK0        ; ENABLE TASK REGISTER 0
                JSR         SETMMU          ; RESET THE MMU REGISTERS
                JMP         LAC46           ; JUMP TO THE ERROR HANDLER
; HGET
HGET            LDX         #SEEC0          ; POINT X TO THE HGET MOVEMENT ROUTINE
                STX         VD5             ; SAVE THE MOVEMENT ROUTINE ADDRESS
                CLRB                        ;  HGET FLAG
                BRA         SEDF4
; HPUT
HPUT            LDX         #SEEEF          ; HPUT MOVEMENT ROUTINE ADDRESS
                STX         VD5             ; SAVE THE MOVEMENT ROUTINE ADDRESS
                LDB         #$01            ; HPUT FLAG
SEDF4           TST         HRMODE          ; IS THE HI-RES GRAPHICS MODE ENABLED?
                LBEQ        SE6EF           ; 'HR' ERROR IF NOT IN HI-RES MODE
                LBRN        RAMLINK         ; RAM HOOK
                STB         VD8             ; SAVE THE GET/PUT FLAG
                CMPA        #'@             ; ALLOW HGET@, HPUT@ AS LEGAL SYNTAX
                BNE         SEE06           ; BRANCH IF NOT @
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE IF @ WAS THERE
SEE06           JSR         >SE9E1          ; EVALUATE THE RECTANGLE BOUNDS
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                STB         VD3             ; SAVE THE BUFFER NUMBER
                CLR         VD4             ; SET THE ACTION FLAG TO SHOW AN ACTION SPECIFIED
                JSR         GETCCH          ; GET BASIC'S CURRENT INPUT CHARACTER
                BEQ         SEE38           ; BRANCH IF END OF LINE - NO ACTION SPECIFIED
                COM         VD4             ; SET THE ACTION FLAG TO SHOW THAT AN ACTION WAS SPECIFIED
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR COMMA
                TST         VD8             ; CHECK THE GET/PUT FLAG
                BNE         SEE23           ; BRANCH IF PUT
                LBRA        LB277           ; 'SYNTAX' ERROR - HGET MAY NOT SPECIFY AN ACTION
SEE23           LDB         #$05            ; FIVE POSSIBLE ACTIONS
                LDX         #SEEE0          ; POINT TO THE ACTION ROUTINE ADDRESS
SEE28           LDU         ,X++            ; GET THE ACTION ROUTINE ADDRESS
                CMPA        ,X+             ; COMPARE THE DESIRED ACTION TO THIS ROUTINE'S TOKEN
                BEQ         SEE34           ; SEARCH NO MORE - A MATCH WAS FOUND
                DECB                        ;  DECREMENT COUNTER
                BNE         SEE28           ; LOOP UNTIL ALL ACTIONS CHECKED
                JMP         LB277           ; 'SYNTAX' ERROR IF ILLEGAL ACTION DESIRED
SEE34           STU         VD5             ; SAVE THE ACTION ADDRESS
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC'S INPUT LINE
SEE38           JSR         >SE0CB          ; PUT THE HBUFF BLOCK INTO LOGICAL ADDRESS SPACE OF TASK REGISTER 1
                JSR         SELTASK1        ; SELECT TASK REGISTER 1
                LDB         VD3             ; GET THE BUFFER NUMBER
                JSR         >SEF18          ; GET THE START AND END OF THIS BUFFER'S DATA
                LDD         HORBEG          ; GET THE STARING HORIZONTAL COORDINATE
                CMPD        HOREND          ; COMPARE IT TO THE ENDING COORDINATE
                BLE         SEE50           ; BRANCH IF START <= END COORDINATE
                LDX         HOREND          ; GET THE ENDING COORDINATE
                STX         HORBEG          ; SAVE IT AS THE STARTING COORDINATE
                STD         HOREND          ; NOW SAVE THE STARTING COORDINATE AS THE ENDING COORDINATE
SEE50           LDD         VERBEG          ; GET THE VERTICAL STARTING COORDINATE
                CMPD        VEREND          ; COMPARE IT TO THE ENDING COORDINATE
                BLE         SEE5D           ; BRANCH IF START <= END COORDINATE
                LDX         VEREND          ; GET THE ENDING COORDINATE
                STX         VERBEG          ; SAVE IT AS THE STARTING COORDINATE
                STD         VEREND          ; NOW SAVE THE STARTING COORDINATE AS THE ENDING COORDINATE
; ROUND OFF THE HORIZONTAL START AND END COORDINATES TO AN EVEN NUMBER OF BYTES
SEE5D           LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                LDB         #$F8            ; ROUND OFF MASK FOR HSCREEN 3 (EIGHT PIXELS PER BYTE)
                CMPA        #$03            ; HSCREEN 3?
                BEQ         SEE6D
                LDB         #$FC            ; ROUND OFF MASK FOR HSCREEN 1 OR 4 (FOUR PIXELS PER BYTE)
                CMPA        #$02            ; HSCREEN 2?
                BNE         SEE6D           ; NO IT'S HSCREEN 1 OR 4
                LDB         #$FE            ; ROUND OFF MASK FOR HSCREEN 2 (TWO PIXELS PER BYTE)
SEE6D           TFR         B,A             ; SAVE MASK IN BOTH ACCA AND ACCB
                ANDA        HORBEG+1        ; ROUND OFF HORIZONTAL START COORDINATE
                STA         HORBEG+1        ; SAVE NEW START COORDINATE
                ANDB        HOREND+1        ; ROUND OFF HORIZONTAL END COORDINATE
                STB         HOREND+1        ; SAVE NEW END COORDINATE
                JSR         >SE9DB          ; CALCULATE THE DIFFERENCE BETWEEN THE HORIZONTAL START AND END
                STD         HOREND          ; SAVE THE HORIZONTAL DIFFERENCE
                JSR         >SE9CD          ; CALCULATE THE DIFFERENCE BETWEEN THE VERTICAL START AND END
                ADDD        #1              ; ADD ONE TO THE VERTICAL DIFFERENCE (INCLUSIVE START AND END)
                STD         VEREND          ; SAVE THE VERTICAL DIFFERENCE
; CONVERT THE HORIZONTAL DIFFERENCE (IN PIXELS) INTO A BYTE DIFFERENCE
                LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                CMPA        #$02            ; HSCREEN 2?
                BEQ         SEE96           ; YES; DIVIDE PIXEL COUNT BY TWO (TWO PIXELS PER BYTE)
                CMPA        #$03            ; HSCREEN 3?
                BNE         SEE92           ; NO; DIVIDE PIXEL COUNT BY FOUR (FOUR PIXELS PER BYTE)
                LSR         HOREND          ; HSCREEN 3; DIVIDE PIXEL COUNT BY EIGHT (EIGHT PIXELS PER BYTE)
                ROR         HOREND+1        ; DIVIDE THE HORIZONTAL DIFFERENCE BY 2
SEE92           LSR         HOREND
                ROR         HOREND+1        ; DIVIDE THE HORIZONTAL DIFFERENCE BY 2
SEE96           LSR         HOREND
                ROR         HOREND+1        ; DIVIDE THE HORIZONTAL DIFFERENCE BY 2
                LDD         HOREND
                ADDD        #1              ; ADD ONE TO THE HORIZONTAL DIFFERENCE (INCLUSIVE START AND END)
                STD         HOREND          ; SAVE THE HORIZONTAL DIFFERENCE
                JSR         HCALPOS         ; POINT X TO THE FIRST BYTE TO MOVE
                LDY         VD5             ; POINT Y TO THE ACTION ADDRESS
SEEA7           LDB         HOREND+1        ; GET THE LS BYTE OF HORIZONTAL DIFFERENCE
                PSHS        X               ; SAVE THE MOVEMENT POINTER
SEEAB           JSR         ,Y              ; PERFORM THE APPROPRIATE MOVEMENT ACTION
                DECB                        ;  DECREMENT THE HORIZONTAL MOVEMENT COUNTER
                BNE         SEEAB           ; LOOP UNTIL ALL BYTES ON THIS ROW MOVED
                PULS        X               ; RESTORE THE MOVEMENT POINTER
                JSR         >SEA45          ; MOVE THE MOVEMENT POINTER DOWN ONE ROW
                DEC         VEREND+1        ; DECREMENT THE VERTICAL DIFFERENCE (ROW COUNTER)
                BNE         SEEA7           ; LOOP UNTIL ALL ROWS MOVED
                JSR         SELTASK0        ; SELECT TASK REGISTER 0 AS THE ACTIVE TASK
                JSR         SETMMU          ; SET UP THE MMU REGISTERS
                RTS         WHY             ; NOT MAKE THE JSR ABOVE A JMP
; HGET'S BYTE MOVEMENT ROUTINE
SEEC0           LDA         ,X+             ; GET A BBYTE FROM THE HI-RES SCREEN
                BSR         SEEC7           ; POINT U TO PROPER BUFFER LOCATION
                STA         ,U              ; SAVE THE BYTE IN THE BUFFER
                RTS
SEEC7           LDU         VCF             ; GET THE BUFFER POINTER
                LEAU        $01,U           ; BUMP IT UP BY ONE
                STU         VCF             ; SAVE IT
                CMPU        VD1             ; COMPARE THE NEW POINTER TO THE END OF THE BUFFER SPACE
                BHI         SEED3           ; 'FC' FUNCTION CALL ERROR IF PAST THE END OF THE BUFFER
                RTS
SEED3           LDS         #TMPSTACK-2     ; RESET THE TEMPORARY STACK POINTER
                JSR         SELTASK0        ; SELECT TASK REGISTER 0 AS THE ACTIVE TASK
                JSR         SETMMU          ; SET UP THE MMU REGISTERS
                JMP         LB44A           ; ILLEGAL FUNCTION CALL ERROR
SEEE0           FDB         SEEEF           ; ADDRESS OF PSET ACTION ROUTINE
                FCB         $BD             ; TOKEN FOR PSET
                FDB         SEEF6           ; ADDRESS OF PRESET ACTION ROUTINE
                FCB         $BE             ; TOKEN FOR PRESET
                FDB         SEF07           ; ADDRESS OF OR ACTION ROUTINE
                FCB         $B1             ; TOKEN FOR OR
                FDB         SEEFE           ; ADDRESS OF AND ACTION ROUTINE
                FCB         $B0             ; TOKEN FOR AND
                FDB         SEF10           ; ADDRESS OF NOT ACTION ROUTINE
                FCB         $A8             ; TOKEN FOR NOT
; HPUT'S MOVEMENT ROUTINES
; PSET (DEFAULT ROUTINE)
SEEEF           BSR         SEEC7           ; POINT U TO THE PROPER BUFFER LOCATION
                LDA         ,U              ; GET A BYTE FROM THE BUFFER
                STA         ,X+             ; PUT IT BACK ON THE SCREEN
                RTS
; PRESET
SEEF6           BSR         SEEC7           ; POINT U TO THE PROPER BUFFER LOCATION
                LDA         ,U              ; GET A BYTE FROM THE BUFFER
                COMA                        ;
                STA         ,X+             ; PUT IT BACK ON THE SCREEN
                RTS
; AND
SEEFE           BSR         SEEC7           ; POINT U TO THE PROPER BUFFER LOCATION
                LDA         ,U              ; GET A BYTE FROM THE BUFFER
                ANDA        ,X              ; 'AND' IT WITH THE SCREEN DATA
                STA         ,X+             ; PUT IT BACK ON THE SCREEN
                RTS
; OR
SEF07           BSR         SEEC7           ; POINT U TO THE PROPER BUFFER LOCATION
                LDA         ,U              ; GET A BYTE FROM THE BUFFER
                ORA         ,X              ; 'OR' IT WITH THE SCREEN DATA
                STA         ,X+             ; PUT IT BACK ON THE SCREEN
                RTS
; NOT
SEF10           BSR         SEEC7           ; POINT U TO THE PROPER BUFFER LOCATION
; THIS IS A MAJOR BUG - SHOULD BE LDA ,U
                LDA         ,X              ; GET A BYTE FROM THE SCREEN, SHOULD BE FROM THE BUFFER
                COMA                        ;  COMPLEMENT THE BYTE
                STA         ,X+             ; PUT IT BACK ON THE SCREEN
                RTS
SEF18           LDY         #HRESBUFF       ; POINT Y TO THE START OF THE BUFFER SPACE
                LDA         ,Y              ; GET THE FIRST BYTE
                CMPA        #$FF            ; ARE ANY BUFFERS ACTIVE?
                BNE         SEF2C           ; YES, SEARCH FOR THE CORRECT BUFFER
                JMP         >SEED3          ; 'FC' ERROR IF NO BUFFERS ACTIVE
SEF25           LDY         ,Y              ; SKIP TO NEXT BUFFER
                LBEQ        SEED3           ; 'FC' ERROR IF THERE ARE NO MORE ACTIVE BUFFERS
SEF2C           CMPB        $02,Y           ; COMPARE THE DESIRED BUFFER TO THE CURRENT BUFFER NUMBER
                BNE         SEF25           ; NO, MATCH, CHECK THE NEXT BUFFER
                LDD         $03,Y           ; GET THE SIZE OF THE SELECTED BUFFER
                LEAY        $04,Y           ; SKIP TO ONE BYTE BEFORE THE START BUFFER DATA
                STY         VCF             ; SAVE THE START OF THE BUFFER DATA
                LEAY        $01,Y           ; MOVE TO THE ACTUAL START OF DATA
                LEAY        D,Y             ; ADD IN THE SIZE OF THE DATA
                STY         VD1             ; SAVE THE ADDRESS OF THE END OF THE DATA
                RTS
; HPRINT
HPRINT          TST         HRMODE          ; CHECK THE HI-RES GRAPHICS MODE
                LBEQ        SE6EF           ; 'HR' ERROR IF NOT HI-RES GRAPHICS MODE
                LBRN        RAMLINK         ; RAM HOOK
                JSR         LB26A           ; SYNTAX CHECK FOR '('
                JSR         >SE7B2          ; EVALUATE HORIZONTAL AND VERTICAL COORDINATE
                JSR         LB267           ; SYNTAX CHECK FOR ')'
                JSR         >SYNCOMMA       ; SYNTAX CECK FOR COMMA
                JSR         LB156           ; EVALUATE EXPRESSION
                TST         VALTYP          ; CHECK THE TYPE OF VARIABLE EVALUATED
                BNE         SEF62           ; BRANCH IF NOT NUMERIC - REALLY SHOULD BE BMI
                JSR         LBDD9           ; CONVERT FLOATING POINT NUMBER INTO A STRING
                JSR         LB516           ; SAVE THE STRING IN STRING SPACE
SEF62           JSR         LB657           ; CALCULATE THE LENGTH AND ADDRESS OF THE STRING
                STB         >H.PCOUNT       ; SAVE THE LENGTH OF THE STRING
                LDY         #H.PBUF         ; POINT TO THE HPRINT BUFFER
SEF6C           DECB                        ;  DECREMENT THE CHARACTER COUNT
                BMI         SEF75           ; BRANCH IF ALL CHARACTERS PRINTED
                LDA         ,X+             ; GET A CHARACTER FROM THE STRING
                STA         ,Y+             ; SAVE IT IN THE HPRINT BUFFER
                BRA         SEF6C           ; KEEP GOING UNTIL DONE
SEF75           LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                LDB         #40             ; 40 CHARACTERS MAX IN THE 320 PIXEL WIDE MODE
                CMPA        #$03            ; CHECK THE HSCREEN MODE
                BCS         SEF7F           ; BRANCH IF 40 COLUMN RESOLUTION
                LDB         #80             ; 80 CHARACTERS MAX IN THE 640 PIXEL WIDE MODE
SEF7F           CLRA                        ;  CLEAR THE MOST SIGNIFICANT BYTE OF ACCD
                SUBD        HORBEG          ; SUBTRACT THE HORIZONTAL PRINT POSITION
                BMI         SF001           ; EXIT IF HORIZONTAL PRINT POSITION > LINE LENGTH
                CMPB        >H.PCOUNT       ; IS THE PRINT CHARACTER COUNT > LINE LENGTH?
                BHI         SEF8E           ; BRANCH IF NOT
                STB         >H.PCOUNT       ; FORCE THE PRINT CHARACTER COUNT TO EQUAL THE LINE LENGTH
                BEQ         SF001           ; EXIT IF LINE LENGTH = 0
SEF8E           LDA         #ROWMAX-1       ; GET THE HIGHEST POSSIBLE ROW NUMBER
                CMPA        VERBEG+1        ; AND COMPARE IT TO THE PRINT ROW
                BGE         SEF96           ; BRANCH IF PRINTING ON A LEGAL ROW NUMBER
                STA         VERBEG+1        ; PRINT ON BOTTOM ROW (HIGHEST NUMBER) IF ILLEGAL ROW SPECIFIED
SEF96           JSR         >SF08C          ; ADJUST ROW AND COLUMN NUMBERS FOR PRINTING ON HI-RES SCREEN
                JSR         HCALPOS         ; POINT X TO THE SCREEN ADDRESS; ACCA = PIXEL MASK
                LDY         #H.PBUF         ; POINT TO THE HPRINT BUFFER
                LDB         >H.PCOUNT       ; GET THE NUMBER OF CHARACTERS IN THE PRINT BUFFER
SEFA3           LDA         ,Y              ; GET A CHARACTER FROM THE PRINT BUFFER
                ANDA        #$7F            ; MASK OFF THE GRAPHICS BIT (BIT 7)
                SUBA        #$20            ; SUBTRACT OUT THE CONTROL CODES
                BPL         SEFAD           ; BRANCH IF IT WAS NOT A CONTROL CODE
                LDA         #$00            ; FORCE A CONTROL CODE TO PRINT A BLANK
SEFAD           STA         ,Y+             ; PUT THE 'MASSAGED' CHARACTER BACK INTO THE BUFFER
                DECB                        ;  BUMP CHARACTER DOWN ONE
                BGT         SEFA3           ; LOOP UNTIL ALL CHARACTERS DONE
                LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                DECA                        ;  CONVERT 1-4 TO 0-3
                ASLA                        ;  MULTIPLY BY TWO - THE LOOKUP TABLE HAS TWO BYTES/ENTRY
                LDY         #SF002          ; POINT TO THE CHARACTER PRINT ROUTINE ADDRESS TABLE
                LDY         A,Y             ; GET THE ADDRESS OF THE CHARACTER PRINT ROUTINE
                STY         VD1             ; AND SAVE IT IN VD1
; THIS SECTION OF CODE WILL PRINT THE BUFFER TO THE HI-RES SCREEN
                LDA         #$08            ; 8 ROWS PER HI-RES CHARACTER
                STA         VD3             ; TEMP SAVE THE ROW COUNTER
                LDY         #H.PBUF         ; POINT TO THE PRINT BUFFER
                LDU         #SF09D          ; POINT TO THE HI-RES CHARACTER GENERATOR 'ROM'
                LDB         >H.FCOLOR       ; GET THE FOREGROUND COLOR
                JSR         PIXELFIL        ; FILL ACCB WITH ALL FOREGROUND COLOR PIXELS
                STB         ALLCOL          ; SAVE THE PIXEL-FILLED BYTE
                JSR         SELTASK1        ; SWITCH IN TASK REGISTER 1
                LDA         >H.PCOUNT       ; GET THE CHARACTER COUNT
SEFD9           PSHS        Y,X,A
SEFDB           LDB         ,Y+             ; GET A CHARACTER FROM THE PRINT BUFFER
                CLRA                        ;  CLEAR THE MOST SIGNIFICANT BYTE OF ACCD
                ASLB                        ;
                ASLB                        ;
                ROLA                        ;
                ASLB                        ;
                ROLA                        ;  MULTIPLY ACCD BY 8 - 8 BYTES PER CHARACTER
                LDA         D,U             ; GET THE FIRST BYTE OF THE GRAPHIC CHARACTER FROM THE 'ROM'
                JSR         [VD1]           ; PUT THE BYTE ON THE HI-RES SCREEN
                DEC         >H.PCOUNT       ; DECREMENT CHARACTER COUNT
                BGT         SEFDB           ; KEEP GOING UNTIL ALL CHARACTERS DONE
                PULS        A,X,Y           ; RESTORE THE PRINT BUFFER POINTER, 'ROM' POINTER & CHAR COUNT
                DEC         VD3             ; BUMP THE ROW COUNTER DOWN ONE
                BEQ         SEFFE           ; BRANCH IF ALL ROWS DONE
                STA         >H.PCOUNT       ; RESET THE CHARACTER COUNT
                LEAU        $01,U           ; ADJUST THE 'ROM' POINTER DOWN ONE ROW
                JSR         >SEA45          ; MOVE X DONW ONE HI-RES ROW
                BRA         SEFD9           ; KEEP LOOPING UNTIL THE WHOLE ROW IS DONE
SEFFE           JSR         SELTASK0        ; MAKE TASK REGISTER 0 ACTIVE
SF001           RTS
; TABLE OF ADDRESSES OF HI-RES PRINT DRIVERS
SF002           FDB         SF01A           ; MODE 1
                FDB         SF045           ; MODE 2
                FDB         SF00A           ; MODE 3
                FDB         SF01A           ; MODE 4
; MODE 3 PRINT DRIVER
SF00A           PSHS        A               ; SAVE THE CHARACTER MASK
                COMA                        ;  INVERT THE MASK
                ANDA        ,X              ; 'AMD' IT WITH THE SCREEN DATA - CREATE A HOLE FOR THE CHARACTER
                STA         ,X              ; DATA AND THEN PUT IT BACK ON THE SCREEN
                PULS        A               ; GET THE CHARACTER MASK BACK
                ANDA        ALLCOL          ; 'AND' IT WITH THE PIXEL COLOR BYTE - 'COLOR' THE DATA
                ORA         ,X              ; 'OR' IT WITH THE SCREEN DATA - FILL THE 'HOLE' CREATED ABOVE
                STA         ,X+             ; WITH THE 'COLORED' CHARACTER DATA AND PUT THE DATA ON THE SCREEN
                RTS
; MODES 1,4 PRINT DRIVER
SF01A           PSHS        Y               ; SAVE THE PRINT BUFFER POINTER
                LDY         #SF035          ; POINT TO THE TABLE OF 4 COLOR PIXEL MASKS
                TFR         A,B             ; COPY CHARACTER DATA TO ACCB
                LSRA                        ;
                LSRA                        ;
                LSRA                        ;
                LSRA                        ;  SHIFT THE HIGH ORDER NIBBLE TO THE LOW ORDER NIBBLE
                LDA         A,Y             ; GET THE PIXEL MASK FOR THE HIGH NIBBLE
                JSR         >SF00A          ; DISPLAY THE HIGH ORDER NIBBLE DATA ON THE SCREEN
                ANDB        #$0F            ; MASK OFF THE HIGH ORDER NIBBLE
                LDA         B,Y             ; GET THE PIXEL MASK FOR THE LOW NIBBLE
                JSR         >SF00A          ; DISPLAY THE LOW ORDER NIBBLE DATA ON THE SCREEN
                PULS        Y               ; RESTORE THE PRINT BUFFER POINTER
                RTS
; FOUR COLOR PIXEL MASKS
SF035           FCB         $00,$03,$0C,$0F,$30,$33 ; 4 COLOR PIXEL MASKS
                FCB         $3C,$3F,$C0,$C3,$CC,$CF
                FCB         $F0,$F3,$FC,$FF
; MODE 2 PRINT DRIVER
SF045           PSHS        Y,A             ; SAVE THE PRINT BUFFER POINTER AND THE CHARACTER DATA
                LDY         #SF06C          ; POINT TO THE TABLE OF 16 COLOR MASKS
                LSRA                        ;
                LSRA                        ;
                LSRA                        ;
                LSRA                        ;  SHIFT THE HIGH ORDER PIXEL TO BITS 0-3
                ASLA                        ;  MULTIPLY BY 2, THERE ARE 2 BYTES PER TABLE ENTRY
                LDD         A,Y             ; GET THE FIRST FOUR PIXEL MASKS FROM THE TABLE
                JSR         >SF00A          ; DISPLAY THE FIRST TWO PIXELS
                TFR         B,A             ; PUT THE NEXT TWO PIXELS' DATA INTO ACCA
                JSR         >SF00A          ; DISPLAY THE NEXT TWO PIXELS
                PULS        A               ; GET THE CHARACTER DATA BACK
                ANDA        #$0F            ; MASK OFF THE HIGH NIBBLE
                ASLA                        ;  MULTIPLY BY 2, THERE ARE 2 BYTES PER TABLE ENTRY
                LDD         A,Y             ; GET THE LAST FOUR PIXEL MASKS FROM THE TABLE
                JSR         >SF00A          ; DISPLAY THE NEXT TWO PIXELS
                TFR         B,A             ; PUT THE NEXT TWO PIXELS' DATA INTO ACCA
                JSR         >SF00A          ; DISPLAY THE LAST TWO PIXELS
                PULS        Y               ; RESTORE THE PRINT BUFFER POINTER
                RTS         WASTED;         ; THIS AND ABOVE INSTRUCTION SHOULD BE PULS Y,PC
; 16 COLOR PIXEL MASKS - DOUBLE BYTE WIDE
SF06C           FDB         $0000,$000F,$00F0
                FDB         $00FF,$0F00,$0F0F
                FDB         $0FF0,$0FFF,$F000
                FDB         $F00F,$F0F0,$F0FF
                FDB         $FF00,$FF0F,$FFF0
                FDB         $FFFF
; CONVERT THE PRINT POSITION FROM CHARACTER ROWS AND COLUMNS TO PIXEL ROWS
; AND COLUMNS; EACH CHARACTER IS 8 PIXELS WIDE AND 8 PIXELS DEEP.
SF08C           LDD         HORBEG          ; GET THE PRINT COLUMN POSITION
                ASLB                        ;
                ASLB                        ;
                ROLA                        ;
                ASLB                        ;
                ROLA                        ;  SHIFT ACCD LEFT THREE TIMES MULTIPLY COLUMN POSITION BY EIGHT
                STD         HORBEG          ; SAVE NEW COLUMN POSITION IN TERMS OF PIXELS (8 PIXELS/CHARACTER)
                LDA         VERBEG+1        ; GET THE PRINT ROW NUMBER
                ASLA                        ;
                ASLA                        ;
                ASLA                        ;  SHIFT ACCA LEFT THREE TIMES MULTIPLY ROW POSITION BY EIGHT
                STA         VERBEG+1        ; SAVE NEW ROW POSITION IN TERMS OF PIXELS (8 PIXELS/CHARACTER)
                RTS
; HI-RES CHARACTER GENERATOR 'ROM'
; SPECIAL CHARACTERS AND NUMBERS
SF09D           FCB         $00,$00,$00,$00,$00,$00 ; BLANK
                FCB         $00,$00
                FCB         $10,$10,$10,$10,$10,$00 ; !
                FCB         $10,$00
                FCB         $28,$28,$28,$00,$00,$00 ; "
                FCB         $00,$00
                FCB         $28,$28,$7C,$28,$7C,$28 ; #
                FCB         $28,$00
                FCB         $10,$3C,$50,$38,$14,$78 ; $
                FCB         $10,$00
                FCB         $60,$64,$08,$10,$20,$4C ; %
                FCB         $0C,$00
                FCB         $20,$50,$50,$20,$54,$48 ; &
                FCB         $34,$00
                FCB         $10,$10,$20,$00,$00,$00 ; '
                FCB         $00,$00
                FCB         $08,$10,$20,$20,$20,$10 ; (
                FCB         $08,$00
                FCB         $20,$10,$08,$08,$08,$10 ; )
                FCB         $20,$00
                FCB         $00,$10,$54,$38,$38,$54 ;
                FCB         $10,$00
                FCB         $00,$10,$10,$7C,$10,$10 ; +
                FCB         $00,$00
                FCB         $00,$00,$00,$00,$00,$10 ; ,
                FCB         $10,$20
                FCB         $00,$00,$00,$7C,$00,$00 ; -
                FCB         $00,$00
                FCB         $00,$00,$00,$00,$00,$00 ; .
                FCB         $10,$00
                FCB         $00,$04,$08,$10,$20,$40 ; /
                FCB         $00,$00
                FCB         $38,$44,$4C,$54,$64,$44 ; 0
                FCB         $38,$00
                FCB         $10,$30,$10,$10,$10,$10 ; 1
                FCB         $38,$00
                FCB         $38,$44,$04,$38,$40,$40 ; 2
                FCB         $7C,$00
                FCB         $38,$44,$04,$08,$04,$44 ; 3
                FCB         $38,$00
                FCB         $08,$18,$28,$48,$7C,$08 ; 4
                FCB         $08,$00
                FCB         $7C,$40,$78,$04,$04,$44 ; 5
                FCB         $38,$00
                FCB         $38,$40,$40,$78,$44,$44 ; 6
                FCB         $38,$00
                FCB         $7C,$04,$08,$10,$20,$40 ; 7
                FCB         $40,$00
                FCB         $38,$44,$44,$38,$44,$44 ; 8
                FCB         $38,$00
                FCB         $38,$44,$44,$38,$04,$04 ; 9
                FCB         $38,$00
                FCB         $00,$00,$10,$00,$00,$10 ; :
                FCB         $00,$00
                FCB         $00,$00,$10,$00,$00,$10 ;
                FCB         $10,$20
                FCB         $08,$10,$20,$40,$20,$10 ; >
                FCB         $08,$00
                FCB         $00,$00,$7C,$00,$7C,$00 ;
                FCB         $00,$00
                FCB         $20,$10,$08,$04,$08,$10 ; <
                FCB         $20,$00
                FCB         $38,$44,$04,$08,$10,$00 ; ?
                FCB         $10,$00
; UPPER CASE CHARACTERS
                FCB         $38,$44,$04,$34,$4C,$4C ; @
                FCB         $38,$00
                FCB         $10,$28,$44,$44,$7C,$44 ; A
                FCB         $44,$00
                FCB         $78,$24,$24,$38,$24,$24 ; B
                FCB         $78,$00
                FCB         $38,$44,$40,$40,$40,$44 ; C
                FCB         $38,$00
                FCB         $78,$24,$24,$24,$24,$24 ; D
                FCB         $78,$00
                FCB         $7C,$40,$40,$70,$40,$40 ; E
                FCB         $7C,$00
                FCB         $7C,$40,$40,$70,$40,$40 ; F
                FCB         $40,$00
                FCB         $38,$44,$40,$40,$4C,$44 ; G
                FCB         $38,$00
                FCB         $44,$44,$44,$7C,$44,$44 ; H
                FCB         $44,$00
                FCB         $38,$10,$10,$10,$10,$10 ; I
                FCB         $38,$00
                FCB         $04,$04,$04,$04,$04,$44 ; J
                FCB         $38,$00
                FCB         $44,$48,$50,$60,$50,$48 ; K
                FCB         $44,$00
                FCB         $40,$40,$40,$40,$40,$40 ; L
                FCB         $7C,$00
                FCB         $44,$6C,$54,$54,$44,$44 ; M
                FCB         $44,$00
                FCB         $44,$44,$64,$54,$4C,$44 ; N
                FCB         $44,$00
                FCB         $38,$44,$44,$44,$44,$44 ; O
                FCB         $38,$00
                FCB         $78,$44,$44,$78,$40,$40 ; P
                FCB         $40,$00
                FCB         $38,$44,$44,$44,$54,$48 ; Q
                FCB         $34,$00
                FCB         $78,$44,$44,$78,$50,$48 ; R
                FCB         $44,$00
                FCB         $38,$44,$40,$38,$04,$44 ; S
                FCB         $38,$00
                FCB         $7C,$10,$10,$10,$10,$10 ; T
                FCB         $10,$00
                FCB         $44,$44,$44,$44,$44,$44 ; U
                FCB         $38,$00
                FCB         $44,$44,$44,$28,$28,$10 ; V
                FCB         $10,$00
                FCB         $44,$44,$44,$44,$54,$6C ; W
                FCB         $44,$00
                FCB         $44,$44,$28,$10,$28,$44 ; X
                FCB         $44,$00
                FCB         $44,$44,$28,$10,$10,$10 ; Y
                FCB         $10,$00
                FCB         $7C,$04,$08,$10,$20,$40 ; Z
                FCB         $7C,$00
                FCB         $38,$20,$20,$20,$20,$20 ; ]
                FCB         $38,$00
                FCB         $00,$40,$20,$10,$08,$04 ; \
                FCB         $00,$00
                FCB         $38,$08,$08,$08,$08,$08 ; [
                FCB         $38,$00
                FCB         $10,$38,$54,$10,$10,$10 ; UP ARROW
                FCB         $10,$00
                FCB         $00,$10,$20,$7C,$20,$10 ; LEFT ARROW
                FCB         $00,$00
; LOWER CASE CHARACTERS
                FCB         $10,$28,$44,$00,$00,$00 ; ^
                FCB         $00,$00
                FCB         $00,$00,$38,$04,$3C,$44 ; a
                FCB         $3C,$00
                FCB         $40,$40,$58,$64,$44,$64 ; b
                FCB         $58,$00
                FCB         $00,$00,$38,$44,$40,$44 ; c
                FCB         $38,$00
                FCB         $04,$04,$34,$4C,$44,$4C ; d
                FCB         $34,$00
                FCB         $00,$00,$38,$44,$7C,$40 ; e
                FCB         $38,$00
                FCB         $08,$14,$10,$38,$10,$10 ; f
                FCB         $10,$00
                FCB         $00,$00,$34,$4C,$4C,$34 ; g
                FCB         $04,$38
                FCB         $40,$40,$58,$64,$44,$44 ; h
                FCB         $44,$00
                FCB         $00,$10,$00,$30,$10,$10 ; i
                FCB         $38,$00
                FCB         $00,$04,$00,$04,$04,$04 ; j
                FCB         $44,$38
                FCB         $40,$40,$48,$50,$60,$50 ; k
                FCB         $48,$00
                FCB         $30,$10,$10,$10,$10,$10 ; l
                FCB         $38,$00
                FCB         $00,$00,$68,$54,$54,$54 ; m
                FCB         $54,$00
                FCB         $00,$00,$58,$64,$44,$44 ; n
                FCB         $44,$00
                FCB         $00,$00,$38,$44,$44,$44 ; o
                FCB         $38,$00
                FCB         $00,$00,$78,$44,$44,$78 ; p
                FCB         $40,$40
                FCB         $00,$00,$3C,$44,$44,$3C ; q
                FCB         $04,$04
                FCB         $00,$00,$58,$64,$40,$40 ; r
                FCB         $40,$00
                FCB         $00,$00,$3C,$40,$38,$04 ; s
                FCB         $78,$00
                FCB         $20,$20,$70,$20,$20,$24 ; t
                FCB         $18,$00
                FCB         $00,$00,$44,$44,$44,$4C ; u
                FCB         $34,$00
                FCB         $00,$00,$44,$44,$44,$28 ; v
                FCB         $10,$00
                FCB         $00,$00,$44,$54,$54,$28 ; w
                FCB         $28,$00
                FCB         $00,$00,$44,$28,$10,$28 ; x
                FCB         $44,$00
                FCB         $00,$00,$44,$44,$44,$3C ; y
                FCB         $04,$38
                FCB         $00,$00,$7C,$08,$10,$20 ; z
                FCB         $7C,$00
                FCB         $08,$10,$10,$20,$10,$10 ; {
                FCB         $08,$00
                FCB         $10,$10,$10,$00,$10,$10 ; |
                FCB         $10,$00
                FCB         $20,$10,$10,$08,$10,$10 ; }
                FCB         $20,$00
                FCB         $20,$54,$08,$00,$00,$00 ; ~
                FCB         $00,$00
                FCB         $00,$00,$00,$00,$00,$00 ; underline
                FCB         $7C,$00
; HDRAW
HDRAW           TST         HRMODE          ; CHECK HI-RES GRAPHICS MODE
                LBEQ        SE6EF           ; 'HR' ERROR IF HI-RES MODE NOT ENABLED
                LBRN        RAMLINK         ; RAM HOOK
                LDX         #0              ; X=0, ACCB=1; END OF DRAW COMMAND LINE VALUES
                LDB         #$01            ; WHEN THESE VALUES ARE PULLED OFF THE STACK,
                PSHS        X,B             ; THE DRAW COMMAND WILL END
                STB         SETFLG          ; SET PSET/PRESET FLAG TO PSET
                STX         VD5             ; CLEAR UPDATE AND DRAW FLAGS
                JSR         >SE731          ; SET ACTIVE COLOR BYTE
                JSR         LB156           ; EVALUATE EXPRESSION
SF3B8           JSR         LB654           ; GET LENGTH AND ADDRESS OF COMMAND STRING
                BRA         SF3C5           ; INTERPRET THE COMMAND STRING
SF3BD           JSR         >SF591          ; GET THE NEXT CHARACTER FROM THE COMMAND LINE
                JMP         >SF5A7          ; EVALUATE A DECIMAL VALUE IN COMMAND LINE
SF3C3           PULS        B,X             ; GET NEXT COMMAND LINE TO BE INTERPRETED FROM THE STACK
SF3C5           STB         VD8             ; SET COMMAND LENGTH COUNTER
                BEQ         SF3C3           ; GET NEW COMMAND LINE IF ZERO
                STX         VD9             ; SET COMMAND LINE ADDRESS
                LBEQ        SF4D0           ; EXIT ROUTINE IF ADDRESS = 0
SF3CF           TST         VD8             ; TEST COMMAND LENGTH COUNTER
                BEQ         SF3C3           ; GET NEW LINE IF 0
                JSR         >SF591          ; GET A COMMAND CHARACTER
                CMPA        #';             ; CHECK FOR A SEMI-COLON
                BEQ         SF3CF           ; IGNORE SEMI-COLONS
                CMPA        #''             ; CHECK FOR APOSTROPHE
                BEQ         SF3CF           ; IGNORE APOSTROPHE
                CMPA        #'N             ; UPDATE CHECK?
                BNE         SF3E6           ; BRANCH IF NOT
                COM         VD5             ; TOGGLE UPDATE FLAG; 0 = UPDATE, FF = NO UPDATE
                BRA         SF3CF           ; GET NEXT COMMAND
SF3E6           CMPA        #'B             ; CHECK DRAW FLAG?
                BNE         SF3EE           ; BRANCH IF NOT
                COM         VD6             ; TOGGLE DRAW FLAG; 0 = DRAW LINE, FF = DON'T DRAW LINE
                BRA         SF3CF           ; GET ENXT COMMAND
SF3EE           CMPA        #'X             ; SUBSTRING?
                LBEQ        SF4A1           ; GO EXECUTE A COMMAND SUBSTRING
                CMPA        #'M             ; MOVE THE DRAW POSITION?
                LBEQ        SF54C           ; BRANCH IF YES, GO MOVE IT
                PSHS        A               ; SAVE CURRENT COMMAND
                LDB         #$01            ; DEFAULT VALUE IF NO NUMBER FOLLOWS COMMAND
                CLR         VD3             ; CLEAR MS BYTE OF SUBCOMMAND VALUE
                STB         VD4             ; SAVE LS BYTE OF SUBCOMMAND VALUE
                TST         VD8             ; CHECK COMMAND LENGTH COUNTER
                BEQ         SF417           ; BRANCH IF NO COMMANDS LEFT
                JSR         >SF591          ; GET A COMMAND CHARACTER
                JSR         LB3A2           ; SET CARRY IF NOT ALPHA
                PSHS        CC              ; SAVE CARRY FLAG
                JSR         >SF5F2          ; MOVE COMMAND POINTER BACK ONE
                PULS        CC              ; RESTORE CARRY FLAG
                BCC         SF417           ; BRANCH IF NEXT COMMAND IS ALPHA
                BSR         SF3BD           ; EVALUATE DECIMAL COMMAND LINE VALUE - RETURN VALUE IN ACCD & VD3
SF417           PULS        A               ; GET CURRENT COMMAND BACK
                CMPA        #'C             ; CHANGE COLOR?
                BEQ         SF445           ; BRANCH IF YES
                CMPA        #'A             ; CHANGE ANGLE?
                BEQ         SF451           ; BRANCH IF YES
                CMPA        #'S             ; CHANGE SCALE?
                BEQ         SF45C           ; BRANCH IF YES
                CMPA        #'U             ; GO UP?
                BEQ         SF496           ; BRANCH IF YES
                CMPA        #'D             ; GO DOWN?
                BEQ         SF492           ; BRANCH IF YES
                CMPA        #'L             ; GO LEFT?
                BEQ         SF48C           ; BRANCH IF YES
                CMPA        #'R             ; GO RIGHT?
                BEQ         SF485           ; BRANCH IF YES
                SUBA        #'E             ; MASK OFF ASCII FOR LETTER E-H COMMAND CHECKS
                BEQ         SF473           ; BRANCH IF E (45 DEGREES)
                DECA                        ;  CHECK FOR F
                BEQ         SF46D           ; BRANCH IF F (135 DEGREES)
                DECA                        ;  CHECK FOR G
                BEQ         SF47D           ; BRANCH IF G (225 DEGREES)
                DECA                        ;  CHECK FOR H
                BEQ         SF467           ; BRANCH IF H (315 DEGREES)
                JMP         LB44A           ; ILLEGAL FUNCTION CALL ERROR IF ILLEGAL SUBCOMMAND
; CHANGE COLOR
SF445           JSR         >SE711          ; ADJUST COLOR CODE FOR PROPER GRAPHICS MODE
                STB         >H.FCOLOR       ; SAVE NEW FOREGROUND COLOR
                JSR         >SE731          ; SET UP COLOR BYTES
                LBRA        SF3CF           ; GO PROCESS ANOTHER COMMAND
; CHANGE ANGLE
SF451           CMPB        #$04            ; ONLY ANGLES 0-3 ARE LEGAL
                LBCC        LB44A           ; ILLEGAL FUNCTION CALL ERROR
                STB         ANGLE           ; SAVE DRAW ANGLE
                LBRA        SF3CF           ; GO PROCESS ANOTHER COMMAND
; CHANGE SCALE
SF45C           CMPB        #63             ; ONLY 0-63 ARE LEGAL
                LBCC        LB44A           ; ILLEGAL FUNCTION CALL ERROR
                STB         SCALE           ; SAVE DRAW SCALE
                LBRA        SF3CF           ; GO PROCESS ANOTHER COMMAND
; 315 DEGREES
SF467           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
                BSR         NEGACCD         ; MAKE HORIZONTAL DIFFERENCE NEGATIVE
                BRA         SF46F           ; BRANCH AROUND NEXT INSTRUCTION
; 135 DEGREES
SF46D           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
SF46F           TFR         D,X             ; COPY HORIZONTAL DIFFERENCE TO VERTICAL DIFFERENCE
                BRA         SF4D4           ; GO MOVE THE DRAW POSITION
; 45 DEGREES
SF473           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
                TFR         D,X             ; COPY HORIZONTAL DIFFERENCE TO VERTICAL DIFFERENCE
                BSR         NEGACCD         ; MAKE HORIZONTAL DIFFERENCE NEGATIVE
                EXG         D,X             ; SWAP HOR AND VER DIFFERENCES
                BRA         SF4D4           ; GO MOVE THE DRAW POSITION
; 225 DEGREES
SF47D           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
                TFR         D,X             ; COPY HORIZONTAL DIFFERENCE TO VERTICAL DIFFERENCE
                BSR         NEGACCD         ; MAKE HORIZONTAL DIFFERENCE NEGATIVE
                BRA         SF4D4           ; GO MOVE THE DRAW POSITION
; GO RIGHT
SF485           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
SF487           LDX         #0              ; X=0; VERT DIFFERENCE = 0
                BRA         SF4D4           ; GO MOVE THE DRAW POSITION
; GO LEFT
SF48C           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
                BSR         NEGACCD         ; MAKE HORIZONTAL DIFFERENCE NEGATIVE
                BRA         SF487           ; MAKE VERTICAL DIFFERENCE ZERO AND MOVE THE DRAW POSITION
; GO DOWN
SF492           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
                BRA         SF49A           ; MAKE VER DIFF=0, EXCHANGE HOR & VER DIFFS AND MOVE DRAW POSITION
; GO UP
SF496           LDA         VD3             ; NOW ACCD = VALUE OF THE SUBCOMMAND
                BSR         NEGACCD         ; MAKE HORIZONTAL DIFFERENCE NEGATIVE
SF49A           LDX         #0              ; X=0; HORIZONTAL DIFFERENCE = 0
                EXG         X,D             ; SWAP HOR AND VER DIFFERENCES
                BRA         SF4D4           ; GO MOVE THE DRAW POSITION
; EXECUTE A COMMAND SUB STRING
SF4A1           JSR         >SF611          ; INTERPRET CURRENT COMMAND AS IF IT WERE A BASIC VARIABLE
                LDB         #$02            ;
                JSR         LAC33           ; SEE IF AT LEAST FOUR BYTES OF FREE RAM ARE LEFT
                LDB         VD8             ; GET CURRENT COMMAND LENGTH
                LDX         VD9             ; GET CURRENT COMMAND COUNTER
                PSHS        X,B             ; SAVE THEM ON THE STACK
                JMP         >SF3B8          ; EVALUATE NUMERICAL VALUE IN COMMAND LINE
; MULTIPLY HOR OR VER DIFFERENCE BY SCALE FACTOR, DIVIDE PRODUCT BY 4 AND RETURN VALUE IN ACCD
SF4B2           LDB         SCALE           ; GET DRAW SCALE
                BEQ         SF4D1           ; BRANCH IF ZERO (DEFAULT TO FULL SCALE)
                CLRA                        ;  CLEAR THE MS BYTE
                EXG         D,X             ; SWAP DIFFERENCE AND SCALE FACTOR
                STA         ,-S             ; SAVE MS BYTE OF DIFFERENCE ON STACK (SIGN INFORMATION)
                BPL         SF4BF           ; BRANCH IF POSITIVE DIFFERENCE
                BSR         NEGACCD         ; FORCE THE DIFFERENCE TO BE A POSITIVE VALUE
SF4BF           JSR         >SEBCB          ; MULT DIFFERENCE BY SCALE FACTOR
                TFR         U,D             ; SAVE 2 MS BYTES IN ACCD
                LSRA                        ;
                RORB                        ;  DIVIDE ACCD BY 2
                LSRA                        ;
                RORB                        ;  DO IT AGAIN, EACH SCALE INCREMENT IS 1/4 FULL SCALE
                TST         ,S+             ; CHECK SIGN OF ORIGINAL DIFFERENCE
                BPL         SF4D0           ; RETURN IF IT WAS POSITIVE
; NEGATE ACCD
NEGACCD         NEGA                        ;
                NEGB                        ;
                SBCA        #$00            ; NEGATE ACCD
SF4D0           RTS
SF4D1           TFR         X,D             ; TRANSFER UNCHANGED DIFFERENCE TO ACCD
                RTS
; MOVE THE DRAW POSITION - ADD THE ORTHOGONAL DIFFERENCES IN ACCD (HORIZONTAL)
; AND X (VERTICAL) TO THE CURRENT POSITION; DRAW A LINE AFTER THE MOVE
SF4D4           PSHS        B,A             ; SAVE THE HORIZONTAL DIFFERENCE
                BSR         SF4B2           ; APPLY SCALE FACTOR TO VERTICAL
                PULS        X               ; GET HORIZONTAL DIFFERENCE
                PSHS        B,A             ; SAVE THE VERTICAL DIFFERENCE
                BSR         SF4B2           ; APPLY THE SCALE FACTOR TO HORIZONTAL
                PULS        X               ; GET THE VERTICAL DIFFERENCE
                LDY         ANGLE           ; GET DRAW ANGLE AND SCALE
                PSHS        Y               ; SAVE THEM ON THE STACK
SF4E5           TST         ,S              ; CHECK DRAW ANGLE
                BEQ         SF4F1           ; BRANCH IF NO ANGLE
                EXG         X,D             ; SWAP HORIZONTAL AND VERTICAL DIFFERENCES
                BSR         NEGACCD         ; NEGATE ACCD
                DEC         ,S              ; DECR ANGLE
                BRA         SF4E5           ; CHECK ANGLE AGAIN
SF4F1           PULS        Y               ; PULL ANGLE AND SCALE OFF OF THE STACK
                LDU         #0              ; DEFAULT HORIZONTAL END POSITION TO 0
                ADDD        HORDEF          ; ADD DIFFERENCE TO HORIZONTAL START
                BMI         SF4FC           ; GO FORCE HORIZONTAL COORD TO 0 IF RESULT IS NEGATIVE
                TFR         D,U             ; SAVE HORIZONTAL END POSITION IN U
SF4FC           TFR         X,D             ; PUT DIFFERENCE IN ACCD
                LDX         #0              ; DEFAULT THE VERTICAL END POSITION TO 0
                ADDD        VERDEF          ; ADD THE DIFFERENCE TO VERTICAL START
                BMI         SF507           ; VERTICAL COORD = 0 IF RESULT IS NEGATIVE
                TFR         D,X             ; SAVE VERTICAL POSITION IN X
; MOVE THE DRAW POSITION; ENTER WITH ABSOLUTE HORIZONTAL POSITION
; IN U REGISTER AND ABSOULTE VERTICAL POSITION IN X REGISTER.
SF507           CMPU        #640            ; COMPARE TO MAX HORIZONTAL COORDINATE
                BCS         SF510           ; BRANCH IF WITHIN RANGE
                LDU         #640-1          ; FORCE MAXIMUM VALUE IF NOT
SF510           LDA         HRMODE          ; GET HI-RES GRAPHICS MODE
                CMPA        #$02            ; SEE WHICH ONE
                BGT         SF51F           ; BRANCH IF MODE 3 OR 4
                CMPU        #320            ; MAX HORIZONTAL COORD FOR 320x192 MODES (1 AND 2)
                BCS         SF51F           ; BRANCH IF WITHIN LIMITS
                LDU         #320-1          ; FORCE TO MAXIMUM IF NOT
SF51F           CMPX        #192            ; IS VERTICAL COORD WITHIN RANGE?
                BCS         SF527           ; BRANCH IF IT IS
                LDX         #192-1          ; FORCE TO MAXIMUM IF NOT
SF527           LDD         HORDEF          ; GET LAST HORIZONTAL POSITION
                STD         HORBEG          ; MAKE IT THE HORIZONTAL START
                LDD         VERDEF          ; GET LAST VERTICAL POSITION
                STD         VERBEG          ; MAKE IT THE VERTICAL START
                STX         VEREND          ; SAVE VERTICAL END COORD
                STU         HOREND          ; SAVE HORIZONTAL END COORDINATE
                TST         VD5             ; CHECK UPDATE FLAG
                BNE         SF53B           ; BRANCH IF NO UPDATE
                STX         VERDEF          ; UPDATE VERTICAL POSITION OF DRAW POINTER
                STU         HORDEF          ; DO THE SAME WITH THE HORIZONTAL DRAW POINTER
SF53B           JSR         >SEA0D          ; NORMALIZE COORDS IN HOREND,VEREND AND HORBEG,VERBEG
                TST         VD6             ; CHECK DRAW FLAG
                BNE         SF545           ; BRANCH IF NO DRAW
                JSR         >SE94E          ; DRAWLINE FROM (HORBEG,VERBEG) TO (HOREND,VEREND)
SF545           CLR         VD5             ; RESET UPDATE FLAG
                CLR         VD6             ; RESET DRAW FLAG
                JMP         >SF3CF          ; GO GET ANOTHER COMMAND
; SET THE DRAW POSITION
SF54C           JSR         >SF591          ; GET A CHAR FROM COMMAND LINE
                PSHS        A               ; SAVE IT
                JSR         >SF578          ; EVALUATE THE HORIZONTAL DIFFERENCE
                PSHS        B,A             ; SAVE IT ON THE STACK
                JSR         >SF591          ; GET A CHAR FROM COMMAND LINE
                CMPA        #',             ; CHECK FOR COMMA
                LBNE        LB44A           ; ILLEGAL FUCNTION CALL ERROR IF NO COMMA
                JSR         >SF575          ; EVALUATE THE VERTICAL DIFFERENCE
                TFR         D,X             ; SAVE VERTICAL DIFFERENCE IN X
                PULS        U               ; GET HORIZONTAL DIFFERENCE IN U
                PULS        A               ; GET FIRST COMMAND CHARACTER
                CMPA        #'+             ; CHECK FOR PLUS
                BEQ         SF570           ; TREAT VALUES IN X AND U AS DIFFERENCES AND MOVE POINTER
                CMPA        #'-             ; CHECK FOR MINUS
                BNE         SF507           ; IF NOT '+' OR '-', MOVE THE POINTER TO THE COORDINATES IN U AND ACCD
SF570           TFR         U,D             ; PUT HORIZONTAL DIFFERENCE IN ACCD; X CONTAINS THE VERTICAL DIFFERENCE
                JMP         >SF4D4          ; GOMOVE THE DRAW POSITION
SF575           JSR         >SF591          ; GET A CHAR FROM COMMAND LINE
SF578           CMPA        #'+             ; CHECK FOR LEADING + (RELATIVE MOTION)
                BEQ         SF583           ; BRANCH IF RELATIVE
                CMPA        #'-             ; DO THE SAME FOR THE MINUS SIGN
                BEQ         SF584           ; BRANCH IF RELATIVE
                JSR         >SF5F2          ; MOVE COMMAND STRING BACK ONE IF NOT RELATIVE MOTION
SF583           CLRA                        ;  IF ACCA=0, THEN '+' IF ACCA <> 0, THEN '-'
SF584           PSHS        A               ; SAVE ADD/SUB FLAG; 0=ADD, <> 0 = SUBTRACT
                JSR         >SF3BD          ; EVALUATE DECIMAL NUMBER IN COMMAND STRING - RETURN VALUE IN ACCD
                TST         ,S+             ; CHECK THE ADD/SUBTRACT FLAG AND CLEAN UP THE STACK
                BEQ         SF590           ; BRANCH IF ADD
; THIS IS A BUG; SHOULD BE JSR NEGACCD INSTEAD OF THE NEXT TWO INSTRUCTIONS
                NEGB                        ;
                SBCA        #$00
SF590           RTS
; GET NEXT COMMAND - RETURN VALUE IN ACCA
SF591           PSHS        X               ; SAVE X REGISTER
SF593           TST         VD8             ; CHECK COMMAND COUNTER
                LBEQ        LB44A           ; ILLEGAL FUNCTION CALL ERROR IF NO COMMAND DATA LEFT
                LDX         VD9             ; GET COMMAND ADDRESS
                LDA         ,X+             ; GET COMMAND
                STX         VD9             ; SAVE NEW COMMAND ADDRESS
                DEC         VD8             ; DECREMENT COMMAND COUNTER
                CMPA        #SPACE          ; CHECK FOR BLANK
                BEQ         SF593           ; IGNORE BLANKS
                PULS        X,PC            ; RESTORE X REGISTER AND RETURN
SF5A7           CMPA        #'=             ; CHECK FOR A VARIABLE EQUATE
                BNE         SF5B6           ; BRANCH IF NOT VARIABLE EQUATE
                PSHS        U,Y             ; SAVE REGISTERS
                BSR         SF611           ; INTERPRET THE VARIABLE IN THE COMMAND LINE
                JSR         LB3E9           ; CONVERT VARIABLE INTO A POSITIVE INTEGER IN ACCD
                STD         VD3             ; SAVE THE SUBCOMMAND VALUE
                PULS        Y,U,PC          ; RESTORE REGISTERS AND RETURN
SF5B6           JSR         >SF608          ; CLEAR CARRY IF NUMERIC
                LBCS        LB44A           ; ILLEGAL FUNCTION CALL IF NOT NUMERIC
                CLR         VD3             ;
                CLR         VD4             ; INITIALIZE THE SUBCOMMAND VALUE TO ZERO
; STRIP A DECIMAL ASCII VALUE FROM THE COMMAND STRING AND RETURN THE BINARY VALUE IN VD3
SF5C1           SUBA        #'0             ; MASK OFF ASCII
                STA         VD7             ; SAVE TEMPORARILY
                LDD         VD3             ; GET THE CURRENT SUBCOMMAND VALUE
                BSR         SF5FD           ; MULTIPLY ACCD BY 10
                ADDB        VD7             ; ADD THE CURRENT DIGIT
                ADCA        #$00            ; PROPAGATE THE CARRY
                STD         VD3             ; SAVE THE NEW SUBCOMMAND VALUE
                LDA         HRMODE          ; GET THE HI-RES GRAPHICS MODE
                CMPA        #$02            ; IS IT A 640 OR 320 BYTES/PIXEL ROW MODE?
                BGT         SF5DA           ; BRANCH IF 640 PIXELS/HORIZONTAL ROW MODE
                LDD         #320-1          ; MAXIMUM HORIZONTAL PIXELS IN THE 320 PIXEL MODE
                BRA         SF5DD
SF5DA           LDD         #640-1          ; MAXIMUM HORIZONTAL PIXELS IN THE 640 PIXEL MODE
SF5DD           CMPD        VD3             ; COMPARE THE SUBCOMMAND TO THE MAXIMUM PERMISSABLE
                LBLT        LB44A           ; ILLEGAL FUNCTION CALL IF SUBCOMMAND TOO BIG
                LDD         VD3             ; THIS INSTRUCTION IS USELESS
                TST         VD8             ; CHECK THE COMMAND COUNTER
                BEQ         SF5FA           ; BRANCH IF NO COMMANDS LEFT
                JSR         >SF591          ; GET ANOTHER COMMAND
                JSR         >SF608          ; CLEAR CARRY IF NUMERIC
                BCC         SF5C1           ; BRANCH IF MORE NUMERIC DATA TO CONVERT
SF5F2           INC         VD8             ; ADD ONE TO THE COMMAND COUNTER
                LDX         VD9             ;
                LEAX        $-01,X          ;
                STX         VD9             ; MOVE THE COMMAND STRING BACK ONE
SF5FA           LDD         VD3             ; LOAD ACCD WITH THE VALUE OF THE SUBCOMMAND
                RTS
; MULTIPLY ACCD BY TEN
SF5FD           ASLB                        ;
                ROLA                        ;  MULTIPLY ACCD BY 2
                PSHS        B,A             ; SAVE ACCD TIME 2
                ASLB                        ;
                ROLA                        ;
                ASLB                        ;
                ROLA                        ;  NOW ACCD = ACCD * 8
                ADDD        ,S++            ; ADD ACCD*2; THE RESULT IS NOW ACCD*10
                RTS
; CLEAR THE CARRY FLAG IF ACCA CONTAINS A NUMERIC ASCII VALUE ($30-$39)
SF608           CMPA        #'0
                BCS         SF610           ; RETURN IF LESS THAN ASCII ZERO
                SUBA        #'9+1
                SUBA        #-('9+1)        ; SET CARRY IF NOT 0-9
SF610           RTS
; INTERPRET THE CURRENT COMMAND STRING AS IF IT WERE A BASIC VARIABLE
SF611           LDX         VD9             ; GET THE COMMAND POINTER
                PSHS        X               ; SAVE IT
                JSR         >SF591          ; GET A COMMAND STRING CHARACTER
                JSR         LB3A2           ; SET CARRY IF NOT UPPER CASE ALPHA
                LBCS        LB44A           ; ILLEGAL FUNCTION CALL ERROR IF NOT ALPHA - ILLEGAL VARIABLE NAME
SF61F           JSR         >SF591          ; GET COMMAND STRING CHARACTER
                CMPA        #';             ; CHECK FOR A SEMICOLON (SUBCOMMAND SEPARATOR)
                BNE         SF61F           ; LOOP UNTIL SEMICOLON FOUND
                PULS        X               ; GET THE START OF THE VARIABLE NAME
                LDU         CHARAD          ; GET THE CURRENT ADDRESS OF THE VARIABLE NAME
                PSHS        U               ; SAVE IT
                STX         CHARAD          ; PUT THE COMMAND POINTER IN PLACE OF BASIC'S INPUT POINTER
                JSR         LB284           ; EVALUATE AN ALPHA EXPRESSION
                PULS        X               ; GET BASIC'S POINTER BACK
                STX         CHARAD          ; RESTORE BASIC'S INPUT POINTER
                RTS
; WIDTH
WIDTH           CLR         HRMODE          ; TURN OFF HI-RES GRAPHICS MODE
                LBRN        RAMLINK         ; RAM HOOK
                CMPA        #$00            ; TEST FOR END OF LINE - NO ARGUMENT GIVEN
                BEQ         SF64F           ; 'FC' ERROR IF NO ARGUMENT
                JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                CMPB        #32             ; 32 COLUMNS
                BEQ         COL32
                CMPB        #40             ; 40 COLUMNS
                BEQ         COL40
                CMPB        #80             ; 80 COLUMNS
                BEQ         COL80
SF64F           JMP         LB44A           ; ILLEGAL FUNCTION CALL ERROR
; 32 COLUMNS
COL32           CLRA                        ;  32 COLUMN MODE FLAG
                STA         HRWIDTH         ; SAVE THE HI-RES TEXT MODE
                JSR         CLRSCRN         ; CLEAR THE 32 COLUMN SCREEN
                LBSR        SETTEXT         ; SETUP THE VIDEO MODE REGISTERS
                RTS
; 40 COLUMNS
COL40           LDA         #$01            ; 40 COLUMN MODE FLAG
                STA         HRWIDTH         ; SAVE THE HI-RES TEXT MODE
                LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                LDA         #40             ; 40 COLUMNS
                LDB         #ROWMAX         ; MAXIMUM NUMBER OF ROWS
                STD         >H.COLUMN       ; SAVE THE NUMBER OF COLUMNS AND ROWS
                LDD         #HRESSCRN+40*ROWMAX*2 ; END OF THE HI-RES TEXT SCREEN
SF66D           STD         >H.DISPEN       ; SAVE THE END OF THE HI-RES TEXT SCREEN
                BSR         SF68C           ; RESET HI-RES TEXT SCREEN
                LBSR        SF778           ; PUT BLOCK 7.1 INTO LOGICAL BLOCK 1
                LBSR        SETTEXT         ; SETUP THE VIDEO MODE REGISTERS
                RTS
; 80 COLUMNS
COL80           LDA         #$02            ; 80 COLUMN MODE FLAG
                STA         HRWIDTH         ; SAVE THE HI-RES TEXT MODE
                LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                LDA         #80             ; 80 COLUMNS
                LDB         #ROWMAX         ; MAXIMUM NUMBER OF ROWS
                STD         >H.COLUMN       ; SAVE THE NUMBER OF COLUMNS AND ROWS
                LDD         #HRESSCRN+80*ROWMAX*2 ; END OF THE HI-RES TEXT SCREEN
                BRA         SF66D
SF68C           LDX         #HRESSCRN       ; POINT X TO THE TOP OF THE HI-RES TEXT SCREEN
                LBRN        RAMLINK         ; RAM HOOK
                STX         >H.CRSLOC       ; SAVE THE START OF THE HI-RES TEXT SCREEN
                LDA         #SPACE          ; INITIALIZE CHARACTERS TO SPACES
                LDB         >H.CRSATT       ; GET THE CHARACTER ATTRIBUTES
SF69B           STD         ,X++            ; SAVE THE CHARACTER AND ATTRIBUTES IN HI-RES TEXT SCREEN
                CMPX        >H.DISPEN       ; COMPARE TO THE END OF HI-RES TEXT SCREEN
                BCS         SF69B           ; LOOP UNTIL ALL MEMORY INITIALIZED
                LDX         #HRESSCRN       ; RESET X TO THE TOP OF THE SCREEN
                CLRA                        ;
                STA         >H.CURSX        ; SET THE CURSOR X COORDINATE (COLUMN) TO ZERO
                STA         >H.CURSY        ; SET THE CURSOR Y COORDIANTE (ROW) TO ZERO
                RTS
; CLS PATCH ENTERED FROM $8C4C
ALINK23         PULS        CC              ; RESTORE THE ZERO FLAG
                LBRN        RAMLINK         ; RAM HOOK
                BEQ         SF6E0           ; CLEAR THE SCREEN CURSOR ATTRIBUTES IF NO ARGUMENT
                JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                TSTB                        ;  TEST ARGUMENT
                BEQ         SF6E0           ; BRANCH IF CLS 0
                CMPB        #$08            ; CHECK FOR CLS 8
                BHI         SF6E7           ; BRANCH IF > CLS 8
                DECB                        ;  CHANGE 1-8 TO 0-7
                LEAY        IM.PALET,PC     ; POINT TO THE PALETTE REGISTER IMAGES
                LDA         B,Y             ; GET THE COLOR IN THE PALETTE REGISTER
                STA         V_BORDER        ; AND SAVE IT AS THE NEW BORDER COLOR
                LBSR        SF766           ; SET THE BORDER COLOR IN THE 40 & 80 COLUMN VIDEO MODE IMAGES
                STB         >H.CRSATT       ; SAVE THE ADJUSTED CLS ARGUMENT AS THE NEW ATTRIBUTE BYTE
                LDA         #SPACE
                LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO LOGICAL BLOCK 1
                LDX         #HRESSCRN       ; POINT X TO THE TOP OF THE HI-RES TEXT SCREEN
                STX         >H.CRSLOC       ; PUT THE CURSOR AT THE TOP OF THE SCREEN
                BSR         SF69B           ; CLEAR THE SCREEN
SF6DC           LBSR        SF778           ; REMOVE THE HI-RES TEXT SCREEN FROM THE LOGICAL ADDRESS SPACE
                RTS
SF6E0           LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO LOGICAL BLOCK 1
                BSR         SF68C           ; CLEAR THE HI-RES TEXT SCREEN
                BRA         SF6DC           ; PUT BLOCK 7.1 BACK INTO LOGICAL BLOCK 1
SF6E7           CLR         >H.CRSATT       ; RESET THE ATTRIBUTE BYTE TO ZERO
                LDA         IM.PALET        ; GET THE COLOR IN PALETTE REGISTER 0
                STA         V_BORDER        ; AND SAVE IT AS THE NEW BORDER COLOR
                BSR         SF766           ; ALSO SAVE IT IN THE 40 AND 80 COLUMN VIDEO REGISTER IMAGES
                CMPB        #100            ; CHECK FOR CLS 100
SF6F4           BEQ         SF730           ; IF CLS 100, THEN PRINT THE AUTHORS' NAMES - THIS WILL ONLY BE
; DONE THE FIRST TIME CLS 100 IS EXECUTED, THIS CODE WILL BE
; OVERWRITTEN BY NOPs WHEN THE AUTHORS' NAMES ARE DISPLAYED.
                BSR         SF772           ; PUT THE HI-RES TEXT SCREEN INTO LOGICAL BLOCK 1
                BSR         SF68C           ; CLEAR THE HI-RES TEXT SCREEN
                BSR         SF778           ; PUT BLOCK 7.1 BACK INTO LOGICAL BLOCK 1
                LDX         #MICROMS-1      ; POINT TO MICROWARE'S COMMERCIAL MESSAGE
                JMP         STRINOUT        ; COPY A STRING TO CONSOLE OUT ($B99C)
; MICROWARE COMMERCIAL
MICROMS         FCC         'Microware Systems Corp.'
                FCB         $0D,$00
; NAMES OF THE AUTHORS
; THE INITIALIZATION CODE WILL COPY THE AUTHOR'S NAMES INTO THIS SPOT
AUTHORMS        FCB         $00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00
SF730           BSR         SF772           ; PUT THE HI-RES TEXT SCREEN INTO LOGICAL BLOCK 1
                LBSR        SF68C           ; CLEAR THE HI-RES TEXT SCREEN
                BSR         SF778           ; PUT BLOCK 7.1 BACK INTO LOGICAL BLOCK 1
                LDX         #AUTHORMS-1     ; POINT TO THE AUTHOR MESSAGE
                JSR         >STRINOUT       ; COPY A STRING TO CONSOLE OUT
                PSHS        X
                LEAX        >SF6F4,PC       ; POINT TO THE INSTRUCTION WHICH BRANCHES TO THIS ROUTINE
                LDA         #$12            ; OP CODE OF A NOP
                STA         ,X+             ; REPLACE THE BRANCH TO THIS ROUTINE WITH 2 NOPs MAKING IT SO
                STA         ,X              ; THAT THIS ROUTINE MAY ONLY BE ENTERED ONE TIME
                LEAX        >AUTHORMS,PC    ; POINT TO THE AUTHORS CODED NAMES
; REPLACE THE AUTHORS' NAMES AND THE CODE THAT DISPLAYS THEM WITH NOPs
SF74D           STA         ,X+             ; SAVE A NOP
                CMPX        #SF74D          ; CHECK FOR END OF THE DISPLAY NAME ROUTINE
                BCS         SF74D           ; LOOP UNTIL DONE
                PULS        X               ; RESTORE X; THIS AND THE RTS FOLLOWING SHOULD BE PULS X,PC
                RTS
; GET AN INPUT LINE FOR BASIC PATCH ENTERED FROM $A38D
ALINK27         TST         HRWIDTH         ; CHECK FOR HI-RES TEXT MODE
                BNE         SF761           ; BRANCH IF IN A HI-RES TEXT MODE
                JSR         CLRSCRN         ; CLEAR THE 32 COLUMN SCREEN
SF75E           JMP         LA390           ; RE-ENTER THE MAIN STREAM OF CODE ($A390)
SF761           LBSR        SF6E0           ; RESET THE HI-RES TEXT SCREEN
                BRA         SF75E
; SAVE THE VALUE IN ACCA AS THE BORDER COLOR IN THE 40 AND 80 COLUMN VIDEO MODE IMAGES
SF766           PSHS        Y
                LEAY        SE03B,PC        ; POINT TO THE 40 COLUMN MODE REGISTER IMAGE
                STA         $03,Y           ; SAVE THE BORDER COLOR IN THE 40 COLUMN VIDEO MODE REGISTER IMAGE
                STA         $0C,Y           ; SAVE THE BORDER COLOR IN THE 80 COLUMN VIDEO MODE REGISTER IMAGE
                PULS        Y,PC
SF772           ORCC        #$50            ; DISABLE THE INTERRUPTS
                LBSR        SELTEXT         ; PUT BLOCK 6.6 INTO LOGICAL BLOCK 1
                RTS
SF778           LBSR        SETMMU          ; COPY THE MMU IMAGES INTO THE MMU REGISTERS
                ANDCC       #$AF            ; ENABLE IRQ, FIRQ
                RTS
; PATCH 24 - BLINK THE CURSOR PATCH ENTERED FROM $A0D4
ALINK24         BSR         SF787           ; BLINK THE CURSOR
                JSR         KEYIN           ; GET A KEY
                BEQ         ALINK24         ; LOOP UNTIL A KEY IS PRESSED
                PULS        B,X,PC
SF787           DEC         BLKCNT          ; DECREMENT THE CURSOR BLINK DELAY
                BNE         SF7A8           ; IT'S NOT TIME TO BLINK THE CURSOR
                LDB         #11             ; CURSOR BLINK DELAY CONSTANT
                STB         BLKCNT          ; RESET THE CURSOR BLINK DELAY COUNTER
                BSR         SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                LDX         >H.CRSLOC       ; POINT TO THE CURSOR CHARACTER
                LDA         $01,X           ; GET THE CURSOR CHARACTER'S ATTRIBUTES
                BITA        #$40            ; IS THE UNDERLINE MODE ACTIVE?
                BEQ         SF79F           ; BRANCH IF NOT ACTIVE UNDERLINE
                LDA         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                BRA         SF7A4           ; PUT IT ON THE SCREEN
SF79F           LDA         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                ORA         #$40            ; FORCE THE UNDERLINE ATTRIBUTE
SF7A4           STA         $01,X           ; SAVE THE NEW CURSOR ATTRIBUTES IN THE HI-RES TEXT SCREEN
                BSR         SF778           ; RESTORE THE NORMAL BASIC PROGRAM BLOCK TO LOGICAL BLOCK 1
SF7A8           LDX         #DEBDEL         ; GET THE KEYBOARD DEBOUNCE DELAY
                JMP         LA7D3           ; GO WAIT A WHILE ($A7D3)
; PATCH 22 - PUT A CHARACTER ON THE SCREEN PATCH ENTERED FROM $BC3D
ALINK22         BSR         SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                LBRN        RAMLINK         ; RAM HOOK
                LDX         >H.CRSLOC       ; POINT TO THE CURSOR CHARACTER
                CMPA        #BS             ; BACKSPACE CHARACTER?
                BNE         SF7C4           ; NO
; DO A BACKSPACE HERE
                CMPX        #HRESSCRN       ; ARE WE AT THE UPPER LEFT-HAND CORNER OF THE SCREEN?
                BEQ         SF7DE           ; YES, DO NOT ALLOW A BACKSPACE
                BSR         SF7E2           ; DO A BACKSPACE ON THE HI-RES SCREEN
                BRA         SF7DE
SF7C4           CMPA        #CR             ; ENTER KEY?
                BNE         SF7CC           ; NO
                BSR         SF827           ; DO A CARRIAGE RETURN ON THE HI-RES SCREEN
                BRA         SF7D7           ; CHECK TO SEE IF THE SCREEN SHOULD BE SCROLLED
SF7CC           CMPA        #$20            ; CHECK FOR A CONTROL CHARACTER
                BCS         SF7DE           ; DO NOTHING IF A CONTROL CHARACTER
                LDB         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                STD         ,X              ; PUT THE NEW CHARACTER AND ATTRIBUTES INTO THE HI-RES TEXT SCREEN
                BSR         SF807           ; MOVE THE CURSOR FORWARD ONE CHARACTER
SF7D7           CMPX        >H.DISPEN       ; CHECK FOR THE END OF THE HI-RES TEXT SCREEN
                BCS         SF7DE           ; BRANCH IF NOT AT THE END
                BSR         SF854           ; SCROLL THE SCREEN UP ONE ROW
SF7DE           BSR         SF778           ; RESTORE THE NORMAL BASIC PROGRAM BLOCK TO LOGICAL BLOCK 1
                PULS        A,B,X,PC
; DO A HI-RES BACKSPACE HERE
SF7E2           PSHS        B,A
                LDA         #SPACE          ; SPACE CHARACTER
                LDB         >H.CRSATT       ; GET THE ATTRIBUTES RAM IMAGE
                STD         ,X              ; SAVE A SPACE ON THE SCREEN AT THE OLD CURSOR POSITION
                ORB         #$40            ; FORCE THE UNDERLINE ATTRIBUTE
                STD         $-02,X          ; SAVE AN UNDERLINED SPACE AS THE NEW CURSOR CHARACTER
                LEAX        $-02,X          ; MOVE THE CURSOR POINTER BACK TWO
                STX         >H.CRSLOC       ; AND SAVE IT IN RAM
                LDD         >H.CURSX        ; GET THE COLUMN AND ROW POSITION OF THE OLD CURSOR
                DECA                        ;  BUMP THE COLUMN NUMBER DOWN ONE
                BPL         SF802           ; BRANCH IF NO WRAP-AROUND
                DECB                        ;  BUMP THE ROW COUNTER DOWN ONE
                STB         >H.CURSY        ; SAVE THE NEW CURSOR ROW NUMBER
                LDA         >H.COLUMN       ; GET THE NUMBER OF CHARACTERS PER ROW
                DECA                        ;  MAKE THE HIGHEST ALLOWABLE COLUMN NUMBER (ZERO IS FIRST)
SF802           STA         >H.CURSX        ; SAVE THE NEW CURSOR COLUMN NUMBER
                PULS        A,B,PC
SF807           PSHS        B,A
                LDA         #$20            ; GET THE CURSOR CHARACTER
                LDB         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                ORB         #$40            ; FORCE THE UNDERLINE ATTRIBUTE
                LEAX        $02,X           ; MOVE THE POINTER UP ONE CHARACTER POSITION
                STD         ,X              ; SAVE THE NEW CHARACTER ATTRIBUTES IN THE HI-RES TEXT SCREEN
                STX         >H.CRSLOC       ; SAVE THE NEW CURSOR POSITION
                LDD         >H.CURSX        ; GET THE OLD CURSOR ROW AND COLUMN NUMBERS
                INCA                        ;  BUMP THE COLUMN NUMBER UP ONE
                CMPA        >H.COLUMN       ; CHECK FOR WRAP-AROUND TO NEXT ROW
                BCS         SF802           ; BRANCH IF NO WRAP-AROUND
                INCB                        ;  BUMP THE ROW NUMBER UP ONE
                STB         >H.CURSY        ; SAVE THE NEW ROW NUMBER
                CLRA                        ;  SET THE COLUMN NUMBER TO ZERO
                BRA         SF802
; DO A HI-RES CARRIAGE RETURN
SF827           PSHS        B,A
                LDA         #SPACE          ; SPACE CHARACTER
                LDB         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
SF82E           STD         ,X++            ; SAVE A SPACE CHARACTER AND ADVANCE THE CURSOR POINTER ONE CHARACTER
                PSHS        A
                LDA         >H.CURSX        ; GET THE CURSOR'S COLUMN NUMBER
                INCA                        ;  BUMP IT UP ONE
                STA         >H.CURSX        ; SAVE THE NEW COLUMN NUMBER
                CMPA        >H.COLUMN       ; HAS IT WRAPPED AROUND?
                PULS        A
                BCS         SF82E           ; BRANCH IF NO WRAP-AROUND
                STX         >H.CRSLOC       ; SAVE THE NEW CURSOR POINTER
                CLR         >H.CURSX        ; SET THE CURSOR COLUMN NUMBER TO ZERO
                INC         >H.CURSY        ; BUMP THE ROW NUMBER UP ONE
                LDA         #$20            ; GET THE CURSOR CHARACTER
                LDB         >H.CRSATT       ; ACCB ALREADY CONTAINS THIS VALUE
                ORB         #$40            ; FORCE THE UNDERLINE ATTRIBUTE
                STD         ,X              ; SAVE AN UNDERLINED CHARACTER AS THE NEW CURSOR CHARACTER
                PULS        A,B,PC
; SCROLL THE SCREEN
SF854           PSHS        B,A
                LDX         #HRESSCRN       ; POINT TO THE START OF THE HI-RES TEXT SCREEN
                LDA         >H.COLUMN       ; GET THE NUMBER OF CHARACTERS PER ROW
                CMPA        #40             ; 40 CHARACTERS PER ROW?
                BNE         SF86E           ; BRANCH IF 80 CHARACTERS PER ROW
; SCROLL A 40 CHARACTER ROW
SF860           LDD         2*40,X          ; GET A CHARACTER AND ATTRIBUTE FROM ONE ROW DOWN
                STD         ,X++            ; AND MOVE THEM UP TO THE PRESENT ROW
                CMPX        #HRESSCRN+(ROWMAX-1)*40*2 ; PAST THE END OF THE HI-RES TEXT SCREEN?
                BCS         SF860           ; NO, KEEP MOVING CHARACTERS AND ATTRIBUTES
SF86A           BSR         SF87B           ; FILL THE LAST ROW WITH SPACES
                PULS        A,B,PC
; SCROLL AN 80 CHARACTER SCREEN
SF86E           LDD         80*2,X          ; GET A CHARACTER AND ATTRIBUTES FROM ONE ROW DOWN
                STD         ,X++            ; AND MOVE THEM UP TO THE PRESENT ROW
                CMPX        #HRESSCRN+(ROWMAX-1)*80*2 ; PAST THE END OF THE HI-RES TEXT SCREEN?
                BCS         SF86E           ; NO, KEEP MOVING CHARACTERS AND ATTRIBUTES
                BRA         SF86A
; FILL THE LAST ROW WITH SPACES
SF87B           CLR         >H.CURSX        ; RESET THE COLUMN NUMBER TO ZERO
                LDA         #ROWMAX-1       ; GET THE HIGHEST ROW NUMBER (ZERO IS LOWEST)
                STA         >H.CURSY        ; AND SAVE IT AS THE CURRENT ROW NUMBER
                LDA         #SPACE          ; SPACE CHARACTER
                LDB         >H.CRSATT       ; GET THE ATTRIBUTES RAM IMAGE
                PSHS        X               ; SAVE THE CURRENT CHARACTER POINTER
SF88A           STD         ,X++            ; SAVE A CHARACTER AND ATTRIBUTES TO THE HI-RES TEXT SCREEN
                CMPX        >H.DISPEN       ; CHECK FOR THE END OF THE HI-RES TEXT SCREEN
                BNE         SF88A           ; BRANCH IF NOT AT THE END OF THE HI-RES TEXT SCREEN
                CLR         >H.CURSX        ; RESET THE COLUMN NUMBER TO ZERO
                PULS        X               ; RESTORE THE CHARACTER POINTER
                LDA         #$20            ; GET THE CURSOR CHARACTER
                LDB         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                ORB         #$40            ; FORCE THE UNDERLINE ATTRIBUTE
                STD         ,X              ; SAVE THE NEW CURSOR CHARACTER
                STX         >H.CRSLOC       ; SAVE THE NEW CURSOR POINTER
                RTS
; PRINT @ PATCH ENTERED FROM $B902
ALINK26         TST         DEVNUM          ; CHECK THE DEVICE NUMBER
                BNE         SF8AB           ; BRANCH IF NOT THE SCREEN
                TST         HRWIDTH         ; CHECK THE HI-RES TEXT MODE
                BNE         SF8B1           ; BRANCH IF A HI-RES TEXT MODE IS SET
SF8AB           JSR         LA35F           ; SET UP THE PRINT PARAMETERS
                JMP         LB95F           ; RE-ENTER THE MAIN STREAM OF CODE ($B95F)
SF8B1           LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                TST         >H.CURSX        ; CHECK THE CURSOR'S X COORDINATE
                PSHS        CC              ; SAVE THE ZERO FLAG
                LBSR        SF778           ; RESTORE THE NORMAL BASIC PROGRAM BLOCK TO LOGICAL BLOCK 1
                PULS        CC              ; RESTORE THE ZERO FLAG
                LBNE        LB958           ; BRANCH IF THE CURSOR IS NOT AT THE START OF THE LINE ($B958)
                RTS
; PRINT @ PATCH ENTERED FROM $B902
ALINK25         TST         HRWIDTH         ; CHECK THE HI-RES TEXT MODE
                BNE         SF8CD           ; 'HP' ERROR IF THE HI-RES TEXT MODE IS NOT SET
                JSR         LA554           ; MOVE THE CURSOR TO THE PROPER PRINT POSITION
                JMP         LB905           ; RE-ENTER THE MAIN STREAM OF CODE ($B905)
SF8CD           LDB         #39*2           ; 'HP' ERROR
                JMP         LAC46           ; JUMP TO ERROR HANDLER ($AC46)
; LOCATE
LOCATE          LDB         HRWIDTH         ; IS THE HI-RES TEXT MODE ENABLED?
                LBRN        RAMLINK         ; RAM HOOK
                BEQ         SF8CD           ; 'HP' ERROR IF NOT ENABLED
                PSHS        B               ; SAVE THE HI-RES TEXT MODE
                JSR         >SE7B2          ; EVALUATE TWO EXPRESSIONS
                LDA         BINVAL+1        ; GET THE FIRST OF THE TWO EXPRESSIONS (COLUMN NUMBER)
                PULS        B               ; RESTORE THE FIRST ARGUMENT
                CMPB        #$01            ; GET BACK THE HI-RES TEXT MODE
                BNE         SF8EB           ; BRANCH IF NOT 40 COLUMN MODE
                CMPA        #40             ; 40 COLUMNS MAXIMUM IN 40 COLUMN MODE
                BRA         SF8ED           ; DO A RANGE CHECK
SF8EB           CMPA        #80             ; 80 COLUMNS MAXIMUM IN 80 COLUMN MODE
SF8ED           LBCC        LB44A           ; ILLEGAL FUNCTION CALL ERROR
                LDB         VERBEG+1        ; GET THE SECOND ARGUMENT (ROW NUMBER)
                CMPB        #ROWMAX         ; RANGE CHECK ON THE ROW NUMBER
                BCC         SF8ED           ; 'FC' ERROR IF ROW NUMBER IS TOO LARGE
                PSHS        B,A             ; SAVE THE COLUMN AND ROW NUMBERS
                LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                STD         >H.CURSX        ; SAVE THE NEW COLUMN AND ROW NUMBERS AS THOSE OF THE CURSOR
                LDX         >H.CRSLOC       ; GET THE CURRENT CURSOR POINTER
                LDA         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                STA         $01,X           ; AND SAVE IT AS THE ATTRIBUTES IN THE OLD CURSOR POSITION
                LDA         >H.COLUMN       ; GET THE NUMBER OF CHARACTERS/ROW
                ASLA                        ;  MULTIPLY BY TWO - TWO BYTES PER CHARACTER (CHAR AND ATTR)
                MUL                         ;  GET THE ROW OFFSET TO THE PROPER CHARACTER
                LDX         #HRESSCRN       ; POINT TO THE START OF THE HI-RES TEXT SCREEN
                LEAX        D,X             ; ADD ROW OFFSET TO THE START OF THE HI-RES TEXT SCREEN
                PULS        A,B             ; RESTORE THE NEW CURSOR COLUMN AND ROW NUMBERS
                ASLA                        ;  MULTIPLY COLUMN NUMBER BY TWO - TWO BYTES PER CHARACTER (CHAR AND ATTR)
                TFR         A,B             ; SAVE COLUMN OFFSET IN ACCB
                ABX                         ;  ADD THE COLUMN OFFSET TO THE CURRENT CURSOR POINTER
                LDA         >H.CRSATT       ; GET THE CURSOR ATTRIBUTES RAM IMAGE
                ORA         #$40            ; FORCE UNDERLINE ATTRIBUTE
                STA         $01,X           ; SAVE THE NEW CURSOR ATTRIBUTE IN THE HI-RES TEXT SCREEN
                STX         >H.CRSLOC       ; SAVE THE NEW CURSOR POINTER
                LBSR        SF778           ; RESTORE THE NORMAL BASIC PROGRAM BLOCK TO LOGICAL BLOCK 1
                RTS
; HSTAT
HSTAT           TST         HRWIDTH         ; IS THE HI-RES TEXT MODE ENABLED?
                LBRN        RAMLINK         ; RAM HOOK
                BEQ         SF8CD           ; 'HP' ERROR IF HI-RES TEXT MODE NOT ENABLED
                LBSR        SF772           ; PUT THE HI-RES TEXT SCREEN INTO THE LOGICAL ADDRESS SPACE
                LDX         >H.CRSLOC       ; GET THE CURRENT CURSOR POINTER
                LDD         ,X              ; GET THE CURSOR CHARACTER ATTRIBUTES
                STD         VCB             ; AND SAVE THEM
                LDD         >H.CURSX        ; GET THE CURRENT COLUMN AND ROW NUMBER
                STD         VCD             ; AND SAVE THEM
                LBSR        SF778           ; RESTORE THE NORMAL BASIC PROGRAM BLOCK TO LOGICAL BLOCK 1
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                LDB         #$01
                JSR         LB56D           ; RESERVE SPACE FOR A ONE CHARACTER STRING IN STRING SPACE
                LDA         VCB             ; GET THE CURSOR CHARACTER
                JSR         LB511           ; THIS IS REALLY A WASTE - THE JSR LB56D ABOVE SHOULD JUST BE A
                STA         ,X              ; SAVE THE CURSOR CHARACTER IN THE NEWLY RESERVED STRING SPACE
                JSR         LB54C           ; PUT THE STRING ONTO THE STRING STACK
                LDX         VARDES          ; POINT TO THE STRING'S VARIABLE DESCRIPTOR
                TST         $-01,X          ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBPL        LB151           ; TYPE MISMATCH ERROR IF NUMERIC VARIABLE
                LDY         FPA0+2          ; POINT Y TO THE START OF THE STRING DESCRIPTOR
                LDB         #$05            ; VARIABLE DESCRIPTORS ARE 5 BYTES LONG
SF963           LDA         ,Y+             ; COPY THE DATA FROM THE STRING DESCRIPTOR
                STA         ,X+             ; TO THE VARIABLE DESCRIPTOR
                DECB                        ;  DECREMENT THE DESCRIPTOR COUNTER
                BNE         SF963           ; LOOP UNTIL DONE
                LDX         TEMPPT          ; THIS CODE IS DESIGNED TO REMOVE THE ABOVE ALLOCATED STRING FROM
                LEAX        $-05,X          ; THE STRING STACK - IT MAY CAUSE BUGS BECAUSE IT DOESN'T RESET
                STX         TEMPPT          ; LASTPT; LDX LASTPT, JSR LB675 WOULD BE MUCH BETTER
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                CLRA                        ;  ZERO OUT THE MS BYTE OF ACCD
                LDB         VCB+1           ; GET THE CURSOR ATTRIBUTES
                JSR         GIVABF          ; CONVERT ACCD TO FLOATING POINT
                LDX         VARDES          ; POINT X TO THE VARIABLE DESCRIPTOR
                TST         $-01,X          ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBMI        LB151           ; TYPE MISMATCH ERROR IF STRING VARIABLE
                JSR         LBC35           ; PACK FPA0 AND STORE IT IN THE DESCRIPTOR POINTED TO BY X
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                CLRA                        ;  ZERO OUT THE MS BYTE OF ACCD
                LDB         VCD             ; GET THE X COORDINATE OF THE CURSOR POSITION
                JSR         GIVABF          ; CONVERT ACCD TO FLOATING POINT
                LDX         VARDES          ; POINT X TO THE VARIABLE DESCRIPTOR
                TST         $-01,X          ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBMI        LB151           ; TYPE MISMATCH ERROR IF STRING VARIABLE
                JSR         LBC35           ; PACK FPA0 AND STORE IT IN THE DESCRIPTOR POINTED TO BY X
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                CLRA                        ;  ZERO OUT THE MS BYTE OF ACCD
                LDB         VCD+1           ; GET THE Y COORDINATE OF THE CURSOR POSITION
                JSR         GIVABF          ; CONVERT ACCD TO FLOATING POINT
                LDX         VARDES          ; POINT X TO THE VARIABLE DESCRIPTOR
                TST         $-01,X          ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBMI        LB151           ; TYPE MISMATCH ERROR IF STRING VARIABLE
                JSR         LBC35           ; PACK FPA0 AND STORE IT IN THE DESCRIPTOR POINTED TO BY X
                RTS
; ATTR
ATTR            JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB (CHARACTER COLOR)
                LBRN        RAMLINK         ; RAM HOOK
                CMPB        #$08            ; 8 CHARACTER COLORS MAXIMUM
                LBCC        LB44A           ; ILLFUNC ILLEGAL FUNCTION CALL ERROR IF CHARACTER COLOR > 8
                ASLB                        ;
                ASLB                        ;
                ASLB                        ;  SHIFT THE CHARACTER COLOR INTO BITS 3-6
                PSHS        B               ; SAVE THE SHIFTED COLOR ON THE STACK
                JSR         GETCCH          ; GET THE CURRENT INPUT CHARACTER
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB (BACKGROUND COLOR)
                CMPB        #$08            ; 8 MAXIMUM BACKGROUND COLORS
                LBCC        LB44A           ; ILLEGAL FUNCTION CALL ERROR IF > 8
                ORB         ,S              ; 'OR' IN THE CHARACTER COLOR
                LEAS        $01,S           ; REMOVE TEMPORARY CHARACTER FROM STACK; ORB ,S+ ABOVE IS MORE EFFICIENT
                ANDB        #$3F            ; MASK OFF BITS 6,7; THIS INSTRUCTION IS UNNECESSARY
                PSHS        B               ; SAVE THE CHARACTER AND BACKGROUND COLORS ON THE STACK
                JSR         GETCCH          ; GET THE CURRENT INPUT CHARACTER
SF9E3           BEQ         SFA06           ; BRANCH IF END OF LINE
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                CMPA        #'B             ; CHECK FOR THE BLINK ATTRIBUTE FLAG
                BNE         SF9F6           ; BRANCH IF NOR BLINK ATTRIBUTE FLAG
                PULS        B
                ORB         #$80            ; SET BIT 7 WHICH IS THE BLINK ATTRIBUTE BIT
                PSHS        B
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC'S INPUT LINE
                BRA         SF9E3           ; KEEP CHECKING FOR ATTRIBUTE FLAGS
SF9F6           CMPA        #'U             ; CHECK FOR THE UNDERLINE ATTRIBUTE
                LBNE        LB44A           ; ILLEGAL FUNCION CALL ERROR
                PULS        B
                ORB         #$40            ; SET BIT 6 WHICH IS THE UNDERLINE ATTRIBUTE BIT
                PSHS        B
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC'S INPUT LINE
                BRA         SF9E3           ; KEEP CHECKING FOR ATTRIBUTE FLAGS
SFA06           PULS        B               ; GET THE NEW ATTRIBUTE BYTE FROM THE STACK
                STB         >H.CRSATT       ; AND SAVE IT AS THE CURSOR ATTRIBUTES
                RTS

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
; UNUSED GARBAGE BYTES?
                FCB         $00,$00,$00,$00
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; UNUSED GARBAGE BYTES?
                FCB         $A1,$2A,$C4,$0B
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

; UNUSED GARBAGE BYTES?
                FCB         $38,$44,$40,$40,$40,$44,$38,$10,$44,$00,$44,$44,$44,$4C,$34,$00
                FCB         $08,$10,$38,$44,$7C,$40,$38,$00,$10,$28,$38,$04,$3C,$44,$3C,$00
                FCB         $28,$00,$38,$04,$3C,$44,$3C,$00,$20,$10,$38,$04,$3C,$44,$3C,$00
                FCB         $10,$00,$38,$04,$3C,$44,$3C,$00,$00,$00,$38,$44,$40,$44,$38,$10
                FCB         $10,$28,$38,$44,$7C,$40,$38,$00,$28,$00,$38,$44,$7C,$40,$38,$00
                FCB         $20,$10,$38,$44,$7C,$40,$38,$00,$28,$00,$30,$10,$10,$10,$38,$00
                FCB         $10,$28,$00,$30,$10,$10,$38,$00,$00,$18,$24,$38,$24,$24,$38,$40
                FCB         $44,$10,$28,$44,$7C,$44,$44,$00,$10,$10,$28,$44,$7C,$44,$44,$00
                FCB         $08,$10,$38,$44,$44,$44,$38,$00,$00,$00,$68,$14,$3C,$50,$3C,$00
                FCB         $3C,$50,$50,$78,$50,$50,$5C,$00,$10,$28,$38,$44,$44,$44,$38,$00
                FCB         $28,$00,$38,$44,$44,$44,$38,$00,$00,$00,$38,$4C,$54,$64,$38,$00
                FCB         $10,$28,$00,$44,$44,$4C,$34,$00,$20,$10,$44,$44,$44,$4C,$34,$00
                FCB         $38,$4C,$54,$54,$54,$64,$38,$00,$44,$38,$44,$44,$44,$44,$38,$00
                FCB         $28,$44,$44,$44,$44,$44,$38,$00,$38,$40,$38,$44,$38,$04,$38,$00
                FCB         $08,$14,$10,$38,$10,$50,$3C,$00,$10,$10,$7C,$10,$10,$00,$7C,$00
                FCB         $10,$28,$10,$00,$00,$00,$00,$00,$08,$14,$10,$38,$10,$10,$20,$40
                FCB         $00,$10,$18,$1C,$1C,$18,$10,$00,$00,$08,$18,$38,$38,$18,$08,$00
                FCB         $00,$00,$00,$7E,$3C,$18,$00,$00,$00,$00,$18,$3C,$7E,$00,$00,$00
                FCB         $00,$FF,$00,$FF,$FF,$00,$FF,$00,$00,$00,$30,$3C,$14,$1C,$00,$00
                FCB         $00,$7E,$42,$5A,$5A,$42,$7E,$00,$00,$7E,$7E,$00,$00,$7E,$7E,$00
                FCB         $00,$3C,$3C,$3C,$3C,$3C,$3C,$00,$00,$00,$7E,$7E,$7E,$7E,$00,$00
                FCB         $00,$7E,$24,$18,$18,$24,$7E,$00,$00,$7F,$00,$7F,$7F,$00,$7F,$00
                FCB         $00,$FE,$00,$FE,$FE,$00,$FE,$00,$38,$44,$40,$40,$40,$44,$38,$10
                FCB         $44,$00,$44,$44,$44,$4C,$34,$00,$08,$10,$38,$44,$7C,$40,$38,$00
                FCB         $10,$28,$38,$04,$3C,$44,$3C,$00,$28,$00,$38,$04,$3C,$44,$3C,$00
                FCB         $20,$10,$38,$04,$3C,$44,$3C,$00,$10,$00,$38,$04,$3C,$44,$3C,$00
                FCB         $00,$00,$38,$44,$40,$44,$38,$10,$10,$28,$38,$44,$7C,$40,$38,$00
                FCB         $28,$00,$38,$44,$7C,$40,$38,$00,$20,$10,$38,$44,$7C,$40,$38,$00
                FCB         $28,$00,$30,$10,$10,$10,$38,$00,$10,$28,$00,$30,$10,$10,$38,$00
                FCB         $00,$18,$24,$38,$24,$24,$38,$40,$44,$10,$28,$44,$7C,$44,$44,$00
                FCB         $10,$10,$28,$44,$7C,$44,$44,$00,$08,$10,$38,$44,$44,$44,$38,$00
                FCB         $00,$00,$68,$14,$3C,$50,$3C,$00,$3C,$50,$50,$78,$50,$50,$5C,$00
                FCB         $10,$28,$38,$44,$44,$44,$38,$00,$28,$00,$38,$44,$44,$44,$38,$00
                FCB         $00,$00,$38,$4C,$54,$64,$38,$00,$10,$28,$00,$44,$44,$4C,$34,$00
                FCB         $20,$10,$44,$44,$44,$4C,$34,$00,$38,$4C,$54,$54,$54,$64,$38,$00
                FCB         $44,$38,$44,$44,$44,$44,$38,$00,$28,$44,$44,$44,$44,$44,$38,$00
                FCB         $38,$40,$38,$44,$38,$04,$38,$00,$08,$14,$10,$38,$10,$50,$3C,$00
                FCB         $10,$10,$7C,$10,$10,$00,$7C,$00,$10,$28,$10,$00,$00,$00,$00,$00
                FCB         $08,$14,$10,$38,$10,$10,$20,$40,$00,$10,$18,$1C,$1C,$18,$10,$00
                FCB         $00,$08,$18,$38,$38,$18,$08,$00,$00,$00,$00,$7E,$3C,$18,$00,$00
                FCB         $00,$00,$18,$3C,$7E,$00,$00,$00,$00,$FF,$00,$FF,$FF,$00,$FF,$00
                FCB         $00,$00,$30,$3C,$14,$1C,$00,$00,$00,$7E,$42,$5A,$5A,$42,$7E,$00
                FCB         $00,$7E,$7E,$00,$00,$7E,$7E,$00,$00,$3C,$3C,$3C,$3C,$3C,$3C,$00
                FCB         $00,$00,$7E,$7E,$7E,$7E,$00,$00,$00,$7E,$24,$18,$18,$24,$7E,$00
                FCB         $00,$7F,$00,$7F,$7F,$00,$7F,$00,$00,$FE,$00,$FE,$FE,$00,$FE,$00
                FCB         $38,$44,$40,$40,$40,$44,$38,$10,$44,$00,$44,$44,$44,$4C,$34,$00
                FCB         $08,$10,$38,$44,$7C,$40,$38,$00,$10,$28,$38,$04,$3C,$44,$3C,$00
                FCB         $28,$00,$38,$04,$3C,$44,$3C,$00,$20,$10,$38,$04,$3C,$44,$3C,$00
                FCB         $10,$00,$38,$04,$3C,$44,$3C,$00,$00,$00,$38,$44,$40,$44,$38,$10
                FCB         $10,$28,$38,$44,$7C,$40,$38,$00,$28,$00,$38,$44,$7C,$40,$38,$00
                FCB         $20,$10,$38,$44,$7C,$40,$38,$00,$28,$00,$30,$10,$10,$10,$38,$00
                FCB         $10,$28,$00,$30,$10,$10,$38,$00,$00,$18,$24,$38,$24,$24,$38,$40
                FCB         $44,$10,$28,$44,$7C,$44,$44,$00,$10,$10,$28,$44,$7C,$44,$44,$00
                FCB         $08,$10,$38,$44,$44,$44,$38,$00,$00,$00,$68,$14,$3C,$50,$3C,$00
                FCB         $3C,$50,$50,$78,$50,$50,$5C,$00,$10,$28,$38,$44,$44,$44,$38,$00
                FCB         $28,$00,$38,$44,$44,$44,$38,$00,$00,$00,$38,$4C,$54,$64,$38,$00
                FCB         $10,$28,$00,$44,$44,$4C,$34,$00,$20,$10,$44,$44,$44,$4C,$34,$00
                FCB         $38,$4C,$54,$54,$54,$64,$38,$00,$44,$38,$44,$44,$44,$44,$38,$00
                FCB         $28,$44,$44,$44,$44,$44,$38,$00,$38,$40,$38,$44,$38,$04,$38,$00
                FCB         $08,$14,$10,$38,$10,$50,$3C,$00,$10,$10,$7C,$10,$10,$00,$7C,$00
                FCB         $10,$28,$10,$00,$00,$00,$00,$00,$08,$14,$10,$38,$10,$10,$20,$40
                FCB         $00,$10,$18,$1C,$1C,$18,$10,$00,$00,$08,$18,$38,$38,$18,$08,$00

; START OF ADDITIONAL VARIABLES USED BY SUPER EXTENDED BASIC
;
; WE JUST SET THEIR LOCATIONS USING EQU EQUATES BECAUSE THEY ARE
; NOT INITIALIZED WITH MEANINGFUL VALUES IN ROM.
H.CRSLOC        EQU         $FE00           ; CURRENT LOCATION OF CURSOR
H.CURSX         EQU         $FE02           ; X POSITION OF CURSOR
H.CURSY         EQU         $FE03           ; Y POSITION OF CURSOR
H.COLUMN        EQU         $FE04           ; COLUMNS ON HI-RES ALPHA SCREEN
H.ROW           EQU         $FE05           ; ROWS ON HI-RES ALPHA SCREEN
H.DISPEN        EQU         $FE06           ; END OF HI-RES DISPLAY SCREEN
H.CRSATT        EQU         $FE08           ; CURRENT CURSOR'S ATTRIBUTES
H.FCOLOR        EQU         $FE0A           ; FOREGROUND COLOR
H.BCOLOR        EQU         $FE0B           ; BACKGROUND COLOR
H.ONBRK         EQU         $FE0C           ; ON BRK GOTO LINE NUMBER
H.ONERR         EQU         $FE0E           ; ON ERR GOTO LINE NUMBER
H.ERROR         EQU         $FE10           ; ERROR NUMBER ENCOUNTERED OR $FF (NO ERROR)
H.ONERRS        EQU         $FE11           ; ON ERR SOURCE LINE NUMBER
H.ERLINE        EQU         $FE13           ; LINE NUMBER WHERE ERROR OCCURRED
H.ONBRKS        EQU         $FE15           ; ON BRK SOURCE LINE NUMBER
H.ERRBRK        EQU         $FE17           ; STILL UNKNOWN, HAS TO DO WITH ERR, BRK
H.PCOUNT        EQU         $FE18           ; PRINT COUNT, CHARACTERS TO BE HPRINTED
H.PBUF          EQU         $FE19           ; PRINT BUFFER, HPRINT CHARS. STORED HERE
INT.FLAG        EQU         $FEED           ; INTERRUPT VALID FLAG. 0=NOT VALID, $55=VALID

INT.JUMP        EQU         $FEEE
INT.SWI3        EQU         $FEEE
INT.SWI2        EQU         $FEF1
INT.FIRQ        EQU         $FEF4
INT.IRQ         EQU         $FEF7
INT.SWI         EQU         $FEFA
INT.NMI         EQU         $FEFD

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$00,$00,$00,$00,$00,$00,$00,$00
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FCB         $00,$00,$00,$00,$00,$00,$00,$00,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
                FDB         $0000
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCB         $20,$0B,$80,$00,$08,$2E,$40,$04,$F9,$7F,$F7,$DF,$FC,$FF,$FD,$7F
                FCB         $F1,$7F,$E5,$0F,$F3,$3F,$EB,$BF,$90,$0E,$30,$05,$10,$0E,$70,$05
                FCB         $F7,$2F,$F7,$2F,$F3,$BF,$FB,$6F,$A0,$0B,$94,$0D,$80,$04,$70,$01
                FCB         $F6,$AF,$F7,$8F,$F5,$BF,$F7,$AF,$00,$0C,$20,$0D,$20,$04,$32,$00
                FCB         $F1,$BF,$F7,$1F,$F3,$AF,$FF,$0F,$60,$08,$30,$07,$00,$05,$F2,$05
                FCB         $FF,$DF,$FD,$CF,$F7,$BF,$FF,$5F,$50,$08,$30,$0F,$20,$00,$70,$03
                FCB         $F6,$AF,$FD,$4F,$FD,$BF,$FF,$DF,$10,$0F,$C4,$05,$80,$07,$70,$00
                FCB         $F5,$8F,$F7,$4F,$F0,$6F,$F3,$0F,$A0,$0A,$00,$0C,$91,$00,$70,$09
                FCB         $F1,$7F,$F5,$FD,$F5,$CF,$F7,$4F,$20,$06,$C1,$0D,$00,$08,$72,$0D
                FCB         $FC,$3F,$FD,$3F,$F7,$DF,$FF,$9F,$20,$0C,$A0,$05,$B1,$21,$72,$01
                FCB         $F5,$DF,$F9,$2F,$F7,$AF,$F3,$DF,$20,$08,$60,$05,$80,$06,$70,$01
                FCB         $FB,$7F,$FF,$CF,$FF,$1F,$F3,$9F,$00,$08,$B0,$04,$11,$09,$F3,$00
                FCB         $F1,$9F,$F5,$8F,$F7,$DF,$FB,$AF,$A0,$0D,$91,$05,$00,$00,$70,$01
                FCB         $F5,$1F,$F9,$3F,$F5,$AF,$FD,$AF,$00,$0B,$80,$05,$21,$0C,$72,$04
                FCB         $DD,$1F,$FD,$4F,$F3,$6F,$FB,$AF,$A0,$0D,$41,$07,$30,$04,$D2,$04
                FCB         $F0,$3F,$B1,$2F,$F7,$1F,$F9,$1F,$80,$0C,$A0,$0D,$80,$0B,$51,$0E
                FCB         $F1,$2F,$F4,$DF,$FD,$8F,$F7,$1F,$50,$07,$21,$07,$30,$8C,$F0,$00
                FCB         $41,$0A,$20,$0E,$40,$0D,$F1,$05,$FA,$3F,$F5,$8F,$F3,$4F,$FF,$CF
                FCB         $90,$0C,$21,$05,$E0,$23,$70,$00,$F6,$0F,$F7,$8F,$DF,$DF,$F6,$9F
                FCB         $30,$0E,$10,$0D,$80,$04,$53,$01,$FD,$FF,$F5,$3F,$F5,$FF,$F7,$2F
                FCB         $20,$03,$24,$07,$70,$08,$61,$0C,$F6,$9F,$FE,$AF,$F3,$CF,$FF,$BF
                FCB         $04,$00,$50,$05,$30,$84,$73,$00,$FD,$5F,$FD,$2F,$F1,$DF,$FB,$9F
                FCB         $50,$0C,$90,$07,$B0,$01,$71,$04,$F9,$8F,$FD,$0F,$F5,$5F,$FB,$FF
                FCB         $54,$0E,$20,$0F,$00,$0A,$F0,$03,$F1,$0F,$FD,$8F,$F0,$AF,$F2,$8F
                FCB         $20,$38,$81,$0D,$B0,$04,$71,$00,$FC,$BF,$B1,$8F,$F5,$4F,$FB,$BF
                FCB         $90,$0F,$E1,$01,$E0,$0C,$70,$00,$F0,$EF,$FD,$3F,$F3,$7F,$FB,$0F
                FCB         $F0,$0C,$E0,$07,$80,$08,$70,$01,$FD,$9F,$F5,$0F,$F3,$CF,$F2,$8F
                FCB         $00,$0D,$D1,$05,$E0,$04,$F0,$03,$F1,$0F,$F6,$EF,$F3,$07,$F6,$4B
                FCB         $80,$0A,$90,$07,$10,$06,$70,$00,$FF,$9F,$F4,$2F,$F3,$2F,$F7,$0F
                FCB         $00,$0F,$B1,$0D,$20,$04,$F3,$03,$F7,$1F,$F7,$0F,$F9,$2F,$F7,$5F
                FCB         $A0,$49,$80,$0D,$40,$83,$F3,$05,$F4,$6F,$F5,$EF,$F3,$9F,$FE,$0F
                FCB         $90,$08,$C0,$09,$C0,$05,$F2,$05,$F5,$0F,$F5,$0F,$F7,$AF,$F3,$8F
                FDB         $800B
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                FDB         INT.SWI3
                FDB         INT.SWI2
                FDB         INT.FIRQ
                FDB         INT.IRQ
                FDB         INT.SWI
                FDB         INT.NMI
                FDB         DLDBUG
                END
