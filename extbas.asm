; Extended Color BASIC 1.1
; Copied from the PDF version of Extended Color BASIC Unravelled.
; Fixed up to assemble in Mamou

; Revision History
; 04/19/2013 added conditional assembly of any version:
; VEREXTBAS=10 ; for Extended Color Basic 1.0
; VEREXTBAS=11 ; for Extended Color Basic 1.1
; VEREXTBAS=20 ; for Extended Color Basic 2.0 (default)
;
; 04/05/2009 r23 Extended Color BASIC 1.1 (Match ROM)

; $Id: $

                ORG         $8000
MAGIC           FCC         'EX'

; MOVE EXTENDED BASIC'S COMMAND INTERPRETATION TABLE FROM ROM TO RAM
CPYROM          LDX         #L80DE          ; ROM ADDRESS
                LDU         #COMVEC+10      ; RAM ADDRESS
                LDB         #10             ; 10 BYTES TO MOVE
                JSR         >LA59A          ; MOVE B BYTES FROM (X) TO (U)
                LDX         #LB277          ; ADDRESS OF SYNTAX ERROR
                STX         $03,U           ; PUT SYNTAX ERROR IN ADDRESS OF DISK
                STX         $08,U           ; BASICS COMMAND INTERPRETATION LOOPS
                LDX         #XIRQSV         ; PUT EXBASICS IRQ SERVICING ROUTINE
                STX         IRQVEC+1        ; ADDRESS IN THE IRQ VECTOR
                LDX         ZERO            ; GET X=0
                STX         TIMVAL          ; INITIALIZE TIMER = 0
                JSR         >XVEC18         ; INITIALIZE A BUNCH OF VARIABLES
                LDD         #$2C05          ; INITIALIZE DLOAD TO 1200 BAUD AND
                STD         DLBAUD          ; TIMEOUT CONSTANT TO 5
                LDX         #USR0           ; INITIALIZE ADDRESS OF START OF
                STX         USRADR          ; USR JUMP TABLE
; INITIALIZE THE USR CALLS TO FC ERROR
                LDU         #LB44A          ; ADDRESS OF FC ERROR ROUTINE
                LDB         #10             ; 10 USR CALLS IN EX BASIC
L8031           STU         ,X++            ; STORE FC ERROR AT USR ADDRESSES
                DECB                        ;  FINISHED ALL 10?
                BNE         L8031           ; NO
; MODIFY THE RAM HOOKS FOR THE NEW ROUTINES CONTAINED IN EXT BASIC
                LDA         #$7E            ; OP CODE OF JMP
                STA         RVEC20          ;
                LDX         #XVEC20         ;
                STX         RVEC20+1        ; COMMAND INTERPRETATION LOOP
                STA         RVEC15          ;
                LDX         #XVEC15         ;
                STX         RVEC15+1        ; EXPRESSION EVALUATION
                STA         RVEC19          ;
                LDX         #XVEC19         ;
                STX         RVEC19+1        ; ASCII TO FLOATING POINT CONVERSION
                STA         RVEC9           ;
                LDX         #XVEC9          ;
                STX         RVEC9+1         ; PRINT
                STA         RVEC17          ;
                LDX         #XVEC17         ;
                STX         RVEC17+1        ; ERROR DRIVER
                STA         RVEC4           ;
                LDX         #XVEC4          ;
                STX         RVEC4+1         ; CONSOLE IN
                STA         RVEC3           ;
                LDX         #XVEC3          ;
                STX         RVEC3+1         ; CONSOLE OUT
                STA         RVEC8           ;
                LDX         #XVEC8          ;
                STX         RVEC8+1         ; CLOSE A FILE
                STA         RVEC23          ;
                LDX         #XVEC23         ;
                STX         RVEC23+1        ; CRUNCH A BASIC LINE
                STA         RVEC18          ;
                LDX         #XVEC18         ;
                STX         RVEC18+1        ; RUN
                STA         EXPJMP          ; STORE OP CODE OF JMP
                LDX         #L8489          ; GET EXPONENTIATION ADDRESS
                STX         EXPJMP+1        ; SAVE IT
                JSR         >L96E6          ; GO INITIALIZE EXBAS GRAPHICS VARIABLES
                LDA         PIA0+3          ; ENABLE PIA0 TO
                ORA         #$01            ; PASS 60HZ
                STA         PIA0+3          ; INTERRUPT TO MPU
                LDX         #$444B          ; 'DK' FIRST TWO BYTES OF DISK ROM
                CMPX        DOSBAS          ; COMPARE TO DISK ROM ADDRESS
                LBEQ        DOSBAS+2        ; BRANCH IF DISK BASIC EXISTS
                ANDCC       #$AF            ; ENABLE INTERRUPTS
L80B2           LDX         #L80E6+1        ; POINT TO SIGN ON MESSAGE
                JSR         STRINOUT        ; DISPLAY IT
L80B8           LDX         #XBWMST         ; GET EXBAS WARM START (RESET) VECTOR
                STX         RSTVEC          ; SAVE IT
                JMP         >LA0E2          ; SET WARM START FLAG, ENTER BASIC
; EXBAS WARM START ENTRY POINT
; -----------------------------------------------------------------------------
                if          VEREXTBAS<20
; -----------------------------------------------------------------------------
XBWMST          NOP                         ;  WARM START ENABLE
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
XBWMST          FCB         $ff
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                CLR         PLYTMR
                CLR         PLYTMR+1        ; CLEAR PLAY TIMER
                LDA         PIA0+3          ; ENABLE PIA0 TO
                ORA         #$01            ; PASS 60HZ
                STA         PIA0+3          ; INTERRUPT TO MPU
                JMP         BAWMST          ; JUMP TO BASICS WARM START



; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
; THIS CODE IS NOT USED BY ANY OF THE BASICS
L80D0           LDA         PIA1+2          ; READ PIA PORT B
                BITA        #2              ; CHECK MEM SIZE JUMPER
                BNE         L80DA           ; BRANCH IF HIGH
                STA         SAMREG+29       ; SET SAM CNTL REG MEM SIZE TO 64K
L80DA           JMP         ,X              ; JUMP TO ADDRESS IN X REG
                FCB         $00,$00         ; DEAD SPACE
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; THIS CODE IS A PATCH TO FIX THE PCLEAR BUG
L80D0           LDA         CURLIN          ; GET THE CURRENT LINE NUMBER
                INCA                        ;  TEST FOR DIRECT MODE
                BEQ         L80DD           ; RETURN IF DIRECT MODE
                TFR         Y,D             ; SAVE OFFSET IN ACCD
                SUBD        TXTTAB          ; SUBTRACT OUT START OF BASIC
                ADDD        CHARAD          ; ADD THE CURRENT BASIC INPUT POINTER
                STD         CHARAD          ; SAVE NEW BASIC INPUT POINTER
L80DD           RTS
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



L80DE           FCB         25              ; 25 EXBAS COMMANDS
L80DF           FDB         L8183           ; EXBAS RESERVED WORD DICTIONARY TABLE
L80E1           FDB         L813C           ; EXBAS RESERVED WORD HANDLER
L80E3           FCB         14              ; 14 EXBAS SECONDARY COMMANDS
L80E4           FDB         L821E           ; EXBAS SECONDARY RESERVED WORD TABLE
L80E6           FDB         L8168           ; EXBAS SECONDARY RESERVED WORD HANDLER
L80E8           FCC         'EXTENDED COLOR BASIC '



; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
                FCC         '1.0'           ; MINOR VERSION NUMBER
; -----------------------------------------------------------------------------
                else
                if          VEREXTBAS<20
; -----------------------------------------------------------------------------
                FCC         '1.1'           ; MINOR VERSION NUMBER
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                FCC         '2.0'           ; MINOR VERSION NUMBER
; -----------------------------------------------------------------------------
                endif
                endif
; -----------------------------------------------------------------------------



L8100           FCB         CR

; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
L8101           FCC         'COPYRIGHT (C) 1981 BY TANDY' ; COPYRIGHT YEAR
; -----------------------------------------------------------------------------
                else
                if          VEREXTBAS<20
; -----------------------------------------------------------------------------
L8101           FCC         'COPYRIGHT (C) 1982 BY TANDY' ; COPYRIGHT YEAR
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
L8101           FCC         'COPR. 1982, 1986 BY TANDY  ' ; COPYRIGHT YEAR
; -----------------------------------------------------------------------------
                endif
                endif
; -----------------------------------------------------------------------------



L811C           FCB         CR
L811D           FCC         'UNDER LICENSE FROM MICROSOFT'
L8139           FCB         CR,CR,0
; EXBAS COMMAND INTERPRETATION LOOP
L813C           CMPA        #$CB            ; $CB IS LARGEST EX BASIC COMMAND TOKEN
                BHI         L8148           ; BRANCH IF > LARGEST TOKEN
                LDX         #L81F0          ; POINT X TO EXBAS DISPATCH TABLE FOR COMMANDS
                SUBA        #$B5            ; $B5 IS SMALLEST EXBAS TOKEN
                JMP         >LADD4          ; INTERPRET BASIC TOKEN HANDLER
L8148           CMPA        #$FF            ; CHECK FOR SECONDARY TOKEN
                BEQ         L8154           ; BRANCH IF IT IS SECONDARY
                CMPA        #$CD            ; LARGEST EXBAS TOKEN
                BLS         L8165           ; SYNTAX ERROR FOR USING & FN
L8150           JMP         [COMVEC+23]     ; GO TO DISK BASIC RESERVED WORD HANDLER
L8154           JSR         GETNCH          ; GET AN INPUT CHARACTER FROM BASIC
                CMPA        #$90            ; TOKEN FOR MID$
                LBEQ        L86D6           ; BRANCH IF MID$
                CMPA        #$9F            ; TOKEN FOR TIMER
                LBEQ        L8960           ; BRANCH IF TIMER
                JSR         RVEC22          ; HOOK INTO RAM
L8165           JMP         >LB277          ; SYNTAX ERROR
; EXBAS SECONDARY COMMAND HANDLER
L8168           CMPB        #2*33           ; 80+33 IS LARGEST EXBAS SECONDARY COMMAND
                BLS         L8170           ; BRANCH IF LEGITIMATE EXBAS SECONDARY TOKEN
L816C           JMP         [COMVEC+28]     ; GO TO DISK BASIC SECONDARY COMMAND HANDLER
L8170           SUBB        #2*20           ; SUBTRACT OUT 20 BASIC SECONDARY COMMANDS
                CMPB        #2*8            ; HEX$ TOKEN
                BHI         L817D           ; BRANCH IF > HEX$
                PSHS        B               ; SAVE TOKEN OFFSET
                JSR         >LB262          ; EVALUATE EXPRESSION IN PARENTHESES
                PULS        B               ; GET TOKEN OFFSET BACK
L817D           LDX         #L8257          ; EXBAS SECONDARY COMMAND JUMP TABLE
                JMP         >LB2CE          ; JUMP TO SECONDARY FUNCT1ON HANDLER
; RESERVED WORD TABLE FOR EXTENDED BASIC
; TOKEN #
L8183           FCS         'DEL'           ; B5
L8186           FCS         'EDIT'          ; B6
L818A           FCS         'TRON'          ; B7
L818E           FCS         'TROFF'         ; B8
L8193           FCS         'DEF'           ; B9
L8196           FCS         'LET'           ; BA
L8199           FCS         'LINE'          ; BB
L819D           FCS         'PCLS'          ; BC
L81A1           FCS         'PSET'          ; BD
L81A5           FCS         'PRESET'        ; BE
L81AB           FCS         'SCREEN'        ; BF
L81B1           FCS         'PCLEAR'        ; C0
L81B7           FCS         'COLOR'         ; C1
L81BC           FCS         'CIRCLE'        ; C2
L81C2           FCS         'PAINT'         ; C3
L81C7           FCS         'GET'           ; C4
L81CA           FCS         'PUT'           ; C5
L81CD           FCS         'DRAW'          ; C6
L81D1           FCS         'PCOPY'         ; C7
L81D6           FCS         'PMODE'         ; C8
L81DB           FCS         'PLAY'          ; C9
L81DF           FCS         'DLOAD'         ; CA
L81E4           FCS         'RENUM'         ; CB
L81E9           FCS         'FN'            ; CC
L81EB           FCS         'USING'         ; CD
; DISPATCH TABLE FOR EXTENDED BASIC COMMANDS
; TOKEN #
L81F0           FDB         DEL             ; DEL B5
L81F2           FDB         EDIT            ; EDIT B6
L81F4           FDB         TRON            ; TRON B7
L81F6           FDB         TROFF           ; TROFF B8
L81F8           FDB         DEF             ; DEF B9
L81FA           FDB         LET             ; LET BA
L81FC           FDB         LINE            ; LINE BB
L81FE           FDB         PCLS            ; PCLS BC
L8200           FDB         PSET            ; PSET BD
L8202           FDB         PRESET          ; PRESET BE
L8204           FDB         SCREEN          ; SCREEN BF
L8206           FDB         PCLEAR          ; PCLEAR C0
L8208           FDB         COLOR           ; COLOR C1
L820A           FDB         CIRCLE          ; CIRCLE C2
L820C           FDB         PAINT           ; PAINT C3
L820E           FDB         GET             ; GET C4
L8210           FDB         PUT             ; PUT C5
L8212           FDB         DRAW            ; DRAW C6
L8214           FDB         PCOPY           ; PCOPY C7
L8216           FDB         PMODETOK        ; PMODE C7
L8218           FDB         PLAY            ; PLAY C9
L821A           FDB         DLOAD           ; DLOAD CA
L821C           FDB         RENUM           ; RENUM CB
; SECONDARY FUNCTION FOR EXTENDED BASIC
; TOKENS ARE PRECEEDED WITH AN $FF BYTE
; TOKEN #
L821E           FCS         'ATN'           ; 94
L8221           FCS         'COS'           ; 95
L8224           FCS         'TAN'           ; 96
L8227           FCS         'EXP'           ; 97
L822A           FCS         'FIX'           ; 98
L822D           FCS         'LOG'           ; 99
L8230           FCS         'POS'           ; 9A
L8233           FCS         'SQR'           ; 9B
L8236           FCS         'HEX$'          ; 9C
L823A           FCS         'VARPTR'        ; 9D
L8240           FCS         'INSTR'         ; 9E
L8245           FCS         'TIMER'         ; 9F
L824A           FCS         'PPOINT'        ; A0
L8250           FCS         'STRING$'       ; A1
; JUMP TABLE FOR EXTENDED BASIC SECONDARY FUNCTIONS
; TOKEN #
L8257           FDB         ATN             ; ATN 94
L8259           FDB         COS             ; COS 95
L825B           FDB         TAN             ; TAN 96
L825D           FDB         BEXP            ; EXP 97
L825F           FDB         FIX             ; FIX 98
L8261           FDB         LOG             ; LOG 99
L8263           FDB         POS             ; POS 9A
L8265           FDB         SQR             ; SQR 9B
L8267           FDB         HEXDOL          ; HEXDOL 9C
L8269           FDB         VARPTRTOK       ; VARPT 9D
L826B           FDB         INSTR           ; INSTR 9E
L826D           FDB         TIMER           ; TIMER 9F
L826F           FDB         PPOINT          ; PPOINT A0
L8271           FDB         STRING          ; STRING A1
; CONSOLE OUT RAM HOOK
XVEC3           TST         DEVNUM          ; CHECK DEVICE NUMBER
                LBEQ        L95AC           ; BRANCH IF SCREEN
                PSHS        B               ; SAVE CHARACTER
                LDB         DEVNUM          ; GET DEVICE NUMBER AND
                CMPB        #-3             ; CHECK FOR DLOAD
                PULS        B               ; GET CHARACTER BACK
                BNE         L8285           ; RETURN IF NOT DLOAD
                LEAS        $02,S           ; TAKE RETURN OFF STACK & GO BACK TO ROUTINE
; THAT CALLED CONSOLE OUT
L8285           RTS
; CLOSE FILES RAM HOOK - THIS CODE CORRECTS A
; BUG IN 1.0 BASIC WHICH WAS FIXED BY 1.1 BASIC
XVEC8           LDA         DEVNUM          ; GET DEVICE NUMBER
                INCA                        ;  CHECK FOR CASSETTE
                BNE         L8285           ; RETURN IF NOT CASSETTE
                LDA         FILSTA          ; GET FILE STATUS
                CMPA        #$02            ; OPEN FOR OUTPUT?
                BNE         L8285           ; RETURN IF NOT OPEN FOR OUTPUT
                LDA         CINCTR          ; GET CHARACTER BUFFER COUNTER
                BNE         L8285           ; RETURN IF NOT EMPTY
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                LEAS        $02,S           ; GET RETURN ADDRESS OFF OF STACK
                JMP         >LA444          ; WRITE END OF FILE TAPE BLOCK
; RUN RAM HOOK
XVEC18          LDD         #$BA42          ; MID HIGH VALUE + MID LOW VALUE
                STD         VOLHI           ; INITIALIZE PLAY VOLUME
                LDA         #$02
                STA         TEMPO           ; INITIALIZE TEMPO TO 2
                STA         OCTAVE          ; INITIALIZE OCTAVE TO 3
                ASLA                        ;  X2
                STA         NOTELN          ; INITIALIZE NOTE LENGTH TO 5
                CLR         DOTVAL          ; LEAR NOTE TIMER SCALE FACTOR
                LDD         ZERO            ; ZERO ACCD
                STD         ANGLE           ; INITIALIZE DRAW ANGLE AND SCALE TO 1
                LDB         #128            ; INITIALIZE HORIZONTAL DEFAULT
                STD         HORDEF          ; COORDINATE TO MID POSITION
                LDB         #96             ; INITIALIZE VERTICAL DEFAULT
                STD         VERDEF          ; COORDINATE TO MID POSITION
                RTS
; COMMAND INTERPRETATION LOOP RAM HOOK
XVEC20          LEAS        $02,S           ; PURGE RETURN ADDRESS FROM STACK
L82BB           ANDCC       #$AF            ; ENABLE INTERRUPTS
                JSR         >LADEB          ; CHECK FOR KEYBOARD BREAK
                LDX         CHARAD          ; GET CURRENT BASIC LINE
                STX         TINPTR          ; POINTER AND SAVE IT
                LDA         ,X+             ; GET CURRENT INPUT CHARACTER AND ADVANCE POINTER
                BEQ         L82CF           ; BRANCH IF END OF LINE
                CMPA        #':             ; CHECK FOR COLON
                BEQ         L82F1           ; CONTINUE INTERPRETING IF COLON
                JMP         >LB277          ; SNYTAX ERROR - COLON ONLY LEGAL LINE SEPARATOR
L82CF           LDA         ,X++            ; GET 1ST BYTE OF ADDRESS OF NEXT
                STA         ENDFLG          ; BASIC LINE AND SAVE IT
                BNE         L82D8           ; BRANCH IF NOT END OF PROGRAM
                JMP         >LAE15          ; RETURN TO DIRECT MODE - PRINT OK
L82D8           LDD         ,X+             ; GET LINE NUMBER OF NEXT LINE
                STD         CURLIN          ; SAVE LINE NUMBER
                STX         CHARAD          ; SAVE ADDRESS NEXT BYTE TO INTERPRET
                LDA         TRCFLG          ; TEST THE TRACE FLAG
                BEQ         L82F1           ; BRANCH IF TRACE OFF
                LDA         #$5B            ; <LEFT HAND MARKER FOR TRON LINE NUMBER
                JSR         PUTCHR          ; OUTPUT A CHARACTER
                LDA         CURLIN          ; GET MS BYTE OF LINE NUMBER
                JSR         >LBDCC          ; CONVERT ACCD TO DECIMAL AND PRINT ON SCREEN
                LDA         #$5D            ; > RIGHT HAND MARKER FOR TRON LINE NUMBER
                JSR         PUTCHR          ; OUTPUT A CHARACTER
L82F1           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                TFR         CC,B            ; SAVE STATUS IN ACCB
                CMPA        #$98            ; CSAVE TOKEN
                BEQ         L8316           ; GO DO A CSAVE
                CMPA        #$97            ; CLOAD TOKEN
                BEQ         L8311           ; PROCESS CLOAD
                TFR         B,CC            ; GET STATUS REG BACK
                JSR         >LADC6          ; LINK BACK TO BASICS INTERPRETATION LOOP
                BRA         L82BB           ; GO TO MAIN INTERPRETATION LOOP
; CRUNCH RAM HOOK
XVEC23          LDX         $02,S           ; CHECK TO SEE IF THE ROUTINE CALLING CRUNCH
                CMPX        #LAC9D          ; IS COMING FROM THE MAIN LOOP IN BASIC
                BNE         L8310           ; AND BRANCH IF NOT
                LDX         #L82F1          ; IF IT IS, DO NOT RETURN TO COLOR BASIC
                STX         $02,S           ; BUT TO THE EXBAS PATCH INSTEAD
L8310           RTS
L8311           JSR         >L8C62          ; CHECK EXBAS CLOAD HANDLER
                BRA         L82BB           ; GO TO MAIN INTERPRETATION LOOP
L8316           BSR         L831A           ; DO A CSAVE
                BRA         L82BB           ; GO TO MAIN INTERPRETATION LOOP
L831A           JSR         GETNCH          ; GET A CHAR FROM BASIC
                CMPA        #'M             ; CHECK FOR CSAVEM
                LBNE        CSAVE           ; BRANCH IF ITS NOT CSAVEM
; CSAVEM
                JSR         GETNCH          ; GET A CHAR FROM BASIC
                JSR         >LA578          ; GET NAME OF FILE FROM BASIC
                BSR         L836C           ; GO GET THE START ADDRESS
                STX         CASBUF+13       ; PUT IT IN HEADER BUFFER
                BSR         L836C           ; GO GET END ADDRESS
                CMPX        $02,S           ; COMPARE TO START ADDRESS
                LBCS        LB44A           ; FC ERROR IF START > END
                BSR         L836C           ; GO GET XFER ADDRESS
                STX         CASBUF+11       ; PUT IT IN HEADER BUFFER
                JSR         GETCCH          ; GET NEW CHARACTER
                BNE         L8310           ; RETURN IF NOT END OF LINE
                LDA         #$02            ; FILE TYPE (MACHINE LANGUAGE)
                LDX         ZERO            ; X = 0000 FILE MODE AND ASCII FLAG
                JSR         >LA65F          ; WRITE HEADER BLOCK
                CLR         FILSTA          ; CLOSE CASSETTE FILES
                INC         BLKTYP          ; BLOCK TYPE = 1
                JSR         WRLDR           ; GO WRITE LEADER
                LDX         $04,S           ; GET STARTING ADDRESS
L834D           STX         CBUFAD          ; STORE BUFFER START ADOR
                LDA         #255            ; BLOCK SIZE = 2SS
                STA         BLKLEN          ; STORE IN BLOCK SIZE
                LDD         $02,S           ; GET ENDING ADDRESS
                SUBD        CBUFAD          ; SUBTRACT START ADDRESS
                BCC         L835E           ; BRANCH IF MORE TO BE WRITTEN
                LEAS        $06,S           ; CLEAN UP STACK
                JMP         >LA491          ; WRITE FINAL BLOCK
L835E           CMPD        #$00FF          ; AT LEAST 1 FULL BLK LEFT?
                BCC         L8367           ; YES
                INCB                        ;  NO - PUT WHATS LEFT IN BLKLEN
                STB         BLKLEN          ; BUFFER SIZE
L8367           JSR         SNDBLK          ; WRITE A BLOCK
                BRA         L834D           ; GO DO SOME MORE
L836C           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVAL EXPR - RETURN VALUE IN X
                LDU         ,S              ; SAVE RETURN ADDRESS IN U
                STX         ,S              ; PUT THE EXPRESSION ON THE STACK
                TFR         U,PC            ; RETURN TO CALLING ADDRESS
; COS
; THE VALUE OF COS(X) IS DETERMINED BY THE TRIG IDENTITY COS(X)=SIN((PI/2)+X)
COS             LDX         #L83AB          ; POINT X TO FP CONSTANT (P1/2)
                JSR         >LB9C2          ; ADD FPA0 TO (X)
L837E           JMP         SIN             ; JUMP TO SIN ROUTINE
; TAN
; THE VALUE OF TAN(X) IS DETERMINED BY THE TRIG IDENTITY TAN(X)=SIN(X)/COS(X)
TAN             JSR         >LBC2F          ; PACK FPA0 AND MOVE IT TO FPA3
                CLR         RELFLG          ; RESET QUADRANT FLAG
                BSR         L837E           ; CALCULATE SIN OF ARGUMENT
                LDX         #V4A            ; POINT X TO FPA5
                JSR         >LBC35          ; PACK FPA0 AND MOVE IT TO FPA5
                LDX         #V40            ; POINT X TO FPA3
                JSR         >LBC14          ; MOVE FPA3 TO FPA0
                CLR         FP0SGN          ; FORCE FPA0 MANTISSA TO BE POSITIVE
                LDA         RELFLG          ; GET THE QUADRANT FLAG - COS NEGATIVE IN QUADS 2,3
                BSR         L83A6           ; CALCULATE VALUE OF COS(FPA0)
                TST         FP0EXP          ; CHECK EXPONENT OF FPA0
                LBEQ        LBA92           ; OV ERROR IF COS(X)=0
                LDX         #V4A            ; POINT X TO FPA5
L83A3           JMP         >LBB8F          ; DIVIDE (X) BY FPA0 - SIN(X)/COS(X)
L83A6           PSHS        A               ; SAVE SIGN FLAG ON STACK
                JMP         >LBFA6          ; EXPAND POLYNOMIAL
L83AB           FCB         $81,$49,$0F,$DA,$A2 ; 1.57079633 (PI/2)
; ATN
; A 12 TERM TAYLOR SERIES IS USED TO EVALUATE THE
; ARCTAN EXPRESSION. TWO DIFFERENT FORMULI ARE USED
; TO EVALUATE THE EXPRESSION DEPENDING UPON
; WHETHER OR NOT THE ARGUMENT SQUARED IS > OR < 1.0
; IF X**2<1 THEN ATN=X-(X**3)/3+(X**5)/5-(X**7)/7..
; IF X**2>=1 THEN ATN=PI/2-(1/X-1/((X**3)*3)+(1/((X**5)*5)-)
ATN             LDA         FP0SGN          ; GET THE SIGN OF THE MANTISSA AND
                PSHS        A               ; SAVE IT ON THE STACK
                BPL         L83B8           ; BRANCH IF POSITIVE MANTISSA
                BSR         L83DC           ; CHANGE SIGN OF FPA0
L83B8           LDA         FP0EXP          ; GET EXPONENT OF FPA0 AND
                PSHS        A               ; SAVE IT ON THE STACK
                CMPA        #$81            ; IS FPAO < 1.0?
                BLO         L83C5           ; YES
                LDX         #LBAC5          ; POINT X TO FP CONSTANT 1.0
                BSR         L83A3           ; GET RECIPROCAL OF FPA0
L83C5           LDX         #L83E0          ; POINT (X) TO TAYLOR SERIES COEFFICIENTS
                JSR         >LBEF0          ; EXPAND POLYNOMIAL
                PULS        A               ; GET EXPONENT OF ARGUMENT
                CMPA        #$81            ; WAS ARGUMENT < 1.0?
                BLO         L83D7           ; YES
                LDX         #L83AB          ; POINT (X) TO FP NUMBER (PI/2)
                JSR         >LB9B9          ; SUBTRACT FPA0 FROM (PI/2)
L83D7           PULS        A               ; GET SIGN OF INITIAL ARGUMENT MANTISSA
                TSTA                        ;  AND SET FLAGS ACCORDING TO IT
                BPL         L83DF           ; RETURN IF ARGUMENT WAS POSITIVE
L83DC           JMP         >LBEE9          ; CHANGE MANTISSA SIGN OF FPA0
L83DF           RTS

; TCHEBYSHEV MODIFIED TAYLOR SERIES COEFFICIENTS FOR ARCTANGENT
L83E0           FCB         $0B             ; TWELVE COEFFICIENTS
L83E1           FCB         $76,$B3,$83,$BD,$D3 ; -6.84793912E-04 1/23
L83E6           FCB         $79,$1E,$F4,$A6,$F5 ; +4.85094216E-03 1/21
L83EB           FCB         $7B,$83,$FC,$B0,$10 ; -0.0161117018 1/19
L83F0           FCB         $7C,$0C,$1F,$67,$CA ; +0.0342096381 1/17
L83F5           FCB         $7C,$DE,$53,$CB,$C1 ; -0.0542791328 1/15
L83FA           FCB         $7D,$14,$64,$70,$4C ; +0.0724571965 1/13
L83FF           FCB         $7D,$B7,$EA,$51,$7A ; -0.0898023954 1/11
L8404           FCB         $7D,$63,$30,$88,$7E ; +0.110932413 1/9
L8409           FCB         $7E,$92,$44,$99,$3A ; -0.142839808 1/7
L840E           FCB         $7E,$4C,$CC,$91,$C7 ; +0.199999121 1/5
L8413           FCB         $7F,$AA,$AA,$AA,$13 ; -0.333333316 1/3
L8418           FCB         $81,$00,$00,$00,$00 ; +1.000000000 1/1

; TCHEBYSHEV MODIFIED TAYLOR SERIES COEFFICIENTS FOR LN(X)

L841D           FCB         $03             ; FOUR COEFFICIENTS
L841E           FCB         $7F,$5E,$56,$CB,$79 ; 0.434255942 (2/7)*(1/LN(2))
L8423           FCB         $80,$13,$9B,$0B,$64 ; 0.576584541 (2/5)*(1/LN(2))
L8428           FCB         $80,$76,$38,$93,$16 ; 0.961800759 (2/3)*(1/LN(2))
L842D           FCB         $82,$38,$AA,$3B,$20 ; 2.88539007 (2/1)*(1/LN(2))
L8432           FCB         $80,$35,$04,$F3,$34 ; 1/SQR(2)
L8437           FCB         $81,$35,$04,$F3,$34 ; SQR(2)
L843C           FCB         $80,$80,$00,$00,$00 ; -.5
L8441           FCB         $80,$31,$72,$17,$F8 ; LN(2)

; LOG - NATURAL LOGARITHM (LN)
; THE NATURAL OR NAPERIAN LOGARITHM IS CALCULATED USING
; MATHEMATICAL IDENTITIES. FPA0 IS OF THE FORM FPA0=A*(2**B) (SCIENTIFIC
; NOTATION). THEREFORE, THE LOG ROUTINE DETERMINES THE VALUE OF
; LN(A*(2**B)). A SERIES OF MATHEMATICAL IDENTITIES WILL EXPAND THIS
; TERM: LN(A*(2**B))=(-1/2+(1/LN(2))*(LN(A*SQR(2)))+B)*LN(2). ALL OF
; THE TERMS OF THE LATTER EXPRESSION ARE CONSTANTS EXCEPT FOR THE
; LN(A*SQR(2)) TERM WHICH IS EVALUATED USING THE TAYLOR SERIES EXPANSION
LOG             JSR         >LBC6D          ; CHECK STATUS OF FPA0
                LBLE        LB44A           ; FC ERROR IF NEGATIVE OR ZERO
                LDX         #L8432          ; POINT (X) TO FP NUMBER (1/SQR(2))
                LDA         FP0EXP          ; GET EXPONENT OF ARGUMENT
                SUBA        #$80            ; SUBTRACT OFF THE BIAS AND
                PSHS        A               ; SAVE IT ON THE STACK
                LDA         #$80            ; FORCE EXPONENT OF FPA0
                STA         FP0EXP          ; TO BE ZERO
                JSR         >LB9C2          ; ADD FPA0 TO (X)
                LDX         #L8437          ; POINT X TO SQR(2)
                JSR         >LBB8F          ; DIVIDE SQR(2) BY FPA0
                LDX         #LBAC5          ; POINT X TO FP VALUE OF 1.00
                JSR         >LB9B9          ; SUBTRACT FPA0 FROM (X)
; NOW FPA0 = (1-SQR(2)*X)/(1+SQR(2)*X) WHERE X IS ARGUMENT
                LDX         #L841D          ; POINT X TO TABLE OF COEFFICIENTS
                JSR         >LBEF0          ; EXPAND POLYNOMIAL
                LDX         #L843C          ; POINT X TO FP VALUE OF (-.5)
                JSR         >LB9C2          ; ADD FPA0 TO X
                PULS        B               ; GET EXPONENT OF ARGUMENT BACK (WITHOUT BIAS)
                JSR         >LBD99          ; ADD ACCB TO FPA0
                LDX         #L8441          ; POINT X TO LN(2)
                JMP         >LBACA          ; MULTIPLY FPA0 * LN(2)
; SQR
SQR             JSR         >LBC5F          ; MOVE FPA0 TO FPA1
                LDX         #LBEC0          ; POINT (X) TO FP NUMBER (.5)
                JSR         >LBC14          ; COPY A PACKED NUMBER FROM (X) TO FPA0
; ARITHMETIC OPERATOR FOR EXPONENTIATION JUMPS
; HERE. THE FORMULA USED TO EVALUATE EXPONENTIATION
; IS A**X=E**(X LN A) = E**(FPA0*LN(FPA1)), E=2.7182818
L8489           BEQ         BEXP            ; DO A NATURAL EXPONENTIATION IF EXPONENT = 0
                TSTA                        ;  CHECK VALUE BEING EXPONENTIATED
                BNE         L8491           ; AND BRANCH IF IT IS <> 0
                JMP         >LBA3A          ; FPA0=0 IF RAISING ZERO TO A POWER
L8491           LDX         #V4A            ; PACK FPA0 AND SAVE
                JSR         >LBC35          ; IT IN FPA5 (ARGUMENTS EXPONENT)
                CLRB                        ;  ACCB=DEFAULT RESULT SIGN FLAG 0=POSITIVE
                LDA         FP1SGN          ; CHECK THE SIGN OF ARGUMENT
                BPL         L84AC           ; BRANCH IF POSITIVE
                JSR         INT             ; CONVERT EXPONENT INTO AN INTEGER
                LDX         #V4A            ; POINT X TO FPA5 (ORIGINAL EXPONENT)
                LDA         FP1SGN          ; GET MANTISSA SIGN OF FPA1 (ARGUMENT)
                JSR         >LBCA0          ; COMPARE FPA0 TO (X) AND
                BNE         L84AC           ; BRANCH IF NOT EQUAL
                COMA                        ;  TOGGLE FPA1 MANTISSA SIGN - FORCE POSITIVE
                LDB         CHARAC          ; GET LS BYTE OF INTEGER VALUE OF EXPONENT (RESULT SIGN FLAG)
L84AC           JSR         >LBC4C          ; COPY FPA1 TO FPA0; ACCA = MANTISSA SIGN
                PSHS        B               ; PUT RESULT SIGN FLAG ON THE STACK
                JSR         LOG             ; GET NATURAL LOGARITHM OF FPA0
                LDX         #V4A            ; POINT (X) TO FPA5
                JSR         >LBACA          ; MULTIPLY FPA0 BY FPA5
                BSR         BEXP            ; CALCULATE E**(FPA0)
                PULS        A               ; GET RESULT SIGN FLAG FROM THE STACK
                RORA                        ;  AND BRANCH IF NEGATIVE
                LBCS        LBEE9           ; CHANGE SIGN OF FPA0 MANTISSA
                RTS
; CORRECTION FACTOR FOR EXPONENTIAL FUNCTION
L84C4           FCB         $81,$38,$AA,$3B,$29 ; 1.44269504 ( CF )

; TCHEBYSHEV MODIFIED TAYLOR SERIES COEFFICIENTS FOR E**X

L84C9           FCB         $07             ; EIGHT COEFFICIENTS
L84CA           FCB         $71,$34,$58,$3E,$56 ; 2.14987637E-05: 1/(7!*(CF**7))
L84CF           FCB         $74,$16,$7E,$B3,$1B ; 1.4352314E-04 : 1/(6!*(CF**6))
L84D4           FCB         $77,$2F,$EE,$E3,$85 ; 1.34226348E-03: 1/(5!*(CF**5))
L84D9           FCB         $7A,$1D,$84,$1C,$2A ; 9.61401701E-03: 1/(4!*(CF**4))
L84DE           FCB         $7C,$63,$59,$58,$0A ; 0.0555051269 : 1/(3!*(CF**3))
L84E3           FCB         $7E,$75,$FD,$E7,$C6 ; 0.240226385 : 1/(2!*(CF**2))
L84E8           FCB         $80,$31,$72,$18,$10 ; 0.693147186 : 1/(1!*(CF**1))
L84ED           FCB         $81,$00,$00,$00,$00 ; 1.

; EXP ( E**X)
; THE EXPONENTIAL FUNCTION IS EVALUATED BY FIRST MULTIPLYING THE
; ARGUMENT BY A CORRECTION FACTOR (CF). AFTER THIS IS DONE, AN
; ARGUMENT >= 127 WILL YIELD A ZERO RESULT (NO UNDERFLOW) FOR A
; NEGATIVE ARGUMENT OR AN 'OV' (OVERFLOW) ERROR FOR A POSITIVE
; ARGUMENT. THE POLYNOMIAL COEFFICIENTS ARE MODIFIED TO REFLECT
; THE CF MULTIPLICATION AT THE START OF THE EVALUATION PROCESS.
BEXP            LDX         #L84C4          ; POINT X TO THE CORRECTION FACTOR
                JSR         >LBACA          ; MULTIPLY FPA0 BY (X)
                JSR         >LBC2F          ; PACK FPA0 AND STORE IT IN FPA3
                LDA         FP0EXP          ; GET EXPONENT OF FPA0 AND
                CMPA        #$88            ; COMPARE TO THE MAXIMUM VALUE
                BLO         L8504           ; BRANCH IF FPA0 < 128
L8501           JMP         >LBB5C          ; SET FPA0 = 0 OR OV ERROR
L8504           JSR         INT             ; CONVERT FPA0 TO INTEGER
                LDA         CHARAC          ; GET LS BYTE OF INTEGER
                ADDA        #$81            ; WAS THE ARGUMENT =127, IF SO
                BEQ         L8501           ; THEN OV ERROR; THIS WILL ALSO ADD THE $80 BIAS
; REQUIRED WHEN THE NEW EXPONENT IS CALCULATED BELOW
                DECA                        ;  DECREMENT ONE FROM THE EXPONENT, BECAUSE $81, NOT $80 WAS USED ABOVE
                PSHS        A               ; SAVE EXPONENT OF INTEGER PORTION ON STACK
                LDX         #V40            ; POINT (X) TO FPA3
                JSR         >LB9B9          ; SUBTRACT FPA0 FROM (X) - GET FRACTIONAL PART OF ARGUMENT
                LDX         #L84C9          ; POINT X TO COEFFICIENTS
                JSR         >LBEFF          ; EVALUATE POLYNOMIAL FOR FRACTIONAL PART
                CLR         RESSGN          ; FORCE THE MANTISSA TO BE POSITIVE
                PULS        A               ; GET INTEGER EXPONENT FROM STACK
                JSR         >LBB48          ; CALCULATE EXPONENT OF NEW FPA0 BY ADDING THE EXPONENTS OF THE
; INTEGER AND FRACTIONAL PARTS
                RTS
; FIX
FIX             JSR         >LBC6D          ; CHECK STATUS OF FPA0
                BMI         L852C           ; BRANCH IF FPA0 = NEGATIVE
L8529           JMP         INT             ; CONVERT FPA0 TO INTEGER
L852C           COM         FP0SGN          ; TOGGLE SIGN OF FPA0 MANTISSA
                BSR         L8529           ; CONVERT FPA0 TO INTEGER
                JMP         >LBEE9          ; TOGGLE SIGN OF FPA0
; EDIT
EDIT            JSR         >L89AE          ; GET LINE NUMBER FROM BASIC
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
L8538           LDA         #$01            ; LIST FLAG
                STA         VD8             ; SET FLAG TO LIST LINE
                JSR         >LAD01          ; GO FIND THE LINE NUMBER IN PROGRAM
                LBCS        LAED2           ; ERROR #7 UNDEFINED LINE #'
                JSR         >LB7C2          ; GO UNCRUNCH LINE INTO BUFFER AT LINBUF+1
                TFR         Y,D             ; PUT ABSOLUTE ADDRESS OF END OF LINE TO ACCD
                SUBD        #LINBUF+2       ; SUBTRACT OUT THE START OF LINE
                STB         VD7             ; SAVE LENGTH OF LINE
L854D           LDD         BINVAL          ; GET THE HEX VALUE OF LINE NUMBER
                JSR         >LBDCC          ; LIST THE LINE NUMBER ON THE SCREEN
                JSR         >LB9AC          ; PRINT A SPACE
                LDX         #LINBUF+1       ; POINT X TO BUFFER
                LDB         VD8             ; CHECK TO SEE IF LINE IS TO BE
                BNE         L8581           ; LISTED TO SCREEN - BRANCH IF IT IS
L855C           CLRB                        ;  RESET DIGIT ACCUMULATOR - DEFAULT VALUE
L855D           JSR         >L8687          ; GET KEY STROKE
                JSR         >L90AA          ; SET CARRY IF NOT NUMERIC
                BLO         L8570           ; BRANCH IF NOT NUMERIC
                SUBA        #'0             ; MASK OFF ASCII
                PSHS        A               ; SAVE IT ON STACK
                LDA         #10             ; NUMBER BEING CONVERTED IS BASE 10
                MUL                         ;  MULTIPLY ACCUMULATED VALUE BY BASE (10)
                ADDB        ,S+             ; ADD DIGIT TO ACCUMULATED VALUE
                BRA         L855D           ; CHECK FOR ANOTHER DIGIT
L8570           SUBB        #$01            ; REPEAT PARAMETER IN ACCB; IF IT
                ADCB        #$01            ; IS 0, THEN MAKE IT 1
                CMPA        #'A             ; ABORT?
                BNE         L857D           ; NO
                JSR         >LB958          ; PRINT CARRIAGE RETURN TO SCREEN
                BRA         L8538           ; RESTART EDIT PROCESS - CANCEL ALL CHANGES
L857D           CMPA        #'L             ; LIST?
                BNE         L858C           ; NO
L8581           BSR         L85B4           ; LIST THE LINE
                CLR         VD8             ; RESET THE LIST FLAG TO NO LIST
                JSR         >LB958          ; PRINT CARRIAGE RETURN
                BRA         L854D           ; GO INTERPRET ANOTHER EDIT COMMAND
L858A           LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF THE STACK
L858C           CMPA        #CR             ; ENTER KEY?
                BNE         L859D           ; NO
                BSR         L85B4           ; ECHO THE LINE TO THE SCREEN
L8592           JSR         >LB958          ; PRINT CARRIAGE RETURN
                LDX         #LINBUF+1       ; RESET BASICS INPUT POINTER
                STX         CHARAD          ; TO THE LINE INPUT BUFFER
                JMP         >LACA8          ; GO PUT LINE BACK IN PROGRAM
L859D           CMPA        #'E             ; EXIT?
                BEQ         L8592           ; YES - SAME AS ENTER EXCEPT NO ECHO
                CMPA        #'Q             ; QUIT?
                BNE         L85AB           ; NO
                JSR         >LB958          ; PRINT CARRIAGE RETURN TO SCREEN
                JMP         >LAC73          ; GO TO COMMAND LEVEL - MAKE NO CHANGES
L85AB           BSR         L85AF           ; INTERPRET THE REMAINING COMMANDS AS SUBROUTINES
                BRA         L855C           ; GO INTERPRET ANOTHER EDIT COMMAND
L85AF           CMPA        #SPACE          ; SPACE BAR?
                BNE         L85C3           ; NO
L85B3           FCB         SKP2            ; SKIP TWO BYTES
; DISPLAY THE NEXT ACCB BYTES OF THE LINE IN THE BUFFER TO THE SCREEN

L85B4           LDB         #LBUFMX-1       ; 250 BYTES MAX IN BUFFER
L85B6           LDA         ,X              ; GET A CHARACTER FROM BUFFER
                BEQ         L85C2           ; EXIT IF ITS A 0
                JSR         PUTCHR          ; SEND CHAR TO CONSOLE OUT
                LEAX        $01,X           ; MOVE POINTER UP ONE
                DECB                        ;  DECREMENT CHARACTER COUNTER
                BNE         L85B6           ; LOOP IF NOT DONE
L85C2           RTS
L85C3           CMPA        #'D             ; DELETE?
                BNE         L860F           ; NO
L85C7           TST         ,X              ; CHECK FOR END OF LINE
                BEQ         L85C2           ; AND BRANCH IF SO
                BSR         L85D1           ; REMOVE A CHARACTER
                DECB                        ;  DECREMENT REPEAT PARAMETER
                BNE         L85C7           ; BRANCH IF NOT DONE
                RTS
; REMOVE ONE CHARACTER FROM BUFFER
L85D1           DEC         VD7             ; DECREMENT LENGTH OF BUFFER
                LEAY        -1,X            ; POINT Y TO ONE BEFORE CURRENT BUFFER POINTER
L85D5           LEAY        $01,Y           ; INCREMENT TEMPORARY BUFFER POINTER
                LDA         $01,Y           ; GET NEXT CHARACTER
                STA         ,Y              ; PUT IT IN CURRENT POSITION
                BNE         L85D5           ; BRANCH IF NOT END OF LINE
                RTS
L85DE           CMPA        #'I             ; INSERT?
                BEQ         L85F5           ; YES
                CMPA        #'X             ; EXTEND?
                BEQ         L85F3           ; YES
                CMPA        #'H             ; HACK?
                BNE         L8646           ; NO
                CLR         ,X              ; TURN CURRENT BUFFER POINTER INTO END OF LINE FLAG
                TFR         X,D             ; PUT CURRENT BUFFER POINTER IN ACCD
                SUBD        #LINBUF+2       ; SUBTRACT INITIAL POINTER POSITION
                STB         VD7             ; SAVE NEW BUFFER LENGTH
L85F3           BSR         L85B4           ; DISPLAY THE LINE ON THE SCREEN
L85F5           JSR         >L8687          ; GET A KEYSTROKE
                CMPA        #CR             ; ENTER KEY?
                BEQ         L858A           ; YES - INTERPRET ANOTHER COMMAND - PRINT LINE
                CMPA        #ESC            ; ESCAPE?
                BEQ         L8625           ; YES - RETURN TO COMMAND LEVEL - DONT PRINT LINE
                CMPA        #BS             ; BACK SPACE?
                BNE         L8626           ; NO
                CMPX        #LINBUF+1       ; COMPARE POINTER TO START OF BUFFER
                BEQ         L85F5           ; DO NOT ALLOW BS IF AT START
                BSR         L8650           ; MOVE POINTER BACK ONE, BS TO SCREEN
                BSR         L85D1           ; REMOVE ONE CHARACTER FROM BUFFER
                BRA         L85F5           ; GET INSERT SUB COMMAND
L860F           CMPA        #'C             ; CHANGE?
                BNE         L85DE           ; NO
L8613           TST         ,X              ; CHECK CURRENT BUFFER CHARACTER
                BEQ         L8625           ; BRANCH IF END OF LINE
                JSR         >L8687          ; GET A KEYSTROKE
                BLO         L861E           ; BRANCH IF LEGITIMATE KEY
                BRA         L8613           ; TRY AGAIN IF ILLEGAL KEY
L861E           STA         ,X+             ; INSERT NEW CHARACTER INTO BUFFER
                BSR         L8659           ; SEND NEW CHARACTER TO SCREEN
                DECB                        ;  DECREMENT REPEAT PARAMETER
                BNE         L8613           ; BRANCH IF NOT DONE
L8625           RTS
L8626           LDB         VD7             ; GET LENGTH OF LINE
                CMPB        #LBUFMX-1       ; COMPARE TO MAXIMUM LENGTH
                BNE         L862E           ; BRANCH IF NOT AT MAXIMUM
                BRA         L85F5           ; IGNORE INPUT IF LINE AT MAXIMUM LENGTH
L862E           PSHS        X               ; SAVE CURRENT BUFFER POINTER
L8630           TST         ,X+             ; SCAN THE LINE UNTIL END OF
                BNE         L8630           ; LINE (0) IS FOUND
L8634           LDB         ,-X             ; DECR TEMP LINE POINTER AND GET A CHARACTER
                STB         $01,X           ; PUT CHARACTER BACK DOWN ONE SPOT
                CMPX        ,S              ; HAVE WE REACHED STARTING POINT?
                BNE         L8634           ; NO - KEEP GOING
                LEAS        $02,S           ; PURGE BUFFER POINTER FROM STACK
                STA         ,X+             ; INSERT NEW CHARACTER INTO THE LINE
                BSR         L8659           ; SEND A CHARACTER TO CONSOLE OUT
                INC         VD7             ; ADD ONE TO BUFFER LENGTH
                BRA         L85F5           ; GET INSERT SUB COMMAND
L8646           CMPA        #BS             ; BACKSPACE?
                BNE         L865C           ; NO
L864A           BSR         L8650           ; MOVE POINTER BACK 1, SEND BS TO SCREEN
                DECB                        ;  DECREMENT REPEAT PARAMETER
                BNE         L864A           ; LOOP UNTIL DONE
                RTS
L8650           CMPX        #LINBUF+1       ; COMPARE POINTER TO START OF BUFFER
                BEQ         L8625           ; DO NOT ALLOW BS IF AT START
                LEAX        -1,X            ; MOVE POINTER BACK ONE
                LDA         #BS             ; BACK SPACE
L8659           JMP         PUTCHR          ; SEND TO CONSOLE OUT
L865C           CMPA        #'K             ; KILL?
                BEQ         L8665           ; YES
                SUBA        #'S             ; SEARCH?
                BEQ         L8665           ; YES
                RTS
L8665           PSHS        A               ; SAVE KILL/SEARCH FLAG ON STACK
                BSR         L8687           ; GET A KEYSTROKE (TARGET CHARACTER)
                PSHS        A               ; AND SAVE IT ON STACK
L866B           LDA         ,X              ; GET CURRENT BUFFER CHARACTER
                BEQ         L8685           ; AND RETURN IF END OF LINE
                TST         $01,S           ; CHECK KILL/SEARCH FLAG
                BNE         L8679           ; BRANCH IF KILL
                BSR         L8659           ; SEND A CHARACTER TO CONSOLE OUT
                LEAX        $01,X           ; INCREMENT BUFFER POINTER
                BRA         L867C           ; CHECK NEXT INPUT CHARACTER
L8679           JSR         >L85D1          ; REMOVE ONE CHARACTER FROM BUFFER
L867C           LDA         ,X              ; GET CURRENT INPUT CHARACTER
                CMPA        ,S              ; COMPARE TO TARGET CHARACTER
                BNE         L866B           ; BRANCH IF NO MATCH
                DECB                        ;  DECREMENT REPEAT PARAMETER
                BNE         L866B           ; BRANCH IF NOT DONE
L8685           PULS        Y,PC            ; THE Y PULL WILL CLEAN UP THE STACK FOR THE 2 PSHS A

; GET A KEYSTRKE
L8687           JSR         >LA171          ; CALL CONSOLE IN : DEV NBR=SCREEN
                CMPA        #$7F            ; GRAPHIC CHARACTER?
                BCC         L8687           ; YES - GET ANOTHER CHAR
                CMPA        #$5F            ; SHIFT UP ARROW (QUIT INSERT)
                BNE         L8694           ; NO
                LDA         #ESC            ; REPLACE W/ESCAPE CODE
L8694           CMPA        #CR             ; ENTER KEY
                BEQ         L86A6           ; YES
                CMPA        #ESC            ; ESCAPE?
                BEQ         L86A6           ; YES
                CMPA        #BS             ; BACKSPACE?
                BEQ         L86A6           ; YES
                CMPA        #SPACE          ; SPACE
                BLO         L8687           ; GET ANOTHER CHAR IF CONTROL CHAR
                ORCC        #$01            ; SET CARRY
L86A6           RTS
; TRON
TRON            FCB         SKP1LD          ; SKIP ONE BYTE AND LDA #$4F
; TROFF
TROFF           CLRA                        ;  TROFF FLAG
                STA         TRCFLG          ; TRON/TROFF FLAG:0=TROFF, <> 0=TRON
                RTS
; POS
POS             LDA         DEVNUM          ; GET DEVICE NUMBER
                PSHS        A               ; SAVE IT ON STACK
                JSR         >LA5AE          ; GET DEVICE NUMBER
                JSR         >LA406          ; FILE STATUS CHECK
                JSR         >LA35F          ; SET UP TAB FIELD WIDTH
                LDB         DEVPOS          ; GET PRINT POSITION
                JMP         >LA5E4          ; CONVERT PRINT POSITION TO FLOATING POINT
; VARPTR
VARPTRTOK       JSR         >LB26A          ; SYNTAX CHECK FOR (
                LDD         ARYEND          ; GET ADDR OF END OF ARRAYS
                PSHS        B,A             ; SAVE IT ON STACK
                JSR         >LB357          ; GET VARIABLE DESCRIPTOR
                JSR         >LB267          ; SYNTAX CHECK FOR )
                PULS        A,B             ; GET END OF ARRAYS ADDR BACK
                EXG         X,D             ; SWAP END OF ARRAYS AND VARIABLE DESCRIPTOR
                CMPX        ARYEND          ; COMPARE TO NEW END OF ARRAYS
                BNE         L8724           ; FC ERROR IF VARIABLE WAS NOT DEFINED PRIOR TO CALLING VARPTR
                JMP         GIVABF          ; CONVERT VARIABLE DESCRIPTOR INTO A FP NUMBER
; MID$(OLDSTRING,POSITION,LENGTH)=REPLACEMENT
L86D6           JSR         GETNCH          ; GET INPUT CHAR FROM BASIC
                JSR         >LB26A          ; SYNTAX CHECK FOR (
                JSR         >LB357          ; GET VARIABLE DESCRIPTOR ADDRESS AND
                PSHS        X               ; SAVE IT ON THE STACK
                LDD         $02,X           ; POINT ACCD TO START OF OLDSTRING
                CMPD        FRETOP          ; COMPARE TO START OF CLEARED SPACE
                BLS         L86EB           ; BRANCH IF <=
                SUBD        MEMSIZ          ; SUBTRACT OUT TOP OF CLEARED SPACE
                BLS         L86FD           ; BRANCH IF STRING IN STRING SPACE
L86EB           LDB         ,X              ; GET LENGTH OF OLDSTRING
                JSR         >LB56D          ; RESERVE ACCB BYTES IN STRING SPACE
                PSHS        X               ; SAVE RESERVED SPACE STRING ADDRESS ON STACK
                LDX         $02,S           ; POINT X TO OLDSTRING DESCRIPTOR
                JSR         >LB643          ; MOVE OLDSTRING INTO STRING SPACE
                PULS        X,U             ; GET OLDSTRING DESCRIPTOR ADDRESS AND RESERVED STRING
                STX         $02,U           ; ADDRESS AND SAVE RESERVED ADDRESS AS OLDSTRING ADDRESS
                PSHS        U               ; SAVE OLDSTRING DESCRIPTOR ADDRESS
L86FD           JSR         >LB738          ; SYNTAX CHECK FOR COMMA AND EVALUATE LENGTH EXPRESSION
                PSHS        B               ; SAVE POSITION PARAMETER ON STACK
                TSTB                        ;  CHECK POSITION PARAMETER AND BRANCH
                BEQ         L8724           ; IF START OF STRING
                LDB         #$FF            ; DEFAULT REPLACEMENT LENGTH = $FF
                CMPA        #')             ; CHECK FOR END OF MID$ STATEMENT AND
                BEQ         L870E           ; BRANCH IF AT END OF STATEMENT
                JSR         >LB738          ; SYNTAX CHECK FOR COMMA AND EVALUATE LENGTH EXPRESSION
L870E           PSHS        B               ; SAVE LENGTH PARAMETER ON STACK
                JSR         >LB267          ; SYNTAX CHECK FOR )
                LDB         #$B3            ; TOKEN FOR =
                JSR         >LB26F          ; SYNTAX CHECK FOR =
                BSR         L8748           ; EVALUATE REPLACEMENT STRING
                TFR         X,U             ; SAVE REPLACEMENT STRING ADDRESS IN U
                LDX         $02,S           ; POINT X TO OLOSTRING DESCRIPTOR ADDRESS
                LDA         ,X              ; GET LENGTH OF OLDSTRING
                SUBA        $01,S           ; SUBTRACT POSITION PARAMETER
                BCC         L8727           ; INSERT REPLACEMENT STRING INTO OLDSTRING
L8724           JMP         >LB44A          ; FC ERROR IF POSITION > LENGTH OF OLDSTRING
L8727           INCA                        ;  NOW ACCA = NUMBER OF CHARACTERS TO THE RIGHT
; (INCLUSIVE) OF THE POSITION PARAMETER
                CMPA        ,S              ; COMPARE TO LENGTH PARAMETER
                BCC         L872E           ; BRANCH IF NEW STRING WILL FIT IN OLDSTRING
                STA         ,S              ; IF NOT, USE AS MUCH OF LENGTH PARAMETER AS WILL FIT
L872E           LDA         $01,S           ; GET POSITION PARAMETER
                EXG         A,B             ; ACCA=LENGTH OF REPL STRING, ACCB=POSITION PARAMETER
                LDX         $02,X           ; POINT X TO OLDSTRING ADDRESS
                DECB                        ;  BASICS POSITION PARAMETER STARTS AT 1 THIS ROUTINE
; WANTS IT TO START AT ZERO
                ABX                         ;  POINT X TO POSITION IN OLDSTRING WHERE THE REPLACEMENT WILL GO
                TSTA                        ;  IF THE LENGTH OF THE REPLACEMENT STRING IS ZERO
                BEQ         L8746           ; THEN RETURN
                CMPA        ,S              ; IF THE LENGTH OF REPLACEMENT STRING IS <= THE
                BLS         L873F           ; ADJUSTED LENGTH PARAMETER, THEN BRANCH
                LDA         ,S              ; OTHERWISE USE AS MUCH ROOM AS IS AVAILABLE
L873F           TFR         A,B             ; SAVE NUMBER OF BYTES TO MOVE IN ACCB
                EXG         U,X             ; SWAP SOURCE AND DESTINATION POINTERS
                JSR         >LA59A          ; MOVE (B) BYTES FROM (X) TO (U)
L8746           PULS        A,B,X,PC        ; CLEAN UP THE STACK AND RETURN
L8748           JSR         >LB156          ; EVALUATE EXPRESSION
                JMP         >LB654          ; TM ERROR IF NUMERIC; RETURN WITH X POINTING
; TO STRING, ACCB = LENGTH
; STRING
STRING          JSR         >LB26A          ; SYNTAX CHECK FOR (
                JSR         EVALEXPB        ; EVALUATE EXPRESSION; ERROR IF > 255
                PSHS        B               ; SAVE LENGTH OF STRING
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB156          ; EVALUATE EXPRESSION
                JSR         >LB267          ; SYNTAX CHECK FOR )
                LDA         VALTYP          ; GET VARIABLE TYPE
                BNE         L8768           ; BRANCH IF STRING
                JSR         >LB70E          ; CONVERT FPA0 INTO AN INTEGER IN ACCB
                BRA         L876B           ; SAVE THE STRING IN STRING SPACE
L8768           JSR         >LB6A4          ; GET FIRST BYTE OF STRING
L876B           PSHS        B               ; SAVE FIRST BYTE OF EXPRESSION
                LDB         $01,S           ; GET LENGTH OF STRING
                JSR         >LB50F          ; RESERVE ACCB BYTES IN STRING SPACE
                PULS        A,B             ; GET LENGTH OF STRING AND CHARACTER
                BEQ         L877B           ; BRANCH IF NULL STRING
L8776           STA         ,X+             ; SAVE A CHARACTER IN STRING SPACE
                DECB                        ;  DECREMENT LENGTH
                BNE         L8776           ; BRANCH IF NOT DONE
L877B           JMP         >LB69B          ; PUT STRING DESCRIPTOR ONTO STRING STACK
; INSTR
INSTR           JSR         >LB26A          ; SYNTAX CHECK FOR (
                JSR         >LB156          ; EVALUATE EXPRESSION
                LDB         #$01            ; DEFAULT POSITION = 1 (SEARCH START)
                PSHS        B               ; SAVE START
                LDA         VALTYP          ; GET VARIABLE TYPE
                BNE         L879C           ; BRANCH IF STRING
                JSR         >LB70E          ; CONVERT FPA0 TO INTEGER IN ACCB
                STB         ,S              ; SAVE START SEARCH VALUE
                BEQ         L8724           ; BRANCH IF START SEARCH AT ZERO
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB156          ; EVALUATE EXPRESSION - SEARCH STRING
                JSR         >LB146          ; TM ERROR IF NUMERIC
L879C           LDX         FPA0+2          ; SEARCH STRING DESCRIPTOR ADDRESS
                PSHS        X               ; SAVE ON THE STACK
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >L8748          ; EVALUATE TARGET STRING EXPRESSION
                PSHS        X,B             ; SAVE ADDRESS AND LENGTH ON STACK
                JSR         >LB267          ; SYNTAX CHECK FOR ')'
                LDX         $03,S           ; LOAD X WITH SEARCH STRING DESCRIPTOR ADDRESS
                JSR         >LB659          ; AND GET THE LENGTH ANDADDRESS OF SEARCH STRING
                PSHS        B               ; SAVE LENGTH ON STACK

; AT THIS POINT THE STACK HAS THE FOLLOWING INFORMATION
; ON IT: 0,S-SEARCH LENGTH; 1,S-TARGET LENGTH; 2 3,S-TARGET
; ADDRESS; 4 5,S-SEARCH DESCRIPTOR ADDRESS; 6,S-SEARCH POSITION
                CMPB        $06,S           ; COMPARE LENGTH OF SEARCH STRING TO START
                BLO         L87D9           ; POSITION; RETURN 0 IF LENGTH < START
                LDA         $01,S           ; GET LENGTH OF TARGET STRING
                BEQ         L87D6           ; BRANCH IF TARGET STRING = NULL
                LDB         $06,S           ; GET START POSITION
                DECB                        ;  MOVE BACK ONE
                ABX                         ;  POINT X TO POSITION IN SEARCH STRING WHERE SEARCHING WILL START
L87BE           LEAY        ,X              ; POINT Y TO SEARCH POSITION
                LDU         $02,S           ; POINT U TO START OF TARGET
                LDB         $01,S           ; LOAD ACCB WITH LENGTH OF TARGET
                LDA         ,S              ; LOAD ACCA WITH LENGTH OF SEARCH
                SUBA        $06,S           ; SUBTRACT SEARCH POSITION FROM SEARCH LENGTH
                INCA                        ;  ADD ONE
                CMPA        $01,S           ; COMPARE TO TARGET LENGTH
                BLO         L87D9           ; RETURN 0 IF TARGET LENGTH > WHATS LEFT OF SEARCH STRING
L87CD           LDA         ,X+             ; GET A CHARACTER FROM SEARCH STRING
                CMPA        ,U+             ; COMPARE IT TO TARGET STRING
                BNE         L87DF           ; BRANCH IF NO MATCH
                DECB                        ;  DECREMENT TARGET LENGTH
                BNE         L87CD           ; CHECK ANOTHER CHARACTER
L87D6           LDB         $06,S           ; GET MATCH POSITION
L87D8           FCB         SKP1            ; SKIP NEXT BYTE
L87D9           CLRB                        ;  MATCH ADDRESS = 0
                LEAS        $07,S           ; CLEAN UP THE STACK
                JMP         >LB4F3          ; CONVERT ACCB TO FP NUMBER
L87DF           INC         $06,S           ; INCREMENT SEARCH POSITION
                LEAX        $01,Y           ; MOVE X TO NEXT SEARCH POSITION
                BRA         L87BE           ; KEEP LOOKING FOR A MATCH
; ASCII TO FLOATING POINT CONVERSION RAM HOOK
XVEC19          CMPA        #'&
L87E7           BNE         L8845           ; RETURN IF NOT HEX OR OCTAL VARIABLE
                LEAS        $02,S           ; PURGE RETURN ADDRESS FROM STACK
; PROCESS A VARIABLE PRECEEDED BY A & (&H,&O)
L87EB           CLR         FPA0+2          ; CLEAR BOTTOM TWO
                CLR         FPA0+3          ; BYTES OF FPA0
                LDX         #FPA0+2         ; BYTES 2,3 OF FPA0 = (TEMPORARY ACCUMULATOR)
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                CMPA        #'O             ; OCTAL VALUE?
                BEQ         L880A           ; YES
                CMPA        #'H             ; HEX VALUE?
                BEQ         L881F           ; YES
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BRA         L880C           ; DEFAULT TO OCTAL (&O)
L8800           CMPA        #'8
                LBHI        LB277           ; SYNTAX ERROR IF
                LDB         #$03            ; BASE 8 MULTIPLIER
                BSR         L8834           ; ADD DIGIT TO TEMPORARY ACCUMULATOR
; EVALUATE AN &O VARIABLE
L880A           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
L880C           BLO         L8800           ; BRANCH IF NUMERIC
L880E           CLR         FPA0            ; CLEAR 2 HIGH ORDER
                CLR         FPA0+1          ; BYTES OF FPA0
                CLR         VALTYP          ; SET VARXABLE TYPE TO NUMERIC
                CLR         FPSBYT          ; ZERO OUT SUB BYTE OF FPA0
                CLR         FP0SGN          ; ZERO OUT MANTISSA SIGN OF FPA0
                LDB         #$A0            ; SET EXPONENT OF FPA0
                STB         FP0EXP          ;
                JMP         >LBA1C          ; GO NORMALIZE FPA0
; EVALUATE AN &H VARIABLE
L881F           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BLO         L882E           ; BRANCH IF NUMERIC
                JSR         >LB3A2          ; SET CARRY IF NOT ALPHA
L8826           BLO         L880E           ; BRANCH IF NOT ALPHA OR NUMERIC
                CMPA        #'G             ; CHECK FOR LETTERS A-F
                BCC         L880E           ; BRANCH IF >= G (ILLEGAL HEX LETTER)
                SUBA        #'A-('9+1)      ; SUBTRACT ASCII DIFFERENCE BETWEEN A AND 9
L882E           LDB         #$04            ; BASE 16 DIGIT MULTIPLIER = 2**4
                BSR         L8834           ; ADD DIGIT TO TEMPORARY ACCUMULATOR
                BRA         L881F           ; KEEP EVALUATING VARIABLE
L8834           ASL         $01,X           ; MULTIPLY TEMPORARY
                ROL         ,X              ; ACCUMULATOR BY TWO
                LBCS        LBA92           ; OV' OVERFLOW ERROR
                DECB                        ;  DECREMENT SHIFT COUNTER
                BNE         L8834           ; MULTIPLY TEMPORARY ACCUMULATOR AGAIN
L883F           SUBA        #'0             ; MASK OFF ASCII
                ADDA        $01,X           ; ADD DIGIT TO TEMPORARY
                STA         $01,X           ; ACCUMULATOR AND SAVE IT
L8845           RTS
; EXPRESSION EVALUATION RAM HOOK
XVEC15          PULS        U               ; PULL RETURN ADDRESS AND SAVE IN U REGISTER
                CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                LDX         CHARAD          ; CURRENT INPUT POINTER TO X
                JSR         GETNCH          ; GET CHARACTER FROM BASIC
                CMPA        #'&             ; HEX AND OCTAL VARIABLES ARE PRECEEDED BY &
                BEQ         L87EB           ; PROCESS A & VARIABLE
                CMPA        #$CC            ; TOKEN FOR FN
                BEQ         L88B4           ; PROCESS FN CALL
                CMPA        #$FF            ; CHECK FOR SECONDARY TOKEN
                BNE         L8862           ; NOT SECONDARY
                JSR         GETNCH          ; GET CHARACTER FROM BASIC
                CMPA        #$83            ; TOKEN FOR USR
                LBEQ        L892C           ; PROCESS USR CALL
L8862           STX         CHARAD          ; RESTORE BASICS INPUT POINTER
                JMP         ,U              ; RETURN TO CALLING ROUTINE
L8866           LDX         CURLIN          ; GET CURRENT LINE NUMBER
                LEAX        $01,X           ; IN DIRECT MODE?
L886A           BNE         L8845           ; RETURN IF NOT IN DIRECT MODE
                LDB         #2*11           ; ILLEGAL DIRECT STATEMENT ERROR
L886E           JMP         >LAC46          ; PROCESS ERROR
; DEF
DEF             LDX         [CHARAD]        ; GET TWO INPUT CHARS
                CMPX        #$FF83          ; TOKEN FOR USR
                LBEQ        L890F           ; BRANCH IF DEF USR
                BSR         L88A1           ; GET DESCRIPTOR ADDRESS FOR FN VARIABLE NAME
                BSR         L8866           ; DONT ALLOW DEF FN IF IN DIRECT MODE
                JSR         >LB26A          ; SYNTAX CHECK FOR (
                LDB         #$80            ; GET THE FLAG TO INDICATE ARRAY VARIABLE SEARCH DISABLE
                STB         ARYDIS          ; AND SAVE IT IN THE ARRAY DISABLE FLAG
                JSR         >LB357          ; GET VARIABLE DESCRIPTOR
                BSR         L88B1           ; TM ERROR IF STRING
                JSR         >LB267          ; SYNTAX CHECK FOR )
                LDB         #$B3            ; TOKEN FOR =
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR =
                LDX         V4B             ; GET THE ADDRESS OF THE FN NAME DESCRIPTOR
                LDD         CHARAD          ; GET THE CURRENT INPUT POINTER ADDRESS AND
                STD         ,X              ; SAVE IT IN FIRST 2 BYTES OF THE DESCRIPTOR
                LDD         VARPTR          ; GET THE DESCRIPTOR ADDRESS OF THE ARGUMENT
                STD         $02,X           ; VARIABLE AND SAVE IT IN THE DESCRIPTOR OF THE FN NAME
                JMP         DATA            ; MOVE INPUT POINTER TO END OF LINE OR SUBLINE
L88A1           LDB         #$CC            ; TOKEN FOR FN
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR FN
                LDB         #$80            ; GET THE FLAG TO INDICATE ARRAY VARIABLE SEARCH DISABLE FLAG
                STB         ARYDIS          ; AND SAVE IT IN ARRAY VARIABLE FLAG
                ORA         #$80            ; SET BIT 7 OF CURRENT INPUT CHARACTER TO INDICATE AN FN VARIABLE
                JSR         >LB35C          ; GET THE DESCRIPTOR ADDRESS OF THIS
                STX         V4B             ; VARIABLE AND SAVE IT IN V4B
L88B1           JMP         >LB143          ; TM ERROR IF STRING VARIABLE
; EVALUATE AN FN CALL
L88B4           BSR         L88A1           ; GET THE DESCRIPTOR OF THE FN NAME
                PSHS        X               ; VARIABLE AND SAVE IT ON THE STACK
                JSR         >LB262          ; SYNTAX CHECK FOR ( & EVALUATE EXPR
                BSR         L88B1           ; TM ERROR IF STRING VARIABLE
                PULS        U               ; POINT U TO FN NAME DESCRIPTOR
                LDB         #2*25           ; UNDEFINED FUNCTION CALL ERROR
                LDX         $02,U           ; POINT X TO ARGUMENT VARIABLE DESCRIPTOR
                BEQ         L886E           ; BRANCH TO ERROR HANDLER
                LDY         CHARAD          ; SAVE CURRENT INPUT POINTER IN Y
                LDU         ,U              ; POINT U TO START OF FN FORMULA AND
                STU         CHARAD          ; SAVE IT IN INPUT POINTER
                LDA         $04,X           ; GET FP VALUE OF
                PSHS        A               ; ARGUMENT VARIABLE, CURRENT INPUT
                LDD         ,X              ; POINTER, AND ADDRESS OF START
                LDU         $02,X           ; OF FN FORMULA AND SAVE
                PSHS        U,Y,X,B,A       ; THEM ON THE STACK
                JSR         >LBC35          ; PACK FPA0 AND SAVE IT IN (X)
L88D9           JSR         >LB141          ; EVALUATE FN EXPRESSION
                PULS        A,B,X,Y,U       ; RESTORE REGISTERS
                STD         ,X              ; GET THE FP
                STU         $02,X           ; VALUE OF THE ARGUMENT
                PULS        A               ; VARIABLE OFF OF THE
                STA         $04,X           ; STACK AND RE-SAVE IT
                JSR         GETCCH          ; GET FINAL CHARACTER OF THE FN FORMULA
                LBNE        LB277           ; SYNTAX ERROR IF NOT END OF LINE
                STY         CHARAD          ; RESTORE INPUT POINTER
L88EF           RTS
; ERROR DRIVER RAM HOOK
XVEC17          CMPB        #2*25           ; CHECK FOR EXBAS ERROR NUMBER
                BLO         L88EF           ; BRANCH IF < EXBAS ERROR
                JSR         >LA7E9          ; TURN CASSETTE MOTOR OFF
                JSR         >LA974          ; DISABLE ANALOG MULTIPLEXER
                JSR         >LAD33          ; DO PART OF A NEW
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         >LB95C          ; MOVE CURSOR TO START OF NEXT LINE
                JSR         >LB9AF          ; SEND A ? TO CONSOLE OUT
                LDX         #L890B-25*2     ; POINT X TO EXBAS ERRORS
                JMP         >LAC60          ; PROCESS ERROR
; ADDITIONAL ERROR MESSAGES ADDED BY EXTENDED BASIC
L890B           FCC         'UF'            ; 25 UNDEFINED FUNCTION (FN) CALL
L890D           FCC         'NE'            ; 26 FILE NOT FOUND
; DEF USR
L890F           JSR         GETNCH          ; SKIP PAST SECOND BYTE OF DEF USR TOKEN
                BSR         L891C           ; GET FN NUMBER
                PSHS        X               ; SAVE FN EXEC ADDRESS STORAGE LOC
                BSR         L8944           ; CALCULATE EXEC ADDRESS
                PULS        U               ; GET FN EXEC ADDRESS STORAGE LOC
                STX         ,U              ; SAVE EXEC ADDRESS
                RTS
L891C           CLRB                        ;  DEFAULT TO USR0 IF NO ARGUMENT
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BCC         L8927           ; BRANCH IF NOT NUMERIC
                SUBA        #'0             ; MASK OFF ASCII
                TFR         A,B             ; SAVE USR NUMBER IN ACCB
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
L8927           LDX         USRADR          ; GET ADDRESS OF STORAGE LOCs FOR USR ADDRESS
                ASLB                        ;  X2 - 2 BYTES/USR ADDRESS
                ABX                         ;  ADD OFFSET TO START ADDRESS OF STORAGE LOCs
                RTS
; PROCESS A USR CALL
L892C           BSR         L891C           ; GET STORAGE LOC OF EXEC ADDRESS FOR USR N
                LDX         ,X              ; GET EXEC ADDRESS AND
                PSHS        X               ; PUSH IT ONTO STACK
                JSR         >LB262          ; SYNTAX CHECK FOR ( & EVALUATE EXPR
                LDX         #FP0EXP         ; POINT X TO FPA0
                LDA         VALTYP          ; GET VARIABLE TYPE
                BEQ         L8943           ; BRANCH IF NUMERIC, STRING IF <> 0
                JSR         >LB657          ; GET LENGTH & ADDRESS OF STRING VARIABLE
                LDX         FPA0+2          ; GET POINTER TO STRING DESCRIPTOR
                LDA         VALTYP          ; GET VARIABLE TYPE
L8943           RTS         JUMP            ; TO USR ROUTINE (PSHS X ABOVE)
L8944           LDB         #$B3            ; TOKEN FOR =
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR =
                JMP         >LB73D          ; EVALUATE EXPRESSION, RETURN VALUE IN X
; EXTENDED BASICS IRQ ROUTINE
XIRQSV          LDA         PIA0+3          ; GET PIA0, PORT B CONTROL REGISTER
                BMI         L8952           ; BRANCH IF 60 HZ INTERRUPT
                RTI         RETURN          ; IF 63.5 MICROSECOND INTERRUPT
L8952           LDA         PIA0+2          ; RESET PIA INTERRUPT FLAG
L8955           LDX         TIMVAL          ; GET REAL TIME CLOCK
                LEAX        $01,X           ; INCREMENT IT
                STX         TIMVAL          ; SAVE IT
                JMP         >L9C3E          ; GO CHECK SOME MORE STUFF
L8960           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BSR         L8944           ; GET NEW TIMER VALUE
                STX         TIMVAL          ; SET TIMER COUNTER
                RTS
; TIMER
TIMER           LDX         TIMVAL          ; GET TIMER VALUE
                STX         FPA0+2          ; SAVE TIMER VALUE IN BOTTOM OF FPA0
                JMP         >L880E          ; CONVERT BALANCE OF FPA0 TO POSITIVE INTEGER
; DEL
DEL             LBEQ        LB44A           ; FC ERROR IF NO ARGUMENT
                JSR         >LAF67          ; CONVERT A DECIMAL BASiC NUMBER TO BINARY
                JSR         >LAD01          ; FIND RAM ADDRESS OF START OF A BASIC LINE
                STX         VD3             ; SAVE RAM ADDRESS OF STARTING LINE NUMBER
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         L8990           ; BRANCH IF END OF LINE
                CMPA        #$AC            ; TOKEN FOR -'
                BNE         L89BF           ; TERMINATE COMMAND IF LINE NUMBER NOT FOLLOWED BY -
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BEQ         L898C           ; IF END OF LINE, USE DEFAULT ENDING LINE NUMBER
                BSR         L89AE           ; CONVERT ENDING LINE NUMBER TO BINARY
                BRA         L8990           ; AND SAVE IT IN BINVAL
L898C           LDA         #$FF            ; USE $FFXX AS DEFAULT ENDING
                STA         BINVAL          ; LINE NUMBER - SAVE IT IN BINVAL
L8990           LDU         VD3             ; POINT U TO STARTING LINE NUMBER ADDRESS
L8992           FCB         SKP2            ; SKIP TWO BYTES
L8993           LDU         ,U              ; POINT U TO START OF NEXT LINE
                LDD         ,U              ; CHECK FOR END OF PROGRAM
                BEQ         L899F           ; BRANCH IF END OF PROGRAM
                LDD         $02,U           ; LOAD ACCD WITH THIS LINES NUMBER
                SUBD        BINVAL          ; SUBTRACT ENDING LINE NUMBER ADDRESS
                BLS         L8993           ; BRANCH IF = < ENDING LINE NUMBER
L899F           LDX         VD3             ; GET STARTING LINE NUMBER
                BSR         L89B8           ; MOVE (U) TO (X) UNTIL END OF PROGRAM
                JSR         >LAD21          ; RESET BASICS INPUT POINTER AND ERASE VARIABLES
                LDX         VD3             ; GET STARTING LINE NUMBER ADDRESS
                JSR         >LACF1          ; RECOMPUTE START OF NEXT LINE ADDRESSES
                JMP         >LAC73          ; JUMP TO BASICS MAIN COMMAND LOOP
L89AE           JSR         >LAF67          ; GO GET LINE NUMBER CONVERTED TO BINARY
                JMP         >LA5C7          ; MAKE SURE THERES NO MORE ON THIS LINE
L89B4           LDA         ,U+             ; GET A BYTE FROM (U)
                STA         ,X+             ; MOVE THE BYTE TO (X)
L89B8           CMPU        VARTAB          ; COMPARE TO END OF BASIC
                BNE         L89B4           ; BRANCH IF NOT AT END
                STX         VARTAB          ; SAVE (X) AS NEW END OF BASIC
L89BF           RTS
; LINE INPUT
L89C0           JSR         >L8866          ; BS ERROR IF IN DIRECT MODE
                JSR         GETNCH          ; GET A CHAR FROM BASIC
                CMPA        #'#             ; CHECK FOR DEVICE NUMBER FLAG AND
                BNE         L89D2           ; BRANCH IF NOT THERE
                JSR         >LA5A5          ; CHECK FOR VALID DEVICE NUMBER
                JSR         >LA3ED          ; CHECK FOR OPEN FILE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
L89D2           CMPA        #'"             ; CHECK FOR PROMPT STRING
                BNE         L89E1           ; BRANCH IF NO PROMPT STRING
                JSR         >LB244          ; STRIP OFF PROMPT STRING & PUT IT ON STRING STACK
                LDB         #';
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR;
                JSR         >LB99F          ; REMOVE PROMPT STRING FROM STRING STACK & SEND TO CONSOLE OUT
L89E1           LEAS        -2,S            ; RESERVE TWO STORAGE SLOTS ON STACK
                JSR         >LB035          ; INPUT A LINE FROM CURRENT INPUT DEVICE
                LEAS        $02,S           ; CLEAN UP THE STACK
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         >LB357          ; SEARCH FOR A VARIABLE
                STX         VARDES          ; SAVE POINTER TO VARIABLE DESCRIPTOR
                JSR         >LB146          ; TM ERROR IF VARIABLE TYPE = NUMERIC
                LDX         #LINBUF         ; POINT X TO THE STRING BUFFER WHERE THE INPUT STRING WAS STORED
                CLRA                        ;  TERMINATOR CHARACTER 0 (END OF LINE)
                JSR         >LB51A          ; PARSE THE INPUT STRING AND STORE IT IN THE STRING SPACE
                JMP         >LAFA4          ; REMOVE DESCRIPTOR FROM STRING STACK
L89FC           JSR         >LAF67          ; STRIP A DECIMAL NUMBER FROM BASIC INPUT LINE
                LDX         BINVAL          ; GET BINARY VALUE
                RTS
L8A02           LDX         VD1             ; GET CURRENT OLD NUMBER BEING RENUMBERED
L8A04           STX         BINVAL          ; SAVE THE LINE NUMBER BEING SEARCHED FOR
                JMP         >LAD01          ; GO FIND THE LINE NUMBER IN BASIC PROGRAM
; RENUM
RENUM           JSR         >LAD26          ; ERASE VARIABLES
                LDD         #10             ; DEFAULT LINE NUMBER INTERVAL
                STD         VD5             ; SAVE DEFAULT RENUMBER START LINE NUMBER
                STD         VCF             ; SAVE DEFAULT INTERVAL
                CLRB                        ;  NOW ACCD = 0
                STD         VD1             ; DEFAULT LINE NUMBER OF WHERE TO START RENUMBERING
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BCC         L8A20           ; BRANCH IF NOT NUMERIC
                BSR         L89FC           ; CONVERT DECIMAL NUMBER IN BASIC PROGRAM TO BINARY
                STX         VD5             ; SAVE LINE NUMBER WHERE RENUMBERING STARTS
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
L8A20           BEQ         L8A3D           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                BCC         L8A2D           ; BRANCH IF NEXT CHARACTER NOT NUMERIC
                BSR         L89FC           ; CONVERT DECIMAL NUMBER IN BASIC PROGRAM TO BINARY
                STX         VD1             ; SAVE NEW RENUMBER LINE
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
L8A2D           BEQ         L8A3D           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                BCC         L8A3A           ; BRANCH IF NEXT CHARACTER NOT NUMERIC
                BSR         L89FC           ; CONVERT DECIMAL NUMBER IN BASIC PROGRAM TO BINARY
                STX         VCF             ; SAVE NEW INTERVAL
                BEQ         L8A83           ; FC' ERROR
L8A3A           JSR         >LA5C7          ; CHECK FOR MORE CHARACTERS ON LINE - SYNTAX ERROR IF ANY
L8A3D           BSR         L8A02           ; GO GET ADDRESS OF OLD NUMBER BEING RENUMBERED
                STX         VD3             ; SAVE ADDRESS
                LDX         VD5             ; GET NEXT RENUMBERED LINE NUMBER TO USE
                BSR         L8A04           ; FIND THE LINE NUMBER IN THE BASIC PROGRAM
                CMPX        VD3             ; COMPARE TO ADDRESS OF OLD LINE NUMBER
                BLO         L8A83           ; FC ERROR IF NEW ADDRESS < OLD ADDRESS
                BSR         L8A67           ; MAKE SURE RENUMBERED LINE NUMBERS WILL BE IN RANGE
                JSR         >L8ADD          ; CONVERT ASCII LINE NUMBERS TO EXPANDED BINARY
                JSR         >LACEF          ; RECALCULATE NEXT LINE RAM ADDRESSES
                BSR         L8A02           ; GET RAM ADDRESS OF FIRST LINE TO BE RENUMBERED
                STX         VD3             ; SAVE IT
                BSR         L8A91           ; MAKE SURE LINE NUMBERS EXIST
                BSR         L8A68           ; INSERT NEW LINE NUMBERS IN LINE HEADERS
                BSR         L8A91           ; INSERT NEW LINE NUMBERS IN PROGRAM STATEMENTS
                JSR         >L8B7B          ; CONVERT PACKED BINARY LINE NUMBERS TO ASCII
                JSR         >LAD26          ; ERASE VARIABLES
                JSR         >LACEF          ; RECALCULATE NEXT LINE RAM ADDRESS
                JMP         >LAC73          ; GO BACK TO BASICS MAIN LOOP
L8A67           FCB         SKP1LD          ; SKIP ONE BYTE - LDA #$4F
L8A68           CLRA                        ;  NEW LINE NUMBER FLAG - 0 INSERT NEW LINE NUMBERS
                STA         VD8             ; SAVE NEW LINE NUMBER FLAG; 0 = INSERT NEW NUMBERS
                LDX         VD3             ; GET ADDRESS OF OLD LINE NUMBER BEING RENUMBERED
                LDD         VD5             ; GET THE CURRENT RENUMBERED LINE NUMBER
                BSR         L8A86           ; RETURN IF END OF PROGRAM
L8A71           TST         VD8             ; CHECK NEW LINE NUMBER FLAG
                BNE         L8A77           ; BRANCH IF NOT INSERTING NEW LINE NUMBERS
                STD         $02,X           ; STORE THE NEW LINE NUMBER IN THE BASIC PROGRAM
L8A77           LDX         ,X              ; POINT X TO THE NEXT LINE IN BASIC
                BSR         L8A86           ; RETURN IF END OF PROGRAM
                ADDD        VCF             ; ADD INTERVAL TO CURRENT RENUMBERED LINE NUMBER
                BLO         L8A83           ; FC ERROR IF LINE NUMBER > $FFFF
                CMPA        #MAXLIN         ; LARGEST LINE NUMBER = $F9FF
                BLO         L8A71           ; BRANCH IF LEGAL LINE NUMBER
L8A83           JMP         >LB44A          ; FC ERROR IF LINE NUMBER MS BYTE > $F9
; TEST THE TWO BYTES POINTED TO BY (X).
; NORMAL RETURN IF <> 0. IF = 0 (END OF
; PROGRAM) RETURN IS PULLED OFF STACK AND
; YOU RETURN TO PREVIOUS SUBROUTINE CALL.
L8A86           PSHS        B,A             ; SAVE ACCD
                LDD         ,X              ; TEST THE 2 BYTES POINTED TO BY X
                PULS        A,B             ; RESTORE ACCD
                BNE         L8A90           ; BRANCH IF NOT END OF PROGRAM
                LEAS        $02,S           ; PURGE RETURN ADDRESS FROM STACK
L8A90           RTS
L8A91           LDX         TXTTAB          ; GET START OF BASIC PROGRAM
                LEAX        -1,X            ; MOVE POINTER BACK ONE
L8A95           LEAX        $01,X           ; MOVE POINTER UP ONE
                BSR         L8A86           ; RETURN IF END OF PROGRAM
L8A99           LEAX        $03,X           ; SKIP OVER NEXT LINE ADDRESS AND LINE NUMBER
L8A9B           LEAX        $01,X           ; MOVE POINTER TO NEXT CHARACTER
                LDA         ,X              ; CHECK CURRENT CHARACTER
                BEQ         L8A95           ; BRANCH IF END OF LINE
                STX         TEMPTR          ; SAVE CURRENT POINTER
                DECA                        ;
                BEQ         L8AB2           ; BRANCH IF START OF PACKED NUMERIC LINE
                DECA                        ;
                BEQ         L8AD3           ; BRANCH IF LINE NUMBER EXISTS
                DECA                        ;
                BNE         L8A9B           ; MOVE TO NEXT CHARACTER IF > 3
L8AAC           LDA         #$03            ; SET 1ST BYTE = 3 TO INDICATE LINE
                STA         ,X+             ; NUMBER DOESNT CURRENTLY EXIST
                BRA         L8A99           ; GO GET ANOTHER CHARACTER
L8AB2           LDD         $01,X           ; GET MS BYTE OF LINE NUMBER
                DEC         $02,X           ; DECREMENT ZERO CHECK BYTE
                BEQ         L8AB9           ; BRANCH IF MS BYTE <> 0
                CLRA                        ;  CLEAR MS BYTE
L8AB9           LDB         $03,X           ; GET LS BYTE OF LINE NUMBER
                DEC         $04,X           ; DECREMENT ZERO CHECK FLAG
                BEQ         L8AC0           ; BRANCH IF IS BYTE <> 0
                CLRB                        ;  CLEAR LS BYTE
L8AC0           STD         $01,X           ; SAVE BINARY LINE NUMBER
                STD         BINVAL          ; SAVE TRIAL LINE NUMBER
                JSR         >LAD01          ; FIND RAM ADDRESS OF A BASIC LINE NUMBER
L8AC7           LDX         TEMPTR          ; GET BACK POINTER TO START OF PACKED LINE NUMBER
                BLO         L8AAC           ; BRANCH IF NO LINE NUMBER MATCH FOUND
                LDD         V47             ; GET START ADDRESS OF LINE NUMBER
                INC         ,X+             ; SET 1ST BYTE = 2, TO INDICATE LINE NUMBER EXISTS IF CHECKING FOR
; EXISTENCE OF LINE NUMBER, SET IT = 1 IF INSERTING LINE NUMBERS
                STD         ,X              ; SAVE RAM ADDRESS OF CORRECT LINE NUMBER
                BRA         L8A99           ; GO GET ANOTHER CHARACTER
L8AD3           CLR         ,X              ; CLEAR CARRY FLAG AND 1ST BYTE
                LDX         $01,X           ; POINT X TO RAM ADDRESS OF CORRECT LINE NUMBER
                LDX         $02,X           ; PUT CORRECT LINE NUMBER INTO (X)
                STX         V47             ; SAVE IT TEMPORARILY
                BRA         L8AC7           ; GO INSERT IT INTO BASIC LINE
L8ADD           LDX         TXTTAB          ; GET BEGINNING OF BASIC PROGRAM
                BRA         L8AE5
L8AE1           LDX         CHARAD          ; GET CURRENT INPUT POINTER
                LEAX        $01,X           ; AND BUMP IT ONE
L8AE5           BSR         L8A86           ; RETURN IF END OF PROGRAM
                LEAX        $02,X           ; SKIP PAST NEXT LINE ADDRESS
L8AE9           LEAX        $01,X           ; ADVANCE POINTER BY ONE
L8AEB           STX         CHARAD          ; SAVE NEW BASIC INPUT POINTER
L8AED           JSR         GETNCH          ; GET NEXT CHARACTER FROM BASIC
L8AEF           TSTA                        ;  CHECK THE CHARACTER
                BEQ         L8AE1           ; BRANCH IF END OF LINE
                BPL         L8AED           ; BRANCH IF NOT A TOKEN
                LDX         CHARAD          ; GET CURRENT INPUT POINTER
                CMPA        #$FF            ; IS THIS A SECONDARY TOKEN?
                BEQ         L8AE9           ; YES - IGNORE IT
                JSR         RVEC22          ; HOOK INTO RAM AND CHECK FOR USER ADDED TOKENS
                CMPA        #$A7            ; TOKEN FOR THEN?
                BEQ         L8B13           ; YES
                CMPA        #$84            ; TOKEN FOR ELSE?
                BEQ         L8B13           ; YES
                CMPA        #$81            ; TOKEN FOR GO?
                BNE         L8AED           ; NO
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                CMPA        #$A5            ; TOKEN FOR TO?
                BEQ         L8B13           ; YES
                CMPA        #$A6            ; TOKEN FOR SUB?
                BNE         L8AEB           ; NO
L8B13           JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                BLO         L8B1B           ; BRANCH IF NUMERIC
L8B17           JSR         GETCCH          ; GET CURRENT BASIC INPUT CHARRACTER
                BRA         L8AEF           ; KEEP CHECKING THE LINE
L8B1B           LDX         CHARAD          ; GET CURRENT INPUT ADDRESS
                PSHS        X               ; SAVE IT ON THE STACK
                JSR         >LAF67          ; CONVERT DECIMAL BASIC NUMBER TO BINARY
                LDX         CHARAD          ; GET CURRENT INPUT POINTER
L8B24           LDA         ,-X             ; GET PREVIOUS INPUT CHARACTER
                JSR         >L90AA          ; CLEAR CARRY IF NUMERIC INPUT VALUE
                BLO         L8B24           ; BRANCH IF NON-NUMERIC
                LEAX        $01,X           ; MOVE POINTER UP ONE
                TFR         X,D             ; NOW ACCD POINTS TO ONE PAST END OF LINE NUMBER
                SUBB        $01,S           ; SUBTRACT PRE-NUMERIC POINTER LS BYTE
                SUBB        #$05            ; MAKE SURE THERE ARE AT LEAST 5 CHARACTERS IN THE NUMERIC LINE

                BEQ         L8B55           ; BRANCH IF EXACTLY 5
                BLO         L8B41           ; BRANCH IF < 5
                LEAU        ,X              ; TRANSFER X TO U
                NEGB                        ;  NEGATE B
                LEAX        B,X             ; MOVE X BACK B BYTES
                JSR         >L89B8          ; MOVE BYTES FROM (U) TO (X) UNTIL
; U = END OF BASIC; (I) = NEW END OF BASIC
                BRA         L8B55
; FORCE FIVE BYTES OF SPACE FOR THE LINE NUMBER
L8B41           STX         V47             ; SAVE END OF NUMERIC VALUE
                LDX         VARTAB          ; GET END OF BASIC PROGRAM
                STX         V43             ; SAVE IT
                NEGB                        ;  NEGATE B
                LEAX        B,X             ; ADD IT TO END OF NUMERIC POiNTER
                STX         V41             ; SAVE POINTER
                STX         VARTAB          ; STORE END OF BASIC PROGRAM
                JSR         >LAC1E          ; ACCD = TOP OF ARRAYS - CHECK FOR ENOUGH ROOM
                LDX         V45             ; GET AND SAVE THE
                STX         CHARAD          ; NEW CURRENT INPUT POINTER
L8B55           PULS        X               ; RESTORE POINTER TO START OF NUMERIC VALUE
                LDA         #$01            ; NEW LINE NUMBER FLAG
                STA         ,X              ; SAVE NEW LINE FLAG
                STA         $02,X           ;
                STA         $04,X           ;
                LDB         BINVAL          ; GET MS BYTE OF BINARY LINE NUMBER
                BNE         L8B67           ; BRANCH IF IT IS NOT ZERO
                LDB         #$01            ; SAVE A 1 IF BYTE IS 0; OTHERWISE, BASIC WILL THINK IT IS THE END OF A LINE
                INC         $02,X           ; IF 2,X = 2, THEN PREVIOUS BYTE WAS A ZERO
L8B67           STB         $01,X           ; SAVE MS BYTE OF BINARY LINE NUMBER
                LDB         BINVAL+1        ; GET IS BYTE OF BINARY LINE NUMBER
                BNE         L8B71           ; BRANCH IF NOT A ZERO BYTE
                LDB         #$01            ; SAVE A 1 IF BYTE IS A 0
                INC         $04,X           ; IF 4,X = 2, THEN PREVIOUS BYTE WAS A 0
L8B71           STB         $03,X           ; SAVE LS BYTE OF BINARY LINE NUMBER
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                CMPA        #',             ; IS IT A COMMA?
                BEQ         L8B13           ; YES - PROCESS ANOTHER NUMERIC VALUE
                BRA         L8B17           ; NO - GO GET AND PROCESS AN INPUT CHARACTER
L8B7B           LDX         TXTTAB          ; POINT X TO START OF BASIC PROGRAM
                LEAX        -1,X            ; MOVE POINTER BACK ONE
L8B7F           LEAX        $01,X           ; MOVE POINTER UP ONE
                LDD         $02,X           ; GET ADDRESS OF NEXT LINE
                STD         CURLIN          ; SAVE IT IN CURLIN
                JSR         >L8A86          ; RETURN IF END OF PROGRAM
                LEAX        $03,X           ; SKIP OVER ADDRESS OF NEXT LINE AND 1ST BYTE OF LINE NUMBER
L8B8A           LEAX        $01,X           ; MOVE POINTER UP ONE
L8B8C           LDA         ,X              ; GET CURRENT CHARACTER
                BEQ         L8B7F           ; BRANCH IF END OF LINE
                DECA                        ;  INPUT CHARACTER = 1? - VALID LINE NUMBER
                BEQ         L8BAE           ; YES
                SUBA        #$02            ; INPUT CHARACTER 3? - UL LINE NUMBER
                BNE         L8B8A           ; NO
                PSHS        X               ; SAVE CURRENT POSITION OF INPUT POINTER
                LDX         #L8BD9-1        ; POINT X TO UL MESSAGE
                JSR         STRINOUT        ; PRINT STRING TO THE SCREEN
                LDX         ,S              ; GET INPUT POINTER
                LDD         $01,X           ; GET THE UNDEFINED LINE NUMBER
                JSR         >LBDCC          ; CONVERT NUMBER IN ACCD TO DECIMAL AND DISPLAY IT
                JSR         >LBDC5          ; PRINT IN XXXX XXXX = CURRENT LINE NUMBER
                JSR         >LB958          ; SEND A CR TO CONSOLE OUT
                PULS        X               ; GET INPUT POINTER BACK
L8BAE           PSHS        X               ; SAVE CURRENT POSITION OF INPUT POINTER
                LDD         $01,X           ; LOAD ACCD WITH BINARY VALUE OF LINE NUMBER
                STD         FPA0+2          ; SAVE IN BOTTOM 2 BYTES OF FPA0
                JSR         >L880E          ; ADJUST REST OF FPA0 AS AN INTEGER
                JSR         >LBDD9          ; CONVERT FPA0 TO ASCII, STORE IN LINE NUMBER
                PULS        U               ; LOAD U WITH PREVIOUS ADDRESS OF INPUT POINTER
                LDB         #$05            ; EACH EXPANDED LINE NUMBER USES 5 BYTES
L8BBE           LEAX        $01,X           ; MOVE POINTER FORWARD ONE
                LDA         ,X              ; GET AN ASCII BYTE
                BEQ         L8BC9           ; BRANCH IF END OF NUMBER
                DECB                        ;  DECREMENT BYTE COUNTER
                STA         ,U+             ; STORE ASCII NUMBER IN BASIC LINE
                BRA         L8BBE           ; CHECK FOR ANOTHER DIGIT
L8BC9           LEAX        ,U              ; TRANSFER NEW LINE POINTER TO (X)
                TSTB                        ;  DOES THE NEW LINE NUMBER REQUIRE 5 BYTES?
                BEQ         L8B8C           ; YES - GO GET ANOTHER INPUT CHARACTER
                LEAY        ,U              ; SAVE NEW LINE POINTER IN Y
                LEAU        B,U             ; POINT U TO END OF 5 BYTE PACKED LINE NUMBER BLOCK
                JSR         >L89B8          ; MOVE BYTES FROM (U) TO (X) UNTIL END OF PROGRAM
                LEAX        ,Y              ; LOAD (X) WITH NEW LINE POINTER
                BRA         L8B8C           ; GO GET ANOTHER INPUT CHARACTER
L8BD9           FCC         'UL '           ; UNKNOWN LINE NUMBER MESSAGE
                FCB         0
; CONVERT AN INTEGER INTO AN ASCII STRING AND PRINT IT ON THE SCREEN
HEXDOL          JSR         >LB740          ; CONVERT FPA0 INTO A POSITIVE 2 BYTE INTEGER
                LDX         #STRBUF+2       ; POINT TO TEMPORARY BUFFER
                LDB         #$04            ; CONVERT 4 NIBBLES
L8BE5           PSHS        B               ; SAVE NIBBLE COUNTER
                CLRB                        ;  CLEAR CARRY FLAG
                LDA         #$04            ; 4 SHIFTS
L8BEA           ASL         FPA0+3          ; SHIFT BOTTOM TWO BYTES OF
                ROL         FPA0+2          ; FPA0 LEFT ONE BIT (X2)
                ROLB                        ;  IF OVERFLOW, ACCB <> 0
                DECA                        ;  DECREMENT SHIFT COUNTER AND
                BNE         L8BEA           ; BRANCH IF NOT DONE
                TSTB                        ;  CHECK FOR OVERFLOW
                BNE         L8BFF           ; BRANCH IF OVERFLOW
                LDA         ,S              ; GET NIBBLE COUNTER,
                DECA                        ;  DECREMENT IT AND
                BEQ         L8BFF           ; BRANCH IF DONE
                CMPX        #STRBUF+2       ; DO NOT DO A CONVERSION UNTIL A NON-ZERO
                BEQ         L8C0B           ; BYTE IS FOUND - LEADING ZERO SUPPRESSION
L8BFF           ADDB        #'0             ; ADD IN ASCII ZERO
                CMPB        #'9             ; COMPARE TO ASCII 9
                BLS         L8C07           ; BRANCH IF < 9
                ADDB        #'A-('9+1)      ; ADD ASCII OFFSET IF HEX LETTER
L8C07           STB         ,X+             ; STORE HEX VALUE AND ADVANCE POINTER
                CLR         ,X              ; CLEAR NEXT BYTE - END OF STRING FLAG
L8C0B           PULS        B               ; GET NIBBLE COUNTER,
                DECB                        ;  DECREMENT IT AND
                BNE         L8BE5           ; BRANCH IF NOT DONE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF OF STACK
                LDX         #STRBUF+1       ; RESET POINTER
                JMP         >LB518          ; SAVE STRING ON STRING STACK
; DLOAD
DLOAD           JSR         >LA429          ; CLOSE FILES




; -----------------------------------------------------------------------------
                if          VEREXTBAS<20
; -----------------------------------------------------------------------------



; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
; the DLOAD bug. the code in version 1.0 did not allow for the fact
; that the current basic input character was not in ACCA following the
; CLOSEing of cassette files (JSR LA429).

DLDBUG          CLR         ,-S             ; SAVE DEFAULT TOKEN (NON DLOADM) ON STACK
                CMPA        #'M             ; IS IT DLOADM?
                BNE         L8C25           ; NO
                STA         ,S              ; SAVE THE M ON THE STACK
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; patch to fix the DLOAD bug.

DLDBUG          JSR         GETCCH          ; GET THE CURRENT INPUT CHARACTER
                SUBA        #'M             ; CHECK FOR DLOADM
                PSHS        A               ; SAVE DLOADM (=0), OLOAD (<>0) FLAG
                BNE         L8C25           ; BRANCH IF OLOAD
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

                JSR         GETNCH          ; GET AN INPUT CHAR FROM BASIC
L8C25           JSR         >LA578          ; GET THE NAME OF FILE FROM BASIC
                JSR         GETCCH          ; GET CURRENT INPUT CHAR FROM BASIC
                BEQ         L8C44           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                CMPA        #',             ; CHECK FOR TWO CONSECUTIVE COMMAS
                BEQ         L8C44           ; BRANCH IF,, - IF THIS CASE IS SELECTED
; THE BAUD DELAY MUST HAVE BEEN PREVIOUSLY STORED IN DIBAUD
                JSR         EVALEXPB        ; EVAL EXPR, RETURN VALUE IN ACCB
L8C36           LDA         #44*4           ; DELAY VALUE FOR 300 BAUD
                TSTB                        ;  WAS ARGUMENT = 0?
                BEQ         L8C42           ; YES - 300 BAUD
                LDA         #44             ; DELAY VALUE FOR 1200 BAUD
                DECB                        ;  CHECK FOR ARGUMENT OF 1
                LBNE        LB44A           ; FC ERROR IF NOT ZERO OR ONE OR COMMA
L8C42           STA         DLBAUD          ; SAVE DELAY VALUE
L8C44           JSR         >L8CD0          ; TRANSMIT FILE NAME AND READ IN FILE STATUS
                PSHS        A               ; SAVE ACCA
                LDA         #-3             ; DLOAD DEVICE NUMBER TO -3
                STA         DEVNUM          ; SET DEVICE NUMBER TO DLOAD
                PULS        A               ; RESTORE ACCA
                TST         ,S+             ; DLOAD OR DLOADM?


; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
                BNE         L8C85           ; DLOADM
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; BNE became BEQ as necessitated by the previous fix to DLOAD
                BEQ         L8C85           ; DLOADM
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------

; READ IN A DLOAD FILE
                JSR         >LA5C7          ; CHECK FOR END OF LINE - SYNTAX ERROR IF NOT

; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; Version 2.0 (see coco3.asm)
DLDBUG          ORCC        #$50            ; DISABLE INTERRUPTS
                LDA         #$0A
                STA         >INIT0          ; COCO3, WITH MMU, NO IRQ, NO FIRQ, RAM AT FEXX, 32K INTERNAL ROM
                CLR         >ROMCLR         ; ROM DISABLED
                JMP         >DOSBAS         ; INITIALIZE COCO3 DOS BASIC
L8C28           CLR         >$FEED          ; INT.FLAG
                CLR         PIA1+3
L8C2E           LDA         #$CC
                STA         >INIT0
                CLR         >ROMCLR
L8C36           RTS
                PSHS        X,B,A
                LDX         $88
                LDB         HRWIDTH
                LBNE        $F7AE           ; ALINK22
                LDB         1,S
                JMP         >LA30E
L8C46           PSHS        CC
                TST         HRWIDTH
                BEQ         L8C4F
                JMP         >$F6AD          ; ALINK23
L8C4F           PULS        CC
                JMP         >LA913
                NOP                         ;
                FCB         $C7
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------


                TSTB                        ;  CHECK ASCII FLAG
                BEQ         L8C5F           ; FM ERROR IF NOT ASCII
                JSR         >LAD19          ; GO DO A NEW
                JMP         >LAC7C          ; JUMP BACK TO BASICS MAIN INPUT LOOP;
; DLOAD FILES MUST BE ASCII FILES
L8C5F           JMP         >LA616          ; BAD FILE MODE ERROR
; EXBAS CLOAD PROCESSOR
L8C62           JSR         GETNCH          ; GET A CHAR FROM BASIC
                CMPA        #'M             ; CHECK FOR CLOADM
                LBNE        CLOAD           ; GO DO A CLOAD
                CLR         FILSTA          ; CLOSE FILES
                JSR         GETNCH          ; GET A CHAR FROM BASIC
                JSR         >LA578          ; STRIP A FILENAME OFF OF THE BASIC LINE
                JSR         >LA648          ; SEARCH FOR FILE
                TST         CASBUF+10       ; CHECK FILE MODE
                LBEQ        LA505           ; BRANCH TO CLOADM IF NOT BLOCK LOAD
                LDU         CASBUF+8        ; SAVE FILE TYPE AND ASCII FLAG IN U
                DEC         DEVNUM          ; SET DEVICE NUMBER TO -1 (CASSETTE)
                JSR         >LA635          ; GO READ IN A DATA BLOCK
                TFR         U,D             ; PUT FILE TYPE & ASCII FLAG BACK IN ACCD
; STRIP A LOAD OFFSET FROM THE BASIC LINE, THEN LOAD IN BLOCKS OF
; DATA (CLOADM,DLOADM) WHICH ARE PRECEEDED BY A 5 BYTE PRE OR POSTB16
; AMBLE. THE PREAMBLE CONTAINS A BLOCK LENGTH AND A LOAD ADDRESS SO
; THAT ANY NUMBER OF NON-CONTIGUOUS BLOCKS MAY BE LOADED. THE POST-
; AMBLE WILL TERMINATE THE LOADING PROCESS AND PROVIDE A TRANSFER ADDRESS
L8C85           SUBD        #$200           ; CHECK FILE STATUS;
                BNE         L8C5F           ; FM ERROR IF MODE <> 2 OR TYPE <> 0
                LDX         ZERO            ; ZERO THE X REG - DEFAULT OFFSET
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         L8C96           ; BRANCH IF END OF LINE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVAL INTEGER EXPR - RETURN VALUE IN X
L8C96           STX         VD3             ; SAVE OFFSET
                JSR         >LA5C7          ; SYNTAX ERROR IF MORE CHARS ON LINE
L8C9B           BSR         L8CC6           ; GO GET EOF FLAG FROM CONSOLE IN
                PSHS        A               ; SAVE IT ON THE STACK
                BSR         L8CBF           ; READ IN BLOCK LENGTH FROM CONSOLE IN
                TFR         D,Y             ; AND SAVE IT IN Y
                BSR         L8CBF           ; GET LOAD ADDRESS FROM CONSOLE IN
                ADDD        VD3             ; ADD OFFSET TO LOAD ADDRESS
                STD         EXECJP          ; SAVE IN EXEC ADDRESS
                TFR         D,X             ; SAVE LOAD ADDRESS IN X
                LDA         ,S+             ; GET EOF FLAG FROM STACK
                LBNE        LA42D           ; CLOSE FILES IF POSTAMBLE BLOCK
L8CB1           BSR         L8CC6           ; GET A CHARACTER FROM CONSOLE IN
                STA         ,X              ; SAVE IT IN RAM
                CMPA        ,X+             ; COMPARE SAVED BYTE TO ACTUAL BYTE
                BNE         L8CCD           ; 'IO ERROR IF NOT = (SAVED IN ROM OR BAD RAM)
                LEAY        -1,Y            ; DECREMENT BYTE COUNT
                BNE         L8CB1           ; READ MORE CHARACTERS
                BRA         L8C9B           ; LOOK FOR ANOTHER BLOCK OF DATA
; GET 2 CHARACTERS - RETURN THEM IN ACCD
L8CBF           BSR         L8CC1           ; GET A CHARACTER IN ACCB
L8CC1           BSR         L8CC6           ; GET A CHARACTER IN ACCA
                EXG         A,B             ; SAVE IT IN ACCB
L8CC5           RTS
L8CC6           JSR         >LA176          ; GET A CHARACTER FROM CONSOLE IN
                TST         CINBFL          ; IS FILE EMPTY?
                BEQ         L8CC5           ; RETURN IF NOT EMPTY
L8CCD           JMP         >LA619          ; IO ERROR IF EMPTY
L8CD0           BSR         L8D14           ; TRANSMIT FILE NAME, RETURN FILE STATUS
                PSHS        B,A             ; SAVE FILE STATUS ON STACK
                INCA                        ;  CHECK FILE TYPE
                BEQ         L8CDD           ; NE ERROR IF FILE NOT FOUND
                LDU         ZERO            ; ZERO U REG :FIRST BLOCK NUMBER
                BSR         L8CE4           ; READ IN 128 CHARACTERS
                PULS        A,B,PC          ; GET FILE STATUS BACK AND RETURN
L8CDD           LDB         #2*26           ; NE ERROR
                JMP         >LAC46          ; GO TO ERROR SERVICING ROUTINE
; REFILL CONSOLE IN CHARACTER BUFFER FROM DLOAD
L8CE2           LDU         CBUFAD          ; GET BLOCK NUMBER
L8CE4           LEAX        $01,U           ; INCREMENT BLOCK NUMBER
                STX         CBUFAD          ; AND SAVE IT
                LDX         #CASBUF         ; USE CASBUF AS DLOAD INPUT BUFFER
                JSR         >L8D7C          ; READ 128 CHARACTERS (ONE BLOCK) INTO BUFFER
                JMP         >LA644          ; RESET CONSOLE IN BUFFER
; CONSOLE IN RAM HOOK
XVEC4           LDA         DEVNUM          ; GET DEVICE NUMBER
                CMPA        #-3             ; DLOAD DEVICE NUMBER
                BNE         L8D01           ; BRANCH IF NOT OLOAD
                LEAS        $02,S           ; PURGE 1ST RETURN ADDR FROM STACK
                CLR         CINBFL          ; RESET EMPTY/FULL FLAG
                TST         CINCTR          ; ANY CHARACTERS LEFT IN BUFFER?
                BNE         L8D02           ; YES, GO GET ONE
                COM         CINBFL          ; SET EMPTY/FULL FLAG TO EOF
L8D01           RTS
L8D02           PSHS        U,Y,X,B         ; SAVE REGISTERS
                LDX         CINPTR          ; GET CONSOLE IN CHARACTER BUFFER
                LDA         ,X+             ; GET A CHARACTER
                PSHS        A               ; SAVE IT ON THE STACK
                STX         CINPTR          ; SAVE NEW CHARACTER BUFFER
                DEC         CINCTR          ; DECREMENT CHARACTER COUNTER
                BNE         L8D12           ; RETURN IF BUFFER NOT EMPTY
                BSR         L8CE2           ; GO REFILL THE CHARACTER BUFFER
L8D12           PULS        A,B,X,Y,U,PC    ; RESTORE REGISTERS AND RETURN
; TRANSMIT FILE NAME - READ FILE STATUS FROM SENDER
L8D14           CLRA                        ;  RESET ATTEMPT COUNTER
                PSHS        X,B,A           ; SAVE SPACE ON STACK FOR TEMP VARIABLES
                LEAY        ,S              ; STACK TO Y (TFR S,Y) - SAVE VARIABLE POINTER
                BRA         L8D1D
L8D1B           BSR         L8D48           ; INCREMENT ATTEMPT COUNTER
L8D1D           LDA         #$8A            ; GET FILE REQUEST CONTROL CODE
                BSR         L8D58           ; AND TRANSMIT IT
                BNE         L8D1B           ; BRANCH IF NO ECHO OR ERROR
                LDX         #CFNBUF+1       ; POINT TO CASS FILE NAME BUFFER
L8D26           LDA         ,X+             ; GET CHARACTER FROM NAME BUFFER
                JSR         >L8E04          ; OUTPUT IT TO RS 232 PORT
                CMPX        #CFNBUF+9       ; COMPARE TO END OF BUFFER
                BNE         L8D26           ; LOOP UNTIL DONE
                BSR         L8D62           ; OUTPUT CHECK BYTE AND LOOK FOR ACKNOWLEDGE
                BNE         L8D1B           ; TRANSMIT NAME AGAIN IF NO ACKNOWLEDGE
                BSR         L8D72           ; GET FILE TYPE $FF = NOT FOUND
                BNE         L8D1B           ; BRANCH IF ERROR
                STA         $02,Y           ; SAVE FILE TYPE
                BSR         L8D72           ; READ ASCII FLAG
                BNE         L8D1B           ; BRANCH IF ERROR
                STA         $03,Y           ; SAVE ASCII FLAG
                BSR         L8D6B           ; READ CHECK BYTE FROM SENDER
                BNE         L8D1B           ; BRANCH IF NO CHECKBYTE MATCH
                LEAS        $02,S           ; PURGE ATTEMPT COUNTER & CHECK BYTE FROM STACK
                PULS        A,B,PC          ; RETURN FILE STATUS IN ACCD
; INCREMENT ATTEMPT COUNTER - AFTER 5 TRIES, GIVE UP (IO ERROR)
L8D48           INC         ,Y              ; INCREMENT ATTEMPT COUNTER
                LDA         ,Y              ; GET ATTEMPT COUNTER
                CMPA        #$05            ; IS THIS THE FIFTH TRY?
                BLO         L8D6A           ; NO
                LDA         #$BC            ; YES ; TIME TO QUIT-GET ABORT CODE
                JSR         >L8E0C          ; OUTPUT ABORT CODE OVER THE RS 232 PORT
                JMP         >LA619          ; IO ERROR
; ECHO CHECK - OUTPUT A CHARACTER, READ A CHARACTER AND
; COMPARE IT TO THE OUTPUT CHARACTER. Z=0 IF NO MATCH OR ERROR
L8D58           PSHS        A               ; SAVE COMPARE CHARACTER ON STACK
                BSR         L8DB8           ; SEND A CHARACTER OUT
                BNE         L8D60           ; BRANCH IF READ ERROR
                CMPA        ,S              ; COMPARE RECEIVED CHARACTER TO TRANSMITTED CHARACTER
L8D60           PULS        A,PC            ; RESTORE COMPARE CHARACTER AND RETURN
; TRANSMIT XOR CHECKBYTE AND READ ACKNOWLEGE ($C8)
; RETURN ZERO FLAG SET IF NO ERROR AND ACKNOWLEGE
L8D62           LDA         $01,Y           ; GET XOR CHECKBYTE
                BSR         L8DB8           ; OUTPUT XOR CHECKBYTE AND READ ONE BYTE
                BNE         L8D6A           ; BRANCH IF READ ERROR
                CMPA        #$C8            ; COMPARE INPUT BYTE TO ACKNOWLEDGE CODE
L8D6A           RTS
; READ XOR CHECKBYTE THEN LOAD ACCUMULATED XOR CHECKBYTE.
; SET ZERO FLAG IF ACCUMULATED CHECK BYTE = 0
L8D6B           BSR         L8D72           ; INPUT A CHARACTER FROM RS 232
                BNE         L8D6A           ; BRANCH IF TIMEOUT
                LDA         $01,Y           ; GET CHECK BYTE
                RTS
L8D72           BSR         L8DBC           ; INPUT A CHARACTER FROM RS 232
                PSHS        A,CC            ; SAVE CHARACTER AND ZERO FLAG ON STACK
                EORA        $01,Y           ; EXCLUSIVE OR INPUT
                STA         $01,Y           ; CHARACTER WITH CHECK BYTE
                PULS        CC,A,PC         ; RESTORE CHARACTER AND ZERO FLAG
; REQUEST A BLOCK FROM RS 232 INPUT -
; LOAD THE RECEIVED DATA INTO THE BUFFER POINTED TO BY X
; U REGFISTER CONTAINS THE BLOCK NUMBER; RETURN Z=1 IF NO
; ERRORS, CHARACTER COUNT IN ACCA; ACCA = 0 IF FILE EMPTY
L8D7C           CLRA                        ;  RESET ATTEMPT COUNTER
                PSHS        U,Y,X,B,A       ; SAVE SPACE FOR STACK BUFFER
                ASL         $07,S           ; 6,7 S (U REG) CONTAIN THE 14 BIT BLOCK NUMBER -
                ROL         $06,S           ; PUT THE BOTTOM 7 BITS IN 7,S AND THE
                LSR         $07,S           ; TOP SEVEN BITS IN 6,S
                LEAY        ,S              ; STACK POINTER TO Y (TFR S,Y)
                BRA         L8D8B
L8D89           BSR         L8D48           ; INCREMENT ATTEMPT COUNTER
L8D8B           LDA         #$97            ; TRANSMIT A BLOCK REQUEST CODE, ECHO
                BSR         L8D58           ; CHECK AND RESET CHECK BYTE
                BNE         L8D89           ; BRANCH IF NO MATCH OR ERROR
                LDA         $06,Y           ; SEND OUT HIGH ORDER SEVEN BITS
                BSR         L8E04           ; OF BLOCK NUMBER
                LDA         $07,Y           ; SEND OUT LOW ORDER SEVEN BITS
                BSR         L8E04           ; OF BLOCK NUMBER
                BSR         L8D62           ; TRANSMIT CHECK BYTE AND GET ACKNOWLEDGE
                BNE         L8D89           ; BRANCH IF ERROR OR NO ACKNOWLEDGE
                BSR         L8D72           ; READ CHARACTER COUNT
                BNE         L8D89           ; BRANCH IF READ ERROR
                STA         $04,Y           ; SAVE CHARACTER COUNT IN STACK VARIABLES
                LDX         $02,Y           ; GET VARIABLES POINTER FROM STACK BUFFER
; READ IN A BLOCK OF 128 CHARACTERS - THE HOST WILL TRANSMIT 128
; CHARACTERS REGARDLESS OF HOW MANY ARE VALID. OF HOW MANY ARE VALID.
                LDB         #128            ; 128 CHARACTERS/BUFFER
L8DA7           BSR         L8D72           ; READ A CHARACTER
                BNE         L8D89           ; RESTART PROCESS IF READ ERROR
                STA         ,X+             ; SAVE THE CHARACTER IN BUFFER
                DECB                        ;  DECREMENT CHARACTER COUNTER
                BNE         L8DA7           ; BRANCH IF NOT DONE
                BSR         L8D6B           ; INPUT XOR CHECKBYTE
                BNE         L8D89           ; RESTART PROCESS IF READ ERROR OR BAD CHECKBYTE
                LEAS        $04,S           ; PURGE ATTEMPT COUNTER, CHECK BYTE AND LOAD ADDRESS FROM STACK
                PULS        A,B,X,PC        ; RETURN CHARACTER COUNT IN ACCA
L8DB8           CLR         $01,Y           ; CLEAR CHECK BYTE
                BSR         L8E0C           ; OUTPUT A CHARACTER OVER RS 232 PORT
; READ A CHARACTER FROM THE RS 232 INPUT PORT.
; RETURN CHARACTER IN ACCA. EXIT WITH Z=0
; FOR TIMEOUT ERROR, Z = 1 FOR VALID BYTE INPUT.
L8DBC           CLRA                        ;  CLEAR ATTEMPT COUNTER
                PSHS        X,B,CC          ; SAVE REGISTERS AND INTERRUPT STATUS
                ORCC        #$50            ; DISABLE INTERRUPTS
                LDA         TIMOUT          ; GET TIMEOUT VARIABLE DELAY
                LDX         ZERO            ; X=0: TIMEOUT CONSTANT DELAY
L8DC5           BSR         L8DE6           ; GO GET RS 232 STATUS
                BCC         L8DC5           ; LOOP IF SPACING
L8DC9           BSR         L8DE6           ; GET RS 232 STATUS
                BLO         L8DC9           ; LOOP IF MARKING
                BSR         L8DF9           ; DELAY 1/2 BIT TIME
                LDB         #$01            ; GET BIT SHIFT COUNTER AND BIT
                PSHS        B               ; MASK AND SAVE IT ON STACK
                CLRA                        ;  RESET DATA BYTE
L8DD4           BSR         L8DF7           ; GO DELAY ONE BIT TIME
                LDB         PIA1+2          ; RS 232 INPUT TO
                RORB                        ;  CARRY FLAG
                BCC         L8DDE           ; BRANCH IF RS 232 INPUT = 0 (SPACING)
                ORA         ,S              ; IF MARKING, OR A 1 BIT INTO DATA BYTE
L8DDE           ASL         ,S              ; SHIFT BIT COUNTER ONE BIT TO LEFT
                BCC         L8DD4           ; CARRY WILL BE SET AFTER 8 SHIFTS
                LEAS        $01,S           ; PULL BIT COUNTER OFF THE STACK
                PULS        CC,B,X,PC       ; RESTORE INTERRUPT STATUS & RETURN
; PUT RS 232 STATUS INTO THE CARRY FLAG AND CHECK FOR TIMEOUT
L8DE6           LDB         PIA1+2          ; RS 232 INPUT TO
                RORB                        ;  CARRY FLAG
                LEAX        $01,X           ; INCREMENT CONSTANT TIMEOUT
                BNE         L8DF6           ; DELAY, RETURN IF <> 0
                DECA                        ;  DECREMENT VARIABLE TIMEOUT
                BNE         L8DF6           ; DELAY: RETURN IF <> 0
; DLOAD HAS TIMED OUT HERE
                LEAS        $02,S           ; PURGE RETURN ADDRESS OFF STACK
                PULS        CC,B,X          ; CLEAN UP STACK/RESTORE INTERRUPTS
                INCA                        ;  SET ACCA = 1 ZERO FLAG = 0
L8DF6           RTS
; DELAY LOOP -- COUNT DOWN DLBAUD
L8DF7           BSR         L8DF9           ; CALL DELAY ROUTINE
L8DF9           PSHS        A               ; SAVE ACCA
                LDA         DLBAUD          ; GET DLOAD DELAY - 1/2 BIT TIME DELAY
L8DFD           BRN         L8DFD           ; DUMMY INST - JUST ADD TO DELAY
                DECA                        ;  DEC DELAY TIMER
                BNE         L8DFD           ; NOT DONE
                PULS        A,PC            ; RESTORE ACCA AND RETURN

L8E04           PSHS        A               ; SAVE CHARACTER ON STACK
                EORA        $01,Y           ; EOR CHARACTER WITH 1,Y AND
                STA         $01,Y           ; SAVE RESULT IN 1,Y
                PULS        A               ; GET CHARACTER BACK
; SEND CHAR IN ACCA OUT OVER RS232 OUTPUT
L8E0C           PSHS        B,A,CC          ; SAVE ACCD AND INTERRUPT STATUS
                ORCC        #$50            ; DISABLE INTERRUPTS
                BSR         L8DF7           ; DELAY AWHILE
                BSR         L8DF7           ; DELAY SOME MORE
                CLR         PIA1            ; SET R5232 OUTPUT TO SPACING
                BSR         L8DF7           ; DELAY SOME MORE - START BIT
                LDB         #$01            ; BIT CTR - SEND 8 BITS
                PSHS        B               ; SAVE BIT CTR ON STACK
L8E1D           LDA         $02,S           ; GET OUTPUT BYTE
                ANDA        ,S              ; AND IT W/THE BIT CTR
                BEQ         L8E25           ; THIS BIT IN OUTPUT BYTE = 0
                LDA         #$02            ; OUTPUT BIT = 1; SET R5232 TO MARKING
L8E25           STA         PIA1            ; BET R5232 TO VALUE IN ACCA
                BSR         L8DF7           ; DELAY FOR AWHILE
                ASL         ,S              ; SHIFT BIT CTR
                BCC         L8E1D           ; WHEN CARRY SET, 8 BITS DONE
                LDA         #$02            ; WHEN DONE, SET R5232 TO MARKING
                STA         PIA1            ; SET R5232 OUTPUT
                LEAS        $01,S           ; PULL BIT CTR OFF THE STACK
                PULS        CC,A,B,PC       ; RESTORE ACCD, INTERRUPTS & RETURN
; PROCESS EXCLAMATION POINT
L8E37           LDA         #$01            ; SET SPACES
                STA         VD9             ; COUNTER = 1
; PROCESS STRING ITEM - LIST
L8E3B           DECB                        ;  DECREMENT FORMAT STRING LENGTH COUNTER
                JSR         >L8FD8          ; SEND A '+' TO CONSOLE OUT IF VDA <>0
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                LBEQ        L8ED8           ; EXIT PRINT USING IF END OF LINE
                STB         VD3             ; SAVE REMAINDER FORMAT STRING LENGTH
                JSR         >LB156          ; EVALUATE EXPRESSION
                JSR         >LB146          ; TM ERROR IF NUMERIC VARIABLE
                LDX         FPA0+2          ; GET ITEM - LIST DESCRIPTOR ADDRESS
                STX         V4D             ; AND SAVE IT IN V4D
                LDB         VD9             ; GET SPACES COUNTER
                JSR         >LB6AD          ; PUT ACCB BYTES INTO STRING SPACE & PUT DESCRIPTOR ON STRING STACK
                JSR         >LB99F          ; PRINT THE FORMATTED STRING TO CONSOLE OUT
; PAD FORMAT STRING WITH SPACES IF ITEM - LIST STRING < FORMAT STRING LENGTH
                LDX         FPA0+2          ; POINT X TO FORMATTED STRING DESCRIPTOR ADDRESS
                LDB         VD9             ; GET SPACES COUNTER
                SUBB        ,X              ; SUBTRACT LENGTH OF FORMATTED STRING
L8E5F           DECB                        ;  DECREMENT DIFFERENCE
                LBMI        L8FB3           ; GO INTERPRET ANOTHER ITEM - LIST
                JSR         >LB9AC          ; PAD FORMAT STRING WITH A SPACE
                BRA         L8E5F           ; KEEP PADDING
; PERCENT SIGN - PROCESS A %SPACES% COMMAND
L8E69           STB         VD3             ; SAVE THE CURRENT FORMAT STRING
                STX         TEMPTR          ; COUNTER AND POINTER
                LDA         #$02            ; INITIAL SPACES COUNTER = 2
                STA         VD9             ; SAVE IN SPACES COUNTER
L8E71           LDA         ,X              ; GET A CHARACTER FROM FORMAT STRING
                CMPA        #'%             ; COMPARE TO TERMINATOR CHARACTER
                BEQ         L8E3B           ; BRANCH IF END OF SPACES COMMAND
                CMPA        #$20            ; BLANK SPACE
                BNE         L8E82           ; BRANCH IF ILLEGAL CHARACTER
                INC         VD9             ; ADD ONE TO SPACES COUNTER
                LEAX        $01,X           ; MOVE FORMAT POINTER UP ONE
                DECB                        ;  DECREMENT LENGTH COUNTER
                BNE         L8E71           ; BRANCH IF NOT END OF FORMAT STRING
L8E82           LDX         TEMPTR          ; RESTORE CURRENT FORMAT STRING COUNTER
                LDB         VD3             ; AND POINTER TO POSITION BEFORE SPACES COMMAND
                LDA         #'%             ; SEND A % TO CONSOLE OUT AS A DEBUGGING AID
; ERROR PROCESSOR - ILLEGAL CHARACTER OR BAD SYNTAX IN FORMAT STRING
L8E88           JSR         >L8FD8          ; SEND A +' TO CONSOLE OUT IF VDA <> 0
                JSR         PUTCHR          ; SEND CHARACTER TO CONSOLE OUT
                BRA         L8EB9           ; GET NEXT CHARACTER IN FORMAT STRING
; PRINT RAM HOOK
XVEC9           CMPA        #$CD            ; USING TOKEN
                BEQ         L8E95           ; BRANCH IF PRINT USING
                RTS
; PRINT USING
; VDA IS USED AS A STATUS BYTE: BIT 6 = COMMA FORCE
; BIT 5=LEADING ASTERISK FORCE; BIT 4 = FLOATING $ FORCE
; BIT 3 = PRE SIGN FORCE; BIT 2 = POST SIGN FORCE; BIT 0 = EXPONENTIAL FORCE
L8E95           LEAS        $02,S           ; PURGE RETURN ADDRESS OFF THE STACK
                JSR         >LB158          ; EVALUATE FORMAT STRING
                JSR         >LB146          ; TM ERROR IF VARIABLE TYPE = NUMERIC
                LDB         #';             ; CHECK FOR ITEM LIST SEPARATOR
                JSR         >LB26F          ; SYNTAX CHECK FOR ;
                LDX         FPA0+2          ; GET FORMAT STRING DESCRIPTOR ADDRESS
                STX         VD5             ; AND SAVE IT IN VD5
                BRA         L8EAE           ; GO PROCESS FORMAT STRING
L8EA8           LDA         VD7             ; CHECK NEXT PRINT ITEM FLAG AND
                BEQ         L8EB4           ; FC ERROR IF NO FURTHER PRINT ITEMS
                LDX         VD5             ; RESET FORMAT STRING POINTER TO START OF STRING
L8EAE           CLR         VD7             ; RESET NEXT PRINT ITEM FLAG
                LDB         ,X              ; GET LENGTH OF FORMAT STRING
                BNE         L8EB7           ; INTERPRET FORMAT STRING IF LENGTH > 0
L8EB4           JMP         >LB44A          ; FC ERROR IF FORMAT STRING = NULL
L8EB7           LDX         $02,X           ; POINT X TO START OF FORMAT STRING
; INTERPRET THE FORMAT STRING
L8EB9           CLR         VDA             ; CLEAR THE STATUS BYTE
L8EBB           CLR         VD9             ; CLEAR LEFT DIGIT COUNTER
                LDA         ,X+             ; GET A CHARACTER FROM FORMAT STRING
                CMPA        #'!             ; EXCLAMATION POINT?
                LBEQ        L8E37           ; YES - STRING TYPE FORMAT
                CMPA        #'#             ; NUMBER SIGN? (DIGIT LOCATOR)
                BEQ         L8F24           ; YES - NUMERIC TYPE FORMAT
                DECB                        ;  DECREMENT FORMAT STRING LENGTH
                BNE         L8EE2           ; BRANCH IF NOT DONE
                JSR         >L8FD8          ; SEND A + TO CONSOLE OUT IF VDA <> 0
                JSR         PUTCHR          ; SEND CHARACTER TO CONSOLE OUT
L8ED2           JSR         GETCCH          ; GET CURRENT CHARACTER FROM BASIC
                BNE         L8EA8           ; BRANCH IF NOT END OF LINE
                LDA         VD7             ; GET NEXT PRINT ITEM FLAG
L8ED8           BNE         L8EDD           ; BRANCH IF MORE PRINT ITEMS
                JSR         >LB958          ; SEND A CARRIAGE RETURN TO CONSOLE OUT
L8EDD           LDX         VD5             ; POINT X TO FORMAT STRING DESCRIPTOR
                JMP         >LB659          ; RETURN ADDRESS AND LENGTH OF FORMAT STRING - EXIT PRINT USING
L8EE2           CMPA        #'+             ; CHECK FOR + (PRE-SIGN FORCE)
                BNE         L8EEF           ; NO PLUS
                JSR         >L8FD8          ; SEND A +' TO CONSOLE OUT IF VDA <> 0
                LDA         #$08            ; LOAD THE STATUS BYTE WITH 8;
                STA         VDA             ; PRE-SIGN FORCE FLAG
                BRA         L8EBB           ; INTERPRET THE REST OF THE FORMAT STRING
L8EEF           CMPA        #'.             ; DECIMAL POINT?
                BEQ         L8F41           ; YES
                CMPA        #'%             ; PERCENT SIGN?
                LBEQ        L8E69           ; YES
                CMPA        ,X              ; COMPARE THE PRESENT FORMAT STRING INPUT
; CHARACTER TO THE NEXT ONE IN THE STRING
L8EFB           BNE         L8E88           ; NO MATCH - ILLEGAL CHARACTER
; TWO CONSECUTIVE EQUAL CHARACTERS IN FORMAT STRING
                CMPA        #'$             ; DOLLAR SIGN?
                BEQ         L8F1A           ; YES - MAKE THE DOLLAR SIGN FLOAT
                CMPA        #'*             ; ASTERISK?
                BNE         L8EFB           ; NO - ILLEGAL CHARACTER
                LDA         VDA             ; GRAB THE STATUS BYTE AND BET BIT 5
                ORA         #$20            ; TO INDICATE THAT THE OUTPUT WILL
                STA         VDA             ; BE LEFT PADDED WITH ASTERISKS
                CMPB        #2              ; CHECK TO SEE IF THE $$ ARE THE LAST TWO
                BLO         L8F20           ; CHARACTERS IN THE FORMAT STRING AND BRANCH IF SO
                LDA         $01,X           ; GET THE NEXT CHARACTER AFTER **
                CMPA        #'$             ; CHECK FOR **$
                BNE         L8F20           ; CHECK FOR MORE CHARACTERS
                DECB                        ;  DECREMENT STRING LENGTH COUNTER
                LEAX        $01,X           ; MOVE FORMAT STRING POINTER UP ONE
                INC         VD9             ; ADD ONE TO LEFT DIGIT COUNTER - FOR ASTERISK PAD AND
; FLOATING DOLLAR SIGN COMBINATION
L8F1A           LDA         VDA             ; GET THE STATUS BYTE AND SET
                ORA         #$10            ; BIT 4 TO INDICATE A
                STA         VDA             ; FLOATING DOLLAR SIGN
L8F20           LEAX        $01,X           ; MOVE FORMAT STRING POINTER UP ONE
                INC         VD9             ; ADD ONE TO LEFT DIGIT (FLOATING $ OR ASTERISK PAD)
; PROCESS CHARACTERS TO THE LEFT OF THE DECIMAL POINT IN THE FORMAT STRING
L8F24           CLR         VD8             ; CLEAR THE RIGHT DIGIT COUNTER
L8F26           INC         VD9             ; ADD ONE TO LEFT DIGIT COUNTER
                DECB                        ;  DECREMENT FORMAT STRING LENGTH COUNTER
                BEQ         L8F74           ; BRANCH IF END OF FORMAT STRING
                LDA         ,X+             ; GET THE NEXT FORMAT CHARACTER
                CMPA        #'.             ; DECIMAL POINT?
                BEQ         L8F4F           ; YES
                CMPA        #'#             ; NUMBER SIGN?
                BEQ         L8F26           ; YES
                CMPA        #',             ; COMMA?
                BNE         L8F5A           ; NO
                LDA         VDA             ; GET THE STATUS BYTE
                ORA         #$40            ; AND SET BIT 6 WHICH IS THE
                STA         VDA             ; COMMA SEPARATOR FLAG
                BRA         L8F26           ; PROCESS MORE CHARACTERS TO LEFT OF DECIMAL POINT
; PROCESS DECIMAL POINT IF NO DIGITS TO LEFT OF IT
L8F41           LDA         ,X              ; GET NEXT FORMAT CHARACTER
                CMPA        #'#             ; IS IT A NUMBER SIGN?
                LBNE        L8E88           ; NO
                LDA         #1              ; SET THE RIGHT DIGIT COUNTER TO 1 -
                STA         VD8             ; ALLOW ONE SPOT FOR DECIMAL POINT
                LEAX        $01,X           ; MOVE FORMAT POINTER UP ONE
; PROCESS DIGITS TO RIGHT OF DECIMAL POINT
L8F4F           INC         VD8             ; ADD ONE TO RIGHT DIGIT COUNTER
                DECB                        ;  DECREMENT FORMAT LENGTH COUNTER
                BEQ         L8F74           ; BRANCH IF END OF FORMAT STRING
                LDA         ,X+             ; GET A CHARACTER FROM FORMAT STRING
                CMPA        #'#             ; IS IT NUMBER SIGN?
                BEQ         L8F4F           ; YES - KEEP CHECKING
; CHECK FOR EXPONENTIAL FORCE
L8F5A           CMPA        #$5E            ; CHECK FOR UP ARROW
                BNE         L8F74           ; NO UP ARROW
                CMPA        ,X              ; IS THE NEXT CHARACTER AN UP ARROW?
                BNE         L8F74           ; NO
                CMPA        $01,X           ; AND THE NEXT CHARACTER?
                BNE         L8F74           ; NO
                CMPA        $02,X           ; HOW ABOUT THE 4TH CHARACTER?
                BNE         L8F74           ; NO, ALSO
                CMPB        #4              ; CHECK TO SEE IF THE 4 UP ARROWS ARE IN THE
                BLO         L8F74           ; FORMAT STRING AND BRANCH IF NOT
                SUBB        #4              ; MOVE POINTER UP 4 AND SUBTRACT
                LEAX        $04,X           ; FOUR FROM LENGTH
                INC         VDA             ; INCREMENT STATUS BYTE - EXPONENTIAL FORM
; CHECK FOR A PRE OR POST - SIGN FORCE AT END OF FORMAT STRING
L8F74           LEAX        -1,X            ; MOVE POINTER BACK ONE
                INC         VD9             ; ADD ONE TO LEFT DIGIT COUNTER FOR PRE-SIGN FORCE
                LDA         VDA             ; PRE-SIGN
                BITA        #$08            ; FORCE AND
                BNE         L8F96           ; BRANCH IF SET
                DEC         VD9             ; DECREMENT LEFT DIGIT NO PRE-SIGN FORCE
                TSTB                        ;  CHECK LENGTH COUNTER AND BRANCH
                BEQ         L8F96           ; IF END OF FORMAT STRING
                LDA         ,X              ; GET NEXT FORMAT STRING CHARACTER
                SUBA        #'-             ; CHECK FOR MINUS SIGN
                BEQ         L8F8F           ; BRANCH IF MINUS SIGN
                CMPA        #('+)-('-)      ; CHECK FOR PLUS SIGN
                BNE         L8F96           ; BRANCH IF NO PLUS SIGN
                LDA         #$08            ; GET THE PRE-SIGN FORCE FLAG
L8F8F           ORA         #$04            ; OR IN POST-SIGN FORCE FLAG
                ORA         VDA             ; OR IN THE STATUS BYTE
                STA         VDA             ; SAVE THE STATUS BYTE
                DECB                        ;  DECREMENT FORMAT STRING LENGTH
; EVALUATE NUMERIC ITEM-LIST
L8F96           JSR         GETCCH          ; GET CURRENT CHARACTER
                LBEQ        L8ED8           ; BRANCH IF END OF LINE
                STB         VD3             ; SAVE FORMAT STRING LENGTH WHEN FORMAT EVALUATION ENDED
                JSR         >LB141          ; EVALUATE EXPRESSION
                LDA         VD9             ; GET THE LEFT DIGIT COUNTER
                ADDA        VD8             ; ADD IT TO THE RIGHT DIGIT COUNTER
                CMPA        #17             ;
                LBHI        LB44A           ; FC ERROR IF MORE THAN 16 DIGITS AND DECIMAL POiNT
                JSR         >L8FE5          ; CONVERT ITEM-LIST TO FORMATTED ASCII STRING
                LEAX        -1,X            ; MOVE BUFFER POINTER BACK ONE
                JSR         STRINOUT        ; DISPLAY THE FORMATTED STRING TO CONSOLE OUT
L8FB3           CLR         VD7             ; RESET NEXT PRINT ITEM FLAG
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BEQ         L8FC6           ; BRANCH IF END OF LINE
                STA         VD7             ; SAVE CURRENT CHARACTER (<>0) IN NEXT PRINT ITEM FLAG
                CMPA        #';             ; CHECK FOR ; - ITEM-LIST SEPARATOR AND
                BEQ         L8FC4           ; BRANCH IF SEMICOLON
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                BRA         L8FC6           ; PROCESS NEXT PRINT ITEM
L8FC4           JSR         GETNCH          ; GET NEXT INPUT CHARACTER
L8FC6           LDX         VD5             ; GET FORMAT STRING DESCRIPTOR ADDRESS
                LDB         ,X              ; GET LENGTH OF FORMAT STRING
                SUBB        VD3             ; SUBTRACT AMOUNT OF FORMAT STRING LEFT AFTER LAST PRINT ITEM
                LDX         $02,X           ; GET FORMAT STRING START ADDRESS AND ADVANCE
                ABX                         ;  POINTER TO START OF UNUSED FORMAT STRING
                LDB         VD3             ; =GET AMOUNT OF UNUSED FORMAT STRING
                LBNE        L8EB9           ; =REINTERPRET FORMAT STRING FROM THAT POINT
                JMP         >L8ED2          ; REINTERPRET FORMAT STRING FROM THE START IF ENTIRELY
; USED ON LAST PRINT ITEM
; PRINT A + TO CONSOLE OUT IF THE STATUS BYTE <> 0
L8FD8           PSHS        A               ; RESTORE ACCA AND RETURN
                LDA         #'+             ; GET ASCII PLUS SIGN
                TST         VDA             ; CHECK THE STATUS BYTE AND
                BEQ         L8FE3           ; RETURN IF = 0
                JSR         PUTCHR          ; SEND A CHARACTER TO CONSOLE OUT
L8FE3           PULS        A,PC            ; RETURN ACCA AND RETURN
; CONVERT ITEM-LIST TO DECIMAL ASCII STRING
L8FE5           LDU         #STRBUF+4       ; POINT U TO STRING BUFFER
                LDB         #SPACE          ; BLANK
                LDA         VDA             ; GET THE STATUS FLAG AND
                BITA        #$08            ; CHECK FOR A PRE-SIGN FORCE
                BEQ         L8FF2           ; BRANCH IF NO PRE-SIGN FORCE
                LDB         #'+             ; PLUS SIGN
L8FF2           TST         FP0SGN          ; CHECK THE SIGN OF FPA0
                BPL         L8FFA           ; BRANCH IF POSITIVE
                CLR         FP0SGN          ; FORCE FPA0 SIGN TO BE POSITIVE
                LDB         #'-             ; MINUS SIGN
L8FFA           STB         ,U+             ; SAVE THE SIGN IN BUFFER
                LDB         #'0             ; PUT A ZERO INTO THE BUFFER
                STB         ,U+             ;
                ANDA        #$01            ; CHECK THE EXPONENTIAL FORCE FLAG IN
                LBNE        L910D           ; THE STATUS BYTE - BRANCH IF ACTIVE
                LDX         #LBDC0          ; POINT X TO FLOATING POINT 1E + 09
                JSR         >LBCA0          ; COMPARE FPA0 TO (X)
                BMI         L9023           ; BRANCH IF FPA0 < 1E+09
                JSR         >LBDD9          ; CONVERT FP NUMBER TO ASCII STRING
L9011           LDA         ,X+             ; ADVANCE POINTER TO END OF
                BNE         L9011           ; ASCII STRING (ZERO BYTE)
L9015           LDA         ,-X             ; MOVE THE
                STA         $01,X           ; ENTIRE STRING
                CMPX        #STRBUF+3       ; UP ONE
                BNE         L9015           ; BYTE
                LDA         #'%             ; INSERT A % SIGN AT START OF
                STA         ,X              ; STRING - OVERFLOW ERROR
                RTS
L9023           LDA         FP0EXP          ; GET EXPONENT OF FPA0
                STA         V47             ; AND SAVE IT IN V47
                BEQ         L902C           ; BRANCH IF FPA0 = 0
                JSR         >L91CD          ; CONVERT FPA0 TO NUMBER WITH 9 SIGNIFICANT
; PLACES TO LEFT OF DECIMAL POINT
L902C           LDA         V47             ; GET BASE 10 EXPONENT OFFSET
                LBMI        L90B3           ; BRANCH IF FPA0 < 100,000,000
                NEGA                        ;  CALCULATE THE NUMBER OF LEADING ZEROES TO INSERT -
                ADDA        VD9             ; SUBTRACT BASE 10 EXPONENT OFFSET AND 9 (FPA0 HAS
                SUBA        #$09            ; 9 PLACES TO LEFT OF EXPONENT) FROM LEFT DIGIT COUNTER
                JSR         >L90EA          ; PUT ACCA ZEROES IN STRING BUFFER
                JSR         >L9263          ; INITIALIZE DECIMAL POINT AND COMMA COUNTERS
                JSR         >L9202          ; CONVERT FPA0 TO DECIMAL ASCII IN THE STRING BUFFER
                LDA         V47             ; GET BASE 10 EXPONENT AND PUT THAT MANY
                JSR         >L9281          ; ZEROES IN STRING BUFFER - STOP AT DECIMAL POINT
                LDA         V47             ; WASTED INSTRUCTION - SERVES NO PURPOSE
                JSR         >L9249          ; CHECK FOR DECIMAL POINT
                LDA         VD8             ; GET THE RIGHT DIGIT COUNTER
                BNE         L9050           ; BRANCH IF RIGHT DIGlT COUNTER <> 0
                LEAU        -1,U            ; MOVE BUFFER POINTER BACK ONE - DELETE
; DECIMAL POINT IF NO RIGHT DIGITS SPECiFIED
L9050           DECA                        ;  SUBTRACT ONE (DECIMAL POINT)
                JSR         >L90EA          ; PUT ACCA ZEROES INTO BUFFER (TRAILING ZEROES)
L9054           JSR         >L9185          ; INSERT ASTERISK PADDING, FLOATING $, AND POST-SIGN
                TSTA                        ;  WAS THERE A POST-SIGN?
                BEQ         L9060           ; NO
                CMPB        #'*             ; IS THE FIRST CHARACTER AN $?
                BEQ         L9060           ; YES
                STB         ,U+             ; STORE THE POST-SIGN
L9060           CLR         ,U              ; CLEAR THE LAST CHARACTER IN THE BUFFER

; REMOVE ANY EXTRA BLANKS OR ASTERISKS FROM THE
; STRING BUFFER TO THE LEFT OF THE DECIMAL POINT
                LDX         #STRBUF+3       ; POINT X TO THE START OF THE BUFFER
L9065           LEAX        $01,X           ; MOVE BUFFER POINTER UP ONE
                STX         TEMPTR          ; SAVE BUFFER POINTER IN TEMPTR
                LDA         VARPTR+1        ; GET ADDRESS OF DECIMAL POINT IN BUFFER, SUBTRACT
                SUBA        TEMPTR+1        ; CURRENT POSITION AND SUBTRACT LEFT DIGIT COUNTER -
                SUBA        VD9             ; THE RESULT WILL BE ZERO WHEN TEMPTR+1 IS POINTING
; TO THE FIRST DIGIT OF THE FORMAT STRING
                BEQ         L90A9           ; RETURN IF NO DIGITS TO LEFT OF THE DECiMAL POINT
                LDA         ,X              ; GET THE CURRENT BUFFER CHARACTER
                CMPA        #SPACE          ; SPACE?
                BEQ         L9065           ; YES - ADVANCE POINTER
                CMPA        #'*             ; ASTERISK?
                BEQ         L9065           ; YES - ADVANCE POINTER
                CLRA                        ;  A ZERO ON THE STACK IS END OF DATA POINTER
L907C           PSHS        A               ; PUSH A CHARACTER ONTO THE STACK
                LDA         ,X+             ; GET NEXT CHARACTER FROM BUFFER
                CMPA        #'-             ; MINUS SIGN?
                BEQ         L907C           ; YES
                CMPA        #'+             ; PLUS SIGN?
                BEQ         L907C           ; YES
                CMPA        #'$             ; DOLLAR SIGN?
                BEQ         L907C           ; YES
                CMPA        #'0             ; ZERO?
                BNE         L909E           ; NO - ERROR
                LDA         $01,X           ; GET CHARACTER FOLLOWING ZERO
                BSR         L90AA           ; CLEAR CARRY IF NUMERIC
                BLO         L909E           ; BRANCH IF NOT A NUMERIC CHARACTER - ERROR
L9096           PULS        A               ; PULL A CHARACTER OFF OF THE STACK
                STA         ,-X             ; AND PUT IT BACK IN THE STRING BUFFER
                BNE         L9096           ; KEEP GOING UNTIL ZERO FLAG
                BRA         L9065           ; KEEP CLEANING UP THE INPUT BUFFER
L909E           PULS        A               ; REMOVE THE CHARACTERS ON
                TSTA                        ;  = THE STACK AND EXIT WHEN
                BNE         L909E           ; ZERO FLAG FOUND
                LDX         TEMPTR          ; GET THE STRING BUFFER START POINTER
                LDA         #'%             ; PUT A % SIGN BEFORE THE ERROR POSITION TO
                STA         ,-X             ; INDICATE AN ERROR
L90A9           RTS

; CLEAR CARRY IF NUMERIC
L90AA           CMPA        #'0             ; ASCII ZERO
                BLO         L90B2           ; RETURN IF ACCA < ASCII 0
                SUBA        #'9+1           ;
                SUBA        #-('9+1)        ; CARRY CLEAR IF NUMERIC
L90B2           RTS

; PROCESS AN ITEM-LIST WHICH IS < 100,000,000
L90B3           LDA         VD8             ; GET RIGHT DIGIT COUNTER
                BEQ         L90B8           ; BRANCH IF NO FORMATTED DIGITS TO THE RIGHT OF DECIMAL PT
                DECA                        ;  SUBTRACT ONE FOR DECIMAL POINT
L90B8           ADDA        V47             ; ADD THE BASE 10 EXPONENT OFFSET - ACCA CONTAINS THE
; NUMBER OF SHIFTS REQUIRED TO ADJUST FPA0 TO THE SPECIFIED
; NUMBER OF DlGITS TO THE RIGHT OF THE DECIMAL POINT
                BMI         L90BD           ; IF ACCA >= 0 THEN NO SHIFTS ARE REQUIRED
                CLRA                        ;  FORCE SHIFT COUNTER = 0
L90BD           PSHS        A               ; SAVE INITIAL SHIFT COUNTER ON THE STACK
L90BF           BPL         L90CB           ; EXIT ROUTINE IF POSITIVE
                PSHS        A               ; SAVE SHIFT COUNTER ON STACK
                JSR         >LBB82          ; DIVIDE FPA0 BY 10 - SHIFT ONE DIGIT TO RIGHT
                PULS        A               ; GET SHIFT COUNTER FROM THE STACK
                INCA                        ;  BUMP SHIFT COUNTER UP BY ONE
                BRA         L90BF           ; CHECK FOR FURTHER DIVISION
L90CB           LDA         V47             ; GET BASE 10 EXPONENT OFFSET, ADD INITIAL SHIFT COUNTER
                SUBA        ,S+             ; AND SAVE NEW BASE 10 EXPONENT OFFSET - BECAUSE
                STA         V47             ; FPA0 WAS SHIFTED ABOVE
                ADDA        #$09            ; =ADD NINE (SIGNIFICANT PLACES) AND BRANCH IF THERE ARE NO
                BMI         L90EE           ; =ZEROES TO THE LEFT OF THE DECIMAL POINT IN THIS PRINT ITEM
                LDA         VD9             ; DETERMINE HOW MANY FILLER ZEROES TO THE LEFT OF THE DECIMAL
                SUBA        #$09            ; POINT. GET THE NUMBER OF FORMAT PLACES TO LEFT OF DECIMAL
                SUBA        V47             ; POINT, SUBTRACT THE BASE 10 EXPONENT OFFSET AND THE CONSTANT 9
                BSR         L90EA           ; (UNNORMALIZATION)-THEN OUTPUT THAT MANY ZEROES TO THE BUFFER
                JSR         >L9263          ; INITIALIZE DECIMAL POINT AND COMMA COUNTERS
                BRA         L90FF           ; PROCESS THE REMAINDER OF THE PRINT ITEM

; PUT (ACCA+1) ASCII ZEROES IN BUFFER
L90E2           PSHS        A               ; SAVE ZERO COUNTER
                LDA         #'0             ; INSERT A ZERO INTO
                STA         ,U+             ; THE BUFFER
                PULS        A               ; RESTORE ZERO COUNTER
; PUT ACCA ASCII ZEROES INTO THE BUFFER
L90EA           DECA                        ;  DECREMENT ZERO COUNTER
                BPL         L90E2           ; BRANCH IF NOT DONE
                RTS
L90EE           LDA         VD9             ; GET THE LEFT DIGIT COUNTER AND PUT
                BSR         L90EA           ; THAT MANY ZEROES IN THE STRiNG BUFFER
                JSR         >L924D          ; PUT THE DECIMAL POINT IN THE STRING BUFFER
                LDA         #-9             ; DETERMINE HOW MANY FILLER ZEROES BETWEEN THE DECIMAL POINT
                SUBA        V47             ; AND SIGNIFICANT DATA. SUBTRACT BASE 10 EXPONENT FROM -9
                BSR         L90EA           ; (UNNORMALIZATION) AND OUTPUT THAT MANY ZEROES TO BUFFER
                CLR         V45             ; CLEAR THE DECIMAL POINT COUNTER - SUPPRESS THE DECIMAL POINT
                CLR         VD7             ; CLEAR THE COMMA COUNTER - SUPPRESS COMMAS
L90FF           JSR         >L9202          ; DECODE FPA0 INTO A DECIMAL ASCII STRING
                LDA         VD8             ; GET THE RIGHT DIGIT COUNTER
                BNE         L9108           ; BRANCH IF RIGHT DIGIT COUNTER <> 0
                LDU         VARPTR          ; RESET BUFFER PTR TO THE DECIMAL POINT IF NO DIGITS TO RIGHT
L9108           ADDA        V47             ; ADD BASE 10 EXPONENT - A POSITIVE ACCA WILL CAUSE THAT MANY
; FILLER ZEROES TO BE OUTPUT TO THE RIGHT OF LAST SIGNIFICANT DATA
; SIGNIFICANT DATA
                LBRA        L9050           ; INSERT LEADING ASTERISKS, FLOATING DOLLAR SIGN, ETC

; FORCE THE NUMERIC OUTPUT FORMAT TO BE EXPONENTIAL FORMAT
L910D           LDA         FP0EXP          ; GET EXPONENT OF FPA0 AND
                PSHS        A               ; SAVE IT ON THE STACK
                BEQ         L9116           ; BRANCH IF FPA0 = 0
                JSR         >L91CD          ; CONVERT FPA0 INTO A NUMBER WITH 9 SIGNIFICANT
; DIGITS TO THE LEFT OF THE DECIMAL POINT
L9116           LDA         VD8             ; GET THE RIGHT DIGIT COUNTER
                BEQ         L911B           ; BRANCH IF NO FORMATTED DIGITS TO THE RIGHT
                DECA                        ;  SUBTRACT ONE FOR THE DECIMAL POINT
L911B           ADDA        VD9             ; ADD TO THE LEFT DIGIT COUNTER
                CLR         STRBUF+3        ; CLEAR BUFFER BYTE AS TEMPORARY STORAGE LOCATION
                LDB         VDA             ; GET THE STATUS BYTE FOR A
                ANDB        #$04            ; POST-BYTE FORCE; BRANCH IF
                BNE         L9129           ; A POST-BYTE FORCE
                COM         STRBUF+3        ; TOGGLE BUFFER BYTE TO -1 IF NO POST-BYTE FORCE
L9129           ADDA        STRBUF+3        ; SUBTRACT 1 IF NO POST BYTE FORCE
                SUBA        #$09            ; SUBTRACT 9 (DUE TO THE CONVERSION TO 9
; SIGNIFICANT DIGITS TO LEFT OF DECIMAL POINT)
                PSHS        A               ; =SAVE SHIFT COUNTER ON THE STACK - ACCA CONTAINS THE NUMBER
; =OF SHIFTS REQUIRED TO ADJUST FPA0 FOR THE NUMBER OF
; =FORMATTED PLACES TO THE RIGHT OF THE DECIMAL POINT.
L9130           BPL         L913C           ; NO MORE SHIFTS WHEN ACCA >= 0
                PSHS        A               ; SAVE SHIFT COUNTER
                JSR         >LBB82          ; DIVIDE FPA0 BY 10 - SHIFT TO RIGHT ONE
                PULS        A               ; RESTORE THE SHIFT COUNTER
                INCA                        ;  ADD 1 TO SHIFT COUNTER
                BRA         L9130           ; CHECK FOR FURTHER SHIFTING (DIVISION)
L913C           LDA         ,S              ; GET THE INITIAL VALUE OF THE SHIFT COUNTER
                BMI         L9141           ; AND BRANCH IF SHIFTING HAS TAKEN PLACE
                CLRA                        ;  RESET ACCA IF NO SHIFTING HAS TAKEN PLACE
L9141           NEGA                        ;  CALCULATE THE POSITION OF THE DECIMAL POINT BY
                ADDA        VD9             ; NEGATING SHIFT COUNTER, ADDING THE LEFT DIGIT COUNTER
                INCA                        ;  PLUS ONE AND THE POST-BYTE POSlTION, IF USED
                ADDA        STRBUF+3        ;
                STA         V45             ; SAVE DECIMAL POINT COUNTER
                CLR         VD7             ; CLEAR COMMA COUNTER - NO COMMAS INSERTED
                JSR         >L9202          ; CONVERT FPA0 INTO ASCII DECIMAL STRING
                PULS        A               ; =GET THE INITIAL VALUE OF SHIFT COUNTER AND
                JSR         >L9281          ; =INSERT THAT MANY ZEROES INTO THE BUFFER
                LDA         VD8             ; GET THE RIGHT DIGIT COUNTER AND BRANCH
                BNE         L915A           ; IF NOT ZERO
                LEAU        -1,U            ; MOVE BUFFER POINTER BACK ONE
; CALCULATE VALUE OF EXPONENT AND PUT IN STRING BUFFER
L915A           LDB         ,S+             ; GET ORIGINAL EXPONENT OF FPA0
                BEQ         L9167           ; BRANCH IF EXPONENT = 0
                LDB         V47             ; GET BASE 10 EXPONENT
                ADDB        #$09            ; ADD 9 FOR 9 SIGNIFICANT DIGIT CONVERSION
                SUBB        VD9             ; SUBTRACT LEFT DIGIT COUNTER
                SUBB        STRBUF+3        ; ADD ONE TO EXPONENT IF POST-SIGN FORCE
L9167           LDA         #'+             ; PLUS SIGN
                TSTB                        ;  TEST EXPONENT
                BPL         L916F           ; BRANCH IF POSITIVE EXPONENT
                LDA         #'-             ; MINUS SIGN
                NEGB                        ;  CONVERT EXPONENT TO POSITIVE NUMBER
L916F           STA         $01,U           ; PUT SIGN OF EXPONENT IN STRING BUFFER
                LDA         #'E             ; PUT AN E (EXPONENTIATION FLAG) IN
                STA         ,U++            ; BUFFER AND SKIP OVER THE SIGN
                LDA         #'0-1           ; INITIALIZE TENS DIGIT TO ASCII ZERO MINUS ONE
; BINARY EXPONENT IN ACCB TO ASCII VALUE IN ACCA
L9177           INCA                        ;  ADD ONE TO TENS DIGIT COUNTER


; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
                SUBB        #12             ; SUBTRACT 12 FROM EXPONENT AND ADD ONE TO TENS
                BCC         L9177           ; DIGIT IF NO CARRY. TENS DIGIT DONE IF THERE IS A CARRY
                ADDB        #'9+3           ; ADD ASCII BIAS TO UNITS DIGIT
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; fix minor bug in the ascii to floating point conversion in PRINT USING

                SUBB        #10             ; SUBTRACT 10 FROM EXPONENT AND ADD ONE TO TENS
                BCC         L9177           ; DIGIT IF NO CARRY. TENS DIGIT DONE IF THERE IS A CARRY
                ADDB        #'9+1           ; ADD ASCII BIAS TO UNITS DIGIT
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



                STD         ,U++            ; SAVE EXPONENT IN BUFFER
                CLR         ,U              ; CLEAR FINAL BYTE IN BUFFER - PRINT TERMINATOR
                JMP         >L9054          ; INSERT ASTERISK PADDING, FLOATING DOLLAR SIGN, ETC.
; INSERT ASTERISK PADDING, FLOATING $ AND PRE-SIGN
L9185           LDX         #STRBUF+4       ; POINT X TO START OF PRINT ITEM BUFFER
                LDB         ,X              ; GET SIGN BYTE OF ITEM-LIST BUFFER
                PSHS        B               ; AND SAVE IT ON THE STACK
                LDA         #SPACE          ; DEFAULT PAD WITH BLANKS
                LDB         VDA             ; GET STATUS BYTE AND CHECK FOR
                BITB        #$20            ; ASTERISK LEFT PADDING
                PULS        B               ; GET SIGN BYTE AGAIN
                BEQ         L919E           ; BRANCH IF NO PADDING
                LDA         #'*             ; PAD WITH ASTERISK
                CMPB        #SPACE          ; WAS THE FIRST BYTE A BLANK (POSITIVE)?
                BNE         L919E           ; NO
                TFR         A,B             ; TRANSFER PAD CHARACTER TO ACCB
L919E           PSHS        B               ; SAVE FIRST CHARACTER ON STACK
L91A0           STA         ,X+             ; STORE PAD CHARACTER IN BUFFER
                LDB         ,X              ; GET NEXT CHARACTER IN BUFFER
                BEQ         L91B6           ; INSERT A ZERO IF END OF BUFFER
                CMPB        #'E             ; CHECK FOR AN E AND
                BEQ         L91B6           ; PUT A ZERO BEFORE IT
                CMPB        #'0             ; REPLACE LEADING ZEROES WITH
                BEQ         L91A0           ; PAD CHARACTERS
                CMPB        #',             ; REPLACE LEADING COMMAS
                BEQ         L91A0           ; WITH PAD CHARACTERS
                CMPB        #'.             ; CHECK FOR DECIMAL POINT
                BNE         L91BA           ; AND DONT PUT A ZERO BEFORE IT
L91B6           LDA         #'0             ; REPLACE PREVIOUS CHARACTER
                STA         ,-X             ; WITH A ZERO
L91BA           LDA         VDA             ; GET STATUS BYTE, CHECK
                BITA        #$10            ; FOR FLOATING $
                BEQ         L91C4           ; BRANCH IF NO FLOATING $
                LDB         #'$             ; STORE A $ IN
                STB         ,-X             ; BUFFER
L91C4           ANDA        #$04            ; CHECK PRE-SIGN FLAG
                PULS        B               ; GET SIGN CHARACTER
                BNE         L91CC           ; RETURN IF POST-SIGN REQUIRED
                STB         ,-X             ; STORE FIRST CHARACTER
L91CC           RTS

; CONVERT FPA0 INTO A NUMBER OF THE FORM - NNN,NNN,NNN X 10**M.
; THE EXPONENT M WILL BE RETURNED IN V47 (BASE 10 EXPONENT).
L91CD           PSHS        U               ; SAVE BUFFER POINTER
                CLRA                        ;  INITIAL EXPONENT OFFSET = 0
L91D0           STA         V47             ; SAVE EXPONENT OFFSET
                LDB         FP0EXP          ; GET EXPONENT OF FPA0
                CMPB        #$80            ; COMPARE TO EXPONENT OF .5
                BHI         L91E9           ; AND BRANCH IF FPA0 > = 1.0
; IF FPA0 < 1.0, MULTIPLY IT BY 1E+09 UNTIL IT IS >= 1
                LDX         #LBDC0          ; POINT X TO FP NUMBER (1E+09)
                JSR         >LBACA          ; MULTIPLY FPA0 BY 1E+09
                LDA         V47             ; GET EXPONENT OFFSET
                SUBA        #$09            ; SUBTRACT 9 (BECAUSE WE MULTIPLIED BY 1E+09 ABOVE)
                BRA         L91D0           ; CHECK TO SEE IF > 1.0
L91E4           JSR         >LBB82          ; DIVIDE FPA0 BY 10
                INC         V47             ; INCREMENT EXPONENT OFFSET
L91E9           LDX         #LBDBB          ; POINT X TO FP NUMBER (999,999,999)
                JSR         >LBCA0          ; COMPARE FPA0 TO X
                BGT         L91E4           ; BRANCH IF FPA0 > 999,999,999
L91F1           LDX         #LBDB6          ; POINT X TO FP NUMBER (99,999,999.9)
                JSR         >LBCA0          ; COMPARE FPA0 TO X
                BGT         L9200           ; RETURN IF 999,999,999 > FPA0 > 99,999,999.9
                JSR         >LBB6A          ; MULTIPLY FPA0 BY 10
                DEC         V47             ; DECREMENT EXPONENT OFFSET
                BRA         L91F1           ; KEEP UNNORMALIZING
L9200           PULS        U,PC            ; RESTORE BUFFER POINTER AND RETURN

; CONVERT FPA0 INTO AN INTEGER, THEN DECODE IT
; INTO A DECIMAL ASCII STRING IN THE BUFFER
L9202           PSHS        U               ; SAVE BUFFER POINTER
                JSR         >LB9B4          ; ADD .5 TO FPA0 (ROUND OFF)
                JSR         >LBCC8          ; CONVERT FPA0 TO INTEGER FORMAT
                PULS        U               ; RESTORE BUFFER POINTER

; CONVERT FPA0 INTO A DECIMAL ASCII STRING
                LDX         #LBEC5          ; POINT X TO UNNORMALIZED POWERS OF 10
                LDB         #$80            ; INITIALIZE DIGIT COUNTER TO 0 + $80.
; BIT 7 SET IS USED TO INDICATE THAT THE POWER OF 10 MANTISSA
; IS NEGATIVE. WHEN YOU ADD A NEGATIVE MANTISSA, IT IS
; THE SAME AS SUBTRACTING A POSITIVE ONE AND BIT 7 OF ACCB
; IS HOW THIS ROUTINE KNOWS THAT A SUBTRACTION IS OCCURRING.
L9211           BSR         L9249           ; CHECK FOR COMMA INSERTION
L9213           LDA         FPA0+3          ; ADD A POWER OF 10 MANTISSA TO FPA0.
                ADDA        $03,X           ; IF THE MANTISSA IS NEGATIVE, A SUBTRACTION
                STA         FPA0+3          ; WILL BE WHAT REALLY TAKES PLACE.
                LDA         FPA0+2          ;
                ADCA        $02,X           ;
                STA         FPA0+2          ;
                LDA         FPA0+1          ;
                ADCA        $01,X           ;
                STA         FPA0+1          ;
                LDA         FPA0            ;
                ADCA        ,X              ;
                STA         FPA0            ;
                INCB                        ;  ADD ONE TO DIGIT COUNTER
                RORB                        ;  ROTATE CARRY INTO BIT 7
                ROLB                        ;  SET OVERFLOW FLAG - BRANCH IF CARRY SET AND
                BVC         L9213           ; ADDING MANTISSA OR CARRY CLEAR AND SUBTRACTING MANTISSA
                BCC         L9235           ; BRANCH IF SUBTRACTING MANTISSA
                SUBB        #10+1           ; TAKE THE 9S COMPLEMENT
                NEGB                        ;  IF ADDING MANTISSA
L9235           ADDB        #'0-1           ; ADD IN ASCII OFFSET
                LEAX        $04,X           ; MOVE TO NEXT POWER OF 10 MANTISSA
                TFR         B,A             ; SAVE DIGIT IN ACCA
                ANDA        #$7F            ; MASK OFF ADD/SUBTRACT FLAG (BIT 7)
                STA         ,U+             ; STORE DIGIT IN BUFFER
                COMB                        ;  TOGGLE ADD/SUBTRACT FLAG
                ANDB        #$80            ; MASK OFF EVERYTHING BUT ADD/SUB FLAG
                CMPX        #LBEE9          ; COMPARE TO END OF UNNORMALIZED POWERS OF 10
                BNE         L9211           ; BRANCH IF NOT DONE
                CLR         ,U              ; PUT A ZERO AT END OF INTEGER
; DECREMENT DECIMAL POINT COUNTER AND CHECK FOR COMMA INSERTION
L9249           DEC         V45             ; DECREMENT DECIMAL POINT COUNTER
                BNE         L9256           ; NOT TIME FOR DECIMAL POINT
L924D           STU         VARPTR          ; SAVE BUFFER POINTER-POSITION OF THE DECIMAL POINT
                LDA         #'.             ; STORE A DECIMAL
                STA         ,U+             ; POINT IN THE OUTPUT BUFFER
                CLR         VD7             ; =CLEAR COMMA COUNTER - NOW IT WILL TAKE 255
; =DECREMENTS BEFORE ANOTHER COMMA WILL BE INSERTED
                RTS
L9256           DEC         VD7             ; DECREMENT COMMA COUNTER
                BNE         L9262           ; RETURN IF NOT TIME FOR COMMA
                LDA         #$03            ; RESET COMMA COUNTER TO 3; THREE
                STA         VD7             ; DIGITS BETWEEN COMMAS
                LDA         #',             ; PUT A COMMA INTO
                STA         ,U+             ; THE BUFFER
L9262           RTS
; INITIALIZE DECIMAL POINT AND COMMA COUNTERS
L9263           LDA         V47             ; GET THE BASE 10 EXPONENT OFFSET
                ADDA        #10             ; ADD 10 (FPA0 WAS NORMALIZED TO 9 PLACES LEFT
                STA         V45             ; OF DECIMAL POINT) - SAVE IN DECIMAL POINT COUNTER
                INCA                        ;  ADD ONE FOR THE DECIMAL POINT
L926A           SUBA        #$03            ; DIVIDE DECIMAL POINT COUNTER BY 3; LEAVE
                BCC         L926A           ; THE REMAINDER IN ACCA
                ADDA        #$05            ; CONVERT REMAINDER INTO A NUMBER FROM 1-3
                STA         VD7             ; SAVE COMMA COUNTER
                LDA         VDA             ; GET STATUS BYTE
                ANDA        #$40            ; CHECK FOR COMMA FLAG
                BNE         L927A           ; BRANCH IF COMMA FLAG ACTIVE
                STA         VD7             ; CLEAR COMMA COUNTER - 255 DIGITS OUTPUT BEFORE A COMMA
L927A           RTS

; INSERT ACCA ZEROES INTO THE BUFFER
L927B           PSHS        A               ; SAVE ZEROES COUNTER
                BSR         L9249           ; CHECK FOR DECIMAL POINT
                PULS        A               ; RESTORE ZEROES COUNTER
L9281           DECA                        ;  DECREMENT ZEROES COUNTER AND
                BMI         L928E           ; RETURN IF < 0
                PSHS        A               ; SAVE ZEROES COUNTER
                LDA         #'0             ; PUT A ZERO INTO
                STA         ,U+             ; THE BUFFER
                LDA         ,S+             ; RESTORE THE ZEROES COUNTER
                BNE         L927B           ; BRANCH IF NOT DONE
L928E           RTS
; GRAPHICS PACKAGE ********
; GET THE ADDRESS OF THE ROUTINE WHICH
; WILL CONVERT HOR & VER COORDINATES INTO
; AN ABSOLUTE RAM ADDRESS AND PIXEL MASK
; DEPENDING UPON THE CURRENT PMODE AND
; RETURN THE ADDRESS IN U.

L928F           LDU         #L929C          ; JUMP TABLE ADDRESS TO U
                LDA         PMODE           ; GET PMODE VALUE
                ASLA                        ;  MUL ACCA X2 - 2 BYTES PER ADDRESS
                LDU         A,U             ; GET JUMP ADDRESS
                RTS

; CONVERT VER COORD (VERBEG) & NOR COORD (HORBEG) INTO
; ABSOLUTE SCREEN ADDR IN X AND PIXEL MASK IN ACCA.
L9298           BSR         L928F           ; GO GET JUMP ADDRESS
                JMP         ,U              ; GO TO IT

; JUMP TABLE -- HOR, VER COORD CONVERSION
L929C           FDB         L92A6           ; PMODE 0
L929E           FDB         L92C2           ; PMODE 1
L92A0           FDB         L92A6           ; PMODE 2
L92A2           FDB         L92C2           ; PMODE 3
L92A4           FDB         L92A6           ; PMODE 4

; HOR, VER COORD CONVERSION ROUTINE FOR 2
; COLOR HIRES GRAPHICS MODES
L92A6           PSHS        U,B             ; SAVE REGISTERS
                LDB         HORBYT          ; GET NUMBER BYTES/HOR GRAPHIC ROW
                LDA         VERBEG+1        ; GET VERTICAL COORDINATE
                MUL                         ;  CALCULATE VERTICAL BYTE OFFSET
                ADDD        BEGGRP          ; ADD IN START OF GRAPHIC PAGE
                TFR         D,X             ; SAVE TEMP VALUE IN X REG
                LDB         HORBEG+1        ; GET HORIZONTAL COORDINATE
                LSRB                        ;  THREE LSRBS EQUALS DIVIDE BY 8 -
                LSRB                        ;  IN THE TWO COLOR MODE THERE ARE
                LSRB                        ;  8 PIXELS/BYTE
                ABX                         ;  ADD HOR BYTE OFFSET
                LDA         HORBEG+1        ; GET HORIZONTAL COORDINATE
                ANDA        #$07            ; KEEP ONLY BITS 0-2, WHICH CONTAIN THE NUMBER
; OF THE PIXEL IN THE BYTE
                LDU         #L92DD          ; POINT U TO MASK LOOKUP TABLE
                LDA         A,U             ; GET PIXEL MASK - THE MASK WILL HAVE ONE BIT SET WHICH
; CORRESPONDS TO THE PIXEL SELECTED.
                PULS        B,U,PC          ; RESTORE REGISTERS

; HOR, VER COORDINATE CONVERSION ROUTINE
; FOR 4 COLOR HI RES GRAPHICS MODES
L92C2           PSHS        U,B             ; SAVE REGISTERS
                LDB         HORBYT          ; GET NUMBER BYTES/HOR GRAPHIC ROW
                LDA         VERBEG+1        ; GET VERTICAL COORDINATE
                MUL                         ;  CALCULATE VERTICAL OFFSET
                ADDD        BEGGRP          ; ADD THE START OF GRAPHIC PAGE
                TFR         D,X             ; SAVE IN X REGISTER
                LDB         HORBEG+1        ; GET HORIZONTAL COORDINATE
                LSRB                        ;  TWO LSRBS = DIVIDE BY 4 IN THE 4
                LSRB                        ;  COLOR MODE THERE ARE 4 PIXELS/BYTE
                ABX                         ;  ADD HORIZONTAL BYTE OFFSET
                LDA         HORBEG+1        ; GET HORIZONTAL COORDINATE
                ANDA        #$03            ; KEEP ONLY BITS 0,1 WHICH CONTAIN THE NUMBER OF THE PIXEL TO CHANGE (4 COLOR)
                LDU         #L92E5          ; POINT U TO MASK LOOKUP TABLE
                LDA         A,U             ; GET THE MASK FOR THE PROPER PIXEL
                PULS        B,U,PC          ; RESTORE REGISTERS AND RETURN
; 2 COLOR MODE PIXEL MASKS
L92DD           FCB         $80,$40,$20,$10,$08,$04
                FCB         $02,$01
; 4 COLOR MODE PIXEL MASKS
L92E5           FCB         $C0,$30,$0C,$03
; MOVE X REG DOWN ONE GRAPHIC ROW
L92E9           LDB         HORBYT          ; GET NUMBER BYTES/HOR ROW
                ABX                         ;  ADD TO ABSOLUTE SCREEN POSITION
                RTS

; ENTER W/ABSOLUTE SCREEN POSITION IN X, THE PIXEL
; MASK IN ACCA - ADJUST X AND ACCA TO THE NEXT
; PIXEL TO THE RIGHT IN THE TWO COLOR MODE.
L92ED           LSRA                        ;  SHIFT ONE BIT TO RIGHT
                BCC         L92F3           ; BRANCH IF IN SAME BYTE
                RORA                        ;  IF YOU HAVE MOVED TO NEXT BYTE, SET BIT 7 IN ACCA
                LEAX        $01,X           ; AND ADD ONE TO X.
L92F3           RTS


; MOVE ABSOLUTE SCREEN ADDRESS OF CURRENT
; HOR, VER COORD ONE TO RIGHT AND ADJUST
; THE PIXEL MASK FOR THE 4 COLOR MODE
L92F4           LSRA                        ;  SHIFT MASK ONE BIT TO RIGHT
                BCC         L92ED           ; SHIFT RIGHT AGAIN IF SAME BYTE
                LDA         #$C0            ; SET PIXEL #3 IF NEW BYTE
                LEAX        $01,X           ; ADD ONE TO ABS SCREEN POSITION
                RTS

; EVALUATE TWO EXPRESSIONS - PUT THE FIRST
; VALUE (HOR COORD) IN HORBEG AND THE
; SECOND (VER COORD) IN VERBEG.
L92FC           JSR         >LB734          ; EVALUATE TWO EXPRESSIONS FROM THE BASIC LINE -
; RETURN WITH THE 1ST VALUE IN BINVAL AND THE 2ND IN ACCB
                LDY         #HORBEG         ; POINT Y TO TEMP STORAGE LOC
L9303           CMPB        #192            ; IS VERT COORD > 191?
                BLO         L9309           ; NO
                LDB         #191            ; FORCE VER COORD TO 191
L9309           CLRA                        ;  HIGH ORDER BYTE OF VER COORD
                STD         $02,Y           ; SAVE VERTICAL COORDINATE
                LDD         BINVAL          ; GET RAW HORIZONTAL COORDINATE
                CMPD        #256            ; IS IT WITHIN RANGE?
                BLO         L9317           ; YES
                LDD         #255            ; FORCE IT TO 255 IF NOT IN RANGE
L9317           STD         ,Y              ; SAVE HORIZONTAL COORDINATE
                RTS
; NORMALIZE HORIZONTAL AND VERTICAL COORDINATES FOR THE PROPER PHODE
; RETURN NORMALIZED VALUES IN (HORBEG,VERBEG)
L931A           JSR         >L92FC          ; GO GET HOR & VER COORDINATES
L931D           LDU         #HORBEG         ; POINT U TO HOR & VER COORDS
L9320           LDA         PMODE           ; GET PHODE
                CMPA        #$02            ; CHECK MODE
                BCC         L932C           ; BRANCH IF > 1
                LDD         $02,U           ; GET THE VERT COORD
                LSRA                        ;  DIVIDE ACCD BY TWO SINCE
                RORB                        ;  PMODES 0&1 HAVE ONLY 96 VERT BLOCKS
                STD         $02,U           ; SAVE NEW VERT COORD
L932C           LDA         PMODE           ; GET PMODE
                CMPA        #$04            ; CHECK PMODE
                BCC         L9338           ; BRANCH IF PMODE = 4
                LDD         ,U              ; GET HOR COORD
                LSRA                        ;  DIVIDE HORIZONTAL COORDINATE
                RORB                        ;  BY 2-PMODES 0,1,2,3 HAVE ONLY 128 HOR BLOCKS
                STD         ,U              ; SAVE NEW HOR COORD
L9338           RTS
; PPOINT
PPOINT          JSR         >L93B2          ; EVAL TWO EXPRESSIONS, RETURN VALUES IN (HORBEG,VERBEG)
                JSR         >L931D          ; NORMALIZE EXPRESSIONS FOR PROPER PMODE
                JSR         >L9298          ; CONVERT COORDS INTO ABS SCREEN POSITION & PIXEL MASK
                ANDA        ,X              ; AND PIXEL MASK WITH CONTENTS OF SCREEN
                LDB         PMODE           ; GET CURRENT PMODE
                RORB                        ;  SHIFT RIGHT
                BCC         L935B           ; BRANCH IF PMODE 0,2,4 (2 COLOR)
L9349           CMPA        #$04            ; IS THE ON PIXEL IN THE 2 RIGHTMOST BITS?
                BLO         L9351           ; BRANCH IF IT IS
                RORA                        ;  =SHIFT RIGHT
                RORA                        ;  =ONE PIXEL
                BRA         L9349           ; KEEP CHECKING
L9351           INCA                        ;  ADD 1 TO COLOR - BASIC USES 1 TO 4, NOT 0 TO 3
                ASLA                        ;  TIMES 2
                ADDA        CSSVAL          ; ADD COLOR SET (0 OR 8)
                LSRA                        ;  DIVIDE ACCB BY TWO - COLORS RANGE FROM 0 - 8
L9356           TFR         A,B             ; TRANSFER COLOR INFO INTO ACCB
                JMP         >LB4F3          ; CONVERT ACCB TO FP NUMBER
L935B           TSTA                        ;  IS 2 COLOR PIXEL ON?
                BEQ         L9356           ; NO
                CLRA                        ;  FORCE THE TWO-COLOR ON VALUE TO BE
                BRA         L9351           ; ONE OR FIVE (DEPENDING ON CSS).
; PSET
PSET            LDA         #$01            ; PSET FLAG
                BRA         L9366
; PRESET
PRESET          CLRA                        ;  PRESET FLAG
L9366           STA         SETFLG          ; STORE FLAG 0 = PRESET, 1 = PSET
                JSR         >LB26A          ; SYNTAX CHECK FOR (
                JSR         >L931A          ; EVAL HOR & VER COORDS AND NORMALIZE
                JSR         >L9581          ; EVALUATE COLOR - RETURN IN WCOLOR; ALLCOL
; WILL BE ONE BYTE WITH ALL PIXELS SET TO THAT COLOR
                JSR         >LB267          ; SYNTAX CHECK FOR )
                JSR         >L9298          ; CALCULATE THE ABSOLUTE ADDRESS OF THE
; BYTE TO PSET/PRESET - RETURN ADDRESS IN X - THE MASK
; OF PIXEL TO CHANGE RETURNED IN ACCA SET A PIXEL ON
; SCREEN - ABS POSIT IN X, MASK IN ACCA, COLOR IN ALLCOL
; TURN ON THE PIXEL (POINTED TO BY X, PIXEL MASK IN ACCA) TO THE COLOR
; IN ALLCOL. SET CHGFLG <> 0 IF THE PIXEL COLOR IS CHANGED.
L9377           LDB         ,X              ; GET BYTE FROM THE SCREEN
                PSHS        B               ; SAVE IT ON STACK
                TFR         A,B             ; PUT PIXEL MASK IN ACCB
                COMA                        ;  INVERT PIXEL MASK
                ANDA        ,X              ; AND WITH SCREEN DATA - KEEP ALL PIXELS
; EXCEPT THE ONE TO MODIFY
                ANDB        ALLCOL          ; CONVERT PIXEL IN THE PIXEL MASK TO THE PROPER COLOR
                PSHS        B               ; SAVE IT ON STACK
                ORA         ,S+             ; OR IT INTO THE REST OF THE PIXELS
                STA         ,X              ; PUT IT ON SCREEN
                SUBA        ,S+             ; SUBTRACT OLD BYTE FROM NEW BYTE; ACCA=0 IF NEW BYTE = OLD BYTE
                ORA         CHGFLG          ; OR DIFFERENCE WITH CHANGE FLAG
                STA         CHGFLG          ; SAVE IT - CHGFLG WILL BE = 0 IF THE GRAPHIC BYTE IS UNCHANGED
                RTS
; EVALUATE TWO SETS OF COORDINATES SEPARATED BY A MINUS
; SIGN. PUT 1ST SET OF COORDS AT (HORBEG,VERBEG), SECOND
; SET AT (HOREND,VEREND). IF NOTHING BEFORE MINUS SIGN, PUT
; (HORDEF,VERDEF) AT (HORBEG, VERBEG)
L938F           LDX         HORDEF          ; GET LAST HORIZ END POINT
                STX         HORBEG          ; PUT IN START POINT STORAGE LOC
                LDX         VERDEF          ; GET LAST VERT END POINT
                STX         VERBEG          ; PUT IN START POINT VERT STORAGE LOC
                CMPA        #$AC            ; TOKEN FOR MINUS SIGN
                BEQ         L939E           ; BRANCH IF NO STARTING COORDINATES GIVEN
                JSR         >L93B2          ; GO GET STARTING COORDINATES
L939E           LDB         #$AC            ; TOKEN FOR MINUS SIGN
                JSR         >LB26F          ; GO DO A SYNTAX CHECK
                JSR         >LB26A          ; SYNTAX CHECK FOR A (
                JSR         >LB734          ; EVALUATE 2 EXPRESSIONS
                LDY         #HOREND         ; TEMP STORAGE LOCATION FOR END COORDINATES OF LINE COMMAND
                JSR         >L9303          ; GET END POINT COORDS
                BRA         L93B8           ; CHECK SYNTAX FOR )
L93B2           JSR         >LB26A          ; SYNTAX CHECK FOR (
                JSR         >L92FC          ; EVALUATE 2 EXPRESSIONS
L93B8           JMP         >LB267          ; SYNTAX CHECK FOR ) AND RETURN LINE
LINE            CMPA        #$89            ; INPUT TOKEN
                LBEQ        L89C0           ; GO DO LINE INPUT COMMAND
                CMPA        #'(             ; CHECK FOR (
                BEQ         L93CE           ; GO LOOK FOR START AND END POINTS
                CMPA        #$AC            ; CHECK TOKEN FOR MINUS SIGN
                BEQ         L93CE           ; GO GET START AND END POINTS
                LDB         #'@             ; CHECK FOR @ SIGN
                JSR         >LB26F          ; DO A SYNTAX CHECK
L93CE           JSR         >L938F          ; GET STARTING AND ENDING COORDINATES
                LDX         HOREND          ; GET ENDING HORIZ COORDINATE
                STX         HORDEF          ; PUT IN LAST USED HOR END POINT
                LDX         VEREND          ; GET ENDING VER COORD
                STX         VERDEF          ; PUT IN LAST USED VER END POINT
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                CMPA        #$BE            ; PRESET TOKEN?
                BEQ         L93E9           ; YES
                CMPA        #$BD            ; PSET TOKEN?
                LBNE        LB277           ; SYNTAX ERROR IF NOT PSET OR PRESET
                LDB         #$01            ; PSET FLAG
L93E8           FCB         SKP1LD          ; SKIP ONE BYTE, LOAD ACCA WITH $5F
L93E9           CLRB                        ;  PRESET FLAG
                PSHS        B               ; SAVE PSET/PRESET FLAG
                JSR         GETNCH          ; GET ANOTHER CHAR
                JSR         >L9420          ; NORMALIZE START/END COORDS
                PULS        B               ; GET PSET/PRESET FLAG
                STB         SETFLG          ; SAVE IT
                JSR         >L959A          ; SET ACTIVE COLOR BYTE
                JSR         GETCCH          ; GET ANOTHER CHARACTER
                LBEQ        L94A1           ; BRANCH IF NO BOX TO BE DRAWN
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                LDB         #'B             ; BOX?
                JSR         >LB26F          ; GO DO A SYNTAX CHECK FOR A B
                BNE         L9429           ; FOUND A B AND SOMETHING FOLLOWS
                BSR         L9444           ; DRAW A HORIZ LINE
                BSR         L946E           ; DRAW A VERTICAL LINE
                LDX         HORBEG          ; GET HOR START COORD
                PSHS        X               ; SAVE IT ON STACK
                LDX         HOREND          ; GET HORIZONTAL END COORDINATE AND
                STX         HORBEG          ; PUT THEM IN HORIZONTAL START COORDINATE
                BSR         L946E           ; DRAW VERTICAL LINE
                PULS        X               ; GET THE PREVIOUS HORIZONTAL START COORDINATE
                STX         HORBEG          ; RESTORE IT
                LDX         VEREND          ; GET VER END COORD
                STX         VERBEG          ; PUT INTO START
                BRA         L9444           ; DRAW HORIZ LINE
; NORMALIZE START COORDS IN (HORBEG,VERBEG) & END COORDS IN (HOREND,VEREND)
L9420           JSR         >L931D          ; NORMALIZE COORDS IN (HORBEG,VERBEG)
                LDU         #HOREND
                JMP         >L9320          ; NORMALIZE COORDS IN (HOREND,VEREND)
L9429           LDB         #'F             ;
                JSR         >LB26F          ; GO DO A SYNTAX CHECK FOR AN F
                BRA         L9434           ; FILL THE BOX
L9430           LEAX        -1,X            ; MOVE VER COORD UP ONE
L9432           STX         VERBEG          ; SAVE NEW VERTICAL START COORDINATE
; DRAW A SERIES OF HORIZONTAL LINES FROM VER START TO VER END
L9434           JSR         >L9444          ; DRAW A HORIZ LINE
                LDX         VERBEG          ; GET START VER COORD
                CMPX        VEREND          ; COMPARE TO END VER COORD
                BEQ         L9443           ; RETURN IF EQUAL
                BCC         L9430           ; BRANCH IF START HOR > END HOR
                LEAX        $01,X           ; MOVE HOR COORD DOWN ONE
                BRA         L9432           ; KEEP DRAWING LINES
L9443           RTS         WASTED          ; BYTE - SHOULD USE L946B INSTEAD
; DRAW A HORIZONTAL LINE FROM HOREND TO HORBEG
; AT VER COORD VERBEG; COLOR IN ALLCOL
L9444           LDX         HORBEG          ; GET STARTING COORDS
                PSHS        X               ; SAVE EM
                JSR         >L971D          ; GET ABSOLUTE VALUE OF HOREND - HORBEG (HORIZONTAL COORDINATE)
                BCC         L9451           ; BRANCH IF END > START
                LDX         HOREND          ; TRANSFER END COORD TO START
                STX         HORBEG          ;
L9451           TFR         D,Y             ; SAVE DIFFERENCE IN Y
                LEAY        $01,Y           ; ADD ONE TO DIFFERENCE - TURN ON STARTING & ENDING COORDS
                JSR         >L9298          ; GET ABS SCREEN POS TO X AND PIXEL MASK TO ACCA
                PULS        U               ; GET START COORDS
                STU         HORBEG          ; RESTORE THEM
                BSR         L9494           ; POINT U TO ROUTINE TO MOVE PIXEL POINTERS TO RIGHT
L945E           STA         VD7             ; SAVE PIXEL MASK
                JSR         >L9377          ; TURN ON PIXEL
                LDA         VD7             ; GET OLD PIXEL MASK
                JSR         ,U              ; MOVE TO NEXT ONE TO RIGHT
                LEAY        -1,Y            ; DEC COUNTER
                BNE         L945E           ; LOOP IF NOT DONE
L946B           RTS
L946C           PULS        A,B             ; CLEAN UP STACK
; DRAW A VERTICAL LINE FROM VEREND TO VERBEG AT HOR COORD HORBEG
L946E           LDD         VERBEG          ; GET END COORDS
                PSHS        B,A             ; SAVE THEM
                JSR         >L9710          ; CALCULATE ABSOLUTE VALUE OF VEREND-VERBEG
                BCC         L947B           ; BRANCH IF END COORD > START COORD
                LDX         VEREND          ;
                STX         VERBEG          ; SWITCH VER COORDS IF END COORD IS TO RIGHT OF START
L947B           TFR         D,Y             ; LENGTH OF LINE TO Y
                LEAY        $01,Y           ; SET BOTH START AND END COORDS
                JSR         >L9298          ; GET ABSOLUTE SCREEN POS TO X, MASK TO ACCA
                PULS        U               ; GET END COORD
                STU         VERBEG          ; RESTORE THEM
                BSR         L949D           ; POINT U TO ROUTINE TO MOVE DOWN ONE ROW
                BRA         L945E           ; DRAW VERT LINE

; JUMP TABLE OF ADDRESSES OF ROUTINES WHICH WILL MOVE THE
; ABSOLUTE SCREEN ADDRESS POINTER ONE PIXEL TO THE RIGHT.
L948A           FDB         L92ED           ; PMODE 0
L948C           FDB         L92F4           ; PMODE 1
L948E           FDB         L92ED           ; PMODE 2
L9490           FDB         L92F4           ; PMODE 3
L9492           FDB         L92ED           ; PMODE 4
; POINT U TO ROUTINE WHICH WILL MOVE PIXEL ONE TO RIGHT
L9494           LDU         #L948A          ; POINT TO JUMP TABLE
                LDB         PMODE           ; GET PMODE VALUE
                ASLB                        ;  X2
                LDU         B,U             ; GET JUMP ADDRESS
                RTS
L949D           LDU         #L92E9          ; POINT U TO ROUTINE TO MOVE ABS POS DOWN ONE ROW
                RTS
; DRAW LINE FROM (HORBEG.VERBEG) TO (HOREND,VEREND)
L94A1           LDY         #L950D          ; POINT Y TO INCR VERBEG
                JSR         >L9710          ; CALCULATE VEREND - VERBEG (VERTICAL DIFFERENCE)
                LBEQ        L9444           ; DRAW A HORIZONTAL LINE IF DELTA V = 0
                BCC         L94B2           ; BRANCH IF VER END COORD > VER START COORD
                LDY         #L951B          ; POINT Y TO DECR VER COORD (VERBEG)
L94B2           PSHS        B,A             ; SAVE DELTA V
                LDU         #L9506          ; POINT U TO INCR HOR COORD
                JSR         >L971D          ; CALCULATE HOREND-HORBEG (HOR DIFFERENCE)
                BEQ         L946C           ; DRAW A VERTICAL LINE IF DELTA H = 0
                BCC         L94C1           ; BRANCH IF HOR END COORD > HOR START COORD
                LDU         #L9514          ; POINT U TO DECR HOR COORD
L94C1           CMPD        ,S              ; COMPARE DELTA H TO DELTA V
                PULS        X               ; PUT DELTA V IN X
                BCC         L94CC           ; BRANCH IF DELTA H > DELTA V
                EXG         U,Y             ; SWAP CHANGE HOR & CHANGE VER ADDRESS
                EXG         D,X             ; EXCHANGE DELTA HOR & DELTA VER
L94CC           PSHS        U,B,A           ; SAVE THE LARGER OF DELTA V, DELTA H
; AND THE INCREMENT/DECREMENT ADDRESS
                PSHS        B,A             ; SAVE WHICHEVER IS LARGER OF DELTA V, DELTA H
                LSRA                        ;
                RORB                        ;  DIVIDE BY 2, SHIFT ACCD RIGHT ONE BIT
                BLO         L94DD           ; BRANCH IF ODD NUMBER
                CMPU        #L950D+1        ; SEE IF INCR OR DECR
                BLO         L94DD           ; BRANCH IF INCR
                SUBD        #1              ; SUBTRACT 1 IF DECREMENT
L94DD           PSHS        X,B,A           ; SAVE SMALLEST DELTA (X) AND INITIAL MINOR COORDINATE
; COUNTER WHICH IS 1/2 OF LARGEST DELTA
                JSR         >L928F          ; POINT U TO PROPER COORDINATE TO SCREEN CONVERSION ROUTINE
; DRAW THE LINE HERE - AT THIS POINT THE STACK HAS THE DRAW DATA ON IT
; 0 1,S=MINOR COORDINATE INCREMENT COUNTER
; 2 3,S=ASSOLUTE VALUE OF THE SMALLEST DELTA COORDINATE
; 4 5,S=ABSOLUTE VALUE OF THE LARGEST DELTA COORDINATE
; 6 7,S=LARGEST COORDINATE COUNTER (HOW MANY TIMES THROUGH THE DRAW
; LOOP. INITIALLY SET TO ABSOLUTE VALUE OF LARGEST DELTA COORD
; 8 9,S=ADDRESS OF THE ROUTINE WHICH WILL INCREMENT OR DECREMENT
; THE LARGEST DELTA COORDINATE
L94E2           JSR         ,U              ; CONVERT (X,Y) COORDINATES TO ABSOLUTE SCREEN ADDRESS
                JSR         >L9377          ; TURN ON A PIXEL
                LDX         $06,S           ; GET DISTANCE COUNTER
                BEQ         L9502           ; BRANCH IF LINE IS COMPLETELY DRAWN
                LEAX        -1,X            ; DECR ONE
                STX         $06,S           ; SAVE IT
                JSR         [$08,S]         ; INCR/DECR COORDINATE-WHICH HAS THE LARGEST DELTA
                LDD         ,S              ; GET THE MIHOR COORDINATE INCREMENT COUNTER
                ADDD        $02,S           ; ADD THE SMALLEST DIFFERENCE
                STD         ,S              ; SAVE NEW MINOR COORDINATE INCREMENT COUNTER
                SUBD        $04,S           ; SUBTR OUT THE LARGEST DIFFERENCE AND
                BLO         L94E2           ; BRANCH IF RESULT NOT > LARGEST DIFFERENCE
                STD         ,S              ; IF >=, THEN STORE NEW MIHOR COORDINATE INCREMENT
                JSR         ,Y              ; INCREMENT/DECREMENT COORDINATE WHICH HAS THE SMALLEST DELTA
                BRA         L94E2           ; KEEP GOING
L9502           PULS        X               ;
                PULS        A,B,X,Y,U,PC    ; CLEAN UP THE STACK AND RETURN

; THESE ROUTINES ARE USED TO INCREMENT OR DECREMENT THE
; HORIZONTAL & VERTICAL COORDINATES. THEY NEED TO BE KEPT
; IN THIS ORDER (INCR,INCR,DECR,DECR).
; INCR HORBEG (HOR COORD)
L9506           LDX         HORBEG          ; GET COORDINATE
                LEAX        $01,X           ; ADD ONE
                STX         HORBEG          ; SAVE COORDINATE
                RTS
; INCR VERBEG (VER COORD)
L950D           LDX         VERBEG          ; GET COORDINATE
                LEAX        $01,X           ; ADD ONE
                STX         VERBEG          ; SAVE COORDINATE
                RTS
; DECR HORSES (HOR COORD)
L9514           LDX         HORBEG          ; GET COORDINATE
                LEAX        -1,X            ; SUBTRACT ONE
                STX         HORBEG          ; SAVE COORDINATE
                RTS
; DECR VERBEG (VER COORD)
L951B           LDX         VERBEG          ; GET COORDINATE
                LEAX        -1,X            ; SUBTRACT ONE
                STX         VERBEG          ; SAVE COORDINATE
                RTS

; GET MAXIMUM VALUE OF HOR/VER COORDINATES
; NORMALIZED FOR PROPER PMODE. RETURN VALUES
; HOR = VD3 VER = VD5
L9522           LDU         #VD3            ; POINT U TO TEMP STORAGE AREA (VD3)
                LDX         #255            ; MAXIMUM VALUE HORIZONTAL COORD (255)
                STX         ,U              ; SAVE IT
                LDX         #191            ; MAXIMUM VALUE VERTICAL COORD (191)
                STX         $02,U           ; SAVE IT
                JMP         >L9320          ; GO CONVERT THEM TO PROPER PMODE
; PCLS
PCLS            BEQ         L9542           ; CLEAR TO BACKGROUND COLOR IF NO ARGUMENT
                BSR         L955A           ; EVALUATE EXPRESSION, CONVERT TO PROPER COLOR CODE
L9536           LDA         #$55            ; CONSIDER EACH BYTE AS 4 GROUPS OF 2 BIT SUB-NIBBLES
                MUL                         ;  MULT BY COLOR
                LDX         BEGGRP          ; GET STARTING ADOR
L953B           STB         ,X+             ; SET BYTE TO PROPER COLOR
                CMPX        ENDGRP          ; AT END OF GRAPHIC PAGE?
                BNE         L953B           ; NO
                RTS
L9542           LDB         BAKCOL          ; GET BACKGROUND COLOR
                BRA         L9536
; COLOR
COLOR           CMPA        #',             ; CHECK FOR COMMA AND
                BEQ         L9552           ; BRANCH IF FOREGROUND COLOR ARGUMENT MISSING
                BSR         L955A           ; EVALUATE FIRST ARGUMENT
                STB         FORCOL          ; STORE IN FOREGROUND LOCATION
                JSR         GETCCH          ; GET NEXT INPUT CHARACTER
                BEQ         L9559           ; RETURN IF NONE
L9552           JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMMA
                BSR         L955A           ; EVALUATE LAST ARGUMENT
                STB         BAKCOL          ; STORE IN BACKGROUND COLOR
L9559           RTS
; EVALUATE AN EXPRESSION AND CONVERT IT TO A PROPER COLOR CODE
; DEPENDING ON THE PMODE AND CSS; ILLEGAL FUNCTION CALL IF > 8 -
; RETURN COLOR VALUE IN ACCB; CSS VALUE IN ACCA
L955A           JSR         EVALEXPB        ; EVALUATE EXPRESSION
L955D           CMPB        #$09            ; ONLY ALLOW 0-8
                LBCC        LB44A           ; ILLEGAL FUNCTION CALL IF BAD COLOR
                CLRA                        ;  VDG CSS VALUE FOR FIRST COLOR SET
                CMPB        #$05            ; FIRST OR SECOND COLOR SET?
                BLO         L956C           ; BRANCH IF FIRST SET
                LDA         #$08            ; VDG CSS VALUE FOR SECOND COLOR SET
                SUBB        #$04            ; MAKE 5-8 BECOME 1-4
L956C           PSHS        A               ; SAVE VDG CSS VALUE ON THE STACK
                LDA         PMODE           ; GET PMODE
                RORA                        ;  4 COLOR OR 2 COLOR
                BCC         L957B           ; 2 COLOR
                TSTB                        ;  WAS COLOR = 0
                BNE         L9578           ; NO
L9576           LDB         #$04            ; IF SO, MAKE IT 4
L9578           DECB                        ;  CONVERT 1-4 TO 0-3
L9579           PULS        A,PC            ; PUT VDG CSS VALUE IN ACCA AND RETURN
L957B           RORB                        ;  CHECK ONLY THE LSB OF COLOR IF IN 2 COLOR MODE
                BLO         L9576           ; BRANCH IF ODD - FORCE ACCB TO 3
                CLRB                        ;  FORCE ACCB = 0 IF EVEN
                BRA         L9579           ; RETURN
; SET THE CURRENT ACTIVE COLOR AND ALL PIXEL BYTE
; TO FOREGROUND/BACKGROUND COLOR DEPENDING ON
; PSET, PRESET IF NO EXPRESSION , ) OR
; ,. OTHERWISE EVALUATE THE EXPRESSION
L9581           JSR         >L959A          ; GET THE COLOR OF A BYTE
                JSR         GETCCH          ; CHECK CURRENT INPUT CHARACTER
                BEQ         L9598           ; BRANCH IF NONE
                CMPA        #')             ; CHECK FOR ) AND BRANCH IF
                BEQ         L9598           ; NO MORE ARGUMENTS
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                CMPA        #',             ; WAS NEXT CHARACTER A COMMA?
                BEQ         L9598           ; YES
                JSR         >L955A          ; EVALUATE EXPRESSION, RETURN COLOR IN ACCB
                BSR         L95A2           ; TEMP STORE COLOR AND ALL PIXEL BYTE
L9598           JMP         GETCCH          ; CHECK INPUT CHARACTER AND RETURN
; SET THE ACTIVE COLOR BYTE AND THE ALL ACTIVE COLOR BYTE
L959A           LDB         FORCOL          ; GET FOREGROUND COLOR
                TST         SETFLG          ; CHECK PSET/PRESET FLAG
                BNE         L95A2           ; BRANCH IF PSET
                LDB         BAKCOL          ; GET BACKGROUND COLOR
L95A2           STB         WCOLOR          ; TEMP STORE COLOR
                LDA         #$55            ; CONSIDER A BYTE AS 4 PIXELS
                MUL                         ;  SET COLOR ON ALL 4 PIXElS
                STB         ALLCOL          ; SAVE BYTE WITH ALL PIXELS TURNED ON
                RTS
L95AA           BNE         L95CF           ; BRANCH IF GRAPHIC MODE, OTHERWISE SET UP ALPHA GRAPHIC MODE
; THIS CODE WILL RESET THE DISPLAY PAGE REGISTER IN THE
; SAM CHIP TO 2 ($400) AND RESET THE SAMS VDG CONTROL
; REGISTER TO 0 (ALPHA-NUMERICS). IN ADDITION, IT WILL
; RESET THE VDG CONTROL PINS TO ALPHA-GRAPHICS MODE.
; SET UP THE SAM AND VDG TO GRAPHICS MODE
L95AC           PSHS        X,B,A           ; SAVE REGISTERS
                LDX         #SAMREG+8       ; POINT X TO THE MIDDLE OF THE SAM CNTL REG
                STA         10,X            ;
                STA         $08,X           ;
                STA         $06,X           ;
                STA         $04,X           ; RESET SAM DISPLAY PAGE TO $400
                STA         $02,X           ;
                STA         $01,X           ;
                STA         -2,X            ;
                STA         -4,X            ;
                STA         -6,X            ; RESET SAMS VDG TO ALPHA-NUMERIC MODE
                STA         -8,X            ;
                LDA         PIA1+2          ; GET DATA FROM PIA1, PORT B
                ANDA        #$07            ; FORCE ALL BITS TO ZERO, KEEP ONLY CSS DATA
                STA         PIA1+2          ; PUT THE VDG INTO ALPHA-GRAPHICS MODE
                PULS        A,B,X,PC        ; RETURN
L95CF           PSHS        X,B,A
                LDA         PMODE           ; GET CURRENT PMODE VALUE
                ADDA        #$03            ; ADD 3 - NOW 3-7 ONLY 5 OF 8 POSSIBLE MODES USED
                LDB         #$10            ; $10 OFFSET BETWEEN PMODES
                MUL                         ;  GET PMODE VALUES FOR VDG GM0, GM1, GM2
                ORB         #$80            ; FORCE BIT 7 HIGH (VDG A/G CONTROL)
                ORB         CSSVAL          ; OR IN THE VDG CSS DATA
                LDA         PIA1+2          ; GET PIA1, PORT B
                ANDA        #$07            ; MASK OFF THE VDG CONTROL DATA
                PSHS        A               ; SAVE IT
                ORB         ,S+             ; OR IT WITH THE VDG VALUES CALCULATED ABOVE
                STB         PIA1+2          ; STORE IT INTO THE PIA
                LDA         BEGGRP          ; GET MSB OF START OF GRAPHIC PAGE
                LSRA                        ;  DIVIDE BY 2 - ACCA CONTAINS HOW MANY 512 BYTE
; BLOCKS IN STARTING ADDR
                JSR         >L960F          ; GO SET SAM CONTROL REGISTER
                LDA         PMODE           ; GET PMODE VALUE
                ADDA        #$03            ; ADD IN BIAS TO ADJUST TO PMODE THE SAM REGISTER WANTS
                CMPA        #$07            ; WAS PMODE 4?
                BNE         L95F7           ; NO
                DECA                        ;  DECREMENT ACCA IF PMODE 4 (SAME VDG AS PMODE3)
L95F7           BSR         L95FB           ; SET THE SAMS VDG REGISTER
                PULS        A,B,X,PC        ; RESTORE REGISTERS AND RETURN
L95FB           LDB         #$03            ; 3 BITS IN SAM VDG CONTROL REGISTER
; ENTER WITH DATA TO GO IN VDG REGISTER IN BOTTOM 3 BITS OF ACCA
                LDX         #SAMREG         ; POINT X TO SAM CONTROL REGISTER
L9600           RORA                        ;  PUT A BIT INTO CARRY FLAG
                BCC         L9607           ; BRANCH IF BIT WAS A ZERO
                STA         $01,X           ; SET SAM REGISTER BIT
                BRA         L9609           ; DO NEXT BIT
L9607           STA         ,X              ; CLEAR SAM REGISTER
L9609           LEAX        $02,X           ; NEXT BIT IN REGISTER
                DECB                        ;  DONE ALL BITS?
                BNE         L9600           ; NO
                RTS
L960F           LDB         #$07            ; 7 BITS IN SAM DISPLAY PAGE REGISTER
                LDX         #SAMREG+6       ; POINT X TO SAM DISPLAY PAGE REGISTER
                BRA         L9600           ; GO SET THE REGISTER
L9616           LDA         PIA1+2          ; GET PIA1, PORT B
                ANDA        #$F7            ; MASK OFF VDG CSS CONTROL BIT
                ORA         CSSVAL          ; OR IN CSS COLOR DATA
                STA         PIA1+2          ; RESTORE IT IN PIA1
                RTS
; PMODE
PMODETOK        CMPA        #',             ; CHECK FOR COMMA - FIRST ARGUMENT MAY BE MISSING
                BEQ         L9650           ; IT IS A COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION
                CMPB        #$05            ; > 4?
                BCC         L966D           ; YES, ILLEGAL FUNCTION CALL


; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
; supposed to be start of graphic ram. patched later to not be hardcoded value
;
; NOTE "extended basic unravelled" suggested this should be "lda #6", but
; changing it to direct mode addressing instead of immediate makes it match
; the mess rom dump sha1. i don't have coco hardware here to confirm this though.

                LDA         6               ; hardcoded start of graphic ram $600
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
                LDA         GRPRAM          ; GET THE START OF GRAPHIC RAM
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



L962E           STA         BEGGRP          ; SET START GRAPHIC PAGE
                ASLB                        ;  MULT MODE BY 2 - TABLE HAS 2 BYTES PER ENTRY
                LDU         #L9706+1        ; LOOKUP TABLE
                ADDA        B,U             ; ADD THE AMOUNT OF MEMORY REQUIRED FOR ONE GRAPHIC PAGE
                CMPA        TXTTAB          ; COMPARE TO MSB OF START OF BASIC PROGRAM
                BHI         L966D           ; FC ERROR IF END OF GRAPHIC PAGE > START OF BASIC PROGRAM
                STA         ENDGRP          ; STORE THE END OF GRAPHIC PAGE
                LEAU        -1,U            ; POINT U TO PREVIOUS BYTE IN TABLE
                LDA         B,U             ; GET THE NUMBER OF BYTES/HORIZONTAL LINE
                STA         HORBYT          ; AND SAVE IT IN HORBYT
                LSRB                        ;  RESTORE PMODE VALUE
                STB         PMODE           ; SAVE IT
                CLRA                        ;  BACKGROUND COLOR
                STA         BAKCOL          ; SET BACKGROUND COLOR TO ZERO
                LDA         #$03            ; FOREGROUND COLOR
                STA         FORCOL          ; SET FOREGROUND COLOR
                JSR         GETCCH          ; IS THERE A STARTING PAGE NUMBER?
                BEQ         L966C           ; NO
L9650           JSR         >LB738          ; EVALUATE EXPRESSION
                TSTB                        ;  SET FLAGS
                BEQ         L966D           ; ILLEGAL FUNCTION CALL - CANT START ON PAGE ZERO
                DECB                        ;  BUMP ONE BASIC STARTS ON PAGE 1, THIS ROUTINE AT 0
                LDA         #$06            ; EACH GRAPHIC PAGE = 6 X 256 (1.5K)
                MUL                         ;  MULT BY PAGE NUMBER
                ADDB        GRPRAM          ; ADD IN START OF GRAPHIC RAM
                PSHS        B               ; SAVE TEMP START ADDR
                ADDB        ENDGRP          ; ADD CURRENT END ADDR
                SUBB        BEGGRP          ; SUB OUT CURRENT START ADDR - (ADDS THE SIZE OF ONE GRAPHIC PAGE)
                CMPB        TXTTAB          ; IS IT > CURRENT START OF BASIC PROGRAM
                BHI         L966D           ; YES! ILLEGAL FUNCTION CALL
                STB         ENDGRP          ; SAVE AS END OF GRAPHIC PAGE
                PULS        B               ; GET TEMP START ADOR
                STB         BEGGRP          ; SAVE AS START OF GRAPHIC PAGE
L966C           RTS
L966D           JMP         >LB44A          ; ILLEGAL FUNCTION CALL'
; SCREEN
SCREEN          CMPA        #',             ; CHECK FOR A COMMA
                BEQ         L967F           ; BRANCH IF COMMA - FIRST ARGUMENT MISSING
                JSR         EVALEXPB        ; EVALUATE EXPRESSION
                TSTB                        ;  ZERO FLAG SET IF ALPHA, NOT SET IF GRAPHIC SCREEN
                JSR         >L95AA          ; SET UP THE SAM & VDG FOR PROPER GRAPHIC MODE
                JSR         GETCCH          ; GET NEXT CHARACTER
                BEQ         L966C           ; RETURN IF NOTHING ELSE ON LINE
L967F           JSR         >LB738          ; CHECK FOR COMMA AND EVALUATE EXPRESSION
                TSTB                        ;  SET FLAGS
                BEQ         L9687           ; BRANCH IF COLOR SET ZERO
                LDB         #$08            ; VALUE FOR COLOR SET ONE
L9687           STB         CSSVAL          ; SAVE IN VDG CSS RAM IMAGE
                BRA         L9616           ; GO SET IT INTO PIA
; PCLEAR
PCLEAR          JSR         EVALEXPB        ; EVALUATE EXPRESSION, RETURN VALUE IN ACCB
                TSTB                        ;  SET FLAGS
                BEQ         L966D           ; BRANCH IF PCLEAR0 - FC ERROR
                CMPB        #$09            ; TRYING TO CLEAR MORE THAN 8 PAGES?
                BCC         L966D           ; YES ILLEGAL FUNCTION CALL
                LDA         #$06            ; 6 X 256 (1.5K) PER GRAPHIC PAGE
                MUL                         ;  MULT BY NUMBER OF PAGES
                ADDB        GRPRAM          ; ADD IN START OF GRAPHIC RAM
                TFR         B,A             ; MOVE B TO MSB OF REG ACCD
                LDB         #$01            ; REG D NOW CONTAINS TOP OF PCLEARED SPACE +1
                TFR         D,Y             ; SAVE IN Y
                CMPD        ENDGRP          ; COMPARE TOP OF PCLEARED SPACE TO END OF CURRENT GRAPHIC PAGE



; -----------------------------------------------------------------------------
                if          VEREXTBAS<11
; -----------------------------------------------------------------------------
; THIS CODE REFLECTS THE INFAMOUS PCLEAR BUG
                LBLO        LB44A           ; IF TRYING TO CLEAR LESS THAN END OF CURRENT PAGE = 'ILLEGAL FUNCTION CALL'
                SUBD        TXTTAB          ; SUBTRACT START OF RAM
                ADDD        VARTAB          ; ADD END OF BASIC PROGRAM
                TFR         D,X             ; X=TOP OF PCLEARED SPACE + SIZE OF BASIC PROGRAM
                ADDD        #200            ; ADD 200 - LEAVE SOME ROOM FOR STACK
                SUBD        FRETOP          ; SUBTRACT OUT TOP OF CLEARED SPACE
                BCC         L966D           ; NO ROOM LEFT - 'ILLEGAL FUNCTION CALL'
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
; THIS CODE REFLECTS THE INFAMOUS PCLEAR BUG
                BLO         L966D           ; FC ERROR IF TRYING TO CLEAR < END OF GRAPHIC RAM
                SUBD        TXTTAB          ; SUBTRACT START OF BASIC PROGRAM
                ADDD        VARTAB          ; ADD END OF BASIC PROGRAM
                TFR         D,X             ; X=TOP OF PCLEARED SPACE + LENGTH OF BASIC PROGRAM
                INCA                        ;  ADD 256 - LEAVE SOME ROOM FOR THE STACK
                SUBD        FRETOP          ; SUBTRACT OUT TOP OF CLEARED SPACE
                BCC         L966D           ; FC ERROR - NO ROOM LEFT
                JSR         >L80D0          ; ADJUST BASICS INPUT POINTER
                NOP                         ;  SPACE FILLER FOR EXBAS 1.1
; -----------------------------------------------------------------------------
                endif
; -----------------------------------------------------------------------------



                LDU         VARTAB          ; GET END OF BASIC PROGRAM
                STX         VARTAB          ; STORE NEW END OF BASIC PROGRAM
                CMPU        VARTAB          ; COMPARE OLD END TO NEW END
                BCC         L96D4           ; BRANCH IF OLD END > NEW END
L96BD           LDA         ,-U             ; GET BYTE FROM OLD PROGRAM
                STA         ,-X             ; MOVE TO NEW PROGRAM LOCATION
                CMPU        TXTTAB          ; AT THE BEGINNING OF OLD PROGRAM?
                BNE         L96BD           ; NO
                STY         TXTTAB          ; SAVE NEW STARTING ADDRESS
                CLR         -1,Y            ; CLEAR BYTE JUST BEFORE PROGRAM
L96CB           JSR         >LACEF          ; PUT CORRECT ADDRESSES IN FIRST 2 BYTES OF EACH LINE
                JSR         >LAD26          ; DO PART OF A NEW
                JMP         >LAD9E          ; GO BACK TO BASICS MAIN LOOP
L96D4           LDU         TXTTAB          ; GET START OF BASIC PROGRAM
                STY         TXTTAB          ; STORE NEW STARTING ADDR
                CLR         -1,Y            ; CLEAR THE BYTE JUST BEFORE PROGRAM
L96DB           LDA         ,U+             ; GET BYTE FROM OLD PROGRAM
                STA         ,Y+             ; MOVE TO NEW PROG LOCATION
                CMPY        VARTAB          ; AT END OF OLD PROGRAM?
                BNE         L96DB           ; NO
                BRA         L96CB           ; GO RESET SOME POINTERS
; INITIALIZATION ROUTINE FOR EXBAS GRAPHICS VARIABLES
L96E6           LDB         #$1E            ;
                STB         TXTTAB          ; SET START OF BASIC PROG TO 1E00
                LDA         #$06
L96EC           STA         GRPRAM          ; =CONSTANT OFFSET OF $600
                STA         BEGGRP          ; START OF GRAPHICS PAGE TO $600
                CLRA                        ;  PMODE 0
                STA         PMODE           ; SET PMODE TO 0
                LDA         #$10            ; 16 BYTES/HOR GRAPHIC ROW
                STA         HORBYT          ; SAVE IT
                LDA         #$03            ; SET FOREGROUND COLOR TO 3
                STA         FORCOL          ; SET FOREGROUND COLOR TO 3
                LDA         #$0C            ;
                STA         ENDGRP          ; SET END OF GRAPHICS PAGE TO $C00
                LDX         TXTTAB          ; GET START OF PROGRAM
                CLR         -1,X            ; CLEAR ONE BYTE JUST BEFORE PROGRAM
L9703           JMP         >LAD19          ; GO DO A NEW
; TABLE OF HOW MANY BYTES/GRAPHIC ROW AND HOW MUCH RAM
; FOR ONE HI RES SCREEN FOR THE PMODES. ROWS FIRST,
; BYTES (IN 256 BYTE BLOCKS) SECOND.
L9706           FCB         $10,$06         ; PMODE 0
L9708           FCB         $20,$0C         ; PMODE 1
L970A           FCB         $10,$0C         ; PMODE 2
L970C           FCB         $20,$18         ; PMODE 3
L970E           FCB         $20,$18         ; PMODE 4
; CALC ABS(VEREND - VERBEG)
L9710           LDD         VEREND          ; GET VERTICAL ENDING ADDRESS
                SUBD        VERBEG          ; SUBTRACT OUT VERTICAL BEGINNING ADDRESS
L9714           BCC         L9751           ; RETURN IF END >= START
                PSHS        CC              ; SAVE STATUS (WHICH COORDINATE IS GREATER)
                JSR         >L9DC3          ; NEGATE ACCD IF START COORD > END COORD
                PULS        CC,PC           ; RESTORE STATUS AND RETURN
; CALC ABS(HOREND - HORBEG)
L971D           LDD         HOREND          ; GET HORIZONTAL END COORD
                SUBD        HORBEG          ; SUB OUT HORIZONTAL START COORD
                BRA         L9714           ; GET ABSOLUTE VALUE
; PCOPY
PCOPY           BSR         L973F           ; EVALUATE SOURCE PAGE NUMBER AND RETURN MSB OF
; ADDRESS OF START OF PAGE IN ACCD
                PSHS        B,A             ; SAVE PAGE 1 OFFSET
                LDB         #$A5            ; TOKEN FOR TO
                JSR         >LB26F          ; SYNTAX CHECK FOR TO
                BSR         L973F           ; EVALUATE PAGE NUMBER
                PULS        X               ; SET ADDRESS OF SOURCE PAGE TO X
                TFR         D,U             ; ADDRESS OF DESTINATION PAGE TO U
                LDY         #$300           ; MOVE $300 PAIRS OF BYTES (ONE GRAPHIC PAGE)
L9736           LDD         ,X++            ; GET TWO BYTES FROM SOURCE
                STD         ,U++            ; PUT INTO DESTINATION PAGE
                LEAY        -1,Y            ; DECREMENT COUNTER
                BNE         L9736           ; NOT DONE YET
                RTS
L973F           JSR         EVALEXPB        ; EVALUATE EXPRESSION
                TSTB                        ;  PAGE ZERO?
                BEQ         L9752           ; YES - ILLEGAL FUNCTION CALL
; THIS IS A FLAKEY ERROR CHECK - IT WILL LET YOU PCOPY OVER
; THE TOP OF THE BASIC PROGRAM IN SOME INSTANCES.
                CMPB        TXTTAB          ; IS PAGE NUMBER > MSB OF START ADDR OF BASIC PROG?
                BHI         L9752           ; FC ERROR IF SO - BAD ERROR CHECK
                DECB                        ;  BUMP PAGE NUMBER DOWN 1, BASIC STARTS AT 1, THIS
; ROUTINE STARTS AT ZERO
                LDA         #$06            ; 6*256 (1.5K) PER GRAPHIC PAGE
                MUL                         ;  GET OFFSET OF THIS PAGE NUMBER
                ADDB        GRPRAM          ; GET START OF GRAPHIC RAM- ACCB NOW CONTAINS
; MSB OF ADDRESS OF THIS PAGE
                EXG         A,B             ; NON ACCD HAS ADDRESS OF PAGE START
L9751           RTS
L9752           JMP         >LB44A          ; ILLEGAL FUNCTION CALL
; GET
GET             CLRB                        ;  GET FLAG
                BRA         L975A           ; THIS SHOULD BE FCB SKP2 - IT WOULD SAVE A BYTE
; PUT
PUT             LDB         #$01            ; PUT FLAG
L975A           STB         VD8             ; SAVE GET/PUT FLAG
                JSR         RVEC22          ; HOOK INTO RAM
L975F           CMPA        #'@             ; CHECK FOR @ SIGN
                BNE         L9765           ; NO @ SIGN
                JSR         GETNCH          ; GO GET NEXT INPUT CHARACTER
L9765           JSR         >L938F          ; GO EVALUATE START AND END POINTS - STORE START AT (HORBEG,VERSEG), END AT (HOREND,VEREND)
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >L98CC          ; GET POINTER TO ARRAY DESCRIPTOR
                TFR         X,D             ; SAVE DESCRIPTOR + 2 IN ACCD
                LDU         ,X              ; SAVE OFFSET TO NEXT ARRAY IN U
                LEAU        -2,U            ; POINT U TO START OF DESCRIPTOR
                LEAU        D,U             ; POINT U TO END OF ARRAY
                STU         VD1             ; SAVE END OF DATA (END OF ARRAY)
                LEAX        $02,X           ; POINT X TO NUMBER OF DIMENSIONS AND
                LDB         ,X              ; GET NUMBER DIMENSIONS IN ACCB
                ASLB                        ;  TIMES 2 - 2 BYTES/DIMENSION
                ABX                         ;  POINT X TO START OF ARRAY DATA
                STX         VCF             ; SAVE START OF DATA (START OF ARRAY DATA)
                LDA         VALTYP          ; CHECK VARIABLE TYPE
                BNE         L9752           ; FC ERROR IF STRING VARIABLE
                CLR         VD4             ; GET/PUT GRAPHIC/ACTION FLAG
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                BEQ         L97B7           ; BRANCH IF END OF LINE
                COM         VD4             ; TOGGLE GET/PUT GRAPHIC/ACTION FLAG
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                TST         VD8             ; CHECK GET/PUT FLAG
                BNE         L979A           ; BRANCH IF PUT
                LDB         #'G             ; CHECK FOR FULL GRAPHIC OPTION
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR A G
                BRA         L97CA           ; SKIP AROUND THE NO G OPTION CODE
L979A           LDB         #$05            ; FIVE LEGAL TOKENS AT END OF PUT
                LDX         #L9839          ; POINT X TO LOOK UP TABLE
L979F           LDU         ,X++            ; GET CLEAR BIT ACTION ROUTINE ADDRESS
                LDY         ,X++            ; GET SET BIT ACTION ROUTINE ADDRESS
                CMPA        ,X+             ; CHECK FOR ONE OF THE FIVE LEGAL TOKENS
                BEQ         L97AE           ; FOUND ONE
                DECB                        ;  CHECKED ALL FIVE?
                BNE         L979F           ; NO - KEEP GOING
                JMP         >LB277          ; SYNTAX ERROR IF TOKEN NOT FOUND
L97AE           STY         VD5             ; ARRAY SET BIT ACTION ROUTINE ADDRESS
                STU         VD9             ; ARRAY CLEAR BIT ACTION ROUTINE ADDRESS
                JSR         GETNCH          ; GET INPUT CHAR FROM BASIC
                BRA         L97CA           ; SKIP AROUND THE NO G OPTION CODE
; NO G OPTION OR ACTION SPECIFIED BY BASIC INPUT LINE
L97B7           LDB         #$F8            ; BOTTOM 3 BITS MASK (8 PIXELS/BYTE)
                LDA         PMODE           ; GET CURRENT PMODE
                RORA                        ;  BIT 0 TO CARRY
                BCC         L97C0           ; BRANCH IF PMODE 0,2,4 (2 COLOR)
                LDB         #$FC            ; BOTTOM 2 BITS MASK (4 COLOR MODE - 4 PIXELS/BYTE)
L97C0           TFR         B,A             ; SAVE ACCB IN ACCA
                ANDB        HORBEG+1        ;
                STB         HORBEG+1        ; MASK THE PIXEL COUNTER (BITS 0,1=
                ANDA        HOREND+1        ; 4 COLOR, BITS 0-2=2 COLOR) BITS OFF
                STA         HOREND+1        ; THE HORIZONTAL DIFFERENCE
L97CA           JSR         >L971D          ; CALC HORIZ DIFFERENCE ABS(HOREND-HORBEG)
                BCC         L97D3           ; BRANCH IF END > START
                LDX         HOREND          ; MAKE START = END IF
                STX         HORBEG          ; START > END
L97D3           STD         HOREND          ; SAVE HORIZ DIFFERENCE
                JSR         >L9710          ; CALC VERT DIFFERENCE ABS (VEREND-VERBEG)
                BCC         L97DE           ; BRANCH IF END > START
                LDX         VEREND          ; MAKE START = END IF
                STX         VERBEG          ; START > END
L97DE           STD         VEREND          ; SAVE VERT DIFFERENCE
                LDA         PMODE           ; GET PMODE BIT 0
                RORA                        ;  TO THE CARRY FLAG
                LDD         HOREND          ; GET HORIZ DIFFERENCE
                BCC         L97EB           ; BRANCH IF PMODE = 0,2,4(2 COLOR)
                ADDD        HOREND          ; DOUBLE HORIZ DIFF - 2X AS MANY BYTES FOR
                STD         HOREND          ; NUMBER OF PIXELS IN PMODES 1,3
L97EB           JSR         >L9420          ; NORMALIZE DIFFERENCES
                LDD         HOREND          ; GET HORIZ DIFFERENCE
                LDX         VEREND          ;
                LEAX        $01,X           ;
                STX         VEREND          ; ADD 1 TO VERT DIFFERENCE
                TST         VD4             ; CHECK FOR G OPTION OR GET ACTION
                BNE         L9852           ; AND BRANCH IF GIVEN
                LSRA                        ;
                RORB                        ;
                LSRA                        ;
                RORB                        ;
                LSRA                        ;
                RORB                        ;  DIVIDE HORIZONTAL DIFFERENCE BY 8
                ADDD        #1              ; ADD ONE TO QUOTIENT
                STD         HOREND          ; SAVE NEW HOR DIFFERENCE
                JSR         >L9298          ; CONVERT (HORBEG,VERSEG) INTO ABSOLUTE SCREEN
; (X) AND PIXEL MASK (ACCA)
L9808           LDB         HOREND+1        ; GET HORIZ DIFFERENCE
                PSHS        X               ; SAVE SCREEN POSITION
L980C           TST         VD8             ; CHECK THE GET/PUT FLAG
                BEQ         L9831           ; BRANCH IF GET
                BSR         L9823           ; INCREMENT ARRAY DATA POINTER
                LDA         ,U              ; GET DATA FROM ARRAY
                STA         ,X+             ; PUT IT ON THE SCREEN
L9816           DECB                        ;  DECREMENT HORIZ DIFFERENCE
                BNE         L980C           ; BRANCH IF NOT AT END OF HORIZ LINE
                PULS        X               ; GET SCREEN POSITION BACK
                JSR         >L92E9          ; MOVE ABS POSITION DOWN ONE ROW
                DEC         VEREND+1        ; DECREMENT VERTICAL DIFFERENCE
                BNE         L9808           ; BRANCH IF NOT DONE
L9822           RTS
L9823           LDU         VCF             ;
                LEAU        $01,U           ;
                STU         VCF             ; ADD ONE TO CURRENT ARRAY DATA POINTER
                CMPU        VD1             ; COMPARE TO END OF DATA
                BNE         L9822           ; RETURN IF NOT AT END
L982E           JMP         >LB44A          ; ILLEGAL FUNCTION CALL
L9831           LDA         ,X+             ; GET DATA FROM SCREEN
                BSR         L9823           ; INCREMENT ARRAY DATA POINTER
                STA         ,U              ; STORE IN ARRAY
                BRA         L9816           ; KEEP LOOPING TILL DONE

L9839           FDB         L9894,L989B
L983D           FCB         $BD             ; TOKEN FOR PSET
L983E           FDB         L989B,L9894
L9842           FCB         $BE             ; TOKEN FOR PRESET
L9843           FDB         L98B1,L989B
L9847           FCB         $B1             ; TOKEN FOR OR
L9848           FDB         L9894,L98B1
L984C           FCB         $B0             ; TOKEN FOR AND
L984D           FDB         L98A1,L98A1
L9851           FCB         $A8             ; TOKEN FOR NOT
; GET/PUT WITH 'G' OPTION SPECIFIED
L9852           ADDD        #1              ; ADD ONE TO HORIZ DIFFERENCE
                STD         HOREND          ; AND SAVE IT
                LDA         VD8             ; CHECK GET/PUT FLAG AND
                BNE         L9864           ; BRANCH IF PUT
                LDU         VD1             ; GET END OF ARRAYS
L985D           STA         ,-U             ; THIS CODE WILL
                CMPU        VCF             ; ZERO OUT THE ENTIRE
                BHI         L985D           ; 'GET' ARRAY
L9864           JSR         >L9298          ; =CONVERT (HORBEG,VERBEG) INTO ABSOLUTE SCREEN POSITION
; =(X) AND PIXEL MASK (ACCA)
                LDB         PMODE           ; GET CURRENT PMODE
                RORB                        ;  BIT 0 TO CARRY
                BCC         L986E           ; BRANCH IF PMODE 0,2,4 (2 COLOR)
                ANDA        #$AA            ; USE $AA AS THE PIXEL MASK IN 4 COLOR MODE
L986E           LDB         #$01            ; INITIALIZE SHIFT CTR
                LDY         VCF             ; POINT Y TO ARRAY DATA
L9873           PSHS        X,A             ; SAVE PIXEL MASK (ACCA) AND ABS SCRN POS (X) ON STACK
                LDU         HOREND          ; GET THE HORIZONTAL DIFFERENCE
L9877           PSHS        U,A             ; SAVE PIXEL MASK AND HORIZ DIFF
                LSRB                        ;  SHIFT BIT CTR RIGHT
                BCC         L9884           ; BRANCH IF ALL 8 SHIFTS NOT DONE
                RORB                        ;  SHIFT CARRY BACK INTO ACCB
                LEAY        $01,Y           ; INCREMENT ARRAY DATA POINTER
                CMPY        VD1             ; COMPARE TO END OF ARRAY
                BEQ         L982E           ; FC ERROR IF AT END
L9884           TST         VD8             ; CHECK THE GET/PUT FLAG AND
                BEQ         L98A7           ; BRANCH IF GET
                BITB        ,Y              ; TEST A BIT IN ARRAY DATA
                BEQ         L9890           ; BRANCH IF ZERO
                JMP         [VD5]           ; JUMP TO ACTION ROUTINE FOR ARRAY BIT SET
L9890           JMP         [VD9]           ; JUMP TO ACTION ROUTINE FOR ARRAY BIT CLEAR
L9894           COMA                        ;  MASK SOURCE DATA
                ANDA        ,X              ; OFF OF SCREEN DATA
                STA         ,X              ; SAVE TO SCREEN
                BRA         L98B1
L989B           ORA         ,X              ; OR SOURCE DATA WITH SCREEN
                STA         ,X              ; SAVE TO SCREEN
                BRA         L98B1
L98A1           EORA        ,X              ; INVERT THE PIXEL
                STA         ,X              ; SAVE TO SCREEN
                BRA         L98B1
L98A7           BITA        ,X              ; TEST THE PIXEL
                BEQ         L98B1           ; BRANCH IF IT IS OFF
                TFR         B,A             ; PUT SHIFT CTR IN ACCA
                ORA         ,Y              ; TURN ON PROPER BIT IN
                STA         ,Y              ; THE ARRAY DATA
L98B1           PULS        A,U             ; RESTORE PIXEL MASK AND HOR DIFF
                JSR         >L92ED          ; MOVE SCRN POS & PIXEL MASK ONE TO RIGHT (TWO COLOR MODE)
                LEAU        -1,U            ;
                CMPU        ZERO            ; DECR HORIZ DIFFERENCE AND
                BNE         L9877           ; BRANCH IF NOT ZERO
                LDX         $01,S           ; GET ABS SCRN POS FROM STACK
                LDA         HORBYT          ; GET NUMBER BYTES/GRAPHIC ROW
                LEAX        A,X             ; MOVE SCRN POS DOWN ONE ROW
                PULS        A               ; PULL PIXEL MASK OFF THE STACK
                LEAS        $02,S           ; GET X OFF THE STACK
                DEC         VEREND+1        ; DECR VERT ROW CTR
                BNE         L9873           ; BRANCH IF NOT DONE
                RTS         RETURN          ; FROM GET/PUT COMMAND
L98CC           JSR         >LB357          ; EVAL ALPHA EXPR, RETURN DESCRIPTOR PTR IN X
                LDB         ,-X             ; STRIP OFF THE VARIABLE
                LDA         ,-X             ; NAME (2 ALPHA-NUMERIC CHARACTERS) AND
                TFR         D,U             ; STORE THEM IN U
                LDX         ARYTAB          ; GET START OF ARRAYS
L98D7           CMPX        ARYEND          ; COMPARE TO END OF ARRAYS
                LBEQ        LB44A           ; FC ERROR IF UNDEFINED ARRAY
                CMPU        ,X              ; COMPARE TARGET NAME TO ARRAY NAME
                BEQ         L98E8           ; RETURN IF CORRECT ARRAY FOUND
                LDD         $02,X           ; GET OFFSET TO NEXT ARRAY AND
                LEAX        D,X             ; ADD TO POINTER
                BRA         L98D7           ; KEEP SEARCHING FOR MATCH
L98E8           LEAX        $02,X           ; MOVE POINTER TO OFFSET TO NEXT ARRAY
                RTS         WASTED          ; BYTE
L98EB           RTS
; PAINT
PAINT           CMPA        #'@             ; CHECK FOR @ SIGN
                BNE         L98F2           ; SKIP IF NOT
                JSR         GETNCH          ; READ A CHARACTER FROM BASIC INPUT LINE
L98F2           JSR         >L93B2          ; SYNTAX CHECK FOR (, TWO EXPRESSION, AND ).
; SAVE HOR COORD IN HORSES, VER COORD IN VERSES
                JSR         >L931D          ; NORMALIZE THE HOR, VER COORDINATES
                LDA         #$01            ; PSET VALUE
                STA         SETFLG          ; SET PSET/PRESET FLAG TO PSET
                JSR         >L9581          ; GET PAINT COLOR CODE & SET THE ACTIVE COLOR AND ALL PIXEL BYTES
                LDD         WCOLOR          ; GET THEM
                PSHS        B,A             ; SAVE THEM ON STACK
                JSR         GETCCH          ; GET CURRENT CHARACTER FROM INPUT LINE
                BEQ         L990A           ; BRANCH IF NONE LEFT - DEFAULT BORDER COLOR TO FOREGROUND, PAINT COLOR TO BACKGROUND
                JSR         >L9581          ; EVALUATE THE BORDER COLOR
L990A           LDA         ALLCOL          ; GET BORDER COLOR ALL PIXEL BYTE
                STA         VD8             ; TEMP SAVE IT
                PULS        A,B             ; GET PAINT ACTIVE COLORS BACK
                STD         WCOLOR          ; RESAVE IT
                CLRA                        ;  STORE A BLOCK OF PAINT DATA ON THE STACK WHICH
                PSHS        U,X,B,A         ; WILL ACT AS AN END OF PAINT DATA FLAG.
; THE CLRA WILL CAUSE THE UP/DN FLAG TO BE ZERO WHICH IS
; USED AS A FLAG TO EXIT THE PAINT ROUTINE
                JSR         >L9522          ; GET NORMALIZED MAX HOR/VER VALUES - RETURN RESULT IN VD3, VD5
                JSR         >L928F          ; POINT U TO THE ROUTINE WHICH WILL SELECT A PIXEL

; 'PAINT' THE FIRST HORIZONTAL LINE FROM THE START COORDINATES
                STU         VD9             ; SAVE IT
                JSR         >L99DF          ; PAINT FROM CURRENT HOR COORD TO ZERO
                BEQ         L9931           ; BRANCH IF NO PAINTING DONE - HIT BORDER INSTANTLY
                JSR         >L99CB          ; PAINT TOWARD MAX HOR COORD
                LDA         #$01            ;
                STA         VD7             ; UP/DN FLAG UP=1; DOWN=$FF
                JSR         >L99BA          ; SAVE POSITIVE GOING LINE INFO ON STACK
                NEG         VD7             ; SET UP/ON FLAG = FF
                JSR         >L99BA          ; SAVE NEGATIVE GOING LINE INFO ON STACK
L9931           STS         TMPSTK          ; TEMP STORE STACK POINTER
L9934           TST         CHGFLG          ; SEE IF PAINTED COLOR IS DIFFERENT THAN ORIGINAL COLOR
                BNE         L993B           ; BRANCH IF DATA HAS BEEN MODIFIED
                LDS         TMPSTK          ; GET STACK POINTER BACK
L993B           PULS        A,B,X,U         ; GET DATA FOR THE NEXT LINE SEGMENT TO CHECK FROM THE STACK
                CLR         CHGFLG          ; CLEAR CHANGE FLAG
                STS         TMPSTK          ; TEMP SAVE STACK
                LEAX        $01,X           ; ADD ONE TO START HOR COORD - 1
                STX         HORBEG          ; PUT IT AT CURRENT HOR COORD ADDR
                STU         VD1             ; LENGTH OF PARENT LINE
                STA         VD7             ; SAVE UP/DN FLAG
                BEQ         L98EB           ; EXIT ROUTINE IF UP/DN FLAG = 0
                BMI         L9954           ; BRANCH IF UP/DN FLAG = DOWN
; CHECK LINE BELOW CURRENT DATA
                INCB                        ;  INCREMENT VER COORD
                CMPB        VD6             ; COMPARE TO MAXIMUM VER COORD
                BLS         L9958           ; BRANCH IF NOT GREATER - PROCESS LINE
                CLRB                        ;  SET VER COORD TO ZERO TO FORCE WRAP AROUND
L9954           TSTB                        ;  TEST VER COORD
                BEQ         L9934           ; PROCESS ANOTHER BLOCK OF PAINT DATA IF
; WRAP AROUND - DISCARD ANY LINE BELOW
; VER COORD = 0 OR ABOVE MAX VERTICAL COORD
                DECB                        ;  DEC VER COORD - WE ARE TESTING UP/DN FLAG = UP IF THERE
; PROCESS A HOR LINE THAT WAS STORED ON STACK - LIMIT CHECKS HAVE BEEN DONE
L9958           STB         VERBEG+1        ; SAVE CURRENT VER COORD
                JSR         >L99DF          ; PAINT FROM HOR COORD TO ZERO OR BORDER
                BEQ         L996E           ; IF NUMBER OF PAINTED PIXELS = 0, COMPLEMENT LENGTH
                CMPD        #3              ; SEE IF < 3 PIXELS WERE PAINTED - IF FEWER THAN
; THREE PIXELS WERE PAINTED THEN THERE IS NO NEED TO
; CHECK FOR MORE DATA TO PAINT ON THE LINE TO THE
; LEFT OF THE CURRENT POSITION IN THE OPPOSITE
; DIRECTION THAT THE UP/DN FLAG IS CURRENTLY SET TO
                BLO         L9969           ; BRANCH IF NO NEED TO CHECK FOR PAINTABLE DATA
                LEAX        -2,X            ; MOVE THE HORIZONTAL COORDINATE TWO PIXELS TO THE LEFT
                BSR         L99A1           ; SAVE A BLOCK OF PAINT DATA IN THE DIRECTION
; OPPOSITE TO THE UP/DN FLAG
L9969           JSR         >L99CB          ; CONTINUE PAINTING LINE TO THE RIGHT
L996C           BSR         L99BA           ; SAVE A BLOCK OF PAINT DATA IN THE SAME
; DIRECTION AS THE UP/DN FLAG

; THIS CODE WILL INSURE THAT THE CURRENT LINE IS
; EXAMINED TO THE RIGHT FOR 'PAINTABLE' PIXELS FOR
; A LENGTH EQUAL TO THE LENGTH OF THE 'PARENT' LINE
L996E           COMA                        ;
                COMB                        ;  COMPLEMENT LENGTH OF LINE JUST PAINTED
L9970           ADDD        VD1             ; ADD TO LENGTH OF PARENT LINE
                STD         VD1             ; SAVE DIFFERENCE OF LINE JUST PAINTED AND PARENT LINE
                BLE         L998C           ; BRANCH IF PARENT LINE IS SHORTER
                JSR         >L9506          ; GO INC HOR COORD
                JSR         >L9A12          ; CHECK FOR BORDER COLOR
                BNE         L9983           ; NOT BORDER COLOR -
                LDD         #-1             ; GO DECREMENT ONE FROM LENGTH OF DIFFERENCE
                BRA         L9970           ; LINE AND KEEP LOOKING FOR NON BORDER COLOR
L9983           JSR         >L9514          ; GO DEC HOR COORD
                BSR         L99C6           ; GET AND SAVE HOR COORD
                BSR         L99E8           ; PAINT FORWARD TO MAX HOR COORD OR BORDER
                BRA         L996C           ; SAVE A BLOCK OF PAINT DATA AND KEEP CHECKING

; CHECK TO SEE IF THE CURRENT LINE EXTENDS FURTHER TO
; THE RIGHT THAN THE PARENT LINE AND PUT A BLOCK OF
; PAINT DATA ON THE STACK IF IT IS MORE THAN 2 PIXELS
; PAST THE END OF THE PARENT LINE
L998C           JSR         >L9506          ; INC CURRENT HOR COORD
                LEAX        D,X             ; POINT X TO THE RIGHT END OF THE PARENT LINE
                STX         HORBEG          ; AND SAVE IT AS THE CURRENT HORIZ COORDINATE
                COMA                        ;  = ACCD CONTAINS A NEGATIVE NUMBER CORRESPONDING TO
                COMB                        ;  = THE NUMBER OF PIXELS THE CURRENT LINE EXTENDS
                SUBD        #1              ; PAST THE RIGHT END OF THE PARENT LINE. CONVERT
                BLE         L999E           ; TO A POSITIVE NUMBER AND BRANCH IF THE LINE DOESNT EXTEND
                TFR         D,X             ; SAVE THE PORTION OF THE LINE TO THE RIGHT OF THE PARENT LINE
; AS THE LENGTH
                BSR         L99A1           ; =SAVE A BLOCK OF PAINT DATA IN THE DIRECTION OPPOSITE THE
; =CURRENT UP/DN FLAG
L999E           JMP         >L9934          ; PROCESS MORE PAINT DATA BLOCKS

; BLOCKS OF PAINT DATA ARE STORED ON THE STACK SO THAT PAINT
; CAN REMEMBER WHERE IT SHOULD GO BACK TO PAINT UP OR DOWN
; FROM THE CURRENT LINE IT IS PAINTING. THESE BLOCKS OF DATA
; REPRESENT HORIZONTAL LINES ABOVE OR BELOW THE CURRENT LINE
; BEING PAINTED AND REQUIRE SIX BYTES OF STORAGE ON THE STACK.
; THE DATA ARE STORED AS FOLLOWS: ,S=UP/DOWN FLAG; 1,S=VER COORD
; OF LINE; 2 3,S=LEFTMOST HOR COORD OF LINE; 4 5,S=LENGTH OF LINE
; SAVE A BLOCK OF PAINT DATA FOR A LINE IN THE
; OPPOSITE DIRECTION OF THE CURRENT UP/DN FLAG
L99A1           STD         VCB             ; SAVE NUMBER PIXELS PAINTED
                PULS        Y               ; PUT RETURN ADDR IN Y
                LDD         HORBEG          ; GET HORIZONTAL START COORDINATE
                PSHS        X,B,A           ; PUT ON STACK
                LDA         VD7             ; GET U/D FLAG
                NEGA                        ;  REVERSE THE UP/DN FLAG
L99AC           LDB         VERBEG+1        ; GET VERTICAL START COORDINATE
                PSHS        B,A             ; SAVE VERTICAL START COORDINATE AND U/D FLAG
                PSHS        Y               ; PUT RETURN ADDR BACK ON STACK
; CODE BELOW CHECKS FOR ABILITY TO STORE FOUR BYTES IN
; FREE RAM, HOWEVER THE PAINT ROUTINE WILL STORE SIX
; BYTES IN FREE RAM - FIRST INSTRUCTION SHOULD BE LDB #3
                LDB         #$02            ; CHECK TO SEE IF THERES ENOUGH FREE
                JSR         >LAC33          ; RAM FOR 4 BYTES TEMP STORAGE
                LDD         VCB             ; GET LENGTH OF RIGHT PAINTED LINE
                RTS

; SAVE A BLOCK OF PAINT DATA FOR A LINE IN
; THE SAME DIRECTION AS THE CURRENT UP/DN FLAG
L99BA           STD         VCB             ; SAVE LENGTH OF RIGHT HOR PAINTED LINE
                PULS        Y               ; SAVE RETURN ADDRESS IN Y
                LDD         HOREND          ; HORIZONTAL START COORDINATE
                PSHS        X,B,A           ; SAVE HORIZONTAL START COORDINATE AND LENGTH
                LDA         VD7             ; GET UP/DOWN FLAG (1 OR -1)
                BRA         L99AC           ; SAVE THE PAINT DATA ON THE STACK
L99C6           LDX         HORBEG          ; GET CURRENT HOR COORD
                STX         HOREND          ; SAVE IT
                RTS
; GO HERE TO FINISH PAINTING TO RIGHT AFTER YOU HAVE PAINTED TO THE LEFT
L99CB           STD         VCD             ; SAVE COUNT OF THE NUMBER OF PIXELS PAINTED
                LDY         HOREND          ; GET LAST HOR START COORD
                BSR         L99C6           ; SAVE CURRENT HOR COORD - NOW HOREND CONTAINS COORDINATE
; THE LEFT BORDER OF THIS HORIZONTAL LINE
                STY         HORBEG          ; START PAINTING TO RIGHT FROM THE LEFT PAINT START COORD
                BSR         L99E8           ; PAINT TOWARDS THE RIGHT
                LDX         VCD             ; GET THE NUMBER OF PIXELS PAINTED WHEN GOING TOWARDS LEFT PIXELS
                LEAX        D,X             ; ADD TO NUMBER PAINTED GOING TOWARD RIGHT
                ADDD        #1              ; ADD 1 TO PAINT COUNT TOWARD RIGHT - ACCD = LENGTH OF PAINTED LINE

                RTS
; PAINT FROM HOR COORD TO ZERO OR HIT BORDER
; RETURN WITH Z = 1 IF NO PAINTING DONE
L99DF           JSR         >L99C6          ; PUT STARTING HOR COORD IN HOREND
                LDY         #L9514          ; (DECR HOR COORD ADDRESS) TO Y
                BRA         L99EE           ; GO PAINT THE LINE
; PAINT FROM HOR COORD TO MAX HOR COORD OR HIT
; BORDER-RETURN Z=1 IF NO PAINTING DONE
L99E8           LDY         #L9506          ; PUT INCR HOR COORD ADDR IN Y
                JSR         ,Y              ; INCR HOR COORD - THE LEFT PAINT ROUTINE PAINTED THE FIRST COORD
L99EE           LDU         ZERO            ; ZERO U REG - INITIAL PIXEL PAINT COUNTER
                LDX         HORBEG          ; GET HOR COORD
L99F2           BMI         L9A0B           ; BRANCH IF HORIZONTAL COORDINATE IS > $7F OR < 0
                CMPX        VD3             ; COMPARE CURRENT HOR COORD TO MAX VALUE
                BHI         L9A0B           ; BRANCH IF > MAX
                PSHS        U,Y             ; SAVE PAINT COUNTER, INC/DEC POINTER
                BSR         L9A12           ; CHECK FOR BORDER PIXEL
                BEQ         L9A09           ; HIT BOROER
                JSR         >L9377          ; SET PIXEL TO PAINT COLOR - PAINTING IS DONE HERE
                PULS        Y,U             ; RESTORE PAINT COUNTER AND INC/DEC POINTER
                LEAU        $01,U           ; ADD ONE TO PAINT COUNTER
                JSR         ,Y              ; INCR OR DECR HOR COORD DEPENDING ON CONTENTS OF Y
                BRA         L99F2           ; KEEP PAINTING THE LINE
L9A09           PULS        Y,U             ; RESTORE PAINT COUNTER AND INC/DEC POINTER
L9A0B           TFR         U,D             ; SAVE PAINT COUNTER IN ACCD
                TFR         D,X             ; SAVE PAINT COUNTER IN X
                SUBD        ZERO            ; SET FLAGS ACCDRDING TO CONDITION OF PAINT COUNTER
                RTS
; CHECK FOR BORDER COLOR - ENTER W/VD9 CONTAINING
; ADDRESS OF ROUTINE TO GET ABS SCREEN ADDRESS
; AND PIXEL MASK - EXIT WITH Z = 1 IF HIT BORDER COLOR PIXEL
L9A12           JSR         [VD9]           ; GET THE ADDR AND PIXEL MASK
                TFR         A,B             ; COPY PIXEL MASK TO ACCB
                ANDB        VD8             ; AND PIXEL MASK W/BORDER COLOR; ACCB = ONE PIXEL OF BORDER COLOR
                PSHS        B,A             ; PUSH MASK AND BORDER PIXEL
                ANDA        ,X              ; PUT CURRENT PIXEL DATA INTO ACCB AND
                CMPA        $01,S           ; COMPARE IT TO BORDER COLOR; Z FLAG = 1 IF MATCH
                PULS        A,B,PC          ; RESTORE MASK AND BORDER PIXEL - THEN RETURN
; PLAY
PLAY            LDX         ZERO            ; DEFAULT VALUES FOR LENGTH OF PLAY COMMAND AND ADDRESS
                LDB         #$01            ; OF START OF PLAY STRING IF USED FOR PLAY (NULL STRING)
                PSHS        X,B             ; SAVE DEFAULT VALUES
                JSR         >LB156          ; EVALUATE EXPRESSION
                CLRB                        ;
                JSR         >LA9A2          ; SET UP DA TO PASS THROUGH ANA MUX
                JSR         >LA976          ; ENABLE ANA MUX
L9A32           JSR         >LB654          ; POINT X TO START OF PLAY STRING AND PUT LENGTH
; OF STRING INTO ACCB
                BRA         L9A39           ; INEFFICIENT - SHOULD BE FCB SKP2
L9A37           PULS        B,X             ; GET PLAY STRING START AND LENGTH
L9A39           STB         VD8             ; LENGTH OF PLAY COMMAND
                BEQ         L9A37           ; GET NEW STRING DATA IF LENGTH = 0
                STX         VD9             ; START OF PLAY STRING
                LBEQ        LA974           ; DISABLE ANA MUX AND RETURN IF X = 0
L9A43           TST         VD8             ; SEE IF LENGTH OF STRING = 0
                BEQ         L9A37           ; GET NEW DATA IF SO
                JSR         >L9B98          ; GET A COMMAND CHARACTER IF NOT
                CMPA        #';             ; SUB COMMAND TERMINATED
                BEQ         L9A43           ; IGNORE SEMICOLONS
                CMPA        #''             ; CHECK FOR APOSTROPHE
                BEQ         L9A43           ; IGNORE THEM TOO
                CMPA        #'X             ; CHECK FOR AN EXECUTABLE SUBSTRING
                LBEQ        L9C0A           ; GO PROCESS SUB COMMAND
                BSR         L9A5C           ; CHECK FOR OTHER COMMANDS
                BRA         L9A43           ; KEEP GOING THROUGH INTERPRETATION LOOP
; OCTAVE
L9A5C           CMPA        #'O             ; ADJUST OCTAVE?
                BNE         L9A6D           ; NO
                LDB         OCTAVE          ; GET CURRENT OCTAVE
                INCB                        ;  LEGAL VALUES ARE 1-5 BUT INTERNALLY THE COMPUTER USES 0-4
                BSR         L9AC0           ; MODIFIER CHECK
                DECB                        ;  COMPENSATE FOR INCB ABOVE
                CMPB        #$04            ; MAXIMUM VALUE OF 4
                BHI         L9ACD           ; FC ERROR
                STB         OCTAVE          ; SAVE NEW VALUE OF OCTAVE
                RTS
; VOLUME
L9A6D           CMPA        #'V             ; ADJUST VOLUME?
                BNE         L9A8B           ; NO
                LDB         VOLHI           ; GET CURRENT HIGH VOLUME LIMIT
                LSRB                        ;  SHIFT 2 BITS TO RIGHT DA IS ONLY 6 BITS (BIT 2 - BIT 7) -
                LSRB                        ;  TO MANIPULATE THE DATA IT MUST BE IN BITS 0-5
                SUBB        #31             ; SUBTRACT OUT MID VALUE OFFSET
                BSR         L9AC0           ; MODIFIER CHECK
                CMPB        #31             ; MAXIMUM ALLOWED RANGE IS 31
                BHI         L9ACD           ; FC ERROR
                ASLB                        ;
                ASLB                        ;  MOVE NEW VALUE BACK TO BITS 2-7
                PSHS        B               ; SAVE NEW VOLUME ON THE STACK
                LDD         #$7E7E          ; PUT MID VALUE IN HIGH AND LOW LIMIT
                ADDA        ,S              ; ADD NEW VOLUME TO HIGH LIMIT
                SUBB        ,S+             ; SUBTR NEW VOLUME FROM LOW LIMIT
                STD         VOLHI           ; SAVE NEW VOLUME LIMITS
                RTS
; NOTE LENGTH
L9A8B           CMPA        #'L             ; SET NOTE LENGTH?
                BNE         L9AB2           ; NO
                LDB         NOTELN          ; GET CURRENT LENGTH
                BSR         L9AC0           ; MODIFIER CHECK
                TSTB                        ;
                BEQ         L9ACD           ; FC ERROR IF LENGTH = 0
                STB         NOTELN          ; SAVE NEW NOTE LENGTH
                CLR         DOTVAL          ; RESET NOTE TIMER SCALE FACTOR
L9A9A           BSR         L9A9F           ; CHECK FOR A DOTTED NOTE
                BCC         L9A9A           ; BRANCH IF DOTTED NOTE
                RTS
; SCALE FACTOR - DOTTED NOTE
L9A9F           TST         VD8             ; CHECK COMMAND LENGTH
                BEQ         L9AAD           ; ITS EMPTY
                JSR         >L9B98          ; GET COMMAND CHARACTER
                CMPA        #'.             ; CHECK FOR DOTTED NOTE
                BEQ         L9AAF           ; BRANCH ON DOTTED NOTE AND CLEAR CARRY FLAG
                JSR         >L9BE2          ; MOVE COMMAND STRING POINTER BACK ONE AND ADD ONE TO
; LENGTH
L9AAD           COMA                        ;  SET CARRY FLAG
                RTS
L9AAF           INC         DOTVAL          ; ADD ONE TO NOTE TIMER SCALE FACTOR
                RTS
; TEMPO
L9AB2           CMPA        #'T             ; MODIFY TEMPO?
                BNE         L9AC3           ; NO
                LDB         TEMPO           ; GET CURRENT TEMPO
                BSR         L9AC0           ; EVALUATE MODIFIER
                TSTB                        ;  SET FLAGS
                BEQ         L9ACD           ; FC ERROR IF ITS 0
                STB         TEMPO           ; SAVE NEW TEMPO
                RTS
L9AC0           JMP         >L9BAC          ; EVALUATE THE >,<,+,-,= OPERATORS
; PAUSE
L9AC3           CMPA        #'P             ; PAUSE COMMAND?
                BNE         L9AEB           ; NO
                JSR         >L9CCB          ; EVALUATE A DECIMAL COMMAND STRING VALUE
                TSTB                        ;  CHECK FOR LEGAL EXPRESSION AND
                BNE         L9AD0           ; BRANCH IF PAUSE VALUE <> 0
L9ACD           JMP         >LB44A          ; FC ERROR IF PAUSE <> 0
L9AD0           LDA         DOTVAL          ; SAVE CURRENT VALUE OF VOLUME AND NOTE
                LDX         VOLHI           ; TIMER SCALE
                PSHS        X,A             ;
                LDA         #$7E            ; MID VALUE OF DA CONVERTER
                STA         VOLHI           ; SET VOLUME = 0
                STA         VOLLOW          ;
                CLR         DOTVAL          ; RESET NOTE TIMER SCALE FACTOR
                BSR         L9AE7           ; GO PLAY A NOTE OF 0 VOLUME
                PULS        A,X             ;
                STA         DOTVAL          ; RESTORE VALUE OF VOLUME
                STX         VOLHI           ; AND NOTE TIMER SCALE
                RTS
L9AE7           CLR         ,-S             ; PUSH NOTE NUMBER 0 ONTO STACK
                BRA         L9B2B           ; GO PLAY IT
; NOTE
L9AEB           CMPA        #'N             ; LETTER N BEFORE THE NUMBER OF A NOTE?
                BNE         L9AF2           ; NO - ITS OPTIONAL
                JSR         >L9B98          ; GET NEXT COMMAND CHARACTER
L9AF2           CMPA        #'A             ; CHECK FOR NOTE A
                BLO         L9AFA           ; BELOW
                CMPA        #'G             ; CHECK FOR NOTE B
                BLS         L9AFF           ; FOUND NOTE A-G
L9AFA           JSR         >L9BBE          ; EVALUATE DECIMAL NUMERIC EXPRESSION IN COMMAND STRING
                BRA         L9B22           ; PROCESS NOTE VALUE
; PROCESS A NOTE HERE
L9AFF           SUBA        #'A             ; MASK OFF ASCII
                LDX         #L9C5B          ; LOAD X WITH NOTE JUMP TABLE
                LDB         A,X             ; GET NOTE
                TST         VD8             ; ANY COMMAND CHARACTERS LEFT?
                BEQ         L9B22           ; NO
                JSR         >L9B98          ; GET COMMAND CHARACTER
                CMPA        #'#             ; SHARP NOTE?
                BEQ         L9B15           ; YES
                CMPA        #'+             ; SHARP NOTE?
                BNE         L9B18           ; NO
L9B15           INCB                        ;  ADD 1 TO NOTE NUMBER (SHARP)
                BRA         L9B22           ; PROCESS NOTE
L9B18           CMPA        #'-             ; FLAT NOTE?
                BNE         L9B1F           ; NO
                DECB                        ;  SUBTR 1 FROM NOTE NUMBER (FLAT)
                BRA         L9B22           ; PROCESS NOTE
L9B1F           JSR         >L9BE2          ; MOVE COMMAND STRING PTR BACK ONE AND ADD ONE
; TO COMMAND LENGTH CTR
L9B22           DECB                        ;  =ADJUST NOTE NUMBER, BASIC USES NOTE NUMBERS 1-12, INTERNALLY
; =COMPUTER USES 0-11
                CMPB        #12-1           ; MAXIMUM NOTE VALUE
                BHI         L9ACD           ; FC ERROR IF > 11
                PSHS        B               ; SAVE NOTE VALUE
                LDB         NOTELN          ; GET NOTE LENGTH
L9B2B           LDA         TEMPO           ; GET TEMPO
                MUL                         ;  CALCULATE NOTE DURATION
                STD         VD5             ; SAVE NOTE DURATION
; THE IRQ INTERRUPT IS USED TO PROVIDE A MASTER TIMING REFERENCE FOR
; THE PLAY COMMAND. WHEN A NOTE IS DONE, THE IRQ SERVICING
; ROUTINE WILL RETURN CONTROL TO THE MAIN PLAY COMMAND INTERPRETATION LOOP
                LEAU        $01,S           ; LOAD U W/CURRENT VALUE OF (STACK POINTER+1) SO THAT THE STACK
; POINTER WILL BE PROPERLY RESET WHEN IRQ VECTORS
; YOU OUT OF THE PLAY TIMING ROUTINES BELOW
                LDA         OCTAVE          ; GET CURRENT OCTAVE
                CMPA        #$01            ;
                BHI         L9B64           ; BRANCH IF OCTAVE > 1
; OCTAVES 1 AND 2 USE A TWO BYTE DELAY TO SET THE PROPER FREQUENCY
                LDX         #L9C62          ; POINT TO DELAY TABLE
                LDB         #2*12           ; 24 BYTES DATA/OCTAVE
                MUL                         ;  CALC OCTAVE TABLE OFFSET
                ABX                         ;  POINT TO CORRECT OCTAVE TABLE
                PULS        B               ; GET NOTE VALUE BACK
                ASLB                        ;  X 2 - 2 BYTES/NOTE
                ABX                         ;  POINT TO CORRECT NOTE
                LEAY        ,X              ; GET POINTER TO Y REG (TFR X,Y)
                BSR         L9B8C           ; CALCULATE NOTE TIMER VALUE
                STD         PLYTMR          ; SAVE IT
; MAIN SOUND GENERATION LOOP - ONLY THE IRQ SERVICE WILL GET YOU OUT
; OF THIS LOOP (OCTAVES 1 AND 2)
L9B49           BSR         L9B57           ; MID VALUE TO DA AND WAIT
                LDA         VOLHI           ; GET HIGH VALUE
                BSR         L9B5A           ; STORE TO DA AND WAIT
                BSR         L9B57           ; MID VALUE TO DA AND WAIT
                LDA         VOLLOW          ; GET LOW VALUE
                BSR         L9B5A           ; STORE
                BRA         L9B49           ; KEEP LOOPING
L9B57           LDA         #$7E            ; DA MID VALUE AND RS 232 MARKING
                NOP                         ;  DELAY SOME - FINE TUNE PLAY FREQUENCY
L9B5A           STA         PIA1            ; STORE TO DA CONVERTER
                LDX         ,Y              ; GET DELAY FROM OCTAVE TABLE
L9B5F           LEAX        -1,X            ;
                BNE         L9B5F           ; COUNT X TO ZERO - PROGRAMMABLE DELAY
                RTS
; OCTAVES 3,4 AND 5 USE A ONE BYTE DELAY TO SET THE PROPER FREQUENCY
L9B64           LDX         #L9C92-2*12     ; POINT TO DELAY TABLE
                LDB         #12             ; 12 BYTES DATA PER OCTAVE
                MUL                         ;  CALC OCTAVE TABLE OFFSET
                ABX                         ;  POINT TO CORRECT OCTAVE TABLE
                PULS        B               ; GET NOTE VALUE BACK
                ABX                         ;  POINT TO CORRECT NOTE
                BSR         L9B8C           ; CALCULATE NOTE TIMER VALUE
                STD         PLYTMR          ; SAVE IT
L9B72           BSR         L9B80           ; MID VALUE TO DA AND WAIT
                LDA         VOLHI           ; GET HIGH VALUE
                BSR         L9B83           ; STORE TO DA AND WAIT
                BSR         L9B80           ; MID VALUE TO DA AND WAIT
                LDA         VOLLOW          ; GET LOW VALUE
                BSR         L9B83           ; STORE TO DA AND WAIT
                BRA         L9B72           ; KEEP GOING
; PUT MID VALUE TO DA CONVERTER AND WAIT A WHILE
L9B80           LDA         #$7E            ; DA CONVERTER MID VALUE AND KEEP RS 232 OUTPUT MARKING
                NOP                         ;  DELAY SOME - FINE TUNE PLAY FREQUENCY
L9B83           STA         PIA1            ; STORE IN DA CONVERTER
                LDA         ,X              ; GET DELAY VALUE FROM OCTAVE TABLE
L9B88           DECA                        ;  COUNT ACCA TO ZERO - TIME DELAY
                BNE         L9B88           ; COUNT ACCA TO ZERO - TIME DELAY
                RTS
; CALCULATE NOTE TIMER VALUE - RETURN WITH VALUE IN ACCD -
; THE LARGER ACCD IS, THE LONGER THE NOTE WILL PLAY
L9B8C           LDB         #$FF            ; NOTE TIMER BASE VALUE
                LDA         DOTVAL          ; GET NOTE TIMER SCALE FACTOR
                BEQ         L9B97           ; USE DEFAULT VALUE IF 0
                ADDA        #$02            ; ADD IN CONSTANT TIMER SCALE FACTOR
                MUL                         ;  MULTIPLY SCALE FACTOR BY BASE VALUE
                LSRA                        ;  DIVIDE ACCD BY TWO - EACH INCREMENT OF DOTVAL
                RORB                        ;  WILL INCREASE NOTE TIMER BY 128
L9B97           RTS
; GET NEXT COMMAND - RETURN VALUE IN ACCA
L9B98           PSHS        X               ; SAVE X REGISTER
L9B9A           TST         VD8             ; CHECK COMMAND COUNTER
                BEQ         L9BEB           ; FC ERROR IF NO COMMAND DATA LEFT
                LDX         VD9             ; GET COMMAND ADDR
                LDA         ,X+             ; GET COMMAND
                STX         VD9             ; SAVE NEW ADDRESS
                DEC         VD8             ; DECREMENT COMMAND CTR
                CMPA        #SPACE          ; CHECK FOR BLANK
                BEQ         L9B9A           ; IGNORE BLANKS
                PULS        X,PC            ; RESTORE X RESISTER AND RETURN
; EVALUATE THE >,<,+,-,= OPERATORS - ENTER WITH THE VALUE TO
; BE OPERATED ON IN ACCB, RETURN NEW VALUE IN SAME
L9BAC           BSR         L9B98           ; GET A COMMAND CHARACTER
                CMPA        #'+             ; ADD ONE?
                BEQ         L9BEE           ; YES
                CMPA        #'-             ; SUBTRACT ONE?
                BEQ         L9BF2           ; YES
                CMPA        #'>             ; MULTIPLY BY TWO?
                BEQ         L9BFC           ; YES
                CMPA        #'<             ; DIVIDE BY TWO?
                BEQ         L9BF7           ; YES
L9BBE           CMPA        #'=             ; CHECK FOR VARIABLE EQUATE - BRANCH IF SO; ACCB WILL BE
                BEQ         L9C01           ; SET TO THE VALUE OF THE BASIC VARIABLE IN THE COMMAND
; STRING WHICH MUST BE NUMERIC, LESS THAN 256
; AND THE VARIABLE MUST BE FOLLOWED BY A SEMICOLON.
                JSR         >L90AA          ; CLEAR CARRY IF NUMERIC
                BLO         L9BEB           ; FC ERROR IF NON NUMERIC
                CLRB                        ;  UNITS DIGIT = 0
; STRIP A DECIMAL ASCII VALUE OFF OF THE COMMAND STRING
; AND RETURN BINARY VALUE IN ACCB
L9BC8           SUBA        #'0             ; MASK OFF ASCII
                STA         VD7             ; SAVE VALUE TEMPORARILY
                LDA         #10             ; BASE 10
                MUL                         ;  MULT BY DIGIT
                TSTA                        ;
                BNE         L9BEB           ; FC ERROR IF RESULT > 255
                ADDB        VD7             ; GET TEMPORARY VALUE
                BLO         L9BEB           ; FC ERROR IF RESULT > 255
                TST         VD8             ;
                BEQ         L9BF1           ; RETURN IF NO COMMANDS LEFT
                JSR         >L9B98          ; GET ANOTHER COMMAND
                JSR         >L90AA          ; CLEAR CARRY IF NUMERIC
                BCC         L9BC8           ; BRANCH IF MORE NUMERIC DATA
L9BE2           INC         VD8             ; ADD ONE TO COMMAND COUNTER AND
                LDX         VD9             ; MOVE COMMAND STRING BACK ONE
                LEAX        -1,X            ;
                STX         VD9             ;
                RTS
L9BEB           JMP         >LB44A          ; FC ERROR
L9BEE           INCB                        ;  ADD ONE TO PARAMETER
                BEQ         L9BEB           ; FC ERROR IF ADDING 1 TO 255
L9BF1           RTS
L9BF2           TSTB                        ;
                BEQ         L9BEB           ; FC ERROR IF TRYING TO DECREMENT 0
                DECB                        ;  SUBTRACT ONE FROM PARAMETER
                RTS
L9BF7           TSTB                        ;
                BEQ         L9BEB           ; FC ERROR IF DIVIDING BY ZERO
                LSRB                        ;  DIVIDE BY TWO
                RTS
L9BFC           TSTB                        ;
                BMI         L9BEB           ; FC ERROR IF RESULT WOULD BE > 255
                ASLB                        ;  MULTIPLY BY TWO
                RTS
L9C01           PSHS        U,Y             ; SAVE U,Y REGISTERS
                BSR         L9C1B           ; INTERPRET COMMAND STRING AS IF IT WERE A BASIC VARIABLE
                JSR         >LB70E          ; CONVERT FPA0 TO AN INTEGER VALUE IN ACCB
                PULS        Y,U,PC          ; RESTORE U,Y REGISTERS AND RETURN
L9C0A           JSR         >L9C1B          ; EVALUATE AN EXPRESSION IN THE COMMAND STRING
                LDB         #2
                JSR         >LAC33          ; =ROOM FOR 4 BYTES ON STACK?
                LDB         VD8             ; GET THE CURRENT COMMAND LENGTH AND POINTER AND
                LDX         VD9             ; SAVE THEM ON THE STACK
                PSHS        X,B             ;
                JMP         >L9A32          ; GO INTERPRET AND PROCESS THE NEW PLAY SUB COMMAND
; INTERPRET THE PRESENT COMMAND STRING AS IF IT WERE A BASIC VARIABLE
L9C1B           LDX         VD9             ; GET COMMAND POINTER
                PSHS        X               ; SAVE IT
                JSR         >L9B98          ; GET A COMMAND CHARACTER
                JSR         >LB3A2          ; SET CARRY IF NOT ALPHA
                BLO         L9BEB           ; FC ERROR IF NOT ALPHA - ILLEGAL VARIABLE NAME
L9C27           JSR         >L9B98          ; GET A COMMAND CHARACTER
                CMPA        #';             ; CHECK FOR SEMICOLON - COMMAND SEPARATOR
                BNE         L9C27           ; BRANCH UNTIL FOUND
                PULS        X               ; GET SAVED COMMAND POINTER
                LDU         CHARAD          ; GET BASICS INPUT POINTER
                PSHS        U               ; SAVE IT
                STX         CHARAD          ; PUT PLAY COMMAND POINTER IN PLACE OF BASICS INPUT POINTER
                JSR         >LB284          ; EVALUATE AN ALPHA EXPRESSION P GET NEW STRING DESCRIPTOR
                PULS        X               ; RESTORE BASICS INPUT POINTER
                STX         CHARAD          ;
                RTS
; MORE OF EXTENDED BASICS IRQ ROUTINE
L9C3E           CLRA                        ;  CLEAR ACCA
                TFR         A,DP            ; SET THE DIRECT PAGE TO ZERO
                LDD         PLYTMR          ; GET THE PLAY TIMER
                LBEQ        LA9BB           ; BRANCH TO COLOR BASICS IRQ ROUTINE IF ZERO
                SUBD        VD5             ; SUBTRACT OUT PLAY INTERVAL
                STD         PLYTMR          ; SAVE THE NEW TIMER VALUE
                BHI         L9C5A           ; BRANCH IF PLAY COMMAND NOT DONE
                CLR         PLYTMR          ; RESET MSB OF PLAY TIMER IF DONE
                CLR         PLYTMR+1        ; RESET LSB OF PLAY TIMER
                PULS        A               ; GET THE CONDITION CODE REG
                LDS         $07,S           ; LOAD THE STACK POINTER WITH THE CONTENTS OF THE U REGISTER
; WHICH WAS STACKED WHEN THE INTERRUPT WAS HONORED.
                ANDA        #$7F            ; CLEAR E FLAG - MAKE COMPUTER THINK THIS WAS AN FIRQ
                PSHS        A               ; SAVE CONDITION CODE
; THE RTI WILL NOW NOT RETURN TO WHERE IT WAS
; INTERRUPTED FROM - IT WILL RETURN TO THE MAIN PLAY
; COMMAND INTERPRETATION LOOP.
L9C5A           RTI         RETURN
; TABLE OF NUMERICAL NOTE VALUES FOR LETTER NOTES
L9C5B           FCB         10,12,1,3,5,6,8 ; NOTES A,B,C,D,E,F,G
; TABLE OF DELAYS FOR OCTAVE 1
L9C62           FDB         $01A8,$0190,$017A ; DELAYS FOR OCTAVE 1
                FDB         $0164,$0150,$013D
                FDB         $012B,$011A,$010A
                FDB         $00FB,$00ED,$00DF
; TABLE OF DELAYS FOR OCTAVE 2
L9C7A           FDB         $00D3,$00C7,$00BB ; DELAYS FOR OCTAVE 2
                FDB         $00B1,$00A6,$009D
                FDB         $0094,$008B,$0083
                FDB         $007C,$0075,$006E
; TABLE OF DELAYS FOR OCTAVES 3,4,5
L9C92           FCB         $A6,$9C,$93,$8B,$83,$7B ; DELAYS FOR OCTAVES 3,4,5
                FCB         $74,$6D,$67,$61,$5B,$56
                FCB         $51,$4C,$47,$43,$3F,$3B
                FCB         $37,$34,$31,$2E,$2B,$28
                FCB         $26,$23,$21,$1F,$1D,$1B
                FCB         $19,$18,$16,$14,$13,$12
DRAW            LDX         ZERO            ; X=0. ACCB=1; END OF DRAW COMMAND LINE VALUES -
                LDB         #$01            ; WHEN THESE VALUES ARE PULLED OFF THE
                PSHS        X,B             ; STACK, THE DRAW COMMAND WILL END
                STB         SETFLG          ; SET PSET/PRESET FLAG TO PSET
                STX         VD5             ; CLEAR UPDATE FLAG AND DRAW FLAG
                JSR         >L959A          ; SET ACTIVE COLOR BYTE
                JSR         >LB156          ; EVALUATE EXPRESSION
L9CC6           JSR         >LB654          ; GET THE LENGTH AND ADDRESS OF THE COMMAND STRING
                BRA         L9CD3           ; INTERPRET THE COMMAND STRING
L9CCB           JSR         >L9B98          ; GET NEXT CHARACTER FROM COMMAND LINE
                JMP         >L9BBE          ; EVALUATE A DECIMAL VALUE IN COMMAND LINE
L9CD1           PULS        B,X             ; GET NEXT COMMAND LINE TO BE INTERPRETED OFF THE STACK
L9CD3           STB         VD8             ; SET COMMAND LENGTH CTR
                BEQ         L9CD1           ; GET NEW COMMAND LINE IF 0
                STX         VD9             ; SET COMMAND LINE ADDRESS
                LBEQ        L9DC7           ; EXIT ROUTINE IF ADDRESS = 0
L9CDD           TST         VD8             ; TEST COMMAND LENGTH CTR
                BEQ         L9CD1           ; GET NEW LINE IF 0
                JSR         >L9B98          ; GET A COMMAND CHAR
                CMPA        #';             ; CHECK FOR SEMICOLON
                BEQ         L9CDD           ; IGNORE SEMICOLONS
                CMPA        #''             ; CHECK FOR APOSTROPHES
                BEQ         L9CDD           ; IGNORE APOSTROPHES
                CMPA        #'N             ; UPDATE CHECK?
                BNE         L9CF4           ; NO
                COM         VD5             ; TOGGLE UPDATE FLAG 0 = UPDATE, FF = NO UPDATE
                BRA         L9CDD           ; GET NEXT COMMAND
L9CF4           CMPA        #'B             ; CHECK DRAW FLAG?
                BNE         L9CFC           ; NO
                COM         VD6             ; TOGGLE DRAW FLAG 0 = DRAW LINE, FF = DONT DRAW LINE
                BRA         L9CDD           ; GET NEXT COMMAND
L9CFC           CMPA        #'X             ; SUBSTRING?
                LBEQ        L9D98           ; GO EXECUTE A COMMAND
                CMPA        #'M             ; MOVE THE DRAW POSITION ?
                LBEQ        L9E32           ; YES; GO MOVE IT
                PSHS        A               ; SAVE CURRENT COMMAND
                LDB         #$01            ; DEFAULT VALUE IF NO NUMBER FOLLOWS COMMAND
                TST         VD8             ; CHECK COMMAND LENGTH CTR
                BEQ         L9D21           ; BRANCH IF NO COMMANDS LEFT
                JSR         >L9B98          ; GET A COMMAND CHAR
                JSR         >LB3A2          ; SET CARRY IF NOT ALPHA
                PSHS        CC              ; SAVE CARRY FLAG
                JSR         >L9BE2          ; MOVE COMMAND POINTER BACK ONE
                PULS        CC              ; RESTORE CARRY FLAG
                BCC         L9D21           ; BRANCH IF NEXT COMMAND IS ALPHA
                BSR         L9CCB           ; EVALUATE A DECIMAL COMMAND LINE VALUE - RETURN VALUE IN ACCB
L9D21           PULS        A               ; GET CURRENT COMMAND BACK
                CMPA        #'C             ; CHANGE COLOR?
                BEQ         L9D4F           ; YES
                CMPA        #'A             ; CHANGE ANGLE?
                BEQ         L9D59           ; YES
                CMPA        #'S             ; CHANGE SCALE?
                BEQ         L9D61           ; YES
                CMPA        #'U             ; GO UP?
                BEQ         L9D8F           ; YES
                CMPA        #'D             ; GO DOWN?
                BEQ         L9D8C           ; YES
                CMPA        #'L             ; GO LEFT?
                BEQ         L9D87           ; YES
                CMPA        #'R             ; GO RIGHT?
                BEQ         L9D82           ; YES
                SUBA        #'E             ; MASK OFF ASCII FOR LETTER E-H COMMAND CHECKS
                BEQ         L9D72           ; BRANCH IF E (45 DEGREES)
                DECA                        ;
                BEQ         L9D6D           ; BRANCH IF F (135 DEGREES)
                DECA                        ;
                BEQ         L9D7B           ; =BRANCH IF G (225 DEGREES)
                DECA                        ;
                BEQ         L9D69           ; BRANCH IF H (31S DEGREES)
L9D4C           JMP         >LB44A          ; FC ERROR IF ILLEGAL COMMAND
; CHANGE COLOR
L9D4F           JSR         >L955D          ; ADJUST COLOR CODE FOR PROPER PMODE
                STB         FORCOL          ; SAVE NEW FOREGROUND COLOR
                JSR         >L959A          ; SET COLOR BYTES (WCOLOR,ALLCOL)
L9D57           BRA         L9CDD           ; GO PROCESS ANOTHER COMMAND
; CHANGE ANGLE
L9D59           CMPB        #$04            ; ONLY 0-3 ARE LEGAL
                BCC         L9D4C           ; FC ERROR IF ANGLE NUMBER > 3
                STB         ANGLE           ; SAVE DRAW ANGLE
                BRA         L9D57           ; GO PROCESS ANOTHER COMMAND
; CHANGE SCALE
L9D61           CMPB        #63             ; ONLY 0-63 ARE LEGAL
                BCC         L9D4C           ; FC ERROR IF SCALE > 63
                STB         SCALE           ; SAVE DRAW SCALE
                BRA         L9D57           ; GO PROCESS ANOTHER COMMAND
; 315 DEGREES
L9D69           CLRA                        ;  NEGATE ACCD - MAKE HORIZONTAL
                BSR         L9DC4           ; DIFFERENCE NEGATIVE
L9D6C           FCB         SKP1            ; SKIP ONE BYTE - KEEP HORIZONTAL DIFFERENCE NEGATIVE
; 135 DEGREES
L9D6D           CLRA                        ;  CLEAR MS BYTE OF HORIZONTAL DIFFERENCE
                TFR         D,X             ; COPY HORIZONTAL DIFFERENCE TO VERTICAL DIFFERENCE
                BRA         L9DCB           ; GO MOVE THE DRAW POSITION
; 45 DEGREES
L9D72           CLRA                        ;  CLEAR MS BYTE OF HORIZONTAL DIFFERENCE
                TFR         D,X             ; COPY HORIZONTAL DIFFERENCE TO VERTICAL DIFFERENCE
                BSR         L9DC4           ; NEGATE ACCD - MAKE HORIZONTAL DIFFERENCE NEGATIVE
                EXG         D,X             ; EXCHANGE HORIZONTAL AND VERTICAL DIFFERENCES
                BRA         L9DCB           ; GO MOVE THE DRAW POSITION
; 225 DEGREES
L9D7B           CLRA                        ;  CLEAR MS BYTE OF HORIZONTAL DIFFERENCE
                TFR         D,X             ; COPY HORIZONTAL DIFFERENCE TO VERTICAL DIFFERENCE
                BSR         L9DC4           ; NEGATE ACCD - MAKE HORIZONTAL DIFFERENCE NEGATIVE
                BRA         L9DCB           ; GO MOVE THE DRAW POSITION
; GO RIGHT
L9D82           CLRA                        ;  CLEAR MS BYTE OF HORIZONTAL DIFFERENCE
L9D83           LDX         ZERO            ; X = 0; VERT DIFF = 0
                BRA         L9DCB           ; GO MOVE THE DRAW POSITION
; GO LEFT
L9D87           CLRA                        ;  NEGATE ACCD - MAKE THE HORIZONTAL
                BSR         L9DC4           ; DIFFERENCE NEGATIVE
                BRA         L9D83           ; MAKE VERTICAL DIFFERENCE ZERO AND MOVE THE DRAW POSITION
; GO DOWN
L9D8C           CLRA                        ;  CLEAR MS BYTE OF HORIZONTAL DIFFERENCE
                BRA         L9D92           ; MAKE VERTICAL DIFFERENCE = 0, EXCHANGE HORIZONTAL AND
; VERTICAL DIFFERENCES AND MOVE THE DRAW POSTION
; GO UP
L9D8F           CLRA                        ;  NEGATE ACCD - MAKE THE HORIZONTAL
                BSR         L9DC4           ; DIFFERENCE NEGATIVE
L9D92           LDX         ZERO            ; X = 0; HORZ DIFF = 0
                EXG         X,D             ; EXCHANGE THE HORIZONTAL AND VERTICAL DIFFERENCES
                BRA         L9DCB           ; GO MOVE THE DRAW POSITION
; EXECUTE A COMMAND SUB STRING
L9D98           JSR         >L9C1B          ; INTERPRET CURRENT COMMAND AS IF IT WERE A BASIC VARIABLE
                LDB         #$02            ; =
                JSR         >LAC33          ; =FOUR BYTES OF FREE RAM LEFT?
                LDB         VD8             ;
                LDX         VD9             ; GET CURRENT COMMAND LENGTH AND POINTER
                PSHS        X,B             ; AND SAVE THEM ON THE STACK
                JMP         >L9CC6          ; EVALUATE NUMERICAL VALUE IN COMMAND LINE
; MULTIPLY HOR OR VER DIFFERENCE BY SCALE FACTOR.
; DIVIDE PRODUCT BY 4 AND RETURN VALUE IN ACCD
L9DA9           LDB         SCALE           ; GET DRAW SCALE AND BRANCH IF ZERO - THIS WILL CAUSE A
                BEQ         L9DC8           ; ZERO DEFAULT TO FULL SCALE
                CLRA                        ;  CLEAR MS BYTE
                EXG         D,X             ; EXCHANGE DIFFERENCE AND SCALE FACTOR
                STA         ,-S             ; SAVE MS BYTE OF DIFFERENCE ON STACK (SIGN INFORMATION)
                BPL         L9DB6           ; BRANCH IF POSITIVE DIFFERENCE
                BSR         L9DC3           ; NEGATE ACCD
L9DB6           JSR         >L9FB5          ; MULT DIFFERENCE BY SCALE FACTOR
                TFR         U,D             ; SAVE 2 MS BYTES IN ACCD
                LSRA                        ;
                RORB                        ;
L9DBD           LSRA                        ;
                RORB                        ;  DIVIDE ACCD BY 4 - EACH SCALE INCREMENT IS 1/4 FULL SCALE
                TST         ,S+             ; =CHECK SIGN OF ORIGINAL DIFFERENCE AND
                BPL         L9DC7           ; =RETURN IF POSITIVE
; NEGATE ACCUMULATOR D
L9DC3           NEGA                        ;
L9DC4           NEGB                        ;
                SBCA        #$00            ; NEGATE ACCUMULATOR D IF ACCA=0
L9DC7           RTS
L9DC8           TFR         X,D             ; TRANSFER UNCHANGED DIFFERENCE TO ACCD
                RTS
; MOVE THE DRAW POSITION - ADD THE ORTHOGONAL DIFFERENCES
; IN ACCD (HORIZONTAL) AND X (VERTICAL) TO
; THE CURRENT POSITION; DRAW A LINE AFTER THE MOVE
L9DCB           PSHS        B,A             ; SAVE HORIZ DIFFERENCE
                BSR         L9DA9           ; APPLY SCALE FACTOR TO VERTICAL
                PULS        X               ; GET HORIZ DIFFERENCE
                PSHS        B,A             ; SAVE VERT DIFFERENCE
                BSR         L9DA9           ; APPLY SCALE FACTOR TO HORIZONTAL
                PULS        X               ; GET VERT DIFFERENCE
                LDY         ANGLE           ; GET THE DRAW ANGLE AND SCALE AND SAVE THEM ON
                PSHS        Y               ; THE STACK; USE Y BECAUSE IT IS THE ONLY UNUSED REGISTER
L9DDC           TST         ,S              ; CHECK DRAW ANGLE
                BEQ         L9DE8           ; BRANCH IF NO ANGLE
                EXG         X,D             ; EXCH HOR AND VER DIFFERENCES
                BSR         L9DC3           ; NEGATE ACCD
                DEC         ,S              ; DECR ANGLE
                BRA         L9DDC           ; CHECK ANGLE AGAIN
L9DE8           PULS        Y               ; PULL ANGLE AND SCALE OFF THE STACK
                LDU         ZERO            ; U = 0; DEFAULT HOR END POSITION = 0
                ADDD        HORDEF          ; ADD DIFFERENCE TO HORIZ START
                BMI         L9DF2           ; HORIZ COORD = 0 IF RESULT IS NEG
                TFR         D,U             ; SAVE HOR END POSITION IN U
L9DF2           TFR         X,D             ; PUT VERT DIFFERENCE IN ACCD
                LDX         ZERO            ; X = 0; DEFAULT VER END POSITION = 0
                ADDD        VERDEF          ; ADD DIFFERENCE TO VER START
                BMI         L9DFC           ; VER COORD = 0 IF RESULT IS NEG
                TFR         D,X             ; SAVE VERT END POSITION IN X
; MOVE THE DRAW POSITION; ENTER WITH ABSOLUTE HORIZONTAL POSITION
; IN U REGISTER AND ABSOLUTE VERTICAL POSITlON IN X REGISTER.
L9DFC           CMPU        #256            ; IS HORIZ COORD WITHIN RANGE?
                BLO         L9E05           ; YES
                LDU         #255            ; NO - FORCE TO MAX VALUE
L9E05           CMPX        #192            ; IS VERT COORD WITHIN RANGE?
                BLO         L9E0D           ; YES
                LDX         #191            ; NO - FORCE TO MAX VALUE
L9E0D           LDD         HORDEF          ;
                STD         HORBEG          ;
                LDD         VERDEF          ; COPY THE HOR AND VER POINTERS
                STD         VERBEG          ; INTO THE DRAW LINE START POSITION
                STX         VEREND          ; =
                STU         HOREND          ; SET THE DRAW LINE END POSITION
                TST         VD5             ; CHECK UPDATE FLAG
                BNE         L9E21           ; BRANCH IF NO UPDATE
                STX         VERDEF          ;
                STU         HORDEF          ; UPDATE POSITION OF DRAW POINTER
L9E21           JSR         >L9420          ; NORMALIZE COORDS IN HOREND, VEREND AND HORBEG,VERBEG
                TST         VD6             ; GET DRAW FLAG
                BNE         L9E2B           ; BRANCH IF NO DRAW
                JSR         >L94A1          ; DRAW A LINE FROM (HORBEG,VERBEG) TO (HOREND,VEREND)
L9E2B           CLR         VD5             ; RESET UPDATE FLAG
                CLR         VD6             ; RESET DRAW FLAG
                JMP         >L9CDD          ; GO GET ANOTHER COMMAND
; SET THE DRAW POSITION
L9E32           JSR         >L9B98          ; GET A CHAR FROM COMMAND LINE
                PSHS        A               ; SAVE CHARACTER
                JSR         >L9E5E          ; EVALUATE HORIZ DIFFERENCE
                PSHS        B,A             ; SAVE IT ON STACK
                JSR         >L9B98          ; GET A CHAR FROM COMMAND LINE
                CMPA        #',             ; CHECK FOR COMMA
                LBNE        L9D4C           ; FC ERROR IF NO COMMA
                JSR         >L9E5B          ; EVALUATE VERT DIFFERENCE
                TFR         D,X             ; SAVE VERT DIFFERENCE IN X
                PULS        U               ; GET HORIZ DIFFERENCE IN U
                PULS        A               ; GET FIRST COMMAND CHARACTER
                CMPA        #'+             ; IF FIRST COMMAND CHAR WAS EITHER + OR -, TREAT
                BEQ         L9E56           ; THE VALUES IN U & X AS DIFFERENCES AND MOVE
                CMPA        #'-             ; POINTER, OTHERWISE TREAT U & X AS AN ABSOLUTE
                BNE         L9DFC           ; POSITION AND MOVE THE CURRENT POSITION THERE.
L9E56           TFR         U,D             ; PUT HORIZ DIFFERENCE IN ACCD
                JMP         >L9DCB          ; GO MOVE THE DRAW POSITION
L9E5B           JSR         >L9B98          ; GET A CHAR FROM COMMAND LINE
L9E5E           CMPA        #'+             ; CHECK FOR A LEADING PLUS SIGN (RELATIVE MOTION)
                BEQ         L9E69           ; AND BRANCH IF RELATIVE
                CMPA        #'-             ; CHECK FOR A LEADING MINUS SIGN (RELATIVE MOTION)
                BEQ         L9E6A           ; =AND BRANCH IF RELATIVE
                JSR         >L9BE2          ; MOVE COMMAND STRING BACK ONE IF NOT RELATIVE MOTION
L9E69           CLRA                        ;  ACCA 0 IS + ACCA <> 0 IS -'
L9E6A           PSHS        A               ; SAVE ADD/SUB FLAG
                JSR         >L9CCB          ; EVALUATE DECIMAL NUMBER IN COMMAND STRING - RETURN VALUE IN ACCB

                PULS        A               ; GET ADD/SUB FLAG
                TSTA                        ;  CHECK IT, 0:ADD, <> 0:SUB
                BEQ         L9E78           ; RETURN IF ADD
                CLRA                        ;
                NEGB                        ;
                SBCA        #$00            ; NEGATE ACCB INTO A TWO BYTE SIGNED VALUE IN ACCD
L9E78           RTS

; TABLE OF SINES AND COSINES FOR CIRCLE

L9E79           FDB         $0000,$0001     ; SUBARC 0
L9E7D           FDB         $FEC5,$1919     ; SUBARC 1
L9E81           FDB         $FB16,$31F2     ; SUBARC 2
L9E85           FDB         $F4FB,$4A51     ; SUBARC 3
L9E89           FDB         $EC84,$61F9     ; SUBARC 4
L9E8D           FDB         $E1C7,$78AE     ; SUBARC 5
L9E91           FDB         $D4DC,$8E3B     ; SUBARC 6
L9E95           FDB         $C5E5,$A269     ; SUBARC 7
L9E99           FDB         $B506,$B506     ; SUBARC 8

; CIRCLE
; THE CIRCLE IS ACTUALLY DRAWN AS A 64 SIDED
; POLYGON. IT IS COMPOSED OF 64 LINE COMMANDS
CIRCLE          CMPA        #'@             ; CHECK FOR @ SIGN
                BNE         L9EA3           ; SKIP IF NOT
                JSR         GETNCH          ; GET ANOTHER CHARACTER FROM BASIC
L9EA3           JSR         >L9522          ; GET MAX HOR & VER COORD VALUES AND PUT THEM IN VD3 AND VD5
                JSR         >L93B2          ; GET HOR & VER CENTER COORDS AND PUT IN HORBEG,VERBEG
                JSR         >L931D          ; NORMALIZE START COORDS FOR PROPER PMODE
                LDX         ,U              ; GET HOR COORD
                STX         VCB             ; SAVE IT
                LDX         $02,U           ; GET VERT COORD
                STX         VCD             ; SAVE IT
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB73D          ; EVALUATE EXPRESSION RETURN VALUE IN X
                LDU         #VCF            ; POINT U TO TEMP DATA STORAGE
                STX         ,U              ; SAVE RADIUS
                JSR         >L9320          ; NORMALIZE RADIUS
                LDA         #$01            ; SET TO PSET
                STA         SETFLG          ; SAVE PSET/PRESET FLAG
                JSR         >L9581          ; GO EVALUATE COLOR EXPRESSION AND SAVE IN WCOLOR
                LDX         #$100           ; HEIGHT/WIDTH RATIO DEFAULT VALUE
                JSR         GETCCH          ; GET AN INPUT CHARACTER FROM BASIC
                BEQ         L9EDF           ; BRANCH IF NONE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB141          ; EVALUATE EXPRESSION
                LDA         FP0EXP          ; GET FPA0 EXPONENT, ADD 8 TO IT AND RESAVE IT - THIS
                ADDA        #$08            ; WILL EFFECTIVELY MULTIPLY FPA0 BY 256.
                STA         FP0EXP          ;
                JSR         >LB740          ; EVALUATE EXPRESSION, RETURN VALUE IN X
L9EDF           LDA         PMODE           ; GET CURRENT PMODE VALUE
                BITA        #$02            ; TEST FOR PMODE 0,1,4
                BEQ         L9EE9           ; BRANCH IF SO
                TFR         X,D             ; MULT X BY 2 -FOR PMODES 2,3 THE HOR PIXELS ARE 2X AS LONG AS
                LEAX        D,X             ; PMODES 0,1,4; MULT HW RATIO BY 2 TO COMPENSATE
L9EE9           STX         VD1             ; SAVE HW RATIO
                LDB         #$01            ;
                STB         SETFLG          ; SET PSET/PRESET FLAG TO PSET
                STB         VD8             ; FIRST TIME FLAG - SET TO 0 AFTER ARC DRAWN
                JSR         >L9FE2          ; EVALUATE CIRCLE START POINT (OCTANT, SUBARC)
                PSHS        B,A             ; SAVE START POINT
                JSR         >L9FE2          ; EVALUATE CIRCLE END POINT (OCTANT, SUBARC)
                STD         VD9             ; SAVE END POINT
                PULS        A,B             ; GET START POINT
L9EFD           PSHS        B,A             ; STORE CURRENT CIRCLE POSITION
                LDX         HOREND          ; MOVE HOR, VER COORDS FROM HOREND,VEREND TO
                STX         HORBEG          ; HORBEG, VERBEG R MOVE OLD END COORDINATES
                LDX         VEREND          ; NEW START COORDINATES
                STX         VERBEG          ;
                LDU         #L9E79+2        ; POINT TO TABLE OF SINES & COSINES
                ANDA        #$01            ; =GET OCTANT NUMBER
                BEQ         L9F11           ; =BRANCH IF EVEN
                NEGB                        ;
                ADDB        #$08            ; CONVERT 0-7 TO 8-1 FOR ODD OCTANT NUMBERS
L9F11           ASLB                        ;  =
                ASLB                        ;  =FOUR BYTES/TABLE ENTRY
                LEAU        B,U             ; POINT U TO CORRECT TABLE ENTRY
                PSHS        U               ; SAVE SIN/COS TABLE ENTRY
                JSR         >L9FA7          ; CALCULATE HORIZ OFFSET
                PULS        U               ; GET SIN/COS TABLE PTR
                LEAU        -2,U            ; MOVE TO COSINE (VERT)
                PSHS        X               ; SAVE HORIZ OFFSET
                JSR         >L9FA7          ; CALCULATE VERT OFFSET
                PULS        Y               ; PUT HORIZ OFFSET IN Y
                LDA         ,S              ;
                ANDA        #$03            ;
                BEQ         L9F31           ; BRANCH IF OCTANT 0,3,4,7
                CMPA        #$03            ;
                BEQ         L9F31           ; BRANCH IF OCTANT 0,3,4,7
                EXG         X,Y             ; SWAP HOR AND VERT OFFSETS
L9F31           STX         HOREND          ; SAVE HORIZ OFFSET
; THE HW RATIO WILL ONLY MODIFY THE VERT COORD
                TFR         Y,X             ; LOAD X WITH THE CALCULATED VERT OFFSET
                LDD         VD1             ; GET HW RATIO
                JSR         >L9FB5          ; MULT VERT OFFSET BY HW RATIO
                TFR         Y,D             ; TRANSFER THE PRODUCT TO ACCD
                TSTA                        ;  CHECK OVERFLOW FLAG AND GET MSB RESULT
                LBNE        LB44A           ; FC ERROR IF RESULT > 255
                STB         VEREND          ; SAVE DELTA VER MBS
                TFR         U,D             ; LSB RESULT TO ACCA
                STA         VEREND+1        ; SAVE DELTA VER LSB
                LDA         ,S              ;
                CMPA        #$02            ; BRANCH IF OCTANT = 0,1,6,7 (SUBARC HOR END
                BLO         L9F5B           ; POINT >= HOR CENTER)
                CMPA        #$06            ; BRANCH IF OCTANT = 0,1,6,7 (SUSARC HOR END
                BCC         L9F5B           ; POINT >= HOR CENTER)
                LDD         VCB             ; GET HOR COORD OF CENTER
                SUBD        HOREND          ; SUBTRACT HORIZONTAL DIFFERENCE
                BCC         L9F68           ; BRANCH IF NO UNDERFLOW
                CLRA                        ;
                CLRB                        ;  IF NEW HOR < 0, FORCE IT TO BE 0
                BRA         L9F68           ; SAVE NEW COORD
L9F5B           LDD         VCB             ; GET HOR COORD OF CENTER
                ADDD        HOREND          ; ADD HORIZONTAL DIFFERENCE
                BLO         L9F66           ; BRANCH IF OVERFLOW
                CMPD        VD3             ; COMPARE TO MAX HOR COORD
                BLO         L9F68           ; BRANCH IF < MAX HOR
L9F66           LDD         VD3             ; GET MAX HOR COORD
L9F68           STD         HOREND          ; SAVE NEW HORIZ SU8ARC END COORD
                LDA         ,S              ;
                CMPA        #$04            ; BRANCH IF OCTANT = 0,1,2,3 (SUBARC VERT END
                BLO         L9F7A           ; POINT >= VERT CENTER)
                LDD         VCD             ; GET VER COORD OF CENTER
                SUBD        VEREND          ; SUBTRACT VERTICAL DIFFERENCE
                BCC         L9F87           ; BRANCH IF NO UNDERFIOW
                CLRA                        ;
                CLRB                        ;  IF NEW VERT < 0, FORCE IT TO BE 0
                BRA         L9F87           ; SAVE NEW COORD
L9F7A           LDD         VCD             ; GET VER COORD OF CENTER
                ADDD        VEREND          ; ADD VERTICAL DIFFERENCE
                BLO         L9F85           ; BRANCH IF OVERFLOW
                CMPD        VD5             ; COMPARE TO MAX VERT COORD
                BLO         L9F87           ; BRANCH IF < MAX VER
L9F85           LDD         VD5             ; GET MAX VERT COORD
L9F87           STD         VEREND          ; SAVE NEW VERT SUSARC END COORD
                TST         VD8             ; CHECK FIRST TIME FLAG
                BNE         L9F8F           ; DO NOT DRAW A LINE FIRST TIME THRU -
; BECAUSE THE FIRST TIME YOU WOULD DRAW A LINE
; FROM THE CENTER TO THE FIRST POINT ON THE CIRCLE
                BSR         L9FDF           ; DRAW A LINE
L9F8F           PULS        A,B             ; GET END COORDS
                LSR         VD8             ; SHIFT FIRST TIME FLAG
                BLO         L9F9A           ; DO NOT CHECK FOR END POINT AFTER DRAWING FIRST ARC
                CMPD        VD9             ; COMPARE CURRENT POSITION TO END POINT
                BEQ         L9FA6           ; CIRCLE DRAWING FINISHED
; INCREMENT SUBARC CTR, IF > 7 THEN INCR OCTANT CTR
L9F9A           INCB                        ;  INCR SUBARC CTR
                CMPB        #$08            ; > 7?
                BNE         L9FA3           ; NO
                INCA                        ;  INCR OCTANT CTR
                CLRB                        ;  RESET SUBARC CTR
                ANDA        #$07            ; KEEP IN RANGE OF 0-7; ONCE ACCA = B, THIS WILL MAKE ACCA = 0,
; SO THE END POINT WILL BE (0,0) AND THE CIRCLE ROUTINE WILL END.
L9FA3           JMP         >L9EFD          ; KEEP DRAWING CIRCLE
L9FA6           RTS         EXIT            ; CIRCLE ROUTINE
; MULTIPLY RADIUS BY SIN/COS VALUE AND RETURN OFFSET IN X
L9FA7           LDX         VCF             ; GET RADIUS
                LDD         ,U              ; GET SIN/COS TABLE MODIFIER
                BEQ         L9FB4           ; BRANCH IF = 0 - OFFSET = RADIUS
                SUBD        #1              ; SUBTR 1
                BSR         L9FB5           ; MULT RADIUS BY SIN/COS
                TFR         Y,X             ; RETURN RESULT IN X REG
L9FB4           RTS
; MULTIPLY (UNSIGNED) TWO 16 BIT NUMBERS TOGETHER -
; ENTER WITH ONE NUMBER IN ACCD, THE OTHER IN X
; REG. THE 4 BYTE PRODUCT WILL BE STORED IN 4,S-7,S
; (Y, U REG ON THE STACK). I.E. (AA AB) X (XH XL) =
; 256*AA*XH+16*(AA*XL+AB*XH)+AB*XL. THE 2 BYTE
; MULTIPLIER AND MULTIPLICAND ARE TREATED AS A 1
; BYTE INTEGER PART (MSB) WITH A 1 BYTE FRACTIONAL PART (LSB)
L9FB5           PSHS        U,Y,X,B,A       ; SAVE REGISTERS AND RESERVE STORAGE SPACE ON THE STACK
                CLR         $04,S           ; RESET OVERFLOW FLAG
                LDA         $03,S           ; =
                MUL                         ;  =
                STD         $06,S           ; CALCULATE ACCB*XL, STORE RESULT IN 6,S
                LDD         $01,S           ;
                MUL                         ;  CALCULATE ACCB*XH
                ADDB        $06,S           ; =
                ADCA        #$00            ; =
                STD         $05,S           ; ADD THE CARRY FROM THE 1ST MUL TO THE RESULT OF THE 2ND MUL
                LDB         ,S              ;
                LDA         $03,S           ;
                MUL                         ;  CALCULATE ACCA*XL
                ADDD        $05,S           ; =
                STD         $05,S           ; ADD RESULT TO TOTAL OF 2 PREVIOUS MULTS
                BCC         L9FD4           ; BRANCH IF NO OVERFLOW
                INC         $04,S           ; SET OVERFLOW FLAG (ACCD > $FFFF)
L9FD4           LDA         ,S              ;
                LDB         $02,S           ;
                MUL                         ;  CALCULATE ACCA*XH
                ADDD        $04,S           ; =
                STD         $04,S           ; ADD TO PREVIOUS RESULT
                PULS        A,B,X,Y,U,PC    ; RETURN RESULT IN U,Y
L9FDF           JMP         >L94A1          ; GO DRAW A LINE FROM (HORBEG,VERBEG) TO (HOREND,VEREND)
; CALCULATE START OR END POINT WHICH IS A NUMBER FROM
; 0 TO 63 SAVED AS AN OCTANT NUMBER (0-7) AND A SUBARC NUMBER (0-7)
L9FE2           CLRB                        ;  DEFAULT VALUE OF ZERO
                JSR         GETCCH          ; GET CURRENT INPUT CHAR
                BEQ         L9FF8           ; BRANCH IF NONE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                JSR         >LB141          ; EVALUATE NUMERIC EXPRESSION
                LDA         FP0EXP          ; GET EXPONENT OF FPA0
                ADDA        #$06            ; ADD 6 TO EXPONENT - MULTIPLY EXPONENT BY 64
                STA         FP0EXP          ; RESAVE EXPONENT
                JSR         >LB70E          ; CONVERT FPA0 TO INTEGER IN ACCB
                ANDB        #$3F            ; MAX VALUE OF 63
L9FF8           TFR         B,A             ; SAVE VALUE IN ACCA ALSO
                ANDB        #$07            ; NOW ACCB CONTAINS SUBARC NUMBER
                LSRA                        ;
                LSRA                        ;
                LSRA                        ;  DIVIDE ACCA BY EIGHT - OCTANT NUMBER
                RTS
