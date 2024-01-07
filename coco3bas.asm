; ===========================================================================================================
; Extended Color BASIC 1.1
; Copied from the PDF version of Extended Color BASIC Unravelled.
; Fixed up to assemble in Mamou

                ORG         EXBAS
MAGIC           FCC         "EX"

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
XBWMST          FCB         $ff

                CLR         PLYTMR
                CLR         PLYTMR+1        ; CLEAR PLAY TIMER
                LDA         PIA0+3          ; ENABLE PIA0 TO
                ORA         #$01            ; PASS 60HZ
                STA         PIA0+3          ; INTERRUPT TO MPU
                JMP         BAWMST          ; JUMP TO BASICS WARM START



; THIS CODE IS A PATCH TO FIX THE PCLEAR BUG
L80D0           LDA         CURLIN          ; GET THE CURRENT LINE NUMBER
                INCA                        ;  TEST FOR DIRECT MODE
                BEQ         L80DD           ; RETURN IF DIRECT MODE
                TFR         Y,D             ; SAVE OFFSET IN ACCD
                SUBD        TXTTAB          ; SUBTRACT OUT START OF BASIC
                ADDD        CHARAD          ; ADD THE CURRENT BASIC INPUT POINTER
                STD         CHARAD          ; SAVE NEW BASIC INPUT POINTER
L80DD           RTS



L80DE           FCB         25              ; 25 EXBAS COMMANDS
L80DF           FDB         L8183           ; EXBAS RESERVED WORD DICTIONARY TABLE
L80E1           FDB         L813C           ; EXBAS RESERVED WORD HANDLER
L80E3           FCB         14              ; 14 EXBAS SECONDARY COMMANDS
L80E4           FDB         L821E           ; EXBAS SECONDARY RESERVED WORD TABLE
L80E6           FDB         L8168           ; EXBAS SECONDARY RESERVED WORD HANDLER
L80E8           FCC         "EXTENDED COLOR BASIC "
                FCC         "2.0"           ; MINOR VERSION NUMBER
L8100           FCB         CR
L8101           FCC         "COPR. 1982, 1986 BY TANDY  " ; COPYRIGHT YEAR
L811C           FCB         CR
L811D           FCC         "UNDER LICENSE FROM MICROSOFT"
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
L8183           FCS         "DEL"           ; B5
L8186           FCS         "EDIT"          ; B6
L818A           FCS         "TRON"          ; B7
L818E           FCS         "TROFF"         ; B8
L8193           FCS         "DEF"           ; B9
L8196           FCS         "LET"           ; BA
L8199           FCS         "LINE"          ; BB
L819D           FCS         "PCLS"          ; BC
L81A1           FCS         "PSET"          ; BD
L81A5           FCS         "PRESET"        ; BE
L81AB           FCS         "SCREEN"        ; BF
L81B1           FCS         "PCLEAR"        ; C0
L81B7           FCS         "COLOR"         ; C1
L81BC           FCS         "CIRCLE"        ; C2
L81C2           FCS         "PAINT"         ; C3
L81C7           FCS         "GET"           ; C4
L81CA           FCS         "PUT"           ; C5
L81CD           FCS         "DRAW"          ; C6
L81D1           FCS         "PCOPY"         ; C7
L81D6           FCS         "PMODE"         ; C8
L81DB           FCS         "PLAY"          ; C9
L81DF           FCS         "DLOAD"         ; CA
L81E4           FCS         "RENUM"         ; CB
L81E9           FCS         "FN"            ; CC
L81EB           FCS         "USING"         ; CD
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
L821E           FCS         "ATN"           ; 94
L8221           FCS         "COS"           ; 95
L8224           FCS         "TAN"           ; 96
L8227           FCS         "EXP"           ; 97
L822A           FCS         "FIX"           ; 98
L822D           FCS         "LOG"           ; 99
L8230           FCS         "POS"           ; 9A
L8233           FCS         "SQR"           ; 9B
L8236           FCS         "HEX$"          ; 9C
L823A           FCS         "VARPTR"        ; 9D
L8240           FCS         "INSTR"         ; 9E
L8245           FCS         "TIMER"         ; 9F
L824A           FCS         "PPOINT"        ; A0
L8250           FCS         "STRING$"       ; A1
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
                CMPA        #':'            ; CHECK FOR COLON
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
                CMPA        #'M'            ; CHECK FOR CSAVEM
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
                SUBA        #'0'            ; MASK OFF ASCII
                PSHS        A               ; SAVE IT ON STACK
                LDA         #10             ; NUMBER BEING CONVERTED IS BASE 10
                MUL                         ;  MULTIPLY ACCUMULATED VALUE BY BASE (10)
                ADDB        ,S+             ; ADD DIGIT TO ACCUMULATED VALUE
                BRA         L855D           ; CHECK FOR ANOTHER DIGIT
L8570           SUBB        #$01            ; REPEAT PARAMETER IN ACCB; IF IT
                ADCB        #$01            ; IS 0, THEN MAKE IT 1
                CMPA        #'A'            ; ABORT?
                BNE         L857D           ; NO
                JSR         >LB958          ; PRINT CARRIAGE RETURN TO SCREEN
                BRA         L8538           ; RESTART EDIT PROCESS - CANCEL ALL CHANGES
L857D           CMPA        #'L'            ; LIST?
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
L859D           CMPA        #'E'            ; EXIT?
                BEQ         L8592           ; YES - SAME AS ENTER EXCEPT NO ECHO
                CMPA        #'Q'            ; QUIT?
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
L85C3           CMPA        #'D'            ; DELETE?
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
L85DE           CMPA        #'I'            ; INSERT?
                BEQ         L85F5           ; YES
                CMPA        #'X'            ; EXTEND?
                BEQ         L85F3           ; YES
                CMPA        #'H'            ; HACK?
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
L860F           CMPA        #'C'            ; CHANGE?
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
L865C           CMPA        #'K'            ; KILL?
                BEQ         L8665           ; YES
                SUBA        #'S'            ; SEARCH?
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
                CMPA        #')'            ; CHECK FOR END OF MID$ STATEMENT AND
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
XVEC19          CMPA        #'&'
L87E7           BNE         L8845           ; RETURN IF NOT HEX OR OCTAL VARIABLE
                LEAS        $02,S           ; PURGE RETURN ADDRESS FROM STACK
; PROCESS A VARIABLE PRECEEDED BY A & (&H,&O)
L87EB           CLR         FPA0+2          ; CLEAR BOTTOM TWO
                CLR         FPA0+3          ; BYTES OF FPA0
                LDX         #FPA0+2         ; BYTES 2,3 OF FPA0 = (TEMPORARY ACCUMULATOR)
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC
                CMPA        #'O'            ; OCTAL VALUE?
                BEQ         L880A           ; YES
                CMPA        #'H'            ; HEX VALUE?
                BEQ         L881F           ; YES
                JSR         GETCCH          ; GET CURRENT INPUT CHARACTER
                BRA         L880C           ; DEFAULT TO OCTAL (&O)
L8800           CMPA        #'8'
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
                CMPA        #'G'            ; CHECK FOR LETTERS A-F
                BCC         L880E           ; BRANCH IF >= G (ILLEGAL HEX LETTER)
                SUBA        #'A'-('9'+1)    ; SUBTRACT ASCII DIFFERENCE BETWEEN A AND 9
L882E           LDB         #$04            ; BASE 16 DIGIT MULTIPLIER = 2**4
                BSR         L8834           ; ADD DIGIT TO TEMPORARY ACCUMULATOR
                BRA         L881F           ; KEEP EVALUATING VARIABLE
L8834           ASL         $01,X           ; MULTIPLY TEMPORARY
                ROL         ,X              ; ACCUMULATOR BY TWO
                LBCS        LBA92           ; OV' OVERFLOW ERROR
                DECB                        ;  DECREMENT SHIFT COUNTER
                BNE         L8834           ; MULTIPLY TEMPORARY ACCUMULATOR AGAIN
L883F           SUBA        #'0'            ; MASK OFF ASCII
                ADDA        $01,X           ; ADD DIGIT TO TEMPORARY
                STA         $01,X           ; ACCUMULATOR AND SAVE IT
L8845           RTS
; EXPRESSION EVALUATION RAM HOOK
XVEC15          PULS        U               ; PULL RETURN ADDRESS AND SAVE IN U REGISTER
                CLR         VALTYP          ; SET VARIABLE TYPE TO NUMERIC
                LDX         CHARAD          ; CURRENT INPUT POINTER TO X
                JSR         GETNCH          ; GET CHARACTER FROM BASIC
                CMPA        #'&'            ; HEX AND OCTAL VARIABLES ARE PRECEEDED BY &
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
L890B           FCC         "UF"            ; 25 UNDEFINED FUNCTION (FN) CALL
L890D           FCC         "NE"            ; 26 FILE NOT FOUND
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
                SUBA        #'0'            ; MASK OFF ASCII
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
L8943           RTS                         ; JUMP TO USR ROUTINE (PSHS X ABOVE)
L8944           LDB         #$B3            ; TOKEN FOR =
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR =
                JMP         >LB73D          ; EVALUATE EXPRESSION, RETURN VALUE IN X
; EXTENDED BASICS IRQ ROUTINE
XIRQSV          LDA         PIA0+3          ; GET PIA0, PORT B CONTROL REGISTER
                BMI         L8952           ; BRANCH IF 60 HZ INTERRUPT
                RTI                         ; RETURN IF 63.5 MICROSECOND INTERRUPT
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
                CMPA        #'#'            ; CHECK FOR DEVICE NUMBER FLAG AND
                BNE         L89D2           ; BRANCH IF NOT THERE
                JSR         >LA5A5          ; CHECK FOR VALID DEVICE NUMBER
                JSR         >LA3ED          ; CHECK FOR OPEN FILE
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
L89D2           CMPA        #'"'            ; CHECK FOR PROMPT STRING
                BNE         L89E1           ; BRANCH IF NO PROMPT STRING
                JSR         >LB244          ; STRIP OFF PROMPT STRING & PUT IT ON STRING STACK
                LDB         #';'
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
                CMPA        #','            ; IS IT A COMMA?
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
L8BD9           FCC         "UL "           ; UNKNOWN LINE NUMBER MESSAGE
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
L8BFF           ADDB        #'0'            ; ADD IN ASCII ZERO
                CMPB        #'9'            ; COMPARE TO ASCII 9
                BLS         L8C07           ; BRANCH IF < 9
                ADDB        #'A'-('9'+1)    ; ADD ASCII OFFSET IF HEX LETTER
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
                TSTB                        ;  CHECK ASCII FLAG
                BEQ         L8C5F           ; FM ERROR IF NOT ASCII
                JSR         >LAD19          ; GO DO A NEW
                JMP         >LAC7C          ; JUMP BACK TO BASICS MAIN INPUT LOOP;
; DLOAD FILES MUST BE ASCII FILES
L8C5F           JMP         >LA616          ; BAD FILE MODE ERROR
; EXBAS CLOAD PROCESSOR
L8C62           JSR         GETNCH          ; GET A CHAR FROM BASIC
                CMPA        #'M'            ; CHECK FOR CLOADM
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
                CMPA        #'%'            ; COMPARE TO TERMINATOR CHARACTER
                BEQ         L8E3B           ; BRANCH IF END OF SPACES COMMAND
                CMPA        #$20            ; BLANK SPACE
                BNE         L8E82           ; BRANCH IF ILLEGAL CHARACTER
                INC         VD9             ; ADD ONE TO SPACES COUNTER
                LEAX        $01,X           ; MOVE FORMAT POINTER UP ONE
                DECB                        ;  DECREMENT LENGTH COUNTER
                BNE         L8E71           ; BRANCH IF NOT END OF FORMAT STRING
L8E82           LDX         TEMPTR          ; RESTORE CURRENT FORMAT STRING COUNTER
                LDB         VD3             ; AND POINTER TO POSITION BEFORE SPACES COMMAND
                LDA         #'%'            ; SEND A % TO CONSOLE OUT AS A DEBUGGING AID
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
                LDB         #';'            ; CHECK FOR ITEM LIST SEPARATOR
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
                CMPA        #'!'            ; EXCLAMATION POINT?
                LBEQ        L8E37           ; YES - STRING TYPE FORMAT
                CMPA        #'#'            ; NUMBER SIGN? (DIGIT LOCATOR)
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
L8EE2           CMPA        #'+'            ; CHECK FOR + (PRE-SIGN FORCE)
                BNE         L8EEF           ; NO PLUS
                JSR         >L8FD8          ; SEND A +' TO CONSOLE OUT IF VDA <> 0
                LDA         #$08            ; LOAD THE STATUS BYTE WITH 8;
                STA         VDA             ; PRE-SIGN FORCE FLAG
                BRA         L8EBB           ; INTERPRET THE REST OF THE FORMAT STRING
L8EEF           CMPA        #'.'            ; DECIMAL POINT?
                BEQ         L8F41           ; YES
                CMPA        #'%'            ; PERCENT SIGN?
                LBEQ        L8E69           ; YES
                CMPA        ,X              ; COMPARE THE PRESENT FORMAT STRING INPUT
; CHARACTER TO THE NEXT ONE IN THE STRING
L8EFB           BNE         L8E88           ; NO MATCH - ILLEGAL CHARACTER
; TWO CONSECUTIVE EQUAL CHARACTERS IN FORMAT STRING
                CMPA        #'$'            ; DOLLAR SIGN?
                BEQ         L8F1A           ; YES - MAKE THE DOLLAR SIGN FLOAT
                CMPA        #'*'            ; ASTERISK?
                BNE         L8EFB           ; NO - ILLEGAL CHARACTER
                LDA         VDA             ; GRAB THE STATUS BYTE AND BET BIT 5
                ORA         #$20            ; TO INDICATE THAT THE OUTPUT WILL
                STA         VDA             ; BE LEFT PADDED WITH ASTERISKS
                CMPB        #2              ; CHECK TO SEE IF THE $$ ARE THE LAST TWO
                BLO         L8F20           ; CHARACTERS IN THE FORMAT STRING AND BRANCH IF SO
                LDA         $01,X           ; GET THE NEXT CHARACTER AFTER **
                CMPA        #'$'            ; CHECK FOR **$
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
                CMPA        #'.'            ; DECIMAL POINT?
                BEQ         L8F4F           ; YES
                CMPA        #'#'            ; NUMBER SIGN?
                BEQ         L8F26           ; YES
                CMPA        #','            ; COMMA?
                BNE         L8F5A           ; NO
                LDA         VDA             ; GET THE STATUS BYTE
                ORA         #$40            ; AND SET BIT 6 WHICH IS THE
                STA         VDA             ; COMMA SEPARATOR FLAG
                BRA         L8F26           ; PROCESS MORE CHARACTERS TO LEFT OF DECIMAL POINT
; PROCESS DECIMAL POINT IF NO DIGITS TO LEFT OF IT
L8F41           LDA         ,X              ; GET NEXT FORMAT CHARACTER
                CMPA        #'#'            ; IS IT A NUMBER SIGN?
                LBNE        L8E88           ; NO
                LDA         #1              ; SET THE RIGHT DIGIT COUNTER TO 1 -
                STA         VD8             ; ALLOW ONE SPOT FOR DECIMAL POINT
                LEAX        $01,X           ; MOVE FORMAT POINTER UP ONE
; PROCESS DIGITS TO RIGHT OF DECIMAL POINT
L8F4F           INC         VD8             ; ADD ONE TO RIGHT DIGIT COUNTER
                DECB                        ;  DECREMENT FORMAT LENGTH COUNTER
                BEQ         L8F74           ; BRANCH IF END OF FORMAT STRING
                LDA         ,X+             ; GET A CHARACTER FROM FORMAT STRING
                CMPA        #'#'            ; IS IT NUMBER SIGN?
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
                SUBA        #'-'            ; CHECK FOR MINUS SIGN
                BEQ         L8F8F           ; BRANCH IF MINUS SIGN
                CMPA        #('+')-('-')    ; CHECK FOR PLUS SIGN
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
                CMPA        #';'            ; CHECK FOR ; - ITEM-LIST SEPARATOR AND
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
                LDA         #'+'            ; GET ASCII PLUS SIGN
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
                LDB         #'+'            ; PLUS SIGN
L8FF2           TST         FP0SGN          ; CHECK THE SIGN OF FPA0
                BPL         L8FFA           ; BRANCH IF POSITIVE
                CLR         FP0SGN          ; FORCE FPA0 SIGN TO BE POSITIVE
                LDB         #'-'            ; MINUS SIGN
L8FFA           STB         ,U+             ; SAVE THE SIGN IN BUFFER
                LDB         #'0'            ; PUT A ZERO INTO THE BUFFER
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
                LDA         #'%'            ; INSERT A % SIGN AT START OF
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
                CMPB        #'*'            ; IS THE FIRST CHARACTER AN $?
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
                CMPA        #'*'            ; ASTERISK?
                BEQ         L9065           ; YES - ADVANCE POINTER
                CLRA                        ;  A ZERO ON THE STACK IS END OF DATA POINTER
L907C           PSHS        A               ; PUSH A CHARACTER ONTO THE STACK
                LDA         ,X+             ; GET NEXT CHARACTER FROM BUFFER
                CMPA        #'-'            ; MINUS SIGN?
                BEQ         L907C           ; YES
                CMPA        #'+'            ; PLUS SIGN?
                BEQ         L907C           ; YES
                CMPA        #'$'            ; DOLLAR SIGN?
                BEQ         L907C           ; YES
                CMPA        #'0'            ; ZERO?
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
                LDA         #'%'            ; PUT A % SIGN BEFORE THE ERROR POSITION TO
                STA         ,-X             ; INDICATE AN ERROR
L90A9           RTS

; CLEAR CARRY IF NUMERIC
L90AA           CMPA        #'0'            ; ASCII ZERO
                BLO         L90B2           ; RETURN IF ACCA < ASCII 0
                SUBA        #'9'+1          ;
                SUBA        #-('9'+1)       ; CARRY CLEAR IF NUMERIC
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
                LDA         #'0'            ; INSERT A ZERO INTO
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
L9167           LDA         #'+'            ; PLUS SIGN
                TSTB                        ;  TEST EXPONENT
                BPL         L916F           ; BRANCH IF POSITIVE EXPONENT
                LDA         #'-'            ; MINUS SIGN
                NEGB                        ;  CONVERT EXPONENT TO POSITIVE NUMBER
L916F           STA         $01,U           ; PUT SIGN OF EXPONENT IN STRING BUFFER
                LDA         #'E'            ; PUT AN E (EXPONENTIATION FLAG) IN
                STA         ,U++            ; BUFFER AND SKIP OVER THE SIGN
                LDA         #'0'-1          ; INITIALIZE TENS DIGIT TO ASCII ZERO MINUS ONE
; BINARY EXPONENT IN ACCB TO ASCII VALUE IN ACCA
L9177           INCA                        ;  ADD ONE TO TENS DIGIT COUNTER


; fix minor bug in the ascii to floating point conversion in PRINT USING

                SUBB        #10             ; SUBTRACT 10 FROM EXPONENT AND ADD ONE TO TENS
                BCC         L9177           ; DIGIT IF NO CARRY. TENS DIGIT DONE IF THERE IS A CARRY
                ADDB        #'9'+1          ; ADD ASCII BIAS TO UNITS DIGIT
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
                LDA         #'*'            ; PAD WITH ASTERISK
                CMPB        #SPACE          ; WAS THE FIRST BYTE A BLANK (POSITIVE)?
                BNE         L919E           ; NO
                TFR         A,B             ; TRANSFER PAD CHARACTER TO ACCB
L919E           PSHS        B               ; SAVE FIRST CHARACTER ON STACK
L91A0           STA         ,X+             ; STORE PAD CHARACTER IN BUFFER
                LDB         ,X              ; GET NEXT CHARACTER IN BUFFER
                BEQ         L91B6           ; INSERT A ZERO IF END OF BUFFER
                CMPB        #'E'            ; CHECK FOR AN E AND
                BEQ         L91B6           ; PUT A ZERO BEFORE IT
                CMPB        #'0'            ; REPLACE LEADING ZEROES WITH
                BEQ         L91A0           ; PAD CHARACTERS
                CMPB        #','            ; REPLACE LEADING COMMAS
                BEQ         L91A0           ; WITH PAD CHARACTERS
                CMPB        #'.'            ; CHECK FOR DECIMAL POINT
                BNE         L91BA           ; AND DONT PUT A ZERO BEFORE IT
L91B6           LDA         #'0'            ; REPLACE PREVIOUS CHARACTER
                STA         ,-X             ; WITH A ZERO
L91BA           LDA         VDA             ; GET STATUS BYTE, CHECK
                BITA        #$10            ; FOR FLOATING $
                BEQ         L91C4           ; BRANCH IF NO FLOATING $
                LDB         #'$'            ; STORE A $ IN
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
L9235           ADDB        #'0'-1          ; ADD IN ASCII OFFSET
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
                LDA         #'.'            ; STORE A DECIMAL
                STA         ,U+             ; POINT IN THE OUTPUT BUFFER
                CLR         VD7             ; =CLEAR COMMA COUNTER - NOW IT WILL TAKE 255
; =DECREMENTS BEFORE ANOTHER COMMA WILL BE INSERTED
                RTS
L9256           DEC         VD7             ; DECREMENT COMMA COUNTER
                BNE         L9262           ; RETURN IF NOT TIME FOR COMMA
                LDA         #$03            ; RESET COMMA COUNTER TO 3; THREE
                STA         VD7             ; DIGITS BETWEEN COMMAS
                LDA         #','            ; PUT A COMMA INTO
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
                LDA         #'0'            ; PUT A ZERO INTO
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
                CMPA        #'('            ; CHECK FOR (
                BEQ         L93CE           ; GO LOOK FOR START AND END POINTS
                CMPA        #$AC            ; CHECK TOKEN FOR MINUS SIGN
                BEQ         L93CE           ; GO GET START AND END POINTS
                LDB         #'@'            ; CHECK FOR @ SIGN
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
                LDB         #'B'            ; BOX?
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
L9429           LDB         #'F'            ;
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
L9443           RTS                         ; WASTED BYTE - SHOULD USE L946B INSTEAD
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
COLOR           CMPA        #','            ; CHECK FOR COMMA AND
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
                CMPA        #')'            ; CHECK FOR ) AND BRANCH IF
                BEQ         L9598           ; NO MORE ARGUMENTS
                JSR         SYNCOMMA        ; SYNTAX CHECK FOR COMMA
                CMPA        #','            ; WAS NEXT CHARACTER A COMMA?
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
PMODETOK        CMPA        #','            ; CHECK FOR COMMA - FIRST ARGUMENT MAY BE MISSING
                BEQ         L9650           ; IT IS A COMMA
                JSR         EVALEXPB        ; EVALUATE EXPRESSION
                CMPB        #$05            ; > 4?
                BCC         L966D           ; YES, ILLEGAL FUNCTION CALL


                LDA         GRPRAM          ; GET THE START OF GRAPHIC RAM


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
SCREEN          CMPA        #','            ; CHECK FOR A COMMA
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
L975F           CMPA        #'@'            ; CHECK FOR @ SIGN
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
                LDB         #'G'            ; CHECK FOR FULL GRAPHIC OPTION
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
                RTS                         ; RETURN FROM GET/PUT COMMAND
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
                RTS                         ; WASTED BYTE
L98EB           RTS
; PAINT
PAINT           CMPA        #'@'            ; CHECK FOR @ SIGN
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
                CMPA        #';'            ; SUB COMMAND TERMINATED
                BEQ         L9A43           ; IGNORE SEMICOLONS
                CMPA        #''             ; CHECK FOR APOSTROPHE
                BEQ         L9A43           ; IGNORE THEM TOO
                CMPA        #'X'            ; CHECK FOR AN EXECUTABLE SUBSTRING
                LBEQ        L9C0A           ; GO PROCESS SUB COMMAND
                BSR         L9A5C           ; CHECK FOR OTHER COMMANDS
                BRA         L9A43           ; KEEP GOING THROUGH INTERPRETATION LOOP
; OCTAVE
L9A5C           CMPA        #'O'            ; ADJUST OCTAVE?
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
L9A6D           CMPA        #'V'            ; ADJUST VOLUME?
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
L9A8B           CMPA        #'L'            ; SET NOTE LENGTH?
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
                CMPA        #'.'            ; CHECK FOR DOTTED NOTE
                BEQ         L9AAF           ; BRANCH ON DOTTED NOTE AND CLEAR CARRY FLAG
                JSR         >L9BE2          ; MOVE COMMAND STRING POINTER BACK ONE AND ADD ONE TO
; LENGTH
L9AAD           COMA                        ;  SET CARRY FLAG
                RTS
L9AAF           INC         DOTVAL          ; ADD ONE TO NOTE TIMER SCALE FACTOR
                RTS
; TEMPO
L9AB2           CMPA        #'T'            ; MODIFY TEMPO?
                BNE         L9AC3           ; NO
                LDB         TEMPO           ; GET CURRENT TEMPO
                BSR         L9AC0           ; EVALUATE MODIFIER
                TSTB                        ;  SET FLAGS
                BEQ         L9ACD           ; FC ERROR IF ITS 0
                STB         TEMPO           ; SAVE NEW TEMPO
                RTS
L9AC0           JMP         >L9BAC          ; EVALUATE THE >,<,+,-,= OPERATORS
; PAUSE
L9AC3           CMPA        #'P'            ; PAUSE COMMAND?
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
L9AEB           CMPA        #'N'            ; LETTER N BEFORE THE NUMBER OF A NOTE?
                BNE         L9AF2           ; NO - ITS OPTIONAL
                JSR         >L9B98          ; GET NEXT COMMAND CHARACTER
L9AF2           CMPA        #'A'            ; CHECK FOR NOTE A
                BLO         L9AFA           ; BELOW
                CMPA        #'G'            ; CHECK FOR NOTE B
                BLS         L9AFF           ; FOUND NOTE A-G
L9AFA           JSR         >L9BBE          ; EVALUATE DECIMAL NUMERIC EXPRESSION IN COMMAND STRING
                BRA         L9B22           ; PROCESS NOTE VALUE
; PROCESS A NOTE HERE
L9AFF           SUBA        #'A'            ; MASK OFF ASCII
                LDX         #L9C5B          ; LOAD X WITH NOTE JUMP TABLE
                LDB         A,X             ; GET NOTE
                TST         VD8             ; ANY COMMAND CHARACTERS LEFT?
                BEQ         L9B22           ; NO
                JSR         >L9B98          ; GET COMMAND CHARACTER
                CMPA        #'#'            ; SHARP NOTE?
                BEQ         L9B15           ; YES
                CMPA        #'+'            ; SHARP NOTE?
                BNE         L9B18           ; NO
L9B15           INCB                        ;  ADD 1 TO NOTE NUMBER (SHARP)
                BRA         L9B22           ; PROCESS NOTE
L9B18           CMPA        #'-'            ; FLAT NOTE?
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
                CMPA        #'+'            ; ADD ONE?
                BEQ         L9BEE           ; YES
                CMPA        #'-'            ; SUBTRACT ONE?
                BEQ         L9BF2           ; YES
                CMPA        #'>'            ; MULTIPLY BY TWO?
                BEQ         L9BFC           ; YES
                CMPA        #'<'            ; DIVIDE BY TWO?
                BEQ         L9BF7           ; YES
L9BBE           CMPA        #'='            ; CHECK FOR VARIABLE EQUATE - BRANCH IF SO; ACCB WILL BE
                BEQ         L9C01           ; SET TO THE VALUE OF THE BASIC VARIABLE IN THE COMMAND
; STRING WHICH MUST BE NUMERIC, LESS THAN 256
; AND THE VARIABLE MUST BE FOLLOWED BY A SEMICOLON.
                JSR         >L90AA          ; CLEAR CARRY IF NUMERIC
                BLO         L9BEB           ; FC ERROR IF NON NUMERIC
                CLRB                        ;  UNITS DIGIT = 0
; STRIP A DECIMAL ASCII VALUE OFF OF THE COMMAND STRING
; AND RETURN BINARY VALUE IN ACCB
L9BC8           SUBA        #'0'            ; MASK OFF ASCII
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
                CMPA        #';'            ; CHECK FOR SEMICOLON - COMMAND SEPARATOR
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
L9C5A           RTI                         ; RETURN
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
                CMPA        #';'            ; CHECK FOR SEMICOLON
                BEQ         L9CDD           ; IGNORE SEMICOLONS
                CMPA        #''             ; CHECK FOR APOSTROPHES
                BEQ         L9CDD           ; IGNORE APOSTROPHES
                CMPA        #'N'            ; UPDATE CHECK?
                BNE         L9CF4           ; NO
                COM         VD5             ; TOGGLE UPDATE FLAG 0 = UPDATE, FF = NO UPDATE
                BRA         L9CDD           ; GET NEXT COMMAND
L9CF4           CMPA        #'B'            ; CHECK DRAW FLAG?
                BNE         L9CFC           ; NO
                COM         VD6             ; TOGGLE DRAW FLAG 0 = DRAW LINE, FF = DONT DRAW LINE
                BRA         L9CDD           ; GET NEXT COMMAND
L9CFC           CMPA        #'X'            ; SUBSTRING?
                LBEQ        L9D98           ; GO EXECUTE A COMMAND
                CMPA        #'M'            ; MOVE THE DRAW POSITION ?
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
                CMPA        #'C'            ; CHANGE COLOR?
                BEQ         L9D4F           ; YES
                CMPA        #'A'            ; CHANGE ANGLE?
                BEQ         L9D59           ; YES
                CMPA        #'S'            ; CHANGE SCALE?
                BEQ         L9D61           ; YES
                CMPA        #'U'            ; GO UP?
                BEQ         L9D8F           ; YES
                CMPA        #'D'            ; GO DOWN?
                BEQ         L9D8C           ; YES
                CMPA        #'L'            ; GO LEFT?
                BEQ         L9D87           ; YES
                CMPA        #'R'            ; GO RIGHT?
                BEQ         L9D82           ; YES
                SUBA        #'E'            ; MASK OFF ASCII FOR LETTER E-H COMMAND CHECKS
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
                CMPA        #','            ; CHECK FOR COMMA
                LBNE        L9D4C           ; FC ERROR IF NO COMMA
                JSR         >L9E5B          ; EVALUATE VERT DIFFERENCE
                TFR         D,X             ; SAVE VERT DIFFERENCE IN X
                PULS        U               ; GET HORIZ DIFFERENCE IN U
                PULS        A               ; GET FIRST COMMAND CHARACTER
                CMPA        #'+'            ; IF FIRST COMMAND CHAR WAS EITHER + OR -, TREAT
                BEQ         L9E56           ; THE VALUES IN U & X AS DIFFERENCES AND MOVE
                CMPA        #'-'            ; POINTER, OTHERWISE TREAT U & X AS AN ABSOLUTE
                BNE         L9DFC           ; POSITION AND MOVE THE CURRENT POSITION THERE.
L9E56           TFR         U,D             ; PUT HORIZ DIFFERENCE IN ACCD
                JMP         >L9DCB          ; GO MOVE THE DRAW POSITION
L9E5B           JSR         >L9B98          ; GET A CHAR FROM COMMAND LINE
L9E5E           CMPA        #'+'            ; CHECK FOR A LEADING PLUS SIGN (RELATIVE MOTION)
                BEQ         L9E69           ; AND BRANCH IF RELATIVE
                CMPA        #'-'            ; CHECK FOR A LEADING MINUS SIGN (RELATIVE MOTION)
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
CIRCLE          CMPA        #'@'            ; CHECK FOR @ SIGN
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
L9FA6           RTS                         ; EXIT CIRCLE ROUTINE
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

; ===========================================================================================================
; ------------------------------------------
; Color BASIC ( v1.0, v1.1, v1.2, and v1.3 )
; ------------------------------------------

; Copied from the PDF version of Color BASIC Unravelled.
; Fixed up to assemble in Mamou


                ORG         BASIC
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
RESVEC          LEAY        <LA00E,PCR      ; POINT Y TO WARM START CHECK CODE
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


                JSR         >LAD19          ; G0 DO A NEW

                JMP         >$8002          ; CPYROM copy rom to ram (coco3)
                PSHS        X,B
                TST         HRWIDTH
                LBNE        $F77E           ; ALINK24
LA0D6           JSR         >LA199
                JSR         >KEYIN
                BEQ         LA0D6
                JMP         >LA1B9
                FCB         $72

LA0E2           LDA         #$55            ; WARM START FLAG
                STA         RSTFLG          ; SAVE IT
                BRA         LA0F3           ; GO TO BASICS MAIN LOOP
BAWMST          NOP                         ;  NOP REQD FOR WARM START
                CLR         DEVNUM          ; SET DEVICE NUMBER TO SCREEN
                JSR         >LAD33          ; DO PART OF A NEW
                ANDCC       #$AF            ; ENABLE IRQ,FIRQ
                JSR         >CLS            ; CLEAR SCREEN


LA0F3           JMP         >LAC73          ; GO TO MAIN LOOP OF BASIC

; FIRQ SERVICE ROUTINE
BFRQSV          TST         PIA1+3          ; CARTRIDGE INTERRUPT?
                BMI         LA0FC           ; YES
                RTI


LA0FC           JSR         >$8C28          ; ?


                JSR         >LA7D1          ; KEEP DELAYING



LA102           LEAY        <LA108,PCR      ; Y = ROM-PAK START UP VECTOR
                JMP         >LA02A          ; GO DO INITIALIZATION
LA108           CLR         RSTFLG          ; CLEAR WARM START FLAG
                JMP         >ROMPAK         ; JUMP TO EXTERNAL ROM PACK

; THESE BYTES ARE MOVED TO ADDRESSES $8F - $AA THE DIRECT PAGE
LA10D           FCB         18              ; MID BAND PARTITION OF 1200/2400 HERTZ PERIOD
                FCB         24              ; UPPER LIMIT OF 1200 HERTZ PERIOD
                FCB         10              ; UPPER LIMIT OF 2400 HERTZ PERIOD
                FDB         128             ; NUMBER OF 55S TO CASSETTE LEADER
                FCB         11              ; CURSOR BLINK DELAY
                FDB         $58             ; CONSTANT FOR 600 BAUD VER 1.2 & UP
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
LA147           FCC         "COLOR BASIC "
                FCC         "1.2"           ; for some reason, coco3 has "1.2" in there
LA156           FCB         CR
LA157           FCC         "(C) 198"
                FCC         "2"
                FCC         " TANDY"
LA165           FCB         $00
LA166           FCC         "MICROSOFT"
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
                CMPB        #26             ; WAS IT < 26 (A LETTER)?
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
                RTS                         ; RETURN
; READ THE KEYBOARD
LA238           STB         2,U             ; SAVE NEW COLUMN STROBE VALUE
LA23A           LDA         ,U              ; READ PIA0, PORT A TO SEE IF KEY IS DOWN
; A BIT WILL BE ZERO IF ONE IS
                ORA         #$80            ; MASK OFF THE JOYSTICK COMPARATOR INPUT
                TST         $02,U           ; ARE WE STROBING COLUMN 7?
                BMI         LA244           ; NO
                ORA         #$C0            ; YES, FORCE ROW 6 TO BE HIGH - THIS WILL CAUSE
; THE SHIFT KEY TO BE IGNORED
LA244           RTS                         ; RETURN
LA245           LDB         #51             ; CODE FOR AT SIGN
LA247           LDX         #CONTAB-$36     ; POINT X TO CONTROL CODE TABLE
                CMPB        #33             ; IS KEY < 33?
                BLO         LA264           ; YES (ARROW KEYS, SPACE BAR, ZERO)
                LDX         #CONTAB-$54     ; POINT X TO MIDDLE OF CONTROL TABLE
                CMPB        #48             ; IS KEY >= 48?
                BHS         LA264           ; YES (ENTER,CLEAR,BREAK,AT SIGN)
                BSR         LA22E           ; CHECK SHIFT KEY (ACCA WILL CONTAIN STATUS)
                CMPB        #43             ; IS KEY <= 43?
                BLS         LA25D           ; YES (A NUMBER, COLON OR SEMICOLON)
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
                CMPA        #'z'+1
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
                BEQ         LA444           ; WRITE END OF PROG BLOCK IF BUFFER EMPTY
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
                LDB         #'A'            ; IS THIS AN ASCII SAVE?
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
                CMPA        #'M'            ; IS IT CLOADM?
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
LA5A5           CMPA        #'#'            ; IS NEXT CHARACTER A NUMBER?
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
LA5E8           SEX                         ; CONVERT ACCB TO 2 DIGIT SIGNED INTEGER
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
                CMPB        #'I'            ; IS IT INPUT MODE?
                BEQ         LA624           ; YES
                CMPB        #'O'            ; IS IT OUTPUT MODE?
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
                LDB         #'S'            ; S MEANS SEARCHING
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
LA6CB           LDA         #'F'
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
; make cursor in upper left blink
                BMI         LA700           ; RETURN IF NOT AN END OF PROGRAM BLOCK
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
LA9C5           RTI                         ; RETURN FROM INTERRUPT
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
BROMHK          CMPA        #'9'+1          ; IS THIS CHARACTER >=(ASCII 9)+1?
                BHS         LAA28           ; BRANCH IF > 9; Z SET IF = COLON
                CMPA        #SPACE          ; SPACE?
                BNE         LAA24           ; NO - SET CARRY IF NUMERIC
                JMP         GETNCH          ; IF SPACE, GET NECT CHAR (IGNORE SPACES)
LAA24           SUBA        #'0'            ; SET CARRY IF
                SUBA        #-'0'           ; CHARACTER > ASCII 0
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
LAA66           FCS         "FOR"           ; 80
                FCS         "GO"            ; 81
                FCS         "REM"           ; 82
                FCB         ''+$80          ; 83
                FCS         "ELSE"          ; 84
                FCS         "IF"            ; 85
                FCS         "DATA"          ; 86
                FCS         "PRINT"         ; 87
                FCS         "ON"            ; 88
                FCS         "INPUT"         ; 89
                FCS         "END"           ; 8A
                FCS         "NEXT"          ; 8B
                FCS         "DIM"           ; 8C
                FCS         "READ"          ; 8D
                FCS         "RUN"           ; 8E
                FCS         "RESTORE"       ; 8F
                FCS         "RETURN"        ; 90
                FCS         "STOP"          ; 91
                FCS         "POKE"          ; 92
                FCS         "CONT"          ; 93
                FCS         "LIST"          ; 94
                FCS         "CLEAR"         ; 95
                FCS         "NEW"           ; 96
                FCS         "CLOAD"         ; 97
                FCS         "CSAVE"         ; 98
                FCS         "OPEN"          ; 99
                FCS         "CLOSE"         ; 9A
                FCS         "LLIST"         ; 9B
                FCS         "SET"           ; 9C
                FCS         "RESET"         ; 9D
                FCS         "CLS"           ; 9E
                FCS         "MOTOR"         ; 9F
                FCS         "SOUND"         ; A0
                FCS         "AUDIO"         ; A1
                FCS         "EXEC"          ; A2
                FCS         "SKIPF"         ; A3
                FCS         "TAB("          ; A4
                FCS         "TO"            ; A5
                FCS         "SUB"           ; A6
                FCS         "THEN"          ; A7
                FCS         "NOT"           ; A8
                FCS         "STEP"          ; A9
                FCS         "OFF"           ; AA
                FCS         '+'             ; AB
                FCS         '-'             ; AC
                FCS         '*'             ; AD
                FCS         '/'             ; AE
                FCS         '^'             ; AF
                FCS         "AND"           ; B0
                FCS         "OR"            ; B1
                FCS         '>'             ; B2
                FCS         '='             ; B3
                FCS         '<'             ; B4

; TOKENS FOR THE SECONDARY FUNCTIONS ARE PRECEEDED BY $FF
; TOKEN #
LAB1A           FCS         "SGN"           ; 80
                FCS         "INT"           ; 81
                FCS         "ABS"           ; 82
                FCS         "USR"           ; 83
                FCS         "RND"           ; 84
                FCS         "SIN"           ; 85
                FCS         "PEEK"          ; 86
                FCS         "LEN"           ; 87
                FCS         "STR$"          ; 88
                FCS         "VAL"           ; 89
                FCS         "ASC"           ; 8A
                FCS         "CHR$"          ; 8B
                FCS         "EOF"           ; 8C
                FCS         "JOYSTK"        ; 8D
                FCS         "LEFT$"         ; 8E
                FCS         "RIGHT$"        ; 8F
                FCS         "MID$"          ; 90
                FCS         "POINT"         ; 91
                FCS         "INKEY$"        ; 92
                FCS         "MEM"           ; 93

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
LABAF           FCC         "NF"            ; 0 NEXT WITHOUT FOR
                FCC         "SN"            ; 1 SYNTAX ERROR
                FCC         "RG"            ; 2 RETURN WITHOUT GOSUB
                FCC         "OD"            ; 3 OUT OF DATA
                FCC         "FC"            ; 4 ILLEGAL FUNCTION CALL
                FCC         "OV"            ; 5 OVERFLOW
                FCC         "OM"            ; 6 OUT OF MEMORY
                FCC         "UL"            ; 7 UNDEFINED LINE NUMBER
                FCC         "BS"            ; 8 BAD SUBSCRIPT
                FCC         "DD"            ; 9 REDIMENSIONED ARRAY
                FCC         "/0"            ; 10 DIVISION BY ZERO
                FCC         "ID"            ; 11 ILLEGAL DIRECT STATEMENT
                FCC         "TM"            ; 12 TYPE MISMATCH
                FCC         "OS"            ; 13 OUT OF STRING SPACE
                FCC         "LS"            ; 14 STRING TOO LONG
                FCC         "ST"            ; 15 STRING FORMULA TOO COMPLEX
                FCC         "CN"            ; 16 CAN'T CONTINUE
                FCC         "FD"            ; 17 BAD FILE DATA
                FCC         "AO"            ; 18 FILE ALREADY OPEN
                FCC         "DN"            ; 19 DEVICE NUMBER ERROR
                FCC         "IO"            ; 20 I/O ERROR
                FCC         "FM"            ; 21 BAD FILE MODE
                FCC         "NO"            ; 22 FILE NOT OPEN
                FCC         "IE"            ; 23 INPUT PAST END OF FILE
                FCC         "DS"            ; 24 DIRECT STATEMENT IN FILE
LABE1           FCC         " ERROR"
                FCB         $00
LABE8           FCC         " IN "
                FCB         $00
LABED           FCB         CR
LABEE           FCC         "OK"
                FCB         CR,$00
LABF2           FCB         CR
                FCC         "BREAK"
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
                CMPA        #':'            ; CHECK FOR LINE SEPARATOR
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
LAEE8           LDB         #':'            ; COLON = SUBLINE TERMINATOR CHARACTER
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
                CMPA        #'"'            ; CHECK FOR DOUBLE QUOTES
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
                CMPA        #','            ; IS CHARACTER FOLLOWING LINE NUMBER A COMMA?
                BEQ         LAF54           ; YES
                PULS        B,PC            ; IF NOT, FALL THROUGH TO NEXT COMMAND
LAF67           LDX         ZERO            ; DEFAULT LINE NUMBER OF ZERO
                STX         BINVAL          ; SAVE IT IN BINVAL

; CONVERT LINE NUMBER TO BINARY - RETURN VALUE IN BINVAL

LAF6B           BCC         LAFCE           ; RETURN IF NOT NUMERIC CHARACTER
                SUBA        #'0'            ; MASK OFF ASCII
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
LAFCF           FCC         "?REDO"         ; ?REDO MESSAGE
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
LB002           CMPA        #'#'            ; CHECK FOR DEVICE NUMBER
                BNE         LB00F           ; NO DEVICE NUMBER GIVEN
                JSR         >LA5A5          ; CHECK SYNTAX AND GET DEVICE NUMBER
                JSR         >LA3ED          ; CHECK FOR VALID INPUT FILE
LB00C           JSR         >LB26D          ; SYNTAX CHECK FOR COMMA
LB00F           CMPA        #'"'            ; CHECK FOR PROMPT STRING DELIMITER
                BNE         LB01E           ; BRANCH IF NO PROMPT STRING
                JSR         >LB244          ; PUT PROMPT STRING ON STRING STACK
                LDB         #';'
                JSR         >LB26F          ; DO A SYNTAX CHECK FOR SEMICOLON
                JSR         >LB99F          ; PRINT MESSAGE TO CONSOLE OUT
LB01E           LDX         #LINBUF         ; POINT TO BASICS LINE BUFFER
                CLR         ,X              ; CLEAR 1ST BYTE - FLAG TO INDICATE NO DATA
; IN LINE BUFFER
                TST         DEVNUM          ; CHECK DEVICE NUMBER
                BNE         LB049           ; BRANCH IF NOT SET TO SCREEN
                BSR         LB02F           ; INPUT A STRING TO LINE BUFFER
                LDB         #','            ; INSERT A COMMA AT THE END
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
                CMPA        #'"'            ; CHECK FOR STRING DELIMITER
                BEQ         LB08B           ; BRANCH IF STRING DELIMITER
                LEAX        -1,X            ; BACK UP POINTER
                CLRA                        ;  ZERO = END OF LINE CHARACTER
                STA         CHARAC          ; SAVE AS TERMINATOR
                JSR         >LA35F          ; SET UP PRINT PARAMETERS
                TST         PRTDEV          ; CHECK PRINT DEVICE NUMBER
                BNE         LB08B           ; BRANCH IF CASSETTE - USE TWO ZEROS AS TERMINATOR
; CHARACTERS FOR CASSETTE
                LDA         #':'            ; END OF SUBLINE CHARACTER
                STA         CHARAC          ; SAVE AS TERMINATOR I
                LDA         #','            ; COMMA
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
                CMPA        #','            ; CHECK FOR A COMMA
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
LB0E8           FCC         "?EXTRA IGNORED" ; ?EXTRA IGNORED MESSAGE
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
                CMPA        #','            ; CHECK FOR ANOTHER ARGUMENT
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
                CMPA        #'.'            ; IS IT . (DECIMAL POINT)?
                BEQ         LB22C           ; CONVERT ASCII STRING TO FLOATING POINT
                CMPA        #$AC            ; MINUS TOKEN
                BEQ         LB27C           ; YES - GO PROCESS THE MINUS OPERATOR
                CMPA        #$AB            ; PLUS TOKEN
                BEQ         LB223           ; YES - GET ANOTHER CHARACTER
                CMPA        #'"'            ; STRING DELIMITER?
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
LB267           LDB         #')'            ; SYNTAX CHECK FOR )
                FCB         SKP2            ; SKIP 2 BYTES
LB26A           LDB         #'('            ; SYNTAX CHECK FOR (
                FCB         SKP2            ; SKIP 2 BYTES
SYNCOMMA        EQU         *
LB26D           LDB         #','            ; SYNTAX CHECK FOR COMMA
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
LB37B           CMPA        #'$'            ; CHECK FOR A STRING VARIABLE
                BNE         LB385           ; BRANCH IF IT IS NOT A STRING
                COM         VALTYP          ; SET VARIABLE TYPE TO STRING
                ADDB        #$80            ; SET BIT 7 OF 2ND CHARACTER (STRING)
                JSR         GETNCH          ; GET AN INPUT CHARACTER
LB385           STB         VARNAM+1        ; SAVE 2ND CHARACTER IN VARNAM+1
                ORA         ARYDIS          ; OR IN THE ARRAY DISABLE FLAG - IF = $80,
; DONT SEARCH FOR VARIABLES IN THE ARRAYS
                SUBA        #'('            ; IS THIS AN ARRAY VARIABLE?
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
LB3A2           CMPA        #'A'            ; CARRY SET IF < A
                BCS         LB3AA
                SUBA        #'Z'+1
                SUBA        #-('Z'+1)       ; CARRY CLEAR IF <= 'Z'
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
                CMPA        #','            ; CHECK FOR ANOTHER DIMENSION
                BEQ         LB40A           ; BRANCH IF MORE
                STB         TMPLOC          ; SAVE DIMENSION COUNTER
                JSR         >LB267          ; SYNTAX CHECK FOR A )
                PULS        A,B             ; RESTORE VARIABLE TYPE AND ARRAY
                STD         DIMFLG          ; FLAG - LEAVE DIMENSION LENGTH ON STACK
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
LB518           LDA         #'"'            ; INITIALIZE
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
LB533           CMPA        #'"'            ; COMPARE CHARACTER TO STRING DELIMITER
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
                RTS                         ; *ON THE STRING STACK
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
                CMPA        #')'            ; ARGUMENT DELIMITER?
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
                CMPA        #':'            ; CHECK FOR END OF SUB LINE
                BNE         LB7E2           ; BRNCH IF NOT END OF SUB LINE
                LDB         ,X              ; GET CHARACTER FOLLOWING COLON
                CMPB        #$84            ; TOKEN FOR ELSE?
                BEQ         LB7CB           ; YES - DONT PUT IT IN BUFFER
                CMPB        #$83            ; TOKEN FOR REMARK?
                BEQ         LB7CB           ; YES - DONT PUT IT IN BUFFER
                FCB         SKP2            ; SKIP TWO BYTES
LB7E0           LDA         #'!'            ; EXCLAMATION POINT
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
                CMPA        #'0'            ; DONT CRUNCH ASCII NUMERIC CHARACTERS
                BLO         LB842           ; BRANCH IF NOT NUMERIC
                CMPA        #'9'
                BLS         LB852           ; BRANCH IF NUMERIC
; END UP HERE IF NOT UPPER CASE ALPHA OR NUMERIC
LB842           CLR         V43             ; CLEAR ILLEGAL TOKEN FLAG
LB844           CMPA        #SPACE          ; SPACE?
                BEQ         LB852           ; DO NOT REMOVE SPACES
                STA         V42             ; SAVE INPUT CHARACTER AS SCAN DELIMITER
                CMPA        #'"'            ; CHECK FOR STRING DELIMITER
                BEQ         LB886           ; BRANCH IF STRING
                TST         V44             ; CHECK DATA FLAG AND BRANCH IF CLEAR
                BEQ         LB86B           ; DO NOT CRUNCH DATA
LB852           STA         ,U+             ; SAVE CHARACTER IN BUFFER
                BEQ         LB85C           ; BRANCH IF END OF LINE
                CMPA        #':'            ; CHECK FOR END OF SUBLINE
                BEQ         LB829           ; AND RESET FLAGS IF END OF SUBLINE
LB85A           BRA         LB82D           ; GO GET ANOTHER CHARACTER
LB85C           CLR         ,U+             ; DOUBLE ZERO AT END OF LINE
                CLR         ,U+
                TFR         U,D             ; SAVE ADDRESS OF END OF LINE IN ACCD
                SUBD        #LINHDR         ; LENGTH OF LINE IN ACCD
                LDX         #LINBUF-1       ; SET THE INPUT POINTER TO ONE BEFORE
                STX         CHARAD          ; THE START OF THE CRUNCHED LINE
                RTS                         ; EXIT 'CRUNCH'
LB86B           CMPA        #'?'            ; CHECK FOR "?" - PRINT ABBREVIATION
                BNE         LB873           ; BRANCH IF NOT PRINT ABBREVIATION
                LDA         #$87            ; GET THE PRINT TOKEN AND SAVE IT
                BRA         LB852           ; IN BUFFER
LB873           CMPA        #'''            ; APOSTROPHE IS SAME AS REM
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
LB88A           CMPA        #'0'            ; LESS THAN ASCII ZERO?
                BCS         LB892           ; BRANCH IF SO
                CMPA        #';'+1          ; CHECK FOR NUMERIC VALUE, COLON OR SEMICOLON
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
                LDA         #':'            ; PUT A COLON (SUBLINE) BEFORE ELSE TOKEN
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
LB8FE           CMPA        #'@'            ; CHECK FOR PRINT @
                BNE         LB907           ; NOT PRINT @
LB902           JSR         >LA554          ; MOVE CURSOR TO PROPER PRINT LOCATION
LB905           BRA         LB911           ; GO PRINT THE DATA
LB907           CMPA        #'#'            ; CHECK FOR PRINT NUMBER
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
                CMPA        #','            ; COMMA?
                BEQ         LB966           ; YES - ADVANCE TO NEXT TAB FIELD
                CMPA        #';'            ; SEMICOLON?
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
                CMPA        #','            ; COMMA?
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
                CMPA        #')'            ; 'SYNTAX' ERROR IF NOT ')'
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
LB9AF           LDA         #'?'            ; QUESTION MARK TO CONSOLE OUT
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
                BCS         LB9E2           ; BRANCH IF EXPONENT FPA0 > FPA1
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
                RTS                         ; RETURN IF FP (X) = FPA0
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
LBD11           RTS                         ; *
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
                CMPA        #'-'            ; CHECK FOR A LEADING MINUS SIGN AND BRANCH
                BNE         LBD2D           ; IF NO MINUS SIGN
                COM         COEFCT          ; TOGGLE SIGN; 0 = +; FF = -
                BRA         LBD31           ; INTERPRET THE REST OF THE STRING
LBD2D           CMPA        #'+'            ; CHECK FOR LEADING PLUS SlGN AND BRANCH
                BNE         LBD35           ; IF NOT A PLUS SIGN
LBD31           JSR         GETNCH          ; GET NEXT INPUT CHARACTER FROM BASIC
                BCS         LBD86           ; BRANCH IF NUMERIC CHARACTER
LBD35           CMPA        #'.'            ; DECIMAL POlNT?
                BEQ         LBD61           ; YES
                CMPA        #'E'            ; "E" SHORTHAND FORM (SCIENTIFIC NOTATION)?
                BNE         LBD65           ; NO
; EVALUATE EXPONENT OF EXPONENTIAL FORMAT
                JSR         GETNCH          ; GET NEXT INPUT CHARACTER FROM BASIC
                BCS         LBDA5           ; BRANCH IF NUMERIC
                CMPA        #$AC            ; MINUS TOKEN?
                BEQ         LBD53           ; YES
                CMPA        #'-'            ; ASCII MINUS?
                BEQ         LBD53           ; YES
                CMPA        #$AB            ; PLUS TOKEN?
                BEQ         LBD55           ; YES
                CMPA        #'+'            ; ASCII PLUS?
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
                SUBB        #'0'            ; MASK OFF ASCII
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
                SUBA        #'0'            ; MASK OFF ASCII FROM ACCA, PUSH
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
                LDA         #'-'            ; ASCII MINUS SIGN
LBDE4           STA         ,U+             ; STORE SIGN OF NUMBER
                STU         COEFPT          ; SAVE BUFFER POINTER
                STA         FP0SGN          ; SAVE SIGN (IN ASCII)
                LDA         #'0'            ; ASCII ZERO IF EXPONENT = 0
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
                LDA         #'.'            ; STORE A PERIOD
                STA         ,U+             ; IN THE BUFFER
                TSTB                        ;  CHECK DECIMAL POINT FLAG
                BEQ         LBE4B           ; BRANCH IF NOTHING TO LEFT OF DECIMAL POINT
                LDA         #'0'            ; STORE A ZERO
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
LBE72           ADDB        #'0'-1          ; ADD ASCII OFFSET TO DIGIT
                LEAX        4,X             ; MOVE TO NEXT POWER OF 10 MANTISSA
                TFR         B,A             ; SAVE DIGIT IN ACCA
                ANDA        #$7F            ; MASK OFF BIT 7 (ADD/SUBTRACT FLAG)
                STA         ,U+             ; STORE DIGIT IN STRING BUFFER
                DEC         V45             ; DECREMENT DECIMAL POINT FLAG
                BNE         LBE84           ; BRANCH IF NOT TIME FOR DECIMAL POINT
                LDA         #'.'            ; STORE DECIMAL POINT IN
                STA         ,U+             ; STRING BUFFER
LBE84           COMB                        ;  TOGGLE BIT 7 (ADD/SUBTRACT FLAG)
                ANDB        #$80            ; MASK OFF ALL BUT ADD/SUBTRACT FLAG
                CMPX        #LBEC5+9*4      ; COMPARE X TO END OF MANTISSA TABLE
                BNE         LBE50           ; BRANCH IF NOT AT END OF TABLE
; BLANK TRAILING ZEROS AND STORE EXPONENT IF ANY
LBE8C           LDA         ,-U             ; GET THE LAST CHARACTER; MOVE POINTER BACK
                CMPA        #'0'            ; WAS IT A ZERO?
                BEQ         LBE8C           ; IGNORE TRAILING ZEROS IF SO
                CMPA        #'.'            ; CHECK FOR DECIMAL POINT
                BNE         LBE98           ; BRANCH IF NOT DECIMAL POINT
                LEAU        -1,U            ; STEP OVER THE DECIMAL POINT
LBE98           LDA         #'+'            ; ASCII PLUS SIGN
                LDB         V47             ; GET SCIENTIFIC NOTATION EXPONENT
                BEQ         LBEBA           ; BRANCH IF NOT SCIENTIFIC NOTATION
                BPL         LBEA3           ; BRANCH IF POSITIVE EXPONENT
                LDA         #'-'            ; ASCII MINUS SIGN
                NEGB                        ;  NEGATE EXPONENT IF NEGATIVE
LBEA3           STA         2,U             ; STORE EXPONENT SIGN IN STRING
                LDA         #'E'            ; GET ASCII E (SCIENTIFIC NOTATION
                STA         1,U             ; FLAG) AND SAVE IT IN THE STRING
                LDA         #'0'-1          ; INITIALIZE ACCA TO ASCII ZERO
; CONVERT BINARY VALUE IN ACCB TO DECIMAL
; ASCII NUMBER (< 100) IN ACCD
LBEAB           INCA                        ;  ADD ONE TO 10S DIGIT OF EXPONENT
                SUBB        #10             ; SUBTRACT 10 FROM ACCB
                BCC         LBEAB           ; ADD 1 TO 10S DIGIT IF NO CARRY
                ADDB        #'9'+1          ; CONVERT UNITS DIGIT TO ASCII
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

                FDB         $FEEE           ; INT.SWI3
                FDB         $FEF1           ; INT.SWI2
                FDB         $FEF4           ; INT.FIRQ
                FDB         $FEF7           ; INT.IRQ
                FDB         $FEFA           ; INT.SWI
                FDB         $FEFD           ; INT.NMI
                FDB         $8C1B           ; DLDBUG

;                END

; ===========================================================================================================
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
                LEAY        MMUIMAGE,PCR    ; POINT Y TO THE MMU REGISTER IMAGES
                LDB         #16             ; 16 MMU REGISTERS
SC01B           LDA         ,Y+             ; GET A BYTE FROM THE IMAGE
                STA         ,X+             ; SAVE IT IN THE MMU REGISTER
                DECB                        ;  BUMP COUNTER
                BNE         SC01B           ; LOOP UNTIL DONE
                LDA         #COCO+MMUEN+MC3+MC2+MC1 ; ENABLE COCO COMPATIBLE MODE; ENABLE MMU
                STA         INIT0           ; AND TURN ON THE NORMAL SPARE CHIP SELECT
; MOVE THE INITIALIZATION CODE FROM ROM TO RAM($4000); THIS IS DONE IN
; PREPARATION FOR MOVING BASIC FROM ROM TO RAM.
                LEAX        >BEGMOVE,PCR    ; POINT TO START OF ROM CODE
                LDY         #$4000          ; RAM LOAD ADDRESS
SC02F           LDD         ,X++            ; GRAB TWO BYTES
                LDU         ,X++            ; GRAB TWO MORE BYTES
                STD         ,Y++            ; MOVE FIRST SET OF BYTES
                STU         ,Y++            ; AND THEN THE SECOND
                CMPX        #ENDMOVE        ; ARE ALL BYTES MOVED?
                BCS         SC02F           ; KEEP GOING UNTIL DONE
                JMP         $4000           ; JUMP INTO THE MOVED CODE
; THE REST OF THE CODE IS MOVED INTO RAM TO BE EXECUTED
BEGMOVE         LEAS        -01,S           ; MAKE A TEMPORARY STORAGE LOCATION ON THE STACK
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;
                NOP                         ;  SPACE FILLER NOPS - THEY SERVE NO PURPOSE
                LDA         #$FF
                STA         V_TIMER
                STA         V_TIMER+1       ; SET THE TIMER TO $FFFF AND START IT COUNTING
; SET UP THE VIDEO CONTROL REGISTERS
                LEAX        VIDIMAGE,PCR    ; POINT X TO THE VIDEO CONTROL REGISTER IMAGE
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
                STA         -03,U           ; SAMREG+21 (FFD5); SELECT RAM PAGE 1; USELESS IN THE COCO 3
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
                LEAY        -01,Y           ; LET'S CHECK FOT EH ALT KEY NOW
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
                LEAY        PATCHTAB,PCR    ; POINT Y TO THE PATCH TABLE
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
                CMPA        #'D'
                BNE         SC137
                CMPB        #'K'
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
                LEAY        INTIMAGE,PCR    ; POINT X TO THE INTERRUPT JUMP VECTOR IMAGES
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
                LEAY        PALIMAGE,PCR    ; POINT Y TO THE PALETTE REGISTER IMAGES
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
                LEAY        SC30D,PCR       ; POINT Y TO THE CODED NAMES OF THE AUTHORS
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
                JMP         L8C36+16        ; $8C46
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
                LEAY        >SC355,PCR      ; POINT Y TO THE PATCH DATA
                LDB         ,Y+             ; GET THE NUMBER OF BYTES TO PATCH
                BRA         SC349
SC334           LDX         #$C8B4          ; POINT X TO DISK BASIC 1.1 KEYBOARD PATCH ($C8B4)
                LDA         #$12            ; OP CODE OF A NOP INSTRUCTION
                LDB         #11             ; PATCH 11 BYTES
SC33B           STA         ,X+             ; STORE A NOP
                DECB                        ;  DECREMENT COUNTER
                BNE         SC33B           ; LOOP UNTIL DONE
                LDX         #$C0D9          ; POINT X TO DISK BASIC 1.1 PATCH ADDRESS ($C0D9)
                LEAY        >SC351,PCR      ; POINT Y TO THE PATCH DATA
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

ENDMOVE         EQU         *               ; THE END OF THE DATA THAT'S COPIED TO RAM


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

ENDPIC          EQU         *               ; THE END OF THE AUTHOR'S PICTURE DATA

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
                FCB         $08             ; $00 | $08
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
                FCB         $0B             ; $03 | $08
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
                FCB         $0B             ; $03 | $08
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
                FCB         $88             ; $80 | $08
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
                FCB         $88             ; $80 | $08
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
                LEAX        >IM.MMU,PCR     ; POINT TO THE RAM IMAGE OF THE MMU REGISTERS
                BSR         SE0F1           ; MOVE 16 BYTES INTO THE MMU REGISTERS
                PULS        A,B,X,Y,PC
; PLACE A BLOCK INTO LOGICAL ADDRESS SPACE BLOCK 0.
; ENTER WITH ACCB CONTAINING THE BLOCK NUMBER TO BE PLACED INTO THE LOGICAL ADDRESS SPACE
; EXIT WITH BLOCK 7.0 REPLACED IN BLOCK 0 OF THE LOGICAL ADDRESS SPACE RAM IMAGE
SELBLOK0        PSHS        Y,X,B,A
                LEAX        >IM.MMU,PCR     ; POINT TO THE RAM IMAGE OF THE MMU REGISTERS
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
                LEAX        >IM.MMU,PCR     ; POINT TO THE RAM IMAGE OF THE MMU REGISTERS
                PSHS        X               ; TEMP SAVE
                LDB         #BLOCK6.6       ; GET THE BLOCK WHICH CONTAINS THE HI-RES TEXT SCREEN
                STB         $01,X           ; AND SAVE IT IN THE MMU IMAGE OF TASK REGISTER 0
                BSR         SE0F1           ; COPY THE RAM IMAGE OF THE MMU REGISTERS INTO THE MMU REGISTERS
                PULS        X               ; RESTORE THE MMU IMAGE PONTER
                LDB         #BLOCK7.1       ; GET BLOCK 7.1 (BASIC'S NORMAL LOGICAL BLOCK 1)
                STB         $01,X           ; AND SAVE IT IN THE MMU IMAGE
                PULS        A,B,X,Y,PC
SE0CB           PSHS        Y,X,B,A
                LEAX        >IM.MMU,PCR     ; POINT TO THE MMU RAM IMAGE
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
                LEAX        -01,X           ; UNNECESSARY INSTRUCTION; NEXT ONE SHOULD JUST BE LDA -1,X
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
COMDIC20        FCS         "WIDTH"         ; E2
                FCS         "PALETTE"       ; E3
                FCS         "HSCREEN"       ; E4
                FCS         "LPOKE"         ; E5
                FCS         "HCLS"          ; E6
                FCS         "HCOLOR"        ; E7
                FCS         "HPAINT"        ; E8
                FCS         "HCIRCLE"       ; E9
                FCS         "HLINE"         ; EA
                FCS         "HGET"          ; EB
                FCS         "HPUT"          ; EC
                FCS         "HBUFF"         ; ED
                FCS         "HPRINT"        ; EE
                FCS         "ERR"           ; EF
                FCS         "BRK"           ; F0
                FCS         "LOCATE"        ; F1
                FCS         "HSTAT"         ; F2
                FCS         "HSET"          ; F3
                FCS         "HRESET"        ; F4
                FCS         "HDRAW"         ; F5
                FCS         "CMP"           ; F6
                FCS         "RGB"           ; F7
                FCS         "ATTR"          ; F8

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
FUNDIC20        FCS         "LPEEK"         ; A8
                FCS         "BUTTON"        ; A9
                FCS         "HPOINT"        ; AA
                FCS         "ERNO"          ; AB
                FCS         "ERLIN"         ; AC

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
DISK20MS        FCC         "DISK EXTENDED COLOR BASIC 2.0"
                FCB         $0D
                FCC         "COPR. 1981, 1986 BY TANDY"
                FCB         $0D
                FCC         "UNDER LICENSE FROM MICROSOFT"
                FCB         $0D
MWAREMS         FCC         "AND MICROWARE SYSTEMS CORP."
SE313           FCB         $0D,$0D,$00
DISK21MS        FCC         "DISK EXTENDED COLOR BASIC 2.1"
                FCB         $0D
                FCC         "COPR. 1982, 1986 BY TANDY"
                FCB         $0D
                FCC         "UNDER LICENSE FROM MICROSOFT"
                FCB         $0D
                FCC         "AND MICROWARE SYSTEMS CORP."
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
                SUBA        #'0'            ; MASK OFF ASCII
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
                LEAX        >BAS20ERR,PCR   ; POINT TO ENHANCED BASIC'S ADDITIONAL ERROR CODES
SE496           JSR         LACA0           ; GET A CHARACTER FROM X AND SEND IT TO CONSOLE OUT
                JSR         LACA0           ; DO IT AGAIN
                JMP         LAC65           ; RE-ENTER THE MAIN STREAM OF CODE ($AC65)
SE49F           CMPB        #39*2           ; HI-RES TEXT MODE ERROR
                BNE         SE4B0           ; BRANCH IF NOT
                JSR         LB95C           ; SET UP THE PRINT PARAMETERS
                JSR         LB9AF           ; SEND A '?' TO CONSOLE OUT
                LEAX        >SE4CE,PCR      ; POINT TO ENHANCED BASIC'S ADDITIONAL ERROR CODES
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
BAS20ERR        FCC         "HR"            ; 38 HIRES GRAHICS ERROR
SE4CE           FCC         "HP"            ; 39 HIRES TEXT ERROR
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
                SEX                         ; NOW ACCD = $FFFF IF NOT A REAL ERROR
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
                LEAX        >SE5D5,PCR      ; POINT TO THE BUTTON MASKING ROUTINES
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
SE5FA           LEAX        >IM.RGB,PCR     ; POINT TO THE DEFAULT RGB PALETTE COLORS
                BRA         SE634           ; PUT THE DATA POINTED TO BY X INTO THE PALETTE REGISTERS
SE600           CMPA        #$F6            ; 'CMP' TOKEN?
                BNE         SE60C           ; NO, GET A REGISTER NUMBER AND COLOR
                JSR         GETNCH          ; GET THE NEXT CHARACTER FROM BASIC'S INPUT LINE
; CMP ENTRY POINT - SET THE PALETTE REGISTERS FOR DEFAULT CMP VALUES

; -----------------------------------------------------------------------------
                if          COCOPAL<1
; -----------------------------------------------------------------------------
SE606           LEAX        >IM.CMP,PCR     ; POINT TO THE DEFAULT CMP PALETTE COLORS
; -----------------------------------------------------------------------------
                else
; -----------------------------------------------------------------------------
SE606           LEAX        >IM.RGB,PCR     ; POINT TO THE DEFAULT CMP PALETTE COLORS
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
HCOLOR          CMPA        #','            ; CHECK FOR COMMA, FIRST ARGUMENT NOT GIVEN
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
                CMPA        #')'            ; SYNTAX CHECK FOR ')'
                BEQ         SE72F           ; EXIT IF ')'
                JSR         >SYNCOMMA       ; DO A SYNTAX CHECK FOR A COMMA
                CMPA        #','            ; SYNTAX CHECK FOR A COMMA
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
                RTS                         ; WASTED BYTE
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
                CMPA        #'('            ; CHECK FOR '('
                BEQ         SE899           ; GO LOOK FOR START AND END POINTS
                CMPA        #$AC            ; CHECK FOR MINUS SIGN TOKEN
                BEQ         SE899           ; BRANCH IF NO STARTING POINTS GIVEN
                LDB         #'@'            ; CHECK FOR '@' SIGN
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
                LDB         #'B'            ; DRAW A BOX?
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
SE8EB           LDB         #'F'            ; CHECK FOR FILL OPTION
                JSR         LB26F           ; GO DO A SYNTAX CHECK FOR AN 'F'
                BRA         SE8F6           ; GO 'FILL' THE BOX
SE8F2           LEAX        -01,X           ; MOVE VERTICAL COORD UP ONE
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
                LEAY        -01,Y           ; DEC COUNTER
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
                LEAX        -01,X           ; DECR ONE
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
                LEAX        -01,X           ; SUBTRACT ONE
                STX         HORBEG          ; SAVE NEW HORIZONTAL COORD
                RTS
SE9C6           LDX         VERBEG          ; GET VERTICAL COORD
                LEAX        -01,X           ; SUBTRACT ONE
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
                CMPA        #'@'            ; CHECK FOR @ SIGN (HCIRCLE@ IS LEGAL SYNTAX)
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
                LEAU        -02,U           ; MOVE TO COSINE (VERTICAL)
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
SEB5F           RTS                         ; EXIT CIRCLE ROUTINE
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
                CMPA        #'@'            ; CHECK FOR @ SIGN
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
                LEAX        -02,X           ; MOVE HORIZONTAL COORD TWO PIXELS TO THE LEFT
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
                CMPA        #'@'            ; ALLOW HGET@, HPUT@ AS LEGAL SYNTAX
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
                RTS                         ; WHY NOT MAKE THE JSR ABOVE A JMP
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
                RTS                         ; WASTED THIS AND ABOVE INSTRUCTION SHOULD BE PULS Y,PC
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
                CMPA        #';'            ; CHECK FOR A SEMI-COLON
                BEQ         SF3CF           ; IGNORE SEMI-COLONS
                CMPA        #''             ; CHECK FOR APOSTROPHE
                BEQ         SF3CF           ; IGNORE APOSTROPHE
                CMPA        #'N'            ; UPDATE CHECK?
                BNE         SF3E6           ; BRANCH IF NOT
                COM         VD5             ; TOGGLE UPDATE FLAG; 0 = UPDATE, FF = NO UPDATE
                BRA         SF3CF           ; GET NEXT COMMAND
SF3E6           CMPA        #'B'            ; CHECK DRAW FLAG?
                BNE         SF3EE           ; BRANCH IF NOT
                COM         VD6             ; TOGGLE DRAW FLAG; 0 = DRAW LINE, FF = DON'T DRAW LINE
                BRA         SF3CF           ; GET ENXT COMMAND
SF3EE           CMPA        #'X'            ; SUBSTRING?
                LBEQ        SF4A1           ; GO EXECUTE A COMMAND SUBSTRING
                CMPA        #'M'            ; MOVE THE DRAW POSITION?
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
                CMPA        #'C'            ; CHANGE COLOR?
                BEQ         SF445           ; BRANCH IF YES
                CMPA        #'A'            ; CHANGE ANGLE?
                BEQ         SF451           ; BRANCH IF YES
                CMPA        #'S'            ; CHANGE SCALE?
                BEQ         SF45C           ; BRANCH IF YES
                CMPA        #'U'            ; GO UP?
                BEQ         SF496           ; BRANCH IF YES
                CMPA        #'D'            ; GO DOWN?
                BEQ         SF492           ; BRANCH IF YES
                CMPA        #'L'            ; GO LEFT?
                BEQ         SF48C           ; BRANCH IF YES
                CMPA        #'R'            ; GO RIGHT?
                BEQ         SF485           ; BRANCH IF YES
                SUBA        #'E'            ; MASK OFF ASCII FOR LETTER E-H COMMAND CHECKS
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
                CMPA        #','            ; CHECK FOR COMMA
                LBNE        LB44A           ; ILLEGAL FUCNTION CALL ERROR IF NO COMMA
                JSR         >SF575          ; EVALUATE THE VERTICAL DIFFERENCE
                TFR         D,X             ; SAVE VERTICAL DIFFERENCE IN X
                PULS        U               ; GET HORIZONTAL DIFFERENCE IN U
                PULS        A               ; GET FIRST COMMAND CHARACTER
                CMPA        #'+'            ; CHECK FOR PLUS
                BEQ         SF570           ; TREAT VALUES IN X AND U AS DIFFERENCES AND MOVE POINTER
                CMPA        #'-'            ; CHECK FOR MINUS
                BNE         SF507           ; IF NOT '+' OR '-', MOVE THE POINTER TO THE COORDINATES IN U AND ACCD
SF570           TFR         U,D             ; PUT HORIZONTAL DIFFERENCE IN ACCD; X CONTAINS THE VERTICAL DIFFERENCE
                JMP         >SF4D4          ; GOMOVE THE DRAW POSITION
SF575           JSR         >SF591          ; GET A CHAR FROM COMMAND LINE
SF578           CMPA        #'+'            ; CHECK FOR LEADING + (RELATIVE MOTION)
                BEQ         SF583           ; BRANCH IF RELATIVE
                CMPA        #'-'            ; DO THE SAME FOR THE MINUS SIGN
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
SF5A7           CMPA        #'='            ; CHECK FOR A VARIABLE EQUATE
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
SF5C1           SUBA        #'0'            ; MASK OFF ASCII
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
                LEAX        -01,X           ;
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
SF608           CMPA        #'0'
                BCS         SF610           ; RETURN IF LESS THAN ASCII ZERO
                SUBA        #'9'+1
                SUBA        #-('9'+1)       ; SET CARRY IF NOT 0-9
SF610           RTS
; INTERPRET THE CURRENT COMMAND STRING AS IF IT WERE A BASIC VARIABLE
SF611           LDX         VD9             ; GET THE COMMAND POINTER
                PSHS        X               ; SAVE IT
                JSR         >SF591          ; GET A COMMAND STRING CHARACTER
                JSR         LB3A2           ; SET CARRY IF NOT UPPER CASE ALPHA
                LBCS        LB44A           ; ILLEGAL FUNCTION CALL ERROR IF NOT ALPHA - ILLEGAL VARIABLE NAME
SF61F           JSR         >SF591          ; GET COMMAND STRING CHARACTER
                CMPA        #';'            ; CHECK FOR A SEMICOLON (SUBCOMMAND SEPARATOR)
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
                LEAY        IM.PALET,PCR    ; POINT TO THE PALETTE REGISTER IMAGES
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
MICROMS         FCC         "Microware Systems Corp."
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
                LEAX        >SF6F4,PCR      ; POINT TO THE INSTRUCTION WHICH BRANCHES TO THIS ROUTINE
                LDA         #$12            ; OP CODE OF A NOP
                STA         ,X+             ; REPLACE THE BRANCH TO THIS ROUTINE WITH 2 NOPs MAKING IT SO
                STA         ,X              ; THAT THIS ROUTINE MAY ONLY BE ENTERED ONE TIME
                LEAX        >AUTHORMS,PCR   ; POINT TO THE AUTHORS CODED NAMES
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
                LEAY        SE03B,PCR       ; POINT TO THE 40 COLUMN MODE REGISTER IMAGE
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
                STD         -02,X           ; SAVE AN UNDERLINED SPACE AS THE NEW CURSOR CHARACTER
                LEAX        -02,X           ; MOVE THE CURSOR POINTER BACK TWO
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
                TST         -01,X          ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBPL        LB151           ; TYPE MISMATCH ERROR IF NUMERIC VARIABLE
                LDY         FPA0+2          ; POINT Y TO THE START OF THE STRING DESCRIPTOR
                LDB         #$05            ; VARIABLE DESCRIPTORS ARE 5 BYTES LONG
SF963           LDA         ,Y+             ; COPY THE DATA FROM THE STRING DESCRIPTOR
                STA         ,X+             ; TO THE VARIABLE DESCRIPTOR
                DECB                        ;  DECREMENT THE DESCRIPTOR COUNTER
                BNE         SF963           ; LOOP UNTIL DONE
                LDX         TEMPPT          ; THIS CODE IS DESIGNED TO REMOVE THE ABOVE ALLOCATED STRING FROM
                LEAX        -05,X           ; THE STRING STACK - IT MAY CAUSE BUGS BECAUSE IT DOESN'T RESET
                STX         TEMPPT          ; LASTPT; LDX LASTPT, JSR LB675 WOULD BE MUCH BETTER
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                CLRA                        ;  ZERO OUT THE MS BYTE OF ACCD
                LDB         VCB+1           ; GET THE CURSOR ATTRIBUTES
                JSR         GIVABF          ; CONVERT ACCD TO FLOATING POINT
                LDX         VARDES          ; POINT X TO THE VARIABLE DESCRIPTOR
                TST         -01,X           ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBMI        LB151           ; TYPE MISMATCH ERROR IF STRING VARIABLE
                JSR         LBC35           ; PACK FPA0 AND STORE IT IN THE DESCRIPTOR POINTED TO BY X
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                JSR         >SYNCOMMA       ; SYNTAX CHECK FOR A COMMA
                CLRA                        ;  ZERO OUT THE MS BYTE OF ACCD
                LDB         VCD             ; GET THE X COORDINATE OF THE CURSOR POSITION
                JSR         GIVABF          ; CONVERT ACCD TO FLOATING POINT
                LDX         VARDES          ; POINT X TO THE VARIABLE DESCRIPTOR
                TST         -01,X           ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
                LBMI        LB151           ; TYPE MISMATCH ERROR IF STRING VARIABLE
                JSR         LBC35           ; PACK FPA0 AND STORE IT IN THE DESCRIPTOR POINTED TO BY X
                JSR         LB357           ; EVALUATE A VARIABLE; RETURN X POINTING TO THE VARIABLE DESCRIPTOR
                STX         VARDES          ; SAVE THE VARIABLE DESCRIPTOR
                CLRA                        ;  ZERO OUT THE MS BYTE OF ACCD
                LDB         VCD+1           ; GET THE Y COORDINATE OF THE CURSOR POSITION
                JSR         GIVABF          ; CONVERT ACCD TO FLOATING POINT
                LDX         VARDES          ; POINT X TO THE VARIABLE DESCRIPTOR
                TST         -01,X           ; CHECK THE SECOND CHARACTER OF THE VARIABLE NAME
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
                CMPA        #'B'            ; CHECK FOR THE BLINK ATTRIBUTE FLAG
                BNE         SF9F6           ; BRANCH IF NOR BLINK ATTRIBUTE FLAG
                PULS        B
                ORB         #$80            ; SET BIT 7 WHICH IS THE BLINK ATTRIBUTE BIT
                PSHS        B
                JSR         GETNCH          ; GET A CHARACTER FROM BASIC'S INPUT LINE
                BRA         SF9E3           ; KEEP CHECKING FOR ATTRIBUTE FLAGS
SF9F6           CMPA        #'U'            ; CHECK FOR THE UNDERLINE ATTRIBUTE
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
