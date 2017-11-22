	LIST P = 16F747
	title "Case Study 3"

#include <P16F747.INC>

	__CONFIG _CONFIG1, _FOSC_HS & _CP_OFF & _DEBUG_OFF & _VBOR_2_0 & _BOREN_0 & _MCLR_ON & _PWRTE_ON & _WDT_OFF
	
	__CONFIG _CONFIG2, _BORSEN_0 & _IESO_OFF &_FCMEN_OFF


;#######################################
;#######  WRITTEN BY RENJIE LI   #######
;#######  ALL RIGHTS RESERVED    #######
;#######################################





;############################
;   Variable Declaration   ##
;############################


; PORT Assignments:
; Port B : pin 0-2 is for mode indicators , pin 3 is fault indicator
; Port C : pin 0 is for GREEN button , pin 1 is for RED button
; Port D : customized, declare it later here
; Port E : pin 0-2 is for octal switch


Count   equ 20h ; Counter
Temp    equ 21h ; A temporary register
State   equ 22h ; Program state register, stores value read from octal
Delay   equ 23h ; delay for switch
ADValue equ 24h ; value read from AD
Timer2  equ 25h
Timer1  equ 26h
Timer0  equ 27h
OneSec	equ	28h
;Initial Part

    org 00h
    goto    initPort

    org 04h
    goto    isrService

    org 10h

;##############################
;   PORT INITIALIZATION    ####
;##############################

initPort

    clrf    PORTB ; clear PORT B,C,D,E
    clrf    PORTC
    clrf    PORTD
    clrf    PORTE

    bsf     STATUS,RP0  ; set bit in STATUS register for bank 1
    clrf    TRISB       ; configure Port B as all outputs
    movlw   B'00000011' ; move 0xFF into W register
    movwf   TRISC       ; configure Port C pin0,1 as inputs
    movlw   B'00000111' ; move 0x07 into W register
    movwf   TRISE       ; configure Port E pin 0,1,2 as inputs (octal switch)
    movlw   B'00001010' ; IN ORDER TO MAKE PORTE WORK, LOWER 4 BITS SHOULD BE 1010, [LECTURE 4]
    movwf   ADCON1      ; move to special function A/D register
    movlw   B'00000010' ; move 0x02 into W register
    movwf   TRISD       ; Port D pin 1 input, others output
    bcf     STATUS,RP0  ; select register bank 0
    clrf    Count       ; zero the counter
    clrf    State
    clrf	ADValue

;#######################################################
;#######################################################



;###################################
;       MAIN PART                  #
;###################################

;Mode Selection


waitPress

	btfsc   PORTC,0     ; see if green button pressed
	goto    GreenPress  ; if pressed -- go to routine
	goto    waitPress   ; if not , simply keep checking


GreenPress

	btfss   PORTC,0     ; see if green button still pressed
	goto    waitPress   ; noise --button not pressed--keep checking

GreenRelease

	btfsc   PORTC,0     ; see if green button is released
	goto    GreenRelease; if not, keep waiting

	call    SwitchDelay ; let switch debounce
	;goto    ModeSelection


ModeSelection

	comf    PORTE,0     ; complement PORTE into w register
	andlw	B'00000111' ; only leave the lower 3 bits of PORTE
	movwf   State       ; move the mode state to State register
	movwf   PORTB       ; send the right mode num to LED indicator
	bcf     PORTB,3     ; no initial error indicated on pin 3 LED indicator


	bcf     STATUS,Z
	movlw   B'00000001' ; move 0x01 (mode 1) to w register
	xorwf   State,0     ; compare with w to see if it is mode 1
	btfsc   STATUS,Z    ; if it is mode 1 (Z will be 1)
	goto    ModeOne     ; goto Mode One

	bcf     STATUS,Z
	movlw   B'00000010' ; move 0x02 (mode 2) to w register
	xorwf   State,0     ; compare with w to see if it is mode 2
	btfsc   STATUS,Z    ; if it is mode 2 (Z will be 1)
	goto    ModeTwo     ; goto Mode Two

	bcf     STATUS,Z
	movlw   B'00000011' ; move 0x03 (mode 3) to w register
	xorwf   State,0     ; compare with w to see if it is mode 3
	btfsc   STATUS,Z    ; if it is mode 3 (Z will be 1)
	goto    ModeThree   ; goto Mode Three

	bcf     STATUS,Z
	movlw   B'00000100' ; move 0xF4 (mode 4) to w register
	xorwf   State,0     ; compare with w to see if it is mode 4
	btfsc   STATUS,Z    ; if it is mode 4 (Z will be 1)
	goto    ModeFour    ; goto Mode Four
;------------------------------------------------

;#######################################################

; Fault Processing
; If it is not mode 1,2,3,4, Welcome to FaultProcessing !
; If you make mistakes in mode 1,2,3,4, Welcome to FaultProcessing too !

FaultInfo

;movwf   PORTE,0     ; read PORTE value to w register
;comf    PORTE,0     ; complement PortE to w register
;movwf   PORTB       ; send the right LED indicator to PORTB
	clrf    PORTD       ; clear the armature

Loop

	bsf     PORTB,3     ; PortB pin3 Error LED indicator on
	call    FaultDelay  ; make Port B flash on  
	bcf     PORTB,3     ; PortB pin3 Error LED indicator off
	call    FaultDelay  ; make port B flash off
	goto    Loop        ; Keep the loop

FaultDelay
;   Set delay duration
;   here we want the delay to last for one second
	movlw   06h         ; get most significant hex value +1
	movwf   Timer2      
	movlw   16h
	movwf   Timer1
	movlw   15h
	movwf   Timer0

FaultDelayLoop

	decfsz  Timer0, F
	goto    FaultDelayLoop
	decfsz  Timer1, F
	goto    FaultDelayLoop
	decfsz  Timer2, F
	goto    FaultDelayLoop
	return
;---------------------------------------------

; MODE 1 REALLY WORKS !!! YEAH HAHAHAHAHAHA!! 
;----------------------------------------------------
; Mode 1

ModeOne

;below two expressions might be useless, just follow the Counter.asm

	btfsc   PORTC,0     ; see if green button pressed;   no           
	goto    GreenPress
	btfsc   PORTC,1     ; see if red button pressed
	goto    RedPress    ; if it is , go to redpress
	goto    ModeOne     ; keep checking


RedPress

	btfss   PORTC,1     ; see if red button still pressed
	goto    ModeOne     ; noise - keep checking


RedRelease

	btfsc   PORTC,1     ; see if red button released
	goto    RedRelease  ; no , keep checking
	call    SwitchDelay ; let switch debounce
	btfss   PORTD,1     ; see if solenoid is engaged or disengaged , by Pin 1 in PORTD
	call    SoleToEng   ; if it is disengaged, make it engage
	btfsc	PORTD,1		; see if solenoid is engaged or disengaged , by Pin 1 in PORTD
	call    SoleToDis   ; if it is engaged, make it disengage

	goto    ModeOne    ; when everything is done, return to ModeOne, keep looping

;###########################################################

SoleToEng
; make solenoid engage
	bsf     PORTD,0     ; make it engage
	btfsc   PORTD,1     ; check if it is engaged , this pin is connected to LM311 as an input
	goto    TrnReduced  ; if it is, turn on and off the corresponding transistors 
	btfss	PORTB,3		; 
	goto	SoleToEng	; if it is not mode4, wait for it to engage	
	call	TenSecDelay ; call a ten seconds delay
	btfsc	PORTD,1		; if it is still not indicating engagement
	goto	FaultInfo

TrnReduced

	bsf     PORTD,2     ; turn on the reduced transistor
	bcf     PORTD,0     ; turn off the main transistor
	return

SoleToDis
; make solenoid disengage
	bcf     PORTD,0     ; make it disengage
	bcf     PORTD,2     ; turn off the reduced transistor
	btfss   PORTD,1     ; check if it is disengaged
	return		        ; if it is , return
	goto    SoleToDis   ; if not , keep looping
;--------------------------------------------------------

;##############################################################

ModeTwo

	btfsc   PORTC,0     ;see if green button pressed
	goto    GreenPress  ; if pressed , go to GreenPress

; below two expressions are initialization of A/D Hardware

	movlw   B'01000001' ; select 8 * oscillator , analog input 0 , turn on
	movwf   ADCON0      ; move to special function A/D register

	call    ADDelay     ; delay for Tad prior to A/D start
	bsf     ADCON0,GO   ; start A/D conversion
	call    ADwaitLoop  ; since we might use this in the next modes, here define it as a subroutine

	btfsc   PORTC,1     ; see if red button pressed
	goto    RedPress2   ; go to red press mode 2
	goto    ModeTwo


ADwaitLoop

	btfsc   ADCON0,GO   ; check if A/D is finished
	goto    ADwaitLoop  
	return

RedPress2

	btfss   PORTC,1     ; see if red button released
	goto    ModeTwo     ; no , noise, keep checking

RedRelease2

	btfsc   PORTC,1     ; see if red button released
	goto    RedRelease2 ; no , keep waiting

	call    SwitchDelay

	bcf     STATUS,Z    ; clear zero flag in STATUS register, we wanna check if reading of ADC is 0
	movf    ADRESH,W    ; get A/D value, send it to w register
	iorlw   B'00000000' ; inclusive OR with W, to see if AD value is 0
; check if xorlw also works 1!!!!
	btfsc   STATUS,Z    ; if it is zero ( Z will be 1)
	goto    FaultInfo   ; a fault is indicated

	btfss   PORTD,1     ; check the solenoid
	call    SoleToEng   ; if disengage, make it engage
	call    EngTimer    ; timer for engagement of solenoid
	call    SoleToDis   ; after the given time, disengage the solenoid
	goto    ModeTwo     ; keep looping

EngTimer			;Engage Timer

	movf    ADRESH,W    ; get A/D value
	movwf   ADValue     ; send it to ADValue
	movwf   Count       ; same function with ADValue, but case study asks to do so


QuarterDelay
; 1/4s need approximately 83333 loops , it is 14585h in hex

	movlw   02h         ; get most significant hex value +1
	movwf   Timer2      ; 
	movlw   16h         ;
	movwf   Timer1
	movlw   15h
	movwf   Timer0

QuarterDelayLoop
	btfsc	PORTB,2		; if it is mode 4
	goto	NoInterruption; ignore the RedAgain
	btfsc   PORTC,1     ; check if press the red button again before time finishes
	goto    WTFRedAgain
NoInterruption
	decfsz  Timer0,F
	goto    QuarterDelayLoop
	decfsz  Timer1,F
	goto    QuarterDelayLoop
	decfsz  Timer2,F
	goto    QuarterDelayLoop

ADLoopTimes

	decfsz  ADValue,F   ; decrement ADValue, end when Advalue = 0
	goto    QuarterDelay
	return


WTFRedAgain

	btfss   PORTC,1     ; see if red button still pressed
	goto    ModeTwo     ; no ,noise, restart


WTFRedRelease

	goto	RedRelease2


;####################################################

ModeThree

	btfsc   PORTC,0     ;see if green button pressed
	goto    GreenPress  ; if pressed , go to GreenPress

	clrf	PORTD     ; init of PORTD, pin 4

	movlw   B'01000001' ; select 8 * oscillator , analog input 0 , turn on
	movwf   ADCON0      ; move to special function A/D register

	call    ADDelay     ; delay for Tad prior to A/D start
	bsf     ADCON0,GO   ; start A/D conversion
	call    ADwaitLoop  ; since we might use this in the next modes, here define it as a subroutine


	btfsc   PORTC,1     ; see if red button pressed
	goto    RedPress3   ; go to red press mode 3
	goto    ModeThree


RedPress3

	btfsc   PORTC,1     ; see if red button released
	goto    RedPress3   ; noise -- keep checking


RedRelease3
	btfsc	PORTC,1		; wait until I release the red button
	goto	RedRelease3 ; wait until I release the red button
	btfsc   PORTD,4     ; if PORTD , pin 4 is 0, turn it on,make control active
	goto    ModeThree   ; else goto control deactive part
	goto	CtrlAct     ; goto Control Active part

CtrlAct
	btfsc   PORTC,1     ; see if red button released
	goto    RedRelease3   ; noise -- keep checking
	bsf		PORTD,4		; light the PortD, pin 4, sign of active
	movf    ADRESH,W    ; get A/D value
	movwf   ADValue     ; send it to ADValue
	iorlw   B'00000000' ; inclusive OR with W, to see if AD value is 0
; check if xorlw also works 1!!!!
	btfsc   STATUS,Z    ; if it is zero ( Z will be 1)
	goto    FaultInfo   ; a fault is indicated
	movlw	B'01110000' ; set W as 70h
	subwf	ADValue,1	; substract W from ADvalue, save result in W
	btfsc	STATUS,C	; check the borrow register in Status
	call	SoleToEng	; if AdValue > 70h, engage
	btfss	STATUS,C	; 
	call	SoleToDis	; if AdValue < 70h, disengage
	call    ADDelay     ; delay for Tad prior to A/D start
	bsf     ADCON0,GO   ; start A/D conversion
	call    ADwaitLoop  ; since we might use this in the next modes, here define it as a subroutine
	goto	CtrlAct

;####################################################

ModeFour

	btfsc   PORTC,0     ; see if green button pressed;   no           
	goto    GreenPress

	clrf	PORTD		; initialize PORTD

	movlw   B'01000001' ; select 8 * oscillator , analog input 0 , turn on
	movwf   ADCON0      ; move to special function A/D register

	call    ADDelay     ; delay for Tad prior to A/D start
	bsf     ADCON0,GO   ; start A/D conversion
	call    ADwaitLoop  ; since we might use this in the next modes, here define it as a subroutine
;	movf    ADRESH,W    ; get A/D value
;	movwf   ADValue     ; send it to ADValue

	movf    ADRESH,W    ; get A/D value, send it to w register
	iorlw   B'00000000' ; inclusive OR with W, to see if AD value is 0
; check if xorlw also works 1!!!!
	btfsc   STATUS,Z    ; if it is zero ( Z will be 1)
	goto    FaultInfo   ; a fault is indicated

	btfsc   PORTC,1     ; see if red button pressed
	goto    RedPress4    ; if it is , go to redpress
	goto    ModeFour     ; keep checking


RedPress4

	btfss   PORTC,1     ; see if red button still pressed
	goto    ModeFour     ; noise - keep checking


RedRelease4

	btfsc   PORTC,1     ; see if red button released
	goto    RedRelease4  ; no , keep checking
	call    SwitchDelay ; let switch debounce
	btfss   PORTD,1     ; see if solenoid is engaged or disengaged , by Pin 1 in PORTD
	call    SoleToEng   ; if it is disengaged, make it engage
	call    EngTimer
	call    SoleToDis   ; if it is engaged, make it disengage

	goto    ModeFour    ; when everything is done, return to ModeOne, keep looping


;####################################################

;Delay

SwitchDelay

	movlw   D'20'
	movwf   Delay
	goto    GeneralDelay

ADDelay

	movlw   03h
	movwf   Delay
	goto    GeneralDelay

GeneralDelay

	decfsz  Delay, F
	goto    GeneralDelay
	return
;-----------------------------------------------

TenSecDelay
	movlw	0Ah
	movwf	OneSec
OneSecDelay
	movlw   02h         ; get most significant hex value +1
	movwf   Timer2      ; 
	movlw   16h         ;
	movwf   Timer1
	movlw   15h
	movwf   Timer0

LooopDelay

	decfsz  Timer0,F
	goto    LooopDelay
	decfsz  Timer1,F
	goto    LooopDelay
	decfsz  Timer2,F
	goto    LooopDelay

	decfsz  OneSec,F   ; decrement ADValue, end when Advalue = 0
	goto    OneSecDelay
	
	return



;##########################################
isrService

	goto	isrService

	END