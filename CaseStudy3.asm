LIST P = 16F747
title "Case Study 3"

#include <P16F747.INC>

___CONFIG _CONFIG1, _FOSC_HS & _CP_OFF & _DEBUG_OFF & _VBOR_2_0 & _BOREN_0 & _MCLR_ON & _PWRTE_ON & _WDT_OFF

___CONFIG _CONFIG2, _BORSEN_0 & _IESO_OFF &_FCMEN_OFF


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
State   equ 22h ; Program state register
Delay   equ 23h ; delay for switch
ADValue equ 24h ; value read from AD
Timer2  equ 25h
Timer1  equ 26h
Timer0  equ 27h

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
movlw   B'00000100' ; RA0,RA1,RA3 analog inputs, all other digital
movwf   ADCON1      ; move to special function A/D register
movlw   B'00000010' ; move 0x02 into W register
movwf   TRISD       ; Port D pin 1 input, others output
bcf     STATUS,RP0  ; select register bank 0
clrf    Count       ; zero the counter
clrf    State


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
;clrf    PORTD       ; turn off the solenoid
goto    ModeSelection


ModeSelection

;this part is f**king silly, but I just wanna make it right

;-----------------------------------------------------------
; movfw   PORTE       ; read value from octal switch
; bcf     STATUS,Z    ; clear zero flag in STATUS register
; xorlw   B'00000111' ; compare with 0x07 to see if is Mode 0
; btfsc   STATUS,Z    ; if it is Mode 0 ( Z will be 1)
; goto    FaultInfo   ; go to Fault processing subroutine

; movfw   PORTE       ; read value from octal switch
; bcf     STATUS,Z    ; clear zero flag in STATUS register
; xorlw   B'00000010' ; compare with 0x02 to see if is Mode 5
; btfsc   STATUS,Z    ; if it is Mode 5 ( Z will be 1)
; goto    FaultInfo   ; go to Fault processing subroutine

; movfw   PORTE       ; read value from octal switch
; bcf     STATUS,Z    ; clear zero flag in STATUS register
; xorlw   B'00000001' ; compare with 0x01 to see if is Mode 6
; btfsc   STATUS,Z    ; if it is Mode 6 ( Z will be 1)
; goto    FaultInfo   ; go to Fault processing subroutine

; movfw   PORTE       ; read value from octal switch
; bcf     STATUS,Z    ; clear zero flag in STATUS register
; xorlw   B'00000000' ; compare with 0x00 to see if is Mode 7
; btfsc   STATUS,Z    ; if it is Mode 7 ( Z will be 1)
; goto    FaultInfo   ; go to Fault processing subroutine
;-----------------------------------------------------------

comf    PORTE,0     ; compliment PORTE into w register
movfw   PORTB       ; send the right mode num to LED indicator
bcf     PORTB,3     ; no error indicated on pin 3 LED indicator

movwf   State       ; move the mode state to State register
bcf     STATUS,Z
movlw   B'11111001' ; move 0xF1 (mode 1) to w register
xorfw   State,0     ; compare with w to see if it is mode 1
btfsc   STATUS,Z    ; if it is mode 1 (Z will be 1)
goto    ModeOne     ; goto Mode One

bcf     STATUS,Z
movlw   B'11111010' ; move 0xF2 (mode 2) to w register
xorfw   State,0     ; compare with w to see if it is mode 2
btfsc   STATUS,Z    ; if it is mode 2 (Z will be 1)
goto    ModeTwo     ; goto Mode Two

bcf     STATUS,Z
movlw   B'11111011' ; move 0xF3 (mode 3) to w register
xorfw   State,0     ; compare with w to see if it is mode 3
btfsc   STATUS,Z    ; if it is mode 3 (Z will be 1)
goto    ModeThree   ; goto Mode Three

bcf     STATUS,Z
movlw   B'11111100' ; move 0xF4 (mode 4) to w register
xorfw   State,0     ; compare with w to see if it is mode 4
btfsc   STATUS,Z    ; if it is mode 4 (Z will be 1)
goto    ModeFour    ; goto Mode Four


;#######################################################


; Fault Processing


FaultInfo

;movwf   PORTE,0     ; read PORTE value to w register
comf    PORTE,0     ; compliment PortE to w register
movwf   PORTB       ; send the right LED indicator to PORTB
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
movewf  Timer2      
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


;######################################################

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

;##########################################################

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
goto    SoleToEng   ; if it is disengaged, make it engage
goto    SoleToDis   ; if it is engaged, make it disengage

goto    ModeOne    ; when everything is done, return to ModeOne, keep looping

;###########################################################

SoleToEng
; make solenoid engage

bsf     PORTD,0     ; make it engage
btfsc   PORTD,1     ; check if it is engaged , this pin is connected to LM311 as an input
goto    TrnReduced              ; if it is, return 
goto SoleToEng      ; if not, keep looping

TrnReduced

bsf     PORTD,2     ; turn on the reduced transistor
bcf     PORTD,0     ; turn off the main transistor
goto    ModeOne

SoleToDis
; make solenoid disengage

bcf     PORTD,0     ; make it disengage
bcf     PORTD,2     ; turn off the reduced transistor
btfss   PORTD,1     ; check if it is disengaged
goto    ModeOne              ; if it is , return
goto    SoleToDis   ; if not , keep looping


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
btfsc   ADCON0,GO   ; make sure A/D finished
goto    ADwaitLoop  ; else keep looping

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

btfss   PORTD,0     ; check the solenoid
call    SoleToEng   ; if disengage, make it engage
call    EngTimer    ; timer for engagement of solenoid
call    SoleToDis   ; after the given time, disengage the solenoid
goto    ModeTwo     ; keep looping

EngTimer

movf    ADRESH,W    ; get A/D value
movwf   ADValue     ; send it to ADValue

QuarterDelay
; 1/4s need approximately 83333 loops , it is 14585h in hex

movlw   02h         ; get most significant hex value +1
movwf   Timer2      ; 
movlw   45h         ;
movwf   Timer1
movlw   85h
movwf   Timer0

QuarterDelayLoop

btfsc   PORTC,1     ; check if press the red button again before time finishes
goto    WTFRedAgain
decfsz  Timer0,F
goto    QuarterDelayLoop
decfsz  Timer1,F
goto    QuarterDelayLoop
decfsz  Timer2,F
goto    QuarterDelayLoop

ADLoopTimes

decfsz  ADValue,F   ; decrement ADValue, end when Advalue = 0
goto    QuarterDelayLoop
return


WTFRedAgain

btfss   PORTC,1     ; see if red button still pressed
goto    ModeTwo     ; no ,noise, restart

WTFRedRelease

btfsc   PORTC,0     ; see if red button released
goto    RedRelease2 ; wait until released
call    SwitchDelay ; let switch debounce
goto    QuarterDelay; restart the timer




;####################################################
; Temporary Mode 3, use Mode 1 instead

ModeThree

;below two expressions might be useless, just follow the Counter.asm

btfsc   PORTC,0     ; see if green button pressed;   no           
goto    GreenPress3
btfsc   PORTC,1     ; see if red button pressed
goto    RedPress3    ; if it is , go to redpress
goto    ModeThree     ; keep checking


RedPress3

btfss   PORTC,1     ; see if red button still pressed
goto    ModeThree     ; noise - keep checking


RedRelease3

btfsc   PORTC,1     ; see if red button released
goto    RedRelease3  ; no , keep checking
call    SwitchDelay ; let switch debounce
btfss   PORTD,0     ; see if solenoid is engaged or disengaged
goto    SoleToEng3   ; if it is disengaged, make it engage
goto    SoleToDis3   ; if it is engaged, make it disengage

goto    ModeThree    ; when everything is done, return to ModeOne, keep looping

;###########################################################

SoleToEng3
; make solenoid engage

bsf     PORTD,0     ; make it engage
btfsc   PORTD,0     ; check if it is engaged
return              ; if it is, return 
goto SoleToEng3      ; if not, keep looping


SoleToDis3
; make solenoid disengage

bcf     PORTD,0     ; make it disengage
bcf     PORTD,2     ; turn off the reduced transistor
btfss   PORTD,0     ; check if it is disengaged
return              ; if it is , return
goto    SoleToDis3   ; if not , keep looping




;#####################################################
; Temporary Mode 4 , still use Mode one instead

ModeFour

;below two expressions might be useless, just follow the Counter.asm

btfsc   PORTC,0     ; see if green button pressed;   no           
goto    GreenPress4
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
btfss   PORTD,0     ; see if solenoid is engaged or disengaged
goto    SoleToEng4   ; if it is disengaged, make it engage
goto    SoleToDis4   ; if it is engaged, make it disengage

goto    ModeFour    ; when everything is done, return to ModeOne, keep looping

;###########################################################

SoleToEng4
; make solenoid engage

bsf     PORTD,0     ; make it engage
btfsc   PORTD,0     ; check if it is engaged
return              ; if it is, return 
goto SoleToEng4      ; if not, keep looping


SoleToDis4
; make solenoid disengage

bcf     PORTD,0     ; make it disengage
bcf     PORTD,2     ; turn off the reduced transistor
btfss   PORTD,0     ; check if it is disengaged
return              ; if it is , return
goto    SoleToDis4   ; if not , keep looping





;##########################################
isrService

goto	isrService

END