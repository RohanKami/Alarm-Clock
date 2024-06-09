; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 2kHz square wave at pin P1.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'CLEAR' push button connected to P1.5 is pressed.
$NOLIST
$MODN76E003
$LIST

;  N76E003 pinout:
;                               -------
;       PWM2/IC6/T0/AIN4/P0.5 -|1    20|- P0.4/AIN5/STADC/PWM3/IC3
;               TXD/AIN3/P0.6 -|2    19|- P0.3/PWM5/IC5/AIN6
;               RXD/AIN2/P0.7 -|3    18|- P0.2/ICPCK/OCDCK/RXD_1/[SCL]
;                    RST/P2.0 -|4    17|- P0.1/PWM4/IC4/MISO
;        INT0/OSCIN/AIN1/P3.0 -|5    16|- P0.0/PWM3/IC3/MOSI/T1
;              INT1/AIN0/P1.7 -|6    15|- P1.0/PWM2/IC2/SPCLK
;                         GND -|7    14|- P1.1/PWM1/IC1/AIN7/CLO
;[SDA]/TXD_1/ICPDA/OCDDA/P1.6 -|8    13|- P1.2/PWM0/IC0
;                         VDD -|9    12|- P1.3/SCL/[STADC]
;            PWM5/IC7/SS/P1.5 -|10   11|- P1.4/SDA/FB/PWM1
;                               -------
;

CLK           EQU 16600000 ; Microcontroller system frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

ALARM  equ P3.0
UPDOWN        equ P1.6
SOUND_OUT     equ P1.7
TOGGLE_SWITCH equ P1.5
SECONDS_BUTTON		  equ P0.5
MINUTES_BUTTON	  equ P1.0
HOURS_BUTTON	  equ P1.6
PM_AM		  equ P1.2


; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
Seconds: 	  ds 1
Minutes:	  ds 1
Hours:		  ds 1
Cycle:		  ds 1 ; reserve 2 bytes for am or pm
Seconds_alarm: 	  ds 1
Minutes_alarm:	  ds 1
Hours_alarm:		  ds 1
Cycle_alarm:		  ds 1 ; reserve 2 bytes for am or pm



; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
full_second_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
switch_time_flag: dbit 1 ; flag used to see if we are in time switch mode
timer_flag: 	  dbit 1
alarm_flag:		  dbit 1
alarm_set_flag:	  dbit 1

cseg
; These 'equ' must match the hardware wiring
LCD_RS equ P1.3
;LCD_RW equ PX.X ; Not used in this code, connect the pin to GND
LCD_E  equ P1.4
LCD_D4 equ P0.0
LCD_D5 equ P0.1
LCD_D6 equ P0.2
LCD_D7 equ P0.3

$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
Initial_Message:  db 'xx : xx : xx xx', 0

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	orl CKCON, #0b00001000 ; Input for timer 0 is sysclk/1
	mov a, TMOD
	anl a, #0xf0 ; 11110000 Clear the bits for timer 0
	orl a, #0x01 ; 00000001 Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz wave at pin SOUND_OUT   ;
;---------------------------------;
Timer0_ISR:
	clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 doesn't have 16-bit auto-reload, so
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	
;	JB timer_flag, skipher
	jnb alarm_flag, skipher
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker the pin assigned to 'SOUND_OUT'!
	clr alarm_flag
	
skipher:
	reti
	
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov TH2, #high(TIMER2_RELOAD)
	mov TL2, #low(TIMER2_RELOAD)
	; Set the reload value
	orl T2MOD, #0x80 ; Enable timer 2 autoreload
	mov RCMP2H, #high(TIMER2_RELOAD)
	mov RCMP2L, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
	orl EIE, #0x80 ; Enable timer 2 interrupt ET2=1
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in the ISR.  It is bit addressable.
	cpl P0.4 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	

	jb switch_time_flag, I_Am_done
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	
	jnz Inc_Done
	inc Count1ms+1
	


Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	CJNE a, #low(1000), I_Am_Done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	CJNE a, #high(1000), I_Am_Done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb full_second_flag ; Let the main program know half second had passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a


	; Increment the BCD counter
	;This is where seconds logic starts

	mov a, Seconds
	add a, #0x01

	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Seconds, a
	CJNE a, #0x60, I_Am_Done

	MOV Seconds, #0x00
	MOV A, Minutes
	ADD A, #1
	DA A
	MOV Minutes, A
	CJNE A, #0x60, I_Am_Done

	MOV Minutes, #0x00
	MOV A, Hours
	ADD a, #1
	da A
	MOV Hours, A
	CJNE A, #0x13, I_Am_Done
	MOV Hours, #0x01
	
	
	

	

	

	
I_Am_Done:
	
	MOV A, seconds 
	CJNE A, #0x00, before_final_return
	MOV A, minutes
	CJNE A, #0x00, before_final_return
	MOV a, hours
	CJNE A, #0x12, before_final_return
	
	mov a, Cycle
	CJNE A, #0x01, switch
	mov Cycle, #0x00
	sjmp I_Am_Done
	
	switch:
	MOV a, #0x01
	MOV cycle, a


before_final_return:
	MOV A, Seconds
	CJNE A, Seconds_alarm, final_return
	MOV A, minutes
	CJNE A, minutes_alarm, final_return
	MOV a, hours
	CJNE A, hours_alarm, final_return
	MOV a, cycle
	CJNE a, Cycle_alarm, final_return
	setb alarm_flag

	ljmp final_return


final_return:
	pop psw
	pop acc
	reti

;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
	; Initialization
    mov SP, #0x7F
    mov P0M1, #0x00
    mov P0M2, #0x00
    mov P1M1, #0x00
    mov P1M2, #0x00
    mov P3M2, #0x00
    mov P3M2, #0x00
          
    lcall Timer0_Init
    lcall Timer2_Init
    setb EA   ; Enable Global interrupts
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#Initial_Message)
    setb full_second_flag
	mov Hours, #0x11
	mov Minutes, #0x59
	mov Seconds, #0x50
	MOV cycle, #0x00
	clr switch_time_flag
	clr timer_flag
	MOV Hours_alarm, #0x11
	mov Minutes_alarm, #0x59
	mov Seconds_alarm, #0x50
	mov Cycle_alarm, #0x00
	clr alarm_set_flag 
	
	
	; After initialization the program stays in this 'forever' loop
loop:
	
	
	jb TOGGLE_SWITCH, timer_button_loop  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb TOGGLE_SWITCH, timer_button_loop  ; if the 'CLEAR' button is not pressed skip
	jnb TOGGLE_SWITCH, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'CLEAR' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	setb TR2                ; Start timer 2
	jb switch_time_flag, set_time
	setb switch_time_flag
	ljmp timer_button_loop

set_time:
	clr switch_time_flag
	sjmp loop
	
timer_button_loop:

	jb alarm, switch_time  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb alarm, switch_time  ; if the 'CLEAR' button is not pressed skip
	jnb alarm, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'CLEAR' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	setb TR2                ; Start timer 2
	setb alarm_set_flag
	jb timer_flag, set_alarm
	setb timer_flag
	ljmp loop
	
	
set_alarm:
	clr timer_flag
	sjmp loop


loop_a:
	jb switch_time_flag, switch_time
	sjmp switch_time
		

	
switch_time:
	
	jnb switch_time_flag, switch_Minute
	jb timer_flag, switch_Minute
	jb SECONDS_BUTTON, switch_minute  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SECONDS_BUTTON,  switch_minute  ; if the 'CLEAR' button is not pressed skip
	jnb SECONDS_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction	
	mov a, Seconds
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Seconds, a
	CJNE a, #0x60, switch_minute
	MOV Seconds, #0x00



	
switch_minute:
	jnb switch_time_flag, switch_hour
	jb timer_flag, switch_hour
	jb MINUTES_BUTTON, switch_hour  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MINUTES_BUTTON, switch_hour  ; if the 'CLEAR' button is not pressed skip
	jnb MINUTES_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction	
	mov a, minutes
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov minutes, a
	CJNE a, #0x60, switch_hour
	MOV minutes, #0x00

switch_hour:	
	jnb switch_time_flag, switch_AM_PM
	jb timer_flag, alarm_set
	jb HOURS_BUTTON, switch_am_pm  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb HOURS_BUTTON, switch_am_pm  ; if the 'CLEAR' button is not pressed skip
	jnb HOURS_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction	
	mov a, hours
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov hours, a
	CJNE a, #0x13, switch_am_pm
	MOV hours, #0x01
	ljmp loop_b
	
switch_am_pm:
	jb timer_flag, alarm_set
	jnb PM_AM, alarm_set  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PM_AM, alarm_set  ; if the 'CLEAR' button is not pressed skip
	jnb PM_AM, $		; Wait for button release.  The '$' means: jump to same instruction	
	MOV A, Cycle
	CJNE A, #0x00, switch_flag
	MOV Cycle, #0x01
	ljmp alarm_set
	
switch_flag:
	MOV Cycle, #0x00
	jnb timer_flag, alarm_set
	
alarm_set:
	jb switch_time_flag, alarm_minute
	jb SECONDS_BUTTON, alarm_minute  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb SECONDS_BUTTON,  alarm_minute  ; if the 'CLEAR' button is not pressed skip
	jnb SECONDS_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction	
	mov a, Seconds_alarm
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Seconds_alarm, a
	CJNE a, #0x60, alarm_minute
	MOV Seconds_alarm, #0x00

alarm_minute:

	jb switch_time_flag, alarm_hour
	jb MINUTES_BUTTON, alarm_hour  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MINUTES_BUTTON, alarm_hour  ; if the 'CLEAR' button is not pressed skip
	jnb MINUTES_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction	
	mov a, Minutes_alarm
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Minutes_alarm, a
	CJNE a, #0x60, alarm_hour
	MOV Minutes_alarm, #0x00

alarm_hour:

	jb switch_time_flag, switch_am_pm_alarm
	jb HOURS_BUTTON, switch_am_pm_alarm  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb HOURS_BUTTON, switch_am_pm_alarm  ; if the 'CLEAR' button is not pressed skip
	jnb HOURS_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction	
	mov a, Hours_alarm
	add a, #0x01
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Hours_alarm, a
	CJNE a, #0x13, switch_am_pm_alarm
	MOV Hours_alarm, #0x01
	ljmp loop_b

switch_am_pm_alarm:

	jb switch_time_flag, loop_b
	jnb PM_AM, loop_b  ; if the 'CLEAR' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb PM_AM, loop_b  ; if the 'CLEAR' button is not pressed skip
	jnb PM_AM, $		; Wait for button release.  The '$' means: jump to same instruction	
	MOV A, Cycle_alarm
	CJNE A, #0x00, switch_flag_alarm
	MOV Cycle_alarm, #0x01
	ljmp loop_b
	
switch_flag_alarm:
	MOV Cycle_alarm, #0x00
	jnb timer_flag, loop_b
	
loop_b:

	jb timer_flag, loc
	;Modify here for displaying :)
	
    clr full_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 1)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Hours) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Minutes) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Seconds) ; This macro is also in 'LCD_4bit.inc'
loc: 

	Set_Cursor(2, 1)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'C') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 2)
	Display_char(#'l') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 3)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'o') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 4)
	Display_char(#'c') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 5)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'k') ; This macro is also in 'LCD_4bit.inc'


	jb timer_flag, nextloc
	mov a, cycle
	CJNE A, #0x01, AM
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'P') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 15)
	Display_char(#'M') ; This macro is also in 'LCD_4bit.inc'
	ljmp loop
	
AM:
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'A') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 15)
	Display_char(#'M') ; This macro is also in 'LCD_4bit.inc'
    ljmp loop

nextloc:

    clr full_second_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 1)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Hours_alarm) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Minutes_alarm) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 11)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(Seconds_alarm) ; This macro is also in 'LCD_4bit.inc'
	
	Set_Cursor(2, 1)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'T') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 2)
	Display_char(#'i') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 3)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'m') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 4)
	Display_char(#'e') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 5)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'r') ; This macro is also in 'LCD_4bit.inc'
	

	mov a, Cycle_alarm
	CJNE A, #0x01, AM1
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'P') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 15)
	Display_char(#'M') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(2, 1)

	ljmp loop
	
AM1:

	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_char(#'A') ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 15)
	Display_char(#'M') ; This macro is also in 'LCD_4bit.inc'
    ljmp loop


   





	

	


END
