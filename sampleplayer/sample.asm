

		FNAME "sample.bin"

		
fm1_reg:	equ	$c4
fm1_data:	equ	$c5
fm2_reg:	equ	$c6
fm2_data:	equ	$c7
pcm_reg:	equ	$7e
pcm_data:	equ	$7f


	
		db		$FE
		dw		start, end, start
	
		org		$8000
		
start:	call	convert			; convert sample 8 bits unsigned to 8 bits signed

		ld      a,5
        out     (fm2_reg),a
        ld      a,3
        out     (fm2_data),a	; initialize OPL4

		ld		e,$20
		ld		hl,$0600
		call	set_opl4_wrt	; set write address to $200600 (first 2MB of data is ROM, in other 2MB of address range sample data can be writen)
		
		ld		hl,sample
		ld		de,end - sample
		call	ramtosram		; move sample data
		
		ld		e,$20
		ld		hl,0			
		call	set_opl4_wrt	; address for tone headers for custom tones is $200000
		
		ld		hl,tonedata
		ld		de,12
		call	ramtosram		; store tonedata in SRAM
		
		ld		c,2
		ld		a,%10000
		call	opl4_out_wave	; disable SRAM access	
		
		ld		c,$20
		ld		a,%00000001		; lowest 7 bits of frequency / highest bit of tone number to play
		call	opl4_out_wave
		
		ld		c,$08
		ld		a,%10000000		; lowest 8 bits of tone number to play (of 9 bits)
		call	opl4_out_wave
				
		ld		c,$38
		ld		a,%11110000		; octave / pseudoreverb / lowest 3 bits of frequency
		call	opl4_out_wave
		
		ld		c,$50
		ld		a,0				; total level / level direct
		call	opl4_out_wave
		
		ld		c,$68
		ld		a,0				; key off, tone will only be played if status changes from OFF to ON
		call	opl4_out_wave
		
		ld		c,$68
		ld		a,%10000000		; key on / damp / lfo reset / output channel selection / panpot
		call	opl4_out_wave
		
		ret
		
		
tonedata:	db	$20,$06,$00												; 8bit sample / start address
			db	(end-sample)>>8,		(end-sample) and 255			; loop address
			db	not((end-sample)>>8), 	not((end-sample) and 255)		; end address (invert all bits of sample length)
			db	0						; LFO / VIB
			db	%11110000				; AT / D1R
			db	%00000000				; DL / D2R
			db	%00001111				; RC / RR
			db	%00000000				; AM
			
			
;-------------------------------------------------------
;output data to OPL4
;In: C: register
;    A: data
;Out:
;Changes: af'
opl4_out_wave:
	ex	af,af'
	ld	a,c
	out	(pcm_reg),a
	ex	af,af'
	nop
	out	(pcm_data),a
	ret

	
;-------------------------------------------------------
;move data in ram to SRAM
;In: HL: source data
;    DE: size
;Out: -
;Changes: AF
ramtosram:		
	ld	c,pcm_data
ramtosram_lp:	
	in	a,(fm1_reg)
	rra
	jr	c,ramtosram_lp
	outi
	dec	de
	ld	a,d
	or	e
	jr	nz,ramtosram_lp
	ret
	
	
;-------------------------------------------------------
;set SRAM read/write address
;In: EHL = SRAM address
;Out: C = wave data port (#7f)
;Changes: AF, AF'
set_opl4_wrt:
	ld	c,2    
	ld	a,10001b
	call	opl4_out_wave	;enable SRAM access

	inc	c
	ld	a,e
	and	111111b
	call	opl4_out_wave	;set bit 21-16 address

	inc	c
	ld	a,h
	call	opl4_out_wave	;set bit 15-8 address

	inc	c
 	ld	a,l
	call	opl4_out_wave	;set bit 7-0 address

	ld	a,6
	out	(pcm_reg),a		;SRAM access

	ld	c,pcm_data		;data port
	ret

	
convert:	
		ld	hl,sample
		ld	bc,end-sample
.loop:	
		ld	a,(hl)
		sub	128
		ld	(hl),a
		inc	hl
		dec bc
		ld	a,b
		or	c
		jr	nz,.loop
		ret
		

sample:	INCBIN	"sample.raw"

end:
