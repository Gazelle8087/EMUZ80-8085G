/*
 * Copyright (c) 2023 @Gazelle8087
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*!
 * SBC8085 ROM RAM and UART emulation firmware
 * with extended universal monitor and Grants Basic for 8080
 * 
 * Target: EMUZ80 with MEZ8085 and PIC18F47Q84
 * IDE: MPLAB X v6.0
 * Compiler: MPLAB XC8 v2.36
 * 
 * Modified by Gazelle https://twitter.com/Gazelle8087
 * 2022/10/30 firmware for emuz80 + MEZ8085 first release 
 * 2022/10/31 Correction of comments, change initial clock speed 4.7MHz to 4.0MHz
 * 2022/11/10 extended universal monitor for 8080 Beta0 build in
 * 2022/12/1  Import EMUZ80 MONITOR Rev.B02.2 revision(unimon_8080x.asm)
 *            Z80 code translation optimize(MSBAS80_2200.asm)
 *            Refined ROM allocation declaration(then no warning during build) 
 * 2022/12/18 Inport EMUZ80 monitor Rev.B03 by Aki.h 
 *            (emu8085mon_RevB03_8k.asm rename and patch z80 code by 8080 code)
 * 2022/12/29 Z80 code worng replacement was fixed
 *				(SBC HL,BC & SBC HL,DE)
 * 2023/12/15 0wait ROM read, RAM write. 1wait RAM read
 *
 * These source and HEX can be modified and redistributed
 * under the GPL license.
 * 
 * Original source Written by Tetsuya Suzuki
 * https://github.com/vintagechips/emuz80
 * Target: EMUZ80 - The computer with only Z80 and PIC18F47Q43
 *   
 * emuz80 + MEZ8085 firmware Modified by Satoshi Okue 
 * https://github.com/satoshiokue/EMUZ80-8085
 *
 * Universal Monitor for Zilog Z80  by Haruo Asano
 * https://electrelic.com/electrelic/node/1317
 *
 * extended Universal monitor for emuz80 by A.honda
 * https://github.com/akih-san/EMUZ80-MON/tree/main/Rev.B02.2
 *
 * Grants BASIC for 8080 by Tetsuya Suzuki
 * http://www.amy.hi-ho.ne.jp/officetetsu/storage/sbc8080_datapack.zip
 *  
 * firmware source code for emuz80 written by yyhayami
 * for the CLC configuration and memory mapping/access procedure.
 * https://github.com/yyhayami/emuz80_hayami/tree/main/emuz80_clc.X 
 */


// CONFIG1
#pragma config FEXTOSC = OFF    // External Oscillator Selection (Oscillator not enabled)
#pragma config RSTOSC = HFINTOSC_64MHZ// Reset Oscillator Selection (HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1)

// CONFIG2
#pragma config CLKOUTEN = OFF   // Clock out Enable bit (CLKOUT function is disabled)
#pragma config PR1WAY = ON      // PRLOCKED One-Way Set Enable bit (PRLOCKED bit can be cleared and set only once)
#pragma config CSWEN = ON       // Clock Switch Enable bit (Writing to NOSC and NDIV is allowed)
#pragma config FCMEN = ON       // Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor enabled)
#pragma config JTAGEN = OFF
#pragma config FCMENP = OFF
#pragma config FCMENS = OFF

// CONFIG3
#pragma config MCLRE = EXTMCLR  // MCLR Enable bit (If LVP = 0, MCLR pin is MCLR; If LVP = 1, RE3 pin function is MCLR )
#pragma config PWRTS = PWRT_OFF // Power-up timer selection bits (PWRT is disabled)
#pragma config MVECEN = ON      // Multi-vector enable bit (Multi-vector enabled, Vector table used for interrupts)
#pragma config IVT1WAY = ON     // IVTLOCK bit One-way set enable bit (IVTLOCKED bit can be cleared and set only once)
#pragma config LPBOREN = OFF    // Low Power BOR Enable bit (Low-Power BOR disabled)
#pragma config BOREN = SBORDIS  // Brown-out Reset Enable bits (Brown-out Reset enabled , SBOREN bit is ignored)

// CONFIG4
#pragma config BORV = VBOR_1P9  // Brown-out Reset Voltage Selection bits (Brown-out Reset Voltage (VBOR) set to 1.9V)
#pragma config ZCD = OFF        // ZCD Disable bit (ZCD module is disabled. ZCD can be enabled by setting the ZCDSEN bit of ZCDCON)
#pragma config PPS1WAY = OFF    // PPSLOCK bit One-Way Set Enable bit (PPSLOCKED bit can be set and cleared repeatedly (subject to the unlock sequence))
#pragma config STVREN = ON      // Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
#pragma config LVP = ON         // Low Voltage Programming Enable bit (Low voltage programming enabled. MCLR/VPP pin function is MCLR. MCLRE configuration bit is ignored)
#pragma config XINST = OFF      // Extended Instruction Set Enable bit (Extended Instruction Set and Indexed Addressing Mode disabled)

// CONFIG5
#pragma config WDTCPS = WDTCPS_31// WDT Period selection bits (Divider ratio 1:65536; software control of WDTPS)
#pragma config WDTE = OFF       // WDT operating mode (WDT Disabled; SWDTEN is ignored)

// CONFIG6
#pragma config WDTCWS = WDTCWS_7// WDT Window Select bits (window always open (100%); software control; keyed access not required)
#pragma config WDTCCS = SC      // WDT input clock selector (Software Control)

// CONFIG7
#pragma config BBSIZE = BBSIZE_512// Boot Block Size selection bits (Boot Block size is 512 words)
#pragma config BBEN = OFF       // Boot Block enable bit (Boot block disabled)
#pragma config SAFEN = OFF      // Storage Area Flash enable bit (SAF disabled)

// CONFIG8
#pragma config WRTB = OFF       // Boot Block Write Protection bit (Boot Block not Write protected)
#pragma config WRTC = OFF       // Configuration Register Write Protection bit (Configuration registers not Write protected)
#pragma config WRTD = OFF       // Data EEPROM Write Protection bit (Data EEPROM not Write protected)
#pragma config WRTSAF = OFF     // SAF Write protection bit (SAF not Write Protected)
#pragma config WRTAPP = OFF     // Application Block write protection bit (Application Block not write protected)

// CONFIG9
#pragma config BOOTPINSEL = RC5 // CRC on boot output pin selection (CRC on boot output pin is RC5)
#pragma config BPEN = OFF       // CRC on boot output pin enable bit (CRC on boot output pin disabled)
#pragma config ODCON = OFF      // CRC on boot output pin open drain bit (Pin drives both high-going and low-going signals)

// CONFIG10
#pragma config CP = OFF         // PFM and Data EEPROM Code Protection bit (PFM and Data EEPROM code protection disabled)

// CONFIG11
#pragma config BOOTSCEN = OFF   // CRC on boot scan enable for boot area (CRC on boot will not include the boot area of program memory in its calculation)
#pragma config BOOTCOE = HALT   // CRC on boot Continue on Error for boot areas bit (CRC on boot will stop device if error is detected in boot areas)
#pragma config APPSCEN = OFF    // CRC on boot application code scan enable (CRC on boot will not include the application area of program memory in its calculation)
#pragma config SAFSCEN = OFF    // CRC on boot SAF area scan enable (CRC on boot will not include the SAF area of program memory in its calculation)
#pragma config DATASCEN = OFF   // CRC on boot Data EEPROM scan enable (CRC on boot will not include data EEPROM in its calculation)
#pragma config CFGSCEN = OFF    // CRC on boot Config fuses scan enable (CRC on boot will not include the configuration fuses in its calculation)
#pragma config COE = HALT       // CRC on boot Continue on Error for non-boot areas bit (CRC on boot will stop device if error is detected in non-boot areas)
#pragma config BOOTPOR = OFF    // Boot on CRC Enable bit (CRC on boot will not run)

// CONFIG12
#pragma config BCRCPOLT = hFF   // Boot Sector Polynomial for CRC on boot bits 31-24 (Bits 31:24 of BCRCPOL are 0xFF)

// CONFIG13
#pragma config BCRCPOLU = hFF   // Boot Sector Polynomial for CRC on boot bits 23-16 (Bits 23:16 of BCRCPOL are 0xFF)

// CONFIG14
#pragma config BCRCPOLH = hFF   // Boot Sector Polynomial for CRC on boot bits 15-8 (Bits 15:8 of BCRCPOL are 0xFF)

// CONFIG15
#pragma config BCRCPOLL = hFF   // Boot Sector Polynomial for CRC on boot bits 7-0 (Bits 7:0 of BCRCPOL are 0xFF)

// CONFIG16
#pragma config BCRCSEEDT = hFF  // Boot Sector Seed for CRC on boot bits 31-24 (Bits 31:24 of BCRCSEED are 0xFF)

// CONFIG17
#pragma config BCRCSEEDU = hFF  // Boot Sector Seed for CRC on boot bits 23-16 (Bits 23:16 of BCRCSEED are 0xFF)

// CONFIG18
#pragma config BCRCSEEDH = hFF  // Boot Sector Seed for CRC on boot bits 15-8 (Bits 15:8 of BCRCSEED are 0xFF)

// CONFIG19
#pragma config BCRCSEEDL = hFF  // Boot Sector Seed for CRC on boot bits 7-0 (Bits 7:0 of BCRCSEED are 0xFF)

// CONFIG20
#pragma config BCRCEREST = hFF  // Boot Sector Expected Result for CRC on boot bits 31-24 (Bits 31:24 of BCRCERES are 0xFF)

// CONFIG21
#pragma config BCRCERESU = hFF  // Boot Sector Expected Result for CRC on boot bits 23-16 (Bits 23:16 of BCRCERES are 0xFF)

// CONFIG22
#pragma config BCRCERESH = hFF  // Boot Sector Expected Result for CRC on boot bits 15-8 (Bits 15:8 of BCRCERES are 0xFF)

// CONFIG23
#pragma config BCRCERESL = hFF  // Boot Sector Expected Result for CRC on boot bits 7-0 (Bits 7:0 of BCRCERES are 0xFF)

// CONFIG24
#pragma config CRCPOLT = hFF    // Non-Boot Sector Polynomial for CRC on boot bits 31-24 (Bits 31:24 of CRCPOL are 0xFF)

// CONFIG25
#pragma config CRCPOLU = hFF    // Non-Boot Sector Polynomial for CRC on boot bits 23-16 (Bits 23:16 of CRCPOL are 0xFF)

// CONFIG26
#pragma config CRCPOLH = hFF    // Non-Boot Sector Polynomial for CRC on boot bits 15-8 (Bits 15:8 of CRCPOL are 0xFF)

// CONFIG27
#pragma config CRCPOLL = hFF    // Non-Boot Sector Polynomial for CRC on boot bits 7-0 (Bits 7:0 of CRCPOL are 0xFF)

// CONFIG28
#pragma config CRCSEEDT = hFF   // Non-Boot Sector Seed for CRC on boot bits 31-24 (Bits 31:24 of CRCSEED are 0xFF)

// CONFIG29
#pragma config CRCSEEDU = hFF   // Non-Boot Sector Seed for CRC on boot bits 23-16 (Bits 23:16 of CRCSEED are 0xFF)

// CONFIG30
#pragma config CRCSEEDH = hFF   // Non-Boot Sector Seed for CRC on boot bits 15-8 (Bits 15:8 of CRCSEED are 0xFF)

// CONFIG31
#pragma config CRCSEEDL = hFF   // Non-Boot Sector Seed for CRC on boot bits 7-0 (Bits 7:0 of CRCSEED are 0xFF)

// CONFIG32
#pragma config CRCEREST = hFF   // Non-Boot Sector Expected Result for CRC on boot bits 31-24 (Bits 31:24 of CRCERES are 0xFF)

// CONFIG33
#pragma config CRCERESU = hFF   // Non-Boot Sector Expected Result for CRC on boot bits 23-16 (Bits 23:16 of CRCERES are 0xFF)

// CONFIG34
#pragma config CRCERESH = hFF   // Non-Boot Sector Expected Result for CRC on boot bits 15-8 (Bits 15:8 of CRCERES are 0xFF)

// CONFIG35
#pragma config CRCERESL = hFF   // Non-Boot Sector Expected Result for CRC on boot bits 7-0 (Bits 7:0 of CRCERES are 0xFF)
// #pragma config statements should precede project file includes.
// Use project enums instead of #define for ON and OFF.

#include <xc.h>
#include <stdio.h>

#ifdef _18F47Q43
#define RAM_SIZE 0x1000		// 4kB
#else
#define RAM_SIZE 0x2000		// 8kB
#endif
#define ROM_TOP 0x0000		//ROM top address

#define UART_DREG 0x00		//Data REG
#define UART_CREG 0x01		//Control REG

#define _XTAL_FREQ 64000000UL

//8085 ROM equivalent, see end of this file
//extern const unsigned char rom[];

//8085 RAM equivalent
unsigned char ram[RAM_SIZE] __at(0x600);

// UART3 Transmit
void putch(char c) {
	while(!U3TXIF);		// Wait or Tx interrupt flag set
	U3TXB = c;			// Write data
}
/*// UART3 Recive
char getch(void) {
	while(!U3RXIF);		// Wait for Rx interrupt flag set
	return U3RXB;		// Read data
}*/

// Never called, logically
void __interrupt(irq(default),base(8)) Default_ISR(){}

// main routine
void main(void) {

	// System initialize
	OSCFRQ = 0x08;		// 64MHz internal OSC

	// RESETIN(RE2) output pin
	ANSELE2 = 0;		// Disable analog function
	LATE2 = 0;			// Reset=Low
	TRISE2 = 0;			// Set as output

	// 8085 clock(RA5) by NCO FDC mode
	RA5PPS = 0x3f;		// RA5 assign NCO1
	ANSELA5 = 0;		// Disable analog function
	TRISA5 = 0;			// NCO output pin
	NCO1INC = 0x20000;	// CPU clock 2.0MHz
	NCO1CLK = 0x00;		// Clock source Fosc
	NCO1PFM = 0;		// FDC mode
	NCO1OUT = 1;		// NCO output enable
	NCO1EN = 1;			// NCO enable

	// Address bus A15-A8 pin
	ANSELD = 0x00;		// Disable analog function
	WPUD = 0xff;		// Week pull up
	TRISD = 0xff;		// Set as input

	// Address bus AD7-AD0 pin
	ANSELB = 0x00;		// Disable analog function
	WPUB = 0xff;		// Week pull up
	TRISB = 0xff;		// Set as input

	// RD(RA0) input pin
	ANSELA0 = 0;		// Disable analog function
	WPUA0 = 1;			// Week pull up
	TRISA0 = 1;			// Set as intput

	// WT (RA1) input pin
	ANSELA1 = 0;		// Disable analog function
	WPUA1 = 1;			// Week pull up
	TRISA1 = 1;			// Set as intput

	// IO/M (RA2) input pin
	ANSELA2 = 0;		// Disable analog function
	WPUA2 = 1;			// Week pull up
	TRISA2 = 1;			// Set as intput

	// ALE (RA3)  input pin
	ANSELA3 = 0;		// Disable analog function
	WPUA3 = 1;			// Week pull up
	TRISA3 = 1;			// Set as intput

	// READY (RA4) output pin
	ANSELA4 = 0;		// Disable analog function
	LATA4 = 1;
	TRISA4 = 0;			// Set as output

	// S0 (RC0)  input pin
	ANSELC0 = 0;		// Disable analog function
	WPUC0 = 1;			// Week pull up
	TRISC0 = 1;			// Set as intput

	// S1 (RC1)  input pin
	ANSELC1 = 0;		// Disable analog function
	WPUC1 = 1;			// Week pull up
	TRISC1 = 1;			// Set as intput

	// TRAP (RC2) output pin
	ANSELC2 = 0;		// Disable analog function
	LATC2 = 0;
	TRISC2 = 0;			// Set as output

	// RST5.5 (RC3) output pin
	ANSELC3 = 0;		// Disable analog function
	LATC3 = 0;
	TRISC3 = 0;			// Set as output

	// RST6.5 (RC4) output pin
	ANSELC4 = 0;		// Disable analog function
	LATC4 = 0;
	TRISC4 = 0;		// Set as output

	// RST7.5 (RC5) output pin
	ANSELC5 = 0;		// Disable analog function
	LATC5 = 0;
	TRISC5 = 0;			// Set as output

	// INTA (RC6)  input pin
	ANSELC6 = 0;		// Disable analog function
	WPUC6 = 1;			// Week pull up
	TRISC6 = 1;			// Set as intput

	// HOLD (RC7) output pin
	ANSELC7 = 0;		// Disable analog function
	LATC7 = 0;
	TRISC7 = 0;			// Set as output

	// HLDA (RE0)  input pin
	ANSELE0 = 0;		// Disable analog function
	WPUE0 = 1;			// Week pull up
	TRISE0 = 1;			// Set as intput

	// INTR (RE1) output pin
	ANSELE1 = 0;		// Disable analog function
	LATE1 = 0; 
	TRISE1 = 0;			// Set as output

	// UART3 initialize
	U3BRG = 416;		// 9600bps @ 64MHz
	U3RXEN = 1;			// Receiver enable
	U3TXEN = 1;			// Transmitter enable

	// UART3 Receiver
	ANSELA7 = 0;		// Disable analog function
	TRISA7 = 1;			// RX set as input
	U3RXPPS = 0x07;		// RA7->UART3:RX3;

	// UART3 Transmitter
	ANSELA6 = 0;		// Disable analog function
	LATA6 = 1;			// Default level
	TRISA6 = 0;			// TX set as output
	RA6PPS = 0x26;		// RA6->UART3:TX3;

	U3ON = 1;			// Serial port enable

//========== CLC pin assign ===========    
	CLCIN0PPS = 0x03;	// RA3 <- ALE
	CLCIN1PPS = 0x02;	// RA2 <- IO/M
	CLCIN2PPS = 0x1f;	// RD7 <- A15(A7 for IO)
	CLCIN3PPS = 0x1e;	// RD6 <- A14(A6 for IO)
	CLCIN4PPS = 0x00;	// RA0 <- /RD
	CLCIN5PPS = 0x01;	// RA1 <- /WR
	CLCIN6PPS = 0x1d;	// RD5 <- A13(A5 for IO)
	CLCIN7PPS = 0x1c;	// RD4 <- A12(A4 for IO)

	RA4PPS = 0x06;		// CLC6 -> RA4 -> READY

//========== CLC1 RAM /RD interrupt falling edge ==========
	CLCSELECT = 0;		// select CLC1  

	CLCnSEL0 = 4;		// CLCIN4PPS <- /RD
	CLCnSEL1 = 1;		// CLCIN1PPS <- IO/M 
	CLCnSEL2 = 54;		// CLC4 (RAM address decode)
	CLCnSEL3 = 127;		// NC

	CLCnGLS0 = 0x0A;	// CLK <- /RD no inv + IO/M no inv
	CLCnGLS1 = 0x10;	// D <- CLC4 inv
	CLCnGLS2 = 0;		// R 0 (controlled by firm ware)
	CLCnGLS3 = 0;		// S 0

	CLCnPOL = 0x83;		// Q invert CLK invert D invert
	CLCnCON = 0x8c;		// D-FF, falling edge interrupt

//========== CLC2 ROM /RD interrupt falling edge ==========
	CLCSELECT = 1;		// select CLC2

	CLCnSEL0 = 4;		// CLCIN4PPS <- /RD
	CLCnSEL1 = 1;		// CLCIN1PPS <- IO/M 
	CLCnSEL2 = 2;		// CLCIN2PPS <- A15
	CLCnSEL3 = 127;		// NC

	CLCnGLS0 = 0x01;	// /RD inv
	CLCnGLS1 = 0x04;	// /IO/M inv
	CLCnGLS2 = 0x10;	// A15 inv
	CLCnGLS3 = 0x40;	// 1(0 inv)

	CLCnPOL = 0x80;		// OUT invert (generate 4 input NAND)
	CLCnCON = 0x8A;		// 4 input AND, falling edge interrupt

//========== CLC3 RAM /WT interrupt falling edge ==========
	CLCSELECT = 2;		// select CLC3  

	CLCnSEL0 = 5;		// CLCIN5PPS <- /WR
	CLCnSEL1 = 1;		// CLCIN1PPS <- IO/M 
	CLCnSEL2 = 54;		// CLC4 (RAM address decode)
	CLCnSEL3 = 127;		// NC

	CLCnGLS0 = 0x01;	// /WR inv
	CLCnGLS1 = 0x04;	// /IO/M inv
	CLCnGLS2 = 0x20;	// A15 inv
	CLCnGLS3 = 0x40;	// 1(0 inv)

	CLCnPOL = 0x80;		// OUT inv (generate 4 input NAND)
	CLCnCON = 0x8A;		// 4 input AND, falling edge interrupt

//========== CLC4 RAM address decord 0x8000 - 0x9FFF ==========
	// 8000-9FFF CLC1OUT = 1 (in case of Q84,Q83)
	// 8000-8FFF CLC1OUT = 1 (in case of Q43)

	CLCSELECT = 3;		// select CLC4

	CLCnSEL0 = 2;		// CLCIN2PPS <- A15
	CLCnSEL1 = 3;		// CLCIN3PPS <- A14
	CLCnSEL2 = 6;		// CLCIN6PPS <- A13
#ifndef _18F47Q43
	CLCnSEL3 = 127;		// NC
#else
	CLCnSEL3 = 7;		// CLCIN7PPS <- A12
#endif

	CLCnGLS0 = 0x02;	// A15 no inv
	CLCnGLS1 = 0x04;	// A14 inv
	CLCnGLS2 = 0x10;	// A13 inv
	CLCnGLS3 = 0x40;	// 1(Q83,84) or A12(Q43) inv

	CLCnPOL = 0x00;     // No inv
	CLCnCON = 0x82;     // 4 input AND, no interrupt

//========== CLC5 IO /RD interrupt falling edge  ==========
	CLCSELECT = 4;		// select CLC5  

	CLCnSEL0 = 4;		// CLCIN4PPS <- /RD
	CLCnSEL1 = 1;		// CLCIN1PPS <- IO/M 
	CLCnSEL2 = 2;		// CLCIN2PPS <- A7
	CLCnSEL3 = 3;		// CLC3INPPS <- A6

	CLCnGLS0 = 0x06;	// CLK <- /RD no inv + IO/M inv 
	CLCnGLS1 = 0xA0;	// D <- A7 no inv + A6 no inv
	CLCnGLS2 = 0;		// R 0  (controlled by firm ware)
	CLCnGLS3 = 0;		// S 0

	CLCnPOL = 0x83;     // Q inv CLK inv D inv
	CLCnCON = 0x8c;     // D-FF, falling edge interrupt

//========== CLC6 AND(CLC1,5) -> READY  ==========
	CLCSELECT = 5;		// select CLC6  

	CLCnSEL0 = 51;		// CLC1
	CLCnSEL1 = 127;		// NC
	CLCnSEL2 = 127;		// NC
	CLCnSEL3 = 55;		// CLC5

	CLCnGLS0 = 0x02;	// CLC1 no inv
	CLCnGLS1 = 0x04;	// 1(0 inv)
	CLCnGLS2 = 0x10;	// 1(0 inv)
	CLCnGLS3 = 0x80;	// CLC5 no inv

	CLCnPOL = 0;		// no inv
	CLCnCON = 0x82;		// 4 input AND, no interrupt

//========== CLC7 IO /WT interrupt falling edge  ==========
	CLCSELECT = 6;		// select CLC7  

	CLCnSEL0 = 5;		// CLCIN4PPS <- /WR
	CLCnSEL1 = 1;		// CLCIN1PPS <- IO/M 
	CLCnSEL2 = 2;		// CLCIN2PPS <- A7
	CLCnSEL3 = 3;		// CLC3INPPS <- A6

	CLCnGLS0 = 0x01;	// /WR inv
	CLCnGLS1 = 0x08;	// IO/M no inv
	CLCnGLS2 = 0x10;	// A7 inv
	CLCnGLS3 = 0x40;	// A6 inv

	CLCnPOL = 0x80;		// Q inv (generate 4 input NAND)
	CLCnCON = 0x8A;		// 4 input AND, falling edge interrupt

//========== CLC8 ALE interrupt rising edge ==========
	CLCSELECT = 7;		// select CLC8  

	CLCnSEL0 = 0;		// CLCIN0PPS <- ALE
	CLCnSEL1 = 127;		// NC 
	CLCnSEL2 = 127;		// NC
	CLCnSEL3 = 127;		// NC

	CLCnGLS0 = 0x02;	// ALE no inv
	CLCnGLS1 = 0x04;	// 1(0 inv)
	CLCnGLS2 = 0x10;	// 1(0 inv)
	CLCnGLS3 = 0x40;	// 1(0 inv)

	CLCnPOL = 0x00;		// No inv
	CLCnCON = 0x92;		// 4 input AND, rising edge interrupt

//====================
	printf("\r\nEMUZ80+MEZZ8085 %2.2fMHz\r\n",NCO1INC * 1.5259 / 100000);

	// Unlock IVT
	IVTLOCK = 0x55;
	IVTLOCK = 0xAA;
	IVTLOCKbits.IVTLOCKED = 0x00;

	// Default IVT base address
	IVTBASE = 0x000008;

	// Lock IVT
	IVTLOCK = 0x55;
	IVTLOCK = 0xAA;
	IVTLOCKbits.IVTLOCKED = 0x01;

	// CLC VI enable
	CLC1IF = 0;		// Clear the CLC1 interrupt flag
	CLC2IF = 0;		// Clear the CLC2 interrupt flag
	CLC3IF = 0;		// Clear the CLC3 interrupt flag
	CLC4IF = 0;		// Clear the CLC4 interrupt flag
	CLC5IF = 0;		// Clear the CLC5 interrupt flag
	CLC6IF = 0;		// Clear the CLC6 interrupt flag
	CLC7IF = 0;		// Clear the CLC7 interrupt flag
	CLC8IF = 0;		// Clear the CLC8 interrupt flag

	CLC1IE = 1;		// CLC1 interrupt RAM /RD falling
	CLC2IE = 0;		// CLC2 ROM /RD (no interrupt)
	CLC3IE = 1;		// CLC3 interrupt RAM /WR falling
	CLC4IE = 0;		// (CLC4 used for RAM address decode)
	CLC5IE = 1;		// CLC5 interrupt IO /RD falling
	CLC6IE = 0;		// (CLC6 used for binding READY from each CLC)
	CLC7IE = 1;		// CLC7 interrupt IO /WR falling
	CLC8IE = 1;		// CLC8 interrupt ALE rising

	BSR = 0;		// BSR 0 fixed
	TBLPTRU = 1;	// TBLTPU always 1 fixed (8085ROM at 10000h)
	CLCSELECT = 0;	// CLCSELECT usually 0

	GIE = 1;		// Global interrupt enable
	LATE2 = 1;		// Release reset

	// 8085 start

	while(1) {
		asm(
			"btfsc  PIR9,0,c	\n"
			"bsf    LATE,1,c	\n"
			"btfss  PIR9,0,c	\n"
			"bcf    LATE,1,c	\n"
		);
	}
}

void __interrupt(irq(CLC1),base(8)) RAM_READ_ISR(){
	asm(
		"bsf    CLCnPOL,2,b		\n" // READY = 1 (G3POL = 1)
		"bcf    PIR0,5,c		\n" // clear interrupt flag (CLC1IF = 0)
		"movff  TBLPTRL,FSR0L	\n" // FSR0L = TBLPTRL(lower address)
		"clrf	TRISB,c			\n" // Set AD7-AD0 bus as output
		"movf   PORTD,w			\n" // W = PORTD(upper address)
		"addlw  high _ram		\n" // W =+ RAM locate address
		"movwf  FSR0H,c			\n" // FSR0H = W
		"movff  indf0,LATB		\n" // PORTB = RAM(FSR0H,L)
		"bcf    CLCnPOL,2,b		\n" // G3POL = 0
		"poll_clc1:				\n"
		"btfss	PORTA,0,c		\n" // /RD = H?
		"bra	poll_clc1		\n" // repeat
		"setf	TRISB,c			\n" // Set AD7-AD0 bus as input        
		"retfie	1				\n"	// not ROM read then return
	);
}

void __interrupt(irq(CLC3),base(8)) RAM_WRITE_ISR(){
	asm(
		"bcf	PIR7,5,c		\n" // clear interrupt flag (CLC3IF = 0)
		"movff	TBLPTRL,FSR0L	\n" // FSR0L = TBLPTRL(lower address)
		"movf	PORTD,w			\n" // W = PORTD(upper address)
		"addlw	high _ram		\n" // W =+ RAM locate address
		"movwf	FSR0H,c			\n" // FSR0H = W
		"movff	PORTB,indf0		\n" // RAM(FSR0H,L) = PORTB
		"retfie	1				\n"	// not ROM read then return
	);
}

void __interrupt(irq(CLC5),base(8)) IO_READ_ISR(){
	asm(
		"bsf    CLCSELECT,2,b	\n" // CLCSELECT = 4
		"bsf    CLCnPOL,2,b     \n" // READY = 1 (G3POL = 1)
		"bcf    PIR10,1,c		\n" // clear interrupt flag (CLC5IF = 0)

		"decfsz	PORTD,w,c		\n" // dec PORTD => WREG if 0 then skip next
		"bra	chk_IO_00		\n" // goto next decode
		"clrf	LATB,c			\n" // stats = 0
		"btfsc	PIR9,1,c		\n" // U3TXIF = 1?
		"bsf	LATB,0,c		\n" // set status bit0
		"btfsc	PIR9,0,c		\n" // U3RXIF = 1?
		"bsf	LATB,1,c		\n"	// set status bit1
		"clrf	TRISB,c			\n" // Set data bus as output
		"bcf    CLCnPOL,2,b		\n" // G3POL = 0
		"clrf   CLCSELECT,b		\n" // CLCSELECT = 0
		"poll_01:				\n"
		"btfss	PORTA,0,c		\n" // RD=H?
		"bra	poll_01			\n" // repeat
		"setf	TRISB,c			\n"	// Set AD7-AD0 bus as input
		"retfie	1				\n"

		"chk_IO_00:				\n"
		"incfsz	WREG,w,c		\n" // inc PORTD => WREG if 0 then skip next
		"bra	IORD_exit		\n" // goto next decode
		"movff	U3RXB,LATB		\n" // LATB = U3RXB 
		"clrf	TRISB,c			\n" // Set data bus as output
		"bcf    CLCnPOL,2,b		\n" // G3POL = 0
		"clrf   CLCSELECT,b		\n" // CLCSELECT = 0
		"poll_00:				\n"
		"btfss	PORTA,0,c		\n" // /RD=H?
		"bra	poll_00			\n" // repeat
		"setf	TRISB,c			\n"	// Set AD7-AD0 bus as input
		"retfie	1				\n"

		"IORD_exit:				\n"
		"clrf   CLCSELECT,b		\n" // CLCSELECT = 0
		"retfie	1				\n"
	);
}

void __interrupt(irq(CLC7),base(8)) IO_WRITE_ISR(){
	asm(
		"bcf	PIR14,1			\n"	// clear interrupt flag (CLC7IF = 0)
		"movf	PORTD,w			\n"	// WREG = PORTD(zero flag affected)
		"btfsc	STATUS,2,c		\n"	// not zero skip next instruction 
		"movff	PORTB,U3TXB		\n" // U3TXB = PORTB
		"retfie	1				\n"
	);
}

void __interrupt(irq(CLC8),base(8)) ALE_ISR(){
	asm(
		"movff	PORTB,TBLPTRL	\n" // TBLPTRL = PORTB(lower address)
		"bcf	PIR15,1,c		\n" // clear interrupt flag (CLC8IF = 0)
		"btfsc	CLCDATA,1,b		\n" // ROM read?(CLC2 = L?)
		"retfie	1				\n"	// not ROM read then return

		"movff	PORTD,TBLPTRH	\n" // TBLPTRH = PORTD(upper address)
		"tblrd	*				\n" // TABLAT = ROM(TBLPTRU,H,L)
		"movff	TABLAT,LATB		\n" // PORTB = TABLAT
		"clrf	TRISB,c			\n" // Set AD7-AD0 bus as output
		"poll_clc8:				\n"
		"btfss	PORTA,0,c		\n" // waiting RD=H
		"bra	poll_clc8		\n" // repeat
		"setf	TRISB,c			\n" // Set AD7-AD0 bus as input        
		"retfie	1				\n"
	);
}

const unsigned char rom[] __at(0x10000) = {
//#include "MSBAS80NI.TXT"
#include "emu8085mon_RevB03_8k.txt"
};

const unsigned char rom_3200[] __at(0x13200)= {
#include "MSBAS80_3200.txt"	
};

const unsigned char ram_5400[] __at(0x15400) = {
#include "game80_8K_5400.txt"
};

const unsigned char ram_5b00[] __at(0x15b00) = {
#include "EMUZ80_ZTB_5B00.txt"
};
