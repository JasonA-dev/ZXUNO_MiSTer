//============================================================================
//  SVI-328 based on Sorgelig's ColecoVision Mister port
//
//  
//  Copyright (C) 2017-2019 Sorgelig
//
//  This program is free software; you can redistribute it and/or modify it
//  under the terms of the GNU General Public License as published by the Free
//  Software Foundation; either version 2 of the License, or (at your option)
//  any later version.
//
//  This program is distributed in the hope that it will be useful, but WITHOUT
//  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
//  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
//  more details.
//
//  You should have received a copy of the GNU General Public License along
//  with this program; if not, write to the Free Software Foundation, Inc.,
//  51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//============================================================================


// TODO
//  - Complete Keyboard Mapping
//  - Make Memory size select from OSD
//  - Select PAL/NTSC
//  - OSD Load Keyboard Map 

// Done
//  - Rewind on CAS load or Reset
//  - LED_Disk on Tape Load
//  - Tape Load Graphical wave (file only)
//  - Tape Counter (progress bar)

//Core : 
//Z80 - 3,5555Mhz
//AY - z80/2 = 1,777 Mhz
//Mess :
//Z80 - 3,579545
//AY - 1,789772

//`define Bram


module emu
(
	//Master input clock
	input         CLK_50M,

	//Async reset from top-level module.
	//Can be used as initial reset.
	input         RESET,

	//Must be passed to hps_io module
	inout  [48:0] HPS_BUS,

	//Base video clock. Usually equals to CLK_SYS.
	output        CLK_VIDEO,

	//Multiple resolutions are supported using different CE_PIXEL rates.
	//Must be based on CLK_VIDEO
	output        CE_PIXEL,

	//Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
	//if VIDEO_ARX[12] or VIDEO_ARY[12] is set then [11:0] contains scaled size instead of aspect ratio.
	output [12:0] VIDEO_ARX,
	output [12:0] VIDEO_ARY,

	output  [7:0] VGA_R,
	output  [7:0] VGA_G,
	output  [7:0] VGA_B,
	output        VGA_HS,
	output        VGA_VS,
	output        VGA_DE,    // = ~(VBlank | HBlank)
	output        VGA_F1,
	output [1:0]  VGA_SL,
	output        VGA_SCALER, // Force VGA scaler

	input  [11:0] HDMI_WIDTH,
	input  [11:0] HDMI_HEIGHT,
	output        HDMI_FREEZE,

`ifdef MISTER_FB
	// Use framebuffer in DDRAM (USE_FB=1 in qsf)
	// FB_FORMAT:
	//    [2:0] : 011=8bpp(palette) 100=16bpp 101=24bpp 110=32bpp
	//    [3]   : 0=16bits 565 1=16bits 1555
	//    [4]   : 0=RGB  1=BGR (for 16/24/32 modes)
	//
	// FB_STRIDE either 0 (rounded to 256 bytes) or multiple of pixel size (in bytes)
	output        FB_EN,
	output  [4:0] FB_FORMAT,
	output [11:0] FB_WIDTH,
	output [11:0] FB_HEIGHT,
	output [31:0] FB_BASE,
	output [13:0] FB_STRIDE,
	input         FB_VBL,
	input         FB_LL,
	output        FB_FORCE_BLANK,

`ifdef MISTER_FB_PALETTE
	// Palette control for 8bit modes.
	// Ignored for other video modes.
	output        FB_PAL_CLK,
	output  [7:0] FB_PAL_ADDR,
	output [23:0] FB_PAL_DOUT,
	input  [23:0] FB_PAL_DIN,
	output        FB_PAL_WR,
`endif
`endif

	output        LED_USER,  // 1 - ON, 0 - OFF.

	// b[1]: 0 - LED status is system status OR'd with b[0]
	//       1 - LED status is controled solely by b[0]
	// hint: supply 2'b00 to let the system control the LED.
	output  [1:0] LED_POWER,
	output  [1:0] LED_DISK,

	// I/O board button press simulation (active high)
	// b[1]: user button
	// b[0]: osd button
	output  [1:0] BUTTONS,

	input         CLK_AUDIO, // 24.576 MHz
	output [15:0] AUDIO_L,
	output [15:0] AUDIO_R,
	output        AUDIO_S,   // 1 - signed audio samples, 0 - unsigned
	output  [1:0] AUDIO_MIX, // 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)

	//ADC
	inout   [3:0] ADC_BUS,

	//SD-SPI
	output        SD_SCK,
	output        SD_MOSI,
	input         SD_MISO,
	output        SD_CS,
	input         SD_CD,

	//High latency DDR3 RAM interface
	//Use for non-critical time purposes
	output        DDRAM_CLK,
	input         DDRAM_BUSY,
	output  [7:0] DDRAM_BURSTCNT,
	output [28:0] DDRAM_ADDR,
	input  [63:0] DDRAM_DOUT,
	input         DDRAM_DOUT_READY,
	output        DDRAM_RD,
	output [63:0] DDRAM_DIN,
	output  [7:0] DDRAM_BE,
	output        DDRAM_WE,

	//SDRAM interface with lower latency
	output        SDRAM_CLK,
	output        SDRAM_CKE,
	output [12:0] SDRAM_A,
	output  [1:0] SDRAM_BA,
	inout  [15:0] SDRAM_DQ,
	output        SDRAM_DQML,
	output        SDRAM_DQMH,
	output        SDRAM_nCS,
	output        SDRAM_nCAS,
	output        SDRAM_nRAS,
	output        SDRAM_nWE,

`ifdef MISTER_DUAL_SDRAM
	//Secondary SDRAM
	//Set all output SDRAM_* signals to Z ASAP if SDRAM2_EN is 0
	input         SDRAM2_EN,
	output        SDRAM2_CLK,
	output [12:0] SDRAM2_A,
	output  [1:0] SDRAM2_BA,
	inout  [15:0] SDRAM2_DQ,
	output        SDRAM2_nCS,
	output        SDRAM2_nCAS,
	output        SDRAM2_nRAS,
	output        SDRAM2_nWE,
`endif

	input         UART_CTS,
	output        UART_RTS,
	input         UART_RXD,
	output        UART_TXD,
	output        UART_DTR,
	input         UART_DSR,

	// Open-drain User port.
	// 0 - D+/RX
	// 1 - D-/TX
	// 2..6 - USR2..USR6
	// Set USER_OUT to 1 to read from USER_IN.
	input   [6:0] USER_IN,
	output  [6:0] USER_OUT,

	input         OSD_STATUS
);


//assign ADC_BUS = 'Z;
assign USER_OUT = '1;
assign {SD_SCK, SD_MOSI, SD_CS} = 'Z;
assign {DDRAM_CLK, DDRAM_BURSTCNT, DDRAM_ADDR, DDRAM_DIN, DDRAM_BE, DDRAM_RD, DDRAM_WE} = 0;
 
assign LED_USER   = tape_in;
assign LED_DISK   = {1'b1, ~vsd_sel & sd_act};
assign LED_POWER  = 0;
assign BUTTONS    = 0;
assign VGA_SCALER = 0;


wire [1:0] ar = status[2:1];
wire vga_de;
reg  en216p;
always @(posedge CLK_VIDEO) en216p <= ((HDMI_WIDTH == 1920) && (HDMI_HEIGHT == 1080) && !forced_scandoubler && !scale);

video_freak video_freak
(
	.*,
	.VGA_DE_IN(vga_de),
	.ARX((!ar) ? 12'd4 : (ar - 1'd1)),
	.ARY((!ar) ? 12'd3 : 12'd0),
	.CROP_SIZE(en216p ? 10'd216 : 10'd0),
	.CROP_OFF(0),
	.SCALE(status[11:10])
);

`include "build_id.v" 
parameter CONF_STR = {
	"ZXUNO;;",
	"S0,VHD,Load SD;",
  	"-;",
	"O45,Mode,Color,B&W,Amber,Green;",
	"O12,Aspect ratio,Original,Full Screen,[ARC1],[ARC2];",
	"O79,Scandoubler Fx,None,HQ2x,CRT 25%,CRT 50%;",
	"-;",
	"OAB,Scale,Normal,V-Integer,Narrower HV-Integer,Wider HV-Integer;",
	"-;",
	"O3,Joysticks swap,No,Yes;",
	"R0,Reset;",
        "J,Button;",
        "jn,A;",
	"V,v",`BUILD_DATE
};

/////////////////  CLOCKS  ////////////////////////

wire clk_sys,clk_mem;
wire pll_locked;

relojes relojes
(
	.refclk(CLK_50M),
	.rst(0),
	.outclk_0(clk_sys),
	.outclk_1(clk_mem),
	.locked(pll_locked)
);

/////////////////  HPS  ///////////////////////////

wire [31:0] status;
wire  [1:0] buttons;

wire [31:0] joy0, joy1;

wire        ioctl_download;
wire  [7:0] ioctl_index;
wire        ioctl_wr;
wire        ioctl_wait;
wire [24:0] ioctl_addr;
wire  [7:0] ioctl_dout;
wire        forced_scandoubler;
wire [21:0] gamma_bus;
wire [10:0] PS2Keys;

wire ps2_kbd_clk_o, ps2_kbd_data_o;
wire ps2_kbd_clk_i, ps2_kbd_data_i;

wire ps2_mouse_clk_o, ps2_mouse_data_o;
wire ps2_mouse_clk_i, ps2_mouse_data_i;


wire [31:0] sd_lba;
wire        sd_rd;
wire        sd_wr;
wire        sd_ack;
wire  [8:0] sd_buff_addr;
wire  [7:0] sd_buff_dout;
wire  [7:0] sd_buff_din;
wire        sd_buff_wr;
wire        img_mounted;
wire        img_readonly;
wire [63:0] img_size;

 
hps_io #(.CONF_STR(CONF_STR),.PS2DIV(2000),.PS2WE(1)) hps_io
(
	.clk_sys(clk_sys),
	.HPS_BUS(HPS_BUS),

	.buttons(buttons),
	.status(status),
	.forced_scandoubler(forced_scandoubler),
	.gamma_bus(gamma_bus),

	
	.ps2_mouse_clk_in   ( ps2_mouse_clk_i),
	.ps2_mouse_data_in  ( ps2_mouse_data_i),
	.ps2_mouse_clk_out  ( ps2_mouse_clk_o),
	.ps2_mouse_data_out ( ps2_mouse_data_o),

  	.ps2_kbd_clk_in  ( ps2_kbd_clk_i  ),
	.ps2_kbd_data_in ( ps2_kbd_data_i ),
	.ps2_kbd_clk_out ( ps2_kbd_clk_o  ),
	.ps2_kbd_data_out( ps2_kbd_data_o ),

	.ioctl_download(ioctl_download),
	.ioctl_index(ioctl_index),
	.ioctl_wr(ioctl_wr),
	.ioctl_addr(ioctl_addr),
	.ioctl_dout(ioctl_dout),
	.ioctl_wait(ioctl_wait),
	
	.sd_lba('{sd_lba}),
	.sd_rd(sd_rd),
	.sd_wr(sd_wr),
	.sd_ack(sd_ack),
	.sd_buff_addr(sd_buff_addr),
	.sd_buff_dout(sd_buff_dout),
	.sd_buff_din('{sd_buff_din}),
	.sd_buff_wr(sd_buff_wr),
	.img_mounted(img_mounted),
	.img_readonly(img_readonly),
	.img_size(img_size),

	.status_menumask({status[15]}),	
	
	.ps2_key(PS2Keys),
	
	.joystick_0(joy0), // HPS joy [4:0] {Fire, Up, Down, Left, Right}
	.joystick_1(joy1)

);

////////////////////  RAM  //////////////////
wire [20:0] SRAM_A;
wire [7:0] SRAM_Q;
wire SRAM_WE;

wire [7:0] ram_out;
wire [7:0] ram_in   = ~SRAM_WE ? SRAM_Q : 8'bZ;
assign SRAM_Q = SRAM_WE ? ram_out : 8'bZ;

//mem256M mem256M
//(
//   .clock (clk_sys),
//	.wren  (~SRAM_WE),
//	.address(SRAM_A),
//	.data   (ram_in),
//	.q      (ram_out)
//);

//////////////////   SD   ///////////////////
wire sdclk;
wire sdmosi;
wire sdmiso = vsd_sel ? vsdmiso : SD_MISO;
wire sdss;

reg reset_img;
reg vsd_sel = 0;
always @(posedge clk_sys) begin
	integer to = 0;
	
	if(to) to <= to - 1;
	else reset_img <= 0;

	if(img_mounted) begin
		vsd_sel <= |img_size;
		reset_img <= 1;
		to <= 10000000;
	end
end

wire vsdmiso;
sd_card sd_card
(
	.*,
	.clk_spi(clk_sys),

	.sdhc(1),

	.sck(sdclk),
	.ss(~vsd_sel | sdss),
	.mosi(sdmosi),
	.miso(vsdmiso)
);

assign SD_CS   = vsd_sel | sdss;
assign SD_SCK  = sdclk & ~SD_CS;
assign SD_MOSI = sdmosi & ~SD_CS;

reg sd_act;

always @(posedge clk_sys) begin
	reg old_mosi, old_miso;
	integer timeout = 0;

	old_mosi <= sdmosi;
	old_miso <= sdmiso;

	sd_act <= 0;
	if(timeout < 1000000) begin
		timeout <= timeout + 1;
		sd_act <= 1;
	end

	if((old_mosi ^ sdmosi) || (old_miso ^ sdmiso)) timeout <= 0;
end


/////////////////  RESET  /////////////////////////

wire reset = RESET | status[0] | buttons[1];


////////////////  machine  ////////////////////////


assign AUDIO_S = 1;
assign AUDIO_MIX = 0;

assign CLK_VIDEO = clk_sys;

wire [7:0] R,G,B;
wire hblank, vblank;
wire hsync, vsync;

wire [31:0] joya = status[3] ? joy1 : joy0;
wire [31:0] joyb = status[3] ? joy0 : joy1;




   zxuno #(.FPGA_MODEL(3'b010), .MASTERCLK(28000000)) la_maquina (
    .sysclk(clk_sys),
    .power_on_reset_n(~reset),  // sólo para simulación. Para implementacion, dejar a 1
    .r(ri),
    .g(gi),
    .b(bi),
    .hsync(hsync),
    .vsync(vsync),
    .hblank(hblank),
    .vblank(vblank),
    .csync(),
	 

    .clkps2_i (ps2_kbd_clk_o),
    .dataps2_i(ps2_kbd_data_o),
    .clkps2_o (ps2_kbd_clk_i),
    .dataps2_o(ps2_kbd_data_i),
	 
	 //.turbo_osd(status[8]),
    
	 .ear_ext( tape_in ),  // negada porque el hardware tiene un transistor inversor
    .audio_out_left(),
    .audio_out_right(),
	 
	 .left (AUDIO_L),
	 .right(AUDIO_R), 
    
    .midi_out(),
    .clkbd(),
    .wsbd(),
    .dabd(),
	 
    .uart_tx(UART_TXD),
    .uart_rx(UART_RXD),
    .uart_rts(UART_RTS),


	 .sram_addr(SRAM_A),
    .sram_data(SRAM_Q),
    .sram_we_n(SRAM_WE),
	 

    .flash_cs_n(),
    .flash_clk(),
    .flash_di(),
    .flash_do(),
    
    .sd_cs_n(sdss),
    .sd_clk(sdclk),
    .sd_mosi(sdmosi),
    .sd_miso(sdmiso),
    

    .joy1up  (joya[0]),
    .joy1down(joya[1]),
    .joy1left(joya[2]),
    .joy1right(joya[3]),
    .joy1fire1(joya[4]),
    .joy1fire2(joya[5]),   

	 
    .joy2up  (joyb[0]),
    .joy2down(joyb[1]),
    .joy2left(joyb[2]),
    .joy2right(joyb[3]),
    .joy2fire1(joyb[4]),
    .joy2fire2(joyb[5]),   

//    .mouseclk(ps2_mouse_clk),
//    .mousedata(ps2_mouse_data),
    
    .clk14en_tovga(clk14en_tovga),
    .vga_enable(),
    .scanlines_enable(),
    .freq_option(),
    
    .ad724_xtal(),
    .ad724_mode(),
    .ad724_enable_gencolorclk()
    );

wire [5:0] ri_monochrome, gi_monochrome, bi_monochrome;
wire [1:0] monochrome_switcher=status[5:4];
	 
monochrome monochromergb (
    .monochrome_selection(monochrome_switcher),
    .ri({ri,ri}),
    .gi({gi,gi}),
    .bi({bi,bi}),
    .ro(ri_monochrome),
    .go(gi_monochrome),
    .bo(bi_monochrome)  
  );	 


wire [2:0] ri, gi, bi, ro, go, bo;
wire clk14en_tovga;

/////////////////  VIDEO  /////////////////////////

assign VGA_F1 = 0;
assign VGA_SL = sl[1:0];

wire [2:0] scale = status[9:7];
wire [2:0] sl = scale ? scale - 1'd1 : 3'd0;

reg hs_o, vs_o;
always @(posedge CLK_VIDEO) begin
	hs_o <= ~hsync;
	if(~hs_o & ~hsync) vs_o <= ~vsync;
end
wire freeze_sync;

video_mixer #(.LINE_LENGTH(290), .GAMMA(1), .HALF_DEPTH(0)) video_mixer
(
	.*,

	.ce_pix(clk14en_tovga),

	.scandoubler(scale || forced_scandoubler),
	.hq2x(scale==1),

	.VGA_DE(vga_de),
	.R({ri_monochrome,2'b00}),
	.G({gi_monochrome,2'b00}),
	.B({bi_monochrome,2'b00}),

	// Positive pulses.
	.HSync(hs_o),
	.VSync(vs_o),
	.HBlank(hblank),
	.VBlank(vblank)
);




/////////////////  Tape In   /////////////////////////

wire tape_in;
wire tape_adc, tape_adc_act;

assign tape_in = tape_adc_act & tape_adc;

ltc2308_tape #(.ADC_RATE(120000), .CLK_RATE(42666000)) tape
(
  .clk(clk_sys),
  .ADC_BUS(ADC_BUS),
  .dout(tape_adc),
  .active(tape_adc_act)
);

endmodule
