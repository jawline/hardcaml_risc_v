module cpu_top (
    uart_rx,
    clear,
    clock,
    registers_pc0,
    registers_general00,
    registers_general10,
    registers_general20,
    registers_general30,
    registers_general40,
    registers_general50,
    registers_general60,
    registers_general70,
    registers_general80,
    registers_general90,
    registers_general100,
    registers_general110,
    registers_general120,
    registers_general130,
    registers_general140,
    registers_general150,
    registers_general160,
    registers_general170,
    registers_general180,
    registers_general190,
    registers_general200,
    registers_general210,
    registers_general220,
    registers_general230,
    registers_general240,
    registers_general250,
    registers_general260,
    registers_general270,
    registers_general280,
    registers_general290,
    registers_general300,
    registers_general310,
    uart_tx
);

    input uart_rx;
    input clear;
    input clock;
    output [31:0] registers_pc0;
    output [31:0] registers_general00;
    output [31:0] registers_general10;
    output [31:0] registers_general20;
    output [31:0] registers_general30;
    output [31:0] registers_general40;
    output [31:0] registers_general50;
    output [31:0] registers_general60;
    output [31:0] registers_general70;
    output [31:0] registers_general80;
    output [31:0] registers_general90;
    output [31:0] registers_general100;
    output [31:0] registers_general110;
    output [31:0] registers_general120;
    output [31:0] registers_general130;
    output [31:0] registers_general140;
    output [31:0] registers_general150;
    output [31:0] registers_general160;
    output [31:0] registers_general170;
    output [31:0] registers_general180;
    output [31:0] registers_general190;
    output [31:0] registers_general200;
    output [31:0] registers_general210;
    output [31:0] registers_general220;
    output [31:0] registers_general230;
    output [31:0] registers_general240;
    output [31:0] registers_general250;
    output [31:0] registers_general260;
    output [31:0] registers_general270;
    output [31:0] registers_general280;
    output [31:0] registers_general290;
    output [31:0] registers_general300;
    output [31:0] registers_general310;
    output uart_tx;

    /* signal declarations */
    wire _43;
    wire [31:0] _44;
    wire [31:0] _45;
    wire [31:0] _46;
    wire [31:0] _47;
    wire [31:0] _48;
    wire [31:0] _49;
    wire [31:0] _50;
    wire [31:0] _51;
    wire [31:0] _52;
    wire [31:0] _53;
    wire [31:0] _54;
    wire [31:0] _55;
    wire [31:0] _56;
    wire [31:0] _57;
    wire [31:0] _58;
    wire [31:0] _59;
    wire [31:0] _60;
    wire [31:0] _61;
    wire [31:0] _62;
    wire [31:0] _63;
    wire [31:0] _64;
    wire [31:0] _65;
    wire [31:0] _66;
    wire [31:0] _67;
    wire [31:0] _68;
    wire [31:0] _69;
    wire [31:0] _70;
    wire [31:0] _71;
    wire [31:0] _72;
    wire [31:0] _73;
    wire [31:0] _74;
    wire [31:0] _75;
    wire _35;
    wire _37;
    wire _39;
    wire [1056:0] _42;
    wire [31:0] _76;

    /* logic */
    assign _43 = _42[1056:1056];
    assign _44 = _42[1055:1024];
    assign _45 = _42[1023:992];
    assign _46 = _42[991:960];
    assign _47 = _42[959:928];
    assign _48 = _42[927:896];
    assign _49 = _42[895:864];
    assign _50 = _42[863:832];
    assign _51 = _42[831:800];
    assign _52 = _42[799:768];
    assign _53 = _42[767:736];
    assign _54 = _42[735:704];
    assign _55 = _42[703:672];
    assign _56 = _42[671:640];
    assign _57 = _42[639:608];
    assign _58 = _42[607:576];
    assign _59 = _42[575:544];
    assign _60 = _42[543:512];
    assign _61 = _42[511:480];
    assign _62 = _42[479:448];
    assign _63 = _42[447:416];
    assign _64 = _42[415:384];
    assign _65 = _42[383:352];
    assign _66 = _42[351:320];
    assign _67 = _42[319:288];
    assign _68 = _42[287:256];
    assign _69 = _42[255:224];
    assign _70 = _42[223:192];
    assign _71 = _42[191:160];
    assign _72 = _42[159:128];
    assign _73 = _42[127:96];
    assign _74 = _42[95:64];
    assign _75 = _42[63:32];
    assign _35 = uart_rx;
    assign _37 = clear;
    assign _39 = clock;
    Cpu
        cpu
        ( .clock(_39), .clear(_37), .uart_rx(_35), .uart_tx(_42[1056:1056]), .registers_general310(_42[1055:1024]), .registers_general300(_42[1023:992]), .registers_general290(_42[991:960]), .registers_general280(_42[959:928]), .registers_general270(_42[927:896]), .registers_general260(_42[895:864]), .registers_general250(_42[863:832]), .registers_general240(_42[831:800]), .registers_general230(_42[799:768]), .registers_general220(_42[767:736]), .registers_general210(_42[735:704]), .registers_general200(_42[703:672]), .registers_general190(_42[671:640]), .registers_general180(_42[639:608]), .registers_general170(_42[607:576]), .registers_general160(_42[575:544]), .registers_general150(_42[543:512]), .registers_general140(_42[511:480]), .registers_general130(_42[479:448]), .registers_general120(_42[447:416]), .registers_general110(_42[415:384]), .registers_general100(_42[383:352]), .registers_general90(_42[351:320]), .registers_general80(_42[319:288]), .registers_general70(_42[287:256]), .registers_general60(_42[255:224]), .registers_general50(_42[223:192]), .registers_general40(_42[191:160]), .registers_general30(_42[159:128]), .registers_general20(_42[127:96]), .registers_general10(_42[95:64]), .registers_general00(_42[63:32]), .registers_pc0(_42[31:0]) );
    assign _76 = _42[31:0];

    /* aliases */

    /* output assignments */
    assign registers_pc0 = _76;
    assign registers_general00 = _75;
    assign registers_general10 = _74;
    assign registers_general20 = _73;
    assign registers_general30 = _72;
    assign registers_general40 = _71;
    assign registers_general50 = _70;
    assign registers_general60 = _69;
    assign registers_general70 = _68;
    assign registers_general80 = _67;
    assign registers_general90 = _66;
    assign registers_general100 = _65;
    assign registers_general110 = _64;
    assign registers_general120 = _63;
    assign registers_general130 = _62;
    assign registers_general140 = _61;
    assign registers_general150 = _60;
    assign registers_general160 = _59;
    assign registers_general170 = _58;
    assign registers_general180 = _57;
    assign registers_general190 = _56;
    assign registers_general200 = _55;
    assign registers_general210 = _54;
    assign registers_general220 = _53;
    assign registers_general230 = _52;
    assign registers_general240 = _51;
    assign registers_general250 = _50;
    assign registers_general260 = _49;
    assign registers_general270 = _48;
    assign registers_general280 = _47;
    assign registers_general290 = _46;
    assign registers_general300 = _45;
    assign registers_general310 = _44;
    assign uart_tx = _43;

endmodule
