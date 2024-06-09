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
    uart_tx,
    uart_rx_valid,
    parity_error,
    stop_bit_unstable,
    serial_to_packet_valid
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
    output uart_rx_valid;
    output parity_error;
    output stop_bit_unstable;
    output serial_to_packet_valid;

    /* signal declarations */
    wire _47;
    wire _48;
    wire _49;
    wire _50;
    wire _51;
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
    wire [31:0] _76;
    wire [31:0] _77;
    wire [31:0] _78;
    wire [31:0] _79;
    wire [31:0] _80;
    wire [31:0] _81;
    wire [31:0] _82;
    wire [31:0] _83;
    wire _39;
    wire _41;
    wire _43;
    wire [1060:0] _46;
    wire [31:0] _84;

    /* logic */
    assign _47 = _46[1060:1060];
    assign _48 = _46[1059:1059];
    assign _49 = _46[1058:1058];
    assign _50 = _46[1057:1057];
    assign _51 = _46[1056:1056];
    assign _52 = _46[1055:1024];
    assign _53 = _46[1023:992];
    assign _54 = _46[991:960];
    assign _55 = _46[959:928];
    assign _56 = _46[927:896];
    assign _57 = _46[895:864];
    assign _58 = _46[863:832];
    assign _59 = _46[831:800];
    assign _60 = _46[799:768];
    assign _61 = _46[767:736];
    assign _62 = _46[735:704];
    assign _63 = _46[703:672];
    assign _64 = _46[671:640];
    assign _65 = _46[639:608];
    assign _66 = _46[607:576];
    assign _67 = _46[575:544];
    assign _68 = _46[543:512];
    assign _69 = _46[511:480];
    assign _70 = _46[479:448];
    assign _71 = _46[447:416];
    assign _72 = _46[415:384];
    assign _73 = _46[383:352];
    assign _74 = _46[351:320];
    assign _75 = _46[319:288];
    assign _76 = _46[287:256];
    assign _77 = _46[255:224];
    assign _78 = _46[223:192];
    assign _79 = _46[191:160];
    assign _80 = _46[159:128];
    assign _81 = _46[127:96];
    assign _82 = _46[95:64];
    assign _83 = _46[63:32];
    assign _39 = uart_rx;
    assign _41 = clear;
    assign _43 = clock;
    Cpu
        cpu
        ( .clock(_43), .clear(_41), .uart_rx(_39), .serial_to_packet_valid(_46[1060:1060]), .stop_bit_unstable(_46[1059:1059]), .parity_error(_46[1058:1058]), .uart_rx_valid(_46[1057:1057]), .uart_tx(_46[1056:1056]), .registers_general310(_46[1055:1024]), .registers_general300(_46[1023:992]), .registers_general290(_46[991:960]), .registers_general280(_46[959:928]), .registers_general270(_46[927:896]), .registers_general260(_46[895:864]), .registers_general250(_46[863:832]), .registers_general240(_46[831:800]), .registers_general230(_46[799:768]), .registers_general220(_46[767:736]), .registers_general210(_46[735:704]), .registers_general200(_46[703:672]), .registers_general190(_46[671:640]), .registers_general180(_46[639:608]), .registers_general170(_46[607:576]), .registers_general160(_46[575:544]), .registers_general150(_46[543:512]), .registers_general140(_46[511:480]), .registers_general130(_46[479:448]), .registers_general120(_46[447:416]), .registers_general110(_46[415:384]), .registers_general100(_46[383:352]), .registers_general90(_46[351:320]), .registers_general80(_46[319:288]), .registers_general70(_46[287:256]), .registers_general60(_46[255:224]), .registers_general50(_46[223:192]), .registers_general40(_46[191:160]), .registers_general30(_46[159:128]), .registers_general20(_46[127:96]), .registers_general10(_46[95:64]), .registers_general00(_46[63:32]), .registers_pc0(_46[31:0]) );
    assign _84 = _46[31:0];

    /* aliases */

    /* output assignments */
    assign registers_pc0 = _84;
    assign registers_general00 = _83;
    assign registers_general10 = _82;
    assign registers_general20 = _81;
    assign registers_general30 = _80;
    assign registers_general40 = _79;
    assign registers_general50 = _78;
    assign registers_general60 = _77;
    assign registers_general70 = _76;
    assign registers_general80 = _75;
    assign registers_general90 = _74;
    assign registers_general100 = _73;
    assign registers_general110 = _72;
    assign registers_general120 = _71;
    assign registers_general130 = _70;
    assign registers_general140 = _69;
    assign registers_general150 = _68;
    assign registers_general160 = _67;
    assign registers_general170 = _66;
    assign registers_general180 = _65;
    assign registers_general190 = _64;
    assign registers_general200 = _63;
    assign registers_general210 = _62;
    assign registers_general220 = _61;
    assign registers_general230 = _60;
    assign registers_general240 = _59;
    assign registers_general250 = _58;
    assign registers_general260 = _57;
    assign registers_general270 = _56;
    assign registers_general280 = _55;
    assign registers_general290 = _54;
    assign registers_general300 = _53;
    assign registers_general310 = _52;
    assign uart_tx = _51;
    assign uart_rx_valid = _50;
    assign parity_error = _49;
    assign stop_bit_unstable = _48;
    assign serial_to_packet_valid = _47;

endmodule
