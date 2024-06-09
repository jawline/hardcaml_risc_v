module Cpu (
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
    wire _75;
    wire _76;
    wire _83;
    wire [31:0] _106;
    wire [31:0] _107;
    wire [31:0] _108;
    wire [31:0] _109;
    wire [31:0] _110;
    wire [31:0] _111;
    wire [31:0] _112;
    wire [31:0] _113;
    wire [31:0] _114;
    wire [31:0] _115;
    wire [31:0] _116;
    wire [31:0] _117;
    wire [31:0] _118;
    wire [31:0] _119;
    wire [31:0] _120;
    wire [31:0] _121;
    wire [31:0] _122;
    wire [31:0] _123;
    wire [31:0] _124;
    wire [31:0] _125;
    wire [31:0] _126;
    wire [31:0] _127;
    wire [31:0] _128;
    wire [31:0] _129;
    wire [31:0] _130;
    wire [31:0] _131;
    wire [31:0] _132;
    wire [31:0] _133;
    wire [31:0] _137;
    wire _138 = 1'b0;
    wire _38;
    wire [31:0] _140 = 32'b00000000000000000000000000000100;
    wire [31:0] _141;
    wire [31:0] _39;
    wire [30:0] _142 = 31'b0000000000000000000000000000000;
    wire [31:0] _145;
    wire [31:0] _40;
    wire _41;
    wire _42;
    wire _103;
    wire [31:0] _102;
    wire _101;
    wire _146;
    wire _43;
    wire _97;
    wire _96;
    wire [31:0] _147;
    wire [31:0] _44;
    wire _148;
    wire _45;
    wire [31:0] _149;
    wire [31:0] _46;
    wire _150;
    wire _47;
    wire [31:0] _95;
    wire _94;
    wire [31:0] _93;
    wire [31:0] _151;
    wire [31:0] _48;
    wire _152;
    wire _49;
    wire _153;
    wire _50;
    wire _154;
    wire _51;
    wire [7:0] _80;
    wire _79;
    wire [2:0] _82;
    wire _155;
    wire _52;
    wire [31:0] _135;
    wire [31:0] _53;
    wire [31:0] _134;
    wire [15:0] _156;
    wire [15:0] _54;
    wire _143;
    wire _144;
    wire [31:0] _158 = 32'b00000000000000000000000000000000;
    wire [31:0] _136;
    wire _159;
    wire _157;
    wire _160;
    wire _161;
    wire tx_enable;
    wire [78:0] _78;
    wire _92;
    wire [31:0] _91;
    wire _90;
    wire [31:0] _89;
    wire [31:0] _162;
    wire [31:0] _56;
    wire _163;
    wire _57;
    wire _164;
    wire _58;
    wire _165;
    wire _59;
    wire _85;
    wire [7:0] _84;
    wire vdd = 1'b1;
    wire [7:0] _70;
    wire _61;
    wire [10:0] _68;
    wire _69;
    wire [9:0] _73;
    wire _74;
    wire [67:0] _87;
    wire _88;
    wire [104:0] _99;
    wire _100;
    wire _63;
    wire _65;
    wire [1124:0] _105;
    wire [31:0] _139;

    /* logic */
    assign _75 = _68[10:10];
    assign _76 = _68[9:9];
    assign _83 = _82[0:0];
    assign _106 = _105[1122:1091];
    assign _107 = _105[1090:1059];
    assign _108 = _105[1058:1027];
    assign _109 = _105[1026:995];
    assign _110 = _105[994:963];
    assign _111 = _105[962:931];
    assign _112 = _105[930:899];
    assign _113 = _105[898:867];
    assign _114 = _105[866:835];
    assign _115 = _105[834:803];
    assign _116 = _105[802:771];
    assign _117 = _105[770:739];
    assign _118 = _105[738:707];
    assign _119 = _105[706:675];
    assign _120 = _105[674:643];
    assign _121 = _105[642:611];
    assign _122 = _105[610:579];
    assign _123 = _105[578:547];
    assign _124 = _105[546:515];
    assign _125 = _105[514:483];
    assign _126 = _105[482:451];
    assign _127 = _105[450:419];
    assign _128 = _105[418:387];
    assign _129 = _105[386:355];
    assign _130 = _105[354:323];
    assign _131 = _105[322:291];
    assign _132 = _105[290:259];
    assign _133 = _105[258:227];
    assign _137 = _105[130:99];
    assign _38 = _138;
    assign _141 = _139 + _140;
    assign _39 = _141;
    assign _145 = { _142, _144 };
    assign _40 = _145;
    assign _41 = vdd;
    assign _42 = vdd;
    assign _103 = _99[2:2];
    assign _102 = _99[104:73];
    assign _101 = _99[72:72];
    assign _146 = _105[0:0];
    assign _43 = _146;
    assign _97 = _78[78:78];
    assign _96 = _87[67:67];
    assign _147 = _105[66:35];
    assign _44 = _147;
    assign _148 = _105[34:34];
    assign _45 = _148;
    assign _149 = _105[33:2];
    assign _46 = _149;
    assign _150 = _105[1:1];
    assign _47 = _150;
    assign _95 = _78[77:46];
    assign _94 = _78[45:45];
    assign _93 = _78[44:13];
    assign _151 = _99[70:39];
    assign _48 = _151;
    assign _152 = _99[38:38];
    assign _49 = _152;
    assign _153 = _99[37:37];
    assign _50 = _153;
    assign _154 = _99[1:1];
    assign _51 = _154;
    assign _80 = _78[10:3];
    assign _79 = _78[2:2];
    Uart_tx_0
        tx
        ( .clock(_65), .clear(_63), .data_in_valid(_79), .data_in(_80), .idle(_82[2:2]), .data_in_ready(_82[1:1]), .uart_tx(_82[0:0]) );
    assign _155 = _82[1:1];
    assign _52 = _155;
    assign _135 = _105[194:163];
    assign _53 = _135;
    assign _134 = _105[226:195];
    assign _156 = _134[15:0];
    assign _54 = _156;
    assign _143 = _78[0:0];
    assign _144 = ~ _143;
    assign _136 = _105[162:131];
    assign _159 = _136 == _158;
    assign _157 = _105[1124:1124];
    assign _160 = _157 & _159;
    assign _161 = _160 & _144;
    assign tx_enable = _161;
    dma_memory_to_packet
        dma_out
        ( .clock(_65), .clear(_63), .enable_valid(tx_enable), .enable_value$length(_54), .enable_value$address(_53), .output_packet_ready(_52), .memory_ready(_51), .memory_response_valid(_50), .memory_response_error(_49), .memory_response_read_data(_48), .mem_response$memory_response_ready(_78[78:78]), .memory$memory_write_data(_78[77:46]), .memory$memory_write(_78[45:45]), .memory$memory_address(_78[44:13]), .memory$memory_valid(_78[12:12]), .output$output_packet_last(_78[11:11]), .output$output_packet_data(_78[10:3]), .output$output_packet_valid(_78[2:2]), .done_(_78[1:1]), .busy(_78[0:0]) );
    assign _92 = _78[12:12];
    assign _91 = _87[66:35];
    assign _90 = _87[34:34];
    assign _89 = _87[33:2];
    assign _162 = _99[36:5];
    assign _56 = _162;
    assign _163 = _99[4:4];
    assign _57 = _163;
    assign _164 = _99[3:3];
    assign _58 = _164;
    assign _165 = _99[0:0];
    assign _59 = _165;
    assign _85 = _73[9:9];
    assign _84 = _73[8:1];
    assign _70 = _68[8:1];
    assign _61 = uart_rx;
    Uart_tx
        rx
        ( .clock(_65), .clear(_63), .uart_rx(_61), .stop_bit_unstable(_68[10:10]), .parity_error(_68[9:9]), .data_out(_68[8:1]), .data_out_valid(_68[0:0]) );
    assign _69 = _68[0:0];
    Serial_to_packet
        serial_to_packet
        ( .clock(_65), .clear(_63), .in_valid(_69), .in_data(_70), .out_ready(vdd), .out_last(_73[9:9]), .out_data(_73[8:1]), .out_valid(_73[0:0]) );
    assign _74 = _73[0:0];
    dma
        dma
        ( .clock(_65), .clear(_63), .in__valid(_74), .in__data(_84), .in__last(_85), .out_ready(_59), .out_ack_valid(_58), .out_ack_error(_57), .out_ack_read_data(_56), .out_ack_ready(_87[67:67]), .out_write_data(_87[66:35]), .out_write(_87[34:34]), .out_address(_87[33:2]), .out_valid(_87[1:1]), .in__ready(_87[0:0]) );
    assign _88 = _87[1:1];
    Memory_controller
        Memory_controller
        ( .clock(_65), .clear(_63), .ch_co_controller$ch_to_controller_valid0(_88), .ch_co_controller$ch_to_controller_address0(_89), .ch_co_controller$ch_to_controller_write0(_90), .ch_co_controller$ch_to_controller_write_data0(_91), .ch_co_controller$ch_to_controller_valid1(_92), .ch_co_controller$ch_to_controller_address1(_93), .ch_co_controller$ch_to_controller_write1(_94), .ch_co_controller$ch_to_controller_write_data1(_95), .ch_co_controller$ch_to_controller_valid2(_47), .ch_co_controller$ch_to_controller_address2(_46), .ch_co_controller$ch_to_controller_write2(_45), .ch_co_controller$ch_to_controller_write_data2(_44), .controller_to_ch$controller_to_ch_ready0(_96), .controller_to_ch$controller_to_ch_ready1(_97), .controller_to_ch$controller_to_ch_ready2(_43), .controller_to_ch$controller_to_ch_read_data2(_99[104:73]), .controller_to_ch$controller_to_ch_error2(_99[72:72]), .controller_to_ch$controller_to_ch_valid2(_99[71:71]), .controller_to_ch$controller_to_ch_read_data1(_99[70:39]), .controller_to_ch$controller_to_ch_error1(_99[38:38]), .controller_to_ch$controller_to_ch_valid1(_99[37:37]), .controller_to_ch$controller_to_ch_read_data0(_99[36:5]), .controller_to_ch$controller_to_ch_error0(_99[4:4]), .controller_to_ch$controller_to_ch_valid0(_99[3:3]), .ch_co_controller$ch_to_controller_ready2(_99[2:2]), .ch_co_controller$ch_to_controller_ready1(_99[1:1]), .ch_co_controller$ch_to_controller_ready0(_99[0:0]) );
    assign _100 = _99[71:71];
    assign _63 = clear;
    assign _65 = clock;
    Hart
        hart_0
        ( .clock(_65), .clear(_63), .memory_controller_to_hartmemory_controller_to_hart_valid(_100), .memory_controller_to_hartmemory_controller_to_hart_error(_101), .memory_controller_to_hartmemory_controller_to_hart_read_data(_102), .hart_to_memory_controllerhart_to_memory_controller_ready(_103), .ecall_transaction_finished(_42), .ecall_transaction_set_rd(_41), .ecall_transaction_new_rd(_40), .ecall_transaction_new_pc(_39), .ecall_transaction_error(_38), .is_ecall(_105[1124:1124]), .error(_105[1123:1123]), .registers_general31(_105[1122:1091]), .registers_general30(_105[1090:1059]), .registers_general29(_105[1058:1027]), .registers_general28(_105[1026:995]), .registers_general27(_105[994:963]), .registers_general26(_105[962:931]), .registers_general25(_105[930:899]), .registers_general24(_105[898:867]), .registers_general23(_105[866:835]), .registers_general22(_105[834:803]), .registers_general21(_105[802:771]), .registers_general20(_105[770:739]), .registers_general19(_105[738:707]), .registers_general18(_105[706:675]), .registers_general17(_105[674:643]), .registers_general16(_105[642:611]), .registers_general15(_105[610:579]), .registers_general14(_105[578:547]), .registers_general13(_105[546:515]), .registers_general12(_105[514:483]), .registers_general11(_105[482:451]), .registers_general10(_105[450:419]), .registers_general9(_105[418:387]), .registers_general8(_105[386:355]), .registers_general7(_105[354:323]), .registers_general6(_105[322:291]), .registers_general5(_105[290:259]), .registers_general4(_105[258:227]), .registers_general3(_105[226:195]), .registers_general2(_105[194:163]), .registers_general1(_105[162:131]), .registers_general0(_105[130:99]), .registers_pc(_105[98:67]), .hart_to_memory_controllerhart_to_memory_controller_write_data(_105[66:35]), .hart_to_memory_controllerhart_to_memory_controller_write(_105[34:34]), .hart_to_memory_controllerhart_to_memory_controller_address(_105[33:2]), .hart_to_memory_controllerhart_to_memory_controller_valid(_105[1:1]), .memory_controller_to_hartmemory_controller_to_hart_ready(_105[0:0]) );
    assign _139 = _105[98:67];

    /* aliases */

    /* output assignments */
    assign registers_pc0 = _139;
    assign registers_general00 = _137;
    assign registers_general10 = _136;
    assign registers_general20 = _135;
    assign registers_general30 = _134;
    assign registers_general40 = _133;
    assign registers_general50 = _132;
    assign registers_general60 = _131;
    assign registers_general70 = _130;
    assign registers_general80 = _129;
    assign registers_general90 = _128;
    assign registers_general100 = _127;
    assign registers_general110 = _126;
    assign registers_general120 = _125;
    assign registers_general130 = _124;
    assign registers_general140 = _123;
    assign registers_general150 = _122;
    assign registers_general160 = _121;
    assign registers_general170 = _120;
    assign registers_general180 = _119;
    assign registers_general190 = _118;
    assign registers_general200 = _117;
    assign registers_general210 = _116;
    assign registers_general220 = _115;
    assign registers_general230 = _114;
    assign registers_general240 = _113;
    assign registers_general250 = _112;
    assign registers_general260 = _111;
    assign registers_general270 = _110;
    assign registers_general280 = _109;
    assign registers_general290 = _108;
    assign registers_general300 = _107;
    assign registers_general310 = _106;
    assign uart_tx = _83;
    assign uart_rx_valid = _69;
    assign parity_error = _76;
    assign stop_bit_unstable = _75;
    assign serial_to_packet_valid = _74;

endmodule
