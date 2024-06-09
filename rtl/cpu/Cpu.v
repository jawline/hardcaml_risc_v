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
    wire _69;
    wire [31:0] _100;
    wire [31:0] _101;
    wire [31:0] _102;
    wire [31:0] _103;
    wire [31:0] _104;
    wire [31:0] _105;
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
    wire [31:0] _131;
    wire _132 = 1'b0;
    wire _34;
    wire [31:0] _134 = 32'b00000000000000000000000000000100;
    wire [31:0] _135;
    wire [31:0] _35;
    wire [30:0] _136 = 31'b0000000000000000000000000000000;
    wire [31:0] _139;
    wire [31:0] _36;
    wire _37;
    wire _38;
    wire _97;
    wire [31:0] _96;
    wire _95;
    wire _140;
    wire _39;
    wire _91;
    wire _90;
    wire [31:0] _141;
    wire [31:0] _40;
    wire _142;
    wire _41;
    wire [31:0] _143;
    wire [31:0] _42;
    wire _144;
    wire _43;
    wire [31:0] _89;
    wire _88;
    wire [31:0] _87;
    wire [31:0] _145;
    wire [31:0] _44;
    wire _146;
    wire _45;
    wire _147;
    wire _46;
    wire _148;
    wire _47;
    wire [7:0] _66;
    wire _65;
    wire [2:0] _68;
    wire _149;
    wire _48;
    wire [31:0] _129;
    wire [31:0] _49;
    wire [31:0] _128;
    wire [15:0] _150;
    wire [15:0] _50;
    wire _137;
    wire _138;
    wire [31:0] _152 = 32'b00000000000000000000000000000000;
    wire [31:0] _130;
    wire _153;
    wire _151;
    wire _154;
    wire _155;
    wire tx_enable;
    wire [78:0] _64;
    wire _86;
    wire [31:0] _85;
    wire _84;
    wire [31:0] _83;
    wire [31:0] _156;
    wire [31:0] _52;
    wire _157;
    wire _53;
    wire _158;
    wire _54;
    wire _159;
    wire _55;
    wire _79;
    wire [7:0] _78;
    wire vdd = 1'b1;
    wire [7:0] _73;
    wire _57;
    wire [10:0] _71;
    wire _72;
    wire [9:0] _76;
    wire _77;
    wire [67:0] _81;
    wire _82;
    wire [104:0] _93;
    wire _94;
    wire _59;
    wire _61;
    wire [1124:0] _99;
    wire [31:0] _133;

    /* logic */
    assign _69 = _68[0:0];
    assign _100 = _99[1122:1091];
    assign _101 = _99[1090:1059];
    assign _102 = _99[1058:1027];
    assign _103 = _99[1026:995];
    assign _104 = _99[994:963];
    assign _105 = _99[962:931];
    assign _106 = _99[930:899];
    assign _107 = _99[898:867];
    assign _108 = _99[866:835];
    assign _109 = _99[834:803];
    assign _110 = _99[802:771];
    assign _111 = _99[770:739];
    assign _112 = _99[738:707];
    assign _113 = _99[706:675];
    assign _114 = _99[674:643];
    assign _115 = _99[642:611];
    assign _116 = _99[610:579];
    assign _117 = _99[578:547];
    assign _118 = _99[546:515];
    assign _119 = _99[514:483];
    assign _120 = _99[482:451];
    assign _121 = _99[450:419];
    assign _122 = _99[418:387];
    assign _123 = _99[386:355];
    assign _124 = _99[354:323];
    assign _125 = _99[322:291];
    assign _126 = _99[290:259];
    assign _127 = _99[258:227];
    assign _131 = _99[130:99];
    assign _34 = _132;
    assign _135 = _133 + _134;
    assign _35 = _135;
    assign _139 = { _136, _138 };
    assign _36 = _139;
    assign _37 = vdd;
    assign _38 = vdd;
    assign _97 = _93[2:2];
    assign _96 = _93[104:73];
    assign _95 = _93[72:72];
    assign _140 = _99[0:0];
    assign _39 = _140;
    assign _91 = _64[78:78];
    assign _90 = _81[67:67];
    assign _141 = _99[66:35];
    assign _40 = _141;
    assign _142 = _99[34:34];
    assign _41 = _142;
    assign _143 = _99[33:2];
    assign _42 = _143;
    assign _144 = _99[1:1];
    assign _43 = _144;
    assign _89 = _64[77:46];
    assign _88 = _64[45:45];
    assign _87 = _64[44:13];
    assign _145 = _93[70:39];
    assign _44 = _145;
    assign _146 = _93[38:38];
    assign _45 = _146;
    assign _147 = _93[37:37];
    assign _46 = _147;
    assign _148 = _93[1:1];
    assign _47 = _148;
    assign _66 = _64[10:3];
    assign _65 = _64[2:2];
    Uart_tx_0
        tx
        ( .clock(_61), .clear(_59), .data_in_valid(_65), .data_in(_66), .idle(_68[2:2]), .data_in_ready(_68[1:1]), .uart_tx(_68[0:0]) );
    assign _149 = _68[1:1];
    assign _48 = _149;
    assign _129 = _99[194:163];
    assign _49 = _129;
    assign _128 = _99[226:195];
    assign _150 = _128[15:0];
    assign _50 = _150;
    assign _137 = _64[0:0];
    assign _138 = ~ _137;
    assign _130 = _99[162:131];
    assign _153 = _130 == _152;
    assign _151 = _99[1124:1124];
    assign _154 = _151 & _153;
    assign _155 = _154 & _138;
    assign tx_enable = _155;
    dma_memory_to_packet
        dma_out
        ( .clock(_61), .clear(_59), .enable_valid(tx_enable), .enable_value$length(_50), .enable_value$address(_49), .output_packet_ready(_48), .memory_ready(_47), .memory_response_valid(_46), .memory_response_error(_45), .memory_response_read_data(_44), .mem_response$memory_response_ready(_64[78:78]), .memory$memory_write_data(_64[77:46]), .memory$memory_write(_64[45:45]), .memory$memory_address(_64[44:13]), .memory$memory_valid(_64[12:12]), .output$output_packet_last(_64[11:11]), .output$output_packet_data(_64[10:3]), .output$output_packet_valid(_64[2:2]), .done_(_64[1:1]), .busy(_64[0:0]) );
    assign _86 = _64[12:12];
    assign _85 = _81[66:35];
    assign _84 = _81[34:34];
    assign _83 = _81[33:2];
    assign _156 = _93[36:5];
    assign _52 = _156;
    assign _157 = _93[4:4];
    assign _53 = _157;
    assign _158 = _93[3:3];
    assign _54 = _158;
    assign _159 = _93[0:0];
    assign _55 = _159;
    assign _79 = _76[9:9];
    assign _78 = _76[8:1];
    assign _73 = _71[8:1];
    assign _57 = uart_rx;
    Uart_tx
        rx
        ( .clock(_61), .clear(_59), .uart_rx(_57), .stop_bit_unstable(_71[10:10]), .parity_error(_71[9:9]), .data_out(_71[8:1]), .data_out_valid(_71[0:0]) );
    assign _72 = _71[0:0];
    Serial_to_packet
        serial_to_packet
        ( .clock(_61), .clear(_59), .in_valid(_72), .in_data(_73), .out_ready(vdd), .out_last(_76[9:9]), .out_data(_76[8:1]), .out_valid(_76[0:0]) );
    assign _77 = _76[0:0];
    dma
        dma
        ( .clock(_61), .clear(_59), .in__valid(_77), .in__data(_78), .in__last(_79), .out_ready(_55), .out_ack_valid(_54), .out_ack_error(_53), .out_ack_read_data(_52), .out_ack_ready(_81[67:67]), .out_write_data(_81[66:35]), .out_write(_81[34:34]), .out_address(_81[33:2]), .out_valid(_81[1:1]), .in__ready(_81[0:0]) );
    assign _82 = _81[1:1];
    Memory_controller
        Memory_controller
        ( .clock(_61), .clear(_59), .ch_co_controller$ch_to_controller_valid0(_82), .ch_co_controller$ch_to_controller_address0(_83), .ch_co_controller$ch_to_controller_write0(_84), .ch_co_controller$ch_to_controller_write_data0(_85), .ch_co_controller$ch_to_controller_valid1(_86), .ch_co_controller$ch_to_controller_address1(_87), .ch_co_controller$ch_to_controller_write1(_88), .ch_co_controller$ch_to_controller_write_data1(_89), .ch_co_controller$ch_to_controller_valid2(_43), .ch_co_controller$ch_to_controller_address2(_42), .ch_co_controller$ch_to_controller_write2(_41), .ch_co_controller$ch_to_controller_write_data2(_40), .controller_to_ch$controller_to_ch_ready0(_90), .controller_to_ch$controller_to_ch_ready1(_91), .controller_to_ch$controller_to_ch_ready2(_39), .controller_to_ch$controller_to_ch_read_data2(_93[104:73]), .controller_to_ch$controller_to_ch_error2(_93[72:72]), .controller_to_ch$controller_to_ch_valid2(_93[71:71]), .controller_to_ch$controller_to_ch_read_data1(_93[70:39]), .controller_to_ch$controller_to_ch_error1(_93[38:38]), .controller_to_ch$controller_to_ch_valid1(_93[37:37]), .controller_to_ch$controller_to_ch_read_data0(_93[36:5]), .controller_to_ch$controller_to_ch_error0(_93[4:4]), .controller_to_ch$controller_to_ch_valid0(_93[3:3]), .ch_co_controller$ch_to_controller_ready2(_93[2:2]), .ch_co_controller$ch_to_controller_ready1(_93[1:1]), .ch_co_controller$ch_to_controller_ready0(_93[0:0]) );
    assign _94 = _93[71:71];
    assign _59 = clear;
    assign _61 = clock;
    Hart
        hart_0
        ( .clock(_61), .clear(_59), .memory_controller_to_hartmemory_controller_to_hart_valid(_94), .memory_controller_to_hartmemory_controller_to_hart_error(_95), .memory_controller_to_hartmemory_controller_to_hart_read_data(_96), .hart_to_memory_controllerhart_to_memory_controller_ready(_97), .ecall_transaction_finished(_38), .ecall_transaction_set_rd(_37), .ecall_transaction_new_rd(_36), .ecall_transaction_new_pc(_35), .ecall_transaction_error(_34), .is_ecall(_99[1124:1124]), .error(_99[1123:1123]), .registers_general31(_99[1122:1091]), .registers_general30(_99[1090:1059]), .registers_general29(_99[1058:1027]), .registers_general28(_99[1026:995]), .registers_general27(_99[994:963]), .registers_general26(_99[962:931]), .registers_general25(_99[930:899]), .registers_general24(_99[898:867]), .registers_general23(_99[866:835]), .registers_general22(_99[834:803]), .registers_general21(_99[802:771]), .registers_general20(_99[770:739]), .registers_general19(_99[738:707]), .registers_general18(_99[706:675]), .registers_general17(_99[674:643]), .registers_general16(_99[642:611]), .registers_general15(_99[610:579]), .registers_general14(_99[578:547]), .registers_general13(_99[546:515]), .registers_general12(_99[514:483]), .registers_general11(_99[482:451]), .registers_general10(_99[450:419]), .registers_general9(_99[418:387]), .registers_general8(_99[386:355]), .registers_general7(_99[354:323]), .registers_general6(_99[322:291]), .registers_general5(_99[290:259]), .registers_general4(_99[258:227]), .registers_general3(_99[226:195]), .registers_general2(_99[194:163]), .registers_general1(_99[162:131]), .registers_general0(_99[130:99]), .registers_pc(_99[98:67]), .hart_to_memory_controllerhart_to_memory_controller_write_data(_99[66:35]), .hart_to_memory_controllerhart_to_memory_controller_write(_99[34:34]), .hart_to_memory_controllerhart_to_memory_controller_address(_99[33:2]), .hart_to_memory_controllerhart_to_memory_controller_valid(_99[1:1]), .memory_controller_to_hartmemory_controller_to_hart_ready(_99[0:0]) );
    assign _133 = _99[98:67];

    /* aliases */

    /* output assignments */
    assign registers_pc0 = _133;
    assign registers_general00 = _131;
    assign registers_general10 = _130;
    assign registers_general20 = _129;
    assign registers_general30 = _128;
    assign registers_general40 = _127;
    assign registers_general50 = _126;
    assign registers_general60 = _125;
    assign registers_general70 = _124;
    assign registers_general80 = _123;
    assign registers_general90 = _122;
    assign registers_general100 = _121;
    assign registers_general110 = _120;
    assign registers_general120 = _119;
    assign registers_general130 = _118;
    assign registers_general140 = _117;
    assign registers_general150 = _116;
    assign registers_general160 = _115;
    assign registers_general170 = _114;
    assign registers_general180 = _113;
    assign registers_general190 = _112;
    assign registers_general200 = _111;
    assign registers_general210 = _110;
    assign registers_general220 = _109;
    assign registers_general230 = _108;
    assign registers_general240 = _107;
    assign registers_general250 = _106;
    assign registers_general260 = _105;
    assign registers_general270 = _104;
    assign registers_general280 = _103;
    assign registers_general290 = _102;
    assign registers_general300 = _101;
    assign registers_general310 = _100;
    assign uart_tx = _69;

endmodule
