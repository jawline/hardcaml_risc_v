module cpu (
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
    wire _78;
    wire _79;
    wire _86;
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
    wire [31:0] _134;
    wire [31:0] _135;
    wire [31:0] _136;
    wire [31:0] _137;
    wire [31:0] _138;
    wire [31:0] _139;
    wire [31:0] _140;
    wire [31:0] _141;
    wire [31:0] _142;
    wire [31:0] _143;
    wire [31:0] _144;
    wire [31:0] _145;
    wire [31:0] _146;
    wire [31:0] _147;
    wire [31:0] _148;
    wire [31:0] _152;
    wire _153 = 1'b0;
    wire _38;
    wire [31:0] _155 = 32'b00000000000000000000000000000100;
    wire [31:0] _156;
    wire [31:0] _39;
    wire [30:0] _157 = 31'b0000000000000000000000000000000;
    wire [31:0] _160;
    wire [31:0] _40;
    wire _41;
    wire _42;
    wire _118;
    wire [31:0] _117;
    wire _116;
    wire _115;
    wire [7:0] _90 = 8'b00000000;
    wire [7:0] _88 = 8'b00000000;
    wire [7:0] _87 = 8'b00000000;
    wire [7:0] _195 = 8'b00000000;
    wire [7:0] _193 = 8'b00000000;
    wire [7:0] _191 = 8'b00000001;
    wire [7:0] _192;
    wire [7:0] _189 = 8'b00000000;
    wire _190;
    wire [7:0] _194;
    wire _163;
    wire [7:0] _162;
    wire _166;
    wire _43;
    wire [31:0] _167;
    wire [31:0] _44;
    wire _168;
    wire _45;
    wire _169;
    wire _46;
    wire _170;
    wire _47;
    wire _112;
    wire _111;
    wire [31:0] _171;
    wire [31:0] _48;
    wire _172;
    wire _49;
    wire [31:0] _173;
    wire [31:0] _50;
    wire _174;
    wire _51;
    wire [31:0] _110;
    wire _109;
    wire [31:0] _108;
    wire [31:0] _175;
    wire [31:0] _52;
    wire _176;
    wire _53;
    wire _177;
    wire _54;
    wire _178;
    wire _55;
    wire [7:0] _83;
    wire _82;
    wire [2:0] _85;
    wire _179;
    wire _56;
    wire [31:0] _150;
    wire [31:0] _57;
    wire [31:0] _149;
    wire [15:0] _180;
    wire [15:0] _58;
    wire _158;
    wire _159;
    wire [31:0] _182 = 32'b00000000000000000000000000000000;
    wire [31:0] _151;
    wire _183;
    wire _181;
    wire _184;
    wire _185;
    wire tx_enable;
    wire [78:0] _81;
    wire _107;
    wire [31:0] _106;
    wire _105;
    wire [31:0] _104;
    wire _103;
    wire [104:0] _114;
    wire _186;
    wire _60;
    wire _100;
    wire [7:0] _99;
    wire _98;
    wire [67:0] _102;
    wire _187;
    wire _61;
    wire _95;
    wire [7:0] _94;
    wire vdd = 1'b1;
    wire [7:0] _73;
    wire _63;
    wire [10:0] _71;
    wire _72;
    wire [9:0] _76;
    wire _77;
    wire [20:0] _97;
    wire _161;
    wire _65;
    wire [1:0] _165;
    wire _188;
    wire [7:0] _196;
    wire [7:0] _66;
    reg [7:0] _89;
    wire _91;
    wire clear_message;
    wire _93;
    wire _68;
    wire [1124:0] _120;
    wire [31:0] _154;

    /* logic */
    assign _78 = _71[10:10];
    assign _79 = _71[9:9];
    assign _86 = _85[0:0];
    assign _121 = _120[1122:1091];
    assign _122 = _120[1090:1059];
    assign _123 = _120[1058:1027];
    assign _124 = _120[1026:995];
    assign _125 = _120[994:963];
    assign _126 = _120[962:931];
    assign _127 = _120[930:899];
    assign _128 = _120[898:867];
    assign _129 = _120[866:835];
    assign _130 = _120[834:803];
    assign _131 = _120[802:771];
    assign _132 = _120[770:739];
    assign _133 = _120[738:707];
    assign _134 = _120[706:675];
    assign _135 = _120[674:643];
    assign _136 = _120[642:611];
    assign _137 = _120[610:579];
    assign _138 = _120[578:547];
    assign _139 = _120[546:515];
    assign _140 = _120[514:483];
    assign _141 = _120[482:451];
    assign _142 = _120[450:419];
    assign _143 = _120[418:387];
    assign _144 = _120[386:355];
    assign _145 = _120[354:323];
    assign _146 = _120[322:291];
    assign _147 = _120[290:259];
    assign _148 = _120[258:227];
    assign _152 = _120[130:99];
    assign _38 = _153;
    assign _156 = _154 + _155;
    assign _39 = _156;
    assign _160 = { _157, _159 };
    assign _40 = _160;
    assign _41 = vdd;
    assign _42 = vdd;
    assign _118 = _114[2:2];
    assign _117 = _114[104:73];
    assign _116 = _114[72:72];
    assign _115 = _114[71:71];
    assign _192 = _89 - _191;
    assign _190 = _89 == _189;
    assign _194 = _190 ? _193 : _192;
    assign _163 = _97[20:20];
    assign _162 = _97[19:12];
    assign _166 = _165[0:0];
    assign _43 = _166;
    assign _167 = _114[36:5];
    assign _44 = _167;
    assign _168 = _114[4:4];
    assign _45 = _168;
    assign _169 = _114[3:3];
    assign _46 = _169;
    assign _170 = _120[0:0];
    assign _47 = _170;
    assign _112 = _81[78:78];
    assign _111 = _102[67:67];
    assign _171 = _120[66:35];
    assign _48 = _171;
    assign _172 = _120[34:34];
    assign _49 = _172;
    assign _173 = _120[33:2];
    assign _50 = _173;
    assign _174 = _120[1:1];
    assign _51 = _174;
    assign _110 = _81[77:46];
    assign _109 = _81[45:45];
    assign _108 = _81[44:13];
    assign _175 = _114[70:39];
    assign _52 = _175;
    assign _176 = _114[38:38];
    assign _53 = _176;
    assign _177 = _114[37:37];
    assign _54 = _177;
    assign _178 = _114[1:1];
    assign _55 = _178;
    assign _83 = _81[10:3];
    assign _82 = _81[2:2];
    uart_tx
        tx
        ( .clock(_68), .clear(_65), .data_in_valid(_82), .data_in(_83), .idle(_85[2:2]), .data_in_ready(_85[1:1]), .uart_tx(_85[0:0]) );
    assign _179 = _85[1:1];
    assign _56 = _179;
    assign _150 = _120[194:163];
    assign _57 = _150;
    assign _149 = _120[226:195];
    assign _180 = _149[15:0];
    assign _58 = _180;
    assign _158 = _81[0:0];
    assign _159 = ~ _158;
    assign _151 = _120[162:131];
    assign _183 = _151 == _182;
    assign _181 = _120[1124:1124];
    assign _184 = _181 & _183;
    assign _185 = _184 & _159;
    assign tx_enable = _185;
    dma_memory_to_packet
        dma_out
        ( .clock(_68), .clear(_65), .enable_valid(tx_enable), .enable_value$length(_58), .enable_value$address(_57), .output_packet_ready(_56), .memory_ready(_55), .memory_response_valid(_54), .memory_response_error(_53), .memory_response_read_data(_52), .mem_response$memory_response_ready(_81[78:78]), .memory$memory_write_data(_81[77:46]), .memory$memory_write(_81[45:45]), .memory$memory_address(_81[44:13]), .memory$memory_valid(_81[12:12]), .output$output_packet_last(_81[11:11]), .output$output_packet_data(_81[10:3]), .output$output_packet_valid(_81[2:2]), .done_(_81[1:1]), .busy(_81[0:0]) );
    assign _107 = _81[12:12];
    assign _106 = _102[66:35];
    assign _105 = _102[34:34];
    assign _104 = _102[33:2];
    assign _103 = _102[1:1];
    memory_controller
        Memory_controller
        ( .clock(_68), .clear(_65), .ch_co_controller$ch_to_controller_valid0(_103), .ch_co_controller$ch_to_controller_address0(_104), .ch_co_controller$ch_to_controller_write0(_105), .ch_co_controller$ch_to_controller_write_data0(_106), .ch_co_controller$ch_to_controller_valid1(_107), .ch_co_controller$ch_to_controller_address1(_108), .ch_co_controller$ch_to_controller_write1(_109), .ch_co_controller$ch_to_controller_write_data1(_110), .ch_co_controller$ch_to_controller_valid2(_51), .ch_co_controller$ch_to_controller_address2(_50), .ch_co_controller$ch_to_controller_write2(_49), .ch_co_controller$ch_to_controller_write_data2(_48), .controller_to_ch$controller_to_ch_ready0(_111), .controller_to_ch$controller_to_ch_ready1(_112), .controller_to_ch$controller_to_ch_ready2(_47), .controller_to_ch$controller_to_ch_read_data2(_114[104:73]), .controller_to_ch$controller_to_ch_error2(_114[72:72]), .controller_to_ch$controller_to_ch_valid2(_114[71:71]), .controller_to_ch$controller_to_ch_read_data1(_114[70:39]), .controller_to_ch$controller_to_ch_error1(_114[38:38]), .controller_to_ch$controller_to_ch_valid1(_114[37:37]), .controller_to_ch$controller_to_ch_read_data0(_114[36:5]), .controller_to_ch$controller_to_ch_error0(_114[4:4]), .controller_to_ch$controller_to_ch_valid0(_114[3:3]), .ch_co_controller$ch_to_controller_ready2(_114[2:2]), .ch_co_controller$ch_to_controller_ready1(_114[1:1]), .ch_co_controller$ch_to_controller_ready0(_114[0:0]) );
    assign _186 = _114[0:0];
    assign _60 = _186;
    assign _100 = _97[10:10];
    assign _99 = _97[9:2];
    assign _98 = _97[1:1];
    dma
        dma
        ( .clock(_68), .clear(_65), .in__valid(_98), .in__data(_99), .in__last(_100), .out_ready(_60), .out_ack_valid(_46), .out_ack_error(_45), .out_ack_read_data(_44), .out_ack_ready(_102[67:67]), .out_write_data(_102[66:35]), .out_write(_102[34:34]), .out_address(_102[33:2]), .out_valid(_102[1:1]), .in__ready(_102[0:0]) );
    assign _187 = _102[0:0];
    assign _61 = _187;
    assign _95 = _76[9:9];
    assign _94 = _76[8:1];
    assign _73 = _71[8:1];
    assign _63 = uart_rx;
    uart_rx
        rx
        ( .clock(_68), .clear(_65), .uart_rx(_63), .stop_bit_unstable(_71[10:10]), .parity_error(_71[9:9]), .data_out(_71[8:1]), .data_out_valid(_71[0:0]) );
    assign _72 = _71[0:0];
    serial_to_packet
        serial_to_packet
        ( .clock(_68), .clear(_65), .in_valid(_72), .in_data(_73), .out_ready(vdd), .out_last(_76[9:9]), .out_data(_76[8:1]), .out_valid(_76[0:0]) );
    assign _77 = _76[0:0];
    packet_router
        io_packet_router
        ( .clock(_68), .clear(_65), .valid(_77), .data(_94), .last(_95), .ready0(_61), .ready1(_43), .last1(_97[20:20]), .data1(_97[19:12]), .valid1(_97[11:11]), .last0(_97[10:10]), .data0(_97[9:2]), .valid0(_97[1:1]), .ready(_97[0:0]) );
    assign _161 = _97[11:11];
    assign _65 = clear;
    signal_0
        pulse
        ( .clock(_68), .clear(_65), .valid(_161), .data(_162), .last(_163), .signal(_165[1:1]), .ready(_165[0:0]) );
    assign _188 = _165[1:1];
    assign _196 = _188 ? _195 : _194;
    assign _66 = _196;
    always @(posedge _68) begin
        _89 <= _66;
    end
    assign _91 = _89 == _90;
    assign clear_message = ~ _91;
    assign _93 = clear_message | _65;
    assign _68 = clock;
    hart
        hart_0
        ( .clock(_68), .clear(_93), .memory_controller_to_hartmemory_controller_to_hart_valid(_115), .memory_controller_to_hartmemory_controller_to_hart_error(_116), .memory_controller_to_hartmemory_controller_to_hart_read_data(_117), .hart_to_memory_controllerhart_to_memory_controller_ready(_118), .ecall_transaction_finished(_42), .ecall_transaction_set_rd(_41), .ecall_transaction_new_rd(_40), .ecall_transaction_new_pc(_39), .ecall_transaction_error(_38), .is_ecall(_120[1124:1124]), .error(_120[1123:1123]), .registers_general31(_120[1122:1091]), .registers_general30(_120[1090:1059]), .registers_general29(_120[1058:1027]), .registers_general28(_120[1026:995]), .registers_general27(_120[994:963]), .registers_general26(_120[962:931]), .registers_general25(_120[930:899]), .registers_general24(_120[898:867]), .registers_general23(_120[866:835]), .registers_general22(_120[834:803]), .registers_general21(_120[802:771]), .registers_general20(_120[770:739]), .registers_general19(_120[738:707]), .registers_general18(_120[706:675]), .registers_general17(_120[674:643]), .registers_general16(_120[642:611]), .registers_general15(_120[610:579]), .registers_general14(_120[578:547]), .registers_general13(_120[546:515]), .registers_general12(_120[514:483]), .registers_general11(_120[482:451]), .registers_general10(_120[450:419]), .registers_general9(_120[418:387]), .registers_general8(_120[386:355]), .registers_general7(_120[354:323]), .registers_general6(_120[322:291]), .registers_general5(_120[290:259]), .registers_general4(_120[258:227]), .registers_general3(_120[226:195]), .registers_general2(_120[194:163]), .registers_general1(_120[162:131]), .registers_general0(_120[130:99]), .registers_pc(_120[98:67]), .hart_to_memory_controllerhart_to_memory_controller_write_data(_120[66:35]), .hart_to_memory_controllerhart_to_memory_controller_write(_120[34:34]), .hart_to_memory_controllerhart_to_memory_controller_address(_120[33:2]), .hart_to_memory_controllerhart_to_memory_controller_valid(_120[1:1]), .memory_controller_to_hartmemory_controller_to_hart_ready(_120[0:0]) );
    assign _154 = _120[98:67];

    /* aliases */

    /* output assignments */
    assign registers_pc0 = _154;
    assign registers_general00 = _152;
    assign registers_general10 = _151;
    assign registers_general20 = _150;
    assign registers_general30 = _149;
    assign registers_general40 = _148;
    assign registers_general50 = _147;
    assign registers_general60 = _146;
    assign registers_general70 = _145;
    assign registers_general80 = _144;
    assign registers_general90 = _143;
    assign registers_general100 = _142;
    assign registers_general110 = _141;
    assign registers_general120 = _140;
    assign registers_general130 = _139;
    assign registers_general140 = _138;
    assign registers_general150 = _137;
    assign registers_general160 = _136;
    assign registers_general170 = _135;
    assign registers_general180 = _134;
    assign registers_general190 = _133;
    assign registers_general200 = _132;
    assign registers_general210 = _131;
    assign registers_general220 = _130;
    assign registers_general230 = _129;
    assign registers_general240 = _128;
    assign registers_general250 = _127;
    assign registers_general260 = _126;
    assign registers_general270 = _125;
    assign registers_general280 = _124;
    assign registers_general290 = _123;
    assign registers_general300 = _122;
    assign registers_general310 = _121;
    assign uart_tx = _86;
    assign uart_rx_valid = _72;
    assign parity_error = _79;
    assign stop_bit_unstable = _78;
    assign serial_to_packet_valid = _77;

endmodule
