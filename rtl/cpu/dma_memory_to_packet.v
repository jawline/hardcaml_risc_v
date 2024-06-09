module dma_memory_to_packet (
    memory_response_read_data,
    memory_response_error,
    clear,
    memory_response_valid,
    enable_value$address,
    clock,
    enable_value$length,
    enable_valid,
    output_packet_ready,
    memory_ready,
    busy,
    done_,
    output$output_packet_valid,
    output$output_packet_data,
    output$output_packet_last,
    memory$memory_valid,
    memory$memory_address,
    memory$memory_write,
    memory$memory_write_data,
    mem_response$memory_response_ready
);

    input [31:0] memory_response_read_data;
    input memory_response_error;
    input clear;
    input memory_response_valid;
    input [31:0] enable_value$address;
    input clock;
    input [15:0] enable_value$length;
    input enable_valid;
    input output_packet_ready;
    input memory_ready;
    output busy;
    output done_;
    output output$output_packet_valid;
    output [7:0] output$output_packet_data;
    output output$output_packet_last;
    output memory$memory_valid;
    output [31:0] memory$memory_address;
    output memory$memory_write;
    output [31:0] memory$memory_write_data;
    output mem_response$memory_response_ready;

    /* signal declarations */
    wire [31:0] _49 = 32'b00000000000000000000000000000000;
    wire [31:0] _48 = 32'b00000000000000000000000000000000;
    wire _47;
    wire [31:0] _50;
    wire [31:0] _2;
    wire _52 = 1'b0;
    wire _51;
    wire _54;
    wire _4;
    wire [31:0] _56 = 32'b00000000000000000000000000000000;
    wire _55;
    wire [31:0] _60;
    wire [31:0] _6;
    wire _62 = 1'b0;
    wire _61;
    wire _63;
    wire _8;
    wire [15:0] _74 = 16'b0000000000000001;
    wire _75;
    wire _70 = 1'b0;
    wire _69;
    wire _76;
    wire _67;
    wire _77;
    wire _65;
    wire _78;
    wire _10;
    wire [7:0] _106 = 8'b01000100;
    wire [7:0] _103;
    wire [7:0] _102;
    reg [7:0] _104;
    wire [7:0] _99;
    wire [15:0] _97;
    wire [7:0] _98;
    wire [23:0] _95;
    wire [7:0] _96;
    wire [31:0] _81 = 32'b00000000000000000000000000000000;
    wire [31:0] _80 = 32'b00000000000000000000000000000000;
    wire [31:0] _83 = 32'b00000000000000000000000000000000;
    wire [31:0] _13;
    wire _15;
    wire [31:0] _84;
    wire [31:0] _85;
    wire _79;
    wire [31:0] _86;
    wire [31:0] _16;
    reg [31:0] _82;
    wire [7:0] _94;
    reg [7:0] _100;
    wire [7:0] _90 = 8'b00000000;
    wire _89;
    wire [7:0] _101;
    wire _88;
    wire [7:0] _105;
    wire _87;
    wire [7:0] _107;
    wire [7:0] _17;
    wire _111 = 1'b0;
    wire _110;
    wire _112;
    wire _109;
    wire _113;
    wire _108;
    wire _114;
    wire _19;
    wire _118;
    wire _119;
    wire gnd = 1'b0;
    wire _115;
    wire _120;
    wire _21;
    wire [2:0] _44 = 3'b000;
    wire _24;
    wire [2:0] _43 = 3'b000;
    wire [2:0] _189;
    wire [2:0] _187;
    wire [2:0] _184;
    wire [2:0] _185;
    wire is_memory_response_valid;
    wire [2:0] _182;
    wire [1:0] _125 = 2'b11;
    wire [1:0] _92 = 2'b00;
    wire [1:0] _91 = 2'b00;
    wire [1:0] _162 = 2'b00;
    wire [1:0] _163;
    wire [31:0] _58 = 32'b00000000000000000000000000000000;
    wire [31:0] _57 = 32'b00000000000000000000000000000000;
    wire [31:0] _28;
    wire [31:0] _143;
    wire [31:0] _134 = 32'b11111111111111111111111111111100;
    wire [31:0] _135;
    wire [31:0] _136;
    wire [31:0] _137;
    wire [31:0] _127 = 32'b00000000000000000000000000000100;
    wire [31:0] _128;
    wire [31:0] _129;
    wire [31:0] _130;
    wire _124;
    wire [31:0] _131;
    wire _123;
    wire [31:0] _138;
    wire _122;
    wire [31:0] _144;
    wire [31:0] _29;
    reg [31:0] _59;
    wire [1:0] _158;
    wire [1:0] _156 = 2'b01;
    wire [1:0] _157;
    wire [1:0] _132 = 2'b01;
    wire _133;
    wire [1:0] _159;
    wire [1:0] _160;
    wire [1:0] _152 = 2'b00;
    wire [1:0] _150 = 2'b00;
    wire [1:0] _148 = 2'b01;
    wire [1:0] _149;
    wire [1:0] _151;
    wire [1:0] _153;
    wire [1:0] _154;
    wire _147;
    wire [1:0] _155;
    wire _146;
    wire [1:0] _161;
    wire _145;
    wire [1:0] _164;
    wire [1:0] _30;
    reg [1:0] which_step;
    wire _126;
    wire [2:0] _178;
    wire [15:0] _116 = 16'b0000000000000001;
    wire vdd = 1'b1;
    wire [15:0] _72 = 16'b0000000000000000;
    wire [15:0] _71 = 16'b0000000000000000;
    wire _32;
    wire [15:0] _139 = 16'b0000000000000000;
    wire [15:0] _34;
    wire _140;
    wire _141;
    wire _36;
    wire _142;
    wire [15:0] _171;
    wire [15:0] _167 = 16'b0000000000000001;
    wire [15:0] _168;
    wire [15:0] _169;
    wire _166;
    wire [15:0] _170;
    wire _165;
    wire [15:0] _172;
    wire [15:0] _37;
    reg [15:0] _73;
    wire _117;
    wire [2:0] _179;
    wire _39;
    wire [2:0] _180;
    wire [2:0] _68 = 3'b100;
    wire _177;
    wire [2:0] _181;
    wire [2:0] _46 = 3'b011;
    wire _176;
    wire [2:0] _183;
    wire [2:0] _66 = 3'b010;
    wire _175;
    wire [2:0] _186;
    wire [2:0] _64 = 3'b001;
    wire _174;
    wire [2:0] _188;
    wire _173;
    wire [2:0] _190;
    wire [2:0] _40;
    reg [2:0] current_state;
    wire [2:0] _121 = 3'b000;
    wire _191;
    wire _192;

    /* logic */
    assign _47 = current_state == _46;
    assign _50 = _47 ? _49 : _48;
    assign _2 = _50;
    assign _51 = current_state == _46;
    assign _54 = _51 ? gnd : _52;
    assign _4 = _54;
    assign _55 = current_state == _46;
    assign _60 = _55 ? _59 : _56;
    assign _6 = _60;
    assign _61 = current_state == _46;
    assign _63 = _61 ? vdd : _62;
    assign _8 = _63;
    assign _75 = _73 == _74;
    assign _69 = current_state == _68;
    assign _76 = _69 ? _75 : _70;
    assign _67 = current_state == _66;
    assign _77 = _67 ? gnd : _76;
    assign _65 = current_state == _64;
    assign _78 = _65 ? gnd : _77;
    assign _10 = _78;
    assign _103 = _73[7:0];
    assign _102 = _73[15:8];
    always @* begin
        case (which_step)
        0: _104 <= _102;
        default: _104 <= _103;
        endcase
    end
    assign _99 = _97[15:8];
    assign _97 = _95[23:8];
    assign _98 = _97[7:0];
    assign _95 = _82[31:8];
    assign _96 = _95[7:0];
    assign _13 = memory_response_read_data;
    assign _15 = memory_response_error;
    assign _84 = _15 ? _83 : _13;
    assign _85 = is_memory_response_valid ? _84 : _82;
    assign _79 = current_state == _46;
    assign _86 = _79 ? _85 : _82;
    assign _16 = _86;
    always @(posedge _32) begin
        _82 <= _16;
    end
    assign _94 = _82[7:0];
    always @* begin
        case (which_step)
        0: _100 <= _94;
        1: _100 <= _96;
        2: _100 <= _98;
        default: _100 <= _99;
        endcase
    end
    assign _89 = current_state == _68;
    assign _101 = _89 ? _100 : _90;
    assign _88 = current_state == _66;
    assign _105 = _88 ? _104 : _101;
    assign _87 = current_state == _64;
    assign _107 = _87 ? _106 : _105;
    assign _17 = _107;
    assign _110 = current_state == _68;
    assign _112 = _110 ? vdd : _111;
    assign _109 = current_state == _66;
    assign _113 = _109 ? vdd : _112;
    assign _108 = current_state == _64;
    assign _114 = _108 ? vdd : _113;
    assign _19 = _114;
    assign _118 = _117 ? vdd : gnd;
    assign _119 = _39 ? _118 : gnd;
    assign _115 = current_state == _68;
    assign _120 = _115 ? _119 : gnd;
    assign _21 = _120;
    assign _24 = clear;
    assign _189 = _142 ? _64 : current_state;
    assign _187 = _39 ? _66 : current_state;
    assign _184 = _133 ? _46 : current_state;
    assign _185 = _39 ? _184 : current_state;
    assign is_memory_response_valid = memory_response_valid;
    assign _182 = is_memory_response_valid ? _68 : current_state;
    assign _163 = _142 ? _162 : which_step;
    assign _28 = enable_value$address;
    assign _143 = _142 ? _28 : _59;
    assign _135 = _59 & _134;
    assign _136 = _133 ? _135 : _59;
    assign _137 = _39 ? _136 : _59;
    assign _128 = _59 + _127;
    assign _129 = _126 ? _128 : _59;
    assign _130 = _39 ? _129 : _59;
    assign _124 = current_state == _68;
    assign _131 = _124 ? _130 : _59;
    assign _123 = current_state == _66;
    assign _138 = _123 ? _137 : _131;
    assign _122 = current_state == _121;
    assign _144 = _122 ? _143 : _138;
    assign _29 = _144;
    always @(posedge _32) begin
        _59 <= _29;
    end
    assign _158 = _59[1:0];
    assign _157 = which_step + _156;
    assign _133 = which_step == _132;
    assign _159 = _133 ? _158 : _157;
    assign _160 = _39 ? _159 : which_step;
    assign _149 = which_step + _148;
    assign _151 = _126 ? _150 : _149;
    assign _153 = _117 ? _152 : _151;
    assign _154 = _39 ? _153 : which_step;
    assign _147 = current_state == _68;
    assign _155 = _147 ? _154 : which_step;
    assign _146 = current_state == _66;
    assign _161 = _146 ? _160 : _155;
    assign _145 = current_state == _121;
    assign _164 = _145 ? _163 : _161;
    assign _30 = _164;
    always @(posedge _32) begin
        which_step <= _30;
    end
    assign _126 = which_step == _125;
    assign _178 = _126 ? _46 : current_state;
    assign _32 = clock;
    assign _34 = enable_value$length;
    assign _140 = _34 == _139;
    assign _141 = ~ _140;
    assign _36 = enable_valid;
    assign _142 = _36 & _141;
    assign _171 = _142 ? _34 : _73;
    assign _168 = _73 - _167;
    assign _169 = _39 ? _168 : _73;
    assign _166 = current_state == _68;
    assign _170 = _166 ? _169 : _73;
    assign _165 = current_state == _121;
    assign _172 = _165 ? _171 : _170;
    assign _37 = _172;
    always @(posedge _32) begin
        _73 <= _37;
    end
    assign _117 = _73 == _116;
    assign _179 = _117 ? _121 : _178;
    assign _39 = output_packet_ready;
    assign _180 = _39 ? _179 : current_state;
    assign _177 = current_state == _68;
    assign _181 = _177 ? _180 : current_state;
    assign _176 = current_state == _46;
    assign _183 = _176 ? _182 : _181;
    assign _175 = current_state == _66;
    assign _186 = _175 ? _185 : _183;
    assign _174 = current_state == _64;
    assign _188 = _174 ? _187 : _186;
    assign _173 = current_state == _121;
    assign _190 = _173 ? _189 : _188;
    assign _40 = _190;
    always @(posedge _32) begin
        if (_24)
            current_state <= _44;
        else
            current_state <= _40;
    end
    assign _191 = _121 == current_state;
    assign _192 = ~ _191;

    /* aliases */

    /* output assignments */
    assign busy = _192;
    assign done_ = _21;
    assign output$output_packet_valid = _19;
    assign output$output_packet_data = _17;
    assign output$output_packet_last = _10;
    assign memory$memory_valid = _8;
    assign memory$memory_address = _6;
    assign memory$memory_write = _4;
    assign memory$memory_write_data = _2;
    assign mem_response$memory_response_ready = vdd;

endmodule
