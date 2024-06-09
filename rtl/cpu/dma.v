module dma (
    in__data,
    clear,
    clock,
    in__valid,
    out_ready,
    in__last,
    out_ack_error,
    out_ack_read_data,
    out_ack_valid,
    in__ready,
    out_valid,
    out_address,
    out_write,
    out_write_data,
    out_ack_ready
);

    input [7:0] in__data;
    input clear;
    input clock;
    input in__valid;
    input out_ready;
    input in__last;
    input out_ack_error;
    input [31:0] out_ack_read_data;
    input out_ack_valid;
    output in__ready;
    output out_valid;
    output [31:0] out_address;
    output out_write;
    output [31:0] out_write_data;
    output out_ack_ready;

    /* signal declarations */
    wire [31:0] _91 = 32'b00000000000000000000000000000000;
    wire [31:0] _90 = 32'b00000000000000000000000000000000;
    wire [7:0] _49 = 8'b00000000;
    wire [7:0] _48 = 8'b00000000;
    wire [7:0] _51 = 8'b00000000;
    wire [1:0] _46 = 2'b00;
    wire _47;
    wire [7:0] _52;
    wire [1:0] _43 = 2'b00;
    wire _44;
    wire _45;
    wire [7:0] _53;
    wire [7:0] _2;
    reg [7:0] _50;
    wire [7:0] _60 = 8'b00000000;
    wire [7:0] _59 = 8'b00000000;
    wire [7:0] _62 = 8'b00000000;
    wire [1:0] _57 = 2'b00;
    wire _58;
    wire [7:0] _63;
    wire [1:0] _54 = 2'b01;
    wire _55;
    wire _56;
    wire [7:0] _64;
    wire [7:0] _3;
    reg [7:0] _61;
    wire [7:0] _71 = 8'b00000000;
    wire [7:0] _70 = 8'b00000000;
    wire [7:0] _73 = 8'b00000000;
    wire [1:0] _68 = 2'b00;
    wire _69;
    wire [7:0] _74;
    wire [1:0] _65 = 2'b10;
    wire _66;
    wire _67;
    wire [7:0] _75;
    wire [7:0] _4;
    reg [7:0] _72;
    wire [7:0] _82 = 8'b00000000;
    wire [7:0] _81 = 8'b00000000;
    wire [7:0] _84 = 8'b00000000;
    wire [1:0] _79 = 2'b00;
    wire _80;
    wire [7:0] _85;
    wire [1:0] _76 = 2'b11;
    wire _77;
    wire _78;
    wire [7:0] _86;
    wire [7:0] _5;
    reg [7:0] _83;
    wire [31:0] data_buffer;
    wire [31:0] data_buffer_0;
    wire _89;
    wire [31:0] _94;
    wire _87;
    wire [31:0] _95;
    wire [31:0] _6;
    reg [31:0] data_out;
    wire [31:0] _101 = 32'b00000000000000000000000000000000;
    wire [31:0] _100 = 32'b00000000000000000000000000000000;
    wire [31:0] _133;
    wire [31:0] _103 = 32'b00000000000000000000000000000100;
    wire [31:0] _104;
    wire [31:0] _105;
    wire _99;
    wire [31:0] _106;
    wire _97;
    wire [31:0] _134;
    wire [31:0] _9;
    reg [31:0] data_out_address;
    wire data_out_valid;
    wire _211 = 1'b1;
    wire _209 = 1'b1;
    wire _207 = 1'b1;
    wire _206 = 1'b0;
    wire _205;
    wire _208;
    wire _204;
    wire _210;
    wire [2:0] _36 = 3'b000;
    wire [2:0] _35 = 3'b000;
    wire [31:0] _198 = 32'b00000000000000000000000000000000;
    wire [31:0] _196 = 32'b00000000000000000000000000000010;
    wire [7:0] _130 = 8'b00000000;
    wire [7:0] _129 = 8'b00000000;
    wire [7:0] _141 = 8'b00000000;
    wire [1:0] _139 = 2'b00;
    wire _140;
    wire [7:0] _142;
    wire [1:0] _136 = 2'b11;
    wire _137;
    wire _138;
    wire [7:0] _143;
    wire [7:0] _12;
    reg [7:0] _131;
    wire [7:0] _127 = 8'b00000000;
    wire [7:0] _126 = 8'b00000000;
    wire [7:0] _149 = 8'b00000000;
    wire [1:0] _147 = 2'b00;
    wire _148;
    wire [7:0] _150;
    wire [1:0] _144 = 2'b10;
    wire _145;
    wire _146;
    wire [7:0] _151;
    wire [7:0] _13;
    reg [7:0] _128;
    wire [7:0] _124 = 8'b00000000;
    wire [7:0] _123 = 8'b00000000;
    wire [7:0] _157 = 8'b00000000;
    wire [1:0] _155 = 2'b00;
    wire _156;
    wire [7:0] _158;
    wire [1:0] _152 = 2'b01;
    wire _153;
    wire _154;
    wire [7:0] _159;
    wire [7:0] _14;
    reg [7:0] _125;
    wire [7:0] _121 = 8'b00000000;
    wire [7:0] _120 = 8'b00000000;
    wire [7:0] _16;
    wire [7:0] _165 = 8'b00000000;
    wire [1:0] _163 = 2'b00;
    wire _164;
    wire [7:0] _166;
    wire [1:0] _160 = 2'b00;
    wire _161;
    wire _162;
    wire [7:0] _167;
    wire [7:0] _17;
    reg [7:0] _122;
    wire [31:0] address_buffer;
    wire [31:0] _197;
    wire _199;
    wire [2:0] _200;
    wire _118 = 1'b0;
    wire _117 = 1'b0;
    wire _115;
    wire [1:0] _110 = 2'b11;
    wire [1:0] _108 = 2'b00;
    wire [1:0] _107 = 2'b00;
    wire [1:0] _171 = 2'b00;
    wire [1:0] _168 = 2'b01;
    wire [1:0] _169;
    wire _112;
    wire _113;
    wire [1:0] _170;
    wire [1:0] _172;
    wire [1:0] _18;
    reg [1:0] _109;
    wire _111;
    wire _114;
    wire _116;
    reg _119;
    wire [2:0] _201;
    wire [2:0] _193;
    wire _191 = 1'b0;
    wire _190 = 1'b0;
    wire [1:0] _187 = 2'b11;
    wire vdd = 1'b1;
    wire [1:0] _41 = 2'b00;
    wire _20;
    wire [1:0] _40 = 2'b00;
    wire _22;
    wire [1:0] _173 = 2'b01;
    wire [1:0] _174;
    wire _24;
    wire _38;
    wire _39;
    wire [1:0] _175;
    wire [1:0] _25;
    reg [1:0] _42;
    wire _188;
    wire _189;
    reg _192;
    wire [2:0] _194;
    wire _27;
    wire [2:0] _184;
    wire _29;
    wire [2:0] _182;
    wire [2:0] _180 = 3'b100;
    wire _181;
    wire [2:0] _183;
    wire [2:0] _98 = 3'b011;
    wire _179;
    wire [2:0] _185;
    wire [2:0] _88 = 3'b010;
    wire _178;
    wire [2:0] _186;
    wire [2:0] _34 = 3'b001;
    wire _177;
    wire [2:0] _195;
    wire [2:0] _96 = 3'b000;
    wire _176;
    wire [2:0] _202;
    wire [2:0] _30;
    reg [2:0] current_state;
    wire _203;
    wire _212;
    wire _31;

    /* logic */
    assign _47 = _42 == _46;
    assign _52 = _47 ? _51 : _50;
    assign _44 = _42 == _43;
    assign _45 = _39 & _44;
    assign _53 = _45 ? _16 : _52;
    assign _2 = _53;
    always @(posedge _22) begin
        _50 <= _2;
    end
    assign _58 = _42 == _57;
    assign _63 = _58 ? _62 : _61;
    assign _55 = _42 == _54;
    assign _56 = _39 & _55;
    assign _64 = _56 ? _16 : _63;
    assign _3 = _64;
    always @(posedge _22) begin
        _61 <= _3;
    end
    assign _69 = _42 == _68;
    assign _74 = _69 ? _73 : _72;
    assign _66 = _42 == _65;
    assign _67 = _39 & _66;
    assign _75 = _67 ? _16 : _74;
    assign _4 = _75;
    always @(posedge _22) begin
        _72 <= _4;
    end
    assign _80 = _42 == _79;
    assign _85 = _80 ? _84 : _83;
    assign _77 = _42 == _76;
    assign _78 = _39 & _77;
    assign _86 = _78 ? _16 : _85;
    assign _5 = _86;
    always @(posedge _22) begin
        _83 <= _5;
    end
    assign data_buffer = { _83, _72, _61, _50 };
    assign _89 = current_state == _88;
    assign _94 = _89 ? data_buffer : data_out;
    assign _87 = current_state == _34;
    assign _95 = _87 ? data_buffer : _94;
    assign _6 = _95;
    always @(posedge _22) begin
        data_out <= _6;
    end
    assign _133 = _119 ? address_buffer : data_out_address;
    assign _104 = data_out_address + _103;
    assign _105 = _27 ? _104 : data_out_address;
    assign _99 = current_state == _98;
    assign _106 = _99 ? _105 : data_out_address;
    assign _97 = current_state == _96;
    assign _134 = _97 ? _133 : _106;
    assign _9 = _134;
    always @(posedge _22) begin
        data_out_address <= _9;
    end
    assign data_out_valid = _98 == current_state;
    assign _205 = current_state == _180;
    assign _208 = _205 ? _207 : _206;
    assign _204 = current_state == _34;
    assign _210 = _204 ? _209 : _208;
    assign _140 = _109 == _139;
    assign _142 = _140 ? _141 : _131;
    assign _137 = _109 == _136;
    assign _138 = _113 & _137;
    assign _143 = _138 ? _16 : _142;
    assign _12 = _143;
    always @(posedge _22) begin
        _131 <= _12;
    end
    assign _148 = _109 == _147;
    assign _150 = _148 ? _149 : _128;
    assign _145 = _109 == _144;
    assign _146 = _113 & _145;
    assign _151 = _146 ? _16 : _150;
    assign _13 = _151;
    always @(posedge _22) begin
        _128 <= _13;
    end
    assign _156 = _109 == _155;
    assign _158 = _156 ? _157 : _125;
    assign _153 = _109 == _152;
    assign _154 = _113 & _153;
    assign _159 = _154 ? _16 : _158;
    assign _14 = _159;
    always @(posedge _22) begin
        _125 <= _14;
    end
    assign _16 = in__data;
    assign _164 = _109 == _163;
    assign _166 = _164 ? _165 : _122;
    assign _161 = _109 == _160;
    assign _162 = _113 & _161;
    assign _167 = _162 ? _16 : _166;
    assign _17 = _167;
    always @(posedge _22) begin
        _122 <= _17;
    end
    assign address_buffer = { _122, _125, _128, _131 };
    assign _197 = address_buffer & _196;
    assign _199 = _197 == _198;
    assign _200 = _199 ? _34 : _180;
    assign _115 = ~ _29;
    assign _169 = _109 + _168;
    assign _112 = _96 == current_state;
    assign _113 = _112 & _24;
    assign _170 = _113 ? _169 : _109;
    assign _172 = _29 ? _171 : _170;
    assign _18 = _172;
    always @(posedge _22) begin
        if (_20)
            _109 <= _108;
        else
            _109 <= _18;
    end
    assign _111 = _109 == _110;
    assign _114 = _111 & _113;
    assign _116 = _114 & _115;
    always @(posedge _22) begin
        _119 <= _116;
    end
    assign _201 = _119 ? _200 : current_state;
    assign _193 = _29 ? _88 : current_state;
    assign _20 = clear;
    assign _22 = clock;
    assign _174 = _42 + _173;
    assign _24 = in__valid;
    assign _38 = _34 == current_state;
    assign _39 = _38 & _24;
    assign _175 = _39 ? _174 : _42;
    assign _25 = _175;
    always @(posedge _22) begin
        if (_20)
            _42 <= _41;
        else
            _42 <= _25;
    end
    assign _188 = _42 == _187;
    assign _189 = _188 & _39;
    always @(posedge _22) begin
        _192 <= _189;
    end
    assign _194 = _192 ? _98 : _193;
    assign _27 = out_ready;
    assign _184 = _27 ? _34 : current_state;
    assign _29 = in__last;
    assign _182 = _29 ? _96 : current_state;
    assign _181 = current_state == _180;
    assign _183 = _181 ? _182 : current_state;
    assign _179 = current_state == _98;
    assign _185 = _179 ? _184 : _183;
    assign _178 = current_state == _88;
    assign _186 = _178 ? _98 : _185;
    assign _177 = current_state == _34;
    assign _195 = _177 ? _194 : _186;
    assign _176 = current_state == _96;
    assign _202 = _176 ? _201 : _195;
    assign _30 = _202;
    always @(posedge _22) begin
        if (_20)
            current_state <= _36;
        else
            current_state <= _30;
    end
    assign _203 = current_state == _96;
    assign _212 = _203 ? _211 : _210;
    assign _31 = _212;

    /* aliases */
    assign data_buffer_0 = data_buffer;

    /* output assignments */
    assign in__ready = _31;
    assign out_valid = data_out_valid;
    assign out_address = data_out_address;
    assign out_write = vdd;
    assign out_write_data = data_out;
    assign out_ack_ready = vdd;

endmodule
