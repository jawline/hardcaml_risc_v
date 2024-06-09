module Serial_to_packet (
    clear,
    in_data,
    clock,
    in_valid,
    out_ready,
    out_valid,
    out_data,
    out_last
);

    input clear;
    input [7:0] in_data;
    input clock;
    input in_valid;
    input out_ready;
    output out_valid;
    output [7:0] out_data;
    output out_last;

    /* signal declarations */
    wire [4:0] _30 = 5'b00001;
    wire _31;
    wire _29;
    wire _32;
    wire _93;
    wire [7:0] _92 = 8'b00000000;
    wire [7:0] _91 = 8'b00000000;
    wire [7:0] _87 = 8'b00000000;
    wire [7:0] _86 = 8'b00000000;
    reg [7:0] _88;
    wire [7:0] _84 = 8'b00000000;
    wire [7:0] _83 = 8'b00000000;
    (* RAM_STYLE="block" *)
    reg [7:0] _81[0:15];
    wire [7:0] _82;
    reg [7:0] _85;
    wire _79 = 1'b0;
    wire _78 = 1'b0;
    wire [3:0] _36 = 4'b0000;
    wire [3:0] _35 = 4'b0000;
    wire [3:0] _33 = 4'b0001;
    wire [3:0] READ_ADDRESS_NEXT;
    reg [3:0] _55;
    wire [3:0] READ_ADDRESS;
    wire _54;
    wire [3:0] RA;
    wire [3:0] _59 = 4'b0000;
    wire [3:0] _58 = 4'b0000;
    wire [3:0] _56 = 4'b0001;
    wire [3:0] WRITE_ADDRESS_NEXT;
    reg [3:0] _70;
    wire [3:0] WRITE_ADDRESS;
    wire _76;
    wire _66;
    wire _67;
    wire _52;
    wire _51 = 1'b0;
    wire _50 = 1'b0;
    wire [4:0] _39 = 5'b00001;
    wire _49;
    reg _53;
    wire _68;
    wire _69;
    wire _77;
    reg _80;
    wire [7:0] _89;
    wire _64;
    wire _63 = 1'b0;
    wire _62 = 1'b0;
    wire [4:0] _60 = 5'b00001;
    wire _61;
    reg _65;
    wire _72;
    wire _73;
    wire _71;
    wire _74;
    wire [7:0] _90;
    reg [7:0] _94;
    wire _164 = 1'b0;
    wire [4:0] _162 = 5'b00000;
    wire [4:0] _45 = 5'b00001;
    wire [4:0] _46;
    wire [4:0] _43 = 5'b00001;
    wire [4:0] _44;
    wire [4:0] _47;
    wire [4:0] _96 = 5'b00000;
    wire [4:0] _95 = 5'b00000;
    reg [4:0] _97;
    wire [4:0] packet_buffer_used;
    wire _101 = 1'b0;
    wire _100 = 1'b0;
    wire [4:0] _98 = 5'b10001;
    wire _99;
    reg _102;
    wire _6;
    wire _40;
    wire _158 = 1'b1;
    wire _159;
    wire _157 = 1'b0;
    wire [1:0] _26 = 2'b00;
    wire _8;
    wire [1:0] _25 = 2'b00;
    wire [7:0] _151 = 8'b01010001;
    wire _152;
    wire _153;
    wire [1:0] _154;
    wire [3:0] _146 = 4'b0001;
    wire _147;
    wire [1:0] _148;
    wire [1:0] _149;
    wire [15:0] _141 = 16'b0000000000000001;
    wire [15:0] _120 = 16'b0000000000000000;
    wire [15:0] _119 = 16'b0000000000000000;
    wire [7:0] _128;
    wire [15:0] _129;
    wire [7:0] _126;
    wire [7:0] wr_data;
    wire [15:0] _127;
    wire vdd = 1'b1;
    wire [3:0] _108 = 4'b0000;
    wire [3:0] _107 = 4'b0000;
    wire _12;
    wire [3:0] _114 = 4'b0000;
    wire [3:0] _110 = 4'b0001;
    wire [3:0] _111;
    wire [3:0] _112;
    wire _106;
    wire [3:0] _113;
    wire _104;
    wire [3:0] _115;
    wire [3:0] _13;
    reg [3:0] _109;
    reg [15:0] _130;
    wire [15:0] _131;
    wire [15:0] _122 = 16'b0000000000000001;
    wire [15:0] _123;
    wire [15:0] _124;
    wire _118;
    wire [15:0] _125;
    wire _116;
    wire [15:0] _132;
    wire [15:0] _14;
    reg [15:0] _121;
    wire _142;
    wire [1:0] _143;
    wire _16;
    wire [1:0] _144;
    wire _138;
    wire [1:0] _139;
    wire [1:0] _24 = 2'b11;
    wire _136;
    wire [1:0] _140;
    wire [1:0] _117 = 2'b10;
    wire _135;
    wire [1:0] _145;
    wire [1:0] _105 = 2'b01;
    wire _134;
    wire [1:0] _150;
    wire [1:0] _103 = 2'b00;
    wire _133;
    wire [1:0] _155;
    wire [1:0] _17;
    reg [1:0] current_state;
    wire _156;
    wire _160;
    wire wr_enable;
    wire WR_INT;
    wire _37;
    wire _20;
    wire _161;
    wire rd_data;
    wire RD_INT;
    wire _42;
    wire [4:0] USED_NEXT;
    wire _163;
    reg _165;
    wire _22;
    wire have_buffered_packets;

    /* logic */
    assign _31 = packet_buffer_used == _30;
    assign _29 = _24 == current_state;
    assign _32 = _29 & _31;
    assign _93 = _74 | RD_INT;
    always @(posedge _12) begin
        _88 <= wr_data;
    end
    always @(posedge _12) begin
        if (_69)
            _81[WRITE_ADDRESS] <= wr_data;
    end
    assign _82 = _81[RA];
    always @(posedge _12) begin
        _85 <= _82;
    end
    assign READ_ADDRESS_NEXT = READ_ADDRESS + _33;
    always @(posedge _12) begin
        if (_8)
            _55 <= _36;
        else
            if (_54)
                _55 <= READ_ADDRESS_NEXT;
    end
    assign READ_ADDRESS = _55;
    assign _54 = RD_INT & _53;
    assign RA = _54 ? READ_ADDRESS_NEXT : READ_ADDRESS;
    assign WRITE_ADDRESS_NEXT = WRITE_ADDRESS + _56;
    always @(posedge _12) begin
        if (_8)
            _70 <= _59;
        else
            if (_69)
                _70 <= WRITE_ADDRESS_NEXT;
    end
    assign WRITE_ADDRESS = _70;
    assign _76 = WRITE_ADDRESS == RA;
    assign _66 = ~ RD_INT;
    assign _67 = _65 & _66;
    assign _52 = RD_INT ^ WR_INT;
    assign _49 = _39 < USED_NEXT;
    always @(posedge _12) begin
        if (_8)
            _53 <= _51;
        else
            if (_52)
                _53 <= _49;
    end
    assign _68 = _53 | _67;
    assign _69 = WR_INT & _68;
    assign _77 = _69 & _76;
    always @(posedge _12) begin
        _80 <= _77;
    end
    assign _89 = _80 ? _88 : _85;
    assign _64 = RD_INT ^ WR_INT;
    assign _61 = USED_NEXT == _60;
    always @(posedge _12) begin
        if (_8)
            _65 <= _63;
        else
            if (_64)
                _65 <= _61;
    end
    assign _72 = _65 & WR_INT;
    assign _73 = _72 & RD_INT;
    assign _71 = _22 & WR_INT;
    assign _74 = _71 | _73;
    assign _90 = _74 ? wr_data : _89;
    always @(posedge _12) begin
        if (_8)
            _94 <= _92;
        else
            if (_93)
                _94 <= _90;
    end
    assign _46 = packet_buffer_used - _45;
    assign _44 = packet_buffer_used + _43;
    assign _47 = RD_INT ? _46 : _44;
    always @(posedge _12) begin
        if (_8)
            _97 <= _96;
        else
            if (_42)
                _97 <= USED_NEXT;
    end
    assign packet_buffer_used = _97;
    assign _99 = USED_NEXT == _98;
    always @(posedge _12) begin
        if (_8)
            _102 <= _101;
        else
            if (_42)
                _102 <= _99;
    end
    assign _6 = _102;
    assign _40 = ~ _6;
    assign _159 = _16 ? _158 : _157;
    assign _8 = clear;
    assign _152 = wr_data == _151;
    assign _153 = _16 & _152;
    assign _154 = _153 ? _105 : current_state;
    assign _147 = _109 == _146;
    assign _148 = _147 ? _117 : current_state;
    assign _149 = _16 ? _148 : current_state;
    assign _128 = _121[15:8];
    assign _129 = { _128, wr_data };
    assign _126 = _121[7:0];
    assign wr_data = in_data;
    assign _127 = { wr_data, _126 };
    assign _12 = clock;
    assign _111 = _109 + _110;
    assign _112 = _16 ? _111 : _109;
    assign _106 = current_state == _105;
    assign _113 = _106 ? _112 : _109;
    assign _104 = current_state == _103;
    assign _115 = _104 ? _114 : _113;
    assign _13 = _115;
    always @(posedge _12) begin
        _109 <= _13;
    end
    always @* begin
        case (_109)
        0: _130 <= _127;
        default: _130 <= _129;
        endcase
    end
    assign _131 = _16 ? _130 : _121;
    assign _123 = _121 - _122;
    assign _124 = _16 ? _123 : _121;
    assign _118 = current_state == _117;
    assign _125 = _118 ? _124 : _121;
    assign _116 = current_state == _105;
    assign _132 = _116 ? _131 : _125;
    assign _14 = _132;
    always @(posedge _12) begin
        _121 <= _14;
    end
    assign _142 = _121 == _141;
    assign _143 = _142 ? _24 : current_state;
    assign _16 = in_valid;
    assign _144 = _16 ? _143 : current_state;
    assign _138 = ~ have_buffered_packets;
    assign _139 = _138 ? _103 : current_state;
    assign _136 = current_state == _24;
    assign _140 = _136 ? _139 : current_state;
    assign _135 = current_state == _117;
    assign _145 = _135 ? _144 : _140;
    assign _134 = current_state == _105;
    assign _150 = _134 ? _149 : _145;
    assign _133 = current_state == _103;
    assign _155 = _133 ? _154 : _150;
    assign _17 = _155;
    always @(posedge _12) begin
        if (_8)
            current_state <= _26;
        else
            current_state <= _17;
    end
    assign _156 = current_state == _117;
    assign _160 = _156 ? _159 : _157;
    assign wr_enable = _160;
    assign WR_INT = wr_enable & _40;
    assign _37 = ~ _22;
    assign _20 = out_ready;
    assign _161 = have_buffered_packets & _20;
    assign rd_data = _161;
    assign RD_INT = rd_data & _37;
    assign _42 = RD_INT ^ WR_INT;
    assign USED_NEXT = _42 ? _47 : packet_buffer_used;
    assign _163 = USED_NEXT == _162;
    always @(posedge _12) begin
        if (_8)
            _165 <= vdd;
        else
            if (_42)
                _165 <= _163;
    end
    assign _22 = _165;
    assign have_buffered_packets = ~ _22;

    /* aliases */

    /* output assignments */
    assign out_valid = have_buffered_packets;
    assign out_data = _94;
    assign out_last = _32;

endmodule
