module uart_tx (
    data_in,
    clear,
    data_in_valid,
    clock,
    uart_tx,
    data_in_ready,
    idle
);

    input [7:0] data_in;
    input clear;
    input data_in_valid;
    input clock;
    output uart_tx;
    output data_in_ready;
    output idle;

    /* signal declarations */
    wire _26;
    wire _27;
    wire _155 = 1'b1;
    wire _152 = 1'b0;
    wire _153;
    wire _150;
    wire _148;
    wire _145 = 1'b1;
    wire _76 = 1'b0;
    wire _75 = 1'b0;
    wire _85 = 1'b0;
    wire _86;
    wire _83;
    wire _38 = 1'b0;
    wire _37 = 1'b0;
    wire _66 = 1'b0;
    wire _61;
    wire [1:0] _59;
    wire _60;
    wire [2:0] _57;
    wire _58;
    wire [3:0] _55;
    wire _56;
    wire [4:0] _53;
    wire _54;
    wire [5:0] _51;
    wire _52;
    wire [6:0] _49;
    wire _50;
    wire [7:0] _30 = 8'b00000000;
    wire [7:0] _29 = 8'b00000000;
    wire [7:0] _4;
    wire [7:0] _32;
    wire _28;
    wire [7:0] _33;
    wire [7:0] _5;
    reg [7:0] _31;
    wire _48;
    reg next_data_bit;
    wire _63;
    wire _64;
    wire _36;
    wire _65;
    wire _34;
    wire _67;
    wire _6;
    reg parity_bit;
    wire _81;
    wire _78 = 1'b1;
    wire _79;
    wire _74;
    wire _80;
    wire _72;
    wire _82;
    wire _70;
    wire _84;
    wire _69;
    wire _87;
    wire _7;
    reg _77;
    wire _146;
    wire _144 = 1'b0;
    wire _143;
    wire _147;
    wire _142;
    wire _149;
    wire _141;
    wire _151;
    wire _140;
    wire _154;
    wire [2:0] _23 = 3'b000;
    wire _9;
    wire [2:0] _22 = 3'b000;
    wire _11;
    wire [2:0] _137;
    wire [2:0] _135;
    wire [2:0] _130 = 3'b111;
    wire [2:0] _46 = 3'b000;
    wire [2:0] _45 = 3'b000;
    wire [2:0] _94 = 3'b000;
    wire [2:0] _90 = 3'b001;
    wire [2:0] _91;
    wire [2:0] _92;
    wire _89;
    wire [2:0] _93;
    wire _88;
    wire [2:0] _95;
    wire [2:0] _12;
    reg [2:0] _47;
    wire _131;
    wire [2:0] _132;
    wire [2:0] _133;
    wire [2:0] _128;
    wire [1:0] _123 = 2'b00;
    wire [1:0] _99 = 2'b00;
    wire [1:0] _98 = 2'b00;
    wire [1:0] _105 = 2'b00;
    wire [1:0] _101 = 2'b01;
    wire [1:0] _102;
    wire [1:0] _103;
    wire _97;
    wire [1:0] _104;
    wire _96;
    wire [1:0] _106;
    wire [1:0] _13;
    reg [1:0] which_stop_bit;
    wire _124;
    wire [2:0] _125;
    wire [13:0] _43 = 14'b00000000000000;
    wire [13:0] _41 = 14'b00000000000000;
    wire [13:0] _40 = 14'b00000000000000;
    wire _15;
    wire [13:0] _116 = 14'b00000000000001;
    wire [13:0] _114 = 14'b00000000000000;
    wire [13:0] _112 = 14'b00000000000001;
    wire [13:0] _113;
    wire [13:0] _110 = 14'b11110100001000;
    wire _111;
    wire [13:0] _115;
    wire vdd = 1'b1;
    wire _108 = 1'b0;
    wire _107;
    wire _109;
    wire _16;
    wire [13:0] _117;
    wire [13:0] _17;
    reg [13:0] _42;
    wire switch_cycle;
    wire [2:0] _126;
    wire [2:0] _73 = 3'b100;
    wire _122;
    wire [2:0] _127;
    wire [2:0] _71 = 3'b011;
    wire _121;
    wire [2:0] _129;
    wire [2:0] _35 = 3'b010;
    wire _120;
    wire [2:0] _134;
    wire [2:0] _68 = 3'b001;
    wire _119;
    wire [2:0] _136;
    wire [2:0] _21 = 3'b000;
    wire _118;
    wire [2:0] _138;
    wire [2:0] _18;
    reg [2:0] current_state;
    wire _139;
    wire _156;
    wire _19;

    /* logic */
    assign _26 = _21 == current_state;
    assign _27 = _21 == current_state;
    assign _153 = switch_cycle ? _152 : _77;
    assign _150 = switch_cycle ? next_data_bit : _77;
    assign _148 = switch_cycle ? parity_bit : _77;
    assign _86 = switch_cycle ? _85 : _77;
    assign _83 = switch_cycle ? next_data_bit : _77;
    assign _61 = _59[1:1];
    assign _59 = _57[2:1];
    assign _60 = _59[0:0];
    assign _57 = _55[3:1];
    assign _58 = _57[0:0];
    assign _55 = _53[4:1];
    assign _56 = _55[0:0];
    assign _53 = _51[5:1];
    assign _54 = _53[0:0];
    assign _51 = _49[6:1];
    assign _52 = _51[0:0];
    assign _49 = _31[7:1];
    assign _50 = _49[0:0];
    assign _4 = data_in;
    assign _32 = _11 ? _4 : _31;
    assign _28 = current_state == _21;
    assign _33 = _28 ? _32 : _31;
    assign _5 = _33;
    always @(posedge _15) begin
        _31 <= _5;
    end
    assign _48 = _31[0:0];
    always @* begin
        case (_47)
        0: next_data_bit <= _48;
        1: next_data_bit <= _50;
        2: next_data_bit <= _52;
        3: next_data_bit <= _54;
        4: next_data_bit <= _56;
        5: next_data_bit <= _58;
        6: next_data_bit <= _60;
        default: next_data_bit <= _61;
        endcase
    end
    assign _63 = parity_bit + next_data_bit;
    assign _64 = switch_cycle ? _63 : parity_bit;
    assign _36 = current_state == _35;
    assign _65 = _36 ? _64 : parity_bit;
    assign _34 = current_state == _21;
    assign _67 = _34 ? _66 : _65;
    assign _6 = _67;
    always @(posedge _15) begin
        parity_bit <= _6;
    end
    assign _81 = switch_cycle ? parity_bit : _77;
    assign _79 = switch_cycle ? _78 : _77;
    assign _74 = current_state == _73;
    assign _80 = _74 ? _79 : _77;
    assign _72 = current_state == _71;
    assign _82 = _72 ? _81 : _80;
    assign _70 = current_state == _35;
    assign _84 = _70 ? _83 : _82;
    assign _69 = current_state == _68;
    assign _87 = _69 ? _86 : _84;
    assign _7 = _87;
    always @(posedge _15) begin
        _77 <= _7;
    end
    assign _146 = switch_cycle ? _145 : _77;
    assign _143 = current_state == _73;
    assign _147 = _143 ? _146 : _144;
    assign _142 = current_state == _71;
    assign _149 = _142 ? _148 : _147;
    assign _141 = current_state == _35;
    assign _151 = _141 ? _150 : _149;
    assign _140 = current_state == _68;
    assign _154 = _140 ? _153 : _151;
    assign _9 = clear;
    assign _11 = data_in_valid;
    assign _137 = _11 ? _68 : current_state;
    assign _135 = switch_cycle ? _35 : current_state;
    assign _91 = _47 + _90;
    assign _92 = switch_cycle ? _91 : _47;
    assign _89 = current_state == _35;
    assign _93 = _89 ? _92 : _47;
    assign _88 = current_state == _21;
    assign _95 = _88 ? _94 : _93;
    assign _12 = _95;
    always @(posedge _15) begin
        _47 <= _12;
    end
    assign _131 = _47 == _130;
    assign _132 = _131 ? _73 : current_state;
    assign _133 = switch_cycle ? _132 : current_state;
    assign _128 = switch_cycle ? _73 : current_state;
    assign _102 = which_stop_bit + _101;
    assign _103 = switch_cycle ? _102 : which_stop_bit;
    assign _97 = current_state == _73;
    assign _104 = _97 ? _103 : which_stop_bit;
    assign _96 = current_state == _21;
    assign _106 = _96 ? _105 : _104;
    assign _13 = _106;
    always @(posedge _15) begin
        which_stop_bit <= _13;
    end
    assign _124 = which_stop_bit == _123;
    assign _125 = _124 ? _21 : current_state;
    assign _15 = clock;
    assign _113 = _42 + _112;
    assign _111 = _42 == _110;
    assign _115 = _111 ? _114 : _113;
    assign _107 = current_state == _21;
    assign _109 = _107 ? vdd : _108;
    assign _16 = _109;
    assign _117 = _16 ? _116 : _115;
    assign _17 = _117;
    always @(posedge _15) begin
        _42 <= _17;
    end
    assign switch_cycle = _42 == _43;
    assign _126 = switch_cycle ? _125 : current_state;
    assign _122 = current_state == _73;
    assign _127 = _122 ? _126 : current_state;
    assign _121 = current_state == _71;
    assign _129 = _121 ? _128 : _127;
    assign _120 = current_state == _35;
    assign _134 = _120 ? _133 : _129;
    assign _119 = current_state == _68;
    assign _136 = _119 ? _135 : _134;
    assign _118 = current_state == _21;
    assign _138 = _118 ? _137 : _136;
    assign _18 = _138;
    always @(posedge _15) begin
        if (_9)
            current_state <= _23;
        else
            current_state <= _18;
    end
    assign _139 = current_state == _21;
    assign _156 = _139 ? _155 : _154;
    assign _19 = _156;

    /* aliases */

    /* output assignments */
    assign uart_tx = _19;
    assign data_in_ready = _27;
    assign idle = _26;

endmodule
