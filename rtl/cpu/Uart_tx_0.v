module Uart_tx_0 (
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
    wire _25;
    wire _26;
    wire _149 = 1'b1;
    wire _146 = 1'b0;
    wire _147;
    wire _144;
    wire _142;
    wire _139 = 1'b1;
    wire _75 = 1'b0;
    wire _74 = 1'b0;
    wire _84 = 1'b0;
    wire _85;
    wire _82;
    wire _37 = 1'b0;
    wire _36 = 1'b0;
    wire _65 = 1'b0;
    wire _60;
    wire [1:0] _58;
    wire _59;
    wire [2:0] _56;
    wire _57;
    wire [3:0] _54;
    wire _55;
    wire [4:0] _52;
    wire _53;
    wire [5:0] _50;
    wire _51;
    wire [6:0] _48;
    wire _49;
    wire [7:0] _29 = 8'b00000000;
    wire [7:0] _28 = 8'b00000000;
    wire [7:0] _4;
    wire [7:0] _31;
    wire _27;
    wire [7:0] _32;
    wire [7:0] _5;
    reg [7:0] _30;
    wire _47;
    reg next_data_bit;
    wire _62;
    wire _63;
    wire _35;
    wire _64;
    wire _33;
    wire _66;
    wire _6;
    reg parity_bit;
    wire _80;
    wire _77 = 1'b1;
    wire _78;
    wire _73;
    wire _79;
    wire _71;
    wire _81;
    wire _69;
    wire _83;
    wire _68;
    wire _86;
    wire _7;
    reg _76;
    wire _140;
    wire _138 = 1'b0;
    wire _137;
    wire _141;
    wire _136;
    wire _143;
    wire _135;
    wire _145;
    wire _134;
    wire _148;
    wire [2:0] _22 = 3'b000;
    wire _9;
    wire [2:0] _21 = 3'b000;
    wire _11;
    wire [2:0] _131;
    wire [2:0] _129;
    wire [2:0] _124 = 3'b111;
    wire [2:0] _45 = 3'b000;
    wire [2:0] _44 = 3'b000;
    wire [2:0] _93 = 3'b000;
    wire [2:0] _89 = 3'b001;
    wire [2:0] _90;
    wire [2:0] _91;
    wire _88;
    wire [2:0] _92;
    wire _87;
    wire [2:0] _94;
    wire [2:0] _12;
    reg [2:0] _46;
    wire _125;
    wire [2:0] _126;
    wire [2:0] _127;
    wire [2:0] _122;
    wire [1:0] _117 = 2'b01;
    wire [1:0] _98 = 2'b00;
    wire [1:0] _97 = 2'b00;
    wire [1:0] _104 = 2'b00;
    wire [1:0] _100 = 2'b01;
    wire [1:0] _101;
    wire [1:0] _102;
    wire _96;
    wire [1:0] _103;
    wire _95;
    wire [1:0] _105;
    wire [1:0] _13;
    reg [1:0] _99;
    wire _118;
    wire [2:0] _119;
    wire [4:0] _42 = 5'b00000;
    wire vdd = 1'b1;
    wire [4:0] _40 = 5'b00000;
    wire [4:0] _39 = 5'b00000;
    wire _15;
    wire [4:0] _110 = 5'b00000;
    wire [4:0] _108 = 5'b00001;
    wire [4:0] _109;
    wire [4:0] _106 = 5'b10001;
    wire _107;
    wire [4:0] _111;
    wire [4:0] _16;
    reg [4:0] _41;
    wire switch_cycle;
    wire [2:0] _120;
    wire [2:0] _72 = 3'b100;
    wire _116;
    wire [2:0] _121;
    wire [2:0] _70 = 3'b011;
    wire _115;
    wire [2:0] _123;
    wire [2:0] _34 = 3'b010;
    wire _114;
    wire [2:0] _128;
    wire [2:0] _67 = 3'b001;
    wire _113;
    wire [2:0] _130;
    wire [2:0] _20 = 3'b000;
    wire _112;
    wire [2:0] _132;
    wire [2:0] _17;
    reg [2:0] current_state;
    wire _133;
    wire _150;
    wire _18;

    /* logic */
    assign _25 = _20 == current_state;
    assign _26 = _20 == current_state;
    assign _147 = switch_cycle ? _146 : _76;
    assign _144 = switch_cycle ? next_data_bit : _76;
    assign _142 = switch_cycle ? parity_bit : _76;
    assign _85 = switch_cycle ? _84 : _76;
    assign _82 = switch_cycle ? next_data_bit : _76;
    assign _60 = _58[1:1];
    assign _58 = _56[2:1];
    assign _59 = _58[0:0];
    assign _56 = _54[3:1];
    assign _57 = _56[0:0];
    assign _54 = _52[4:1];
    assign _55 = _54[0:0];
    assign _52 = _50[5:1];
    assign _53 = _52[0:0];
    assign _50 = _48[6:1];
    assign _51 = _50[0:0];
    assign _48 = _30[7:1];
    assign _49 = _48[0:0];
    assign _4 = data_in;
    assign _31 = _11 ? _4 : _30;
    assign _27 = current_state == _20;
    assign _32 = _27 ? _31 : _30;
    assign _5 = _32;
    always @(posedge _15) begin
        _30 <= _5;
    end
    assign _47 = _30[0:0];
    always @* begin
        case (_46)
        0: next_data_bit <= _47;
        1: next_data_bit <= _49;
        2: next_data_bit <= _51;
        3: next_data_bit <= _53;
        4: next_data_bit <= _55;
        5: next_data_bit <= _57;
        6: next_data_bit <= _59;
        default: next_data_bit <= _60;
        endcase
    end
    assign _62 = parity_bit + next_data_bit;
    assign _63 = switch_cycle ? _62 : parity_bit;
    assign _35 = current_state == _34;
    assign _64 = _35 ? _63 : parity_bit;
    assign _33 = current_state == _20;
    assign _66 = _33 ? _65 : _64;
    assign _6 = _66;
    always @(posedge _15) begin
        parity_bit <= _6;
    end
    assign _80 = switch_cycle ? parity_bit : _76;
    assign _78 = switch_cycle ? _77 : _76;
    assign _73 = current_state == _72;
    assign _79 = _73 ? _78 : _76;
    assign _71 = current_state == _70;
    assign _81 = _71 ? _80 : _79;
    assign _69 = current_state == _34;
    assign _83 = _69 ? _82 : _81;
    assign _68 = current_state == _67;
    assign _86 = _68 ? _85 : _83;
    assign _7 = _86;
    always @(posedge _15) begin
        _76 <= _7;
    end
    assign _140 = switch_cycle ? _139 : _76;
    assign _137 = current_state == _72;
    assign _141 = _137 ? _140 : _138;
    assign _136 = current_state == _70;
    assign _143 = _136 ? _142 : _141;
    assign _135 = current_state == _34;
    assign _145 = _135 ? _144 : _143;
    assign _134 = current_state == _67;
    assign _148 = _134 ? _147 : _145;
    assign _9 = clear;
    assign _11 = data_in_valid;
    assign _131 = _11 ? _67 : current_state;
    assign _129 = switch_cycle ? _34 : current_state;
    assign _90 = _46 + _89;
    assign _91 = switch_cycle ? _90 : _46;
    assign _88 = current_state == _34;
    assign _92 = _88 ? _91 : _46;
    assign _87 = current_state == _20;
    assign _94 = _87 ? _93 : _92;
    assign _12 = _94;
    always @(posedge _15) begin
        _46 <= _12;
    end
    assign _125 = _46 == _124;
    assign _126 = _125 ? _70 : current_state;
    assign _127 = switch_cycle ? _126 : current_state;
    assign _122 = switch_cycle ? _72 : current_state;
    assign _101 = _99 + _100;
    assign _102 = switch_cycle ? _101 : _99;
    assign _96 = current_state == _72;
    assign _103 = _96 ? _102 : _99;
    assign _95 = current_state == _20;
    assign _105 = _95 ? _104 : _103;
    assign _13 = _105;
    always @(posedge _15) begin
        _99 <= _13;
    end
    assign _118 = _99 == _117;
    assign _119 = _118 ? _20 : current_state;
    assign _15 = clock;
    assign _109 = _41 + _108;
    assign _107 = _41 == _106;
    assign _111 = _107 ? _110 : _109;
    assign _16 = _111;
    always @(posedge _15) begin
        _41 <= _16;
    end
    assign switch_cycle = _41 == _42;
    assign _120 = switch_cycle ? _119 : current_state;
    assign _116 = current_state == _72;
    assign _121 = _116 ? _120 : current_state;
    assign _115 = current_state == _70;
    assign _123 = _115 ? _122 : _121;
    assign _114 = current_state == _34;
    assign _128 = _114 ? _127 : _123;
    assign _113 = current_state == _67;
    assign _130 = _113 ? _129 : _128;
    assign _112 = current_state == _20;
    assign _132 = _112 ? _131 : _130;
    assign _17 = _132;
    always @(posedge _15) begin
        if (_9)
            current_state <= _22;
        else
            current_state <= _17;
    end
    assign _133 = current_state == _20;
    assign _150 = _133 ? _149 : _148;
    assign _18 = _150;

    /* aliases */

    /* output assignments */
    assign uart_tx = _18;
    assign data_in_ready = _26;
    assign idle = _25;

endmodule
