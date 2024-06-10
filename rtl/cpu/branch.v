module branch (
    b_immediate,
    pc,
    rhs,
    lhs,
    funct3,
    new_rd,
    error
);

    input [31:0] b_immediate;
    input [31:0] pc;
    input [31:0] rhs;
    input [31:0] lhs;
    input [2:0] funct3;
    output [31:0] new_rd;
    output error;

    /* signal declarations */
    wire _19 = 1'b0;
    wire _18 = 1'b0;
    wire _17 = 1'b0;
    wire _16 = 1'b0;
    wire vdd = 1'b1;
    wire _14 = 1'b0;
    wire _13 = 1'b0;
    reg _20;
    wire [31:0] _69;
    wire [31:0] _67 = 32'b00000000000000000000000000000100;
    wire [31:0] _68;
    wire _65;
    wire _66;
    wire [31:0] _70;
    wire [31:0] _63;
    wire [31:0] _61 = 32'b00000000000000000000000000000100;
    wire [31:0] _62;
    wire _60;
    wire [31:0] _64;
    wire [31:0] _58;
    wire [31:0] _56 = 32'b00000000000000000000000000000100;
    wire [31:0] _57;
    wire [30:0] _52;
    wire _50;
    wire _51;
    wire [31:0] _53;
    wire [30:0] _48;
    wire _46;
    wire _47;
    wire [31:0] _49;
    wire _54;
    wire _55;
    wire [31:0] _59;
    wire [31:0] _44;
    wire [31:0] _42 = 32'b00000000000000000000000000000100;
    wire [31:0] _43;
    wire [30:0] _39;
    wire _37;
    wire _38;
    wire [31:0] _40;
    wire [30:0] _35;
    wire _33;
    wire _34;
    wire [31:0] _36;
    wire _41;
    wire [31:0] _45;
    wire [31:0] _32 = 32'b00000000000000000000000000000000;
    wire [31:0] _30;
    wire [31:0] _28 = 32'b00000000000000000000000000000100;
    wire [31:0] _29;
    wire _26;
    wire _27;
    wire [31:0] _31;
    wire [31:0] _3;
    wire [31:0] _24;
    wire [31:0] _22 = 32'b00000000000000000000000000000100;
    wire [31:0] _5;
    wire [31:0] _23;
    wire [31:0] _7;
    wire [31:0] _9;
    wire _21;
    wire [31:0] _25;
    wire [2:0] _11;
    reg [31:0] _71;

    /* logic */
    always @* begin
        case (_11)
        0: _20 <= _13;
        1: _20 <= _14;
        2: _20 <= vdd;
        3: _20 <= vdd;
        4: _20 <= _16;
        5: _20 <= _17;
        6: _20 <= _18;
        default: _20 <= _19;
        endcase
    end
    assign _69 = _5 + _3;
    assign _68 = _5 + _67;
    assign _65 = _9 < _7;
    assign _66 = ~ _65;
    assign _70 = _66 ? _69 : _68;
    assign _63 = _5 + _3;
    assign _62 = _5 + _61;
    assign _60 = _9 < _7;
    assign _64 = _60 ? _63 : _62;
    assign _58 = _5 + _3;
    assign _57 = _5 + _56;
    assign _52 = _7[30:0];
    assign _50 = _7[31:31];
    assign _51 = ~ _50;
    assign _53 = { _51, _52 };
    assign _48 = _9[30:0];
    assign _46 = _9[31:31];
    assign _47 = ~ _46;
    assign _49 = { _47, _48 };
    assign _54 = _49 < _53;
    assign _55 = ~ _54;
    assign _59 = _55 ? _58 : _57;
    assign _44 = _5 + _3;
    assign _43 = _5 + _42;
    assign _39 = _7[30:0];
    assign _37 = _7[31:31];
    assign _38 = ~ _37;
    assign _40 = { _38, _39 };
    assign _35 = _9[30:0];
    assign _33 = _9[31:31];
    assign _34 = ~ _33;
    assign _36 = { _34, _35 };
    assign _41 = _36 < _40;
    assign _45 = _41 ? _44 : _43;
    assign _30 = _5 + _3;
    assign _29 = _5 + _28;
    assign _26 = _9 == _7;
    assign _27 = ~ _26;
    assign _31 = _27 ? _30 : _29;
    assign _3 = b_immediate;
    assign _24 = _5 + _3;
    assign _5 = pc;
    assign _23 = _5 + _22;
    assign _7 = rhs;
    assign _9 = lhs;
    assign _21 = _9 == _7;
    assign _25 = _21 ? _24 : _23;
    assign _11 = funct3;
    always @* begin
        case (_11)
        0: _71 <= _25;
        1: _71 <= _31;
        2: _71 <= _32;
        3: _71 <= _32;
        4: _71 <= _45;
        5: _71 <= _59;
        6: _71 <= _64;
        default: _71 <= _70;
        endcase
    end

    /* aliases */

    /* output assignments */
    assign new_rd = _71;
    assign error = _20;

endmodule
