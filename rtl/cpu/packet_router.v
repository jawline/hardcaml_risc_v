module packet_router (
    ready1,
    ready0,
    clear,
    clock,
    data,
    valid,
    last,
    ready,
    valid0,
    data0,
    last0,
    valid1,
    data1,
    last1
);

    input ready1;
    input ready0;
    input clear;
    input clock;
    input [7:0] data;
    input valid;
    input last;
    output ready;
    output valid0;
    output [7:0] data0;
    output last0;
    output valid1;
    output [7:0] data1;
    output last1;

    /* signal declarations */
    wire _36 = 1'b0;
    wire _37;
    wire [7:0] _38 = 8'b00000000;
    wire [7:0] _39;
    wire _40 = 1'b0;
    wire _34;
    wire _28 = 1'b1;
    wire _29;
    wire _35;
    wire _41;
    wire _46 = 1'b0;
    wire _47;
    wire [7:0] _48 = 8'b00000000;
    wire [7:0] _49;
    wire _50 = 1'b0;
    wire _44;
    wire _42 = 1'b0;
    wire _43;
    wire _45;
    wire _51;
    wire _77;
    wire _75;
    wire _8;
    wire _10;
    wire _25 = 1'b0;
    wire _24 = 1'b0;
    wire _54;
    wire _53;
    wire _55;
    wire _11;
    reg which_tag;
    wire which_tag_0;
    wire _73;
    wire vdd = 1'b1;
    wire [1:0] _32 = 2'b00;
    wire _13;
    wire [1:0] _31 = 2'b00;
    wire _15;
    wire [7:0] _17;
    wire [7:0] _65 = 8'b00000001;
    wire _66;
    wire _67;
    wire [1:0] _68;
    wire _64;
    wire [1:0] _69;
    wire _19;
    wire [1:0] _70;
    wire [1:0] _62;
    wire _21;
    wire [1:0] _60;
    wire [1:0] _58 = 2'b10;
    wire _59;
    wire [1:0] _61;
    wire _57;
    wire [1:0] _63;
    wire [1:0] _52 = 2'b00;
    wire _56;
    wire [1:0] _71;
    wire [1:0] _22;
    reg [1:0] current_state;
    wire [1:0] _30 = 2'b01;
    wire _72;
    wire _74;
    wire _76;
    wire _78;

    /* logic */
    assign _37 = _35 ? _21 : _36;
    assign _39 = _35 ? _17 : _38;
    assign _34 = _30 == current_state;
    assign _29 = which_tag == _28;
    assign _35 = _29 & _34;
    assign _41 = _35 ? _19 : _40;
    assign _47 = _45 ? _21 : _46;
    assign _49 = _45 ? _17 : _48;
    assign _44 = _30 == current_state;
    assign _43 = which_tag == _42;
    assign _45 = _43 & _44;
    assign _51 = _45 ? _19 : _50;
    assign _77 = _58 == current_state;
    assign _75 = _52 == current_state;
    assign _8 = ready1;
    assign _10 = ready0;
    assign _54 = _17[0:0];
    assign _53 = current_state == _52;
    assign _55 = _53 ? _54 : which_tag;
    assign _11 = _55;
    always @(posedge _15) begin
        which_tag <= _11;
    end
    assign _73 = which_tag ? _8 : _10;
    assign _13 = clear;
    assign _15 = clock;
    assign _17 = data;
    assign _66 = _65 < _17;
    assign _67 = ~ _66;
    assign _68 = _67 ? _30 : _58;
    assign _64 = ~ _21;
    assign _69 = _64 ? _68 : current_state;
    assign _19 = valid;
    assign _70 = _19 ? _69 : current_state;
    assign _62 = _21 ? _52 : current_state;
    assign _21 = last;
    assign _60 = _21 ? _52 : current_state;
    assign _59 = current_state == _58;
    assign _61 = _59 ? _60 : current_state;
    assign _57 = current_state == _30;
    assign _63 = _57 ? _62 : _61;
    assign _56 = current_state == _52;
    assign _71 = _56 ? _70 : _63;
    assign _22 = _71;
    always @(posedge _15) begin
        if (_13)
            current_state <= _32;
        else
            current_state <= _22;
    end
    assign _72 = _30 == current_state;
    assign _74 = _72 & _73;
    assign _76 = _74 | _75;
    assign _78 = _76 | _77;

    /* aliases */
    assign which_tag_0 = which_tag;

    /* output assignments */
    assign ready = _78;
    assign valid0 = _51;
    assign data0 = _49;
    assign last0 = _47;
    assign valid1 = _41;
    assign data1 = _39;
    assign last1 = _37;

endmodule
