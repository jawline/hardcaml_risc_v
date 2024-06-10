module memory_controller (
    ch_co_controller$ch_to_controller_write2,
    ch_co_controller$ch_to_controller_write1,
    ch_co_controller$ch_to_controller_write0,
    ch_co_controller$ch_to_controller_write_data2,
    ch_co_controller$ch_to_controller_write_data1,
    ch_co_controller$ch_to_controller_write_data0,
    ch_co_controller$ch_to_controller_address2,
    ch_co_controller$ch_to_controller_address1,
    ch_co_controller$ch_to_controller_address0,
    ch_co_controller$ch_to_controller_valid2,
    ch_co_controller$ch_to_controller_valid1,
    ch_co_controller$ch_to_controller_valid0,
    clock,
    clear,
    controller_to_ch$controller_to_ch_ready0,
    controller_to_ch$controller_to_ch_ready1,
    controller_to_ch$controller_to_ch_ready2,
    ch_co_controller$ch_to_controller_ready0,
    ch_co_controller$ch_to_controller_ready1,
    ch_co_controller$ch_to_controller_ready2,
    controller_to_ch$controller_to_ch_valid0,
    controller_to_ch$controller_to_ch_error0,
    controller_to_ch$controller_to_ch_read_data0,
    controller_to_ch$controller_to_ch_valid1,
    controller_to_ch$controller_to_ch_error1,
    controller_to_ch$controller_to_ch_read_data1,
    controller_to_ch$controller_to_ch_valid2,
    controller_to_ch$controller_to_ch_error2,
    controller_to_ch$controller_to_ch_read_data2
);

    input ch_co_controller$ch_to_controller_write2;
    input ch_co_controller$ch_to_controller_write1;
    input ch_co_controller$ch_to_controller_write0;
    input [31:0] ch_co_controller$ch_to_controller_write_data2;
    input [31:0] ch_co_controller$ch_to_controller_write_data1;
    input [31:0] ch_co_controller$ch_to_controller_write_data0;
    input [31:0] ch_co_controller$ch_to_controller_address2;
    input [31:0] ch_co_controller$ch_to_controller_address1;
    input [31:0] ch_co_controller$ch_to_controller_address0;
    input ch_co_controller$ch_to_controller_valid2;
    input ch_co_controller$ch_to_controller_valid1;
    input ch_co_controller$ch_to_controller_valid0;
    input clock;
    input clear;
    input controller_to_ch$controller_to_ch_ready0;
    input controller_to_ch$controller_to_ch_ready1;
    input controller_to_ch$controller_to_ch_ready2;
    output ch_co_controller$ch_to_controller_ready0;
    output ch_co_controller$ch_to_controller_ready1;
    output ch_co_controller$ch_to_controller_ready2;
    output controller_to_ch$controller_to_ch_valid0;
    output controller_to_ch$controller_to_ch_error0;
    output [31:0] controller_to_ch$controller_to_ch_read_data0;
    output controller_to_ch$controller_to_ch_valid1;
    output controller_to_ch$controller_to_ch_error1;
    output [31:0] controller_to_ch$controller_to_ch_read_data1;
    output controller_to_ch$controller_to_ch_valid2;
    output controller_to_ch$controller_to_ch_error2;
    output [31:0] controller_to_ch$controller_to_ch_read_data2;

    /* signal declarations */
    wire [1:0] _76 = 2'b10;
    wire _77;
    wire _81;
    wire [1:0] _82 = 2'b01;
    wire _83;
    wire _84;
    wire _67;
    wire ram$read_enable;
    wire [31:0] _66 = 32'b00000000000000000000000000000000;
    wire [31:0] _65 = 32'b00000000000000000000000000000000;
    wire _8;
    wire _10;
    wire _12;
    reg is_write_operation;
    wire ram$write_enable;
    wire [31:0] _14;
    wire [31:0] _16;
    wire [31:0] _18;
    reg [31:0] ram$write_data;
    reg [31:0] main_memory_bram[0:1023];
    wire [31:0] _64;
    reg [31:0] ram$read_data;
    wire _71 = 1'b0;
    wire _70 = 1'b0;
    reg was_error;
    wire _79 = 1'b0;
    wire _78 = 1'b0;
    wire [29:0] _46;
    wire [1:0] _40 = 2'b00;
    wire [31:0] ram$write_address;
    wire [31:0] ram$read_address;
    wire [31:0] real_address;
    wire [31:0] _55 = 32'b00000000000000000000010000000000;
    wire _56;
    wire [31:0] _52 = 32'b00000000000000000000000000000000;
    wire [31:0] _50 = 32'b00000000000000000000000000000010;
    wire [31:0] _22;
    wire [31:0] _24;
    wire [31:0] _26;
    reg [31:0] _45;
    wire [31:0] _51;
    wire _53;
    wire _54;
    wire _57;
    wire illegal_operation;
    wire _59;
    wire _28;
    wire _30;
    wire _32;
    reg is_operation;
    wire is_operation_and_is_legal;
    reg _80;
    wire [1:0] _85 = 2'b00;
    wire [1:0] _74 = 2'b00;
    wire [1:0] _73 = 2'b00;
    reg [1:0] last_ch_;
    wire _86;
    wire _87;
    wire _91 = 1'b1;
    wire _90 = 1'b0;
    wire [1:0] _88 = 2'b10;
    wire _89;
    wire _92;
    wire _96 = 1'b1;
    wire _95 = 1'b0;
    wire [1:0] _93 = 2'b01;
    wire _94;
    wire _97;
    wire _107 = 1'b1;
    wire _106 = 1'b0;
    wire [1:0] _104 = 2'b00;
    wire vdd = 1'b1;
    wire [1:0] _42 = 2'b00;
    wire [1:0] _41 = 2'b00;
    wire _37;
    wire [1:0] _102 = 2'b00;
    wire [1:0] _100 = 2'b01;
    wire [1:0] _101;
    wire [1:0] _98 = 2'b10;
    wire _99;
    wire [1:0] _103;
    wire [1:0] _38;
    reg [1:0] which_ch;
    wire _105;
    wire _108;

    /* logic */
    assign _77 = last_ch_ == _76;
    assign _81 = _77 & _80;
    assign _83 = last_ch_ == _82;
    assign _84 = _83 & _80;
    assign _67 = ~ is_write_operation;
    assign ram$read_enable = is_operation_and_is_legal & _67;
    assign _8 = ch_co_controller$ch_to_controller_write2;
    assign _10 = ch_co_controller$ch_to_controller_write1;
    assign _12 = ch_co_controller$ch_to_controller_write0;
    always @* begin
        case (which_ch)
        0: is_write_operation <= _12;
        1: is_write_operation <= _10;
        default: is_write_operation <= _8;
        endcase
    end
    assign ram$write_enable = is_operation_and_is_legal & is_write_operation;
    assign _14 = ch_co_controller$ch_to_controller_write_data2;
    assign _16 = ch_co_controller$ch_to_controller_write_data1;
    assign _18 = ch_co_controller$ch_to_controller_write_data0;
    always @* begin
        case (which_ch)
        0: ram$write_data <= _18;
        1: ram$write_data <= _16;
        default: ram$write_data <= _14;
        endcase
    end
    always @(posedge _37) begin
        if (ram$write_enable)
            main_memory_bram[ram$write_address] <= ram$write_data;
    end
    assign _64 = main_memory_bram[ram$write_address];
    always @(posedge _37) begin
        if (ram$read_enable)
            ram$read_data <= _64;
    end
    always @(posedge _37) begin
        was_error <= illegal_operation;
    end
    assign _46 = _45[31:2];
    assign ram$write_address = { _40, _46 };
    assign _56 = _55 < ram$write_address;
    assign _22 = ch_co_controller$ch_to_controller_address2;
    assign _24 = ch_co_controller$ch_to_controller_address1;
    assign _26 = ch_co_controller$ch_to_controller_address0;
    always @* begin
        case (which_ch)
        0: _45 <= _26;
        1: _45 <= _24;
        default: _45 <= _22;
        endcase
    end
    assign _51 = _45 & _50;
    assign _53 = _51 == _52;
    assign _54 = ~ _53;
    assign _57 = _54 | _56;
    assign illegal_operation = is_operation & _57;
    assign _59 = ~ illegal_operation;
    assign _28 = ch_co_controller$ch_to_controller_valid2;
    assign _30 = ch_co_controller$ch_to_controller_valid1;
    assign _32 = ch_co_controller$ch_to_controller_valid0;
    always @* begin
        case (which_ch)
        0: is_operation <= _32;
        1: is_operation <= _30;
        default: is_operation <= _28;
        endcase
    end
    assign is_operation_and_is_legal = is_operation & _59;
    always @(posedge _37) begin
        _80 <= is_operation_and_is_legal;
    end
    always @(posedge _37) begin
        last_ch_ <= which_ch;
    end
    assign _86 = last_ch_ == _85;
    assign _87 = _86 & _80;
    assign _89 = which_ch == _88;
    assign _92 = _89 ? _91 : _90;
    assign _94 = which_ch == _93;
    assign _97 = _94 ? _96 : _95;
    assign _37 = clock;
    assign _101 = which_ch + _100;
    assign _99 = which_ch == _98;
    assign _103 = _99 ? _102 : _101;
    assign _38 = _103;
    always @(posedge _37) begin
        which_ch <= _38;
    end
    assign _105 = which_ch == _104;
    assign _108 = _105 ? _107 : _106;

    /* aliases */
    assign ram$read_address = ram$write_address;
    assign real_address = ram$write_address;

    /* output assignments */
    assign ch_co_controller$ch_to_controller_ready0 = _108;
    assign ch_co_controller$ch_to_controller_ready1 = _97;
    assign ch_co_controller$ch_to_controller_ready2 = _92;
    assign controller_to_ch$controller_to_ch_valid0 = _87;
    assign controller_to_ch$controller_to_ch_error0 = was_error;
    assign controller_to_ch$controller_to_ch_read_data0 = ram$read_data;
    assign controller_to_ch$controller_to_ch_valid1 = _84;
    assign controller_to_ch$controller_to_ch_error1 = was_error;
    assign controller_to_ch$controller_to_ch_read_data1 = ram$read_data;
    assign controller_to_ch$controller_to_ch_valid2 = _81;
    assign controller_to_ch$controller_to_ch_error2 = was_error;
    assign controller_to_ch$controller_to_ch_read_data2 = ram$read_data;

endmodule
