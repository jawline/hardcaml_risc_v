module Memory_controller (
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
    clear,
    clock,
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
    input clear;
    input clock;
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
    wire [31:0] _51 = 32'b00000000000000000000000000000000;
    wire [31:0] _78;
    wire _79 = 1'b0;
    wire _83;
    wire _84 = 1'b0;
    wire [7:0] _49 = 8'b00000010;
    wire _50;
    wire _88;
    wire [31:0] _91 = 32'b00000000000000000000000000000000;
    wire [31:0] _92;
    wire _93 = 1'b0;
    wire _94;
    wire _95 = 1'b0;
    wire [7:0] _89 = 8'b00000001;
    wire _90;
    wire _96;
    wire _75;
    wire ram$read_enable;
    wire [31:0] _74 = 32'b00000000000000000000000000000000;
    wire [31:0] _73 = 32'b00000000000000000000000000000000;
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
    wire [31:0] _72;
    reg [31:0] ram$read_data;
    wire [31:0] _99 = 32'b00000000000000000000000000000000;
    wire [31:0] _100;
    wire _81 = 1'b0;
    wire _80 = 1'b0;
    reg was_error;
    wire _101 = 1'b0;
    wire _102;
    wire _86 = 1'b0;
    wire _85 = 1'b0;
    wire [29:0] _54;
    wire [1:0] _52 = 2'b00;
    wire [31:0] ram$write_address;
    wire [31:0] ram$read_address;
    wire [31:0] real_address;
    wire [31:0] _63 = 32'b00000000000000000000010000000000;
    wire _64;
    wire [31:0] _60 = 32'b00000000000000000000000000000000;
    wire [31:0] _58 = 32'b00000000000000000000000000000010;
    wire [31:0] _22;
    wire [31:0] _24;
    wire [31:0] _26;
    reg [31:0] _53;
    wire [31:0] _59;
    wire _61;
    wire _62;
    wire _65;
    wire illegal_operation;
    wire _67;
    wire _28;
    wire _30;
    wire _32;
    reg is_operation;
    wire is_operation_and_is_legal;
    reg _87;
    wire _103 = 1'b0;
    wire [7:0] _97 = 8'b00000000;
    wire [7:0] _47 = 8'b00000000;
    wire [7:0] _46 = 8'b00000000;
    reg [7:0] last_ch_;
    wire _98;
    wire _104;
    wire _108 = 1'b1;
    wire _107 = 1'b0;
    wire [7:0] _105 = 8'b00000010;
    wire _106;
    wire _109;
    wire _113 = 1'b1;
    wire _112 = 1'b0;
    wire [7:0] _110 = 8'b00000001;
    wire _111;
    wire _114;
    wire _124 = 1'b1;
    wire _123 = 1'b0;
    wire [7:0] _121 = 8'b00000000;
    wire vdd = 1'b1;
    wire [7:0] _43 = 8'b00000000;
    wire _37;
    wire [7:0] _42 = 8'b00000000;
    wire _39;
    wire [7:0] _119 = 8'b00000000;
    wire [7:0] _117 = 8'b00000001;
    wire [7:0] _118;
    wire [7:0] _115 = 8'b00000010;
    wire _116;
    wire [7:0] _120;
    wire [7:0] _40;
    reg [7:0] which_ch;
    wire _122;
    wire _125;

    /* logic */
    assign _78 = _50 ? ram$read_data : _51;
    assign _83 = _50 ? was_error : _79;
    assign _50 = last_ch_ == _49;
    assign _88 = _50 ? _87 : _84;
    assign _92 = _90 ? ram$read_data : _91;
    assign _94 = _90 ? was_error : _93;
    assign _90 = last_ch_ == _89;
    assign _96 = _90 ? _87 : _95;
    assign _75 = ~ is_write_operation;
    assign ram$read_enable = is_operation_and_is_legal & _75;
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
    always @(posedge _39) begin
        if (ram$write_enable)
            main_memory_bram[ram$write_address] <= ram$write_data;
    end
    assign _72 = main_memory_bram[ram$write_address];
    always @(posedge _39) begin
        if (ram$read_enable)
            ram$read_data <= _72;
    end
    assign _100 = _98 ? ram$read_data : _99;
    always @(posedge _39) begin
        if (_37)
            was_error <= _81;
        else
            was_error <= illegal_operation;
    end
    assign _102 = _98 ? was_error : _101;
    assign _54 = _53[31:2];
    assign ram$write_address = { _52, _54 };
    assign _64 = _63 < ram$write_address;
    assign _22 = ch_co_controller$ch_to_controller_address2;
    assign _24 = ch_co_controller$ch_to_controller_address1;
    assign _26 = ch_co_controller$ch_to_controller_address0;
    always @* begin
        case (which_ch)
        0: _53 <= _26;
        1: _53 <= _24;
        default: _53 <= _22;
        endcase
    end
    assign _59 = _53 & _58;
    assign _61 = _59 == _60;
    assign _62 = ~ _61;
    assign _65 = _62 | _64;
    assign illegal_operation = is_operation & _65;
    assign _67 = ~ illegal_operation;
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
    assign is_operation_and_is_legal = is_operation & _67;
    always @(posedge _39) begin
        if (_37)
            _87 <= _86;
        else
            _87 <= is_operation_and_is_legal;
    end
    always @(posedge _39) begin
        if (_37)
            last_ch_ <= _47;
        else
            last_ch_ <= which_ch;
    end
    assign _98 = last_ch_ == _97;
    assign _104 = _98 ? _87 : _103;
    assign _106 = which_ch == _105;
    assign _109 = _106 ? _108 : _107;
    assign _111 = which_ch == _110;
    assign _114 = _111 ? _113 : _112;
    assign _37 = clear;
    assign _39 = clock;
    assign _118 = which_ch + _117;
    assign _116 = which_ch == _115;
    assign _120 = _116 ? _119 : _118;
    assign _40 = _120;
    always @(posedge _39) begin
        if (_37)
            which_ch <= _43;
        else
            which_ch <= _40;
    end
    assign _122 = which_ch == _121;
    assign _125 = _122 ? _124 : _123;

    /* aliases */
    assign ram$read_address = ram$write_address;
    assign real_address = ram$write_address;

    /* output assignments */
    assign ch_co_controller$ch_to_controller_ready0 = _125;
    assign ch_co_controller$ch_to_controller_ready1 = _114;
    assign ch_co_controller$ch_to_controller_ready2 = _109;
    assign controller_to_ch$controller_to_ch_valid0 = _104;
    assign controller_to_ch$controller_to_ch_error0 = _102;
    assign controller_to_ch$controller_to_ch_read_data0 = _100;
    assign controller_to_ch$controller_to_ch_valid1 = _96;
    assign controller_to_ch$controller_to_ch_error1 = _94;
    assign controller_to_ch$controller_to_ch_read_data1 = _92;
    assign controller_to_ch$controller_to_ch_valid2 = _88;
    assign controller_to_ch$controller_to_ch_error2 = _83;
    assign controller_to_ch$controller_to_ch_read_data2 = _78;

endmodule
