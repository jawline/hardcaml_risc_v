module Load (
    clear,
    clock,
    hart_to_memory_controller$hart_to_memory_controller_ready,
    enable,
    memory_controller_to_hart$memory_controller_to_hart_valid,
    memory_controller_to_hart$memory_controller_to_hart_error,
    memory_controller_to_hart$memory_controller_to_hart_read_data,
    source,
    funct3,
    new_rd,
    error,
    finished,
    memory_controller_to_hart$memory_controller_to_hart_ready,
    hart_to_memory_controller$hart_to_memory_controller_valid,
    hart_to_memory_controller$hart_to_memory_controller_address,
    hart_to_memory_controller$hart_to_memory_controller_write,
    hart_to_memory_controller$hart_to_memory_controller_write_data
);

    input clear;
    input clock;
    input hart_to_memory_controller$hart_to_memory_controller_ready;
    input enable;
    input memory_controller_to_hart$memory_controller_to_hart_valid;
    input memory_controller_to_hart$memory_controller_to_hart_error;
    input [31:0] memory_controller_to_hart$memory_controller_to_hart_read_data;
    input [31:0] source;
    input [2:0] funct3;
    output [31:0] new_rd;
    output error;
    output finished;
    output memory_controller_to_hart$memory_controller_to_hart_ready;
    output hart_to_memory_controller$hart_to_memory_controller_valid;
    output [31:0] hart_to_memory_controller$hart_to_memory_controller_address;
    output hart_to_memory_controller$hart_to_memory_controller_write;
    output [31:0] hart_to_memory_controller$hart_to_memory_controller_write_data;

    /* signal declarations */
    wire [31:0] _62 = 32'b00000000000000000000000000000000;
    wire [31:0] _63;
    wire [31:0] _38 = 32'b00000000000000000000000000000000;
    wire _37;
    wire [31:0] _64;
    wire [31:0] _1;
    wire _67 = 1'b0;
    wire _68;
    wire _66 = 1'b0;
    wire _65;
    wire _69;
    wire _3;
    wire [31:0] aligned_address = 32'b11111111111111111111111111111100;
    wire [31:0] _73;
    wire [31:0] _74;
    wire [31:0] _71 = 32'b00000000000000000000000000000000;
    wire _70;
    wire [31:0] _75;
    wire [31:0] _5;
    wire _86;
    wire _85 = 1'b0;
    wire _33 = 1'b0;
    wire _8;
    wire _32 = 1'b0;
    wire _10;
    wire _12;
    wire _81;
    wire _60;
    wire _14;
    wire _61;
    wire _82;
    wire _79;
    wire _77 = 1'b1;
    wire _78;
    wire _80;
    wire _36 = 1'b0;
    wire _76;
    wire _83;
    wire _15;
    reg current_state;
    wire _84;
    wire _87;
    wire _16;
    wire _20;
    wire _88;
    wire _57 = 1'b0;
    wire _56 = 1'b0;
    wire vdd = 1'b1;
    wire _55 = 1'b0;
    wire _54 = 1'b0;
    wire _53 = 1'b0;
    reg inputs_are_error;
    wire funct3_is_error;
    wire [1:0] _50 = 2'b00;
    wire [1:0] _47 = 2'b01;
    wire [1:0] _46;
    wire [1:0] _48;
    wire [1:0] _45 = 2'b00;
    wire [1:0] _44 = 2'b00;
    wire [1:0] _43;
    wire [1:0] _41 = 2'b01;
    wire [1:0] _40;
    wire [1:0] _42;
    wire [1:0] _39 = 2'b00;
    reg [1:0] unaligned_bits;
    wire _51;
    wire is_unaligned;
    wire _59;
    wire _23;
    wire _89;
    wire [15:0] _117 = 16'b0000000000000000;
    wire [31:0] _118;
    wire [23:0] _115 = 24'b000000000000000000000000;
    wire [31:0] _116;
    wire [31:0] _114 = 32'b00000000000000000000000000000000;
    wire [15:0] _111 = 16'b1111111111111111;
    wire [15:0] _110 = 16'b0000000000000000;
    wire [15:0] _107;
    wire [15:0] _106;
    wire [31:0] _104 = 32'b00000000000000000000000000000000;
    wire _105;
    wire [15:0] half_word;
    wire _109;
    wire [15:0] _112;
    wire [31:0] _113;
    wire [23:0] _101 = 24'b111111111111111111111111;
    wire [23:0] _100 = 24'b000000000000000000000000;
    wire [7:0] _97;
    wire [15:0] _95;
    wire [7:0] _96;
    wire [23:0] _93;
    wire [7:0] _94;
    wire [31:0] full_word;
    wire [7:0] _92;
    wire [31:0] _90 = 32'b00000000000000000000000000000011;
    wire [31:0] _28;
    wire [31:0] alignment_bits;
    reg [7:0] byte;
    wire _99;
    wire [23:0] _102;
    wire [31:0] _103;
    wire [2:0] _30;
    reg [31:0] _119;

    /* logic */
    assign _63 = _61 ? _62 : _38;
    assign _37 = current_state == _36;
    assign _64 = _37 ? _63 : _38;
    assign _1 = _64;
    assign _68 = _61 ? _67 : _66;
    assign _65 = current_state == _36;
    assign _69 = _65 ? _68 : _66;
    assign _3 = _69;
    assign _73 = _28 & aligned_address;
    assign _74 = _61 ? _73 : _71;
    assign _70 = current_state == _36;
    assign _75 = _70 ? _74 : _71;
    assign _5 = _75;
    assign _86 = _61 ? vdd : _85;
    assign _8 = clear;
    assign _10 = clock;
    assign _12 = hart_to_memory_controller$hart_to_memory_controller_ready;
    assign _81 = _12 ? _77 : current_state;
    assign _60 = ~ _59;
    assign _14 = enable;
    assign _61 = _14 & _60;
    assign _82 = _61 ? _81 : current_state;
    assign _79 = _20 ? _36 : current_state;
    assign _78 = current_state == _77;
    assign _80 = _78 ? _79 : current_state;
    assign _76 = current_state == _36;
    assign _83 = _76 ? _82 : _80;
    assign _15 = _83;
    always @(posedge _10) begin
        if (_8)
            current_state <= _33;
        else
            current_state <= _15;
    end
    assign _84 = current_state == _36;
    assign _87 = _84 ? _86 : _85;
    assign _16 = _87;
    assign _20 = memory_controller_to_hart$memory_controller_to_hart_valid;
    assign _88 = is_unaligned | _20;
    always @* begin
        case (_30)
        0: inputs_are_error <= _53;
        1: inputs_are_error <= _54;
        2: inputs_are_error <= _55;
        3: inputs_are_error <= vdd;
        4: inputs_are_error <= _56;
        5: inputs_are_error <= _57;
        6: inputs_are_error <= vdd;
        default: inputs_are_error <= vdd;
        endcase
    end
    assign _46 = _28[1:0];
    assign _48 = _46 & _47;
    assign _43 = _28[1:0];
    assign _40 = _28[1:0];
    assign _42 = _40 & _41;
    always @* begin
        case (_30)
        0: unaligned_bits <= _39;
        1: unaligned_bits <= _42;
        2: unaligned_bits <= _43;
        3: unaligned_bits <= _44;
        4: unaligned_bits <= _45;
        5: unaligned_bits <= _48;
        6: unaligned_bits <= _44;
        default: unaligned_bits <= _44;
        endcase
    end
    assign _51 = unaligned_bits == _50;
    assign is_unaligned = ~ _51;
    assign _59 = is_unaligned | inputs_are_error;
    assign _23 = memory_controller_to_hart$memory_controller_to_hart_error;
    assign _89 = _23 | _59;
    assign _118 = { _117, half_word };
    assign _116 = { _115, byte };
    assign _107 = full_word[31:16];
    assign _106 = full_word[15:0];
    assign _105 = alignment_bits == _104;
    assign half_word = _105 ? _107 : _106;
    assign _109 = half_word[15:15];
    assign _112 = _109 ? _111 : _110;
    assign _113 = { _112, half_word };
    assign _97 = _95[7:0];
    assign _95 = _93[15:0];
    assign _96 = _95[15:8];
    assign _93 = full_word[23:0];
    assign _94 = _93[23:16];
    assign full_word = memory_controller_to_hart$memory_controller_to_hart_read_data;
    assign _92 = full_word[31:24];
    assign _28 = source;
    assign alignment_bits = _28 & _90;
    always @* begin
        case (alignment_bits)
        0: byte <= _92;
        1: byte <= _94;
        2: byte <= _96;
        default: byte <= _97;
        endcase
    end
    assign _99 = byte[7:7];
    assign _102 = _99 ? _101 : _100;
    assign _103 = { _102, byte };
    assign _30 = funct3;
    always @* begin
        case (_30)
        0: _119 <= _103;
        1: _119 <= _113;
        2: _119 <= full_word;
        3: _119 <= _114;
        4: _119 <= _116;
        5: _119 <= _118;
        6: _119 <= _114;
        default: _119 <= _114;
        endcase
    end

    /* aliases */
    assign funct3_is_error = inputs_are_error;

    /* output assignments */
    assign new_rd = _119;
    assign error = _89;
    assign finished = _88;
    assign memory_controller_to_hart$memory_controller_to_hart_ready = vdd;
    assign hart_to_memory_controller$hart_to_memory_controller_valid = _16;
    assign hart_to_memory_controller$hart_to_memory_controller_address = _5;
    assign hart_to_memory_controller$hart_to_memory_controller_write = _3;
    assign hart_to_memory_controller$hart_to_memory_controller_write_data = _1;

endmodule
