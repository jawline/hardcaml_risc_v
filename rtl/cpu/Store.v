module Store (
    memory_controller_to_hart$memory_controller_to_hart_read_data,
    value,
    clear,
    clock,
    hart_to_memory_controller$hart_to_memory_controller_ready,
    memory_controller_to_hart$memory_controller_to_hart_valid,
    enable,
    destination,
    funct3,
    memory_controller_to_hart$memory_controller_to_hart_error,
    error,
    finished,
    memory_controller_to_hart$memory_controller_to_hart_ready,
    hart_to_memory_controller$hart_to_memory_controller_valid,
    hart_to_memory_controller$hart_to_memory_controller_address,
    hart_to_memory_controller$hart_to_memory_controller_write,
    hart_to_memory_controller$hart_to_memory_controller_write_data
);

    input [31:0] memory_controller_to_hart$memory_controller_to_hart_read_data;
    input [31:0] value;
    input clear;
    input clock;
    input hart_to_memory_controller$hart_to_memory_controller_ready;
    input memory_controller_to_hart$memory_controller_to_hart_valid;
    input enable;
    input [31:0] destination;
    input [2:0] funct3;
    input memory_controller_to_hart$memory_controller_to_hart_error;
    output error;
    output finished;
    output memory_controller_to_hart$memory_controller_to_hart_ready;
    output hart_to_memory_controller$hart_to_memory_controller_valid;
    output [31:0] hart_to_memory_controller$hart_to_memory_controller_address;
    output hart_to_memory_controller$hart_to_memory_controller_write;
    output [31:0] hart_to_memory_controller$hart_to_memory_controller_write_data;

    /* signal declarations */
    wire [31:0] _127 = 32'b00000000000000000000000000000000;
    wire [31:0] _128;
    wire [31:0] _54 = 32'b00000000000000000000000000000000;
    wire [31:0] _53 = 32'b00000000000000000000000000000000;
    wire [31:0] _119;
    wire [31:0] _110 = 32'b00000000000000000000000000000000;
    wire [31:0] _109 = 32'b00000000000000000000000000000000;
    wire [31:0] _108 = 32'b00000000000000000000000000000000;
    wire [7:0] _106;
    wire [15:0] _104;
    wire [7:0] _105;
    wire [23:0] _102;
    wire [7:0] _103;
    wire [7:0] _101;
    wire [31:0] _107;
    reg [31:0] _111;
    wire [31:0] _99 = 32'b00000000000000000000000000000000;
    wire [31:0] _98 = 32'b00000000000000000000000000000000;
    wire [15:0] _96;
    wire [15:0] _95;
    wire [31:0] sb_2;
    wire [15:0] _92;
    wire [7:0] _93;
    wire [7:0] _91;
    wire [23:0] _89;
    wire [7:0] _90;
    wire [7:0] _88;
    wire [31:0] _94;
    reg [31:0] _100;
    wire [31:0] _86 = 32'b00000000000000000000000000000000;
    wire [31:0] _85 = 32'b00000000000000000000000000000000;
    wire [31:0] _84 = 32'b00000000000000000000000000000000;
    wire [7:0] _82;
    wire [23:0] _79;
    wire [15:0] _80;
    wire [7:0] _81;
    wire [7:0] _78;
    wire [7:0] _77;
    wire [31:0] _83;
    reg [31:0] _87;
    wire [31:0] _75 = 32'b00000000000000000000000000000000;
    wire [31:0] _74 = 32'b00000000000000000000000000000000;
    wire [15:0] _72;
    wire [15:0] _71;
    wire [31:0] sb_0;
    wire [7:0] _69;
    wire [15:0] _67;
    wire [7:0] _68;
    wire [31:0] _2;
    wire [23:0] _65;
    wire [7:0] _66;
    wire [31:0] _4;
    wire [7:0] _64;
    wire [31:0] _70;
    reg [31:0] _76;
    wire [1:0] unaligned_portion;
    reg [31:0] _112;
    wire [31:0] _113;
    wire _62;
    wire [31:0] _114;
    wire _60;
    wire [31:0] _120;
    wire [31:0] _121;
    wire [31:0] _5;
    reg [31:0] word_to_write;
    wire _125;
    wire [31:0] _126;
    wire _123;
    wire [31:0] _129;
    wire [31:0] _122 = 32'b00000000000000000000000000000000;
    wire [31:0] _130;
    wire [31:0] _6;
    wire _135 = 1'b0;
    wire _136;
    wire _133;
    wire _134;
    wire _132;
    wire _137;
    wire _131 = 1'b0;
    wire _138;
    wire _8;
    wire [31:0] _145;
    wire [31:0] aligned_address = 32'b11111111111111111111111111111100;
    wire [31:0] _143;
    wire _141;
    wire [31:0] _144;
    wire _140;
    wire [31:0] _146;
    wire [31:0] _139 = 32'b00000000000000000000000000000000;
    wire [31:0] _147;
    wire [31:0] _10;
    wire _152;
    wire _150;
    wire _151;
    wire _149;
    wire _153;
    wire _148 = 1'b0;
    wire _154;
    wire _12;
    wire _172 = 1'b1;
    wire _173;
    wire [1:0] _57 = 2'b00;
    wire _16;
    wire [1:0] _56 = 2'b00;
    wire _18;
    wire [1:0] _166;
    wire gnd = 1'b0;
    wire [2:0] _115 = 3'b010;
    wire _116;
    wire _118;
    wire [1:0] _167;
    wire [1:0] _164;
    wire _20;
    wire [1:0] _162;
    wire _22;
    wire [1:0] _160;
    wire [1:0] _158 = 2'b11;
    wire _159;
    wire [1:0] _161;
    wire [1:0] _124 = 2'b10;
    wire _157;
    wire [1:0] _163;
    wire [1:0] _61 = 2'b01;
    wire _156;
    wire [1:0] _165;
    wire [1:0] _59 = 2'b00;
    wire _155;
    wire [1:0] _168;
    wire [1:0] _169;
    wire [1:0] _23;
    reg [1:0] current_state;
    wire _171;
    wire _174;
    wire _170 = 1'b0;
    wire _51;
    wire _25;
    wire _52;
    wire _175;
    wire _26;
    wire _176;
    wire vdd = 1'b1;
    wire _47 = 1'b0;
    wire _46 = 1'b0;
    wire _45 = 1'b0;
    reg inputs_are_error;
    wire funct3_is_error;
    wire [1:0] _42 = 2'b00;
    wire [1:0] _40 = 2'b00;
    wire [1:0] _39;
    wire [1:0] _37 = 2'b01;
    wire [31:0] _29;
    wire [1:0] _36;
    wire [1:0] _38;
    wire [1:0] _35 = 2'b00;
    wire [2:0] _31;
    reg [1:0] unaligned_bits;
    wire _43;
    wire is_unaligned;
    wire _50;
    wire _33;
    wire _177;

    /* logic */
    assign _128 = _118 ? _122 : _127;
    assign _119 = _118 ? _4 : word_to_write;
    assign _106 = _4[7:0];
    assign _104 = _102[15:0];
    assign _105 = _104[15:8];
    assign _102 = _2[23:0];
    assign _103 = _102[23:16];
    assign _101 = _2[31:24];
    assign _107 = { _101, _103, _105, _106 };
    always @* begin
        case (_31)
        0: _111 <= _107;
        1: _111 <= _108;
        2: _111 <= _109;
        3: _111 <= _110;
        4: _111 <= _110;
        5: _111 <= _110;
        6: _111 <= _110;
        default: _111 <= _110;
        endcase
    end
    assign _96 = _4[15:0];
    assign _95 = _2[31:16];
    assign sb_2 = { _95, _96 };
    assign _92 = _89[15:0];
    assign _93 = _92[7:0];
    assign _91 = _4[7:0];
    assign _89 = _2[23:0];
    assign _90 = _89[23:16];
    assign _88 = _2[31:24];
    assign _94 = { _88, _90, _91, _93 };
    always @* begin
        case (_31)
        0: _100 <= _94;
        1: _100 <= sb_2;
        2: _100 <= _98;
        3: _100 <= _99;
        4: _100 <= _99;
        5: _100 <= _99;
        6: _100 <= _99;
        default: _100 <= _99;
        endcase
    end
    assign _82 = _80[7:0];
    assign _79 = _2[23:0];
    assign _80 = _79[15:0];
    assign _81 = _80[15:8];
    assign _78 = _4[7:0];
    assign _77 = _2[31:24];
    assign _83 = { _77, _78, _81, _82 };
    always @* begin
        case (_31)
        0: _87 <= _83;
        1: _87 <= _84;
        2: _87 <= _85;
        3: _87 <= _86;
        4: _87 <= _86;
        5: _87 <= _86;
        6: _87 <= _86;
        default: _87 <= _86;
        endcase
    end
    assign _72 = _2[15:0];
    assign _71 = _4[15:0];
    assign sb_0 = { _71, _72 };
    assign _69 = _67[7:0];
    assign _67 = _65[15:0];
    assign _68 = _67[15:8];
    assign _2 = memory_controller_to_hart$memory_controller_to_hart_read_data;
    assign _65 = _2[23:0];
    assign _66 = _65[23:16];
    assign _4 = value;
    assign _64 = _4[7:0];
    assign _70 = { _64, _66, _68, _69 };
    always @* begin
        case (_31)
        0: _76 <= _70;
        1: _76 <= sb_0;
        2: _76 <= _74;
        3: _76 <= _75;
        4: _76 <= _75;
        5: _76 <= _75;
        6: _76 <= _75;
        default: _76 <= _75;
        endcase
    end
    assign unaligned_portion = _29[1:0];
    always @* begin
        case (unaligned_portion)
        0: _112 <= _76;
        1: _112 <= _87;
        2: _112 <= _100;
        default: _112 <= _111;
        endcase
    end
    assign _113 = _22 ? _112 : word_to_write;
    assign _62 = current_state == _61;
    assign _114 = _62 ? _113 : word_to_write;
    assign _60 = current_state == _59;
    assign _120 = _60 ? _119 : _114;
    assign _121 = _52 ? _120 : word_to_write;
    assign _5 = _121;
    always @(posedge _18) begin
        if (_16)
            word_to_write <= _54;
        else
            word_to_write <= _5;
    end
    assign _125 = current_state == _124;
    assign _126 = _125 ? word_to_write : _122;
    assign _123 = current_state == _59;
    assign _129 = _123 ? _128 : _126;
    assign _130 = _52 ? _129 : _122;
    assign _6 = _130;
    assign _136 = _118 ? _131 : _135;
    assign _133 = current_state == _124;
    assign _134 = _133 ? vdd : _131;
    assign _132 = current_state == _59;
    assign _137 = _132 ? _136 : _134;
    assign _138 = _52 ? _137 : _131;
    assign _8 = _138;
    assign _145 = _118 ? _139 : _143;
    assign _143 = _29 & aligned_address;
    assign _141 = current_state == _124;
    assign _144 = _141 ? _143 : _139;
    assign _140 = current_state == _59;
    assign _146 = _140 ? _145 : _144;
    assign _147 = _52 ? _146 : _139;
    assign _10 = _147;
    assign _152 = _118 ? _148 : vdd;
    assign _150 = current_state == _124;
    assign _151 = _150 ? vdd : _148;
    assign _149 = current_state == _59;
    assign _153 = _149 ? _152 : _151;
    assign _154 = _52 ? _153 : _148;
    assign _12 = _154;
    assign _173 = _22 ? _172 : _170;
    assign _16 = clear;
    assign _18 = clock;
    assign _166 = _20 ? _61 : current_state;
    assign _116 = _31 == _115;
    assign _118 = _116 ? vdd : gnd;
    assign _167 = _118 ? _124 : _166;
    assign _164 = _22 ? _124 : current_state;
    assign _20 = hart_to_memory_controller$hart_to_memory_controller_ready;
    assign _162 = _20 ? _158 : current_state;
    assign _22 = memory_controller_to_hart$memory_controller_to_hart_valid;
    assign _160 = _22 ? _59 : current_state;
    assign _159 = current_state == _158;
    assign _161 = _159 ? _160 : current_state;
    assign _157 = current_state == _124;
    assign _163 = _157 ? _162 : _161;
    assign _156 = current_state == _61;
    assign _165 = _156 ? _164 : _163;
    assign _155 = current_state == _59;
    assign _168 = _155 ? _167 : _165;
    assign _169 = _52 ? _168 : current_state;
    assign _23 = _169;
    always @(posedge _18) begin
        if (_16)
            current_state <= _57;
        else
            current_state <= _23;
    end
    assign _171 = current_state == _158;
    assign _174 = _171 ? _173 : _170;
    assign _51 = ~ _50;
    assign _25 = enable;
    assign _52 = _25 & _51;
    assign _175 = _52 ? _174 : _170;
    assign _26 = _175;
    assign _176 = is_unaligned | _26;
    always @* begin
        case (_31)
        0: inputs_are_error <= _45;
        1: inputs_are_error <= _46;
        2: inputs_are_error <= _47;
        3: inputs_are_error <= vdd;
        4: inputs_are_error <= vdd;
        5: inputs_are_error <= vdd;
        6: inputs_are_error <= vdd;
        default: inputs_are_error <= vdd;
        endcase
    end
    assign _39 = _29[1:0];
    assign _29 = destination;
    assign _36 = _29[1:0];
    assign _38 = _36 & _37;
    assign _31 = funct3;
    always @* begin
        case (_31)
        0: unaligned_bits <= _35;
        1: unaligned_bits <= _38;
        2: unaligned_bits <= _39;
        3: unaligned_bits <= _40;
        4: unaligned_bits <= _40;
        5: unaligned_bits <= _40;
        6: unaligned_bits <= _40;
        default: unaligned_bits <= _40;
        endcase
    end
    assign _43 = unaligned_bits == _42;
    assign is_unaligned = ~ _43;
    assign _50 = is_unaligned | inputs_are_error;
    assign _33 = memory_controller_to_hart$memory_controller_to_hart_error;
    assign _177 = _33 | _50;

    /* aliases */
    assign funct3_is_error = inputs_are_error;

    /* output assignments */
    assign error = _177;
    assign finished = _176;
    assign memory_controller_to_hart$memory_controller_to_hart_ready = vdd;
    assign hart_to_memory_controller$hart_to_memory_controller_valid = _12;
    assign hart_to_memory_controller$hart_to_memory_controller_address = _10;
    assign hart_to_memory_controller$hart_to_memory_controller_write = _8;
    assign hart_to_memory_controller$hart_to_memory_controller_write_data = _6;

endmodule
