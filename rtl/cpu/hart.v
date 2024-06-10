module hart (
    ecall_transaction_error,
    ecall_transaction_new_pc,
    ecall_transaction_new_rd,
    ecall_transaction_set_rd,
    ecall_transaction_finished,
    clear,
    clock,
    hart_to_memory_controllerhart_to_memory_controller_ready,
    memory_controller_to_hartmemory_controller_to_hart_read_data,
    memory_controller_to_hartmemory_controller_to_hart_error,
    memory_controller_to_hartmemory_controller_to_hart_valid,
    memory_controller_to_hartmemory_controller_to_hart_ready,
    hart_to_memory_controllerhart_to_memory_controller_valid,
    hart_to_memory_controllerhart_to_memory_controller_address,
    hart_to_memory_controllerhart_to_memory_controller_write,
    hart_to_memory_controllerhart_to_memory_controller_write_data,
    registers_pc,
    registers_general0,
    registers_general1,
    registers_general2,
    registers_general3,
    registers_general4,
    registers_general5,
    registers_general6,
    registers_general7,
    registers_general8,
    registers_general9,
    registers_general10,
    registers_general11,
    registers_general12,
    registers_general13,
    registers_general14,
    registers_general15,
    registers_general16,
    registers_general17,
    registers_general18,
    registers_general19,
    registers_general20,
    registers_general21,
    registers_general22,
    registers_general23,
    registers_general24,
    registers_general25,
    registers_general26,
    registers_general27,
    registers_general28,
    registers_general29,
    registers_general30,
    registers_general31,
    error,
    is_ecall
);

    input ecall_transaction_error;
    input [31:0] ecall_transaction_new_pc;
    input [31:0] ecall_transaction_new_rd;
    input ecall_transaction_set_rd;
    input ecall_transaction_finished;
    input clear;
    input clock;
    input hart_to_memory_controllerhart_to_memory_controller_ready;
    input [31:0] memory_controller_to_hartmemory_controller_to_hart_read_data;
    input memory_controller_to_hartmemory_controller_to_hart_error;
    input memory_controller_to_hartmemory_controller_to_hart_valid;
    output memory_controller_to_hartmemory_controller_to_hart_ready;
    output hart_to_memory_controllerhart_to_memory_controller_valid;
    output [31:0] hart_to_memory_controllerhart_to_memory_controller_address;
    output hart_to_memory_controllerhart_to_memory_controller_write;
    output [31:0] hart_to_memory_controllerhart_to_memory_controller_write_data;
    output [31:0] registers_pc;
    output [31:0] registers_general0;
    output [31:0] registers_general1;
    output [31:0] registers_general2;
    output [31:0] registers_general3;
    output [31:0] registers_general4;
    output [31:0] registers_general5;
    output [31:0] registers_general6;
    output [31:0] registers_general7;
    output [31:0] registers_general8;
    output [31:0] registers_general9;
    output [31:0] registers_general10;
    output [31:0] registers_general11;
    output [31:0] registers_general12;
    output [31:0] registers_general13;
    output [31:0] registers_general14;
    output [31:0] registers_general15;
    output [31:0] registers_general16;
    output [31:0] registers_general17;
    output [31:0] registers_general18;
    output [31:0] registers_general19;
    output [31:0] registers_general20;
    output [31:0] registers_general21;
    output [31:0] registers_general22;
    output [31:0] registers_general23;
    output [31:0] registers_general24;
    output [31:0] registers_general25;
    output [31:0] registers_general26;
    output [31:0] registers_general27;
    output [31:0] registers_general28;
    output [31:0] registers_general29;
    output [31:0] registers_general30;
    output [31:0] registers_general31;
    output error;
    output is_ecall;

    /* signal declarations */
    wire _211;
    wire _218;
    wire _214 = 1'b0;
    wire _213;
    wire _219;
    wire _2;
    wire [31:0] _220 = 32'b00000000000000000000000000000000;
    wire [31:0] _226;
    wire [31:0] _224;
    wire [31:0] _223 = 32'b00000000000000000000000000000000;
    wire _222;
    wire [31:0] _225;
    wire _221;
    wire [31:0] _227;
    wire [31:0] _37;
    wire _233;
    wire _231;
    wire _230 = 1'b0;
    wire _229;
    wire _232;
    wire _228;
    wire _234;
    wire _39;
    wire [31:0] _240;
    wire [31:0] _238;
    wire [31:0] _237 = 32'b00000000000000000000000000000000;
    wire _236;
    wire [31:0] _239;
    wire _235;
    wire [31:0] _241;
    wire [31:0] _41;
    wire _247;
    wire _245;
    wire _244 = 1'b0;
    wire _243;
    wire _246;
    wire _242;
    wire _248;
    wire _43;
    wire _392;
    wire _390;
    wire _389 = 1'b0;
    wire _388;
    wire _391;
    wire _105 = 1'b0;
    wire _104 = 1'b0;
    wire [31:0] _113 = 32'b00000000000000000000000000000000;
    wire [31:0] _112 = 32'b00000000000000000000000000000000;
    wire [31:0] _378;
    wire _46;
    wire [31:0] _48;
    wire [31:0] _50;
    wire _52;
    wire _54;
    wire [31:0] _207 = 32'b00000000000000000000000000000000;
    wire [31:0] _206 = 32'b00000000000000000000000000000000;
    wire [31:0] _251;
    wire [31:0] _252;
    wire _249;
    wire [31:0] _253;
    wire [31:0] _55;
    reg [31:0] _208;
    wire [31:0] _204 = 32'b00000000000000000000000000000000;
    wire [31:0] _203 = 32'b00000000000000000000000000000000;
    wire [31:0] _255;
    wire [31:0] _256;
    wire _254;
    wire [31:0] _257;
    wire [31:0] _56;
    reg [31:0] _205;
    wire [31:0] _201 = 32'b00000000000000000000000000000000;
    wire [31:0] _200 = 32'b00000000000000000000000000000000;
    wire [31:0] _259;
    wire [31:0] _260;
    wire _258;
    wire [31:0] _261;
    wire [31:0] _57;
    reg [31:0] _202;
    wire [31:0] _198 = 32'b00000000000000000000000000000000;
    wire [31:0] _197 = 32'b00000000000000000000000000000000;
    wire [31:0] _263;
    wire [31:0] _264;
    wire _262;
    wire [31:0] _265;
    wire [31:0] _58;
    reg [31:0] _199;
    wire [31:0] _195 = 32'b00000000000000000000000000000000;
    wire [31:0] _194 = 32'b00000000000000000000000000000000;
    wire [31:0] _267;
    wire [31:0] _268;
    wire _266;
    wire [31:0] _269;
    wire [31:0] _59;
    reg [31:0] _196;
    wire [31:0] _192 = 32'b00000000000000000000000000000000;
    wire [31:0] _191 = 32'b00000000000000000000000000000000;
    wire [31:0] _271;
    wire [31:0] _272;
    wire _270;
    wire [31:0] _273;
    wire [31:0] _60;
    reg [31:0] _193;
    wire [31:0] _189 = 32'b00000000000000000000000000000000;
    wire [31:0] _188 = 32'b00000000000000000000000000000000;
    wire [31:0] _275;
    wire [31:0] _276;
    wire _274;
    wire [31:0] _277;
    wire [31:0] _61;
    reg [31:0] _190;
    wire [31:0] _186 = 32'b00000000000000000000000000000000;
    wire [31:0] _185 = 32'b00000000000000000000000000000000;
    wire [31:0] _279;
    wire [31:0] _280;
    wire _278;
    wire [31:0] _281;
    wire [31:0] _62;
    reg [31:0] _187;
    wire [31:0] _183 = 32'b00000000000000000000000000000000;
    wire [31:0] _182 = 32'b00000000000000000000000000000000;
    wire [31:0] _283;
    wire [31:0] _284;
    wire _282;
    wire [31:0] _285;
    wire [31:0] _63;
    reg [31:0] _184;
    wire [31:0] _180 = 32'b00000000000000000000000000000000;
    wire [31:0] _179 = 32'b00000000000000000000000000000000;
    wire [31:0] _287;
    wire [31:0] _288;
    wire _286;
    wire [31:0] _289;
    wire [31:0] _64;
    reg [31:0] _181;
    wire [31:0] _177 = 32'b00000000000000000000000000000000;
    wire [31:0] _176 = 32'b00000000000000000000000000000000;
    wire [31:0] _291;
    wire [31:0] _292;
    wire _290;
    wire [31:0] _293;
    wire [31:0] _65;
    reg [31:0] _178;
    wire [31:0] _174 = 32'b00000000000000000000000000000000;
    wire [31:0] _173 = 32'b00000000000000000000000000000000;
    wire [31:0] _295;
    wire [31:0] _296;
    wire _294;
    wire [31:0] _297;
    wire [31:0] _66;
    reg [31:0] _175;
    wire [31:0] _171 = 32'b00000000000000000000000000000000;
    wire [31:0] _170 = 32'b00000000000000000000000000000000;
    wire [31:0] _299;
    wire [31:0] _300;
    wire _298;
    wire [31:0] _301;
    wire [31:0] _67;
    reg [31:0] _172;
    wire [31:0] _168 = 32'b00000000000000000000000000000000;
    wire [31:0] _167 = 32'b00000000000000000000000000000000;
    wire [31:0] _303;
    wire [31:0] _304;
    wire _302;
    wire [31:0] _305;
    wire [31:0] _68;
    reg [31:0] _169;
    wire [31:0] _165 = 32'b00000000000000000000000000000000;
    wire [31:0] _164 = 32'b00000000000000000000000000000000;
    wire [31:0] _307;
    wire [31:0] _308;
    wire _306;
    wire [31:0] _309;
    wire [31:0] _69;
    reg [31:0] _166;
    wire [31:0] _162 = 32'b00000000000000000000000000000000;
    wire [31:0] _161 = 32'b00000000000000000000000000000000;
    wire [31:0] _311;
    wire [31:0] _312;
    wire _310;
    wire [31:0] _313;
    wire [31:0] _70;
    reg [31:0] _163;
    wire [31:0] _159 = 32'b00000000000000000000000000000000;
    wire [31:0] _158 = 32'b00000000000000000000000000000000;
    wire [31:0] _315;
    wire [31:0] _316;
    wire _314;
    wire [31:0] _317;
    wire [31:0] _71;
    reg [31:0] _160;
    wire [31:0] _156 = 32'b00000000000000000000000000000000;
    wire [31:0] _155 = 32'b00000000000000000000000000000000;
    wire [31:0] _319;
    wire [31:0] _320;
    wire _318;
    wire [31:0] _321;
    wire [31:0] _72;
    reg [31:0] _157;
    wire [31:0] _153 = 32'b00000000000000000000000000000000;
    wire [31:0] _152 = 32'b00000000000000000000000000000000;
    wire [31:0] _323;
    wire [31:0] _324;
    wire _322;
    wire [31:0] _325;
    wire [31:0] _73;
    reg [31:0] _154;
    wire [31:0] _150 = 32'b00000000000000000000000000000000;
    wire [31:0] _149 = 32'b00000000000000000000000000000000;
    wire [31:0] _327;
    wire [31:0] _328;
    wire _326;
    wire [31:0] _329;
    wire [31:0] _74;
    reg [31:0] _151;
    wire [31:0] _147 = 32'b00000000000000000000000000000000;
    wire [31:0] _146 = 32'b00000000000000000000000000000000;
    wire [31:0] _331;
    wire [31:0] _332;
    wire _330;
    wire [31:0] _333;
    wire [31:0] _75;
    reg [31:0] _148;
    wire [31:0] _144 = 32'b00000000000000000000000000000000;
    wire [31:0] _143 = 32'b00000000000000000000000000000000;
    wire [31:0] _335;
    wire [31:0] _336;
    wire _334;
    wire [31:0] _337;
    wire [31:0] _76;
    reg [31:0] _145;
    wire [31:0] _141 = 32'b00000000000000000000000000000000;
    wire [31:0] _140 = 32'b00000000000000000000000000000000;
    wire [31:0] _339;
    wire [31:0] _340;
    wire _338;
    wire [31:0] _341;
    wire [31:0] _77;
    reg [31:0] _142;
    wire [31:0] _138 = 32'b00000000000000000000000000000000;
    wire [31:0] _137 = 32'b00000000000000000000000000000000;
    wire [31:0] _343;
    wire [31:0] _344;
    wire _342;
    wire [31:0] _345;
    wire [31:0] _78;
    reg [31:0] _139;
    wire [31:0] _135 = 32'b00000000000000000000000000000000;
    wire [31:0] _134 = 32'b00000000000000000000000000000000;
    wire [31:0] _347;
    wire [31:0] _348;
    wire _346;
    wire [31:0] _349;
    wire [31:0] _79;
    reg [31:0] _136;
    wire [31:0] _132 = 32'b00000000000000000000000000000000;
    wire [31:0] _131 = 32'b00000000000000000000000000000000;
    wire [31:0] _351;
    wire [31:0] _352;
    wire _350;
    wire [31:0] _353;
    wire [31:0] _80;
    reg [31:0] _133;
    wire [31:0] _129 = 32'b00000000000000000000000000000000;
    wire [31:0] _128 = 32'b00000000000000000000000000000000;
    wire [31:0] _355;
    wire [31:0] _356;
    wire _354;
    wire [31:0] _357;
    wire [31:0] _81;
    reg [31:0] _130;
    wire [31:0] _126 = 32'b00000000000000000000000000000000;
    wire [31:0] _125 = 32'b00000000000000000000000000000000;
    wire [31:0] _359;
    wire [31:0] _360;
    wire _358;
    wire [31:0] _361;
    wire [31:0] _82;
    reg [31:0] _127;
    wire [31:0] _123 = 32'b00000000000000000000000000000000;
    wire [31:0] _122 = 32'b00000000000000000000000000000000;
    wire [31:0] _363;
    wire [31:0] _364;
    wire _362;
    wire [31:0] _365;
    wire [31:0] _83;
    reg [31:0] _124;
    wire [31:0] _120 = 32'b00000000000000000000000000000000;
    wire [31:0] _119 = 32'b00000000000000000000000000000000;
    wire [31:0] _367;
    wire [31:0] _368;
    wire _366;
    wire [31:0] _369;
    wire [31:0] _84;
    reg [31:0] _121;
    wire [31:0] _117 = 32'b00000000000000000000000000000000;
    wire [31:0] _116 = 32'b00000000000000000000000000000000;
    wire [31:0] _371;
    wire [31:0] _372;
    wire _370;
    wire [31:0] _373;
    wire [31:0] _85;
    reg [31:0] _118;
    wire [31:0] _115 = 32'b00000000000000000000000000000000;
    wire vdd = 1'b1;
    wire [31:0] _110 = 32'b00000000000000000000000000000000;
    wire [31:0] _109 = 32'b00000000000000000000000000000000;
    wire [31:0] _375;
    wire _374;
    wire [31:0] _376;
    wire [31:0] _86;
    reg [31:0] _111;
    wire _108;
    wire _88;
    wire _90;
    wire [1093:0] _210;
    wire _250;
    wire [31:0] _379;
    wire _377;
    wire [31:0] _380;
    wire [31:0] _91;
    reg [31:0] _114;
    wire _215;
    wire _93;
    wire [31:0] _95;
    wire _97;
    wire _99;
    wire [100:0] _217;
    wire _384;
    wire _385;
    wire _103 = 1'b1;
    wire _382;
    wire _383;
    wire _212 = 1'b0;
    wire _381;
    wire _386;
    wire _100;
    reg _107;
    wire _387;
    wire _393;
    wire _101;

    /* logic */
    assign _211 = _210[1093:1093];
    assign _218 = _217[100:100];
    assign _213 = _107 == _212;
    assign _219 = _213 ? _218 : _214;
    assign _2 = _219;
    assign _226 = _217[66:35];
    assign _224 = _210[66:35];
    assign _222 = _107 == _103;
    assign _225 = _222 ? _224 : _223;
    assign _221 = _107 == _212;
    assign _227 = _221 ? _226 : _225;
    assign _37 = _227;
    assign _233 = _217[34:34];
    assign _231 = _210[34:34];
    assign _229 = _107 == _103;
    assign _232 = _229 ? _231 : _230;
    assign _228 = _107 == _212;
    assign _234 = _228 ? _233 : _232;
    assign _39 = _234;
    assign _240 = _217[33:2];
    assign _238 = _210[33:2];
    assign _236 = _107 == _103;
    assign _239 = _236 ? _238 : _237;
    assign _235 = _107 == _212;
    assign _241 = _235 ? _240 : _239;
    assign _41 = _241;
    assign _247 = _217[1:1];
    assign _245 = _210[1:1];
    assign _243 = _107 == _103;
    assign _246 = _243 ? _245 : _244;
    assign _242 = _107 == _212;
    assign _248 = _242 ? _247 : _246;
    assign _43 = _248;
    assign _392 = _217[0:0];
    assign _390 = _210[0:0];
    assign _388 = _107 == _103;
    assign _391 = _388 ? _390 : _389;
    assign _378 = _210[99:68];
    assign _46 = ecall_transaction_error;
    assign _48 = ecall_transaction_new_pc;
    assign _50 = ecall_transaction_new_rd;
    assign _52 = ecall_transaction_set_rd;
    assign _54 = ecall_transaction_finished;
    assign _251 = _210[1091:1060];
    assign _252 = _250 ? _251 : _208;
    assign _249 = _107 == _103;
    assign _253 = _249 ? _252 : _208;
    assign _55 = _253;
    always @(posedge _90) begin
        if (_88)
            _208 <= _207;
        else
            _208 <= _55;
    end
    assign _255 = _210[1059:1028];
    assign _256 = _250 ? _255 : _205;
    assign _254 = _107 == _103;
    assign _257 = _254 ? _256 : _205;
    assign _56 = _257;
    always @(posedge _90) begin
        if (_88)
            _205 <= _204;
        else
            _205 <= _56;
    end
    assign _259 = _210[1027:996];
    assign _260 = _250 ? _259 : _202;
    assign _258 = _107 == _103;
    assign _261 = _258 ? _260 : _202;
    assign _57 = _261;
    always @(posedge _90) begin
        if (_88)
            _202 <= _201;
        else
            _202 <= _57;
    end
    assign _263 = _210[995:964];
    assign _264 = _250 ? _263 : _199;
    assign _262 = _107 == _103;
    assign _265 = _262 ? _264 : _199;
    assign _58 = _265;
    always @(posedge _90) begin
        if (_88)
            _199 <= _198;
        else
            _199 <= _58;
    end
    assign _267 = _210[963:932];
    assign _268 = _250 ? _267 : _196;
    assign _266 = _107 == _103;
    assign _269 = _266 ? _268 : _196;
    assign _59 = _269;
    always @(posedge _90) begin
        if (_88)
            _196 <= _195;
        else
            _196 <= _59;
    end
    assign _271 = _210[931:900];
    assign _272 = _250 ? _271 : _193;
    assign _270 = _107 == _103;
    assign _273 = _270 ? _272 : _193;
    assign _60 = _273;
    always @(posedge _90) begin
        if (_88)
            _193 <= _192;
        else
            _193 <= _60;
    end
    assign _275 = _210[899:868];
    assign _276 = _250 ? _275 : _190;
    assign _274 = _107 == _103;
    assign _277 = _274 ? _276 : _190;
    assign _61 = _277;
    always @(posedge _90) begin
        if (_88)
            _190 <= _189;
        else
            _190 <= _61;
    end
    assign _279 = _210[867:836];
    assign _280 = _250 ? _279 : _187;
    assign _278 = _107 == _103;
    assign _281 = _278 ? _280 : _187;
    assign _62 = _281;
    always @(posedge _90) begin
        if (_88)
            _187 <= _186;
        else
            _187 <= _62;
    end
    assign _283 = _210[835:804];
    assign _284 = _250 ? _283 : _184;
    assign _282 = _107 == _103;
    assign _285 = _282 ? _284 : _184;
    assign _63 = _285;
    always @(posedge _90) begin
        if (_88)
            _184 <= _183;
        else
            _184 <= _63;
    end
    assign _287 = _210[803:772];
    assign _288 = _250 ? _287 : _181;
    assign _286 = _107 == _103;
    assign _289 = _286 ? _288 : _181;
    assign _64 = _289;
    always @(posedge _90) begin
        if (_88)
            _181 <= _180;
        else
            _181 <= _64;
    end
    assign _291 = _210[771:740];
    assign _292 = _250 ? _291 : _178;
    assign _290 = _107 == _103;
    assign _293 = _290 ? _292 : _178;
    assign _65 = _293;
    always @(posedge _90) begin
        if (_88)
            _178 <= _177;
        else
            _178 <= _65;
    end
    assign _295 = _210[739:708];
    assign _296 = _250 ? _295 : _175;
    assign _294 = _107 == _103;
    assign _297 = _294 ? _296 : _175;
    assign _66 = _297;
    always @(posedge _90) begin
        if (_88)
            _175 <= _174;
        else
            _175 <= _66;
    end
    assign _299 = _210[707:676];
    assign _300 = _250 ? _299 : _172;
    assign _298 = _107 == _103;
    assign _301 = _298 ? _300 : _172;
    assign _67 = _301;
    always @(posedge _90) begin
        if (_88)
            _172 <= _171;
        else
            _172 <= _67;
    end
    assign _303 = _210[675:644];
    assign _304 = _250 ? _303 : _169;
    assign _302 = _107 == _103;
    assign _305 = _302 ? _304 : _169;
    assign _68 = _305;
    always @(posedge _90) begin
        if (_88)
            _169 <= _168;
        else
            _169 <= _68;
    end
    assign _307 = _210[643:612];
    assign _308 = _250 ? _307 : _166;
    assign _306 = _107 == _103;
    assign _309 = _306 ? _308 : _166;
    assign _69 = _309;
    always @(posedge _90) begin
        if (_88)
            _166 <= _165;
        else
            _166 <= _69;
    end
    assign _311 = _210[611:580];
    assign _312 = _250 ? _311 : _163;
    assign _310 = _107 == _103;
    assign _313 = _310 ? _312 : _163;
    assign _70 = _313;
    always @(posedge _90) begin
        if (_88)
            _163 <= _162;
        else
            _163 <= _70;
    end
    assign _315 = _210[579:548];
    assign _316 = _250 ? _315 : _160;
    assign _314 = _107 == _103;
    assign _317 = _314 ? _316 : _160;
    assign _71 = _317;
    always @(posedge _90) begin
        if (_88)
            _160 <= _159;
        else
            _160 <= _71;
    end
    assign _319 = _210[547:516];
    assign _320 = _250 ? _319 : _157;
    assign _318 = _107 == _103;
    assign _321 = _318 ? _320 : _157;
    assign _72 = _321;
    always @(posedge _90) begin
        if (_88)
            _157 <= _156;
        else
            _157 <= _72;
    end
    assign _323 = _210[515:484];
    assign _324 = _250 ? _323 : _154;
    assign _322 = _107 == _103;
    assign _325 = _322 ? _324 : _154;
    assign _73 = _325;
    always @(posedge _90) begin
        if (_88)
            _154 <= _153;
        else
            _154 <= _73;
    end
    assign _327 = _210[483:452];
    assign _328 = _250 ? _327 : _151;
    assign _326 = _107 == _103;
    assign _329 = _326 ? _328 : _151;
    assign _74 = _329;
    always @(posedge _90) begin
        if (_88)
            _151 <= _150;
        else
            _151 <= _74;
    end
    assign _331 = _210[451:420];
    assign _332 = _250 ? _331 : _148;
    assign _330 = _107 == _103;
    assign _333 = _330 ? _332 : _148;
    assign _75 = _333;
    always @(posedge _90) begin
        if (_88)
            _148 <= _147;
        else
            _148 <= _75;
    end
    assign _335 = _210[419:388];
    assign _336 = _250 ? _335 : _145;
    assign _334 = _107 == _103;
    assign _337 = _334 ? _336 : _145;
    assign _76 = _337;
    always @(posedge _90) begin
        if (_88)
            _145 <= _144;
        else
            _145 <= _76;
    end
    assign _339 = _210[387:356];
    assign _340 = _250 ? _339 : _142;
    assign _338 = _107 == _103;
    assign _341 = _338 ? _340 : _142;
    assign _77 = _341;
    always @(posedge _90) begin
        if (_88)
            _142 <= _141;
        else
            _142 <= _77;
    end
    assign _343 = _210[355:324];
    assign _344 = _250 ? _343 : _139;
    assign _342 = _107 == _103;
    assign _345 = _342 ? _344 : _139;
    assign _78 = _345;
    always @(posedge _90) begin
        if (_88)
            _139 <= _138;
        else
            _139 <= _78;
    end
    assign _347 = _210[323:292];
    assign _348 = _250 ? _347 : _136;
    assign _346 = _107 == _103;
    assign _349 = _346 ? _348 : _136;
    assign _79 = _349;
    always @(posedge _90) begin
        if (_88)
            _136 <= _135;
        else
            _136 <= _79;
    end
    assign _351 = _210[291:260];
    assign _352 = _250 ? _351 : _133;
    assign _350 = _107 == _103;
    assign _353 = _350 ? _352 : _133;
    assign _80 = _353;
    always @(posedge _90) begin
        if (_88)
            _133 <= _132;
        else
            _133 <= _80;
    end
    assign _355 = _210[259:228];
    assign _356 = _250 ? _355 : _130;
    assign _354 = _107 == _103;
    assign _357 = _354 ? _356 : _130;
    assign _81 = _357;
    always @(posedge _90) begin
        if (_88)
            _130 <= _129;
        else
            _130 <= _81;
    end
    assign _359 = _210[227:196];
    assign _360 = _250 ? _359 : _127;
    assign _358 = _107 == _103;
    assign _361 = _358 ? _360 : _127;
    assign _82 = _361;
    always @(posedge _90) begin
        if (_88)
            _127 <= _126;
        else
            _127 <= _82;
    end
    assign _363 = _210[195:164];
    assign _364 = _250 ? _363 : _124;
    assign _362 = _107 == _103;
    assign _365 = _362 ? _364 : _124;
    assign _83 = _365;
    always @(posedge _90) begin
        if (_88)
            _124 <= _123;
        else
            _124 <= _83;
    end
    assign _367 = _210[163:132];
    assign _368 = _250 ? _367 : _121;
    assign _366 = _107 == _103;
    assign _369 = _366 ? _368 : _121;
    assign _84 = _369;
    always @(posedge _90) begin
        if (_88)
            _121 <= _120;
        else
            _121 <= _84;
    end
    assign _371 = _210[131:100];
    assign _372 = _250 ? _371 : _118;
    assign _370 = _107 == _103;
    assign _373 = _370 ? _372 : _118;
    assign _85 = _373;
    always @(posedge _90) begin
        if (_88)
            _118 <= _117;
        else
            _118 <= _85;
    end
    assign _375 = _217[99:68];
    assign _374 = _107 == _212;
    assign _376 = _374 ? _375 : _111;
    assign _86 = _376;
    always @(posedge _90) begin
        _111 <= _86;
    end
    assign _108 = _103 == _107;
    assign _88 = clear;
    assign _90 = clock;
    decode_and_execute
        decode_and_execute
        ( .clock(_90), .clear(_88), .memory_controller_to_hart_valid(_99), .memory_controller_to_hart_error(_97), .memory_controller_to_hart_read_data(_95), .hart_to_memory_controller_ready(_93), .enable(_108), .instruction(_111), .registers_pc(_114), .registers_general0(_115), .registers_general1(_118), .registers_general2(_121), .registers_general3(_124), .registers_general4(_127), .registers_general5(_130), .registers_general6(_133), .registers_general7(_136), .registers_general8(_139), .registers_general9(_142), .registers_general10(_145), .registers_general11(_148), .registers_general12(_151), .registers_general13(_154), .registers_general14(_157), .registers_general15(_160), .registers_general16(_163), .registers_general17(_166), .registers_general18(_169), .registers_general19(_172), .registers_general20(_175), .registers_general21(_178), .registers_general22(_181), .registers_general23(_184), .registers_general24(_187), .registers_general25(_190), .registers_general26(_193), .registers_general27(_196), .registers_general28(_199), .registers_general29(_202), .registers_general30(_205), .registers_general31(_208), .ecall_transaction_finished(_54), .ecall_transaction_set_rd(_52), .ecall_transaction_new_rd(_50), .ecall_transaction_new_pc(_48), .ecall_transaction_error(_46), .is_ecall(_210[1093:1093]), .error(_210[1092:1092]), .new_registers_general30(_210[1091:1060]), .new_registers_general29(_210[1059:1028]), .new_registers_general28(_210[1027:996]), .new_registers_general27(_210[995:964]), .new_registers_general26(_210[963:932]), .new_registers_general25(_210[931:900]), .new_registers_general24(_210[899:868]), .new_registers_general23(_210[867:836]), .new_registers_general22(_210[835:804]), .new_registers_general21(_210[803:772]), .new_registers_general20(_210[771:740]), .new_registers_general19(_210[739:708]), .new_registers_general18(_210[707:676]), .new_registers_general17(_210[675:644]), .new_registers_general16(_210[643:612]), .new_registers_general15(_210[611:580]), .new_registers_general14(_210[579:548]), .new_registers_general13(_210[547:516]), .new_registers_general12(_210[515:484]), .new_registers_general11(_210[483:452]), .new_registers_general10(_210[451:420]), .new_registers_general9(_210[419:388]), .new_registers_general8(_210[387:356]), .new_registers_general7(_210[355:324]), .new_registers_general6(_210[323:292]), .new_registers_general5(_210[291:260]), .new_registers_general4(_210[259:228]), .new_registers_general3(_210[227:196]), .new_registers_general2(_210[195:164]), .new_registers_general1(_210[163:132]), .new_registers_general0(_210[131:100]), .new_registers_pc(_210[99:68]), .finished(_210[67:67]), .hart_to_memory_controller_write_data(_210[66:35]), .hart_to_memory_controller_write(_210[34:34]), .hart_to_memory_controller_address(_210[33:2]), .hart_to_memory_controller_valid(_210[1:1]), .memory_controller_to_hart_ready(_210[0:0]) );
    assign _250 = _210[67:67];
    assign _379 = _250 ? _378 : _114;
    assign _377 = _107 == _103;
    assign _380 = _377 ? _379 : _114;
    assign _91 = _380;
    always @(posedge _90) begin
        if (_88)
            _114 <= _113;
        else
            _114 <= _91;
    end
    assign _215 = _212 == _107;
    assign _93 = hart_to_memory_controllerhart_to_memory_controller_ready;
    assign _95 = memory_controller_to_hartmemory_controller_to_hart_read_data;
    assign _97 = memory_controller_to_hartmemory_controller_to_hart_error;
    assign _99 = memory_controller_to_hartmemory_controller_to_hart_valid;
    fetch
        fetcher
        ( .memory_controller_to_hartvalid(_99), .memory_controller_to_harterror(_97), .memory_controller_to_hartread_data(_95), .hart_to_memory_controllerready(_93), .should_fetch(_215), .address(_114), .error(_217[100:100]), .instruction(_217[99:68]), .has_fetched(_217[67:67]), .hart_to_memory_controllerwrite_data(_217[66:35]), .hart_to_memory_controllerwrite(_217[34:34]), .hart_to_memory_controlleraddress(_217[33:2]), .hart_to_memory_controllervalid(_217[1:1]), .memory_controller_to_hartready(_217[0:0]) );
    assign _384 = _217[67:67];
    assign _385 = _384 ? _103 : _107;
    assign _382 = _107 == _103;
    assign _383 = _382 ? _212 : _107;
    assign _381 = _107 == _212;
    assign _386 = _381 ? _385 : _383;
    assign _100 = _386;
    always @(posedge _90) begin
        if (_88)
            _107 <= _105;
        else
            _107 <= _100;
    end
    assign _387 = _107 == _212;
    assign _393 = _387 ? _392 : _391;
    assign _101 = _393;

    /* aliases */

    /* output assignments */
    assign memory_controller_to_hartmemory_controller_to_hart_ready = _101;
    assign hart_to_memory_controllerhart_to_memory_controller_valid = _43;
    assign hart_to_memory_controllerhart_to_memory_controller_address = _41;
    assign hart_to_memory_controllerhart_to_memory_controller_write = _39;
    assign hart_to_memory_controllerhart_to_memory_controller_write_data = _37;
    assign registers_pc = _114;
    assign registers_general0 = _220;
    assign registers_general1 = _118;
    assign registers_general2 = _121;
    assign registers_general3 = _124;
    assign registers_general4 = _127;
    assign registers_general5 = _130;
    assign registers_general6 = _133;
    assign registers_general7 = _136;
    assign registers_general8 = _139;
    assign registers_general9 = _142;
    assign registers_general10 = _145;
    assign registers_general11 = _148;
    assign registers_general12 = _151;
    assign registers_general13 = _154;
    assign registers_general14 = _157;
    assign registers_general15 = _160;
    assign registers_general16 = _163;
    assign registers_general17 = _166;
    assign registers_general18 = _169;
    assign registers_general19 = _172;
    assign registers_general20 = _175;
    assign registers_general21 = _178;
    assign registers_general22 = _181;
    assign registers_general23 = _184;
    assign registers_general24 = _187;
    assign registers_general25 = _190;
    assign registers_general26 = _193;
    assign registers_general27 = _196;
    assign registers_general28 = _199;
    assign registers_general29 = _202;
    assign registers_general30 = _205;
    assign registers_general31 = _208;
    assign error = _2;
    assign is_ecall = _211;

endmodule
