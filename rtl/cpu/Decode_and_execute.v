module Decode_and_execute (
    ecall_transaction_error,
    ecall_transaction_new_rd,
    ecall_transaction_set_rd,
    ecall_transaction_new_pc,
    registers_pc,
    hart_to_memory_controller_ready,
    memory_controller_to_hart_read_data,
    memory_controller_to_hart_error,
    memory_controller_to_hart_valid,
    registers_general31,
    registers_general30,
    registers_general29,
    registers_general28,
    registers_general27,
    registers_general26,
    registers_general25,
    registers_general24,
    registers_general23,
    registers_general22,
    registers_general21,
    registers_general20,
    registers_general19,
    registers_general18,
    registers_general17,
    registers_general16,
    registers_general15,
    registers_general14,
    registers_general13,
    registers_general12,
    registers_general11,
    registers_general10,
    registers_general9,
    registers_general8,
    registers_general7,
    registers_general6,
    registers_general5,
    registers_general4,
    registers_general3,
    registers_general2,
    registers_general1,
    registers_general0,
    ecall_transaction_finished,
    clear,
    clock,
    instruction,
    enable,
    memory_controller_to_hart_ready,
    hart_to_memory_controller_valid,
    hart_to_memory_controller_address,
    hart_to_memory_controller_write,
    hart_to_memory_controller_write_data,
    finished,
    new_registers_pc,
    new_registers_general0,
    new_registers_general1,
    new_registers_general2,
    new_registers_general3,
    new_registers_general4,
    new_registers_general5,
    new_registers_general6,
    new_registers_general7,
    new_registers_general8,
    new_registers_general9,
    new_registers_general10,
    new_registers_general11,
    new_registers_general12,
    new_registers_general13,
    new_registers_general14,
    new_registers_general15,
    new_registers_general16,
    new_registers_general17,
    new_registers_general18,
    new_registers_general19,
    new_registers_general20,
    new_registers_general21,
    new_registers_general22,
    new_registers_general23,
    new_registers_general24,
    new_registers_general25,
    new_registers_general26,
    new_registers_general27,
    new_registers_general28,
    new_registers_general29,
    new_registers_general30,
    error,
    is_ecall
);

    input ecall_transaction_error;
    input [31:0] ecall_transaction_new_rd;
    input ecall_transaction_set_rd;
    input [31:0] ecall_transaction_new_pc;
    input [31:0] registers_pc;
    input hart_to_memory_controller_ready;
    input [31:0] memory_controller_to_hart_read_data;
    input memory_controller_to_hart_error;
    input memory_controller_to_hart_valid;
    input [31:0] registers_general31;
    input [31:0] registers_general30;
    input [31:0] registers_general29;
    input [31:0] registers_general28;
    input [31:0] registers_general27;
    input [31:0] registers_general26;
    input [31:0] registers_general25;
    input [31:0] registers_general24;
    input [31:0] registers_general23;
    input [31:0] registers_general22;
    input [31:0] registers_general21;
    input [31:0] registers_general20;
    input [31:0] registers_general19;
    input [31:0] registers_general18;
    input [31:0] registers_general17;
    input [31:0] registers_general16;
    input [31:0] registers_general15;
    input [31:0] registers_general14;
    input [31:0] registers_general13;
    input [31:0] registers_general12;
    input [31:0] registers_general11;
    input [31:0] registers_general10;
    input [31:0] registers_general9;
    input [31:0] registers_general8;
    input [31:0] registers_general7;
    input [31:0] registers_general6;
    input [31:0] registers_general5;
    input [31:0] registers_general4;
    input [31:0] registers_general3;
    input [31:0] registers_general2;
    input [31:0] registers_general1;
    input [31:0] registers_general0;
    input ecall_transaction_finished;
    input clear;
    input clock;
    input [31:0] instruction;
    input enable;
    output memory_controller_to_hart_ready;
    output hart_to_memory_controller_valid;
    output [31:0] hart_to_memory_controller_address;
    output hart_to_memory_controller_write;
    output [31:0] hart_to_memory_controller_write_data;
    output finished;
    output [31:0] new_registers_pc;
    output [31:0] new_registers_general0;
    output [31:0] new_registers_general1;
    output [31:0] new_registers_general2;
    output [31:0] new_registers_general3;
    output [31:0] new_registers_general4;
    output [31:0] new_registers_general5;
    output [31:0] new_registers_general6;
    output [31:0] new_registers_general7;
    output [31:0] new_registers_general8;
    output [31:0] new_registers_general9;
    output [31:0] new_registers_general10;
    output [31:0] new_registers_general11;
    output [31:0] new_registers_general12;
    output [31:0] new_registers_general13;
    output [31:0] new_registers_general14;
    output [31:0] new_registers_general15;
    output [31:0] new_registers_general16;
    output [31:0] new_registers_general17;
    output [31:0] new_registers_general18;
    output [31:0] new_registers_general19;
    output [31:0] new_registers_general20;
    output [31:0] new_registers_general21;
    output [31:0] new_registers_general22;
    output [31:0] new_registers_general23;
    output [31:0] new_registers_general24;
    output [31:0] new_registers_general25;
    output [31:0] new_registers_general26;
    output [31:0] new_registers_general27;
    output [31:0] new_registers_general28;
    output [31:0] new_registers_general29;
    output [31:0] new_registers_general30;
    output error;
    output is_ecall;

    /* signal declarations */
    wire _205 = 1'b0;
    wire _204 = 1'b0;
    wire _3;
    wire _306;
    wire _304;
    wire _302;
    wire _300;
    wire _293 = 1'b0;
    wire _291 = 1'b0;
    wire _289 = 1'b0;
    wire [31:0] _285 = 32'b00000000000000000000000000000000;
    wire [31:0] _283 = 32'b00000000000000000000000000000011;
    wire [31:0] _284;
    wire _286;
    wire _287;
    wire [31:0] _276 = 32'b00000000000000000000000000000000;
    wire [31:0] _274 = 32'b00000000000000000000000000000011;
    wire [31:0] _275;
    wire _277;
    wire _278;
    wire _268;
    wire _261;
    wire _262;
    wire _269;
    wire _279;
    wire _288;
    wire _290;
    wire _292;
    wire _294;
    wire _301;
    wire _303;
    wire _305;
    wire _307;
    wire _207;
    wire _308;
    wire _309;
    wire _4;
    reg _206;
    wire [4:0] _319 = 5'b11111;
    wire _320;
    wire _321;
    wire [31:0] _325;
    wire _312;
    wire [31:0] _326;
    wire [31:0] _310 = 32'b00000000000000000000000000000000;
    wire [31:0] _327;
    wire [31:0] _6;
    wire [4:0] _330 = 5'b11110;
    wire _331;
    wire _332;
    wire [31:0] _333;
    wire _329;
    wire [31:0] _334;
    wire [31:0] _328 = 32'b00000000000000000000000000000000;
    wire [31:0] _335;
    wire [31:0] _8;
    wire [4:0] _338 = 5'b11101;
    wire _339;
    wire _340;
    wire [31:0] _341;
    wire _337;
    wire [31:0] _342;
    wire [31:0] _336 = 32'b00000000000000000000000000000000;
    wire [31:0] _343;
    wire [31:0] _10;
    wire [4:0] _346 = 5'b11100;
    wire _347;
    wire _348;
    wire [31:0] _349;
    wire _345;
    wire [31:0] _350;
    wire [31:0] _344 = 32'b00000000000000000000000000000000;
    wire [31:0] _351;
    wire [31:0] _12;
    wire [4:0] _354 = 5'b11011;
    wire _355;
    wire _356;
    wire [31:0] _357;
    wire _353;
    wire [31:0] _358;
    wire [31:0] _352 = 32'b00000000000000000000000000000000;
    wire [31:0] _359;
    wire [31:0] _14;
    wire [4:0] _362 = 5'b11010;
    wire _363;
    wire _364;
    wire [31:0] _365;
    wire _361;
    wire [31:0] _366;
    wire [31:0] _360 = 32'b00000000000000000000000000000000;
    wire [31:0] _367;
    wire [31:0] _16;
    wire [4:0] _370 = 5'b11001;
    wire _371;
    wire _372;
    wire [31:0] _373;
    wire _369;
    wire [31:0] _374;
    wire [31:0] _368 = 32'b00000000000000000000000000000000;
    wire [31:0] _375;
    wire [31:0] _18;
    wire [4:0] _378 = 5'b11000;
    wire _379;
    wire _380;
    wire [31:0] _381;
    wire _377;
    wire [31:0] _382;
    wire [31:0] _376 = 32'b00000000000000000000000000000000;
    wire [31:0] _383;
    wire [31:0] _20;
    wire [4:0] _386 = 5'b10111;
    wire _387;
    wire _388;
    wire [31:0] _389;
    wire _385;
    wire [31:0] _390;
    wire [31:0] _384 = 32'b00000000000000000000000000000000;
    wire [31:0] _391;
    wire [31:0] _22;
    wire [4:0] _394 = 5'b10110;
    wire _395;
    wire _396;
    wire [31:0] _397;
    wire _393;
    wire [31:0] _398;
    wire [31:0] _392 = 32'b00000000000000000000000000000000;
    wire [31:0] _399;
    wire [31:0] _24;
    wire [4:0] _402 = 5'b10101;
    wire _403;
    wire _404;
    wire [31:0] _405;
    wire _401;
    wire [31:0] _406;
    wire [31:0] _400 = 32'b00000000000000000000000000000000;
    wire [31:0] _407;
    wire [31:0] _26;
    wire [4:0] _410 = 5'b10100;
    wire _411;
    wire _412;
    wire [31:0] _413;
    wire _409;
    wire [31:0] _414;
    wire [31:0] _408 = 32'b00000000000000000000000000000000;
    wire [31:0] _415;
    wire [31:0] _28;
    wire [4:0] _418 = 5'b10011;
    wire _419;
    wire _420;
    wire [31:0] _421;
    wire _417;
    wire [31:0] _422;
    wire [31:0] _416 = 32'b00000000000000000000000000000000;
    wire [31:0] _423;
    wire [31:0] _30;
    wire [4:0] _426 = 5'b10010;
    wire _427;
    wire _428;
    wire [31:0] _429;
    wire _425;
    wire [31:0] _430;
    wire [31:0] _424 = 32'b00000000000000000000000000000000;
    wire [31:0] _431;
    wire [31:0] _32;
    wire [4:0] _434 = 5'b10001;
    wire _435;
    wire _436;
    wire [31:0] _437;
    wire _433;
    wire [31:0] _438;
    wire [31:0] _432 = 32'b00000000000000000000000000000000;
    wire [31:0] _439;
    wire [31:0] _34;
    wire [4:0] _442 = 5'b10000;
    wire _443;
    wire _444;
    wire [31:0] _445;
    wire _441;
    wire [31:0] _446;
    wire [31:0] _440 = 32'b00000000000000000000000000000000;
    wire [31:0] _447;
    wire [31:0] _36;
    wire [4:0] _450 = 5'b01111;
    wire _451;
    wire _452;
    wire [31:0] _453;
    wire _449;
    wire [31:0] _454;
    wire [31:0] _448 = 32'b00000000000000000000000000000000;
    wire [31:0] _455;
    wire [31:0] _38;
    wire [4:0] _458 = 5'b01110;
    wire _459;
    wire _460;
    wire [31:0] _461;
    wire _457;
    wire [31:0] _462;
    wire [31:0] _456 = 32'b00000000000000000000000000000000;
    wire [31:0] _463;
    wire [31:0] _40;
    wire [4:0] _466 = 5'b01101;
    wire _467;
    wire _468;
    wire [31:0] _469;
    wire _465;
    wire [31:0] _470;
    wire [31:0] _464 = 32'b00000000000000000000000000000000;
    wire [31:0] _471;
    wire [31:0] _42;
    wire [4:0] _474 = 5'b01100;
    wire _475;
    wire _476;
    wire [31:0] _477;
    wire _473;
    wire [31:0] _478;
    wire [31:0] _472 = 32'b00000000000000000000000000000000;
    wire [31:0] _479;
    wire [31:0] _44;
    wire [4:0] _482 = 5'b01011;
    wire _483;
    wire _484;
    wire [31:0] _485;
    wire _481;
    wire [31:0] _486;
    wire [31:0] _480 = 32'b00000000000000000000000000000000;
    wire [31:0] _487;
    wire [31:0] _46;
    wire [4:0] _490 = 5'b01010;
    wire _491;
    wire _492;
    wire [31:0] _493;
    wire _489;
    wire [31:0] _494;
    wire [31:0] _488 = 32'b00000000000000000000000000000000;
    wire [31:0] _495;
    wire [31:0] _48;
    wire [4:0] _498 = 5'b01001;
    wire _499;
    wire _500;
    wire [31:0] _501;
    wire _497;
    wire [31:0] _502;
    wire [31:0] _496 = 32'b00000000000000000000000000000000;
    wire [31:0] _503;
    wire [31:0] _50;
    wire [4:0] _506 = 5'b01000;
    wire _507;
    wire _508;
    wire [31:0] _509;
    wire _505;
    wire [31:0] _510;
    wire [31:0] _504 = 32'b00000000000000000000000000000000;
    wire [31:0] _511;
    wire [31:0] _52;
    wire [4:0] _514 = 5'b00111;
    wire _515;
    wire _516;
    wire [31:0] _517;
    wire _513;
    wire [31:0] _518;
    wire [31:0] _512 = 32'b00000000000000000000000000000000;
    wire [31:0] _519;
    wire [31:0] _54;
    wire [4:0] _522 = 5'b00110;
    wire _523;
    wire _524;
    wire [31:0] _525;
    wire _521;
    wire [31:0] _526;
    wire [31:0] _520 = 32'b00000000000000000000000000000000;
    wire [31:0] _527;
    wire [31:0] _56;
    wire [4:0] _530 = 5'b00101;
    wire _531;
    wire _532;
    wire [31:0] _533;
    wire _529;
    wire [31:0] _534;
    wire [31:0] _528 = 32'b00000000000000000000000000000000;
    wire [31:0] _535;
    wire [31:0] _58;
    wire [4:0] _538 = 5'b00100;
    wire _539;
    wire _540;
    wire [31:0] _541;
    wire _537;
    wire [31:0] _542;
    wire [31:0] _536 = 32'b00000000000000000000000000000000;
    wire [31:0] _543;
    wire [31:0] _60;
    wire [4:0] _546 = 5'b00011;
    wire _547;
    wire _548;
    wire [31:0] _549;
    wire _545;
    wire [31:0] _550;
    wire [31:0] _544 = 32'b00000000000000000000000000000000;
    wire [31:0] _551;
    wire [31:0] _62;
    wire [4:0] _554 = 5'b00010;
    wire _555;
    wire _556;
    wire [31:0] _557;
    wire _553;
    wire [31:0] _558;
    wire [31:0] _552 = 32'b00000000000000000000000000000000;
    wire [31:0] _559;
    wire [31:0] _64;
    wire [31:0] _323 = 32'b00000000000000000000000000000000;
    wire [31:0] _322 = 32'b00000000000000000000000000000000;
    wire [31:0] _67;
    wire [31:0] _599 = 32'b00000000000000000000000000000000;
    wire [31:0] _600;
    wire [31:0] _597 = 32'b00000000000000000000000000000000;
    wire [31:0] _595;
    wire [31:0] _593 = 32'b00000000000000000000000000000000;
    wire [31:0] _591 = 32'b00000000000000000000000000000000;
    wire [31:0] _588 = 32'b00000000000000000000000000000100;
    wire [31:0] _589;
    wire [31:0] _585 = 32'b00000000000000000000000000000100;
    wire [31:0] _586;
    wire [31:0] _582 = 32'b00000000000000000000000000000100;
    wire [31:0] _583;
    wire [31:0] _579 = 32'b00000000000000000000000000000100;
    wire [31:0] _580;
    wire [31:0] _264 = 32'b00000000000000000000000000000000;
    wire [31:0] _263 = 32'b00000000000000000000000000000000;
    wire [19:0] _565 = 20'b11111111111111111111;
    wire [19:0] _564 = 20'b00000000000000000000;
    wire [11:0] _562;
    wire _563;
    wire [19:0] _566;
    wire [31:0] decoded_i_immediate;
    wire _561;
    wire [31:0] _568;
    wire [31:0] _569;
    wire [31:0] _68;
    reg [31:0] _265;
    wire [32:0] _267;
    wire [31:0] _577;
    wire [6:0] _254 = 7'b0000000;
    wire [6:0] _253 = 7'b0000000;
    wire [6:0] _571;
    wire _570;
    wire [6:0] _572;
    wire [6:0] _573;
    wire [6:0] _69;
    reg [6:0] _255;
    wire [32:0] _260;
    wire [31:0] _575;
    wire [31:0] _576;
    wire [31:0] _578;
    wire [31:0] _581;
    wire [31:0] _584;
    wire [31:0] _587;
    wire [31:0] _590;
    wire [31:0] _592;
    wire [31:0] _594;
    wire [31:0] _596;
    wire [31:0] _598;
    wire [31:0] _601;
    wire _574;
    wire [31:0] _602;
    wire [31:0] _603;
    wire [31:0] _70;
    reg [31:0] _324;
    wire [4:0] _626 = 5'b00001;
    wire [4:0] _317 = 5'b00000;
    wire [4:0] _316 = 5'b00000;
    wire [4:0] _605;
    wire _604;
    wire [4:0] _606;
    wire [4:0] _607;
    wire [4:0] _71;
    reg [4:0] _318;
    wire _627;
    wire _314 = 1'b0;
    wire _313 = 1'b0;
    wire _73;
    wire _620;
    wire gnd = 1'b0;
    wire _609;
    wire _610;
    wire _611;
    wire _612;
    wire _613;
    wire _614;
    wire _615;
    wire _617;
    wire _618;
    wire _619;
    wire _621;
    wire _608;
    wire _622;
    wire _623;
    wire _74;
    reg _315;
    wire _628;
    wire [31:0] _629;
    wire _625;
    wire [31:0] _630;
    wire [31:0] _624 = 32'b00000000000000000000000000000000;
    wire [31:0] _631;
    wire [31:0] _75;
    wire [31:0] _674 = 32'b00000000000000000000000000000000;
    wire [31:0] _673 = 32'b00000000000000000000000000000000;
    wire [31:0] _78;
    wire [31:0] _699 = 32'b00000000000000000000000000000100;
    wire [31:0] _700;
    wire [31:0] _701;
    wire [31:0] _696 = 32'b00000000000000000000000000000100;
    wire [31:0] _697;
    wire [31:0] _693 = 32'b00000000000000000000000000000100;
    wire [31:0] _694;
    wire [31:0] _296 = 32'b00000000000000000000000000000000;
    wire [31:0] _295 = 32'b00000000000000000000000000000000;
    wire [18:0] _641 = 19'b1111111111111111111;
    wire [18:0] _640 = 19'b0000000000000000000;
    wire _637 = 1'b0;
    wire [3:0] _636;
    wire [5:0] _635;
    wire _634;
    wire _633;
    wire [12:0] _638;
    wire _639;
    wire [18:0] _642;
    wire [31:0] _643;
    wire _632;
    wire [31:0] _644;
    wire [31:0] _645;
    wire [31:0] _79;
    reg [31:0] _297;
    wire [31:0] _257 = 32'b00000000000000000000000000000000;
    wire [31:0] _256 = 32'b00000000000000000000000000000000;
    wire [4:0] _647;
    reg [31:0] _648;
    wire _646;
    wire [31:0] _649;
    wire [31:0] _650;
    wire [31:0] _80;
    reg [31:0] _258;
    wire [32:0] _299;
    wire [31:0] _691;
    wire [31:0] _688 = 32'b00000000000000000000000000000100;
    wire [31:0] _689;
    wire [31:0] _686;
    wire [31:0] _652 = 32'b00000000000000000000000000000000;
    wire [31:0] _651 = 32'b00000000000000000000000000000000;
    wire [11:0] _656 = 12'b000000000000;
    wire [19:0] _655;
    wire [31:0] _657;
    wire _654;
    wire [31:0] _658;
    wire [31:0] _659;
    wire [31:0] _81;
    reg [31:0] _653;
    wire [31:0] _281 = 32'b11111111111111111111111111111110;
    wire [31:0] _280;
    wire [31:0] _282;
    wire [31:0] _271 = 32'b00000000000000000000000000000000;
    wire [31:0] _270 = 32'b00000000000000000000000000000000;
    wire [11:0] _668 = 12'b111111111111;
    wire [11:0] _667 = 12'b000000000000;
    wire _664 = 1'b0;
    wire [9:0] _663;
    wire [7:0] _662;
    wire _661;
    wire [19:0] _665;
    wire _666;
    wire [11:0] _669;
    wire [31:0] _670;
    wire _660;
    wire [31:0] _671;
    wire [31:0] _672;
    wire [31:0] _82;
    reg [31:0] _272;
    wire [31:0] _273;
    wire [31:0] _680 = 32'b00000000000000000000000000000100;
    wire [31:0] _681;
    wire [31:0] _677 = 32'b00000000000000000000000000000100;
    wire [31:0] _84;
    wire [31:0] _678;
    wire [31:0] _679;
    wire [31:0] _682;
    wire [31:0] _683;
    wire [31:0] _684;
    wire [31:0] _685;
    wire [31:0] _687;
    wire [31:0] _690;
    wire [31:0] _692;
    wire [31:0] _695;
    wire [31:0] _698;
    wire [31:0] _702;
    wire _676;
    wire [31:0] _703;
    wire [31:0] _704;
    wire [31:0] _85;
    reg [31:0] _675;
    wire _706;
    wire [31:0] _707;
    wire [31:0] _705 = 32'b00000000000000000000000000000000;
    wire [31:0] _708;
    wire [31:0] _86;
    wire _709;
    wire [31:0] _732 = 32'b00000000000000000000000000000000;
    wire [31:0] _730;
    wire [31:0] _728;
    wire [31:0] _726 = 32'b00000000000000000000000000000000;
    wire [31:0] _724 = 32'b00000000000000000000000000000000;
    wire [31:0] _722 = 32'b00000000000000000000000000000000;
    wire [31:0] _720 = 32'b00000000000000000000000000000000;
    wire [31:0] _718 = 32'b00000000000000000000000000000000;
    wire [31:0] _716 = 32'b00000000000000000000000000000000;
    wire [31:0] _714 = 32'b00000000000000000000000000000000;
    wire [31:0] _712 = 32'b00000000000000000000000000000000;
    wire [31:0] _713;
    wire [31:0] _715;
    wire [31:0] _717;
    wire [31:0] _719;
    wire [31:0] _721;
    wire [31:0] _723;
    wire [31:0] _725;
    wire [31:0] _727;
    wire [31:0] _729;
    wire [31:0] _731;
    wire [31:0] _733;
    wire _711;
    wire [31:0] _734;
    wire [31:0] _710 = 32'b00000000000000000000000000000000;
    wire [31:0] _735;
    wire [31:0] _89;
    wire _758 = 1'b0;
    wire _756;
    wire _754;
    wire _752 = 1'b0;
    wire _750 = 1'b0;
    wire _748 = 1'b0;
    wire _746 = 1'b0;
    wire _744 = 1'b0;
    wire _742 = 1'b0;
    wire _740 = 1'b0;
    wire _738 = 1'b0;
    wire _739;
    wire _741;
    wire _743;
    wire _745;
    wire _747;
    wire _749;
    wire _751;
    wire _753;
    wire _755;
    wire _757;
    wire _759;
    wire _737;
    wire _760;
    wire _736 = 1'b0;
    wire _761;
    wire _91;
    wire [31:0] _784 = 32'b00000000000000000000000000000000;
    wire [31:0] _782;
    wire [31:0] _780;
    wire [31:0] _778 = 32'b00000000000000000000000000000000;
    wire [31:0] _776 = 32'b00000000000000000000000000000000;
    wire [31:0] _774 = 32'b00000000000000000000000000000000;
    wire [31:0] _772 = 32'b00000000000000000000000000000000;
    wire [31:0] _770 = 32'b00000000000000000000000000000000;
    wire [31:0] _768 = 32'b00000000000000000000000000000000;
    wire [31:0] _766 = 32'b00000000000000000000000000000000;
    wire [31:0] _764 = 32'b00000000000000000000000000000000;
    wire [31:0] _765;
    wire [31:0] _767;
    wire [31:0] _769;
    wire [31:0] _771;
    wire [31:0] _773;
    wire [31:0] _775;
    wire [31:0] _777;
    wire [31:0] _779;
    wire [31:0] _781;
    wire [31:0] _783;
    wire [31:0] _785;
    wire _763;
    wire [31:0] _786;
    wire [31:0] _762 = 32'b00000000000000000000000000000000;
    wire [31:0] _787;
    wire [31:0] _93;
    wire _810 = 1'b0;
    wire _808;
    wire _806;
    wire _804 = 1'b0;
    wire _802 = 1'b0;
    wire _800 = 1'b0;
    wire _798 = 1'b0;
    wire _796 = 1'b0;
    wire _794 = 1'b0;
    wire _792 = 1'b0;
    wire _790 = 1'b0;
    wire _791;
    wire _793;
    wire _795;
    wire _797;
    wire _799;
    wire _801;
    wire _803;
    wire _805;
    wire _807;
    wire _809;
    wire _811;
    wire _789;
    wire _812;
    wire _788 = 1'b0;
    wire _813;
    wire _95;
    wire _872 = 1'b0;
    wire _870;
    wire _868;
    wire _866 = 1'b0;
    wire _864 = 1'b0;
    wire _862 = 1'b0;
    wire _860 = 1'b0;
    wire _858 = 1'b0;
    wire _856 = 1'b0;
    wire _854 = 1'b0;
    wire _852 = 1'b0;
    wire _853;
    wire _855;
    wire _857;
    wire _859;
    wire _861;
    wire _863;
    wire _865;
    wire _867;
    wire _869;
    wire _871;
    wire _873;
    wire [1:0] _188 = 2'b00;
    wire [1:0] _187 = 2'b00;
    wire [6:0] _251 = 7'b0110011;
    wire _252;
    wire [1:0] _836;
    wire [6:0] _249 = 7'b0010011;
    wire _250;
    wire [1:0] _837;
    wire [6:0] _247 = 7'b1101111;
    wire _248;
    wire [1:0] _838;
    wire [6:0] _245 = 7'b1100111;
    wire _246;
    wire [1:0] _839;
    wire [6:0] _243 = 7'b0110111;
    wire _244;
    wire [1:0] _840;
    wire [6:0] _241 = 7'b0010111;
    wire _242;
    wire [1:0] _841;
    wire [6:0] _239 = 7'b0001111;
    wire _240;
    wire [1:0] _842;
    wire [6:0] _237 = 7'b1100011;
    wire _238;
    wire [1:0] _843;
    wire [6:0] _230 = 7'b0000011;
    wire _231;
    wire _232;
    wire [100:0] _234;
    wire _235;
    wire [6:0] _228 = 7'b0000011;
    wire _229;
    wire _236;
    wire [1:0] _844;
    wire _98;
    wire [31:0] _100;
    wire _102;
    wire _104;
    wire [31:0] _222 = 32'b00000000000000000000000000000000;
    wire [31:0] _221 = 32'b00000000000000000000000000000000;
    wire [4:0] _815;
    reg [31:0] _816;
    wire _814;
    wire [31:0] _817;
    wire [31:0] _818;
    wire [31:0] _105;
    reg [31:0] _223;
    wire [31:0] _219 = 32'b00000000000000000000000000000000;
    wire [31:0] _218 = 32'b00000000000000000000000000000000;
    wire [31:0] _107;
    wire [31:0] _109;
    wire [31:0] _111;
    wire [31:0] _113;
    wire [31:0] _115;
    wire [31:0] _117;
    wire [31:0] _119;
    wire [31:0] _121;
    wire [31:0] _123;
    wire [31:0] _125;
    wire [31:0] _127;
    wire [31:0] _129;
    wire [31:0] _131;
    wire [31:0] _133;
    wire [31:0] _135;
    wire [31:0] _137;
    wire [31:0] _139;
    wire [31:0] _141;
    wire [31:0] _143;
    wire [31:0] _145;
    wire [31:0] _147;
    wire [31:0] _149;
    wire [31:0] _151;
    wire [31:0] _153;
    wire [31:0] _155;
    wire [31:0] _157;
    wire [31:0] _159;
    wire [31:0] _161;
    wire [31:0] _163;
    wire [31:0] _165;
    wire [31:0] _167;
    wire [31:0] _169;
    wire [4:0] _820;
    reg [31:0] _821;
    wire _819;
    wire [31:0] _822;
    wire [31:0] _823;
    wire [31:0] _170;
    reg [31:0] _220;
    wire [6:0] _215 = 7'b0100011;
    wire _216;
    wire _214;
    wire _217;
    wire [68:0] _225;
    wire _226;
    wire [6:0] _212 = 7'b0100011;
    wire _213;
    wire _227;
    wire [1:0] _845;
    wire _172;
    wire [2:0] _201 = 3'b000;
    wire [2:0] _199 = 3'b000;
    wire [2:0] _198 = 3'b000;
    wire [2:0] _825;
    wire _824;
    wire [2:0] _826;
    wire [2:0] _827;
    wire [2:0] _173;
    reg [2:0] _200;
    wire _202;
    wire [6:0] _195 = 7'b1110011;
    wire _196;
    wire _191;
    wire _197;
    wire is_ecall_0;
    wire _210;
    wire [6:0] _208 = 7'b1110011;
    wire vdd = 1'b1;
    wire [6:0] _193 = 7'b0000000;
    wire _175;
    wire [6:0] _192 = 7'b0000000;
    wire _177;
    wire [31:0] _179;
    wire [6:0] _829;
    wire _828;
    wire [6:0] _830;
    wire [6:0] _831;
    wire [6:0] _180;
    reg [6:0] _194;
    wire _209;
    wire _211;
    wire [1:0] _846;
    wire [1:0] _311 = 2'b10;
    wire _834;
    wire [1:0] _835;
    wire [1:0] _186 = 2'b01;
    wire _833;
    wire [1:0] _847;
    wire [1:0] _560 = 2'b00;
    wire _832;
    wire [1:0] _848;
    wire [1:0] _849;
    wire [1:0] _181;
    reg [1:0] _190;
    wire _851;
    wire _874;
    wire _850 = 1'b0;
    wire _183;
    wire _875;
    wire _184;

    /* logic */
    assign _3 = ecall_transaction_error;
    assign _306 = is_ecall_0 ? _3 : vdd;
    assign _304 = _225[0:0];
    assign _302 = _234[32:32];
    assign _300 = _299[32:32];
    assign _284 = _282 & _283;
    assign _286 = _284 == _285;
    assign _287 = ~ _286;
    assign _275 = _273 & _274;
    assign _277 = _275 == _276;
    assign _278 = ~ _277;
    assign _268 = _267[32:32];
    assign _261 = _260[32:32];
    assign _262 = _252 ? _261 : _206;
    assign _269 = _250 ? _268 : _262;
    assign _279 = _248 ? _278 : _269;
    assign _288 = _246 ? _287 : _279;
    assign _290 = _244 ? _289 : _288;
    assign _292 = _242 ? _291 : _290;
    assign _294 = _240 ? _293 : _292;
    assign _301 = _238 ? _300 : _294;
    assign _303 = _236 ? _302 : _301;
    assign _305 = _227 ? _304 : _303;
    assign _307 = _211 ? _306 : _305;
    assign _207 = _190 == _186;
    assign _308 = _207 ? _307 : _206;
    assign _309 = _183 ? _308 : _206;
    assign _4 = _309;
    always @(posedge _177) begin
        if (_175)
            _206 <= _205;
        else
            _206 <= _4;
    end
    assign _320 = _318 == _319;
    assign _321 = _315 & _320;
    assign _325 = _321 ? _324 : _107;
    assign _312 = _190 == _311;
    assign _326 = _312 ? _325 : _310;
    assign _327 = _183 ? _326 : _310;
    assign _6 = _327;
    assign _331 = _318 == _330;
    assign _332 = _315 & _331;
    assign _333 = _332 ? _324 : _109;
    assign _329 = _190 == _311;
    assign _334 = _329 ? _333 : _328;
    assign _335 = _183 ? _334 : _328;
    assign _8 = _335;
    assign _339 = _318 == _338;
    assign _340 = _315 & _339;
    assign _341 = _340 ? _324 : _111;
    assign _337 = _190 == _311;
    assign _342 = _337 ? _341 : _336;
    assign _343 = _183 ? _342 : _336;
    assign _10 = _343;
    assign _347 = _318 == _346;
    assign _348 = _315 & _347;
    assign _349 = _348 ? _324 : _113;
    assign _345 = _190 == _311;
    assign _350 = _345 ? _349 : _344;
    assign _351 = _183 ? _350 : _344;
    assign _12 = _351;
    assign _355 = _318 == _354;
    assign _356 = _315 & _355;
    assign _357 = _356 ? _324 : _115;
    assign _353 = _190 == _311;
    assign _358 = _353 ? _357 : _352;
    assign _359 = _183 ? _358 : _352;
    assign _14 = _359;
    assign _363 = _318 == _362;
    assign _364 = _315 & _363;
    assign _365 = _364 ? _324 : _117;
    assign _361 = _190 == _311;
    assign _366 = _361 ? _365 : _360;
    assign _367 = _183 ? _366 : _360;
    assign _16 = _367;
    assign _371 = _318 == _370;
    assign _372 = _315 & _371;
    assign _373 = _372 ? _324 : _119;
    assign _369 = _190 == _311;
    assign _374 = _369 ? _373 : _368;
    assign _375 = _183 ? _374 : _368;
    assign _18 = _375;
    assign _379 = _318 == _378;
    assign _380 = _315 & _379;
    assign _381 = _380 ? _324 : _121;
    assign _377 = _190 == _311;
    assign _382 = _377 ? _381 : _376;
    assign _383 = _183 ? _382 : _376;
    assign _20 = _383;
    assign _387 = _318 == _386;
    assign _388 = _315 & _387;
    assign _389 = _388 ? _324 : _123;
    assign _385 = _190 == _311;
    assign _390 = _385 ? _389 : _384;
    assign _391 = _183 ? _390 : _384;
    assign _22 = _391;
    assign _395 = _318 == _394;
    assign _396 = _315 & _395;
    assign _397 = _396 ? _324 : _125;
    assign _393 = _190 == _311;
    assign _398 = _393 ? _397 : _392;
    assign _399 = _183 ? _398 : _392;
    assign _24 = _399;
    assign _403 = _318 == _402;
    assign _404 = _315 & _403;
    assign _405 = _404 ? _324 : _127;
    assign _401 = _190 == _311;
    assign _406 = _401 ? _405 : _400;
    assign _407 = _183 ? _406 : _400;
    assign _26 = _407;
    assign _411 = _318 == _410;
    assign _412 = _315 & _411;
    assign _413 = _412 ? _324 : _129;
    assign _409 = _190 == _311;
    assign _414 = _409 ? _413 : _408;
    assign _415 = _183 ? _414 : _408;
    assign _28 = _415;
    assign _419 = _318 == _418;
    assign _420 = _315 & _419;
    assign _421 = _420 ? _324 : _131;
    assign _417 = _190 == _311;
    assign _422 = _417 ? _421 : _416;
    assign _423 = _183 ? _422 : _416;
    assign _30 = _423;
    assign _427 = _318 == _426;
    assign _428 = _315 & _427;
    assign _429 = _428 ? _324 : _133;
    assign _425 = _190 == _311;
    assign _430 = _425 ? _429 : _424;
    assign _431 = _183 ? _430 : _424;
    assign _32 = _431;
    assign _435 = _318 == _434;
    assign _436 = _315 & _435;
    assign _437 = _436 ? _324 : _135;
    assign _433 = _190 == _311;
    assign _438 = _433 ? _437 : _432;
    assign _439 = _183 ? _438 : _432;
    assign _34 = _439;
    assign _443 = _318 == _442;
    assign _444 = _315 & _443;
    assign _445 = _444 ? _324 : _137;
    assign _441 = _190 == _311;
    assign _446 = _441 ? _445 : _440;
    assign _447 = _183 ? _446 : _440;
    assign _36 = _447;
    assign _451 = _318 == _450;
    assign _452 = _315 & _451;
    assign _453 = _452 ? _324 : _139;
    assign _449 = _190 == _311;
    assign _454 = _449 ? _453 : _448;
    assign _455 = _183 ? _454 : _448;
    assign _38 = _455;
    assign _459 = _318 == _458;
    assign _460 = _315 & _459;
    assign _461 = _460 ? _324 : _141;
    assign _457 = _190 == _311;
    assign _462 = _457 ? _461 : _456;
    assign _463 = _183 ? _462 : _456;
    assign _40 = _463;
    assign _467 = _318 == _466;
    assign _468 = _315 & _467;
    assign _469 = _468 ? _324 : _143;
    assign _465 = _190 == _311;
    assign _470 = _465 ? _469 : _464;
    assign _471 = _183 ? _470 : _464;
    assign _42 = _471;
    assign _475 = _318 == _474;
    assign _476 = _315 & _475;
    assign _477 = _476 ? _324 : _145;
    assign _473 = _190 == _311;
    assign _478 = _473 ? _477 : _472;
    assign _479 = _183 ? _478 : _472;
    assign _44 = _479;
    assign _483 = _318 == _482;
    assign _484 = _315 & _483;
    assign _485 = _484 ? _324 : _147;
    assign _481 = _190 == _311;
    assign _486 = _481 ? _485 : _480;
    assign _487 = _183 ? _486 : _480;
    assign _46 = _487;
    assign _491 = _318 == _490;
    assign _492 = _315 & _491;
    assign _493 = _492 ? _324 : _149;
    assign _489 = _190 == _311;
    assign _494 = _489 ? _493 : _488;
    assign _495 = _183 ? _494 : _488;
    assign _48 = _495;
    assign _499 = _318 == _498;
    assign _500 = _315 & _499;
    assign _501 = _500 ? _324 : _151;
    assign _497 = _190 == _311;
    assign _502 = _497 ? _501 : _496;
    assign _503 = _183 ? _502 : _496;
    assign _50 = _503;
    assign _507 = _318 == _506;
    assign _508 = _315 & _507;
    assign _509 = _508 ? _324 : _153;
    assign _505 = _190 == _311;
    assign _510 = _505 ? _509 : _504;
    assign _511 = _183 ? _510 : _504;
    assign _52 = _511;
    assign _515 = _318 == _514;
    assign _516 = _315 & _515;
    assign _517 = _516 ? _324 : _155;
    assign _513 = _190 == _311;
    assign _518 = _513 ? _517 : _512;
    assign _519 = _183 ? _518 : _512;
    assign _54 = _519;
    assign _523 = _318 == _522;
    assign _524 = _315 & _523;
    assign _525 = _524 ? _324 : _157;
    assign _521 = _190 == _311;
    assign _526 = _521 ? _525 : _520;
    assign _527 = _183 ? _526 : _520;
    assign _56 = _527;
    assign _531 = _318 == _530;
    assign _532 = _315 & _531;
    assign _533 = _532 ? _324 : _159;
    assign _529 = _190 == _311;
    assign _534 = _529 ? _533 : _528;
    assign _535 = _183 ? _534 : _528;
    assign _58 = _535;
    assign _539 = _318 == _538;
    assign _540 = _315 & _539;
    assign _541 = _540 ? _324 : _161;
    assign _537 = _190 == _311;
    assign _542 = _537 ? _541 : _536;
    assign _543 = _183 ? _542 : _536;
    assign _60 = _543;
    assign _547 = _318 == _546;
    assign _548 = _315 & _547;
    assign _549 = _548 ? _324 : _163;
    assign _545 = _190 == _311;
    assign _550 = _545 ? _549 : _544;
    assign _551 = _183 ? _550 : _544;
    assign _62 = _551;
    assign _555 = _318 == _554;
    assign _556 = _315 & _555;
    assign _557 = _556 ? _324 : _165;
    assign _553 = _190 == _311;
    assign _558 = _553 ? _557 : _552;
    assign _559 = _183 ? _558 : _552;
    assign _64 = _559;
    assign _67 = ecall_transaction_new_rd;
    assign _600 = is_ecall_0 ? _67 : _599;
    assign _595 = _234[31:0];
    assign _589 = _84 + _588;
    assign _586 = _84 + _585;
    assign _583 = _84 + _582;
    assign _580 = _84 + _579;
    assign _562 = _179[31:20];
    assign _563 = _562[11:11];
    assign _566 = _563 ? _565 : _564;
    assign decoded_i_immediate = { _566, _562 };
    assign _561 = _190 == _560;
    assign _568 = _561 ? decoded_i_immediate : _265;
    assign _569 = _183 ? _568 : _265;
    assign _68 = _569;
    always @(posedge _177) begin
        if (_175)
            _265 <= _264;
        else
            _265 <= _68;
    end
    Op
        op_imm
        ( .funct3(_200), .funct7(_255), .lhs(_223), .rhs(_265), .error(_267[32:32]), .new_rd(_267[31:0]) );
    assign _577 = _267[31:0];
    assign _571 = _179[31:25];
    assign _570 = _190 == _560;
    assign _572 = _570 ? _571 : _255;
    assign _573 = _183 ? _572 : _255;
    assign _69 = _573;
    always @(posedge _177) begin
        if (_175)
            _255 <= _254;
        else
            _255 <= _69;
    end
    Op_0
        op
        ( .funct3(_200), .funct7(_255), .lhs(_223), .rhs(_258), .error(_260[32:32]), .new_rd(_260[31:0]) );
    assign _575 = _260[31:0];
    assign _576 = _252 ? _575 : _324;
    assign _578 = _250 ? _577 : _576;
    assign _581 = _248 ? _580 : _578;
    assign _584 = _246 ? _583 : _581;
    assign _587 = _244 ? _586 : _584;
    assign _590 = _242 ? _589 : _587;
    assign _592 = _240 ? _591 : _590;
    assign _594 = _238 ? _593 : _592;
    assign _596 = _236 ? _595 : _594;
    assign _598 = _227 ? _597 : _596;
    assign _601 = _211 ? _600 : _598;
    assign _574 = _190 == _186;
    assign _602 = _574 ? _601 : _324;
    assign _603 = _183 ? _602 : _324;
    assign _70 = _603;
    always @(posedge _177) begin
        if (_175)
            _324 <= _323;
        else
            _324 <= _70;
    end
    assign _605 = _179[11:7];
    assign _604 = _190 == _560;
    assign _606 = _604 ? _605 : _318;
    assign _607 = _183 ? _606 : _318;
    assign _71 = _607;
    always @(posedge _177) begin
        if (_175)
            _318 <= _317;
        else
            _318 <= _71;
    end
    assign _627 = _318 == _626;
    assign _73 = ecall_transaction_set_rd;
    assign _620 = is_ecall_0 ? _73 : gnd;
    assign _609 = _252 ? vdd : _315;
    assign _610 = _250 ? vdd : _609;
    assign _611 = _248 ? vdd : _610;
    assign _612 = _246 ? vdd : _611;
    assign _613 = _244 ? vdd : _612;
    assign _614 = _242 ? vdd : _613;
    assign _615 = _240 ? vdd : _614;
    assign _617 = _238 ? gnd : _615;
    assign _618 = _236 ? vdd : _617;
    assign _619 = _227 ? gnd : _618;
    assign _621 = _211 ? _620 : _619;
    assign _608 = _190 == _186;
    assign _622 = _608 ? _621 : _315;
    assign _623 = _183 ? _622 : _315;
    assign _74 = _623;
    always @(posedge _177) begin
        if (_175)
            _315 <= _314;
        else
            _315 <= _74;
    end
    assign _628 = _315 & _627;
    assign _629 = _628 ? _324 : _167;
    assign _625 = _190 == _311;
    assign _630 = _625 ? _629 : _624;
    assign _631 = _183 ? _630 : _624;
    assign _75 = _631;
    assign _78 = ecall_transaction_new_pc;
    assign _700 = _84 + _699;
    assign _701 = is_ecall_0 ? _78 : _700;
    assign _697 = _84 + _696;
    assign _694 = _84 + _693;
    assign _636 = _179[11:8];
    assign _635 = _179[30:25];
    assign _634 = _179[7:7];
    assign _633 = _179[31:31];
    assign _638 = { _633, _634, _635, _636, _637 };
    assign _639 = _638[12:12];
    assign _642 = _639 ? _641 : _640;
    assign _643 = { _642, _638 };
    assign _632 = _190 == _560;
    assign _644 = _632 ? _643 : _297;
    assign _645 = _183 ? _644 : _297;
    assign _79 = _645;
    always @(posedge _177) begin
        if (_175)
            _297 <= _296;
        else
            _297 <= _79;
    end
    assign _647 = _179[24:20];
    always @* begin
        case (_647)
        0: _648 <= _169;
        1: _648 <= _167;
        2: _648 <= _165;
        3: _648 <= _163;
        4: _648 <= _161;
        5: _648 <= _159;
        6: _648 <= _157;
        7: _648 <= _155;
        8: _648 <= _153;
        9: _648 <= _151;
        10: _648 <= _149;
        11: _648 <= _147;
        12: _648 <= _145;
        13: _648 <= _143;
        14: _648 <= _141;
        15: _648 <= _139;
        16: _648 <= _137;
        17: _648 <= _135;
        18: _648 <= _133;
        19: _648 <= _131;
        20: _648 <= _129;
        21: _648 <= _127;
        22: _648 <= _125;
        23: _648 <= _123;
        24: _648 <= _121;
        25: _648 <= _119;
        26: _648 <= _117;
        27: _648 <= _115;
        28: _648 <= _113;
        29: _648 <= _111;
        30: _648 <= _109;
        default: _648 <= _107;
        endcase
    end
    assign _646 = _190 == _560;
    assign _649 = _646 ? _648 : _258;
    assign _650 = _183 ? _649 : _258;
    assign _80 = _650;
    always @(posedge _177) begin
        if (_175)
            _258 <= _257;
        else
            _258 <= _80;
    end
    Branch
        branch
        ( .pc(_84), .lhs(_223), .rhs(_258), .funct3(_200), .b_immediate(_297), .error(_299[32:32]), .new_rd(_299[31:0]) );
    assign _691 = _299[31:0];
    assign _689 = _84 + _688;
    assign _686 = _84 + _653;
    assign _655 = _179[31:12];
    assign _657 = { _655, _656 };
    assign _654 = _190 == _560;
    assign _658 = _654 ? _657 : _653;
    assign _659 = _183 ? _658 : _653;
    assign _81 = _659;
    always @(posedge _177) begin
        if (_175)
            _653 <= _652;
        else
            _653 <= _81;
    end
    assign _280 = _223 + _272;
    assign _282 = _280 & _281;
    assign _663 = _179[30:21];
    assign _662 = _179[19:12];
    assign _661 = _179[31:31];
    assign _665 = { _661, _662, _663, _664 };
    assign _666 = _665[19:19];
    assign _669 = _666 ? _668 : _667;
    assign _670 = { _669, _665 };
    assign _660 = _190 == _560;
    assign _671 = _660 ? _670 : _272;
    assign _672 = _183 ? _671 : _272;
    assign _82 = _672;
    always @(posedge _177) begin
        if (_175)
            _272 <= _271;
        else
            _272 <= _82;
    end
    assign _273 = _84 + _272;
    assign _681 = _84 + _680;
    assign _84 = registers_pc;
    assign _678 = _84 + _677;
    assign _679 = _252 ? _678 : _675;
    assign _682 = _250 ? _681 : _679;
    assign _683 = _248 ? _273 : _682;
    assign _684 = _246 ? _282 : _683;
    assign _685 = _244 ? _653 : _684;
    assign _687 = _242 ? _686 : _685;
    assign _690 = _240 ? _689 : _687;
    assign _692 = _238 ? _691 : _690;
    assign _695 = _236 ? _694 : _692;
    assign _698 = _227 ? _697 : _695;
    assign _702 = _211 ? _701 : _698;
    assign _676 = _190 == _186;
    assign _703 = _676 ? _702 : _675;
    assign _704 = _183 ? _703 : _675;
    assign _85 = _704;
    always @(posedge _177) begin
        if (_175)
            _675 <= _674;
        else
            _675 <= _85;
    end
    assign _706 = _190 == _311;
    assign _707 = _706 ? _675 : _705;
    assign _708 = _183 ? _707 : _705;
    assign _86 = _708;
    assign _709 = _311 == _190;
    assign _730 = _225[68:37];
    assign _728 = _234[100:69];
    assign _713 = _252 ? _712 : _710;
    assign _715 = _250 ? _714 : _713;
    assign _717 = _248 ? _716 : _715;
    assign _719 = _246 ? _718 : _717;
    assign _721 = _244 ? _720 : _719;
    assign _723 = _242 ? _722 : _721;
    assign _725 = _240 ? _724 : _723;
    assign _727 = _238 ? _726 : _725;
    assign _729 = _236 ? _728 : _727;
    assign _731 = _227 ? _730 : _729;
    assign _733 = _211 ? _732 : _731;
    assign _711 = _190 == _186;
    assign _734 = _711 ? _733 : _710;
    assign _735 = _183 ? _734 : _710;
    assign _89 = _735;
    assign _756 = _225[36:36];
    assign _754 = _234[68:68];
    assign _739 = _252 ? _738 : _736;
    assign _741 = _250 ? _740 : _739;
    assign _743 = _248 ? _742 : _741;
    assign _745 = _246 ? _744 : _743;
    assign _747 = _244 ? _746 : _745;
    assign _749 = _242 ? _748 : _747;
    assign _751 = _240 ? _750 : _749;
    assign _753 = _238 ? _752 : _751;
    assign _755 = _236 ? _754 : _753;
    assign _757 = _227 ? _756 : _755;
    assign _759 = _211 ? _758 : _757;
    assign _737 = _190 == _186;
    assign _760 = _737 ? _759 : _736;
    assign _761 = _183 ? _760 : _736;
    assign _91 = _761;
    assign _782 = _225[35:4];
    assign _780 = _234[67:36];
    assign _765 = _252 ? _764 : _762;
    assign _767 = _250 ? _766 : _765;
    assign _769 = _248 ? _768 : _767;
    assign _771 = _246 ? _770 : _769;
    assign _773 = _244 ? _772 : _771;
    assign _775 = _242 ? _774 : _773;
    assign _777 = _240 ? _776 : _775;
    assign _779 = _238 ? _778 : _777;
    assign _781 = _236 ? _780 : _779;
    assign _783 = _227 ? _782 : _781;
    assign _785 = _211 ? _784 : _783;
    assign _763 = _190 == _186;
    assign _786 = _763 ? _785 : _762;
    assign _787 = _183 ? _786 : _762;
    assign _93 = _787;
    assign _808 = _225[3:3];
    assign _806 = _234[35:35];
    assign _791 = _252 ? _790 : _788;
    assign _793 = _250 ? _792 : _791;
    assign _795 = _248 ? _794 : _793;
    assign _797 = _246 ? _796 : _795;
    assign _799 = _244 ? _798 : _797;
    assign _801 = _242 ? _800 : _799;
    assign _803 = _240 ? _802 : _801;
    assign _805 = _238 ? _804 : _803;
    assign _807 = _236 ? _806 : _805;
    assign _809 = _227 ? _808 : _807;
    assign _811 = _211 ? _810 : _809;
    assign _789 = _190 == _186;
    assign _812 = _789 ? _811 : _788;
    assign _813 = _183 ? _812 : _788;
    assign _95 = _813;
    assign _870 = _225[2:2];
    assign _868 = _234[34:34];
    assign _853 = _252 ? _852 : _850;
    assign _855 = _250 ? _854 : _853;
    assign _857 = _248 ? _856 : _855;
    assign _859 = _246 ? _858 : _857;
    assign _861 = _244 ? _860 : _859;
    assign _863 = _242 ? _862 : _861;
    assign _865 = _240 ? _864 : _863;
    assign _867 = _238 ? _866 : _865;
    assign _869 = _236 ? _868 : _867;
    assign _871 = _227 ? _870 : _869;
    assign _873 = _211 ? _872 : _871;
    assign _252 = _194 == _251;
    assign _836 = _252 ? _311 : _190;
    assign _250 = _194 == _249;
    assign _837 = _250 ? _311 : _836;
    assign _248 = _194 == _247;
    assign _838 = _248 ? _311 : _837;
    assign _246 = _194 == _245;
    assign _839 = _246 ? _311 : _838;
    assign _244 = _194 == _243;
    assign _840 = _244 ? _311 : _839;
    assign _242 = _194 == _241;
    assign _841 = _242 ? _311 : _840;
    assign _240 = _194 == _239;
    assign _842 = _240 ? _311 : _841;
    assign _238 = _194 == _237;
    assign _843 = _238 ? _311 : _842;
    assign _231 = _194 == _230;
    assign _232 = _214 & _231;
    Load
        load
        ( .clock(_177), .clear(_175), .enable(_232), .funct3(_200), .source(_223), .memory_controller_to_hart$memory_controller_to_hart_valid(_104), .memory_controller_to_hart$memory_controller_to_hart_error(_102), .memory_controller_to_hart$memory_controller_to_hart_read_data(_100), .hart_to_memory_controller$hart_to_memory_controller_ready(_98), .hart_to_memory_controller$hart_to_memory_controller_write_data(_234[100:69]), .hart_to_memory_controller$hart_to_memory_controller_write(_234[68:68]), .hart_to_memory_controller$hart_to_memory_controller_address(_234[67:36]), .hart_to_memory_controller$hart_to_memory_controller_valid(_234[35:35]), .memory_controller_to_hart$memory_controller_to_hart_ready(_234[34:34]), .finished(_234[33:33]), .error(_234[32:32]), .new_rd(_234[31:0]) );
    assign _235 = _234[33:33];
    assign _229 = _194 == _228;
    assign _236 = _229 & _235;
    assign _844 = _236 ? _311 : _843;
    assign _98 = hart_to_memory_controller_ready;
    assign _100 = memory_controller_to_hart_read_data;
    assign _102 = memory_controller_to_hart_error;
    assign _104 = memory_controller_to_hart_valid;
    assign _815 = _179[19:15];
    always @* begin
        case (_815)
        0: _816 <= _169;
        1: _816 <= _167;
        2: _816 <= _165;
        3: _816 <= _163;
        4: _816 <= _161;
        5: _816 <= _159;
        6: _816 <= _157;
        7: _816 <= _155;
        8: _816 <= _153;
        9: _816 <= _151;
        10: _816 <= _149;
        11: _816 <= _147;
        12: _816 <= _145;
        13: _816 <= _143;
        14: _816 <= _141;
        15: _816 <= _139;
        16: _816 <= _137;
        17: _816 <= _135;
        18: _816 <= _133;
        19: _816 <= _131;
        20: _816 <= _129;
        21: _816 <= _127;
        22: _816 <= _125;
        23: _816 <= _123;
        24: _816 <= _121;
        25: _816 <= _119;
        26: _816 <= _117;
        27: _816 <= _115;
        28: _816 <= _113;
        29: _816 <= _111;
        30: _816 <= _109;
        default: _816 <= _107;
        endcase
    end
    assign _814 = _190 == _560;
    assign _817 = _814 ? _816 : _223;
    assign _818 = _183 ? _817 : _223;
    assign _105 = _818;
    always @(posedge _177) begin
        if (_175)
            _223 <= _222;
        else
            _223 <= _105;
    end
    assign _107 = registers_general31;
    assign _109 = registers_general30;
    assign _111 = registers_general29;
    assign _113 = registers_general28;
    assign _115 = registers_general27;
    assign _117 = registers_general26;
    assign _119 = registers_general25;
    assign _121 = registers_general24;
    assign _123 = registers_general23;
    assign _125 = registers_general22;
    assign _127 = registers_general21;
    assign _129 = registers_general20;
    assign _131 = registers_general19;
    assign _133 = registers_general18;
    assign _135 = registers_general17;
    assign _137 = registers_general16;
    assign _139 = registers_general15;
    assign _141 = registers_general14;
    assign _143 = registers_general13;
    assign _145 = registers_general12;
    assign _147 = registers_general11;
    assign _149 = registers_general10;
    assign _151 = registers_general9;
    assign _153 = registers_general8;
    assign _155 = registers_general7;
    assign _157 = registers_general6;
    assign _159 = registers_general5;
    assign _161 = registers_general4;
    assign _163 = registers_general3;
    assign _165 = registers_general2;
    assign _167 = registers_general1;
    assign _169 = registers_general0;
    assign _820 = _179[11:7];
    always @* begin
        case (_820)
        0: _821 <= _169;
        1: _821 <= _167;
        2: _821 <= _165;
        3: _821 <= _163;
        4: _821 <= _161;
        5: _821 <= _159;
        6: _821 <= _157;
        7: _821 <= _155;
        8: _821 <= _153;
        9: _821 <= _151;
        10: _821 <= _149;
        11: _821 <= _147;
        12: _821 <= _145;
        13: _821 <= _143;
        14: _821 <= _141;
        15: _821 <= _139;
        16: _821 <= _137;
        17: _821 <= _135;
        18: _821 <= _133;
        19: _821 <= _131;
        20: _821 <= _129;
        21: _821 <= _127;
        22: _821 <= _125;
        23: _821 <= _123;
        24: _821 <= _121;
        25: _821 <= _119;
        26: _821 <= _117;
        27: _821 <= _115;
        28: _821 <= _113;
        29: _821 <= _111;
        30: _821 <= _109;
        default: _821 <= _107;
        endcase
    end
    assign _819 = _190 == _560;
    assign _822 = _819 ? _821 : _220;
    assign _823 = _183 ? _822 : _220;
    assign _170 = _823;
    always @(posedge _177) begin
        if (_175)
            _220 <= _219;
        else
            _220 <= _170;
    end
    assign _216 = _194 == _215;
    assign _214 = _186 == _190;
    assign _217 = _214 & _216;
    Store
        store
        ( .clock(_177), .clear(_175), .enable(_217), .funct3(_200), .destination(_220), .value(_223), .memory_controller_to_hart$memory_controller_to_hart_valid(_104), .memory_controller_to_hart$memory_controller_to_hart_error(_102), .memory_controller_to_hart$memory_controller_to_hart_read_data(_100), .hart_to_memory_controller$hart_to_memory_controller_ready(_98), .hart_to_memory_controller$hart_to_memory_controller_write_data(_225[68:37]), .hart_to_memory_controller$hart_to_memory_controller_write(_225[36:36]), .hart_to_memory_controller$hart_to_memory_controller_address(_225[35:4]), .hart_to_memory_controller$hart_to_memory_controller_valid(_225[3:3]), .memory_controller_to_hart$memory_controller_to_hart_ready(_225[2:2]), .finished(_225[1:1]), .error(_225[0:0]) );
    assign _226 = _225[1:1];
    assign _213 = _194 == _212;
    assign _227 = _213 & _226;
    assign _845 = _227 ? _311 : _844;
    assign _172 = ecall_transaction_finished;
    assign _825 = _179[14:12];
    assign _824 = _190 == _560;
    assign _826 = _824 ? _825 : _200;
    assign _827 = _183 ? _826 : _200;
    assign _173 = _827;
    always @(posedge _177) begin
        if (_175)
            _200 <= _199;
        else
            _200 <= _173;
    end
    assign _202 = _200 == _201;
    assign _196 = _194 == _195;
    assign _191 = _186 == _190;
    assign _197 = _191 & _196;
    assign is_ecall_0 = _197 & _202;
    assign _210 = is_ecall_0 ? _172 : vdd;
    assign _175 = clear;
    assign _177 = clock;
    assign _179 = instruction;
    assign _829 = _179[6:0];
    assign _828 = _190 == _560;
    assign _830 = _828 ? _829 : _194;
    assign _831 = _183 ? _830 : _194;
    assign _180 = _831;
    always @(posedge _177) begin
        if (_175)
            _194 <= _193;
        else
            _194 <= _180;
    end
    assign _209 = _194 == _208;
    assign _211 = _209 & _210;
    assign _846 = _211 ? _311 : _845;
    assign _834 = _190 == _311;
    assign _835 = _834 ? _560 : _190;
    assign _833 = _190 == _186;
    assign _847 = _833 ? _846 : _835;
    assign _832 = _190 == _560;
    assign _848 = _832 ? _186 : _847;
    assign _849 = _183 ? _848 : _190;
    assign _181 = _849;
    always @(posedge _177) begin
        if (_175)
            _190 <= _188;
        else
            _190 <= _181;
    end
    assign _851 = _190 == _186;
    assign _874 = _851 ? _873 : _850;
    assign _183 = enable;
    assign _875 = _183 ? _874 : _850;
    assign _184 = _875;

    /* aliases */

    /* output assignments */
    assign memory_controller_to_hart_ready = _184;
    assign hart_to_memory_controller_valid = _95;
    assign hart_to_memory_controller_address = _93;
    assign hart_to_memory_controller_write = _91;
    assign hart_to_memory_controller_write_data = _89;
    assign finished = _709;
    assign new_registers_pc = _86;
    assign new_registers_general0 = _75;
    assign new_registers_general1 = _64;
    assign new_registers_general2 = _62;
    assign new_registers_general3 = _60;
    assign new_registers_general4 = _58;
    assign new_registers_general5 = _56;
    assign new_registers_general6 = _54;
    assign new_registers_general7 = _52;
    assign new_registers_general8 = _50;
    assign new_registers_general9 = _48;
    assign new_registers_general10 = _46;
    assign new_registers_general11 = _44;
    assign new_registers_general12 = _42;
    assign new_registers_general13 = _40;
    assign new_registers_general14 = _38;
    assign new_registers_general15 = _36;
    assign new_registers_general16 = _34;
    assign new_registers_general17 = _32;
    assign new_registers_general18 = _30;
    assign new_registers_general19 = _28;
    assign new_registers_general20 = _26;
    assign new_registers_general21 = _24;
    assign new_registers_general22 = _22;
    assign new_registers_general23 = _20;
    assign new_registers_general24 = _18;
    assign new_registers_general25 = _16;
    assign new_registers_general26 = _14;
    assign new_registers_general27 = _12;
    assign new_registers_general28 = _10;
    assign new_registers_general29 = _8;
    assign new_registers_general30 = _6;
    assign error = _206;
    assign is_ecall = is_ecall_0;

endmodule
