module Fetch (
    memory_controller_to_hartread_data,
    memory_controller_to_harterror,
    memory_controller_to_hartvalid,
    address,
    should_fetch,
    hart_to_memory_controllerready,
    memory_controller_to_hartready,
    hart_to_memory_controllervalid,
    hart_to_memory_controlleraddress,
    hart_to_memory_controllerwrite,
    hart_to_memory_controllerwrite_data,
    has_fetched,
    instruction,
    error
);

    input [31:0] memory_controller_to_hartread_data;
    input memory_controller_to_harterror;
    input memory_controller_to_hartvalid;
    input [31:0] address;
    input should_fetch;
    input hart_to_memory_controllerready;
    output memory_controller_to_hartready;
    output hart_to_memory_controllervalid;
    output [31:0] hart_to_memory_controlleraddress;
    output hart_to_memory_controllerwrite;
    output [31:0] hart_to_memory_controllerwrite_data;
    output has_fetched;
    output [31:0] instruction;
    output error;

    /* signal declarations */
    wire _21;
    wire [31:0] _3;
    wire _6;
    wire _22;
    wire _8;
    wire _23;
    wire [31:0] _24 = 32'b00000000000000000000000000000000;
    wire _25 = 1'b0;
    wire [31:0] _13;
    wire _16;
    wire _18;
    wire _26;
    wire _27 = 1'b1;

    /* logic */
    assign _21 = _8 & _6;
    assign _3 = memory_controller_to_hartread_data;
    assign _6 = memory_controller_to_harterror;
    assign _22 = ~ _6;
    assign _8 = memory_controller_to_hartvalid;
    assign _23 = _8 & _22;
    assign _13 = address;
    assign _16 = should_fetch;
    assign _18 = hart_to_memory_controllerready;
    assign _26 = _18 & _16;

    /* aliases */

    /* output assignments */
    assign memory_controller_to_hartready = _27;
    assign hart_to_memory_controllervalid = _26;
    assign hart_to_memory_controlleraddress = _13;
    assign hart_to_memory_controllerwrite = _25;
    assign hart_to_memory_controllerwrite_data = _24;
    assign has_fetched = _23;
    assign instruction = _3;
    assign error = _21;

endmodule
