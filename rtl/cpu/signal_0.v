module signal_0 (
    last,
    valid,
    clear,
    clock,
    data,
    ready,
    signal
);

    input last;
    input valid;
    input clear;
    input clock;
    input [7:0] data;
    output ready;
    output signal;

    /* signal declarations */
    wire _2;
    wire _4;
    wire _7;
    wire vdd = 1'b1;

    /* logic */
    assign _2 = last;
    assign _4 = valid;
    assign _7 = _4 & _2;

    /* aliases */

    /* output assignments */
    assign ready = vdd;
    assign signal = _7;

endmodule
