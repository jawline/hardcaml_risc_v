`timescale 1ns / 1ps

module hardware_top(
        input sys_clock_p,
        input sys_clock_n,
        input rst_n,
        input uart_rx,
        output led1,
        output led2,
        output led3,
        output led4,
        output uart_tx,
        inout                       hdmi_scl,           //HDMI I2C clock
        inout                       hdmi_sda,           //HDMI I2C data
        output                      hdmi_nreset,        //9134 reset
        output                      vout_hs,            //horizontal synchronization for 9134
        output                      vout_vs,            //vertical synchronization for 9134
        output                      vout_de,            //data valid for 9134
        output                      vout_clk,           //clock for 9134
        output[23:0]                vout_data           //data for 9134
);
wire hart_clock;
wire memory_clock;
wire video_clock;
wire locked;
wire clear;

wire done_9134;
wire                       video_hs;
wire                       video_vs;
wire                       video_de;

clk_wiz_0 ( .clk_in1_p(sys_clock_p), .clk_in1_n(sys_clock_n), .hart_clock(hart_clock), .memory_clock(memory_reference_clock), .video_clock(video_clock), .reset(~rst_n), .locked(locked) );

// TODO: memory clock should be the memory reference clock
assign memory_clock = hart_clock;
assign clear = done_9134;

top cpu (
    .hart_clock$clock(hart_clock),
    .hart_clock$clear(clear),
    .memory_clock$clock(memory_clock),
    .memory_clock$clear(clear),
    .video_clock$clock(video_clock),
    .video_clock$clear(clear),
    .uart_rx(uart_rx)
);

assign uart_tx = cpu.uart_tx;
assign led1 = rst_n;
assign led2 = ~uart_rx;
assign led3 = ~cpu.uart_rx_valid;

assign vout_clk = video_clock;
assign vout_data = cpu.video_out$video_data$vdata;
assign vout_hs = cpu.video_out$video_signals$h_sync;
assign vout_vs = cpu.video_out$video_signals$v_sync;
assign vout_de = cpu.video_out$video_signals$video_active;
assign hdmi_nreset = locked;

endmodule
