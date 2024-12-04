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
wire i2c_clock;
wire hart_clock;
wire video_clock;
wire locked;

wire done_9134;
wire                       video_hs;
wire                       video_vs;
wire                       video_de;
wire                       x_changed;

clk_wiz_0 ( .clk_in1_p(sys_clock_p), .clk_in1_n(sys_clock_n), .hart_clock(hart_clock), .i2c_clock(i2c_clock), .video_clock(video_clock), .reset(~rst_n), .locked(locked) );

top cpu ( .clock(hart_clock), .clear(~done_9134), .uart_rx(uart_rx));

wire[9:0]                  lut_index;
wire[31:0]                 lut_data;

i2c_config i2c_config_m0(
        .rst                        (~locked                  ),
        .clk                        (i2c_clock                ),
        .clk_div_cnt                (16'd500                  ),
        .i2c_addr_2byte             (1'b0                     ),
        .lut_index                  (lut_index                ),
        .lut_dev_addr               (lut_data[31:24]          ),
        .lut_reg_addr               (lut_data[23:8]           ),
        .lut_reg_data               (lut_data[7:0]            ),
        .error                      (                         ),
        .done                       (done_9134                ),
        .i2c_scl                    (hdmi_scl                 ),
        .i2c_sda                    (hdmi_sda                 )
);

lut_hdmi lut_hdmi_m0(
        .lut_index                  (lut_index                ),
        .lut_data                   (lut_data                 )
);

//uart_loopback_top uart_loopback_top( .clock(hart_clock), .clear(~locked), .uart_rx(uart_rx));
//assign uart_tx = uart_loopback_top.uart_tx;
assign uart_tx = cpu.uart_tx;
assign led1 = rst_n;
assign led2 = ~uart_rx;
assign led3 = ~cpu.uart_rx_valid;
assign led4 = ~cpu.serial_to_packet_valid;

assign vout_clk = video_clock;
assign vout_data = cpu.video_out$video_data$vdata;
assign vout_hs = cpu.video_out$video_signals$h_sync;
assign vout_vs = cpu.video_out$video_signals$v_sync;
assign vout_de = cpu.video_out$video_signals$video_active;
assign hdmi_nreset = locked;

endmodule
