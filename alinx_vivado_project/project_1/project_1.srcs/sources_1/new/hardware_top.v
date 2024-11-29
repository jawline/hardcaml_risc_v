`timescale 1ns / 1ps

module hardware_top(
        input sys_clock_p,
        input sys_clock_n,
        input uart_rx,
        input reset_btn,
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
wire locked;
wire vdata;

IBUFDS sys_clk_ibufgds
(
        .O                          (i2c_clock                ),
        .I                          (sys_clk_p                ),
        .IB                         (sys_clk_n                )
);

clk_wiz_0 ( .clk_in1_p(sys_clock_p), .clk_in1_n(sys_clock_n), .clk_out1(hart_clock), .reset(0), .locked(locked) );
top
cpu
( .clock(hart_clock), .clear(~reset_btn), .uart_rx(uart_rx), .video_out$vdata(vdata));


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

assign uart_tx = cpu.uart_tx;
assign led1 = reset_btn;
assign led2 = ~cpu.stop_bit_unstable;
assign led3 = ~cpu.uart_rx_valid;
assign led4 = ~cpu.serial_to_packet_valid;
assign vout_data = {24{vdata}};
endmodule
