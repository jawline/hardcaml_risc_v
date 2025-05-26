`timescale 1ns / 1ps

module hardware_top(
        input sys_clock,
        input rst_n,
        input uart_rx,
        output uart_tx,
        output led1,
        output led2,
        output led3,
        output led4
);
wire hart_clock;

clk_wiz_0 ( .sys_clock(sys_clock), .hart_clock(hart_clock), .reset(~rst_n) );

top cpu ( .clock(hart_clock), .clear(~rst_n), .uart_rx(uart_rx));

assign uart_tx = cpu.uart_tx;
assign led1 = rst_n;
assign led2 = uart_rx;
assign led3 = cpu.uart_rx_valid;
assign led4 = cpu.serial_to_packet_valid;

endmodule
