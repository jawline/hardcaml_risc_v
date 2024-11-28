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
 output uart_tx
    );
    wire clock;
    wire locked;
    
   clk_wiz_0 ( .clk_in1_p(sys_clock_p), .clk_in1_n(sys_clock_n), .clk_out1(clock), .reset(0), .locked(locked) );
   top
        cpu
        ( .clock(clock), .clear(~reset_btn), .uart_rx(uart_rx));
 
    assign uart_tx = cpu.uart_tx;
    assign led1 = reset_btn;
    assign led2 = ~cpu.stop_bit_unstable;
    assign led3 = ~cpu.uart_rx_valid;
    assign led4 = ~cpu.serial_to_packet_valid;
endmodule
