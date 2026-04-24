`timescale 1ns / 1ps

module board_top(

    );
    
    wire clock;
    wire reset;
    wire pl_resetn0;
    
    // Internal wires to connect the two blocks
    wire system_to_zynq; 
    wire zynq_to_system;

    zynq_ultra_ps_e_0 pl_block (
        .pl_clk0(clock), 
        .emio_uart1_txd(zynq_to_system), // Zynq sends
        .emio_uart1_rxd(system_to_zynq), // Zynq receives
        .pl_resetn0(pl_resetn0)
    );
    
    assign reset = !pl_resetn0;
    
    top system (
        .hart_clock$clock(clock), 
        .hart_clock$clear(reset), 
        .memory_clock$clock(clock), 
        .memory_clock$clear(reset), 
        .dma_clock$clock(clock), 
        .dma_clock$clear(reset), 
        .uart_rx(zynq_to_system), 
        .uart_tx(system_to_zynq) 
    );
    
endmodule
