`timescale 1ns / 1ps

module hardware_top(
//Clocks
input board_clock,
input rst_n,

// UART
input uart_rx,
output uart_tx,
        
// LEDs
output led1,
output led2,
output led3,
output led4,

//RAM Interface
inout[15:0] ddr2_dq,
inout[1:0] ddr2_dqs_n,
inout[1:0] ddr2_dqs_p,
output[12:0] ddr2_addr,
output[2:0] ddr2_ba,
output ddr2_ras_n,
output ddr2_cas_n,
output ddr2_we_n,
output ddr2_ck_p,
output ddr2_ck_n,
output ddr2_cke,
output ddr2_cs_n,
output[1:0] ddr2_dm,
output ddr2_odt
);

// Clocks
wire memory_reference_clock;
wire io_delay_ref_clock;

board_clocks clocks(
    .reset(~rst_n),
    .board_clock(board_clock),
    .io_delay_ref_clock(io_delay_ref_clock),
    .memory_clock(memory_reference_clock)
);

// CPU instantiation
wire hart_clock;
wire clear;
wire rst;

// Write Address Channel
wire        axi_id_w;
wire [26:0] axi_addr_w;
wire [7:0]  axi_len_w;
wire [2:0]  axi_size_w;
wire [1:0]  axi_burst_w;
wire        axi_valid_w;
wire        axi_ready_w;

// Write Data Channel
wire [63:0] axi_wdata;
wire [7:0]  axi_wstrb;
wire        axi_wlast;
wire        axi_wvalid;
wire        axi_wready;

// Write Response Channel
wire        axi_bid;
wire [1:0]  axi_bresp;
wire        axi_bvalid;
wire        axi_bready;

// Read Address Channel
wire        axi_id_r;
wire [26:0] axi_addr_r;
wire [7:0]  axi_len_r;
wire [2:0]  axi_size_r;
wire [1:0]  axi_burst_r;
wire        axi_valid_r;
wire        axi_ready_r;

// Read Data Channel
wire         axi_rid;
wire [63:0]  axi_rdata;
wire [1:0]   axi_rresp;
wire         axi_rlast;
wire         axi_rvalid;
wire         axi_rready;

// --- CPU Instantiation ---
top cpu (
    .hart_clock_clock(hart_clock),
    .hart_clock_clear(clear),
    .dma_clock_clock(hart_clock),
    .dma_clock_clear(clear),
    .memory_clock_clock(hart_clock),
    .memory_clock_clear(clear),
    .video_clock_clock(hart_clock),
    .video_clock_clear(clear),
    .uart_rx(uart_rx),

    // Read Responses (Inputs to CPU)
    .axi_i_memory_ARREADY(axi_ready_r),
    .axi_i_memory_RLAST  (axi_rlast),
    .axi_i_memory_RRESP  (axi_rresp),
    .axi_i_memory_RDATA  (axi_rdata),
    .axi_i_memory_RID    (axi_rid),
    .axi_i_memory_RVALID (axi_rvalid),

    // Write Responses (Inputs to CPU)
    .axi_i_memory_BRESP  (axi_bresp),
    .axi_i_memory_BID    (axi_bid),
    .axi_i_memory_WREADY (axi_wready),
    .axi_i_memory_AWREADY(axi_ready_w),
    .axi_i_memory_BVALID (axi_bvalid),
    
    // Write Requests (Outputs from CPU)
    .axi_o_memory_WVALID  (axi_wvalid),
    .axi_o_memory_AWVALID (axi_valid_w),
    .axi_o_memory_AWID    (axi_id_w),
    .axi_o_memory_AWADDR  (axi_addr_w),
    .axi_o_memory_WDATA   (axi_wdata),
    .axi_o_memory_AWLEN   (axi_len_w),
    .axi_o_memory_AWSIZE  (axi_size_w),
    .axi_o_memory_AWBURST (axi_burst_w),
    .axi_o_memory_WSTRB   (axi_wstrb),
    .axi_o_memory_WLAST   (axi_wlast),
    .axi_o_memory_BREADY  (axi_bready),

    // Read Requests (Outputs from CPU)
    .axi_o_memory_ARVALID (axi_valid_r),
    .axi_o_memory_ARID    (axi_id_r),
    .axi_o_memory_ARADDR  (axi_addr_r),
    .axi_o_memory_ARLEN   (axi_len_r),
    .axi_o_memory_ARSIZE  (axi_size_r),
    .axi_o_memory_ARBURST (axi_burst_r),
    .axi_o_memory_RREADY  (axi_rready),

    // UART output
    .uart_tx(uart_tx)
);

// Memory instantiation
wire init_calib_complete;

ddr_memory main_memory (
    .sys_clk_i(memory_reference_clock),
    .clk_ref_i(io_delay_ref_clock),
    .sys_rst(rst_n),
    .aresetn(rst_n),
    .app_sr_req(1'b0),
    .app_ref_req(1'b0),
    .app_zq_req(1'b0),

    // UI clocks
    .ui_clk(hart_clock),
    .ui_clk_sync_rst(rst),
    
    // Memory Physical Interface
    .ddr2_addr(ddr2_addr),
    .ddr2_ba(ddr2_ba),
    .ddr2_cas_n(ddr2_cas_n),
    .ddr2_ck_n(ddr2_ck_n),
    .ddr2_ck_p(ddr2_ck_p),
    .ddr2_cke(ddr2_cke),
    .ddr2_ras_n(ddr2_ras_n),
    .ddr2_we_n(ddr2_we_n),
    .ddr2_dq(ddr2_dq),
    .ddr2_dqs_n(ddr2_dqs_n),
    .ddr2_dqs_p(ddr2_dqs_p),
    .init_calib_complete(init_calib_complete),
    .ddr2_cs_n(ddr2_cs_n),
    .ddr2_dm(ddr2_dm),
    .ddr2_odt(ddr2_odt),

    // AXI Slave Inputs (from CPU)
    .s_axi_awid     (axi_id_w),
    .s_axi_awaddr   (axi_addr_w),
    .s_axi_awlen    (axi_len_w),
    .s_axi_awsize   (axi_size_w),
    .s_axi_awburst  (axi_burst_w),
    .s_axi_awlock   (1'b0),
    .s_axi_awcache  (4'b0011),
    .s_axi_awprot   (3'b000),
    .s_axi_awqos    (4'b0000),
    .s_axi_awvalid  (axi_valid_w),
    .s_axi_wdata    (axi_wdata),
    .s_axi_wstrb    (axi_wstrb),
    .s_axi_wlast    (axi_wlast),
    .s_axi_wvalid   (axi_wvalid),
    .s_axi_bready   (axi_bready),
    .s_axi_arid     (axi_id_r),
    .s_axi_araddr   (axi_addr_r),
    .s_axi_arlen    (axi_len_r),
    .s_axi_arsize   (axi_size_r),
    .s_axi_arburst  (axi_burst_r),
    .s_axi_arlock   (1'b0),
    .s_axi_arcache  (4'b0011),
    .s_axi_arprot   (3'b000),
    .s_axi_arqos    (4'b0000),
    .s_axi_arvalid  (axi_valid_r),
    .s_axi_rready   (axi_rready),

    // AXI Slave Outputs (to CPU)
    .s_axi_awready  (axi_ready_w),
    .s_axi_wready   (axi_wready),
    .s_axi_bid      (axi_bid),
    .s_axi_bresp    (axi_bresp),
    .s_axi_bvalid   (axi_bvalid),
    .s_axi_arready  (axi_ready_r),
    .s_axi_rid      (axi_rid),
    .s_axi_rdata    (axi_rdata),
    .s_axi_rresp    (axi_rresp),
    .s_axi_rlast    (axi_rlast),
    .s_axi_rvalid   (axi_rvalid)
);

assign clear = ~init_calib_complete;

assign led1 = rst_n;
assign led2 = ~uart_rx;
assign led3 = init_calib_complete;
assign led4 = cpu.uart_rx_valid;

endmodule
