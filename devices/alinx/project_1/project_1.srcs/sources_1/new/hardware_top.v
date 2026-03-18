`timescale 1ns / 1ps

module hardware_top(
        input clock_200_p,
        input clock_200_n,
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
        output[23:0]                vout_data,           //data for 9134

        //DDR 3 ports
        // Inouts
        inout [31:0]                       ddr3_dq,                     //ddr3 data
        inout [3:0]                        ddr3_dqs_n,                  //ddr3 dqs negative
        inout [3:0]                        ddr3_dqs_p,                  //ddr3 dqs positive
        // Outputs
        output [14:0]                     ddr3_addr,                    //ddr3 address
        output [2:0]                      ddr3_ba,                      //ddr3 bank
        output                            ddr3_ras_n,                   //ddr3 ras_n
        output                            ddr3_cas_n,                   //ddr3 cas_n
        output                            ddr3_we_n,                    //ddr3 write enable
        output                            ddr3_reset_n,                 //ddr3 reset,
        output [0:0]                      ddr3_ck_p,                    //ddr3 clock negative
        output [0:0]                      ddr3_ck_n,                    //ddr3 clock positive
        output [0:0]                      ddr3_cke,                     //ddr3_cke,
        output [0:0]                      ddr3_cs_n,                    //ddr3 chip select,
        output [3:0]                      ddr3_dm,                      //ddr3_dm
        output [0:0]                      ddr3_odt                     //ddr3_odt
);

// Clocks
clk_wiz_0 ( .clk_in1_p(clock_200_p), .clk_in1_n(clock_200_n), .memory_clock(memory_reference_clock), .video_clock(video_clock), .usr_clk_200(usr_clk_200), .reset(~rst_n), .locked(locked) );

// Video signalling
wire                       video_hs;
wire                       video_vs;
wire                       video_de;

// CPU instantiation

// Tie the hart to the memory clock
wire hart_clock;
wire clear;

// Master to Slave (Requests)
wire         axi_id_w;
wire [29:0] axi_addr_w;
wire [7:0]  axi_len_w;
wire [2:0]  axi_size_w;
wire [1:0]  axi_burst_w;
wire        axi_lock_w;
wire [3:0]  axi_cache_w;
wire [2:0]  axi_prot_w;
wire [3:0]  axi_qos_w;
wire        axi_valid_w;
wire        axi_ready_w; // Slave to Master

// Write Data Channel
wire [127:0] axi_wdata;
wire [15:0]  axi_wstrb;
wire        axi_wlast;
wire        axi_wvalid;
wire        axi_wready; // Slave to Master

// Write Response Channel (Slave to Master)
wire        axi_bid;
wire [1:0]  axi_bresp;
wire        axi_bvalid;
wire        axi_bready; // Master to Slave

// Read Address Channel
wire        axi_id_r;
wire [29:0] axi_addr_r;
wire [7:0]  axi_len_r;
wire [2:0]  axi_size_r;
wire [1:0]  axi_burst_r;
wire        axi_lock_r;
wire [3:0]  axi_cache_r;
wire [2:0]  axi_prot_r;
wire [3:0]  axi_qos_r;
wire        axi_valid_r;
wire        axi_ready_r; // Slave to Master

// Read Data Channel (Slave to Master)
wire         axi_rid;
wire [127:0] axi_rdata;
wire [1:0]  axi_rresp;
wire        axi_rlast;
wire        axi_rvalid;
wire        axi_rready; // Master to Slave

// --- CPU Instantiation ---
top cpu (
    .hart_clock$clock(hart_clock),
    .hart_clock$clear(clear),
    .dma_clock$clock(hart_clock),
    .dma_clock$clear(clear),
    .memory_clock$clock(hart_clock),
    .memory_clock$clear(clear),
    .video_clock$clock(hart_clock),
    .video_clock$clear(clear),
    .uart_rx(uart_rx),

    // Read Responses (Inputs to CPU)
    .axi_i$memory$ARREADY(axi_ready_r),
    .axi_i$memory$RLAST  (axi_rlast),
    .axi_i$memory$RRESP  (axi_rresp),
    .axi_i$memory$RDATA  (axi_rdata),
    .axi_i$memory$RID    (axi_rid),
    .axi_i$memory$RVALID (axi_rvalid),

    // Write Responses (Inputs to CPU)
    .axi_i$memory$BRESP  (axi_bresp),
    .axi_i$memory$BID    (axi_bid),
    .axi_i$memory$WREADY (axi_wready),
    .axi_i$memory$AWREADY(axi_ready_w),
    .axi_i$memory$BVALID (axi_bvalid),
    
    // Write Requests (Outputs from CPU)
    .axi_o$memory$WVALID  (axi_wvalid),
    .axi_o$memory$AWVALID (axi_valid_w),
    .axi_o$memory$AWID    (axi_id_w),
    .axi_o$memory$AWADDR  (axi_addr_w),
    .axi_o$memory$WDATA   (axi_wdata),
    .axi_o$memory$AWLEN   (axi_len_w),
    .axi_o$memory$AWSIZE  (axi_size_w),
    .axi_o$memory$AWBURST (axi_burst_w),
    .axi_o$memory$WSTRB   (axi_wstrb),
    .axi_o$memory$WLAST   (axi_wlast),
    .axi_o$memory$BREADY  (axi_bready),

    // Read Requests (Outputs from CPU)
    .axi_o$memory$ARVALID (axi_valid_r),
    .axi_o$memory$ARID    (axi_id_r),
    .axi_o$memory$ARADDR  (axi_addr_r),
    .axi_o$memory$ARLEN   (axi_len_r),
    .axi_o$memory$ARSIZE  (axi_size_r),
    .axi_o$memory$ARBURST (axi_burst_r),
    .axi_o$memory$RREADY  (axi_rready)
);

assign uart_tx = cpu.uart_tx;

assign vout_clk = cpu.video_out$video_signals$video_clock;
assign vout_data = cpu.video_out$video_data$vdata;
assign vout_hs = cpu.video_out$video_signals$h_sync;
assign vout_vs = cpu.video_out$video_signals$v_sync;
assign vout_de = cpu.video_out$video_signals$video_active;

// Memory instantiation
ddr3 u_ddr3 (
    .sys_clk_i                      (memory_reference_clock),
    .clk_ref_i                      (usr_clk_200),
    .sys_rst                        (rst_n),
    .aresetn                        (rst_n),
    .app_sr_req(1'b0),
    .app_ref_req(1'b0),
    .app_zq_req(1'b0),

    // UI clocks
    .ui_clk                         (clk                    ),
    .ui_clk_sync_rst                (rst                    ),
    
    // Memory Physical Interface
    .ddr3_addr(ddr3_addr),
    .ddr3_ba(ddr3_ba),
    .ddr3_cas_n(ddr3_cas_n),
    .ddr3_ck_n(ddr3_ck_n),
    .ddr3_ck_p(ddr3_ck_p),
    .ddr3_cke(ddr3_cke),
    .ddr3_ras_n(ddr3_ras_n),
    .ddr3_we_n(ddr3_we_n),
    .ddr3_dq(ddr3_dq),
    .ddr3_dqs_n(ddr3_dqs_n),
    .ddr3_dqs_p(ddr3_dqs_p),
    .ddr3_reset_n(ddr3_reset_n),
    .init_calib_complete(init_calib_complete),
    .ddr3_cs_n(ddr3_cs_n),
    .ddr3_dm(ddr3_dm),
    .ddr3_odt(ddr3_odt),

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

assign hart_clock = clk;

// HDMI Initialization (Without these the screen won't come up even with a video signal)

wire[9:0]                  lut_index;
wire[31:0]                 lut_data;

i2c_config i2c_config_m0(
        .rst                        (~locked                  ),
        .clk                        (usr_clk_200              ),
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


assign clear = ~init_calib_complete;

assign led1 = rst_n;
assign led2 = ~uart_rx;
assign led3 = ~cpu.uart_rx_valid;

assign hdmi_nreset = locked;

endmodule
