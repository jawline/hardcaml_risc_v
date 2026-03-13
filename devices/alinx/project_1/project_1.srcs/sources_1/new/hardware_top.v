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
clk_wiz_0 ( .clk_in1_p(clock_200_p), .clk_in1_n(clock_200_n), .memory_clock(memory_reference_clock), .video_clock(video_clock), .i2c_clock(i2c_clock), .reset(~rst_n), .locked(locked) );

// Video signalling
wire                       video_hs;
wire                       video_vs;
wire                       video_de;


// Writes (Request)
wire        S0_AXI_WVALID;
wire        S0_AXI_AWVALID;
wire [3:0]  S0_AXI_AWID;     // ID Width 4
wire [29:0] S0_AXI_AWADDR;   // Address Width 32
wire [31:0] S0_AXI_WDATA;    // Data Width 32
wire [7:0]  S0_AXI_AWLEN;
wire [2:0]  S0_AXI_AWSIZE;
wire [1:0]  S0_AXI_AWBURST;
wire        S0_AXI_AWLOCK;
wire [3:0]  S0_AXI_AWCACHE;
wire [2:0]  S0_AXI_AWPROT;
wire [3:0]  S0_AXI_AWQOS;
wire [3:0]  S0_AXI_WSTRB;    // 32-bit data / 8 = 4 strobe bits
wire        S0_AXI_WLAST;
wire        S0_AXI_BREADY;

// Reads (Request)
wire        S0_AXI_ARVALID;
wire [3:0]  S0_AXI_ARID;     // ID Width 4
wire [29:0] S0_AXI_ARADDR;   // Address Width 32
wire [7:0]  S0_AXI_ARLEN;
wire [2:0]  S0_AXI_ARSIZE;
wire [1:0]  S0_AXI_ARBURST;
wire [3:0]  S0_AXI_ARQOS;
wire [2:0]  S0_AXI_ARPROT;
wire [3:0]  S0_AXI_ARCACHE;
wire        S0_AXI_ARLOCK;
wire        S0_AXI_RREADY;

// Read response assignments
wire arready;
wire rlast;
wire[1:0] rresp;
wire[31:0] rdata;
wire[3:0] rid;
wire rvalid;

// Write Response Assignments
wire[1:0] bresp;
wire[3:0] bid;
wire wready;
wire awready;
wire bvalid;

// CPU instantiation

// Tie the hart to the memory clock
wire hart_clock;
wire clear;

top cpu (
    .hart_clock$clock(hart_clock),
    .hart_clock$clear(clear),
    .dma_clock$clock(hart_clock),
    .dma_clock$clear(clear),
    .memory_clock$clock(hart_clock),
    .memory_clock$clear(clear),
    .video_clock$clock(video_clock),
    .video_clock$clear(clear),
    .uart_rx(uart_rx),

    // Read response assignments
    .axi_i$memory$ARREADY(arready),
    .axi_i$memory$RLAST(rlast),
    .axi_i$memory$RRESP(rresp),
    .axi_i$memory$RDATA(rdata),
    .axi_i$memory$RID(rid),
    .axi_i$memory$RVALID(rvalid),

    // Write Response Assignments
    .axi_i$memory$BRESP(bresp),
    .axi_i$memory$BID(bid),
    .axi_i$memory$WREADY(wready),
    .axi_i$memory$AWREADY(awready),
    .axi_i$memory$BVALID(bvalid),
    
    // Writes (Request)
    .axi_o$memory$WVALID  (S0_AXI_WVALID),
    .axi_o$memory$AWVALID (S0_AXI_AWVALID),
    .axi_o$memory$AWID    (S0_AXI_AWID),
    .axi_o$memory$AWADDR  (S0_AXI_AWADDR),
    .axi_o$memory$WDATA   (S0_AXI_WDATA),
    .axi_o$memory$AWLEN   (S0_AXI_AWLEN),
    .axi_o$memory$AWSIZE  (S0_AXI_AWSIZE),
    .axi_o$memory$AWBURST (S0_AXI_AWBURST),
    .axi_o$memory$WSTRB   (S0_AXI_WSTRB),
    .axi_o$memory$WLAST   (S0_AXI_WLAST),
    .axi_o$memory$BREADY  (S0_AXI_BREADY),

    // Reads (Request)
    .axi_o$memory$ARVALID (S0_AXI_ARVALID),
    .axi_o$memory$ARID    (S0_AXI_ARID),
    .axi_o$memory$ARADDR  (S0_AXI_ARADDR),
    .axi_o$memory$ARLEN   (S0_AXI_ARLEN),
    .axi_o$memory$ARSIZE  (S0_AXI_ARSIZE),
    .axi_o$memory$ARBURST (S0_AXI_ARBURST),
    .axi_o$memory$RREADY  (S0_AXI_RREADY)
);

assign uart_tx = cpu.uart_tx;

assign vout_data = cpu.video_out$video_data$vdata;
assign vout_hs = cpu.video_out$video_signals$h_sync;
assign vout_vs = cpu.video_out$video_signals$v_sync;
assign vout_de = cpu.video_out$video_signals$video_active;

// AXI4 Cache (TODO: Replace this with an internal one)

// Writes (Request)
wire          m_axi_wvalid;
wire          m_axi_awvalid;
wire [31:0]   m_axi_awaddr_full;    
wire [29:0]   m_axi_awaddr;    
wire [127:0]  m_axi_wdata;     // Data Width 128
wire [7:0]    m_axi_awlen;
wire [2:0]    m_axi_awsize;
wire [1:0]    m_axi_awburst;
wire          m_axi_awlock;
wire [3:0]    m_axi_awcache;
wire [2:0]    m_axi_awprot;
wire [3:0]    m_axi_awqos;
wire [15:0]   m_axi_wstrb;     // 128-bit data / 8 = 16 strobe bits
wire          m_axi_wlast;
wire          m_axi_bready;
wire          m_axi_awid;

// Reads (Request)
wire          m_axi_arvalid;
wire [31:0]   m_axi_araddr_full;   
wire [29:0]   m_axi_araddr;     
wire [7:0]    m_axi_arlen;
wire [2:0]    m_axi_arsize;
wire [1:0]    m_axi_arburst;
wire [3:0]    m_axi_arqos;
wire [2:0]    m_axi_arprot;
wire [3:0]    m_axi_arcache;
wire          m_axi_arlock;
wire          m_axi_rready;
wire          m_axi_arid;

// Read response assignments
wire m_arready;
wire m_rlast;
wire[1:0] m_rresp;
wire[127:0] m_rdata;
wire m_rid;
wire m_rvalid;

// Write Response Assignments
wire[1:0] m_bresp;
wire m_bid;
wire m_wready;
wire m_awready;
wire m_bvalid;

// Writes (Request)
assign S0_AXI_AWLOCK  = 1'b0;
assign S0_AXI_AWCACHE = 4'b1111;
assign S0_AXI_AWPROT  = 3'b00;
assign S0_AXI_AWQOS   = 4'b0000;

// Reads (Request)
assign S0_AXI_ARQOS   = 4'b0000;
assign S0_AXI_ARPROT  = 3'b00;
assign S0_AXI_ARCACHE = 4'b1111;
assign S0_AXI_ARLOCK  = 1'b0;

assign m_axi_awaddr = m_axi_awaddr_full[29:0];
assign m_axi_araddr = m_axi_araddr_full[29:0];

axi4_cache u_axi4cache
(
    .ACLK(hart_clock),
    .ARESETN(rst_n),

    // Writes (Request)
    .S0_AXI_WVALID  (S0_AXI_WVALID),
    .S0_AXI_AWVALID (S0_AXI_AWVALID),
    .S0_AXI_AWID    (S0_AXI_AWID),
    .S0_AXI_AWADDR  (S0_AXI_AWADDR),
    .S0_AXI_WDATA   (S0_AXI_WDATA),
    .S0_AXI_AWLEN   (S0_AXI_AWLEN),
    .S0_AXI_AWSIZE  (S0_AXI_AWSIZE),
    .S0_AXI_AWBURST (S0_AXI_AWBURST),
    .S0_AXI_AWLOCK  (S0_AXI_AWLOCK),
    .S0_AXI_AWCACHE (S0_AXI_AWCACHE),
    .S0_AXI_AWPROT  (S0_AXI_AWPROT),
    .S0_AXI_AWQOS   (S0_AXI_AWQOS),
    .S0_AXI_WSTRB   (S0_AXI_WSTRB),
    .S0_AXI_WLAST   (S0_AXI_WLAST),
    .S0_AXI_BREADY  (S0_AXI_BREADY),

    // Reads (Request)
    .S0_AXI_ARVALID (S0_AXI_ARVALID),
    .S0_AXI_ARID    (S0_AXI_ARID),
    .S0_AXI_ARADDR  (S0_AXI_ARADDR),
    .S0_AXI_ARLEN   (S0_AXI_ARLEN),
    .S0_AXI_ARSIZE  (S0_AXI_ARSIZE),
    .S0_AXI_ARBURST (S0_AXI_ARBURST),
    .S0_AXI_ARQOS   (S0_AXI_ARQOS),
    .S0_AXI_ARPROT  (S0_AXI_ARPROT),
    .S0_AXI_ARCACHE (S0_AXI_ARCACHE),
    .S0_AXI_ARLOCK  (S0_AXI_ARLOCK),
    .S0_AXI_RREADY  (S0_AXI_RREADY),
    
    // Reads (Response)
    .S0_AXI_RLAST(rlast),
    .S0_AXI_RRESP(rresp),
    .S0_AXI_RDATA(rdata),
    .S0_AXI_RID(rid),
    .S0_AXI_RVALID(rvalid),
    .S0_AXI_ARREADY(arready),

    // Writes (Response)
    .S0_AXI_BRESP(bresp),
    .S0_AXI_BID(bid),
    .S0_AXI_BVALID(bvalid),
    .S0_AXI_AWREADY(awready),
    .S0_AXI_WREADY(wready),

    // Read response assignments
    .M0_AXI_ARREADY(m_arready),
    .M0_AXI_RLAST(m_rlast),
    .M0_AXI_RRESP(m_rresp),
    .M0_AXI_RDATA(m_rdata),
    .M0_AXI_RID(m_rid),
    .M0_AXI_RVALID(m_rvalid),

    // Write Response Assignments
    .M0_AXI_BRESP(m_bresp),
    .M0_AXI_BID(m_bid),
    .M0_AXI_WREADY(m_wready),
    .M0_AXI_AWREADY(m_awready),
    .M0_AXI_BVALID(m_bvalid),
    
     // Writes (Request)
    .M0_AXI_WVALID  (m_axi_wvalid),
    .M0_AXI_AWVALID (m_axi_awvalid),
    .M0_AXI_AWADDR  (m_axi_awaddr_full),
    .M0_AXI_WDATA   (m_axi_wdata),
    .M0_AXI_AWLEN   (m_axi_awlen),
    .M0_AXI_AWSIZE  (m_axi_awsize),
    .M0_AXI_AWBURST (m_axi_awburst),
    .M0_AXI_AWLOCK  (m_axi_awlock),
    .M0_AXI_AWCACHE (m_axi_awcache),
    .M0_AXI_AWPROT  (m_axi_awprot),
    .M0_AXI_AWQOS   (m_axi_awqos),
    .M0_AXI_WSTRB   (m_axi_wstrb),
    .M0_AXI_WLAST   (m_axi_wlast),
    .M0_AXI_BREADY  (m_axi_bready),
    .M0_AXI_AWID    (m_axi_awid),
    
    // Reads (Request)
    .M0_AXI_ARVALID (m_axi_arvalid),
    .M0_AXI_ARADDR  (m_axi_araddr_full),
    .M0_AXI_ARLEN   (m_axi_arlen),
    .M0_AXI_ARSIZE  (m_axi_arsize),
    .M0_AXI_ARBURST (m_axi_arburst),
    .M0_AXI_ARQOS   (m_axi_arqos),
    .M0_AXI_ARPROT  (m_axi_arprot),
    .M0_AXI_ARCACHE (m_axi_arcache),
    .M0_AXI_ARLOCK  (m_axi_arlock),
    .M0_AXI_RREADY  (m_axi_rready),
    .M0_AXI_ARID    (m_axi_arid)
);

// Memory instantiation

ddr3 u_ddr3
(
    .sys_clk_i                      (memory_reference_clock),
    .sys_rst                        (rst_n),
    .aresetn                        (rst_n),
    .app_sr_req(1'b0),
    .app_ref_req(1'b0),
    .app_zq_req(1'b0),

    // UI clocks
    .ui_clk                         (clk                    ),
    .ui_clk_sync_rst                (rst                    ),

    // Memory interface ports
    .ddr3_addr                      (ddr3_addr              ),
    .ddr3_ba                        (ddr3_ba                ),
    .ddr3_cas_n                     (ddr3_cas_n             ),
    .ddr3_ck_n                      (ddr3_ck_n              ),
    .ddr3_ck_p                      (ddr3_ck_p              ),
    .ddr3_cke                       (ddr3_cke               ),
    .ddr3_ras_n                     (ddr3_ras_n             ),
    .ddr3_we_n                      (ddr3_we_n              ),
    .ddr3_dq                        (ddr3_dq                ),
    .ddr3_dqs_n                     (ddr3_dqs_n             ),
    .ddr3_dqs_p                     (ddr3_dqs_p             ),
    .ddr3_reset_n                   (ddr3_reset_n           ),
    .init_calib_complete            (init_calib_complete    ),
    .ddr3_cs_n                      (ddr3_cs_n              ),
    .ddr3_dm                        (ddr3_dm                ),
    .ddr3_odt                       (ddr3_odt               ),

    // Writes (Request)
    .s_axi_wvalid   (m_axi_wvalid),
    .s_axi_awvalid  (m_axi_awvalid),
    .s_axi_awid     (m_axi_awid),
    .s_axi_awaddr   (m_axi_awaddr),
    .s_axi_wdata    (m_axi_wdata),
    .s_axi_awlen    (m_axi_awlen),
    .s_axi_awsize   (m_axi_awsize),
    .s_axi_awburst  (m_axi_awburst),
    .s_axi_awlock   (m_axi_awlock),
    .s_axi_awcache  (m_axi_awcache),
    .s_axi_awprot   (m_axi_awprot),
    .s_axi_awqos    (m_axi_awqos),
    .s_axi_wstrb    (m_axi_wstrb),
    .s_axi_wlast    (m_axi_wlast),
    .s_axi_bready   (m_axi_bready),
    
    // Reads (Request)
    .s_axi_arvalid  (m_axi_arvalid),
    .s_axi_arid     (m_axi_arid),
    .s_axi_araddr   (m_axi_araddr),
    .s_axi_arlen    (m_axi_arlen),
    .s_axi_arsize   (m_axi_arsize),
    .s_axi_arburst  (m_axi_arburst),
    .s_axi_arqos    (m_axi_arqos),
    .s_axi_arprot   (m_axi_arprot),
    .s_axi_arcache  (m_axi_arcache),
    .s_axi_arlock   (m_axi_arlock),
    .s_axi_rready   (m_axi_rready),
    
    // Reads (Response)
    .s_axi_rlast(m_rlast),
    .s_axi_rresp(m_rresp),
    .s_axi_rdata(m_rdata),
    .s_axi_rid(m_rid),
    .s_axi_rvalid(m_rvalid),
    .s_axi_arready(m_arready),
    
    // Writes (Response)
    .s_axi_bresp(m_bresp),
    .s_axi_bid(m_bid),
    .s_axi_bvalid(m_bvalid),
    .s_axi_awready(m_awready),
    .s_axi_wready(m_wready)
);

assign hart_clock = clk;

// HDMI Initialization (Without these the screen won't come up even with a video signal)

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


assign clear = ~(done_9134);

assign led1 = rst_n;
assign led2 = ~uart_rx;
assign led3 = ~cpu.uart_rx_valid;

assign vout_clk = video_clock;
assign hdmi_nreset = locked;

endmodule
