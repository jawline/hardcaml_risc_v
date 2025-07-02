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

wire memory_clock;
wire cpu_clock;
wire io_delay_ref_clock;

// Read response assignments
wire rready;
wire rlast;
wire[1:0] rresp;
wire[31:0] rdata;
wire[7:0] rid;
wire rvalid;

// Write Response Assignments
wire[1:0] bresp;
wire[7:0] bid;
wire wready;
wire awready;
wire bvalid;

wire[24:0] axi_o$memory$awaddr;
wire[24:0] axi_o$memory$araddr;
wire [7:0] axi_o$memory$awid;
wire [7:0] axi_o$memory$arid;
wire [31:0] axi_o$memory$wdata;
wire [3:0] axi_o$memory$wstrb;
    
// TODO: Async fifo the memory to cross into the CPU clock domain rather than tethering the CPU to the memory ui clock
board_clocks clocks(.reset(~rst_n), .board_clock(board_clock), .io_delay_ref_clock(io_delay_ref_clock), .memory_clock(memory_clock));

wire cpu_rst;

top cpu (
.clock(cpu_clock),
.clear(cpu_rst),
.uart_rx(uart_rx),
.uart_tx(uart_tx),

//// Read response assignments
.axi_i$memory$RREADY(rready),
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

 // Write requests
.axi_o$memory$AWVALID(axi_o$memory$AWVALID),
.axi_o$memory$WVALID(axi_o$memory$WVALID),
.axi_o$memory$AWID(axi_o$memory$AWID),
.axi_o$memory$AWADDR(axi_o$memory$AWADDR),
.axi_o$memory$WDATA(axi_o$memory$WDATA),
.axi_o$memory$WSTRB(axi_o$memory$WSTRB),
.axi_o$memory$AWLEN(axi_o$memory$AWLEN),
.axi_o$memory$AWSIZE(axi_o$memory$AWSIZE),
.axi_o$memory$AWBURST(axi_o$memory$AWBURST),
.axi_o$memory$WLAST(axi_o$memory$WLAST),

// Read Requests
.axi_o$memory$ARVALID(axi_o$memory$ARVALID),
.axi_o$memory$ARID(axi_o$memory$ARID),
.axi_o$memory$ARLEN(axi_o$memory$ARLEN),
.axi_o$memory$ARSIZE(axi_o$memory$ARSIZE),
.axi_o$memory$ARBURST(axi_o$memory$ARBURST),
.axi_o$memory$ARADDR(axi_o$memory$ARADDR),
.axi_o$memory$RREADY(axi_o$memory$RREADY),
.axi_o$memory$BREADY(axi_o$memory$BREADY)
);

// Write response assignments
ddr_memory main_memory (
.aresetn(rst_n),
.sys_rst(rst_n),
.app_sr_req(1'b0),
.app_ref_req(1'b0),
.app_zq_req(1'b0),
.sys_clk_i(memory_clock),
.clk_ref_i(io_delay_ref_clock),
.ui_clk(cpu_clock),
.ui_clk_sync_rst(cpu_rst),

.init_calib_complete(init_calib_complete),

 // DDR2 interface
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
.ddr2_cs_n(ddr2_cs_n),
.ddr2_dm(ddr2_dm),
.ddr2_odt(ddr2_odt),

// Writes (Request)
.s_axi_wvalid(axi_o$memory$WVALID),
.s_axi_awvalid(axi_o$memory$AWVALID),
.s_axi_awid(axi_o$memory$AWID),
.s_axi_awaddr(axi_o$memory$AWADDR),
.s_axi_wdata(axi_o$memory$WDATA),
.s_axi_awlen(axi_o$memory$AWLEN),
.s_axi_awsize(axi_o$memory$AWSIZE),
.s_axi_awburst(axi_o$memory$AWBURST),
.s_axi_awlock(1'b0),
.s_axi_awcache(4'b0011),
.s_axi_awprot(4'b0000),
.s_axi_awqos(4'b0000),
.s_axi_wstrb(axi_o$memory$WSTRB),
.s_axi_wlast(axi_o$memory$WLAST),
.s_axi_bready(axi_o$memory$BREADY),

.s_axi_arvalid(axi_o$memory$ARVALID),
.s_axi_arid(axi_o$memory$ARID),
.s_axi_araddr(axi_o$memory$ARADDR),
.s_axi_arlen(axi_o$memory$ARLEN),
.s_axi_arsize(axi_o$memory$ARSIZE),
.s_axi_arburst(axi_o$memory$ARBURST),
.s_axi_arqos(4'b0000),
.s_axi_arprot(4'b0000),
.s_axi_arcache(4'b0011),
.s_axi_arlock(1'b0),
.s_axi_rready(axi_o$memory$RREADY),

// Reads (Response)
.s_axi_rlast(rlast),
.s_axi_rresp(rresp),
.s_axi_rdata(rdata),
.s_axi_rid(rid),
.s_axi_rvalid(rvalid),
.s_axi_arready(s_axi_arready),

// Writes (Response)
.s_axi_bresp(bresp),
.s_axi_bid(bid),
.s_axi_bvalid(bvalid),
.s_axi_awready(s_axi_awready),
.s_axi_wready(s_axi_wready)
);

// Read response assignments
assign rready = (s_axi_arready);

// Write Response Assignments
assign wready = s_axi_wready;
assign awready = s_axi_awready;

assign led1 = init_calib_complete;
assign led2 = axi_o$memory$awvalid;
assign led3 = s_axi_awready;
assign led4 = s_axi_wready;

endmodule
