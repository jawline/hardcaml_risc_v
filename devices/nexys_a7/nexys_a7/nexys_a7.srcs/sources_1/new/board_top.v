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
wire bvalid;
    
// TODO: Async fifo the memory to cross into the CPU clock domain rather than tethering the CPU to the memory ui clock
board_clocks clocks(.reset(~rst_n), .board_clock(board_clock), .io_delay_ref_clock(io_delay_ref_clock), .memory_clock(memory_clock));

wire cpu_rst;

top cpu (
.clock(cpu_clock),
.clear(cpu_rst),
.uart_rx(uart_rx),
.uart_tx(uart_tx),
.uart_rx_valid(uart_rx_valid)//,

//// Read response assignments
//.axi_i$memory$rready(rready),
//.axi_i$memory$rlast(rlast),
//.axi_i$memory$rresp(rresp),
//.axi_i$memory$rdata(rdata),
//.axi_i$memory$rid(rid),
//.axi_i$memory$rvalid(rvalid),
//
//// Write Response Assignments
//.axi_i$memory$bresp(bresp),
//.axi_i$memory$bid(bid),
//.axi_i$memory$wready(wready),
//.axi_i$memory$bvalid(bvalid),
//
// // Write requests
//.axi_o$memory$awvalid(axi_o$memory$awvalid),
//.axi_o$memory$awid(axi_o$memory$awid),
//.axi_o$memory$awaddr(axi_o$memory$awaddr),
//.axi_o$memory$wdata(axi_o$memory$wdata),
//.axi_o$memory$wstrb(axi_o$memory$wstrb),
//.axi_o$memory$wlast(axi_o$memory$wlast),
//
//// Read Requests
//.axi_o$memory$arvalid(axi_o$memory$arvalid),
//.axi_o$memory$arid(axi_o$memory$arid),
//.axi_o$memory$araddr(axi_o$memory$araddr),
//.axi_o$memory$rready(axi_o$memory$rready)
);

// Write response assignments
ddr_memory main_memory (
.aresetn(rst_n),
.sys_rst(~rst_n),
.app_sr_req(1'b0),
.app_ref_req(1'b0),
.app_zq_req(1'b0),
.sys_clk_i(memory_clock),
.clk_ref_i(io_delay_ref_clock),
.ui_clk(cpu_clock),
.ui_clk_sync_rst(cpu_rst),

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
.s_axi_wvalid(axi_o$memory$awvalid),
.s_axi_awvalid(axi_o$memory$awvalid),
.s_axi_awid(axi_o$memory$awid),
.s_axi_awaddr({axi_o$memory$awaddr, {2'b0}}),
.s_axi_wdata(axi_o$memory$wdata),
.s_axi_awlen(7'b0000001),
.s_axi_awsize(3'b010),
.s_axi_awburst(2'b00),
.s_axi_awlock(1'b0),
.s_axi_awcache(4'b0011),
.s_axi_awprot(4'b0000),
.s_axi_awqos(4'b0000),
.s_axi_wstrb(axi_o$memory$wstrb),
.s_axi_wlast(axi_o$memory$wlast),
.s_axi_bready(axi_o$memory$rready),

 // Reads (Request) main_memory.s_axi_bid
.s_axi_arvalid(axi_o$memory$arvalid),
.s_axi_arid(axi_o$memory$arid),
.s_axi_araddr({axi_o$memory$araddr, {2'b0}}),
.s_axi_arlen(7'b0000001),
.s_axi_arsize('b010),
.s_axi_arburst(2'b00),
.s_axi_arqos(4'b0000),
.s_axi_arprot(4'b0000),
.s_axi_arcache(4'b0011),
.s_axi_arlock(1'b0),
.s_axi_rready(axi_o$memory$rready),

// Reads (Response)
.s_axi_rlast(rlast),
.s_axi_rresp(rresp),
.s_axi_rdata(rdata),
.s_axi_rid(rid),
.s_axi_rvalid(rvalid),

// Writes (Response)
.s_axi_bresp(bresp),
.s_axi_bid(bid),
.s_axi_bvalid(bvalid)
);

// Read response assignments
assign rready = 1'b1; //(main_memory.s_axi_arready & main_memory.s_axi_rready);

// Write Response Assignments
assign wready = 1'b1; // (main_memory.s_axi_awready & main_memory.s_axi_wready);

assign led1 = rst_n;
assign led2 = uart_rx;
assign led3 = uart_tx;
assign led4 = uart_rx_valid;

endmodule
