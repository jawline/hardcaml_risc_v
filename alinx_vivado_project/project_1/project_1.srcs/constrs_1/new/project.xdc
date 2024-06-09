set_property IOSTANDARD DIFF_SSTL15 [get_ports sys_clock_p]

set_property IOSTANDARD DIFF_SSTL15 [get_ports sys_clock_n]
set_property PACKAGE_PIN R4 [get_ports sys_clock_p]
set_property PACKAGE_PIN T4 [get_ports sys_clock_n]

set_property IOSTANDARD LVCMOS33 [get_ports reset_btn]
set_property PACKAGE_PIN F15 [get_ports reset_btn]

set_property IOSTANDARD LVCMOS33 [get_ports uart_rx]
set_property PACKAGE_PIN L14 [get_ports uart_rx]

set_property IOSTANDARD LVCMOS33 [get_ports uart_tx]
set_property PACKAGE_PIN L15 [get_ports uart_tx]

set_property IOSTANDARD LVCMOS33 [get_ports led1]
set_property PACKAGE_PIN L13 [get_ports led1]

set_property IOSTANDARD LVCMOS33 [get_ports led2]
set_property PACKAGE_PIN M13 [get_ports led2]

set_property IOSTANDARD LVCMOS33 [get_ports led3]
set_property PACKAGE_PIN K14 [get_ports led3]

set_property IOSTANDARD LVCMOS33 [get_ports led4]
set_property PACKAGE_PIN K13 [get_ports led4]

create_clock -period 10.417 -name VIRTUAL_clk_out1_clk_wiz_0 -waveform {0.000 5.208}


set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 1.000 [get_ports uart_tx]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 5.000 [get_ports uart_tx]
set_input_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 1.000 [get_ports uart_rx]
set_input_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 5.000 [get_ports uart_rx]

set_input_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 0.000 [get_ports reset_btn]
set_input_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 2.000 [get_ports reset_btn]

set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 0.000 [get_ports led1]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 0.000 [get_ports led1]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 0.000 [get_ports led2]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 0.000 [get_ports led2]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 0.000 [get_ports led3]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 0.000 [get_ports led3]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -min -add_delay 0.000 [get_ports led4]
set_output_delay -clock [get_clocks VIRTUAL_clk_out1_clk_wiz_0] -max -add_delay 0.000 [get_ports led4]