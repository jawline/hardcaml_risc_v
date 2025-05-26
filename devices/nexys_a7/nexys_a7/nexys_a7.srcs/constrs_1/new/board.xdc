set_property CFGBVS VCCO [current_design]
set_property CONFIG_VOLTAGE 3.3 [current_design]

set_property BITSTREAM.CONFIG.SPI_BUSWIDTH 4 [current_design] 
set_property CONFIG_MODE SPIx4 [current_design] 
set_property BITSTREAM.CONFIG.CONFIGRATE 50 [current_design] 

set_property IOSTANDARD LVCMOS33 [get_ports sys_clock]
set_property PACKAGE_PIN E3 [get_ports sys_clock]

set_property IOSTANDARD LVCMOS33 [get_ports rst_n]
set_property PACKAGE_PIN C12 [get_ports rst_n]

set_property IOSTANDARD LVCMOS33 [get_ports uart_rx]
set_property PACKAGE_PIN C4 [get_ports uart_rx]

set_property IOSTANDARD LVCMOS33 [get_ports uart_tx]
set_property PACKAGE_PIN D4 [get_ports uart_tx]

set_property IOSTANDARD LVCMOS33 [get_ports led1]
set_property PACKAGE_PIN H17 [get_ports led1]

set_property IOSTANDARD LVCMOS33 [get_ports led2]
set_property PACKAGE_PIN K15 [get_ports led2]

set_property IOSTANDARD LVCMOS33 [get_ports led3]
set_property PACKAGE_PIN J13 [get_ports led3]

set_property IOSTANDARD LVCMOS33 [get_ports led4]
set_property PACKAGE_PIN N14 [get_ports led4]