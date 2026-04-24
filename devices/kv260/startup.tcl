connect
targets -set -filter {name =~ "PSU"}
puts "Reset"
# Force JTAG boot mode and reset
mwr 0xff5e0200 0x0
rst -system
puts "Sleep reset"
after 10000

puts "Source psu init"
source psu_init.tcl

puts "Psu init"
psu_init
after 10000

puts "Post checks"
psu_post_config
after 10000
psu_ps_pl_isolation_removal
after 10000
psu_ps_pl_reset_config
after 10000

puts "Psu post config"
psu_post_config
after 10000
