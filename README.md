# Hardcaml_risc_v

This project implements a complete Risc-V based system in Hardcaml. The project
includes a Risc-V core, IO system with DMA support, and video signal generator
from a hardware upscaled framebuffer.

The project has been tested on Artix-7 FPGAs but is simple enough that it
should be broadly compatible with most FPGAs. It uses block RAM for memory, and
the amount of available memory can be configured during RTL generation.


## Setting up a development environment

To compile a design you will need an ocaml environment. To compile the test
programs we use in regression tests and examples, you will also need a C and
Rust cross compiler.

### Setting up an Ocaml environment 

If you have opam installed, you can set up a compatible environment with:
```
opam init -y
opam switch create 5.2.0 --repos upstream=git+https://github.com/janestreet/opam-repository.git,default
eval $(opam env --switch 5.2.0)
opam update -y
opam install -y dune core async hardcaml hardcaml_waveterm hardcaml_test_harness hardcaml_axi ppx_hardcaml ppx_deriving hardcaml_xilinx_reports
```

### Setting up a GCC cross compiler

To build some of the test programs you will need a rv32i cross compiler. You can build one using the instructions below (the export PATH line is best added to a .bashrc/.zshrc).

```
git clone git@github.com:riscv-collab/riscv-gnu-toolchain.git
cd riscv-gnu-toolchain
./configure --prefix="$HOME/gcc-riscv/" -with-arch=rv32i -with-abi=rv32i
export PATH=$PATH:$HOME/gcc-riscv/bin/
```

### Setting up a Rust cross compiler

To build some of the test programs you will need a rv32i Rust compiler. If you have rustup installed, this is pretty trivial with:

```
rustup target add riscv32i-unknown-none-elf
rustup component add llvm-tools
cargo install cargo-binutils
```

## Getting Started

To build this project, you will need Hardcaml from the Janestreet bleeding
repository opam repository as well as a gcc and rust cross compiler for the
test applications (instructions below).

If the installation is successful, then the follow command will run all the
tests and should pass:
```
dune runtest
```

Once that is set up, you can generate the RTL for a CPU with Uart at an assumed
100Mhz clock using:
```
dune exec ./bin/generate_cpu.exe -- generate-rtl -include-video-out false  -hart-frequency 100_000_000
```

From there, you will need to generate a top file for your board and then
compile and flash it using your board specific tooling. Once a design is
programmed onto your FPGA you can follow the instructions below to compile and
run one of the test programs.

## Running a program

Once your device is programmed and a UART is attached to your host machine you
can use the provided program_cpu binary.

### Running Hello World

We can run the provided hello world example program by executing:
```
dune build ./test-programs/test-c-programs/hello_world_c/hello_world.o # Compile our example program
dune exec ./bin/program_cpu.exe -- /dev/ttyUSB0 ./_build/default/test-programs/test-c-programs/hello_world_c/hello_world.bin # Program the device and send a clear signal, then listen for replies (replace ttyUSB0 with your uart device)
```

The programmer will set the stty settings to the settings specified in
`bin/uart_settings.ml`. These are the same settings used by `generate_cpu.exe`
so they should match the image you just programmed.

### Running your own programs

The key things we need to do to run a program on our CPU are to:
* Compile it with the program starting at 0x0 (this is the address our CPU clears to)
* Compile it with a rv32i cross compiler

To compile and run a custom program you can start by coping the hello_world
programs for Rust or C at `./test-programs/test-rust-programs/hello-world-rust`
and `./test-programs/test-c-programs/hello_world_c`.

For a new Rust program, the key differences from a normal compilation are in
`.cargo/config.toml` which specifies our cross compiler and linking rules, and
`ld/memory.ld`, which specifies our link locations. The compile script uses
cargo to build the project and then runs `objcopy` to get a raw binary image.
We also have a small assembly file `entry.s` which is our program entry point
and sets up initial register start before jumping to the Rust program start.
Other than that, development is pretty much as normal though you cannot use the
`std` crate.

For a new C program, the `compile` script invokes the cross compiler and then
uses objcopy to produce a raw binary from an elf file. For linking the
`link.ld` file is specified to make sure the program code starts at 0x0. As
with the Rust program, a small assembly file `entry.s` puts the registers into
a known state before jumping to our C entry point.
