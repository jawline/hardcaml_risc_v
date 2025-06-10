# Hardcaml_risc_v

This project implements a complete Risc-V based system in Hardcaml. The project
includes a Risc-V core, IO system with DMA support, and video signal generator
from a hardware upscaled framebuffer.

The project has been tested on Artix-7 FPGAs but is simple enough that it
should be broadly compatible with most FPGAs. It uses block RAM for memory, and
the amount of available memory can be configured during RTL generation.

## Getting Started

To build this project, you will need Hardcaml from the Janestreet bleeding repository opam repository as well as a gcc and rust cross compiler for the test applications (instructions below).

If the installation is successful, then the follow command will run all the tests and should pass:
```
dune runtest
```

Once that is set up, you can generate the RTL for a CPU with Uart at an assumed 100Mhz clock using:
```
dune exec ./bin/generate_cpu.exe -- generate-rtl -include-video-out false  -hart-frequency 100_000_000
```

From there, you will need to generate a top file for your board.

## Setting up an Ocaml environment 

If you have opam installed, you can set up a compatible environment with:
```
opam init -y
opam switch create 5.2.0 --repos upstream=git+https://github.com/janestreet/opam-repository.git,default
eval $(opam env --switch 5.2.0)
opam update -y
opam install -y dune core async hardcaml hardcaml_waveterm hardcaml_test_harness hardcaml_axi ppx_hardcaml ppx_deriving hardcaml_xilinx_reports
```

## Setting up a GCC cross compiler

To build some of the test programs you will need a rv32i cross compiler. You can build one using the instructions below (the export PATH line is best added to a .bashrc/.zshrc).

```
git clone git@github.com:riscv-collab/riscv-gnu-toolchain.git
cd riscv-gnu-toolchain
./configure --prefix="$HOME/gcc-riscv/" -with-arch=rv32i -with-abi=rv32i
export PATH=$PATH:$HOME/gcc-riscv/bin/
```

## Setting up a Rust cross compiler

To build some of the test programs you will need a rv32i Rust compiler. If you have rustup installed, this is pretty trivial with:

```
rustup target add riscv32i-unknown-none-elf
rustup component add llvm-tools
cargo install cargo-binutils
```
