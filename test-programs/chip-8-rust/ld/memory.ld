MEMORY {
    RAM (rwx) : ORIGIN = 0x0, LENGTH = 32K
}

ENTRY(_start)

SECTIONS {
    .init : { *(.init) } > RAM
    .text : { *(.text .text.*) } > RAM
    .rodata : { *(.rodata) } > RAM
    .data : { *(.data) } > RAM
    . = ALIGN(4);
}

_stack = 0x4000;
