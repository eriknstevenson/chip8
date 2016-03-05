# Chip-8 Interpreter

A Chip-8 interpreter based on the specifications found [here](http://devernay.free.fr/hacks/chip8/C8TECH10.HTM#1.0).

## What's Chip-8?

> Chip-8 is a simple, interpreted, programming language which was first used on some do-it-yourself computer systems in the late 1970s and early 1980s. The COSMAC VIP, DREAM 6800, and ETI 660 computers are a few examples. These computers typically were designed to use a television as a display, had between 1 and 4K of RAM, and used a 16-key hexadecimal keypad for input. The interpreter took up only 512 bytes of memory, and programs, which were entered into the computer in hexadecimal, were even smaller.

## Building

Building is straightforward using [stack](https://github.com/commercialhaskell/stack).

    $ git clone https://github.com/narrative/chip8
    $ cd chip8
    $ stack build

**Building on Windows**

A few extra steps are required to get working on Windows due to the project's  ```sdl2``` dependency.

1. Install [msys2](https://msys2.github.io/)
2. Open one of the provided ```mingw``` shells.
3. Update system packages with ```update-core```. You may be prompted to restart the shell following completion of the update.
4. Install ```pkg-config``` and ```sdl2``` with pacman: ```pacman -S mingw-w64-x86_64-pkg-config mingw-w64-x86_64-SDL2```
5. Build with stack: ```stack --skip-msys build```
6. Subsequent builds can be performed from any shell assuming ```sdl2``` has now been built successfully.

## Issues

* Nothing has been implemented yet.
* This is currently an empty repository.
