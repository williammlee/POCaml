# POKEMON GAME INSTALLATION GUIDE

## GUI Interface

### Mac
https://www.xquartz.org/

### Ubuntu
https://sourceforge.net/projects/xming/

## Package Installation
This project requires that you use opam to install the following packages:
- ounit
- yojson
- graphics
- camlimages
- sdl
- conf-sdl-mixer

Use the following command to install all the packages

```opam install -y ounit yojson graphics camlimages sdl conf-sdl-mixer```

NOTE: to install sdl and conf-sdl-mixer, you must install libsdl and 
libsdl_mixer

A command that works with MacPorts:
sudo port install libsdl libsdl_mixer

## Makefile Commands
Upon installing, run `make build` to ensure other commands work.
- `make test` runs the test suite
- `make battletest` runs the battle test simulation
- `make play` runs the entire game

## In-game Commands
Move with wasd
Close the game with q
Increase/decrease the size of window with + or -
Press e to enter an option and r to return between menus
