# Doctor Frown

This repository is the game Doctor Frown, for the Atari 2600. It is written in Assembly Language, utilising the 6502 processor instruction set. It can be compiled using the DASM Compiler, and can be played an any Atari 2600 emulator.

## Important Notes

To compile this source code, the DASM Compiler (or any equivalent) is required to be installed.

To play on PC, an Atari 2600 emulator is required; Stella is a well documented and well trusted option.

## Installation

Pull or Download the Repository to your local machine.

To compile the source code, run the following command in the root directory.

```bash
dasm *.asm -f3 -v0 -o"cart.bin" -l"cart.lst" -s"cart.sym"
```

Three files will be generated.

Load the cart.bin file into any Atari 2600 emulator to play.

## Game Description

Help! The evil Doctor Frown has captured all the Smiles in the Kingdom! Now they're trying to escape, and they need your help! But be careful, as Doctor Frown will stop at nothing to hunt you down!

Doctor Frown is a score based collecting game. Gameplay consists of two rounds which repeat. The first round has the player collecting smiles which drop from the top of the screen. Once the player has collected 5 smiles in that round, the enemy Doctor Frown appears and will chase the player. If the player survives for the countdown, gameplay returns to the first round. The goal of the game is to collect as many points as possible before being caught by Doctor Frown. The Score is displayed on the left, and the enemy countdown is displayed on thr right.

## Further Reading

[Atari 2600 Documentation](https://www.qotile.net/minidig/docs/stella.pdf)

This resource is a PDF Document detailing specifics of the 6507 Processor and TIA Chip of the Atari 2600.

[6502 Opcodes](http://www.6502.org/tutorials/6502opcodes.html)

A resource detailing the opcodes available for the 6502, including clock cycles for each one.

[Atari 2600 Color Palette](https://en.wikipedia.org/wiki/List_of_video_game_console_palettes#Atari_2600)

Wikipedia page detailing the color palette for the Atari 2600; useful for finding hex values for colors.
