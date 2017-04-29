# Making Games for the Atari 2600 by Steven Hugg, CreateSpace (2016)

## 1. Introduction to 6502

16-bit address bus, three general-purpose registers (A, X, Y), little-endian<br>
addressing modes:
* absolute indexed - 8-bit value + 16-bit constant
* zero page mode - only 8-bit values, the memory location $00-$FF which get
                   special treatment, generate smaller code, most of VCS
                   registers live in zero-page space

condition flags - Z (Zero), N (Negative/Sign), C (Carry, set by compare, add,
 substract, and shift operations), V (Overflow)<br>
The branch instructions can only modify th PC by -128 to +127 bytes, so for
 longer distances you'll need `JMP`.<br>
`BCC` (**carry clear, less than**, unsigned), `BCS` (carry set, greater than or
 equal, unsigned), `BMI` (minus, negative set, less than, signed), `BPL` (plus,
 negative clear, greater than or equal, signed)<br>
For subtraction, it must be set first using `SEC` (Set Carry Flag). The
 increment/decrement instructions modify the Negative and Zero flags, while the
 addition/subtraction additionally modify the Carry flag.<br>
the top of the stack is a memory location stored in the S (Stack pointer)
 register. It usually starts at `$FF`.<br>
`BIT` - Same as AND, but just set flags and throw away the result<br>
`ROL` and `ROR` rotates bits via Carry flag.

## 2. The 8butworkshop IDE

[8bitworkshop][8bitworkshop] uses [Javatari][javatari] emulator and [DASM][dasm]
 assembler.

[8bitworkshop]: http://8bitworkshop.com/
[javatari]: http://javatari.org/
[dasm]: http://www.macs.hw.ac.uk/~pjbk/scholar/dasm.html

## 3. VCS Memory Map

TIA (Television Interface Adapter), PIA (Peripheral Interface Adapter), ROM<br>
`$00-$7F` TIA Registers, `$80-$FF` PIA RAM, `$280-$297` PIA Ports and Timer,
 `$F000-$FFFF` Cartridge ROM<br>
`ds` (reserve bytes), `include`, `org`, `equ`, `seg.u` (uninitialized segment),
 `.byte`, `.word`, `seg`<br>
because the VCS only has 13 address pins, and only recognizes 8,192 ($200)
 unique addresses, you could actually declare the origin ($f000) as $1000,
 $3000, $5000, etc.

## 4. Writing Your First Assembly Code

## 5. Painting on the CRT

[Stella Programmer's Guide][stella_programmers_guide]<br>
The microprocessor's clock is the 3.58 MHz oscillator (that generates clock
 pulses called "color clocks") divided by 3.<br>
HORIZONTAL BLANK (HBLANK, 68 color clocks, 22 machine cycles), VISIBLE SCANLINE
 (160 color clocks, 53 machine cycles)
1. Strobe (write to) `WSYNC` (Wait for SYNC) to halt the CPU until the next
   scanline starts.
1. During the initial `HBLANK` period, write whatever TIA registers are needed
   to draw this scanline.
1. While the scanline is drawing, do any additional operations needed to prepare
   for the next scanline.

(3 lines) `VSYNC` signal, (37 lines) vertical blank (`VBLANK`), (192 lines)
 visible scanlines, (30 lines) overscan<br>
`WSYNC` doesn't care which value is stored---it triggers the CPU to wait as soon
 as it receives any write command. A register that triggers an action like this
 is commonly called a *strobe* register.

[stella_programmers_guide]: http://www.alienbill.com/2600/101/docs/stella.html

## 6. Playfield Graphics

Th *playfield* is the lowest-resolution object that the TIA can draw. It's made
 up of 40 pixels which go all the way across the screen. Only 20 pixels are
 unique---the rightmost 20 are either exact duplicates or a mirrored reflection
 of the first 20, depending on a bit in the `CTRLPF` register.<br>
Like everything else the TIA draws, the playfield has to be programmed
 line-by-line. If nothing changes, the TIA will just repeat what was on the
 previous line.<br>
Sometimes you might hear such video display code called a *kernel*, denoting a
 small but well-optimized routine that is timing-sensitive<br>
`COLUPF` (Color-Luminance Playfield/Ball), `COLUBK` (Color-Luminance
 Background), `PF0` (Playfield 0, pixels 0-3, four lower bits are not used),
 `PF1` (Playfield 1, pixels 4-11), `PF2` (Playfield 2, pixels 12-19)

## 7. Players and Sprites

The TIA supports two *player* objects, each eight pixels wide and one pixel
 high. They can be positioned anywhere horizontally on the scanline and the TIA
 remembers their position.<br>
Note that we never said "sprites," since the term had not yet been invented! But
 you can draw sprites with the player objects by changing registers on
 successive scanlines, stacking up horizontal 8-pixel slices. You must also set
 the player's bitmap to zero after the sprite has finished drawing.<br>
You might notice that the `DEX/BNE` loop takes 5 CPU cycles per iteration, which
 means 15 pixels will pass between each iteration. This means we can only
 position objects in 15-pixel increments using this method.<br>
`COLUP0/1` (Color-Luminance Player/Missile 0/1), `RESP0/1` (Reset Player 0/1,
 coarse position, strobe), `GRP0/1` (Graphics Bitmap Player 0/1)

## 8. Color Spirites

When these tables map one table entry to one scanline, they are called
 "single-height" sprites. When a single table entry is used for two successive
 scanlines, they are deemed "double-height" sprites. Sometimes the bitmap table
 is single-height and the color table is double-height.<br>
Now we have to see if this local coordinate is within the sprite bounds, meaning
 if it is less than zero or greater than the height of the sprite. It turns out
 we can do both by using the `BCC` instruction, which is an unsigned "less-than"
 comparison.<br>
[kirkjerk's PlayerPal][playerpal] (web-based sprite tool)

[playerpal]: http://www.alienbill.com/2600/playerpalnext.html

## 9. Sprite Fine Positioning

write to the `RESP0` register to fix the coarse position -> write to the `HMP0`
 register to set a fine adjustment from -7 to +8 pixels -> wait for the `WSYNC`
 -> strobe the `HMOVE` register to apply the changes<br>
divide-by-15 trick - `eor #7` and four `asl`s<br>
other trick - use a lookup table that stores the loop delay and the `HMOVE`
 register value in the same byte (used by Warren Robinett's *Raiders of the Lost
 Ark*)<br>
`HMP0/1` (Horizontal Motion Player 0/1), `HMM0/1` (Horizontal Motion Missle 0/1), `HMBL` (Horizontal Motion Ball), `HMOVE` (Apply Horizontal Motion (fine offsets), apply to all objects, strobe), `HMCLR` (Clear Horizontal Motion Registers, strobe)

## 10. Player/Missile Graphics

two *missiles* and one *ball* are single dots that can be stretched to 1, 2, 4,
 or 8 pixels wide. These objects share the colors of other objects. Missile 0
 shares player 0's color, and missile 1 shares player 1's color. The ball shares
 the same colors as the playfield. instead of setting a bitmap register, you
 just turn them on and off with the `ENAM0/1` registers. Missiles have one
 additional special ability---you can lock their horizontal position to that of
 their corresponding player by setting the 2nd bit of `RESMP0/1`.<br>
`RESM0/1` (Reset Missile 0/1, coarse position, strobe), `RESBL` (Reset Ball,
 coarse position, strobe), `ENAM0/1` (Enable Missile 0/1), `ENABL` (Enable
 Ball), `RESMP0/1` (Reset Missile 0/1 to Player 0/1)

## 11. The SetHorizPos Subroutine

## 12. The PIA Timer

`INTIM` (Timer Counter), `TIM1/8/16T` (Set 1/8/64 Cycle Timer), `T1024T` (Set
 1024 Cycle Timer)

## 13. Joysticks and Switches

```
lda #$02           ; mask for bit 1
bit SWCHB          ; test bits
beq SwitchPressed  ; 0 = pressed
```

The `BIT` instruction has an extra trick, though. It set the `V` (Overflow), `S`
 (Sign) flags to match bits 6 and 7 respectively in the tested value.<br>
Buttons are mapped to bit 7 of `INPT4` (player 0) and `INPT5` (player 1).

```
bit SWCHB          ; test bits
bvc AmateurMode    ; overflow clear = 0 = amateur (B)

lsr SWCHB          ; shift bit 0 (the Game Reset bit) -> Carry
bcc Start          ; Carry clear?

bit SWCHB
bmi .SkipMoveRight ; check bit 7 set
```

`INPT4` (Read Latched Input Port 4), `INPT5` (Read Latched Input Port 5),
 `SWCHA` (Joysticks/Controllers), `SWCHB` (Console Switches, P1/0 Difficulty +
 Color - B/W + Game Select = Game Reset)

## 14. Indirect Addressing

we can use addresses (16-bit quantities) as part of an instruction---for
 example, the `lda SpriteData,y`. But we can also load addresses into RAM. These
 are called them *pointers*, and the 6502 use them via *indirect* addressing
 modes.<br>
The `#<` and `#>` syntax tells the assembler to extract the low and high byte of
 the `SpriteData` address, respectively.<br>
The expression `(SpritePtr),y` is an *indirect indexed* addressing mode. It
 means to look up the 16-bit value at `SpritePtr` and `SpritePtr+1` (low byte
 and high byte), convert it to an address, then add the Y register to it. The
 indirect addressing modes only work in zero-page memory (`$00-$FF`) which
 happens to include all of the VCS's built-in RAM, so we're fine there.

```
SpritePtr .word   ; declare 16-bit pointer (2 bytes)
lda #<SpriteData
sta SpritePtr     ; store lo byte
lda #>SpriteData
sta SpritePtr+1   ; store hi byte

ldy #5
lda (SpritePtr),y ; load value at SpritePtr+Y
```

The other indirect mode is called *indexed indirect* where the addition takes
 place before the lookup. This mode is not as often-used as the `(pointer),y`
 mode in VCS programming, because there is just not that much RAM for multiple
 16-bit pointers! Instead, many games use 8-bit offsets like this:
 `ldy TableOffsets,x`. We are limited to 256 bytes, but that's enough for many
 VCS programs.

```
ldx #4
lda (SpritePtr,x)

lda #2  ; 3rd entry
asl     ;  * 2
tax     ; -> X
lda (ObjectType,x)
```

## 15. A Complex Scene, Part I

## 16. A Complex Scene, Part II

## 17. NUSIZ and Other Delights

## 18. Scoreboard

## 19. Collisions

## 20. Asynchronous Playfields: Bitmap

## 21. Asynchronous Playfields: Bricks

## 22. A Big (48 pixel) Sprite

## 23. Tiny Text

## 24. Six-Digit Scoreboard

## 25. A Big Moveable Sprite

## 26. Sprite Formations

## 27. Advanced Timer Tricks

## 28. Multisprites

## 29. Random Number Generation

## 30. Procedural Generation

## 31. Drawing Lines

## 32. The Sound and Music

## 33. Pseudo-3D: Sunsets and Starry Nights

## 34. Pseudo-3D: Driving Down the Road

## 35. Bank Switching

## 36. Wavetable Audio

## 37. Paddles

## 38. Illegal Opcodes

## 39. Timing Analysis

## 40. Making Games

## 41. Troubleshooting

## A. VCS Memory Map

## B. VCS Colors

## C. 6502 Opcodes

## D. 6502 Instuction Flags

