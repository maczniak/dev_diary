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
 negative clear, greater than or equal, signed)

## 2. The 8butworkshop IDE

## 3. VCS Memory Map

## 4. Writing Your First Assembly Code

## 5. Painting on the CRT

## 6. Playfield Graphics

## 7. Players and Sprites

## 8. Color Spirites

## 9. Sprite Fine Positioning

## 10. Player/Missile Graphics

## 11. The SetHorizPos Subroutine

## 12. The PIA Timer

## 13. Joysticks and Switches

## 14. Indirect Addressing

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

