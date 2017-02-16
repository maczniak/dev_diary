# [Mazes for Programmers][homepage] by Jamis Buck, The Pragmatic Programmers (2015)

[source code][source_code], [errata][errata]<br>
Walter Pullen's [Think Labyrinth!][think_labyrinth]<br>
Mike Bostock's [Visualizing Algorithms][visualizing_algorithms]<br>
author's [blog post][blog_post]

[source_code]: https://pragprog.com/titles/jbmaze/source_code
[errata]: https://pragprog.com/titles/jbmaze/errata
[think_labyrinth]: http://www.astrolog.org/labyrnth.htm
[visualizing_algorithms]: https://bost.ocks.org/mike/algorithms/
[blog_post]: http://weblog.jamisbuck.org/2011/2/7/maze-generation-algorithm-recap

## I. The Basics

### 1. Your First Random Mazes

perfect maze (where every cell can reach every other cell by exactly one path)
 vs braid maze (few or no dead ends, no loops)<br>
texture, bias<br>
Binary Tree algorithm - decide whether to carve a passage north or east randomly in all the cells, perfect maze, binary tree, bias toward northeast<br>
Sidewinder algorithm - either add to the run (east) or close it out (one random north within a run of cells), bias toward north

### 2. Automating and Displaying Your Mazes

[cell.rb][cell_rb], [grid.rb][grid_rb], [binary_tree.rb][binary_tree_rb], [binary_tree_demo.rb][binary_tree_demo_rb], [sidewinder.rb][sidewinder_rb], [sidewinder_demo.rb][sidewinder_demo_rb], [ChunkyPNG][chunkypng]<br>
`Cell.neighbors()` matters a lot!

[cell_rb]: http://media.pragprog.com/titles/jbmaze/code/cell.rb
[grid_rb]: http://media.pragprog.com/titles/jbmaze/code/grid.rb
[binary_tree_rb]: http://media.pragprog.com/titles/jbmaze/code/binary_tree.rb
[binary_tree_demo_rb]: http://media.pragprog.com/titles/jbmaze/code/binary_tree_demo.rb
[sidewinder_rb]: http://media.pragprog.com/titles/jbmaze/code/sidewinder.rb
[sidewinder_demo_rb]: http://media.pragprog.com/titles/jbmaze/code/sidewinder_demo.rb
[chunkypng]: http://chunkypng.com/

### 3. Finding Solutions

Some, like the Pledge or Trémaux algorithms, are useful when you can’t see the
 entire maze. Others, like dead-end filling and the shortest-path algorithms,
 require a more omniscient view.<br>
Dijkstra's algorithm<br>
[distances.rb][distances_rb], [distance_grid.rb][distance_grid_rb], [dijkstra.rb][dijkstra_rb], [longest_path.rb][longest_path_rb], [colored_grid.rb][colored_grid_rb], [coloring.rb][coloring_rb]<br>
Think Labyrinth! [Maze Psychology][maze_psychology]

[distances_rb]: http://media.pragprog.com/titles/jbmaze/code/distances.rb
[distance_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/distance_grid.rb
[dijkstra_rb]: http://media.pragprog.com/titles/jbmaze/code/dijkstra.rb
[longest_path_rb]: http://media.pragprog.com/titles/jbmaze/code/longest_path.rb
[colored_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/colored_grid.rb
[coloring_rb]: http://media.pragprog.com/titles/jbmaze/code/coloring.rb
[maze_psychology]: http://www.astrolog.org/labyrnth/psych.htm

### 4. Avoiding Bias with Random Walks

Aldous-Broder algorithm - absolutely unbiased, random walk, slow-to-finish<br>
Wilson algorithm - absolutely unbiased, loop-erased random walk, slow-to-start<br>
Think Labyrinth! [Maze Classification][maze_classification]<br>
uniform vs nonuniform<br>
[aldous_broder.rb][aldous_broder_rb], [aldous_broder_demo.rb][aldous_broder_demo_rb], [aldous_broder_colored.rb][aldous_broder_colored_rb], [wilsons.rb][wilsons_rb], [wilsons_demo.rb][wilsons_demo_rb]

[maze_classification]: http://www.astrolog.org/labyrnth/algrithm.htm
[aldous_broder_rb]: http://media.pragprog.com/titles/jbmaze/code/aldous_broder.rb
[aldous_broder_demo_rb]: http://media.pragprog.com/titles/jbmaze/code/aldous_broder_demo.rb
[aldous_broder_colored_rb]: http://media.pragprog.com/titles/jbmaze/code/aldous_broder_colored.rb
[wilsons_rb]: http://media.pragprog.com/titles/jbmaze/code/wilsons.rb
[wilsons_demo_rb]: http://media.pragprog.com/titles/jbmaze/code/wilsons_demo.rb

### 5. Adding Constraints to Random Walks

Hunt-and-Kill algorithm - when it meets dead ends, it choices (hunts for) another cell among unvisited cells that has any visited neighbors; paths tend to wind around quite a bit, relatively fewer dead ends, less memory than Recursive Backtracker<br>
Recursive Backtracker - when it meets dead ends, it backtracks (<-> carve) until it finds a cell taht has an unvisited neighbor; it ends when the stack is empty; faster than Hunt-and-Kill, depth-first search<br>
[hunt_and_kill.rb][hunt_and_kill_rb], [hunt_and_kill_demo.rb][hunt_and_kill_demo_rb], [grid.rb][grid_rb], [deadend_counts.rb][deadend_counts_rb], [recursive_backtracker.rb][recursive_backtracker_rb], [recursive_backtracker_demo.rb][recursive_backtracker_demo_rb]

[hunt_and_kill_rb]: http://media.pragprog.com/titles/jbmaze/code/hunt_and_kill.rb
[hunt_and_kill_demo_rb]: http://media.pragprog.com/titles/jbmaze/code/hunt_and_kill_demo.rb
[grid_rb]: http://media.pragprog.com/titles/jbmaze/code/grid.rb
[deadend_counts_rb]: http://media.pragprog.com/titles/jbmaze/code/deadend_counts.rb
[recursive_backtracker_rb]: http://media.pragprog.com/titles/jbmaze/code/recursive_backtracker.rb
[recursive_backtracker_demo_rb]: http://media.pragprog.com/titles/jbmaze/code/recursive_backtracker_demo.rb

## II. Next Steps

### 6. Fitting Mazes to Shapes

masking by killing cells<br>
not available for Binary Tree and Sidewinder algorithms<br>
[killing_cells.rb][killing_cells_rb], [mask.rb][mask_rb], [masked_grid.rb][masked_grid_rb], [simple_mask.rb][simple_mask_rb], [mask.txt][mask_txt], [ascii_mask.rb][ascii_mask_rb], [image_mask.rb][image_mask_rb]

[killing_cells_rb]: http://media.pragprog.com/titles/jbmaze/code/killing_cells.rb
[mask_rb]: http://media.pragprog.com/titles/jbmaze/code/mask.rb
[masked_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/masked_grid.rb
[simple_mask_rb]: http://media.pragprog.com/titles/jbmaze/code/simple_mask.rb
[mask_txt]: http://media.pragprog.com/titles/jbmaze/code/mask.txt
[ascii_mask_rb]: http://media.pragprog.com/titles/jbmaze/code/ascii_mask.rb
[image_mask_rb]: http://media.pragprog.com/titles/jbmaze/code/image_mask.rb

### 7. Going in Circles

adaptive subdivision<br>
[polar_grid.rb][polar_grid_rb], [polar_grid_test.rb][polar_grid_test_rb], [polar_cell.rb][polar_cell_rb], [circle_maze.rb][circle_maze_rb], 

[polar_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/polar_grid.rb
[polar_grid_test_rb]: http://media.pragprog.com/titles/jbmaze/code/polar_grid_test.rb
[polar_cell_rb]: http://media.pragprog.com/titles/jbmaze/code/polar_cell.rb
[circle_maze_rb]: http://media.pragprog.com/titles/jbmaze/code/circle_maze.rb

### 8. Exploring Other Grids

~~plane~~ tiled tesellation - hexagon come together in a honeycomb pattern (sigma maze), triangles form a girder-style lattice (delta maze, Δ)<br>
challenge: uniform tilings, Wythoff's construction, Voronoi diagrams, truncated quadrille (for example, upsilon maze that consists of octagons and squares), rhombitrihexagonal<br>
[hex_cell.rb][hex_cell_rb], [hex_grid.rb][hex_grid_rb], [hex_maze.rb][hex_maze_rb], [triangle_cell.rb][triangle_cell_rb], [triangle_grid.rb][triangle_grid_rb], [delta_maze.rb][delta_maze_rb]

[hex_cell_rb]: http://media.pragprog.com/titles/jbmaze/code/hex_cell.rb
[hex_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/hex_grid.rb
[hex_maze_rb]: http://media.pragprog.com/titles/jbmaze/code/hex_maze.rb
[triangle_cell_rb]: http://media.pragprog.com/titles/jbmaze/code/triangle_cell.rb
[triangle_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/triangle_grid.rb
[delta_maze_rb]: http://media.pragprog.com/titles/jbmaze/code/delta_maze.rb

### 9. Braiding and Weaving Your Mazes

not perfect maze (has loops, examples: Pac-Man, Dungeon crawlers (NetHack) and "open world" games (Zelda, Final Fantasy), First-person shooters (Doom, Quake, Descent)) - braiding (dead-end culling), weaving (allows passages to intersect by moving over and under each other)<br>
braid maze (with no dead ends)<br>
cost (weight) vs. distance

[weighted_cell.rb][weighted_cell_rb], [weighted_grid.rb][weighted_grid_rb], [weighted_maze.rb][weighted_maze_rb], [weave_grid.rb][weave_grid_rb], [weave_maze.rb][weave_maze_rb]

[weighted_cell_rb]: http://media.pragprog.com/titles/jbmaze/code/weighted_cell.rb
[weighted_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/weighted_grid.rb
[weighted_maze_rb]: http://media.pragprog.com/titles/jbmaze/code/weighted_maze.rb
[weave_grid_rb]: http://media.pragprog.com/titles/jbmaze/code/weave_grid.rb
[weave_maze_rb]: http://media.pragprog.com/titles/jbmaze/code/weave_maze.rb

## III. More Algorithms

### 10. Improving Your Weaving

### 11. Growing With Prim's

### 12. Combining, Dividing

## IV. Shapes and Surfaces

### 13. Extending Mazes into Higher Dimensions

### 14. Bending and Folding Your Mazes

### A1. Summary of Maze Algorithms

### A2. Comparison of Maze Algorithms

