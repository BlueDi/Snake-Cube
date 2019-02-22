# Snake-Cube
In this project, a solver for the wooden puzzle Snake Cube was developed in the Prolog programming language.

The program was built using __SICStus Prolog__, and this solver will use the library `clpfd` provided from SICStus.

The user will provide the snake in a specified format, and the program will return a solution for it.

The user can ask for more solutions.

The snake provided should be a list starting and ending with the value `3`, and the mid values should be `1` or `2`, which the `1` will represent a straight in the snake, and the `2` a corner.

Information about the puzzle came from:
https://www.jaapsch.net/puzzles/snakecube.htm

# Instructions:
1. Set the working directory to where the program will write the file. `File->Working Directory…`;
2. Consult the file `main.pl`;
3. Run `snake_cube(+Snake, +Mode)`.
    1. `Snake` - The snake that you want to get a solution; There are some examples in [`main.pl`](https://github.com/BlueDi/Snake-Cube/blob/master/main.pl) such as `snake_2d([3,2,2,2,2,2,2,3])` and `unique_solution([3,2,2,2,2,2,2,2,2,1,2,1,2,2,2,1,2,2,2,2,2,2,2,2,2,2,3])`.
    2. `Mode` - The number of solutions you want, it can be `one` or `all`.

The cube solution will give an array that has the order in which the positions will be visited.

The positions in the cube are the same as in this image:

![Order of the positions in the cube](numberedcube.png)
