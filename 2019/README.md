# Advent of Code 2019

This folder contains my solutions to Advent of Code 2019, which I solved using Haskell. Most files in this directory are named `dayXXpY.hs`, which means that they are my solution to Day XX Part Y of Advent of Code 2019. Moreover, there are also some text files labelled `dayXX.in`, which represents the puzzle input I received to Day XX of Advent of Code 2019.

For the days in which there is a `dayXX.in` file, the corresponding `dayXXpY.hs` files are expecting to receive the puzzle input through standard input, so you should run the program using the following command:

```
$ runghc -Wall dayXXpY < dayXX.in
```

For the days in which there is no `dayXX.in` file, the puzzle input has been encoded as a variable in the corresponding `dayXXpY.hs` files, so you can run these programs without providing any standard input:

```
$ runghc -Wall dayXXpY
```

Also, some programs do not just output the puzzle answer but also make several debug statements. This is usually because the program takes a long time to execute, so the debug statements show that the program is not frozen. Other than the exceptions listed below, in these programs, the last number which is outputted represents the puzzle answer.

Finally, while the instructions above apply to most of the programs in this folder, there are some files which do not follow the above pattern, as documented below:

 * In `day05p1.hs` and `day05p2.hs`, the diagnostic codes are outputted using debug statements and then the whole program data is outputted in a final print statement at the end. In both files, the last diagnostic code represents the actual puzzle answer.
 * For Day 9, both parts have very similar solutions, so there is a single `day09.hs` which outputs the solutions to both Parts 1 and 2.
 * For `day13p1.hs` and `day13p2.hs`, the puzzle input has been encoded as a variable in the Haskell file. However, `day13p2.hs` is not a solution to Day 13 Part 2, but rather a simulation which allows you to play the game from the "arcade cabinet" with "a" representing tilting the joystick left, "d" representing tilting the joystick right, and an empty line representing putting the joystick in neutral. Thus, `day13.in` represents the series of inputs needed to beat the game in `day13p2.hs`, thus allowing you to see the final score.
 * For both `day18p1.hs` and `day18p2.hs`, there are separate `day18p1.in` and `day18p2.in` files because the puzzle input changes slightly between Part 1 and Part 2.
 * `day19p2.hs` does not compute the actual puzzle answer, but instead takes in a list of y-coordinates and outputs whether or not that y-coordinate is a valid possible answer or not. Then, the puzzle answer is found by manually guessing y-coordinates based off of binary search in order to find the least possible valid y-coordinate. In the end, for my puzzle input, the least valid y-coordinate was 1666 and the corresponding x-coordinate was 1031.
 * In `day25p1.hs`, the puzzle input has been encoded as a variable in the Haskell file. However, `day25p1.hs` is not a solution to Day 25 Part 1, but rather simply a program which allows you to play the text adventure game encoded by the Intcode program. Thus, `day25.in` represents the series of inputs needed to beat the game, thus allowing you to see the password.