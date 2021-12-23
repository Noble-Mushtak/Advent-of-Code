# Advent of Code 2021

This folder contains my solutions to Advent of Code 2021, which I solved using Scala 2.13.4.

The solution to both parts for day `XX` are contained in the `main.scala` file in the `dayXX` folder. I have not uploaded my puzzle inputs to this GitHub repo, but you can run the scripts on your own puzzle inputs in the command line as follows:

    scala main.scala /path/to/in.txt

where `/path/to/in.txt` is the path to the file containing the puzzle input for that day.

For Day 15, which was solved using C\+\+17, there is a `Makefile`, so you can compile and run the C\+\+ program using the following commands:

    make main
    ./main < /path/to/in.txt

For Days 16 and 20, which were solved using Python 3.8.10, you can just run the script using the following command:

    python3 main.py /path/to/in.txt

For Day 23, there is an optional `-printpath` command line argument that you can use to print the path that is used to organize the amphipods with the least energy required. If you would like to the path to be printed, run the script as follows:

    scala main.scala /path/to/in.txt -printpath