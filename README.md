# turing-machine
A Turing Machine Simulator

This project is the result of my attempt to synthesize material from two classes I am currently taking at UBC, *PHIL 320 - Symbolic Logic II*, and *CPSC 110 - Computation, Programs, and Programming*.

The design of the Turing machines I simulate is based on the presentation in *Computability and Logic (Fifth Edition)* by Boolos, Jeffrey, and Burgess. This machine uses a binary alphabet (including "blank" as one of the two values) and a tape which is infinite in both directions. so, as I understand from my (quite limited) historical knowledge, it is more similar to Emil Post's independently developed model of a theoretical computing machine than to the machines presented in Turing's original paper.

I have constructed it along the lines of the design method taught in CPSC 110, the online component of which is available on edX under the name [*Systematic Program Design*](https://www.edx.org/course/how-code-systematic-program-design-part-ubcx-spd1x), taught by Gregory Kiczales. I wrote the first version of this program about a month into the term, so as I refine it, I expect to incorporate more of what I learn about the prescribed design methodology.

This program was written in [Racket](https://racket-lang.org/https://racket-lang.org/), a Lisp dialect based on Scheme. Specifically, the first version of this program was written in **Beginning Student Language with List Abbreviations**, a "language level" of Racket with a limited vocabulary, used in the Systematic Program Design course. I may switch language levels later to take advantage of more features.

Racket and DrRacket (its companion IDE) are available [here](https://download.racket-lang.org/). The `.rkt` file is meant to be opened in DrRacket.

After running the code in DrRacket, you can generate a simulation of a turing machine by calling the main function in the REPL (input area). This call will have the form

	```(main (make-config Tape Machine QNum))```

where Tape contains the starting state of the tape, Machine is a list of possible machine states (q-states), and QNum is the number of the initial state (usually 1).