**Lil Compiler**


This is a basic compiler I'm making to learn about IR optimizations. It 
compiles a basic imperative language, which I've creatively named after the 1st
letter of my name (so the file extension is .lmp as in L-imperative). The 
language has if/else, while, assignment, some arithmetic, and printing. 
The compiler turns it into an intermediate representation
of my own making ("L intermediate language," or "Lil"), which can be output
as its own .lil file or as a C file filled with assignment and gotos.
Naming the compiler after the IR doesn't make much sense, but I think it's a
nice pun given the fact that the language and IR are both quite small.
