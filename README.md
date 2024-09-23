# A very ordinary stack machine
I'm making this to distract myself from the constant dread and stress of writing
a PhD thesis with a tight deadline.

It's a stack machine emulator with 2 256-bytes stacks (one for general use and
one for return pointers). No register instructions, replaced by stack manipulation
(mainly top 2 or 3 elements of the stack). Contains a parser (using Nom) & assembler for
assembly code (see [examples](./examples/)).

Nothing pretentious. Published here for friends and others to see if they want.
Hopefully the code isn't *too* bad.