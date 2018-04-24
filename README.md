## JapanOS2018
This repository will be by two students at the Royal Institute of Technology in Stockholm, currently on exchange at the Shibaura Institute of Technology in Tokyo, for a course project in operating systems (ID1206).

The two tasks are the following:
1. Develop a malloc() that works for multi-threaded programs. First, create a version where everything works, but where the free list is protected by a central lock. Second, create a version where each thread has its own free list (thread local storage). Do a performance comparison.
2. Develop a program that creates n threads, linked together in a ring. Between each pair of threads is a construction which enables the passing of a token around the ring. Do a performance review in how fast a token can be passed around the ring (how does it depend on n). Now do the same attempt in Erlang/Elixir, Go or any other language which supports green threads (or maybe a library in C for green threads). Compare performance.

On top of all this, we will both separately write a report on the project.
