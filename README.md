# DataStructuresF95
Classical data structures written in F95 and strictly conformant to F95 standard. 

This repo has been migrated to [ClassicalDataStructuresF95](https://github.com/truongd8593/ClassicalDataStructuresF95), where I setup Visual Studio 2019 project, so that Fortran code could be compiled and executed by Intel Fortran compiler (part of Intel OneAPI HPC Toolkit). I was impressed by Intel debugger which helped me fixed a bug in my quick sort implementation.

Data structures in this repo:

- Singly linked list (fully implemented). Some sorting algorithms will be implemented for linked list, including:
  + Selection sort (fully implemented).
  + Quick sort (partially implemented, ie. defect). GDB debugger integrated in Code::Blocks couldn't help me overcome this defect.
  + Merge sort (partially implemented, ie. defect).
  + Radix sort (partially implemented, two test cases passed).
- Stack (to be implemented).
- Queue (to be implemented).

Reference:

[1] Ramsden, Lin, et al. Fortran 90, a conversion course for Fortran 77 programmers (Manchester lecture notes, v3.0, 1995).

[2] Tran Hanh Nhi, Duong Anh Duc - Introduction to data structures and algorithms (textbook in HCMUS's library, 2003).

[3] N.Wirth - Algorithms + Data structures = Programs (Prentice Hall, 1976).
