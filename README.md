# Enter the Matrix

## Description

**Enter the Matrix** is a School 42 project introducing the fundamentals of **Linear Algebra** through practical OCaml implementations.

The project covers the implementation of vectors, matrices, scalar operations, linear transformations, and other essential concepts used in computer graphics, physics, machine learning, and scientific computing.

Each exercise focuses on a specific mathematical concept and provides a small executable demonstrating its behavior.

---

## Project Structure

```
ex00.ml
ex01.ml
...
ex15.ml
Makefile
```

Each exercise is compiled as an independent executable.

---

## Compilation

Compile every exercise:

```bash
make
```

Compile everything from scratch:

```bash
make re
```

---

## Running

Execute all exercises:

```bash
make run
```

Each executable is launched in order (`ex00` → `ex15`) and prints its own test output.

---

## Cleaning

Remove object files:

```bash
make clean
```

Remove object files and executables:

```bash
make fclean
```

---

## Topics

- Vector operations
- Matrix operations
- Dot product
- Matrix multiplication
- Linear maps
- Vector spaces
- Scalar multiplication
- Fused Multiply-Accumulate (FMA)
- Generic linear algebra structures

---

Developed in **OCaml** using **ocamlopt**.
