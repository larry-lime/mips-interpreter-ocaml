# MIPS Interpreter README

## Description

This MIPS Interpreter is designed to execute a small subset of MIPS machine code. It includes a parser and an assembler to load machine code into a simple MIPS processor model, which consists of a register file, memory, and program counter.

## Prerequisites

- OCaml installed on your machine
- GNU Make
- Docker (if you prefer to run the project in a container)

## Compilation

To compile the code, simply run the following command in the terminal:

```sh
make
```

This command generates an executable named `./ps1`.

## Execution

There are two ways to execute the compiled MIPS Interpreter:

1. **Using an Assembly File:**

```sh
./ps1 <name of assembly file>
```

This command loads and assembles the specified MIPS assembly file, executes the interpreter, and prints the final state of registers and memory. Example assembly files can be found in the `tests/` directory.

2. **Running the Test Suite:**

```sh
./ps1
```

Without any arguments, this command runs a built-in test suite and compares your program's output with expected values for various programs.

## Running with Docker

If you prefer to use Docker for building and running your project, follow these steps:

### Build Docker Image

First, build the Docker image from your project directory.

docker build -t mips_interp .

### Run Docker Container

To run the MIPS Interpreter inside a Docker container:

```sh
docker run --rm mips_interp
```

To execute the interpreter with a specific assembly file inside the Docker container, ensure your Dockerfile copies the assembly files into the image or mount the directory containing your assembly files as a volume.
