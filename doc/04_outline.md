Prompt (175432 tokens):

This github project (called cl-linux-debug) is a tool for investigating memory of a process on linux.
It needs a documentation. Create an outline of the documentation based on the following source code files:


# cl-linux-debug Documentation

## Introduction
cl-linux-debug is a powerful tool designed for investigating the memory of a process running on a Linux operating system. This documentation provides a comprehensive overview of the project's functionality, usage, and implementation.

## Components

### 1. Core Library (cl-linux-debug)
   * **code-info:**
       * **utils:**  Defines basic data types, address ranges, binary search indices, and binary data parsing utilities.
       * **bea-engine:**  Wraps the BeaEngine library for x86/x64 instruction disassembly. Provides functions for disassembling instructions, encoding/decoding arguments, and managing instruction categories and prefixes.
       * **dwarf:**  Parses DWARF debugging information for stack unwinding and function annotations.
       * **elf:**  Loads ELF executable images and extracts sections, symbols, and relocation information.
       * **exe:**  Loads PE executable images and extracts sections and headers.
       * **executable:**  Combines ELF and PE loading, defines loaded images and executables, and disassembles functions.
       * **classes:**  Defines core data structures for representing loaded executables, sections, regions, and symbols.
       * **symbols:**  Processes and registers symbols and regions from ELF files. Implements .got and .plt annotations.
   * **debugger:**
       * **tasks:**  Implements a task scheduler for managing debugger operations asynchronously. Defines tasks for waiting, yielding, exiting, and managing locks.
       * **ptrace:**  Wraps the Linux ptrace system call for process manipulation. Provides functions for attaching, detaching, continuing, and manipulating process registers and memory.
       * **proc:**  Reads process information from the `/proc` filesystem. Provides functions for reading memory maps and thread IDs.
       * **debug-process:**  Implements the core debug process management functionality. Handles thread creation, state changes, and event handling.
       * **debug-attach:**  Implements functions for attaching to and detaching from a running process.
       * **code-injection:**  Provides functions for injecting code into a running process and allocating memory regions for code execution.
       * **classes:**  Defines classes for managing debug threads, thread states, and the debug process.
   * **data-info:**
       * **fields:**  Defines reader macros and functions for accessing data structures using $-prefixed field names.
       * **types:**  Defines XML data definitions for various data structures. Provides base classes for primitive types, compound types, containers, and enums.
       * **type-core:**  Implements core type layout functionality. Defines functions for computing effective sizes, alignments, and handling type proxies.
       * **type-memory:**  Defines memory object references and functions for resolving type information in memory.
       * **type-misc:**  Implements various data type handling, including primitives, integers, booleans, floats, enums, and pointers.
       * **type-stl:**  Implements support for STL data structures, including strings, vectors, bit vectors, deques, sets, and file streams.
       * **glibc:**  Provides support for glibc heap management structures and functions.
       * **wine:**  Provides support for Wine process heap structures and functions.
       * **malloc:**  Implements chunk map and functions for analysing heap structures in memory.
       * **objects:**  Defines an object-oriented memory mirror and functions for analyzing objects in memory.
       * **search:**  Implements search functions for finding strings, vectors, heap words, and changes in memory.
       * **csv-export:**  Provides functions for exporting memory structures to a CSV file.
       * **xml-helper:**  Provides helper functions for parsing and evaluating XML data definitions in the context of memory analysis.
       * **package:**  Defines package structure and exports.

### 2. GUI Library (cl-linux-debug.gui)
   * **tree-model:**  Implements an object-oriented tree model and view for presenting hierarchical data.
   * **debug-hook:**  Implements GUI dialogs for handling uncaught conditions and prompting user input.
   * **memory-browser:**  Implements a memory browser widget for visualizing memory objects.
   * **memory-objects:**  Defines memory object nodes and functions for laying out objects in a tree structure.
   * **list-browser:**  Implements a list browser widget for displaying memory objects in a list view.
   * **patches.gui:**  Applies patches to cl-gtk2-gtk to fix missing functionality.
   * **package:**  Defines package structure and exports.

## Example Usage
```lisp
(enable-gui-debugger-hook)
(start-debug 12345) ; Attach to a process with PID 12345

(let* ((proc (first *debugged-processes*)) ; Get the debugged process
       (main-thread (main-thread-of proc))
       (addr (getf (ptrace-get-registers (thread-id-of main-thread)) :eip))
       (memory (make-memory-mirror main-thread)))
  (browse-object-in-new-window memory addr)) ; Browse memory at current EIP
```

## Documentation Outline
1. **Introduction:**  Provide a brief overview of cl-linux-debug's purpose and capabilities.
2. **Installation:**  Explain how to install the library and its dependencies.
3. **Getting Started:**  Present a simple example demonstrating how to attach to a process and browse its memory.
4. **Core Library:**  
   *  **Executable Images:**  Explain how to load ELF and PE executables, extract sections, and analyze symbols and regions.
   *  **Debugger:**  Describe the task scheduler, ptrace interface, process management, and thread handling.
   *  **Data Structures:**  Document the data definitions for various types, including primitives, compound types, containers, and enums.
   *  **Memory Analysis:**  Explain how to create memory mirrors, access memory data, and analyze heap structures.
   *  **Search Functionality:**  Describe the functions for finding strings, vectors, heap words, and changes in memory.
5. **GUI Library:**  
   *  **Tree Model:**  Explain the object-oriented tree model and view for presenting hierarchical data.
   *  **Debug Hook:**  Describe the GUI dialogs for handling uncaught conditions and prompting user input.
   *  **Memory Browser:**  Document the memory browser widget and its functionality.
   *  **List Browser:**  Document the list browser widget and its functionality.
6. **Advanced Usage:**  
   *  **Code Injection:**  Explain how to inject code into a running process.
   *  **Custom Data Definitions:**  Show how to define custom XML data structures for analyzing specific memory layouts.
7. **Troubleshooting:**  Provide solutions to common problems and errors.
8. **API Reference:**  List all functions and classes with detailed descriptions and usage examples.

## Contributing
Contributions to the documentation are welcome. Please submit pull requests with any improvements or additions.
