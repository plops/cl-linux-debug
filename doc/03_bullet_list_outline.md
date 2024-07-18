
Prompt:

Write a bullet list outline of the following common lisp project.
<02_files.txt>

## cl-linux-debug Project Outline

**Project Structure:**

* **cl-linux-debug:**
    * **xmlisp:** XML serialization/deserialization library.
        * `package.lisp`: Package definition for XMLisp.
        * `XMLisp.lisp`: XMLisp library code.
    * `patches.lisp`: Patches for external libraries.
    * **code-info:** Debugged process executable loading and analysis.
        * `package.lisp`: Package definition for code-info.
        * `utils.lisp`: Utility functions for binary data parsing and search.
        * `classes.lisp`: Class definitions for executable images, sections, and regions.
        * `bea-engine-grovel.lisp`: CFFI groveling for the BeaEngine disassembler library.
        * `bea-engine.lisp`: Disassembly using BeaEngine.
        * `dwarf.lisp`: DWARF unwind info parsing.
        * `elf.lisp`: ELF executable loading.
        * `exe.lisp`: PE/EXE executable loading.
        * `executable.lisp`: High-level executable loading and management.
        * `symbols.lisp`: Symbol table parsing and association with executable regions.
* **cl-linux-debug.gui:** GUI components for the debugger.
    * `patches.gui.lisp`: Patches for external libraries used by the GUI.
    * **gui:**
        * `package.lisp`: Package definition for GUI.
        * `debug-hook.lisp`: Debugger integration and utility functions for the GUI.
        * `tree-model.lisp`: Generic tree model implementation for displaying object hierarchies.
        * `memory-objects.lisp`: Tree model and rendering for memory objects.
        * `memory-browser.lisp`: Widget for browsing and interacting with memory objects.
        * `list-browser.lisp`: Widget for displaying and filtering lists of memory objects.
* **cl-linux-debug.test:** Unit tests for the debugger.
    * **test:**
        * `package.lisp`: Package definition for tests.
        * `tasks.lisp`: Tests for the debug task scheduler and related classes.
* **mk-runtime:**
    * `grovel/common.h`: Header file for C groveling utilities.
    * `load-libs.lisp`: Loads and prepares required libraries for building a runtime.

**Core Functionality:**

* **Process Attaching and Detaching:**
    * Uses ptrace to attach to and detach from a running process.
    * Manages multiple threads within the debugged process.
    * Handles thread state changes and events like forks and execs.
* **Executable Analysis:**
    * Loads ELF and PE executables and maps them into memory.
    * Parses symbol tables, DWARF unwind info, and relocation data.
    * Associates symbols with code and data regions in memory.
* **Memory Reading and Writing:**
    * Reads and writes data from the debugged process memory.
    * Provides binary data parsing utilities for various data types.
    * Implements a binary search index for efficient memory object lookup.
* **Object-Oriented Memory Representation:**
    * Uses a custom type system to describe memory objects and their relationships.
    * Allows defining custom types for specific data structures.
    * Provides a DSL for accessing memory objects and their fields.
* **GUI Interaction:**
    * Displays memory objects in a hierarchical tree view.
    * Provides browsing and navigation tools for exploring memory.
    * Allows setting annotations and marking memory objects with different states.

**Key Features:**

* **Debug Task Scheduler:** Offloads debugger requests to a separate thread for asynchronous execution.
* **Read/Write Lock:** Provides thread-safe access to shared debugger resources.
* **Code Injection:** Enables executing custom code within the debugged process.
* **Progress Dialog:** Informs the user about ongoing background operations.
* **Restart Selection Dialog:** Presents the user with available restarts for handling errors and exceptions.
* **Object Search:** Find memory objects containing specific data patterns or belonging to a certain class.

This outline provides a high-level overview of the cl-linux-debug project. 
Each bullet point can be expanded to include more details about the specific code and functionality.
