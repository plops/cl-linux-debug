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




# Prompt: Write section 4.


## 4. Core Library

This section documents the core functionality of cl-linux-debug, enabling analysis of executable images, control and inspection of running processes, and interpretation of their memory contents.

### 4.1 Executable Images

cl-linux-debug supports loading both ELF and PE executable formats, providing a unified interface for analysing their structure.

#### 4.1.1 Loading and Exploring Images

To load an executable image, use the function `get-executable-image`:

```lisp
(let ((image (get-executable-image "/path/to/executable")))
  ;; Access image data, e.g.:
  (print (path-of image))
  (print (md5-hash-of image))
  (print (entry-address-of image))
  (print (sections-of image)))
```

This function returns a `loaded-executable` object, which aggregates data from all loaded images (the main executable and its shared libraries). You can access the main image via `main-image-of` and all images via `all-images-of`. Each individual image is represented by a `loaded-image` object.

#### 4.1.2 Sections

Executable images consist of sections such as `.text` (code), `.data` (initialized data), `.bss` (uninitialized data), and others. These are accessible via the `sections-of` function:

```lisp
(dolist (section (sections-of image))
  ;; Access section data, e.g.:
  (print (section-name-of section))
  (print (start-address-of section))
  (print (length-of section))
  (print (data-bytes-of section)))
```

Each section is represented by a `loaded-section` object, providing access to its name, address range, and raw data.

#### 4.1.3 Symbols and Regions

Symbols represent named entities within the executable, like variables, functions, and data structures.  cl-linux-debug identifies regions of code associated with these symbols. To find all regions with a given name:

```lisp
(let ((regions (find-regions-by-name executable "my_function")))
  ;; Access region data, e.g.:
  (dolist (region regions)
    (print (start-address-of region))
    (print (length-of region))
    (print (symbol-name-of region))))
```

`find-region-by-address` locates the region containing a specific address. Regions are categorized as objects, functions, plt entries, and got entries.

### 4.2 Debugger

The debugger component provides a low-level interface to control and inspect a running process using the Linux `ptrace` system call.  All operations are managed asynchronously via a task scheduler.

#### 4.2.1 Task Scheduler

The task scheduler allows offloading debugger operations to a background thread, ensuring the main thread remains responsive for GUI interactions.  Define debug tasks using `def-debug-task`:

```lisp
(def-debug-task my-debug-task (process &key arg1 arg2)
  ;; Perform debugger operations here, e.g. access memory or registers
  (let ((value (get-memory-integer process address 4)))
    (print value)))
```

To execute a debug task, use `call-debug-task`:

```lisp
(call-debug-task 'my-debug-task process :arg1 10 :arg2 20)
```

Tasks can wait for events, yield to other tasks, or abort with a condition. The functions `exit-task`, `abort-task`, and `yield-task` control task execution flow. Read-write locks are provided via `make-debug-r/w-lock` and `with-r/w-lock-held` for synchronizing access to shared data.

#### 4.2.2 Ptrace Interface

The `ptrace` system call is wrapped in a set of functions, including:

* `ptrace-attach`: Attach to a running process.
* `ptrace-detach`: Detach from a process.
* `ptrace-continue`: Resume process execution.
* `ptrace-get-registers`: Read process registers.
* `ptrace-set-registers`: Write process registers.
* `ptrace-copy-bytes`: Read or write process memory.

#### 4.2.3 Process Management

The `debug-process` class encapsulates a debugged process, managing threads, signal handling, and event dispatch. Functions like `start-debug` and `stop-debug` control the debug session.

#### 4.2.4 Thread Handling

Each thread within a debugged process is represented by a `debug-thread` object. You can suspend, resume, and control threads individually. The `with-threads-suspended` and `with-thread-suspended` macros simplify thread synchronization.

### 4.3 Data Structures

cl-linux-debug utilizes XML data definitions to describe complex data structures found in process memory. This provides a flexible and extensible system for parsing and interpreting memory contents.

#### 4.3.1 Types

The library defines a hierarchy of type classes, ranging from primitive types (integers, floats, booleans) to compound types (structures, unions) and containers (arrays, pointers). These are defined in the `data-info/types.lisp` file.

#### 4.3.2 Type Layout

The `type-core` module implements core functionality for laying out data structures in memory, including:

* `compute-effective-size`: Calculates the size of a type in memory, considering alignment constraints.
* `compute-effective-alignment`: Determines the alignment requirement of a type.
* `layout-type`: Lays out a type in memory, determining field offsets and handling type proxies.

#### 4.3.3 Type Proxies

Type proxies are used to represent references to global type definitions within other types. They allow efficient representation of complex structures without duplicating information.

### 4.4 Memory Analysis

The library provides powerful tools for analysing the memory of a debugged process.

#### 4.4.1 Memory Mirrors

A memory mirror provides a live view of the process's address space. It is created using `make-memory-mirror`:

```lisp
(let ((memory (make-memory-mirror process)))
  ;; Access memory data, e.g.:
  (get-memory-integer memory address 4))
```

The memory mirror automatically synchronizes with the process, allowing you to track changes in memory contents.

#### 4.4.2 Heap Analysis

The `malloc` module provides functions for analyzing heap structures, identifying allocated chunks, and tracing object references. It can detect and analyse heap structures managed by both glibc and Wine.

#### 4.4.3 Object Analysis

The `object-memory-mirror` class extends the memory mirror with functionality for analysing objects. It can identify objects based on their type information and layout. Functions like `get-address-object-info` provide detailed information about memory regions, including whether they belong to heap chunks, code sections, or global objects.

### 4.5 Search Functionality

The library offers various functions for searching through process memory:

* `find-stl-strings`:  Find STL strings containing a given substring.
* `find-memory-strings`: Find null-terminated strings in any memory region.
* `find-heap-words`: Find specific integer values within heap objects.
* `find-stl-vectors`:  Find STL vectors within a specified size range.
* `find-obj-by-vtable`:  Find objects based on their vtable address.
* `find-changes`:  Track changes in memory contents.

These functions help identify interesting data structures and understand the internal state of the debugged process.

This section provides a comprehensive overview of the cl-linux-debug core library's capabilities. The following sections will delve deeper into specific aspects and advanced usage scenarios.


# Prompt: Write section 5.


## 5. GUI Library

The GUI library provides visual tools for exploring and analyzing the memory of a debugged process. It leverages the core library's functionalities and presents them in a user-friendly graphical interface.

### 5.1 Tree Model (gui/tree-model.lisp)

The foundation of the GUI is a flexible object-oriented tree model, implemented in `gui/tree-model.lisp`. This model allows representing hierarchical data structures, like the organization of memory objects, in a tree view. 

Key features of the tree model include:

* **Object-oriented representation:** Each node in the tree is an instance of a dedicated class, allowing for customized behavior and data storage.
* **Lazy loading:**  Nodes can be expanded on demand, improving performance when dealing with large data sets. This is especially beneficial when analyzing complex memory structures.
* **Dynamic updates:**  The tree model supports dynamic updates, enabling the view to reflect changes in the underlying data, such as modifications in the debugged process's memory.
* **Callbacks and events:**  Callbacks and events are supported, allowing for interactions with the tree view, such as selecting nodes, expanding/collapsing branches, and handling right-click menus.

The tree model is used extensively in the memory browser and list browser, providing a consistent and efficient way to display and interact with complex data.


### 5.2 Debug Hook (gui/debug-hook.lisp)

The debug hook provides a GUI layer for handling debugger events and user interactions. 

* **Restart Selection Dialog:**  When an unhandled condition occurs in the debugged process, the debug hook intercepts it and presents a dialog to the user. This dialog displays the condition details, available restarts, and allows the user to choose a restart or enter debug mode. 
* **Query Dialog:**  Provides a simple dialog for prompting the user for input, such as when setting a new value for a memory object. This allows for interactive debugging and manipulation of the process state.
* **Debug Dialog Stream:**  Implements specialized streams that communicate with the user through dialog boxes. This allows for debugging scripts and interactive evaluation of expressions within the debugged process, with user input and output handled through dialog prompts.
* **Offloaded Computation and Progress Dialog:**  Provides functions for executing long-running computations in a separate thread, while displaying a progress dialog to the user. This prevents the GUI from becoming unresponsive during lengthy operations.

The debug hook ensures a user-friendly and interactive experience by providing visual feedback and prompting the user for actions when needed.


### 5.3 Memory Browser (gui/memory-browser.lisp)

The memory browser provides a graphical visualization of memory objects and their relationships. It leverages the tree model to display memory objects hierarchically, allowing the user to navigate and inspect individual fields and values.

Key features of the memory browser:

* **Hierarchical view:**  Displays memory objects and their fields in a tree structure, allowing for easy navigation and understanding of the object's layout.
* **Data representation:**  Provides columns for displaying the address, name, type, value, and additional information about each memory object. Color coding is used to indicate changes in memory values and the status of objects (e.g., verified, unchecked).
* **Navigation and exploration:**  Allows the user to expand and collapse branches in the tree to explore different levels of the memory structure. Double-clicking a pointer object opens a new browser window for the target object.
* **Contextual menus:**  Provides context menus for individual nodes, offering actions such as browsing linked objects, copying addresses, and rebuilding subtrees.
* **Hotkeys:**  Supports hotkeys for refreshing the memory view and annotating objects.

The memory browser allows for efficient exploration and analysis of memory structures, providing insights into the internal state of the debugged process.


### 5.4 List Browser (gui/list-browser.lisp)

The list browser complements the memory browser by displaying a list of memory objects. This provides a convenient way to view and filter a large collection of objects, particularly useful when working with arrays or collections.

Key features of the list browser:

* **List view:**  Presents memory objects in a tabular format, with columns for index, value, and additional information.
* **Filtering:**  Allows the user to filter the list of objects based on their value or properties. This is useful for narrowing down the objects of interest and identifying specific patterns in memory.
* **Navigation:**  Selecting an object in the list browser updates the associated memory browser, displaying the selected object's structure.
* **New Window Browsing:**  Allows opening new list browser windows for inspecting sub-collections or linked objects.

The list browser, in conjunction with the memory browser, provides a comprehensive view of the memory space, enabling efficient navigation and detailed analysis of individual objects and their relationships.

# Prompt: Write section 6.