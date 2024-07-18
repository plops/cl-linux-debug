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


## 6. Advanced Usage

This section covers advanced usage scenarios for the `cl-linux-debug` library, enabling you to perform more complex tasks like injecting custom code into a running process and defining your own data structures for analyzing specific memory layouts.

### 6.1 Code Injection

Code injection allows you to execute custom code within the context of a debugged process. This can be useful for tasks such as:

* **Modifying process behavior:**  Alter the execution flow of a process by injecting code that performs specific actions.
* **Calling internal functions:**  Invoke functions within the process's address space that are not exposed through its normal interface.
* **Inspecting data:**  Inject code to read and analyze memory regions or data structures that are difficult to access directly.

#### 6.1.1 Allocating Injection Areas

Before injecting code, you need to allocate a memory region within the process's address space. This can be done using the `alloc-injection-area` function:

```lisp
(def-debug-task inject-my-code (thread)
  ;; Allocate a writeable region for our code
  (multiple-value-bind (code-addr code-size)
      (alloc-injection-area thread nil)
    ;; Write our code to the allocated region
    (write-process-data (process-of thread) code-addr code-bytes)
    ;; Execute our injected code
    (%exec-injected-code thread code-addr)))
```

#### 6.1.2 Executing Injected Code

Once the code is written to the allocated memory region, you can execute it using the `%exec-injected-code` function:

```lisp
(def-debug-task %exec-injected-code (thread address)
  ;; Set the instruction pointer to the start of our code
  (%set-registers thread :eip address)
  ;; Resume the thread
  (%resume-thread thread)
  ;; Wait for the thread to hit a breakpoint or signal
  (wait-for-state thread :not-type 'debug-thread-state-running))
```

#### 6.1.3 Injecting Syscalls

The `inject-syscall` function provides a convenient way to execute system calls within the debugged process:

```lisp
;; Get the process ID
(let ((pid (inject-syscall thread 20))) 
  (format t "Process ID: ~A~%" pid))
```

#### 6.1.4 Considerations

* Ensure the injected code is compatible with the process's architecture and runtime environment.
* Be cautious when injecting code, as it can have unintended consequences and potentially crash the process.
* Handle errors and exceptions gracefully, as the injected code may encounter unexpected situations.

### 6.2 Custom Data Definitions

You can define custom data structures using XML to analyze specific memory layouts that are not covered by the built-in data definitions.

#### 6.2.1 Defining Types

Use the `<data-definition>` tag to define a set of related data types within a namespace. Define individual types using tags like `<struct-type>`, `<class-type>`, `<enum-type>`, and `<global-object>`.

```xml
<data-definition namespace='my-game'>
  <struct-type type-name='Player'>
    <int32_t name='health'/>
    <int32_t name='score'/>
    <pointer name='inventory' type-name='Inventory'/>
  </struct-type>

  <struct-type type-name='Inventory'>
    <static-array name='items' count='10' type-name='Item'/>
  </struct-type>

  <struct-type type-name='Item'>
    <int32_t name='id'/>
    <int32_t name='quantity'/>
  </struct-type>

  <global-object name='player1' type-name='Player'/>
</data-definition>
```

#### 6.2.2 Using Custom Types

Load the XML data definition using `load-data-definition` and register it with the type context:

```lisp
(load-data-definition "my-game.xml")
(register-data-definition *type-context* "my-game.xml")

;; Access the global object 'player1'
(let ((player (get-memory-global *memory* $my-game:player1)))
  (format t "Player health: ~A~%" $player.health))
```

#### 6.2.3 Considerations

* Choose meaningful names for your types and namespaces.
* Ensure your data definitions accurately reflect the memory layout of the process you are analyzing.
* Use helper functions and macros like `@`, `$`, and `with-bytes-for-ref` to access and manipulate memory data efficiently.

By mastering these advanced techniques, you can unlock the full potential of `cl-linux-debug` and delve deeper into the inner workings of running processes.


# Prompt: Write section 8.

## 8. API Reference

This section provides a comprehensive list of functions, macros, and classes available in the `cl-linux-debug` and `cl-linux-debug.gui` libraries, along with their descriptions and usage examples.

### 8.1 Core Library (`cl-linux-debug`)

#### 8.1.1 Package: `cl-linux-debug`

* **`*debugged-processes*`:**

   * **Description:** A global list containing all currently debugged processes.
   * **Type:** `list`
   * **Example:** `(first *debugged-processes*)` - Get the first debugged process.

* **`start-debug (process-id)`:**

   * **Description:** Starts debugging a process with the specified PID.
   * **Arguments:**
      * `process-id`: The process ID (PID) of the process to debug.
   * **Returns:** A new `debug-process` instance representing the debugged process.
   * **Example:** `(start-debug 12345)` - Start debugging process with PID 12345.

* **`stop-debug (process)`:**

   * **Description:** Stops debugging the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance to stop debugging.
   * **Returns:** `NIL`.
   * **Example:** `(stop-debug *proc*)` - Stop debugging the process stored in `*proc*`.

* **`resume-all-threads (process)`:**

   * **Description:** Resumes all threads of the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance.
   * **Returns:** `NIL`.
   * **Example:** `(resume-all-threads *proc*)` - Resume all threads of the process stored in `*proc*`.

* **`stop-all-threads (process)`:**

   * **Description:** Suspends all threads of the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance.
   * **Returns:** `NIL`.
   * **Example:** `(stop-all-threads *proc*)` - Suspend all threads of the process stored in `*proc*`.

* **`process-memory-maps (process)`:**

   * **Description:** Retrieves the memory maps of the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance or its process ID.
   * **Returns:** A list of `memory-mapping` structures describing the memory regions of the process.
   * **Example:** `(process-memory-maps *proc*)` - Get memory maps of the process stored in `*proc*`.

* **`read-process-data (process address vector &key start end)`:**

   * **Description:** Reads data from the memory of the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance.
      * `address`: The memory address to read from.
      * `vector`: A byte vector to store the read data.
      * `start`: Optional starting index in the vector.
      * `end`: Optional ending index in the vector.
   * **Returns:** The byte vector containing the read data.
   * **Example:** `(read-process-data *proc* #x1000000 *buffer*)` - Read data from address #x1000000 into `*buffer*`.

* **`write-process-data (process address vector &key start end)`:**

   * **Description:** Writes data to the memory of the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance.
      * `address`: The memory address to write to.
      * `vector`: A byte vector containing the data to write.
      * `start`: Optional starting index in the vector.
      * `end`: Optional ending index in the vector.
   * **Returns:** `NIL`.
   * **Example:** `(write-process-data *proc* #x1000000 *buffer*)` - Write data from `*buffer*` to address #x1000000.

* **`name-of (obj)`:**

   * **Description:** Retrieves the name of a named object, like a symbol or a type definition.
   * **Arguments:**
      * `obj`: The object to retrieve the name from.
   * **Returns:** The name of the object as a keyword or string.
   * **Example:** `(name-of *symbol*)` - Get the name of the symbol stored in `*symbol*`.

* **`task-finished? (task)`:**

   * **Description:** Checks if a debug task has finished execution.
   * **Arguments:**
      * `task`: The `debug-task` instance to check.
   * **Returns:** `T` if the task has finished, otherwise `NIL`.
   * **Example:** `(task-finished? *task*)` - Check if the task stored in `*task*` has finished.

* **`return-values-of (task)`:**

   * **Description:** Retrieves the return values of a finished debug task.
   * **Arguments:**
      * `task`: The finished `debug-task` instance.
   * **Returns:** A list of the return values.
   * **Example:** `(return-values-of *task*)` - Get the return values of the task stored in `*task*`.

* **`signalled-condition-of (task)`:**

   * **Description:** Retrieves the condition that caused a debug task to abort.
   * **Arguments:**
      * `task`: The aborted `debug-task` instance.
   * **Returns:** The condition object, or `NIL` if the task finished successfully.
   * **Example:** `(signalled-condition-of *task*)` - Get the condition that caused the task stored in `*task*` to abort.

* **`ignored-signals-of (process)`:**

   * **Description:** Retrieves the list of signals that are ignored by the debugger for the specified process.
   * **Arguments:**
      * `process`: The `debug-process` instance.
   * **Returns:** A list of signal IDs.
   * **Example:** `(ignored-signals-of *proc*)` - Get the list of ignored signals for the process stored in `*proc*`.

* **`exit-task (&rest retvals)`:**

   * **Description:** Signals successful completion of the current debug task and returns the specified values. This macro is only valid inside `def-debug-task`.
   * **Arguments:**
      * `retvals`: The values to return from the task.
   * **Returns:** Does not return directly, but signals task completion to the scheduler.
   * **Example:** `(exit-task 1 2 3)` - Exit the current task and return values 1, 2, and 3.

* **`abort-task (condition &rest args)`:**

   * **Description:** Signals an error condition in the current debug task. This macro is only valid inside `def-debug-task`.
   * **Arguments:**
      * `condition`: Either a condition object, a format string for `simple-error`, or a condition type symbol.
      * `args`: Optional arguments for the format string or condition type.
   * **Returns:** Does not return directly, but signals task abort to the scheduler.
   * **Example:**
      * `(abort-task "Error: ~A" msg)` - Abort with a `simple-error` using the format string and `msg`.
      * `(abort-task 'type-error :datum data :expected-type 'integer)` - Abort with a `type-error`.

* **`yield-task ()`:**

   * **Description:** Yields control back to the task scheduler, allowing other tasks to run. This macro is only valid inside `def-debug-task`.
   * **Returns:** Does not return directly, but signals task yield to the scheduler.
   * **Example:** `(yield-task)` - Yield control back to the scheduler.

* **`with-exit-unwind (form &body unwinds)`:**

   * **Description:** Executes `form` and ensures that the `unwinds` code is executed before the task exits, regardless of whether it exits normally or due to an abort. This macro is only valid inside `def-debug-task`.
   * **Arguments:**
      * `form`: The code to execute.
      * `unwinds`: The code to execute before the task exits.
   * **Returns:** The result of `form`.
   * **Example:**
     ```lisp
     (with-exit-unwind
         (progn
           (do-something)
           (do-something-else))
       (cleanup-resources))
     ```

* **`make-debug-r/w-lock (&key name)`:**

   * **Description:** Creates a new debug read/write lock.
   * **Arguments:**
      * `name`: Optional name for the lock.
   * **Returns:** A new `debug-task-r/w-lock` instance.
   * **Example:** `(make-debug-r/w-lock :name "My Lock")` - Create a lock named "My Lock".

* **`with-r/w-lock-held ((lock mode) &body code)`:**

   * **Description:** Acquires the specified lock in the given mode, executes the code, and releases the lock afterwards. This macro is only valid inside `def-debug-task`.
   * **Arguments:**
      * `lock`: The `debug-task-r/w-lock` instance to acquire.
      * `mode`: Either `:read` or `:write`.
      * `code`: The code to execute while holding the lock.
   * **Returns:** The result of the last expression in `code`.
   * **Example:**
     ```lisp
     (with-r/w-lock-held (lock :write)
       (modify-shared-resource))
     ```

* **`def-debug-task (name lambda-args &body code)`:**

   * **Description:** Defines a new debug task function that can be executed by the task scheduler.
   * **Arguments:**
      * `name`: The name of the task function.
      * `lambda-args`: The arguments list for the task function.
      * `code`: The body of the task function.
   * **Returns:** `NIL`.
   * **Example:**
     ```lisp
     (def-debug-task my-task (arg1 arg2)
       (do-something-with arg1 arg2)
       (exit-task result))
     ```

* **`call-debug-task (command &rest args)`:**

   * **Description:** Creates a new debug task, schedules it for execution, waits for its completion, and returns its results.
   * **Arguments:**
      * `command`: The function to execute in the task.
      * `args`: The arguments to pass to the function.
   * **Returns:** The return values of the function executed in the task.
   * **Example:** `(call-debug-task #'my-task 1 2)` - Execute `my-task` with arguments 1 and 2.

#### 8.1.2 Package: `cl-linux-debug.code-info`

* **`uint8`**, **`uint16`**, **`uint32`**, **`uint64`**:

   * **Description:** Unsigned integer types of 8, 16, 32, and 64 bits.

* **`int8`**, **`int16`**, **`int32`**, **`int64`**:

   * **Description:** Signed integer types of 8, 16, 32, and 64 bits.

* **`machine-word`**, **`machine-uword`**:

   * **Description:** Signed and unsigned integer types of the machine word size (32-bit on x86, 64-bit on x86-64).

* **`uint8-array`**:

   * **Description:** A specialized type for simple arrays of `uint8`.

* **`index-fixnum`**:

   * **Description:** A specialized type for integers that fit within the fixnum range, optionally divided by a divisor and with a bias.

* **`address-int`**:

   * **Description:** An integer type for representing memory addresses (32-bit unsigned on x86, 64-bit signed on x86-64).

* **`+min-address+`**, **`+max-address+`**:

   * **Description:** The minimum and maximum values of `address-int`.

* **`offset`**, **`address`**:

   * **Description:** Specialized types for representing memory offsets and addresses, allowing both integer and fractional values.

* **`format-hex-offset (offset &key force-sign? prefix)`:**

   * **Description:** Formats an offset or address as a hexadecimal string.

* **`parse-hex-offset (offset)`:**

   * **Description:** Parses a hexadecimal string representation of an offset or address.

* **`signed (value &optional bits)`**:

   * **Description:** Converts an unsigned integer to a signed integer with the specified bit width.

* **`unsigned (value &optional bits)`**:

   * **Description:** Converts a signed integer to an unsigned integer with the specified bit width.

* **`start-address-of (object)`**:

   * **Description:** Retrieves the starting memory address of an object.

* **`length-of (object)`**:

   * **Description:** Retrieves the length in bytes of an object.

* **`start-offset-of (object)`**:

   * **Description:** Retrieves the starting offset within a data buffer of an object.

* **`data-bytes-of (object)`**:

   * **Description:** Retrieves the data bytes of an object, if it is a `data-chunk`.

* **`image-of (object)`**:

   * **Description:** Retrieves the image that an object belongs to, if it is a section or a region.

* **`file-offset-of (object)`**:

   * **Description:** Retrieves the file offset of an object, if it is a section.

* **`section-name-of (object)`**:

   * **Description:** Retrieves the name of a section, if it is an `image-section` or `loaded-section`.

* **`loaded? (object)`**:

   * **Description:** Checks if an object is loaded in memory, if it is a section.

* **`executable? (object)`**:

   * **Description:** Checks if an object is executable, if it is a section.

* **`writable? (object)`**:

   * **Description:** Checks if an object is writable, if it is a section.

* **`is-64bit? (object)`**:

   * **Description:** Checks if an object is 64-bit, if it is an executable image.

* **`find-section-by-address (executable address)`**:

   * **Description:** Finds the section containing the specified address in an executable image.

* **`find-section-by-name (executable name)`**:

   * **Description:** Finds the section with the specified name in an executable image.

* **`entry-address-of (object)`**:

   * **Description:** Retrieves the entry point address of an executable image.

* **`path-of (object)`**:

   * **Description:** Retrieves the file path of an executable image.

* **`shared-lib? (object)`**:

   * **Description:** Checks if an executable image is a shared library.

* **`relocated? (object)`**:

   * **Description:** Checks if an object has been relocated in memory, if it is a `loaded-object`.

* **`image-section-of (object)`**:

   * **Description:** Retrieves the original `image-section` of a `loaded-section`.

* **`mapping-of (object)`**:

   * **Description:** Retrieves the memory mapping associated with an object.

* **`loaded-image-of (object)`**:

   * **Description:** Retrieves the `loaded-image` that a `loaded-section` belongs to.

* **`relocation-offset-of (object)`**:

   * **Description:** Retrieves the relocation offset of a `loaded-object`.

* **`unwind-info-of (object)`**:

   * **Description:** Retrieves the DWARF unwind information for a function, if it is an `executable-region-function`.

* **`origin-of (object)`**:

   * **Description:** Retrieves the original non-relocated object from a `loaded-object`.

* **`executable-of (object)`**:

   * **Description:** Retrieves the `loaded-executable` that an object belongs to.

* **`main-image-of (object)`**:

   * **Description:** Retrieves the main image of a `loaded-executable`.

* **`all-images-of (object)`**:

   * **Description:** Retrieves all images (including shared libraries) of a `loaded-executable`.

* **`sections-of (object)`**:

   * **Description:** Retrieves the sections of an executable image or a `section-set`.

* **`find-region-by-address (executable addr &key next?)`**:

   * **Description:** Finds the region containing the specified address in an executable image.

* **`find-regions-by-name (executable name)`**:

   * **Description:** Finds all regions with the specified name in an executable image.

* **`symbol-name-of (object)`**:

   * **Description:** Retrieves the symbol name associated with an object, if it is an `executable-region`.

* **`md5-hash-of (object)`**:

   * **Description:** Retrieves the MD5 hash of an executable image or a `loaded-object`.

* **`binary-timestamp-of (object)`**:

   * **Description:** Retrieves the binary timestamp of an executable image or a `loaded-object`.

* **`make-chunk-table ()`**:

   * **Description:** Creates a new chunk table, which is a binary tree for efficient lookup of address ranges.

* **`lookup-chunk (table address)`**:

   * **Description:** Finds the chunk containing the specified address in a chunk table.

* **`index-chunks (chunks &key is-64bit?)`**:

   * **Description:** Creates a binary search index for efficient lookup of chunks.

* **`lookup-indexed-chunk (index address)`**:

   * **Description:** Looks up a chunk by address in a binary search index.

* **`make-binsearch-uint32-vec (&optional size &rest flags)`**:

   * **Description:** Creates a new adjustable vector of `uint32` elements suitable for binary search.

* **`with-simple-vector-fill ((sv-var fill-vector elt-type) &body code)`**:

   * **Description:** Provides optimized mass insertion of values into a fill-pointer vector.

* **`with-vector-array ((sv-var vector elt-type &key size) &body code)`**:

   * **Description:** Exposes the underlying simple array of an adjustable vector for efficient access.

* **`with-unsafe-int-read ((reader-name vector) &body code)`**:

   * **Description:** Defines a macro for efficient, unchecked reading of integer values from a byte vector.

* **`with-binsearch-in-array ((name vector elt-type comparator &key array-var right-edge?) &body code)`**:

   * **Description:** Defines a function for efficient binary search within a vector.

* **`binsearch-generic (vector key &key cmp)`**:

   * **Description:** Performs a generic binary search on a vector using a custom comparator.

* **`binsearch-uint32-< (vector key)`**:

   * **Description:** Performs a binary search on a vector of `uint32` elements using the `<` comparator.

* **`binsearch-uint32-<= (vector key)`**:

   * **Description:** Performs a binary search on a vector of `uint32` elements using the `<=` comparator.

* **`binsearch-addr64-< (vector key)`** (x86-64 only):

   * **Description:** Performs a binary search on a vector of `address-int` elements using the `<` comparator.

* **`binsearch-addr64-<= (vector key)`** (x86-64 only):

   * **Description:** Performs a binary search on a vector of `address-int` elements using the `<=` comparator.

* **`make-byte-vector (size)`**:

   * **Description:** Creates a new byte vector of the specified size.

* **`disassemble-function (executable fname)`**:

   * **Description:** Disassembles the function with the given name in the specified executable.

* **`region-unwind-table (region)`**:

   * **Description:** Creates a stack unwind table for a function region from its DWARF unwind information.

* **`unwind-state-cfa (state)`**:

   * **Description:** Accesses the CFA (Canonical Frame Address) value of an unwind state.

### 8.2 GUI Library (`cl-linux-debug.gui`)

#### 8.2.1 Package: `cl-linux-debug.gui`

* **`browse-object-in-new-window (memory ref &key title expand-to-addr)`**:

   * **Description:** Creates a new window displaying a memory object browser for the specified memory reference.
   * **Arguments:**
      * `memory`: A `memory-mirror` or `object-memory-mirror` instance.
      * `ref`: The memory reference to browse.
      * `title`: Optional title for the window.
      * `expand-to-addr`: Optional address to expand the tree to.
   * **Returns:** `NIL`.
   * **Example:** `(browse-object-in-new-window *memory* *ref* :title "My Object")`

* **`enable-gui-debugger-hook ()`**:

   * **Description:** Enables the GUI debugger hook, which intercepts uncaught conditions and displays a dialog for selecting a restart.

### 8.3 Data Definition Language

The data definition language is based on XML and is used to define data structures used for analyzing memory. These definitions are loaded into the system and used by the memory analysis components.

#### 8.3.1 Basic Structure

```xml
<data-definition namespace='my-namespace'>
  <struct-type type-name='my-struct'>
    <int32_t name='field1'/>
    <pointer name='field2' type-name='other-struct'/>
  </struct-type>

  <global-object name='my-global' type-name='my-struct'/>
</data-definition>
```

#### 8.3.2 Common Elements

* `<data-definition>`: Top-level element for grouping related definitions.
  * `namespace`: Optional namespace for the defined types and globals.
* `<struct-type>`: Defines a structure type.
  * `type-name`: The name of the type.
  * `key-field`: Optional name of the field that uniquely identifies objects of this type.
* `<class-type>`: Defines a class type.
  * `type-name`: The name of the type.
  * `original-name`: The original, possibly mangled name of the class.
  * `linux-mangling`: The mangled name of the class on Linux.
  * `windows-mangling`: The mangled name of the class on Windows.
* `<bitfield-type>`: Defines a bitfield type.
  * `type-name`: The name of the type.
  * `base-type`: The underlying integer type of the bitfield.
* `<enum-type>`: Defines an enumeration type.
  * `type-name`: The name of the type.
  * `base-type`: The underlying integer type of the enumeration.
* `<global-object>`: Defines a global variable.
  * `name`: The name of the variable.
  * `type-name`: The type of the variable.
* `<symbol-table>`: Defines a custom symbol table.
  * `name`: The name of the symbol table.
  * `os-type`: The operating system the symbol table is for (e.g., `linux`, `windows`).

#### 8.3.3 Field Types

* `int8_t`, `uint8_t`, `int16_t`, `uint16_t`, `int32_t`, `uint32_t`, `int64_t`, `uint64_t`: Signed and unsigned integer types.
* `long`, `ulong`: Signed and unsigned long integer types (platform-dependent size).
* `intptr_t`, `uintptr_t`: Signed and unsigned integer types large enough to hold a pointer.
* `bool`: Boolean type (usually represented as a byte).
* `padding`: Placeholder for unused memory space.
* `pointer`: Pointer to another object.
  * `type-name`: The type of the object pointed to.
* `pointer/array`: Pointer to an array of objects.
* `static-string`: Null-terminated string embedded in the object.
* `ptr-string`: Pointer to a null-terminated string.
* `stl-string`: STL string object.
* `static-array`: Array with elements stored inline in the object.
  * `count`: Number of elements in the array.
* `stl-vector`, `stl-bit-vector`, `stl-deque`, `stl-set`, `stl-fstream`: STL container types.
* `flag-bit`: Single bit flag.
* `enum`: Enumeration value.
  * `type-name`: The type of the enumeration.
* `compound`: Structure or union type.
  * `type-name`: The type of the compound, if it is a proxy for a global type.
  * `is-union`:  Set to "true" if the compound is a union.

## Conclusion

This API reference serves as a guide to the extensive functionality offered by the `cl-linux-debug` library.  By combining these components, developers can build powerful tools for debugging and analyzing Linux processes.
