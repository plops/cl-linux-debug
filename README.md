# cl-linux-debug 

cl-linux-debug is a Common Lisp library for inspecting and manipulating the memory of a running process on Linux. It provides a rich set of tools for analyzing executable images, controlling process execution, and exploring data structures in memory.

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

For detailed documentation and API reference, please refer to the **doc/** folder in this repository.
