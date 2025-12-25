The idea of this project is to have an interactive explorer for execution graphs of multithreaded programs.

The features should include:

# Representation of Programs
There should be a definion of a type that represents programs. A multithreaded programs consists of
- a list of shared variable declarations (each variable is a 64-bit integer)
- a list of threads, which are meant to be thought of as threads running in parallel; each of which is a pair of a list of local variable declarations and a statement
  - a statement is one of:
    - a no-op
    - an if-statement which is an expression (the condition) and two statements (the then-block and the else-block)
    - a while-statement which is an expression (the condition) and a statement (the body)
    - a sequence-statement which is a pair of statements which are to be thought of as two statements running one after another
    - an assignment, which can be:
      - a store to a shared variable which has an expression on the RHS (additionally with a memory order type (RA or non-atomic))
      - a load from a shared variable (a shared variable on the RHS and a local variable on the LHS) (additionally with a memory access type (RA or non-atomic))
      - an assignment to a local variable (an expression on the RHS and a local variable on the LHS)
    - an assert statement with an expression
    - an assume statement with an expression
  - an expression is one of:
    - a local variable
    - a constant (an integer)
    - a binary operation (a binary operation enum (+, -, *, /)) with a pair of expressions

# Representation of execution graphs
An execution graph consits of several events. The events are organized such that there is one init event and then for each thread (in the underlying program), there is a list of events (these are totally ordered by program order in each thread). The execution graphs also keeps track of a total order in which events have been added to it.

A non-init event is one of
- a read event reading a specific shared location and having a read-from relation that goes to this event from exactly one write event to that same location
- a write event writing a specific value to a specific shared location; there is a coherence order, which orders only write events to the same location and also the init event; all writes events to the same location and also the init event are always totally ordered by the coherence order

# Interpretation of Programs
There should be a function which takes in
- a program,
- an execution graph that has been produced by executing a prefix of that program,
and returns, for each thread, what will happen next by continuing to execute that thread -- that can be one of
- a shared read (of a particular location)
- a shared write (of a particular location with a particular value)
- an assertion violation
- getting blocked (when encountering an invalid assumption)
- finishing execution of that thread

# The Application
## The State
There should a representation for a running instance of this whole application. The state of that application should consits of a tree of execution graphs representing the executions of prefixes of one given program (which is also saved but immutable in the application state). For each execution graph in the tree, if it has children, we should save which thread took a step to produce those children. For each execution graph in the tree, we should also save the next possible action in each thread (no matter which ends up then being scheduled first).

## User interaction capabilities
First of all, the user of the application should be able to see the underlying program that is being executed in the current instance. The user of the application should be able to inspect the execution graph tree and then to select one of the leaf graphs. For that leaf, the user should be able to select which thread will be scheduled. Given this selection, the user should be able to construct all the children graphs. Those should always include the newly added event and it should be properly connect via rf or co to some other events. However, it is also possible that adding a new event requires the removal of some recent events in other threads, so the user should also be able to do this to the execution graph.

## User interface
The entire user interface should be behind an abstract interface so that it is easily possible to, say, have both a terminal interface and one with a GUI.

## Initialization
This Rust program should start by randomly generating a program to execute and creating an application instance with it. The user should then be let interact with that application instance.
