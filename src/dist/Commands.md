The Delta Debugger has three "views", namely :
1. The Code View        : Where your code gets displayed
2. The Stack View       : Where the contents of the programs stack get displayed
3. The Variables View   : Where the currently defined variables in the program is being executed
                          are displyed.
4. The Help View        : The "How to" guide.

Navigating through the Delta Debugger :
- Use the Up, Down, Left and Right Arrow Keys to navigate in a View. You could also use the mouse for navigating Up and Down.
- Use `Tab` and `Shift + Tab` to toggle and toggle-backwards between Views.
- The currently "focussed" View will be evidently visible, scrolling obviously takes effect only on that particular view.
- Use the `Esc` key to stop the debugger program and exit the debugger terminal.

Using the Delta Debugger :
- Use the `b` key to place a breakpoint on the currently selected line.
- Use the `b` key to delete the breakpoint on the currently selected line, if one is already present.
- Use the `r` key to run your program. (This should be done before using the `s` key)
- Use the `n` key to continue to the next breakpoint in your program.
- Use the `s` key to use the step-over functionality and execute the next statement in your code.
- Once your program gets over, you'll be notified through a message in the stack/variables view
- Once your program gets over, if in case of an error, you'll see the error that was thrown by the evaluator and 
  the corresponding stack and variables on which it failed