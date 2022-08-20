Task 3:
----------------------------------------------------------------------
Using reference semantics on silly_add will cause an infinite loop,
since both x and y point to the same memory address.
For example x will do 10 - 1 = 9 and then y will do 9 + 1 = 10, the while loop condition x > 0 is never met.

Using copy semantics on silly_add will not cause an infinite loop,
since x and y have their own copy of num in separate memory addresses.

Reference semantics uses a little bit less memory space and may be a bit faster,
but is not as safe, so i prefer copy semantics in general.
----------------------------------------------------------------------

Tasks completed: 1a, 1b, 2 and 3
