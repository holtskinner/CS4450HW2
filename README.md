# HOMEWORK 2: CS 4450 
**DUE: 3:00PM, 10/16/17**

Each Problem is worth 10 points, for a total of 50

1. Define a function, prob1, by rewriting listComp using the functions map and filter instead of list comprehension. Keep in mind that the inputs are `f`, a function, `p` a function then returns a `Bool`, and `xs`, a list of appropriate type. Failure to use both map and filter will result in 0 points for this problem.

    ```haskell
        listComp f p xs = [ f x | x <- xs, p x ]
    ```

1. Define a function prob2 that takes an Integer as input and returns a list of digits, i.e., [Integer], in order as they occur in the input. For negative inputs, it should return [].

    ```haskell
        *Homework2> prob2 (-1)
        []
        *Homework2> prob2 967896
        [9,6,7,8,9,6]
    ```

1. Define a function prob3 that takes an Integer as input and returns a list of digits, i.e., [Integer] in reverse order as they occur in the input. For negative inputs, it should return [].

    ```haskell
        *Homework2> prob3 (-1)
        []
        *Homework2> prob3 967896
        [6,9,8,7,6,9]
    ```
1. Define a function prob4 that takes a list of non-negative numbers, i.e., [Integer], and multiplies every other digit starting from the right by 2.

    ```haskell
         *Homework2> prob4 [18,1,2,18]
        [36,1,4,18]
        *Homework2> prob4 []
        []
    ```

1. Define a function prob5 that takes a list of non-negative numbers, i.e., [Integer], and calculates the sum of all digits in the list; returning 0 for the empty list.

    ```haskell
        *Homework2> prob5 [18,1,2,18]
        21
        *Homework2> prob5 []
        0
    ```
