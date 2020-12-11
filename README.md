1. Introduction

This project contains various mini-apps and small libraries written in Lisp. 

It has been created for educational purposes only, i.e. as a measure of my progress in learning this language.

Note: for running the tests the lisp-unit framework from https://github.com/OdonataResearchLLC/lisp-unit is required. This is implemented as submodule within the LearnLisp project. After cloning the repository please run:

      git submodule init
      git submodule update

It is also recommended to run the last command each time a git pull is executed.

You can run the tests by executing exectests.lisp from the LearnLisp/Tests directory. For more information regarding the unit testing framework please read the documentation contained in the lisp-unit repo (see above link).

2. Project Contents

This project is divided into three parts:
- Apps
- Libs
- Tests

The Apps directory contains mini-applications mainly created for "hands-on" testing of functionality created in Libs. Some examples would be: retrieving the greatest common divisor based on two integer numbers input by user, getting the prime numbers within a specific interval (also input by user via keyboard), etc.

The Libs directory is the project core. Within its files diverse functionality has been included:
- functions related to mathematical operations: greatest common divisor, least common multiple (Lisp already has these but I created them as exercises), Fibonacci, retrieving a sequence of consecutively following prime numbers
- functions created for sorting arrays by using various algorithms: bubble sort, insertion sort, merge sort, quick sort, heap sort, bucket sort. In addition to this I also created some functions for shuffling array elements either by sequentially comparing elements two by two ("counter" sort, e.g. 1 < 20 > 18 < 19 ...) or by using randomization (shuffle). This shuffling can be useful as a pre-processing stage of some sorting algorithms (e.g. quick sort). Finally a statistics function has been provided for retrieving specific properties of sorted/unsorted sequences, e.g. the number of subsequences that are sorted, maximum number of elements in a sorted subsequence etc. This helps evaluate the efficiency of a shuffling algorithm for example.
- functions used for requesting user input from keyboard. They are mainly used by Apps and work in conjunction with parse functions. The goal was to reduce number of code lines used for requesting input in an app. An example would be requesting the user to enter an integer number that is larger than 2. The user will only be allowed to carry on if the right string has been entered. Another option is to quit the program by pressing ENTER.
- parse functions: integer string parser and decimal string parser. The integer string parser converts and int string to an integer number. The standard Lisp library already has a similar function so I created this mainly as exercise. The decimal string parser may take three data types as argument: integer string, decimal string and fraction string. First two are converted into float numbers. The fraction string is converted into a fraction number unless the numerator gets divided by denominator (then it is treated as an integer string as well). The decimal converter has been implemented using a state machine.

The Tests directory contains tests for the Libs functionality. All tests have been written by using the framework mentioned at point 1.

3. Final

Although my Lisp knowledge has improved in the last months I'm aware there is still much to learn. If you find specific code sections that might be done better, please let me know by commenting on my Github page. I'm open to any constructive feedback as I'm aware it helps me improve my skills. I'd be even more glad if the code I wrote is useful enough that it can be used by other developers in their projects.

Thank you!
