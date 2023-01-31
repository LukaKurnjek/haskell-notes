

{-
Question 1
Write a program that simulates the Tic-Tac-Toe game https://en.wikipedia.org/wiki/Tic-tac-toe. 
Instead of two players playing against each other use the System.Random module to randomly 
pick the X and O choices. You do not have to check if any of the players has won the game.
Simply print the board results at the end of the game. Use a State monad to implement the code.

Developer comment (remove after review):
The case when 2 actual players play against each other and check who has won will be a homework
in the next lesson, because you can nicely code this with help of monadic functions.
-}


{-
Question 2
Write a program for creating a shopping list, where the user can add 3 kinds of
flowers. When the program is started it should display the following message:
    Possible flowers are: daisy, sunflower and tulip.
    Possible actions are:
        add  --flower --amount 
        remove --flower --amout 
        show_list 
If the user does not use a valid comamnd and appropriate options notify him about
this. When the user says show_list print a message of how many of which flowers are
added to the list. Use a state monad when coding the solution.
-}



