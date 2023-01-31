
{-
This homework is intendet for you to practice all skills you have
learned up until now. Solutions to the questions are provided on the
solutions branch in the folder Monad_projects. 

Question 1
Implement a simplified version of the Wordle game. A description of
the official game can be found here: https://en.wikipedia.org/wiki/Wordle

You are provided with the file animals.txt in the Homework24.5 folder. 
In our version the player has to guess the name of an animal from the list. 
He is beeing told how many characters the name contains. For every guess 
colors for each letter that indicate if the letter is correct (green), 
if it exist in another place (yellow) or if it does not exist at all (white). 

The player has 6 attempts to guess the word. After that a new word is randomly
chosen from the animals.txt file contained in the same folder. Use the Data.Text 
module to implement this game. No need to use a reader, writer or state monad. 

Question 2
Use the code from the previous question and update it such that it saves the 
result to a file. If the game is interrupted and later resumed the intermediat 
result is loaded from the file and the player can continue the game where he 
left it. Use the Data.Aeson and Data.ByteString.Lazy module to achive this.

Question 3
Implement a snake game in Haskell. You can check how the game is played here:
http://www.papg.com/show?3AE4

In our game two players compete against each other instead of a player competing 
against a computer. Use the Control.Monad.State module to implement the game. 

The initial positions of the players should be selected randomly. Valid moves 
for a player are set with keyboard keys: a (left), d (right), w (up), s (down).

-}

