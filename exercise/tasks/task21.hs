

-- Question 1
-- Write a function that takes in a dictionary of type Map String Int, where the first element 
-- is "count": n, and n represents the lenght of the Map. The function should run a Reader monad
-- that is parameterize by this Map and returns a bool, which says weather the count number in 
-- the map is representing the actual length of the Map. Try to use the ask and asks functions.


-- Question 2
-- Write a program that asks the user for his name and generater a HTML document that
-- displays a simple web-page with his name. Use the Reader monad. Below you can see 
-- an example of the HTML document for the user name User1.

-- <!DOCTYPE html>
-- <html lang="en">
--   <body>
--     <h1>Your site</h1><h3>Hello User1!</h3>
--   </body>
-- </html>