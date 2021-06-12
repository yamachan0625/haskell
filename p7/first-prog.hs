messyMain :: IO()
messyMain = do 
  print "who is the email for?"
  recipient <- getLine 
  print "What is the title"
  title <- getLine 
  print "Who is the Author"
  author <- getLine
  print ("Dear " ++ recipient ++ ", \n" ++ "Thanks for buying " ++ title ++"\nthanks, \n" ++ author)