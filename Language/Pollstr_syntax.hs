
Data Question   = Question String deriving Show
Data Response a = Response [a] deriving Show

Data Item = Item (Question, Response) deriving Show