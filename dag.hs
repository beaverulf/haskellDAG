-- Programspråk (5DV086)
-- Laboration 2
--
--
-- Authors:   Jonas Nyman, c13jnn  
--			  Björn Lidell, c13bll	

data Vertex = Vertex { weight :: Atom  
                     } deriving (Show)


data Edge = Edge { weight :: Atom
				   ,origin :: Vertex
				   ,destination :: Vertex
				 } deriving (Show)


 add_vertex :: a -> b


