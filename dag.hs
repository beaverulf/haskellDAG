-- Programspråk (5DV086)
-- Laboration 2
--
--
-- Description   :  Implementation of a DAG (directed asyclic graph) in haskell.
-- 					Functions to use the DAG is: add_vertex 
-- File          :  dag.hs
-- Version       :  1.0 
-- Authors       :  Jonas Nyman, c13jnn ,
--			  		Björn Lidell, c13bll	


-- Representation of a DAG, holds a tuple with a list of Vertexes
-- and a list of Edges.
data DAG v e = DAG ([Vertex v], [Edge e]) deriving (Show, Read)

-- Representation of a Vertex, has a weight and an id.
-- Takes a v to be a weight.
data Vertex v = Vertex { vWeight :: v   -- Weight
					   , vid :: Int     -- ID
                       } deriving (Show, Read)

-- Representation of an Edge, has a weight and IDs for
-- the origin vertex and the destination vertex.
-- Takes e to be the weight.
data Edge e = Edge { eWeight :: e   	    -- Weight
				   , origin :: Int  		-- Origin (ID)
				   , destination :: Int     -- Dest (ID)
				   } deriving (Show, Read)


-- Generates an ID for a vertex,
-- the ID returned is the lenght of the vertex list
-- in the DAG. 
generateID :: DAG v e -> Int   
generateID (DAG dag)   
		| null $ fst dag = 0
		| otherwise = length $ fst dag
        

-- Creates a DAG with two empty lists.
create_dag :: DAG a b
create_dag = DAG([],[])


-- Creates a Vertex.
-- Arguments: DAG dag: A dag in order to generate an ID for the vertex.
--                  w: Weight of the vertex.
-- Returns: Vertex 
create_vertex :: DAG v d -> v -> Vertex v
create_vertex (DAG dag) w = Vertex {vWeight = w, vid = id}
							where id = generateID $ DAG dag 

-- Get vertex ID.
getID :: Vertex v -> Int
getID (Vertex _ i ) = i



-- Creates an Edge
-- Arguments: w : Weight of the edge.
--            o : ID of the origin vertex.
--            d : ID of the destination vertex.
-- Returns: Edge. 
create_edge :: e -> Int -> Int -> Edge e
create_edge w o d = Edge {eWeight = w, origin = o, destination = d}


-- **************************************************************************************
insert_edge :: DAG vl el -> Edge el -> DAG vl el -- vikter, inte lista               
insert_edge (DAG dag) new_edge = DAG (fst dag, snd dag ++ [new_edge])                
																					 
-- Insert a Vertex into the DAG                                                      
insert_vertex :: DAG vl el -> Vertex vl -> DAG vl el                                 
insert_vertex (DAG dag) new_vertex = DAG (fst dag ++ [new_vertex], snd dag)          
-- **************************************************************************************


-- Adds a vertex to the DAG by generating the id of the vertex
-- and takes the DAG and inserts a new vertex using create_vertex into
-- the vertex list of a new DAG and taking the edge list of the old dag
-- and place it in the new edge list of the new DAG.
--
-- Arguments: DAG : Old dag to have a vertex inserted into.
-- 			  	w : Weight of the vertex to be inserted.
-- Returns: Tuple of Vertex ID and new DAG with the vertex inserted into.
add_vertex :: DAG v e -> v -> (Int, DAG v e)
add_vertex (DAG dag) w = (id, DAG (fst dag ++ [create_vertex (DAG dag) w], snd dag))
						  where id = generateID $ DAG dag


--check_cycle :: DAG v e -> Bool
--check_cycle (DAG dag) = 


-- Checks if a Vertex with ID a exists in a vertex list.
exists_vertex :: [Vertex v] -> Int -> Bool  
exists_vertex [] _ = False 
exists_vertex (v:xs) a = (getID v == a) || exists_vertex xs a

-- Extract the vertex list from the dag.
extract_vertexlist :: DAG v e -> [Vertex v]
extract_vertexlist (DAG dag) = fst dag

-- Exctracts the edge list from the dag
extract_edgelist :: DAG v e -> [Edge e]
extract_edgelist (DAG dag) = snd dag


check_vertex_in_dag :: DAG v e -> Int -> Bool
check_vertex_in_dag (DAG dag) a = exists_vertex (fst dag) a
	--(DAG ([],_))


-- origin $ head på listan ger origin 

--test_filter :: [Edge e] -> Int -> Bool
--test_filter [] _ = False
--test_filter (o:xs) id | (origin o == id) = True
				--	  | otherwise test_filter xs id	


-- extract the origin vertex from an edge
take_edge :: Edge e -> Int
take_edge edge = origin edge

-- extract the destination vertex from an edge
take_orig :: Edge e -> Int
take_orig edge = destination edge

-- get all origins vertexes from an edgelist in a DAG
get_origins :: [Edge e] -> [Int]
get_origins (xs) = map take_edge xs

--get list of all origin vertexes id from a DAG
get_olist :: DAG v e -> [Int]
get_olist (DAG dag) = map take_edge $ snd dag

--get list of all destination vertexes id from a DAG
get_dlist :: DAG v e -> [Int]
get_dlist (DAG dag) = map take_orig $ snd dag

--get_dests :: [Int] -> [Int] -> Int -> [Int]
--get_dests [] [] _ = []
--get_dests (x:xs) (y:ys) id | (x == id) = y : get_dests xs ys id

-- slå ihop två origin/destination listor till en lista av tupler
make_tuple :: [Int] -> [Int] -> Int -> [(Int, Int)]
make_tuple []     _   _   = []
make_tuple (x:xs) (y:ys) id | (x == id) = (x, y) : make_tuple xs ys id
						 | otherwise = make_tuple xs ys id

-- Ta ut destinationerna frå make_tuple 
get_dests :: [(Int,Int)] -> [Int]
get_dests [] = []
get_dests (x:xs) = snd x : get_dests xs

-- få ut en lista med destinationer från origin id
get_dest :: [Int] -> [Int] -> Int -> [Int]
get_dest x y id = get_dests $ make_tuple x y id

part_list :: [Int] -> [Int] -> Bool
part_list [] _ = False
part_list (x:xs) l1 | (x `elem` l1)  = True
					| otherwise = part_list xs l1



-- gör id till lista 
--is_cycle :: [Int] -> [Int] -> [Int] -> [Int] -> Bool
--is_cycle _ _ [] _ = False
--is_cycle ol dl cd id | (part_list id cd == True) = True
--			         | otherwise = is_cycle ol dl (tail $ cd ++ ncd) (head cd : id) -- (head cd : id) blir nog fel se (*) i papperet, jo det var det
--			         where ncd = get_dest ol dl $ head cd
			--		| is_cycle ol dl tail $ (get_dest ol dl $ head cd) ++ cd id 

--den övre verionen är sämst och komer inte att fungera , problemet med denna är att
-- det kan bli en oändlig loop. Men måste typ göra som pseudo koden och hålla reda på
-- vilka vägar man har tagit. Lika bra att försöka göra en topological sort och inse om det blir cycel
is_cycle :: [Int] -> [Int] -> [Int] -> Int -> Bool
is_cycle _ _ [] _ = False
is_cycle ol dl cd id | (id `elem` cd) = True
			         | otherwise = is_cycle ol dl (tail $ cd ++ ncd) id -- vissa cycler lägger till samma sak här
			         where ncd = get_dest ol dl $ head cd
			--		| is_cycle ol dl tail $ (get_dest ol dl $ head cd) ++ cd id 

--	is_cycle _ _ [] _ = False
--is_cycle ol ol cd id | (id `elem` currdes) = True				
 --otherwise = is_cycle ol dl tail $ (get_dest ol dl $ head cdis_) ++ cd id 
					--__ allori alldes  (get_dest allori alldes head currdes)



-- Måste göra reverse på vertexlistan, kanske funkarrr!!!!

getIDl :: Vertex v -> [Int]
getIDl (Vertex _ i ) = [i]

check_cycle :: DAG v e -> [Vertex v] -> Bool
check_cycle (DAG dag) [] = False
check_cycle (DAG dag) (x:xs) | (is_cycle (getol) (getdl) (getdes) (getID x) == True) = True
							 | otherwise = check_cycle (DAG dag) xs	
							     where getol = get_olist (DAG dag)
          							   getdl = get_dlist (DAG dag)
          							   getdes = get_dest (getol) (getdl) (getID x)





get_destlist :: [(Int,Int)] -> Int -> [Int]
get_destlist [] _ = []
get_destlist (x:xs) id | (snd x == id) = fst x : get_destlist xs id

--get_incomming :: Int -> [Vertex v] -> [Int]
--get_incomming d [] = []
--get_incomming d (x:xs) =   

--L ← Empty list that will contain the sorted nodes
--while there are unmarked nodes do
--    select an unmarked node n
--    visit(n) 
--topo :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
--topo vl unmar tmar pmar top | (null unmar) = top
--							| otherwise = visitt (head unmar) (vl) (tail unmar) (tmar) (pmar) (top)
	--do if (null unmar) then (return top) else visitt x vl unmar tmar pmar top

--visitt :: Int -> [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Int]
--visitt node vl unmar tmar pmar top | (n `elem` unmar) = 

--function visit(node n)
  --  if n has a temporary mark then stop (not a DAG) (n `elem` tmar) = töm unmar och top 
   -- if n is not marked (i.e. has not been visited yet) then
     --   mark n temporarily
      --  for each node m with an edge from n to m do
      --      visit(m)
     --   mark n permanently
     --   unmark n temporarily
     --   add n to head of L

					

							 
-- kolla om d har o som dest 
check_if_o_is_d :: DAG v e -> Int -> Int -> Bool
check_if_o_is_d (DAG dag) o d | (o  `elem` getdes) = False
							  | otherwise = True
							   where getol = get_olist (DAG dag)
          						 	 getdl = get_dlist (DAG dag)
          						  	 getdes = get_dest (getol) (getdl) (d)



 

--check_cycle (DAG dag) (x:[]) = is_cycle (get_olist (DAG dag)) (get_dlist (DAG dag)) (get_dest (get_olist (DAG dag)) (get_dlist (DAG dag)) (getID x)) (getID x)
--check_cycle (DAG dag) (x:xs) = check_cycle (DAG dag) xs 

-- check_vertex_in_dag (DAG dag) (getID x)

	--is_cycle (get_olist (DAG dag)) (get_dlist dag) (get_dest (get_olist dag) (get_dlist dag) x) x


-- add_edge(a,b,w)
-- An edge from the vertex with vertex identifier a to the vertex
-- with vertex identifier b is added to the DAG with weight w.
add_edge :: DAG v e -> Int -> Int -> e -> DAG v e
add_edge (DAG dag) o d w | check_vertex_in_dag (DAG dag) o &&  check_vertex_in_dag (DAG dag) d && check_if_o_is_d (DAG dag) (o) (d) && not (check_cycle (newdag) (reverse $ extract_vertexlist newdag)) = DAG (fst dag, snd dag ++ [create_edge w o d]) 
						 | otherwise = (DAG dag)
						 where newdag = DAG (fst dag, snd dag ++ [create_edge w o d])
						 		   
						 	 -- chcy = check_cycle (newdag) (reverse $ extract_vertexlist newdag)

						 --where chcy = check_cycle (DAG (fst dag, snd dag ++ [create_edge w o d]) reverse )
	--DAG (fst dag, snd dag ++ [create_edge w o d]) 

-- && check_cycle (DAG (fst dag, snd dag ++ [create_edge w o d]))

-- add_edge (DAG dag) o d w = DAG (fst dag, snd dag ++ [create_edge w o d]) 



-- vlist = topological_ordering
-- Computes a topological ordering of the DAG and returns the corresponding 
-- list of vertex identifiers.
-- Recall that a topological ordering is a linear ordering of a directed graph
-- such that if the vertices of the graph are layed out from left to right in that order, 
-- then all edges go from left to right.










