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

--- *********Kolla om o's ID och  d's ID finns i DAGEN och kolla om det blir en cycel i add_edge

-- add_edge(a,b,w)
-- An edge from the vertex with vertex identifier a to the vertex
-- with vertex identifier b is added to the DAG with weight w.
add_edge :: DAG v e -> Int -> Int -> e -> DAG v e
add_edge (DAG dag) o d w | check_vertex_in_dag (DAG dag) o &&  check_vertex_in_dag (DAG dag) d = DAG (fst dag, snd dag ++ [create_edge w o d]) 
						 | otherwise = (DAG dag)
	--DAG (fst dag, snd dag ++ [create_edge w o d]) 


-- add_edge (DAG dag) o d w = DAG (fst dag, snd dag ++ [create_edge w o d]) 



-- vlist = topological_ordering
-- Computes a topological ordering of the DAG and returns the corresponding 
-- list of vertex identifiers.
-- Recall that a topological ordering is a linear ordering of a directed graph
-- such that if the vertices of the graph are layed out from left to right in that order, 
-- then all edges go from left to right.





