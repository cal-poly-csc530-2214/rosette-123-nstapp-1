# Rosette 123

I did the first question to learn some basic Rosette, using verify to reduce my code down to a single match statement. I reasoned that if verify found a counterexample that was not empty, that meant that the forumla was a contingency, and if it found no counterexamples or an empty counterexample, then the formula was a tautology or contradiction, respectively. My code can be found in the hw19au/logic/classify.rkt file.

The remainder of my time, about 3.5 hours, I spent on graph coloring. In the graph_coloring.txt file, my answers to question 6 are written using unicode math symbols. I apologize in advance for the difficulty of reading some of the formulas, but I tried to make it as clear as possible. I really enjoyed the graph coloring question, figuring out what propositional constraints are necessary to represent a k-coloring for a graph was challenging and unlike anything I've previously done in math or CS classes. 

I found it especially interesting that constraining each vertex to a single color is unnecessary, as if there are multiple valid colorings for a vertex any one of them will produce a valid k-coloring for the graph. However, I wonder if removing the single color per vertex constraint would hurt performance, as it would place less constraints on the solver and therefore increase the solution space dramatically. Such a performance comparison would be interesting future work, optimization for SAT solvers seems like a very interesting space for research.

I managed to implement a seemingly correct graph coloring encoding in Racket: it successfully colors the graph given in the original example.rkt file, and colors many of the easy graphs in the problems folder as well. My code can be found in the hw19au/graph-coloring/k-coloring.rkt and examples.rkt files. 

My implementation of graph coloring is rather difficult to read, due to its use of nested for/list calls and nested algebraic operations. Roughly, it first generates an OR clause for each vertex representing all possible colorings and then ANDs all the clauses together, ensuring that each vertex is colored at least once. Then, for each vertex, it generates a negative OR clause for every different color pair and ANDs each of these clauses together, ensuring that each vertex is colored at most once. Since colorings of vertices are represented using integers, I used the combinations function to get all unique pairs of colors for each vertex. Then, for each edge in the graph, an OR clause is added to the CNF that ensures that the vertices on that edge are not the same color. Both the first step and this step use nested for/list calls with ranges to iterate through all vertex/color possibilities. Finally, the completed CNF is run through the provided SAT solver, and if the result is a valid solution, it is converted to a vector of numbers representing the coloring of the graph.

Using the original example in the examples.rkt file, the graph ((1 3 6 8) (2 5 7) (4 6 9) (4 5 9) (7 8) (10) (10) (10) (10) (10) ()) has the valid 4-coloring (0 3 1 1 3 2 2 2 2 2 1).