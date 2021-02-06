# Nodes

    ---
    NodeType Name
    [input|import statement]
    ---
    
    [output OutputType OutputName]
      [fragments]

Nodes consist of a header and a body. The header of a node begins with three hyphens (---) and is also terminated by it. The first line contains the type of the node (See Node Types) and it's name (The type node is an exception because it's internal type is placed in front of the name). If this line is not present or not the first line an error is raised.

This is optionally followed by either input or import statements (See Sockets) which are only available in default node types.

Node bodies contain the output sections (See Sockets).

## Node Types

    ---
    node Name
    [input|import statement]
    ---



