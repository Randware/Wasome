## Description of the individual crates

### typed_ast
The typed ast is the interface between the middle end and the codegen. <br>
It is self-typechecking and has traversal helpers to make using it easier. <br>
These traversal helpers allow an easy determination of all symbols 
available at a specific point in the ast