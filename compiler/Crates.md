## Description of the individual crates

### ast
The ast has two variants, who are implemented with generics: <br>
1. Typed ast <br>
Is the interface between the middle end and the codegen. <br>
It is also self-typechecking <br>
2. Untyped ast <br>
This one is more barebones and uses strings where a type would otherwise be <br>

There are traversal helpers, who are available for both types <br>
These traversal helpers allow an easy determination of all symbols 
available at a specific point in the ast

### shared
All code that doesn't belong anywhere else goes in here