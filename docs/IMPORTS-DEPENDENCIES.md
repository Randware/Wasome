# Introduction

This document outlines the concrete implementation of imports and dependencies.
It also includes Module Paths, which are a concept used in later sections.

## 1. Modules

Modules are a unit of code. They are used throughout wasome, for example, for imports.
Modules are abstract representations of directories and share their properties, including
the need for a name and the ability to contain submodules.

## 2. Module Paths

### 2.1 Purpose

`Module Paths` are references to modules, whether local or external to the current project.
These paths are used throughout the rest of this document to describe import behavior.

### 2.2 Syntax

`Module Paths` use the following syntax: 
`<Origin>/<Path relative to origin>`

The origin is one of the following: 
1. `.`: Relative to the current directory.
2. `<Projectname>`: Relative to the current project
3. `<Name of an external dependency>`: Relative to an external dependency (described later in this document)

Path relative to origin consists of any directories that need to be traversed, separated by slashes.
It may also be empty if there is nothing to be traversed. In this case, the first slash may be omitted.

With the exception of the dot and slash when used as described above, only alphanumerical characters, underscores and hyphens, 
with the latter two being forbidden at either the start or end of a part of a part, are allowed in `Module Paths`.
This also means that it is impossible to reference hidden directories starting with a "." with module paths.
For example, the following specifies `trigonometry` from the `floating_point` directory from the `math` project:
`math/floating_point/trigonometry`

## 3. Dependencies

### 3.1 Purpose

Wasome allows projects to depend on other projects. 
While this proposal doesn't specify how to declare them, it specifies their usage. 

### 3.2 Interface of the compiler

The compiler should be provided with a list of all dependencies. 
These are represented by a vec of dependency structs. A dependency struct looks like this:
`struct Dependency {
    name: String,
    path: PathBuf
}`

Name is the name of the dependency, and path is an operating-system-dependent path to the directory containing the source code.

### 3.3 Converting to filepaths

There needs to be a functionality that converts `Module Paths` into filepaths by using the dependency list. 
It will be used by the Semantic Analyzer to correctly translate paths provided in imports.

### 3.4 Representation in the AST

In the AST, each dependency and the project itself has its own directory in which all code is located.
Note that this directory is virtual and does not exist on disk.
This allows imports to be processed without much effort.

### 3.6 Transitive dependencies

Each project must specify its dependencies on its own and can't
directly use ones from other projects it depends on. For example, if project A depends on project B and project B depends on project C, then project A
can't access project C without first depending on it itself.

### 3.5 Other

Any project can be an external dependency. While any project with a main function can be run on its own, they too can be depended on.

This also means that dependencies can have dependencies themselves. Furthermore, dependency cycles are permitted and do not need special

handling from the user. Multiple dependencies with the same name aren't supported. Dependency versioning isn't supported.

## 4. Imports

### 4.1 Definition

By default, symbols are only visible in the file they were declared in. Imports make it possible to change that.
This means that it is required to import the current module in order to access symbols in other files in the current module.
They must be positioned at the top of the source file wanting to use the symbol and bring them into scope.

Imports always work on an entire module. In other words, it is only possible to import full modules and not only parts of them.
Only symbols marked as `pub` (public) can be accessed via imports.
Attempting to use symbols not declared as `pub` leads to a compiler error.

### 4.2 Syntax

Imports follow the following syntax:
`import "<Path>"`
Where Path is the path of the module being imported. It follows the layout of `Module Path` as described above.

For example, the following imports the module `trigonometry` from the math project:
`import "math/floating_point"`

Each import is assigned a usage name, which is used when accessing imported symbols.
By default, this is the module name but can be overridden (more on that later).
Having multiple imports with the same usage location is a compiler error.

### 4.3 Usage of imported symbols

Symbols inside imported modules are accessed via the following syntax:

`<Usage Name>.<Symbol>`

For example, this accesses the `sin_f64` function inside the trigonometry file:
`floating_point.sin_f64(10.0)`

### Overriding of usage name

It is also possible to override the usage name with the as-syntax:
`import "\<Filepath\>" as \<Usage Name\>`

For example, this imports the floating_point module as fp:
`import "math/floating_point" as fp`

It can now be used like this:
`fp.sin_f64(10.0)`

### Cyclic imports

Imports may be cyclic without having to follow any special rules.
