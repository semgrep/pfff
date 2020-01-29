# External Java Parsers

We are working on incorporating tree-sitter-java grammar into pfff. 

### Writing OCaml

To compile OCaml code

```
$ ocamlc -o <executable> <filename>.ml
$ ./<executable>
```

To compile all of pfff, make sure you have OCaml 4.07.1 and ran `eval $(opam env)`. 

```
$ cd ../..
$ ./configure && make depend && make && make opt
$ ./pfff <your commands>
```

### Running

You can run the following commands to output a JSON of the AST parsed from a `.java` file by tree-sitter.

```
$ cd tree_sitter
$ npm install tree-sitter
$ git clone https://github.com/tree-sitter/tree-sitter-java/
$ cd tree-sitter-java && npm install 
$ cd ..
$ node tree-sitter-parser.js <path/to/java/file>
```

You can run the following commands to output a JSON of the AST parsed from a `.java` file by Babelfish. Since we no longer support Babelfish, this may not be complete. You must have Docker already installed.

```
$ cd tree_sitter
$ pip3 install bblfsh
$ docker run --privileged --rm -it -p 9432:9432 -v bblfsh_cache:/var/lib/bblfshd --name bblfshd bblfsh/bblfshd
$ docker exec -it bblfshd bblfshctl driver install python bblfsh/python-driver
$ python bblfsh-parser.py <path/to/java/file>
```

The following command allows you to run tree-sitter-java across a repository and outputs the number of `.java` files that fails parsing. It also generates a corpus of JSON files from the files that were parsed.

```
bash tree-sitter-parser <FILES_TO_SEARCH>
```

### Todo

- Include support for language selection

### Sources

- [Babelfish Python Client](https://github.com/bblfsh/python-client)
- [tree-sitter](https://github.com/tree-sitter/tree-sitter)
- [tree-sitter-java](https://github.com/tree-sitter/tree-sitter-java/)