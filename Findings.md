Missing functionality that I like / am used to having: these may be either my lack of knowledge of the VSCode Idris plugin, a lack of functionality of the VSCode plugin itself, lack of functionality in the Idris language server, lack of functionality in Idris itself.
* 'global' renaming functionality: e.g. when standing on an identifier, invoke an 'idris.rename' command. All its occurrences (references) are selected and when typing a new name, they all get renamed
* inline variable (the inverse of make-lemma)
* more interactive interactions with the compiler
* better :apropos search
* dot ('method call') notation: not (length (reverse mylist)) but (mylist.reverse.length) (although it's easy to define(?))
* where do I find a list of all operators and their fixity?
* grouping functions as methods in a class?
* indicate type of pattern variables in popup
* intellisense is too eager filling in it top suggestion when a local variable with the name under the cursor exists

* refuel x impossible acts weird on PowerSource, Z, 15, Type


* Fin n as k: Nat, pf: k < n
* Vect n a as Fin n -> a
* repl as initialState: a, prompt: a->String, processInput: unchanged
* if P:Type then t: P->a else f: (P->Void)->a