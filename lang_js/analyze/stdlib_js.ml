(* Yoann Padioleau
 *
 * Copyright (C) 2019 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 * 
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* This module gives access to the Javascript standard library entities,
 * which is especially useful for Graph_code_js.
 *
 * related: 
 *  - Flow standard lib definitions (small set)
 *    https://github.com/facebook/flow/tree/master/lib
 *  - Typescript standard lib definitions (bigger set)
 *    https://github.com/microsoft/TypeScript/tree/master/src/lib
 *  - TypeScript generator, which generates the .generated.d.ts above
 *    https://github.com/Microsoft/TSJS-lib-generator
 *
 *  - TypeScript definition manager (deprecated)
 *    https://github.com/typings/typings
 *  - High quality Typescript type definitions (looks like the new standard)
 *    http://definitelytyped.org/ especially
 *    https://github.com/DefinitelyTyped/DefinitelyTyped/blob/master/types/node/globals.d.ts
 *
 * src: https://stackoverflow.com/questions/46696266/where-can-i-find-documentation-for-typescripts-built-in-types-and-standard-libr
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
let path_stdlib = 
  Filename.concat Config_pfff.path "data/js_stdlib/stdlib.js"
