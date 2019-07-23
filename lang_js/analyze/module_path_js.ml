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
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Resolving paths mentioned in imports (returning a canonical form).
 *
 * You can do:
 *  - import './xxx.js'
 *  - import '../xxx/yyy.js'
 *  - import 'xxx/yyy.js'
 *  - import 'xxx'
 *
 * and this leads to different files on the disk:
 *  - src/xxx.js
 *  - xxx/yyy.js
 *  - node_modules/xxx/yyy.js
 *  - node_modules/xxx/index.js
 *
 * By resolving paths we can have canonical names for imports
 * and so reference the same module entity in codegraph even
 * if the module is represented by different strings.
 * 
 * reference:
 *  - TODO https://nodejs.org/api/modules.html
 *)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let resolve_path ~root ~pwd str =
  let candidates = 
    [Filename.concat root (Filename.concat pwd str);
     Filename.concat root (Filename.concat pwd (spf "%s.js" str));
     Filename.concat root (spf "node_modules/%s" str);
     Filename.concat root (spf "node_modules/%s/index.js" str);

     (* TODO: should look in package.json of package *)
     Filename.concat root (spf "node_modules/%s/lib/index.js" str);
     Filename.concat root (spf "node_modules/%s/lib/api.js" str); (* eslint *)
   ]
  in
  try 
    let found = candidates |> List.find (fun path ->
       Sys.file_exists path && not (Sys.is_directory path)
    )
    in 
    Some (Common.fullpath found)
  with Not_found -> 
    (* TODO: should look in package.json of package
     * in root/package or root/node_modules/package
     *)
   None

              