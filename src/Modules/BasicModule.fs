// Copyright 2015 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

namespace FsInclude

module Opt =

    let lift1 
        (c  : 'T0 -> 'T ) 
        (v0 : 'T0 option) 
        : 'T option =
        match v0 with
        | Some s0 -> Some <| c s0
        | _ -> None

    let lift2
        (c  : 'T0 -> 'T1 -> 'T ) 
        (v0 : 'T0 option) 
        (v1 : 'T1 option) 
        : 'T option =
        match v0, v1 with
        | Some s0, Some s1 -> Some <| c s0 s1
        | _ -> None

    let lift3
        (c  : 'T0 -> 'T1 -> 'T2 -> 'T ) 
        (v0 : 'T0 option) 
        (v1 : 'T1 option) 
        (v2 : 'T2 option) 
        : 'T option =
        match v0, v1, v2 with
        | Some s0, Some s1, Some s2 -> Some <| c s0 s1 s2
        | _ -> None
