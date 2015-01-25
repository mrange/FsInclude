// ### INCLUDE: Common.fs

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

[<AbstractClass>]
type BaseDisposable() =
    [<DefaultValue>]
    val mutable isDisposed : int

    interface System.IDisposable with
        member x.Dispose () =
            if System.Threading.Interlocked.Exchange (&x.isDisposed, 1) = 0 then
                try
                    x.OnDispose ()
                with
                | e -> Log.errorf "%s.Dispose () threw exception: %A" "" e

    abstract OnDispose: unit -> unit

type ActionDisposable(action : unit -> unit) =
    inherit BaseDisposable()

    override x.OnDispose () = action ()

module Disposable =
    let onExitDo (action : unit -> unit) : System.IDisposable = upcast new ActionDisposable(action)

