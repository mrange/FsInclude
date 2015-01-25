// ### INCLUDE: ../Common/Common.fs

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

[<AutoOpen>]
module BasicExtensions =

    open System
    open System.Collections.Generic

    type IEnumerable<'T> with

        member x.ToDictionary_KeepLast<     'TKey
                                        ,   'TValue
                                        when 'TKey : equality
                                        > (     keyExtractor    : Func<'T, 'TKey>
                                            ,   valueExtractor  : Func<'T, 'TValue>
                                            ) : Dictionary<'TKey, 'TValue> =
            let dic = Dictionary<'TKey, 'TValue>()

            for v in x do
                dic.[keyExtractor.Invoke v] <- valueExtractor.Invoke v

            dic

    type IDictionary<'TKey, 'TValue> with

        member x.GetOrDefault (key : 'TKey, defaultValue : 'TValue) =
            let mutable value = defaultValue
            if x.TryGetValue (key, &value) then
                value
            else
                defaultValue

        member x.GetOrAdd (key : 'TKey, addValue : 'TValue) =
            let mutable value = addValue
            if x.TryGetValue (key, &value) then
                value
            else
                x.Add (key, addValue)
                addValue

        member x.GetOrAdd (key : 'TKey, addValueGenerator : Func<'TKey, 'TValue>) =
            let mutable value = Unchecked.defaultof<'TValue>
            if x.TryGetValue (key, &value) then
                value
            else
                value <- addValueGenerator.Invoke key
                x.Add (key, value)
                value


