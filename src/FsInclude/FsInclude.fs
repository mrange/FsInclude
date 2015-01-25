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

module Processor = 

    open System
    open System.Collections.Generic
    open System.IO
    open System.Net
    open System.Text
    open System.Text.RegularExpressions

    type Lines      = ResizeArray<string>
    type Resolver   = string -> seq<string>
    type Blacklister= string -> bool
    type Appender   = int -> string -> unit

    type ProcessContext =
        {
            ParentNamespaces: string []
            Resolver        : Resolver
            Blacklister     : Blacklister
            Prelude         : Lines
            Content         : Lines
            Includes        : ResizeArray<string*string>
            SeenIncludes    : HashSet<string>
            SeenNamespaces  : Dictionary<string, string>
        }

        member x.ParentNamespace = String.Join (".", x.ParentNamespaces)

        static member New pn r b p c = 
                        { 
                            ParentNamespaces= pn
                            Resolver        = r 
                            Blacklister     = b 
                            Prelude         = p 
                            Content         = c 
                            Includes        = ResizeArray<_> ()
                            SeenIncludes    = HashSet<_> (StringComparer.OrdinalIgnoreCase)
                            SeenNamespaces  = Dictionary<_, _> (StringComparer.OrdinalIgnoreCase)
                        }

    type LineAction = ProcessContext -> string -> Match -> unit

    let rec LineActions : (Regex*LineAction) [] = 
        let makeRegex s = Regex (s, RegexOptions.Compiled ||| RegexOptions.ExplicitCapture ||| RegexOptions.Singleline)

        let includeAction (context : ProcessContext) (l : string) (m : Match) : unit =
            let ref = m.Groups.["ref"].Value
            
            if context.SeenIncludes.Contains ref then
                context.Content.Add <| sprintf "// @@@ SKIPPED_INCLUDE (Already seen): %s" ref
            else
                ignore <| context.SeenIncludes.Add ref
                context.Content.Add <| sprintf "// @@@ INCLUDE: %s" ref
                let lines = context.Resolver ref
                AppendLines context lines

        let namespaceAction (context : ProcessContext) (l : string) (m : Match) : unit =
            if context.ParentNamespaces.Length = 0 then
                context.Content.Add l
            else
                let ws  = m.Groups.["ws"].Value
                let ns  = m.Groups.["ns"].Value

                let success, seen = context.SeenNamespaces.TryGetValue ns
                let result = 
                    if success then 
                        seen
                    else
                        let parent  = context.ParentNamespace

                        let result = 
                            if ns.StartsWith ("global", StringComparison.Ordinal) then
                                parent
                            else
                                sprintf "%s.%s" parent ns

                        context.SeenNamespaces.Add (ns, result)

                        result

                context.Content.Add <| sprintf "%snamespace %s" ws result

        let openAction (context : ProcessContext) (l : string) (m : Match) : unit =
            let ws  = m.Groups.["ws"].Value
            let ns  = m.Groups.["ns"].Value

            let success, seen = context.SeenNamespaces.TryGetValue ns
            let result = 
                if success then 
                    seen
                else
                    ns

            context.Content.Add <| sprintf "%sopen %s" ws result

        let defaultAction (context : ProcessContext) (l : string) (m : Match) : unit =
            context.Content.Add l

        [|
            """^.*$"""                                          , defaultAction
            """^\s*//\s+###\s*INCLUDE:\s*(?<ref>\S+)\s*.*$"""   , includeAction
            """^(?<ws>\s*)namespace\s+(?<ns>\S+)\s*.*$"""       , namespaceAction
            """^(?<ws>\s*)open\s+(?<ns>\S+)\s*.*$"""            , openAction
        |] |> Array.map (fun (r,a) -> makeRegex r, a)

    and MatchLineActions (context : ProcessContext) (line : string) : int -> unit = function
        | 0 -> ()
        | i -> 
            let r,la = LineActions.[i - 1]
            let m = r.Match line
            if m.Success then
                la context line m
            else
                MatchLineActions context line (i-1)
    and AppendLines (context : ProcessContext) (lines : seq<string>) : unit =
        for line in lines do
            MatchLineActions context line LineActions.Length

    let ProcessLines (parentNamespaces : string []) (appender : Appender) (resolver : Resolver) (blacklister : Blacklister) (lines : seq<string>) : unit =
        let prelude     = Lines()
        let content     = Lines()
        let context     = ProcessContext.New parentNamespaces resolver blacklister prelude content

        AppendLines context lines

        let indent = 4
        
        for line in prelude do
            appender (0*indent) line

        for line in content do
            appender (0*indent) line

        if parentNamespaces.Length > 0 then
            appender (0*indent) <| sprintf "namespace %s" context.ParentNamespace
            
        appender (0*indent) "module IncludeMetaData = "

        appender (1*indent) <| sprintf """[<Literal>] let IncludeDate = "%s" """ (DateTime.Now.ToString "yyyyMMddTHH:mm:ss")

        context.Includes 
            |> Seq.iteri 
                (fun i (r,f) ->
                    appender (1*indent) <| sprintf """[<Literal>] let Include_%d = @"%s,%s" """ i r f
                )
                

    let ProcessWebFiles (parentNamespaces : string []) (baseUri : Uri) (paths : string []) (appender : Appender) (blacklister : Blacklister) : unit =
        
        let downloadDocument (path : string) = 
            let uri             = Uri (baseUri, path)
            use wc              = new WebClient ()
            let input           = wc.DownloadString (uri)

            seq {
                use stringReader    = new StringReader (input)
                yield sprintf "// @@@ BEGIN_DOCUMENT: %s" path
                let line            = ref ""
                while (line := stringReader.ReadLine(); !line <> null) do
                    yield !line
                yield sprintf "// @@@ END_DOCUMENT: %s" path
            }

        let lines = 
            paths
            |> Seq.collect downloadDocument

        ProcessLines parentNamespaces appender downloadDocument blacklister lines

    let DownloadWebFilesAsString (enclosingNamespace : string option) (baseUri : Uri) (paths : string []) : string =
        let sb = StringBuilder ()

        let appender n s = 
            ignore <| sb.Append (' ', n)
            ignore <| sb.AppendLine s

        ProcessWebFiles (enclosingNamespace |> Option.toArray) baseUri paths appender (fun p -> true)

        sb.ToString ()

    let DownloadWebFilesToStream (sw : StreamWriter) (enclosingNamespace : string option) (baseUri : Uri) (paths : string []) : unit =
        let appender n (s : string) = 
            for n in 1..n do
                sw.Write ' '

            sw.WriteLine s

        ProcessWebFiles (enclosingNamespace |> Option.toArray) baseUri paths appender (fun p -> true)


