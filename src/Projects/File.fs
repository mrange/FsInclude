// @@@ BEGIN_DOCUMENT: Lexing.fs
// (c) Microsoft Corporation 2005-2009.

#nowarn "47" // recursive initialization of LexBuffer


#if INTERNALIZED_FSLEXYACC_RUNTIME
namespace Included.Internal.Utilities.Text.Lexing

#else
namespace Included.Microsoft.FSharp.Text.Lexing
#endif

    open System.Collections.Generic

    // REVIEW: This type showed up on a parsing-intensive performance measurement. Consider whether it can be a struct-record later when we have this feature. -jomo
#if INTERNALIZED_FSLEXYACC_RUNTIME
    type internal Position = 
#else
    type Position = 
#endif
        { pos_fname : string;
          pos_lnum : int;
#if INTERNALIZED_FSLEXYACC_RUNTIME
          pos_orig_lnum : int;
#endif
          pos_bol : int;
          pos_cnum : int; }
        member x.FileName = x.pos_fname
        member x.Line = x.pos_lnum
#if INTERNALIZED_FSLEXYACC_RUNTIME
        member x.OriginalLine = x.pos_orig_lnum
#endif
        member x.Char = x.pos_cnum
        member x.AbsoluteOffset = x.pos_cnum
        member x.StartOfLine = x.pos_bol
        member x.StartOfLineAbsoluteOffset = x.pos_bol
        member x.Column = x.pos_cnum - x.pos_bol
        member pos.NextLine = 
            { pos with 
#if INTERNALIZED_FSLEXYACC_RUNTIME
                    pos_orig_lnum = pos.OriginalLine + 1;
#endif
                    pos_lnum = pos.Line+1; 
                    pos_bol = pos.AbsoluteOffset }
        member pos.EndOfToken(n) = {pos with pos_cnum=pos.pos_cnum + n }
        member pos.AsNewLinePos() = pos.NextLine
        member pos.ShiftColumnBy(by) = {pos with pos_cnum = pos.pos_cnum + by}
        static member Empty = 
            { pos_fname=""; 
              pos_lnum= 0; 
#if INTERNALIZED_FSLEXYACC_RUNTIME
              pos_orig_lnum = 0;
#endif
              pos_bol= 0; 
              pos_cnum=0 }
        static member FirstLine(filename) = 
            { pos_fname=filename; 
#if INTERNALIZED_FSLEXYACC_RUNTIME
              pos_orig_lnum = 1;
#endif
              pos_lnum= 1; 
              pos_bol= 0; 
              pos_cnum=0 }

#if INTERNALIZED_FSLEXYACC_RUNTIME
    type internal LexBufferFiller<'char> = 
#else
    type LexBufferFiller<'char> = 
#endif
        { fillSync : (LexBuffer<'char> -> unit) option
          fillAsync : (LexBuffer<'char> -> Async<unit>) option } 
        
    and [<Sealed>]
#if INTERNALIZED_FSLEXYACC_RUNTIME
        internal LexBuffer<'char>(filler: LexBufferFiller<'char>) as this = 
#else
        LexBuffer<'char>(filler: LexBufferFiller<'char>) as this = 
#endif
        let context = new Dictionary<string,obj>(1) in 
        let extendBufferSync = (fun () -> match filler.fillSync with Some refill -> refill this | None -> invalidOp "attempt to read synchronously from an asynchronous lex buffer")
        let extendBufferAsync = (fun () -> match filler.fillAsync with Some refill -> refill this | None -> invalidOp "attempt to read asynchronously from a synchronous lex buffer")
        let mutable buffer=[||];
        /// number of valid charactes beyond bufferScanStart 
        let mutable bufferMaxScanLength=0;
        /// count into the buffer when scanning 
        let mutable bufferScanStart=0;
        /// number of characters scanned so far 
        let mutable bufferScanLength=0;
        /// length of the scan at the last accepting state 
        let mutable lexemeLength=0;
        /// action related to the last accepting state 
        let mutable bufferAcceptAction=0;
        let mutable eof = false;
        let mutable startPos = Position.Empty ;
        let mutable endPos = Position.Empty

        // Throw away all the input besides the lexeme 
              
        let discardInput () = 
            let keep = Array.sub buffer bufferScanStart bufferScanLength
            let nkeep = keep.Length 
            Array.blit keep 0 buffer 0 nkeep;
            bufferScanStart <- 0;
            bufferMaxScanLength <- nkeep
                 
              
        member lexbuf.EndOfScan () : int =
            // Printf.eprintf "endOfScan, lexBuffer.lexemeLength = %d\n" lexBuffer.lexemeLength;
            if bufferAcceptAction < 0 then 
                failwith "unrecognized input"

            //  Printf.printf "endOfScan %d state %d on unconsumed input '%c' (%d)\n" a s (Char.chr inp) inp;
            //   Printf.eprintf "accept, lexeme = %s\n" (lexeme lexBuffer); 
            lexbuf.StartPos <- endPos;
            lexbuf.EndPos <- endPos.EndOfToken(lexbuf.LexemeLength);
            bufferAcceptAction

        member lexbuf.StartPos
           with get() = startPos
           and  set(b) =  startPos <- b
           
        member lexbuf.EndPos 
           with get() = endPos
           and  set(b) =  endPos <- b

        member lexbuf.Lexeme         = Array.sub buffer bufferScanStart lexemeLength
        member lexbuf.LexemeChar(n)  = buffer.[n+bufferScanStart]
        
        member lexbuf.BufferLocalStore = (context :> IDictionary<_,_>)
        member lexbuf.LexemeLength        with get() : int = lexemeLength    and set v = lexemeLength <- v
        member internal lexbuf.Buffer              with get() : 'char[] = buffer              and set v = buffer <- v
        member internal lexbuf.BufferMaxScanLength with get() = bufferMaxScanLength and set v = bufferMaxScanLength <- v
        member internal lexbuf.BufferScanLength    with get() = bufferScanLength    and set v = bufferScanLength <- v
        member internal lexbuf.BufferScanStart     with get() : int = bufferScanStart     and set v = bufferScanStart <- v
        member internal lexbuf.BufferAcceptAction  with get() = bufferAcceptAction  and set v = bufferAcceptAction <- v
        member internal lexbuf.RefillBuffer = extendBufferSync
        member internal lexbuf.AsyncRefillBuffer = extendBufferAsync

        static member LexemeString(lexbuf:LexBuffer<char>) = 
            new System.String(lexbuf.Buffer,lexbuf.BufferScanStart,lexbuf.LexemeLength)

        member lexbuf.IsPastEndOfStream 
           with get() = eof
           and  set(b) =  eof <- b

        member lexbuf.DiscardInput() = discardInput ()

        member x.BufferScanPos = bufferScanStart + bufferScanLength

        member lexbuf.EnsureBufferSize n = 
            if lexbuf.BufferScanPos + n >= buffer.Length then 
                let repl = Array.zeroCreate (lexbuf.BufferScanPos + n) 
                Array.blit buffer bufferScanStart repl bufferScanStart bufferScanLength;
                buffer <- repl

        static member FromReadFunctions (syncRead : ('char[] * int * int -> int) option, asyncRead : ('char[] * int * int -> Async<int>) option) : LexBuffer<'char> = 
            let extension= Array.zeroCreate 4096
            let fillers = 
                { fillSync = 
                    match syncRead with 
                    | None -> None
                    | Some read -> 
                         Some (fun lexBuffer -> 
                             let n = read(extension,0,extension.Length)
                             lexBuffer.EnsureBufferSize n;
                             Array.blit extension 0 lexBuffer.Buffer lexBuffer.BufferScanPos n;
                             lexBuffer.BufferMaxScanLength <- lexBuffer.BufferScanLength + n); 
                  fillAsync = 
                    match asyncRead with 
                    | None -> None
                    | Some read -> 
                         Some (fun lexBuffer -> 
                                  async { 
                                      let! n = read(extension,0,extension.Length)
                                      lexBuffer.EnsureBufferSize n;
                                      Array.blit extension 0 lexBuffer.Buffer lexBuffer.BufferScanPos n;
                                      lexBuffer.BufferMaxScanLength <- lexBuffer.BufferScanLength + n }) }
            new LexBuffer<_>(fillers)

        // A full type signature is required on this method because it is used at more specific types within its own scope
        static member FromFunction (f : 'char[] * int * int -> int) : LexBuffer<'char> =  LexBuffer<_>.FromReadFunctions(Some(f),None)
        static member FromAsyncFunction (f : 'char[] * int * int -> Async<int>) : LexBuffer<'char> =  LexBuffer<_>.FromReadFunctions(None,Some(f))
              
        static member FromCharFunction f : LexBuffer<char> = 
            LexBuffer<char>.FromFunction(fun (buff,start,len) -> 
                let buff2 = Array.zeroCreate len
                let n = f buff2 len 
                Array.blit buff2 0 buff start len
                n)
        static member FromByteFunction f : LexBuffer<byte> = 
            LexBuffer<byte>.FromFunction(fun (buff,start,len) -> 
                let buff2 = Array.zeroCreate len
                let n = f buff2 len 
                Array.blit buff2 0 buff start len
                n)

        // A full type signature is required on this method because it is used at more specific types within its own scope
        static member FromArray (s: 'char[]) : LexBuffer<'char> = 
            let lexBuffer = 
                new LexBuffer<_> 
                    { fillSync = Some (fun _ -> ()); 
                      fillAsync = Some (fun _ -> async { return () }) }
            let buffer = Array.copy s 
            lexBuffer.Buffer <- buffer;
            lexBuffer.BufferMaxScanLength <- buffer.Length;
            lexBuffer

        static member FromBytes    (arr) = LexBuffer<byte>.FromArray(arr)
        static member FromChars    (arr) = LexBuffer<char>.FromArray(arr) 
        static member FromString (s:string) = LexBuffer<char>.FromChars (s.ToCharArray())

        static member FromTextReader (tr:System.IO.TextReader)  : LexBuffer<char> = 
           LexBuffer<char>.FromFunction(tr.Read) 

        static member FromBinaryReader (br:System.IO.BinaryReader)  : LexBuffer<byte> = 
           LexBuffer<byte>.FromFunction(br.Read) 

        static member FromStream (stream:System.IO.Stream)  : LexBuffer<byte> = 
           LexBuffer<byte>.FromReadFunctions(Some(stream.Read),Some(fun (buf,offset,len) -> stream.AsyncRead(buf,offset=offset,count=len))) 

    module GenericImplFragments = 
        let startInterpret(lexBuffer:LexBuffer<_>)= 
            lexBuffer.BufferScanStart <- lexBuffer.BufferScanStart + lexBuffer.LexemeLength;
            lexBuffer.BufferMaxScanLength <- lexBuffer.BufferMaxScanLength - lexBuffer.LexemeLength;
            lexBuffer.BufferScanLength <- 0;
            lexBuffer.LexemeLength <- 0;
            lexBuffer.BufferAcceptAction <- -1;

        let afterRefill (trans: uint16[] array,sentinel,lexBuffer:LexBuffer<_>,scanUntilSentinel,endOfScan,state,eofPos) = 
            // end of file occurs if we couldn't extend the buffer 
            if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then  
                let snew = int trans.[state].[eofPos] // == EOF 
                if snew = sentinel then 
                    endOfScan()
                else 
                    if lexBuffer.IsPastEndOfStream then failwith "End of file on lexing stream";
                    lexBuffer.IsPastEndOfStream <- true;
                    // Printf.printf "state %d --> %d on eof\n" state snew;
                    scanUntilSentinel(lexBuffer,snew)
            else 
                scanUntilSentinel(lexBuffer, state)

        let onAccept (lexBuffer:LexBuffer<_>,a) = 
            lexBuffer.LexemeLength <- lexBuffer.BufferScanLength;
            lexBuffer.BufferAcceptAction <- a;

    open GenericImplFragments

    [<Sealed>]
#if INTERNALIZED_FSLEXYACC_RUNTIME
    type internal AsciiTables(trans: uint16[] array, accept: uint16[]) =
#else
    type AsciiTables(trans: uint16[] array, accept: uint16[]) =
#endif
        let rec scanUntilSentinel(lexBuffer, state) =
            let sentinel = 255 * 256 + 255 
            // Return an endOfScan after consuming the input 
            let a = int accept.[state] 
            if a <> sentinel then 
                onAccept (lexBuffer,a)
            
            if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then 
                lexBuffer.DiscardInput();
                lexBuffer.RefillBuffer ();
              // end of file occurs if we couldn't extend the buffer 
                afterRefill (trans,sentinel,lexBuffer,scanUntilSentinel,lexBuffer.EndOfScan,state,256 (* == EOF *) )
            else
                // read a character - end the scan if there are no further transitions 
                let inp = int(lexBuffer.Buffer.[lexBuffer.BufferScanPos])
                let snew = int trans.[state].[inp] 
                if snew = sentinel then 
                    lexBuffer.EndOfScan()
                else 
                    lexBuffer.BufferScanLength <- lexBuffer.BufferScanLength + 1;
                    // Printf.printf "state %d --> %d on '%c' (%d)\n" state snew (Char.chr inp) inp;
                    scanUntilSentinel(lexBuffer, snew)
            
        /// Interpret tables for an ascii lexer generated by fslex. 
        member tables.Interpret(initialState,lexBuffer : LexBuffer<byte>) = 
            startInterpret(lexBuffer)
            scanUntilSentinel(lexBuffer, initialState)

        /// Interpret tables for an ascii lexer generated by fslex. 
        member tables.AsyncInterpret(initialState,lexBuffer : LexBuffer<byte>) = 
        
            let rec scanUntilSentinel(lexBuffer,state) : Async<int> = 
                async {  
                    let sentinel = 255 * 256 + 255 
                    // Return an endOfScan after consuming the input 
                    let a = int accept.[state] 
                    if a <> sentinel then 
                        onAccept (lexBuffer,a)
                    
                    if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then 
                        lexBuffer.DiscardInput();
                        do! lexBuffer.AsyncRefillBuffer ();
                       // end of file occurs if we couldn't extend the buffer 
                        return! afterRefill (trans,sentinel,lexBuffer,scanUntilSentinel,endOfScan,state,256 (* == EOF *) )
                    else
                        // read a character - end the scan if there are no further transitions 
                        let inp = int(lexBuffer.Buffer.[lexBuffer.BufferScanPos])
                        let snew = int trans.[state].[inp] 
                        if snew = sentinel then 
                            return! endOfScan()
                        else 
                            lexBuffer.BufferScanLength <- lexBuffer.BufferScanLength + 1;
                            return! scanUntilSentinel(lexBuffer,snew)
                }
            and endOfScan() = 
                async { return lexBuffer.EndOfScan() }
            startInterpret(lexBuffer)
            scanUntilSentinel(lexBuffer, initialState)


        static member Create(trans,accept) = new AsciiTables(trans,accept)

    [<Sealed>]
#if INTERNALIZED_FSLEXYACC_RUNTIME
    type internal UnicodeTables(trans: uint16[] array, accept: uint16[]) = 
#else
    type UnicodeTables(trans: uint16[] array, accept: uint16[]) = 
#endif
        let sentinel = 255 * 256 + 255 
        let numUnicodeCategories = 30 
        let numLowUnicodeChars = 128 
        let numSpecificUnicodeChars = (trans.[0].Length - 1 - numLowUnicodeChars - numUnicodeCategories)/2
        let lookupUnicodeCharacters (state,inp) = 
            let inpAsInt = int inp
            // Is it a fast ASCII character?
            if inpAsInt < numLowUnicodeChars then 
                int trans.[state].[inpAsInt]
            else 
                // Search for a specific unicode character
                let baseForSpecificUnicodeChars = numLowUnicodeChars
                let rec loop i = 
                    if i >= numSpecificUnicodeChars then 
                        // OK, if we failed then read the 'others' entry in the alphabet,
                        // which covers all Unicode characters not covered in other
                        // ways
                        let baseForUnicodeCategories = numLowUnicodeChars+numSpecificUnicodeChars*2
#if FX_WINRT
                        let unicodeCategory = System.Globalization.CharUnicodeInfo.GetUnicodeCategory(inp)
#else
                        let unicodeCategory = System.Char.GetUnicodeCategory(inp)
#endif
                        //System.Console.WriteLine("inp = {0}, unicodeCategory = {1}", [| box inp; box unicodeCategory |]);
                        int trans.[state].[baseForUnicodeCategories + int32 unicodeCategory]
                    else 
                        // This is the specific unicode character
                        let c = char (int trans.[state].[baseForSpecificUnicodeChars+i*2])
                        //System.Console.WriteLine("c = {0}, inp = {1}, i = {2}", [| box c; box inp; box i |]);
                        // OK, have we found the entry for a specific unicode character?
                        if c = inp
                        then int trans.[state].[baseForSpecificUnicodeChars+i*2+1]
                        else loop(i+1)
                
                loop 0
        let eofPos    = numLowUnicodeChars + 2*numSpecificUnicodeChars + numUnicodeCategories 
        
        let rec scanUntilSentinel(lexBuffer,state) =
            // Return an endOfScan after consuming the input 
            let a = int accept.[state] 
            if a <> sentinel then 
                onAccept(lexBuffer,a)
            
            if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then 
                lexBuffer.DiscardInput();
                lexBuffer.RefillBuffer ();
              // end of file occurs if we couldn't extend the buffer 
                afterRefill (trans,sentinel,lexBuffer,scanUntilSentinel,lexBuffer.EndOfScan,state,eofPos)
            else
                // read a character - end the scan if there are no further transitions 
                let inp = lexBuffer.Buffer.[lexBuffer.BufferScanPos]
                
                // Find the new state
                let snew = lookupUnicodeCharacters (state,inp)

                if snew = sentinel then 
                    lexBuffer.EndOfScan()
                else 
                    lexBuffer.BufferScanLength <- lexBuffer.BufferScanLength + 1;
                    // Printf.printf "state %d --> %d on '%c' (%d)\n" s snew (char inp) inp;
                    scanUntilSentinel(lexBuffer,snew)
                          
        // Each row for the Unicode table has format 
        //      128 entries for ASCII characters
        //      A variable number of 2*UInt16 entries for SpecificUnicodeChars 
        //      30 entries, one for each UnicodeCategory
        //      1 entry for EOF

        member tables.Interpret(initialState,lexBuffer : LexBuffer<char>) = 
            startInterpret(lexBuffer)
            scanUntilSentinel(lexBuffer, initialState)

        member tables.AsyncInterpret(initialState,lexBuffer : LexBuffer<char>) = 

            let rec scanUntilSentinel(lexBuffer, state) =
                async {
                    // Return an endOfScan after consuming the input 
                    let a = int accept.[state] 
                    if a <> sentinel then 
                        onAccept(lexBuffer,a)
                    
                    if lexBuffer.BufferScanLength = lexBuffer.BufferMaxScanLength then 
                        lexBuffer.DiscardInput();
                        lexBuffer.RefillBuffer ();
                        // end of file occurs if we couldn't extend the buffer 
                        return! afterRefill (trans,sentinel,lexBuffer,scanUntilSentinel,endOfScan,state,eofPos)
                    else
                        // read a character - end the scan if there are no further transitions 
                        let inp = lexBuffer.Buffer.[lexBuffer.BufferScanPos]
                        
                        // Find the new state
                        let snew = lookupUnicodeCharacters (state,inp)

                        if snew = sentinel then 
                            return! endOfScan()
                        else 
                            lexBuffer.BufferScanLength <- lexBuffer.BufferScanLength + 1;
                            return! scanUntilSentinel(lexBuffer, snew)
                }
            and endOfScan() = 
                async { return lexBuffer.EndOfScan() } 
            startInterpret(lexBuffer)
            scanUntilSentinel(lexBuffer, initialState)

        static member Create(trans,accept) = new UnicodeTables(trans,accept)
// @@@ END_DOCUMENT: Lexing.fs
// @@@ BEGIN_DOCUMENT: Parsing.fs
// (c) Microsoft Corporation 2005-2009. 

#if INTERNALIZED_FSLEXYACC_RUNTIME

namespace Included.Internal.Utilities.Text.Parsing
open Internal.Utilities
open Included.Internal.Utilities.Text.Lexing

#else
namespace Included.Microsoft.FSharp.Text.Parsing
open Included.Microsoft.FSharp.Text.Lexing
#endif



open System
open System.Collections.Generic

#if INTERNALIZED_FSLEXYACC_RUNTIME
type internal IParseState = 
#else
type IParseState = 
#endif
    abstract InputRange: int -> Position * Position
    abstract InputEndPosition: int -> Position 
    abstract InputStartPosition: int -> Position 
    abstract ResultRange: Position * Position
    abstract GetInput: int -> obj 
    abstract ParserLocalStore : IDictionary<string,obj>
    abstract RaiseError<'b> : unit -> 'b 

//-------------------------------------------------------------------------
// This context is passed to the error reporter when a syntax error occurs

[<Sealed>]
#if INTERNALIZED_FSLEXYACC_RUNTIME
type internal ParseErrorContext<'tok>
#else
type ParseErrorContext<'tok>
#endif
         (//lexbuf: LexBuffer<_>,
          stateStack:int list,
          parseState: IParseState, 
          reduceTokens: int list, 
          currentToken: 'tok option, 
          reducibleProductions: int list list, 
          shiftableTokens: int list , 
          message : string) =
      //member x.LexBuffer = lexbuf
      member x.StateStack  = stateStack
      member x.ReduceTokens = reduceTokens
      member x.CurrentToken = currentToken
      member x.ParseState = parseState
      member x.ReducibleProductions = reducibleProductions
      member x.ShiftTokens = shiftableTokens
      member x.Message = message


//-------------------------------------------------------------------------
// This is the data structure emitted as code by FSYACC.  

#if INTERNALIZED_FSLEXYACC_RUNTIME
type internal Tables<'tok> = 
#else
type Tables<'tok> = 
#endif
    { reductions: (IParseState -> obj) array;
      endOfInputTag: int;
      tagOfToken: 'tok -> int;
      dataOfToken: 'tok -> obj; 
      actionTableElements: uint16[];  
      actionTableRowOffsets: uint16[];
      reductionSymbolCounts: uint16[];
      immediateActions: uint16[];
      gotos: uint16[];
      sparseGotoTableRowOffsets: uint16[];
      stateToProdIdxsTableElements: uint16[];  
      stateToProdIdxsTableRowOffsets: uint16[];  
      productionToNonTerminalTable: uint16[];
      /// For fsyacc.exe, this entry is filled in by context from the generated parser file. If no 'parse_error' function
      /// is defined by the user then ParseHelpers.parse_error is used by default (ParseHelpers is opened
      /// at the top of the generated parser file)
      parseError:  ParseErrorContext<'tok> -> unit;
      numTerminals: int;
      tagOfErrorTerminal: int }

//-------------------------------------------------------------------------
// An implementation of stacks.

// This type is in System.dll so for the moment we can't use it in FSharp.Core.dll
//type Stack<'a> = System.Collections.Generic.Stack<'a>

#if INTERNALIZED_FSLEXYACC_RUNTIME
type Stack<'a>(n)  = 
#else
type internal Stack<'a>(n)  = 
#endif
    let mutable contents = Array.zeroCreate<'a>(n)
    let mutable count = 0

    member buf.Ensure newSize = 
        let oldSize = Array.length contents
        if newSize > oldSize then 
            let old = contents
            contents <- Array.zeroCreate (max newSize (oldSize * 2));
            Array.blit old 0 contents 0 count;
    
    member buf.Count = count
    member buf.Pop() = count <- count - 1
    member buf.Peep() = contents.[count - 1]
    member buf.Top(n) = [ for x in contents.[max 0 (count-n)..count - 1] -> x ] |> List.rev
    member buf.Push(x) =
        buf.Ensure(count + 1); 
        contents.[count] <- x; 
        count <- count + 1
        
    member buf.IsEmpty = (count = 0)
    member buf.PrintStack() = 
        for i = 0 to (count - 1) do 
#if FX_NO_CONSOLE
            ()
#else
            System.Console.Write("{0}{1}",(contents.[i]),if i=count-1 then ":" else "-") 
#endif         
exception RecoverableParseError
exception Accept of obj

#if __DEBUG
module Flags = 
    let mutable debug = false
#endif

#if INTERNALIZED_FSLEXYACC_RUNTIME
module internal Implementation = 
#else
module Implementation = 
#endif
    
    // Definitions shared with fsyacc 
    let anyMarker = 0xffff
    let shiftFlag = 0x0000
    let reduceFlag = 0x4000
    let errorFlag = 0x8000
    let acceptFlag = 0xc000
    let actionMask = 0xc000

    let actionValue action = action &&& (~~~ actionMask)                                    
    let actionKind action = action &&& actionMask
    
    //-------------------------------------------------------------------------
    // Read the tables written by FSYACC.  

    type AssocTable(elemTab:uint16[], offsetTab:uint16[]) =
        let cache = new Dictionary<_,_>(2000)

        member t.readAssoc (minElemNum,maxElemNum,defaultValueOfAssoc,keyToFind) =     
            // do a binary chop on the table 
            let elemNumber : int = (minElemNum+maxElemNum)/2
            if elemNumber = maxElemNum 
            then defaultValueOfAssoc
            else 
                let x = int elemTab.[elemNumber*2]
                if keyToFind = x then 
                    int elemTab.[elemNumber*2+1]
                elif keyToFind < x then t.readAssoc (minElemNum ,elemNumber,defaultValueOfAssoc,keyToFind)
                else                    t.readAssoc (elemNumber+1,maxElemNum,defaultValueOfAssoc,keyToFind)

        member t.Read(rowNumber ,keyToFind) =
        
            // First check the sparse lookaside table
            // Performance note: without this lookaside table the binary chop in readAssoc
            // takes up around 10% of of parsing time 
            // for parsing intensive samples such as the bootstrapped F# compiler.
            //
            // Note: using a .NET Dictionary for this int -> int table looks like it could be sub-optimal.
            // Some other better sparse lookup table may be better.
            let mutable res = 0 
            let cacheKey = (rowNumber <<< 16) ||| keyToFind
            let ok = cache.TryGetValue(cacheKey, &res) 
            if ok then res 
            else
                let headOfTable = int offsetTab.[rowNumber]
                let firstElemNumber = headOfTable + 1           
                let numberOfElementsInAssoc = int elemTab.[headOfTable*2]
                let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
                let res = t.readAssoc (firstElemNumber,(firstElemNumber+numberOfElementsInAssoc),defaultValueOfAssoc,keyToFind)
                cache.[cacheKey] <- res
                res

        // Read all entries in the association table
        // Used during error recovery to find all valid entries in the table
        member x.ReadAll(n) =       
            let headOfTable = int offsetTab.[n]
            let firstElemNumber = headOfTable + 1           
            let numberOfElementsInAssoc = int32 elemTab.[headOfTable*2]           
            let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
            [ for i in firstElemNumber .. (firstElemNumber+numberOfElementsInAssoc-1) -> 
                (int elemTab.[i*2], int elemTab.[i*2+1]) ], defaultValueOfAssoc

    type IdxToIdxListTable(elemTab:uint16[], offsetTab:uint16[]) =

        // Read all entries in a row of the table
        member x.ReadAll(n) =       
            let headOfTable = int offsetTab.[n]
            let firstElemNumber = headOfTable + 1           
            let numberOfElements = int32 elemTab.[headOfTable]           
            [ for i in firstElemNumber .. (firstElemNumber+numberOfElements-1) -> int elemTab.[i] ]

    //-------------------------------------------------------------------------
    // interpret the tables emitted by FSYACC.  

    [<NoEquality; NoComparison>]
    [<Struct>]
    type ValueInfo = 
        val value: obj
        val startPos: Position
        val endPos: Position
        new(value,startPos,endPos) = { value=value; startPos=startPos;endPos=endPos }

    let interpret (tables: Tables<'tok>) lexer (lexbuf : LexBuffer<_>) initialState =                                                                      
        let localStore = new Dictionary<string,obj>() in
        localStore.["LexBuffer"] <- lexbuf;
#if __DEBUG
        if Flags.debug then System.Console.WriteLine("\nParser: interpret tables");
#endif
        let stateStack : Stack<int> = new Stack<_>(100)
        stateStack.Push(initialState);
        let valueStack = new Stack<ValueInfo>(100)
        let mutable haveLookahead = false                                                                              
        let mutable lookaheadToken = Unchecked.defaultof<'tok>
        let mutable lookaheadEndPos = Unchecked.defaultof<Position>
        let mutable lookaheadStartPos = Unchecked.defaultof<Position>
        let mutable finished = false
        // After an error occurs, we suppress errors until we've shifted three tokens in a row.
        let mutable errorSuppressionCountDown = 0
        
        // When we hit the end-of-file we don't fail straight away but rather keep permitting shift
        // and reduce against the last token in the token stream 20 times or until we've accepted
        // or exhausted the stack. This allows error recovery rules of the form
        //      input : realInput EOF | realInput error EOF | error EOF
        // where consuming one EOF to trigger an error doesn't result in overall parse failure 
        // catastrophe and the loss of intermediate results.
        //
        let mutable inEofCountDown = false
        let mutable eofCountDown = 20 // Number of EOFs to supply at the end for error recovery
        // The 100 here means a maximum of 100 elements for each rule
        let ruleStartPoss = (Array.zeroCreate 100 : Position array)              
        let ruleEndPoss   = (Array.zeroCreate 100 : Position array)              
        let ruleValues    = (Array.zeroCreate 100 : obj array)              
        let lhsPos        = (Array.zeroCreate 2 : Position array)                                            
        let reductions = tables.reductions
        let actionTable = new AssocTable(tables.actionTableElements, tables.actionTableRowOffsets)
        let gotoTable = new AssocTable(tables.gotos, tables.sparseGotoTableRowOffsets)
        let stateToProdIdxsTable = new IdxToIdxListTable(tables.stateToProdIdxsTableElements, tables.stateToProdIdxsTableRowOffsets)

        let parseState =                                                                                            
            { new IParseState with 
                member p.InputRange(n) = ruleStartPoss.[n-1], ruleEndPoss.[n-1]; 
                member p.InputStartPosition(n) = ruleStartPoss.[n-1]
                member p.InputEndPosition(n) = ruleEndPoss.[n-1]; 
                member p.GetInput(n)    = ruleValues.[n-1];        
                member p.ResultRange    = (lhsPos.[0], lhsPos.[1]);  
                member p.ParserLocalStore = (localStore :> IDictionary<_,_>); 
                member p.RaiseError()  = raise RecoverableParseError  (* NOTE: this binding tests the fairly complex logic associated with an object expression implementing a generic abstract method *)
            }       

#if __DEBUG
        let report haveLookahead lookaheadToken = 
            if haveLookahead then sprintf "%A" lookaheadToken 
            else "[TBC]"
#endif

        // Pop the stack until we can shift the 'error' token. If 'tokenOpt' is given
        // then keep popping until we can shift both the 'error' token and the token in 'tokenOpt'.
        // This is used at end-of-file to make sure we can shift both the 'error' token and the 'EOF' token.
        let rec popStackUntilErrorShifted(tokenOpt) =
            // Keep popping the stack until the "error" terminal is shifted
#if __DEBUG
            if Flags.debug then System.Console.WriteLine("popStackUntilErrorShifted");
#endif
            if stateStack.IsEmpty then 
#if __DEBUG
                if Flags.debug then 
                    System.Console.WriteLine("state stack empty during error recovery - generating parse error");
#endif
                failwith "parse error";
            
            let currState = stateStack.Peep()
#if __DEBUG
            if Flags.debug then 
                System.Console.WriteLine("In state {0} during error recovery", currState);
#endif
            
            let action = actionTable.Read(currState, tables.tagOfErrorTerminal)
            
            if actionKind action = shiftFlag &&  
                (match tokenOpt with 
                 | None -> true
                 | Some(token) -> 
                    let nextState = actionValue action 
                    actionKind (actionTable.Read(nextState, tables.tagOfToken(token))) = shiftFlag) then

#if __DEBUG
                if Flags.debug then System.Console.WriteLine("shifting error, continuing with error recovery");
#endif
                let nextState = actionValue action 
                // The "error" non terminal needs position information, though it tends to be unreliable.
                // Use the StartPos/EndPos from the lex buffer
                valueStack.Push(ValueInfo(box (), lexbuf.StartPos, lexbuf.EndPos));
                stateStack.Push(nextState)
            else
                if valueStack.IsEmpty then 
                    failwith "parse error";
#if __DEBUG
                if Flags.debug then 
                    System.Console.WriteLine("popping stack during error recovery");
#endif
                valueStack.Pop();
                stateStack.Pop();
                popStackUntilErrorShifted(tokenOpt)

        while not finished do                                                                                    
            if stateStack.IsEmpty then 
                finished <- true
            else
                let state = stateStack.Peep()
#if __DEBUG
                if Flags.debug then (Console.Write("{0} value(state), state ",valueStack.Count); stateStack.PrintStack())
#endif
                let action = 
                    let immediateAction = int tables.immediateActions.[state]
                    if not (immediateAction = anyMarker) then
                        // Action has been pre-determined, no need to lookahead 
                        // Expecting it to be a Reduce action on a non-fakeStartNonTerminal ? 
                        immediateAction
                    else
                        // Lookahead required to determine action 
                        if not haveLookahead then 
                            if lexbuf.IsPastEndOfStream then 
                                // When the input runs out, keep supplying the last token for eofCountDown times
                                if eofCountDown>0 then
                                    haveLookahead <- true
                                    eofCountDown <- eofCountDown - 1
                                    inEofCountDown <- true
                                else 
                                    haveLookahead <- false
                            else 
                                lookaheadToken <- lexer lexbuf
                                lookaheadStartPos <- lexbuf.StartPos
                                lookaheadEndPos <- lexbuf.EndPos
                                haveLookahead <- true;

                        let tag = 
                            if haveLookahead then tables.tagOfToken lookaheadToken 
                            else tables.endOfInputTag   
                                    
                        // Printf.printf "state %d\n" state  
                        actionTable.Read(state,tag)
                        
                let kind = actionKind action 
                if kind = shiftFlag then (
                    if errorSuppressionCountDown > 0 then 
                        errorSuppressionCountDown <- errorSuppressionCountDown - 1;
#if __DEBUG
                        if Flags.debug then Console.WriteLine("shifting, reduced errorRecoverylevel to {0}\n", errorSuppressionCountDown);
#endif
                    let nextState = actionValue action                                     
                    if not haveLookahead then failwith "shift on end of input!";
                    let data = tables.dataOfToken lookaheadToken
                    valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos));
                    stateStack.Push(nextState);                                                                
#if __DEBUG
                    if Flags.debug then Console.WriteLine("shift/consume input {0}, shift to state {1}", report haveLookahead lookaheadToken, nextState);
#endif
                    haveLookahead <- false

                ) elif kind = reduceFlag then
                    let prod = actionValue action                                     
                    let reduction = reductions.[prod]                                                             
                    let n = int tables.reductionSymbolCounts.[prod]
                       // pop the symbols, populate the values and populate the locations                              
#if __DEBUG
                    if Flags.debug then Console.Write("reduce popping {0} values/states, lookahead {1}", n, report haveLookahead lookaheadToken);
#endif
                    
                    lhsPos.[0] <- Position.Empty;                                                                     
                    lhsPos.[1] <- Position.Empty;  
                    for i = 0 to n - 1 do                                                                             
                        if valueStack.IsEmpty then failwith "empty symbol stack";
                        let topVal = valueStack.Peep()
                        valueStack.Pop();
                        stateStack.Pop();
                        ruleValues.[(n-i)-1] <- topVal.value;  
                        ruleStartPoss.[(n-i)-1] <- topVal.startPos;  
                        ruleEndPoss.[(n-i)-1] <- topVal.endPos;  
                        if lhsPos.[1] = Position.Empty then lhsPos.[1] <- topVal.endPos;
                        if not (topVal.startPos = Position.Empty) then lhsPos.[0] <- topVal.startPos
                    done;                                                                                           
                    
                    try                                                                                               
                          // Printf.printf "reduce %d\n" prod;                                                       
                        let redResult = reduction parseState                                                          
                        valueStack.Push(ValueInfo(redResult, lhsPos.[0], lhsPos.[1]));
                        let currState = stateStack.Peep()
                        let newGotoState = gotoTable.Read(int tables.productionToNonTerminalTable.[prod], currState)
                        stateStack.Push(newGotoState)
#if __DEBUG
                        if Flags.debug then Console.WriteLine(" goto state {0}", newGotoState)
#endif
                    with                                                                                              
                    | Accept res ->                                                                            
                          finished <- true;                                                                             
                          valueStack.Push(ValueInfo(res, lhsPos.[0], lhsPos.[1])) 
                    | RecoverableParseError ->
#if __DEBUG
                          if Flags.debug then Console.WriteLine("RecoverableParseErrorException...\n");
#endif
                          popStackUntilErrorShifted(None);
                          // User code raised a Parse_error. Don't report errors again until three tokens have been shifted 
                          errorSuppressionCountDown <- 3
                elif kind = errorFlag then (
#if __DEBUG
                    if Flags.debug then Console.Write("ErrorFlag... ");
#endif
                    // Silently discard inputs and don't report errors 
                    // until three tokens in a row have been shifted 
#if __DEBUG
                    if Flags.debug then printfn "error on token '%A' " (if haveLookahead then Some(lookaheadToken) else None);
#endif
                    if errorSuppressionCountDown > 0 then 
                        // If we're in the end-of-file count down then we're very keen to 'Accept'.
                        // We can only do this by repeatedly popping the stack until we can shift both an 'error' token
                        // and an EOF token. 
                        if inEofCountDown && eofCountDown < 10 then 
#if __DEBUG
                            if Flags.debug then printfn "poppin stack, lokking to shift both 'error' and that token, during end-of-file error recovery" ;
#endif
                            popStackUntilErrorShifted(if haveLookahead then Some(lookaheadToken) else None);

                        // If we don't haveLookahead then the end-of-file count down is over and we have no further options.
                        if not haveLookahead then 
                            failwith "parse error: unexpected end of file"
                            
#if __DEBUG
                        if Flags.debug then printfn "discarding token '%A' during error suppression" (if haveLookahead then Some(lookaheadToken) else None);
#endif
                        // Discard the token
                        haveLookahead <- false
                        // Try again to shift three tokens
                        errorSuppressionCountDown <- 3
                    else (

                        let currentToken = if haveLookahead then Some(lookaheadToken) else None
                        let actions,defaultAction = actionTable.ReadAll(state) 
                        let explicit = Set.ofList [ for (tag,_action) in actions -> tag ]
                        
                        let shiftableTokens = 
                           [ for (tag,action) in actions do
                                 if (actionKind action) = shiftFlag then 
                                     yield tag
                             if actionKind defaultAction = shiftFlag  then
                                 for tag in 0 .. tables.numTerminals-1 do  
                                    if not (explicit.Contains(tag)) then 
                                         yield tag ] in

                        let stateStack = stateStack.Top(12) in
                        let reducibleProductions = 
                            [ for state in stateStack do 
                               yield stateToProdIdxsTable.ReadAll(state)  ]

                        let reduceTokens = 
                           [ for (tag,action) in actions do
                                if actionKind(action) = reduceFlag then
                                    yield tag
                             if actionKind(defaultAction) = reduceFlag  then
                                 for tag in 0 .. tables.numTerminals-1 do  
                                    if not (explicit.Contains(tag)) then 
                                         yield tag ] in
                        //let activeRules = stateStack |> List.iter (fun state -> 
                        let errorContext = new ParseErrorContext<'tok>(stateStack,parseState, reduceTokens,currentToken,reducibleProductions, shiftableTokens, "syntax error")
                        tables.parseError(errorContext);
                        popStackUntilErrorShifted(None);
                        errorSuppressionCountDown <- 3;
#if __DEBUG
                        if Flags.debug then System.Console.WriteLine("generated syntax error and shifted error token, haveLookahead = {0}\n", haveLookahead);
#endif
                    )
                ) elif kind = acceptFlag then 
                    finished <- true
#if __DEBUG
                else
                  if Flags.debug then System.Console.WriteLine("ALARM!!! drop through case in parser");  
#endif
        done;                                                                                                     
        // OK, we're done - read off the overall generated value
        valueStack.Peep().value

#if INTERNALIZED_FSLEXYACC_RUNTIME
type internal Tables<'tok> with
#else
type Tables<'tok> with
#endif
    member tables.Interpret (lexer,lexbuf,initialState) = 
        Implementation.interpret tables lexer lexbuf initialState
    
#if INTERNALIZED_FSLEXYACC_RUNTIME
module internal ParseHelpers = 
#else
module ParseHelpers = 
#endif
    let parse_error (_s:string) = ()
    let parse_error_rich = (None : (ParseErrorContext<_> -> unit) option)
// @@@ END_DOCUMENT: Parsing.fs
namespace Included
module IncludeMetaData = 
    [<Literal>] let IncludeDate = "20150122T23:33:15" 
f
    let parse_error (_s:string) = ()
    let parse_error_rich = (None : (ParseErrorContext<_> -> unit) option)
// @@@ END_DOCUMENT: https://raw.githubusercontent.com/fsprojects/FsLexYacc/master/src/FsLexYacc.Runtime/Parsing.fs
namespace Included
module IncludeMetaData = 
    [<Literal>] let IncludeDate = "20150122T23:12:21" 
