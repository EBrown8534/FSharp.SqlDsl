// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

#load "Types.fs"
#load "MainGeneration.fs"
#load "Generation.fs"
open EBrown.SqlDsl
open Types

// Define your library scripting code here

let query = {
    Query.Type = 
        Select { 
            What = Columns [|Name (Raw "OrderId"); Name (Alias ("PoNumber", "PurchaseOrderNumber")); Name (Alias ("Customer", "CustomerName"))|]
            Limit = None
            Group = None
            Sort = None
            Include = [] }
    Table = Raw "OrderItems" }
let queryStr = query |> Generation.query
printfn "%s" queryStr
