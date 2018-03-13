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
            What = Columns [|Name (Raw "OrderId"); Name (Alias (Name (Raw "PoNumber"), "PurchaseOrderNumber")); Name (Alias (Name (Raw "Customer"), "CustomerName"))|]
            Limit = None
            Group = None
            Sort = None
            Include = 
                [{
                    Name = Alias (Name (Raw "Customers"), "Customer")
                    Type = Inner
                    On = [{Filter.Name = Qualified("Customer", "Id"); Clause = Equal (Qualified ("Order", "CustomerId"));}]
                }]}
    Table = Raw "OrderItems" }
let queryStr = query |> Generation.query
printfn "%s" queryStr

let table = {
    Table.Name = "OrderItems"
    Columns = [|{ SqlColumn.Name = "ItemId"; Default = None; Type = Int; Nullable = false }|]
    KeyColumns = [| |]
    Relationships = [] }
