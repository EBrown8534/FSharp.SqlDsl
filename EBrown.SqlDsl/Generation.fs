module EBrown.SqlDsl.Generation
open Types
open MainGeneration
module private Tuple = let map2 fn (v1, v2) = (v1 |> fn, v2 |> fn)

let query q =
    match q.Type with
    | Select s -> s |> printSelectQuery q.Table

let createTable (ct : Table) =
    let nameStr = ct.Name |> quoteName
    let columnsStr, keyColumnsStr = (ct.Columns, ct.KeyColumns) |> Tuple.map2 (Array.map columnToString >> joinStrings ", ")
    let primaryKeyConstraint =
        if ct.KeyColumns.Length = 0 then ""
        else sprintf "CONSTRAINT [PK_%s] PRIMARY KEY (%s)" ct.Name (ct.KeyColumns |> Array.map (fun f -> f.Name |> quoteName) |> joinStrings ", ")
    let foreignKeyConstraints =
        ct.Relationships
        |> List.map (fun relationship ->
            match relationship with
            | ForeignKey (t, cols) ->
                let sourceCols, targetCols = cols |> Array.rev |> Array.fold (fun (sourceCols, targetCols) map -> (map.Source::sourceCols, map.Target::targetCols)) ([], [])
                let fkName = sprintf "%s_%s_%s_%s" ct.Name (sourceCols |> joinStrings "_") t (targetCols |> joinStrings "_")
                let sourceColNames, targetColNames = (sourceCols, targetCols) |> Tuple.map2 (List.map quoteName >> joinStrings ", ")
                sprintf "CONSTRAINT [FK_%s] FOREIGN KEY (%s) REFERENCES [%s] (%s)" fkName sourceColNames t targetColNames)
        |> joinStrings ", "
    let tableStatements = [keyColumnsStr; columnsStr; foreignKeyConstraints; primaryKeyConstraint] |> joinFilterStrings ", "
    sprintf "CREATE TABLE %s (%s)" nameStr tableStatements

let dropTable (ct : Table) =
    let nameStr = ct.Name |> quoteName
    let pkStr =
        if ct.KeyColumns.Length = 0 then ""
        else sprintf "ALTER TABLE %s DROP CONSTRAINT [PK_%s]" nameStr ct.Name
    let fkStrs =
        ct.Relationships
        |> List.map (fun relationship ->
            match relationship with
            | ForeignKey (t, cols) ->
                let sourceCols, targetCols = cols |> Array.rev |> Array.fold (fun (sourceCols, targetCols) map -> (map.Source::sourceCols, map.Target::targetCols)) ([], [])
                let fkName = sprintf "%s_%s_%s_%s" ct.Name (sourceCols |> joinStrings "_") t (targetCols |> joinStrings "_")
                sprintf "ALTER TABLE %s DROP CONSTRAINT [FK_%s]" nameStr fkName)
    let tableStr = sprintf "DROP TABLE %s" nameStr
    [fkStrs; [pkStr]; [tableStr]] |> List.concat |> joinFilterStrings System.Environment.NewLine
