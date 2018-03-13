module EBrown.SqlDsl.MainGeneration
open Types

let timeScaleToString timeScale = match timeScale with | S0 -> '0' | S1 -> '1' | S2 -> '2' | S3 -> '3' | S4 -> '4' | S5 -> '5' | S6 -> '6' | S7 -> '7'
let varSizeToString varSize = match varSize with | Max -> "max" | Size c -> sprintf "%i" c
let decNumSizeToString s = match s with | None -> "" | Some { Precision = p; Scale = s } -> sprintf "(%i%s)" p (match s with | None -> "" | Some s -> sprintf ", %i" s)
let timeScaleOptionToString s = match s with | None -> "" | Some s -> sprintf "(%c)" (s |> timeScaleToString)
let varSizeOptionToString l = match l with | None -> "" | Some l -> sprintf "(%s)" (l |> varSizeToString)
let intOptionToString c = match c with | None -> "" | Some c -> sprintf "(%i)" c

let quoteName n = sprintf "[%s]" n
let joinStrings sep (strs : string seq) = System.String.Join(sep, strs)
let joinFilterStrings sep (strs : string seq) = System.String.Join(sep, strs |> Seq.filter (System.String.IsNullOrEmpty >> not))

let columnToString (sqlType : SqlColumn) =
    let nameStr = sqlType.Name |> quoteName
    let typeStr =
        match sqlType.Type with
        | Bigint -> "bigint"
        | Int -> "int"
        | SmallInt -> "smallint"
        | TinyInt -> "tinyint"
        | Bit -> "bit"
        | Money -> "money"
        | SmallMoney -> "smallmoney"
        | Float b -> sprintf "float%s" (b |> intOptionToString)
        | Real -> sprintf "real"
        | Decimal s -> sprintf "decimal%s" (s |> decNumSizeToString)
        | Numeric s -> sprintf "numeric%s" (s |> decNumSizeToString)
        | Date -> "date"
        | Time s -> sprintf "time%s" (s |> timeScaleOptionToString)
        | SmallDateTime -> "smalldatetime"
        | DateTime -> "datetime"
        | DateTime2 s -> sprintf "datetime2%s" (s |> timeScaleOptionToString)
        | DateTimeOffset s -> sprintf "datetimeoffset%s" (s |> timeScaleOptionToString)
        | Char c -> sprintf "char%s" (c |> intOptionToString)
        | VarChar l -> sprintf "varchar%s" (l |> varSizeOptionToString)
        | Text -> "text"
        | NChar c -> sprintf "nchar%s" (c |> intOptionToString)
        | NVarChar l -> sprintf "nvarchar%s" (l |> varSizeOptionToString)
        | NText -> "ntext"
        | Binary b -> sprintf "binary%s" (b |> intOptionToString)
        | VarBinary l -> sprintf "varbinary%s" (l |> varSizeOptionToString)
        | Image -> "image"
        | Xml t -> sprintf "xml%s" (match t with | None -> "" | Some t -> (match t with | Content -> "(content)" | Document -> "(document)"))
        | UniqueIdentifier -> "uniqueidentifier"
    let nullStr = if sqlType.Nullable then "NULL" else "NOT NULL"
    let defaultStr =
        let argToStr a = match a with | String s -> sprintf "'%s'" s | Integer i -> sprintf "%i" i | Number f -> sprintf "%f" f
        let getStandardFunctionName fn = match fn with | SysUtcDateTime -> "SYSUTCDATETIME()" | GetDate -> "GETDATE()"
        let getDefaultString s =
            match s with
            | Value a -> sprintf " DEFAULT %s" (a |> argToStr)
            | Identity (s, i) -> sprintf " IDENTITY(%i, %i)" s i
            | Function f ->
                match f with
                | Function.Standard s -> sprintf " DEFAULT %s" (s |> getStandardFunctionName)
                | Custom (s, a) -> sprintf " DEFAULT %s(%s)" (s |> quoteName) (a |> Array.map argToStr |> joinStrings ", ")
        match sqlType.Default with | None -> "" | Some s -> s |> getDefaultString
    sprintf "%s %s %s%s" nameStr (typeStr.ToUpper()) nullStr defaultStr

let printName f n = match n with | Raw n -> n |> f | Alias (n, a) -> sprintf "%s AS %s" (n |> f) (a |> quoteName)
let printAliasName n = match n with | Qualified (n1, n2) -> sprintf "%s.%s" (n1 |> quoteName) (n2 |> quoteName) | Name n -> n |> printName quoteName | Func n -> n |> printName id
let printSort s = match s with | Ascending n -> sprintf "%s ASC" (n |> quoteName) | Descending n -> sprintf "%s DESC" (n |> quoteName)

let printSelectQuery t s =
    let str = "SELECT"
    let str = match s.Limit with | None -> str | Some c -> sprintf "%s TOP %i" str c
    let str = match s.What with | Everything -> sprintf "%s *" str | Columns c -> sprintf "%s %s" str (c |> Array.map printAliasName |> joinStrings ", ")
    let str = sprintf "%s FROM %s" str (t |> printName quoteName)
    let str = match s.Group with | None -> str | Some g -> sprintf "%s GROUP BY %s" str (g |> Array.map quoteName |> joinStrings ", ")
    let str = match s.Sort with | None -> str | Some s -> sprintf "%s ORDER BY %s" str (s |> Array.map printSort |> joinStrings ", ")
    str
