module EBrown.SqlDsl.Types
type VarSize = | Size of int | Max
type XmlType = | Content | Document
type TimeScale = | S0 | S1 | S2 | S3 | S4 | S5 | S6 | S7
type StandardFunction = | SysUtcDateTime | GetDate
type Arg = | Integer of int | Number of float | String of string
type Function = | Standard of StandardFunction | Custom of Name : string * Args : Arg array
type Default = | Identity of Seed : int * Increment : int | Function of Function | Value of Arg
type RelationTarget = { Source : string; Target : string }
type Relationship = | ForeignKey of Target : string * Columns : RelationTarget array
type NumericSize = { Precision : int; Scale : int option }

type SqlType =
    | Bigint | Int | SmallInt | TinyInt | Bit | Money | SmallMoney | Real | Date | SmallDateTime | DateTime | Text | NText | Image | UniqueIdentifier
    | Float of Bits : int option
    | Decimal of Size : NumericSize option | Numeric of Size : NumericSize option
    | Time of FractionalSeconds : TimeScale option | DateTime2 of FractionalSeconds : TimeScale option | DateTimeOffset of FractionalSeconds : TimeScale option
    | Char of Characters : int option | NChar of Characters : int option | Binary of Bytes : int option
    | VarChar of Characters : VarSize option | NVarChar of Characters : VarSize option | VarBinary of Bytes : VarSize option
    | Xml of Type : XmlType option

type SqlColumn = { Name : string; Type : SqlType; Nullable : bool; Default : Default option }

type AliasName = | Raw of string | Alias of AliasNameFunc * string
and AliasNameFunc = | Qualified of string * string | Name of AliasName | Func of AliasName

type Table = { Name : string; KeyColumns : SqlColumn array; Columns : SqlColumn array; Relationships : Relationship list }
type SelectWhat = | Everything | Columns of AliasNameFunc array
type SortDirection = | Ascending of string | Descending of string
type FilterClause = | Equal of string | NotEqual of string | Between of string * string
type Filter = { Name : string; Clause : FilterClause }
type Join = { Name : AliasName; On : Filter list }
type SelectQuery = { What : SelectWhat; Limit : int option; Group : string array option; Sort : SortDirection array option; Include : Join list }
type QueryType = | Select of SelectQuery
type Query = { Type : QueryType; Table : AliasName }
