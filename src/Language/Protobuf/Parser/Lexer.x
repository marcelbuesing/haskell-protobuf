{
-- | https://developers.google.com/protocol-buffers/docs/reference/proto3-spec
module Main (main) where
}

%wrapper "basic"

-- Whitespace insensitive
$eol                          ;
$white+                       ;

$letter = [a-zA-Z]
$decimalDigit = 0-9
$octalDigit = 0-7
$hexDigit = [0-9A-Fa-f]

tokens :=
  true         { \s -> TokenTrue }
  false        { \s -> TokenFalse }
  \=           { \s -> TokenEq }
  \{           { \s -> TokenLBracket }
  \}           { \s -> TokenRBracket }
  \;           { \s -> TokenSemiColon }
  double       { \s -> TokenDouble }
  float        { \s -> TokenFloat }
  int32        { \s -> TokenInt32 }
  int64        { \s -> TokenInt64 }
  uint32       { \s -> TokenUInt32 }
  uint64       { \s -> TokenUInt64 }
  sint32       { \s -> TokenSInt32 }
  sint64       { \s -> TokenSInt64 }
  fixed32      { \s -> TokenFixed32 }
  fixed64      { \s -> TokenFixed64 }
  sfixed32     { \s -> TokenSFixed32 }
  sfixed64     { \s -> TokenSFixed64 }
  bool         { \s -> TokenBool }
  string       { \s -> TokenString }
  bytes        { \s -> TokenBytes }
  '            { \s -> TokenQuote }
  +            { \s -> TokenPlus }
  -            { \s -> TokenMinus }
  syntax       { \s -> TokenSyntax }
  import       { \s -> TokenImport }
  weak         { \s -> TokenWeak }
  public       { \s -> TokenPublic }
  option       { \s -> TokenOption }
  repeated     { \s -> TokenRepeated }
  oneof        { \s -> TokenOneof }
  Proto3       { \s -> TokenProto3 }


{

  data Tokens =
    TokenTrue      |
    TokenFalse     |
    TokenEq        |
    TokenLBracket  |
    TokenRBracket  |
    TokenSemiColon |
    TokenDouble    |
    TokenFloat     |
    TokenInt32     |
    TokenInt64     |
    TokenUInt32    |
    TokenUInt64    |
    TokenSInt32    |
    TokenSInt64    |
    TokenFixed32   |
    TokenFixed64   |
    TokenSFixed32  |
    TokenSFixed64  |
    TokenBool      |
    TokenString    |
    TokenBytes     |
    TokenQuote     |
    TokenPlus      |
    TokenMinus     |
    TokenSyntax    |
    TokenImport    |
    TokenWeak      |
    TokenPublic    |
    TokenOption    |
    TokenRepeated  |
    TokenOneof     |
    TokenProto3
    deriving (Eq, Show);

--  data IdentToken =
--    IdentToken       |
--    FullIdentToken   |
--    MessageNameToken |
--    FieldNameToken   |
--    OneOfNameToken   |
--    MapNameToken     |
--    ServiceNameToken |
--    RpcNameToken     |
--    MessageTypeToken |
--    EnumTypeToken
--    deriving (Eq, Show);
--
--  data IntegerLiteralToken =
--    IntLitToken     |
--    DecimalLitToken |
--    OctalLitToken   |
--    HexLitToken
--    deriving (Eq, Show);
--
--  data FloatingPointLiteralToken =
--    FloatLitToken |
--    DecimalsToken |
--    ExponentToken
--    deriving (Eq, Show);
--
--  data BooleanToken =
--    BoolLitToken
--    deriving (Eq, Show);
--
--  data StringLiteralToken =
--    StrLitToken     |
--    CharValueToken  |
--    HexEscapeToken  |
--    OctEscapeToken  |
--    CharEscapeToken |
--    QuoteToken
--    deriving (Eq, Show);
--
--  data EmptyStatementToken = EmptyStatementToken deriving (Eq, Show);
--
--  data ConstantToken = ConstantToken deriving (Eq, Show);
--
--  data SyntaxToken = SyntaxToken deriving (Eq, Show);
--
--  data PackageToken = PackageToken deriving (Eq, Show);
--
--  data OptionToken = OptionToken | OptionNameToken deriving (Eq, Show);
--
--  data FieldToken =
--    TypeToken   |
--    FieldNumberToken
--    deriving (Eq, Show);
--
--  data NormalFieldToken =
--    FieldToken        |
--    FieldOptionsToken |
--    FieldOptionToken
--    deriving (Eq, Show);
--
--  data OneofToken =
--    OneofToken |
--    OneofField
--    deriving (Eq, Show)
--
--  data ReservedToken =
--    ReservedToken  |
--    RangesToken    |
--    RangeToken     |
--    FieldNameToken |
--    deriving (Eq, Show);
--
--  data EnumDefinitionToken =
--    Enum      |
--    EnumBody  |
--    EnumField |
--    EnumValueOption
--

--  data FBasicTypeIdToken =
--    Undefined |
--    Int8    |
--    UInt8   |
--    Int16   |
--    UInt16  |
--    Int32   |
--    UInt32  |
--    Int64   |
--    UInt64  |
--    Boolean |
--    String  |
--    Float   |
--    Double  |
--    ByteBuffer
--    deriving (Eq, Show);

  main = do
    s <- getContents
    print (alexScanTokens s)
}
