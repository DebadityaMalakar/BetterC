library tokenizer;

uses SysUtils;

type
  TokenType = (ttIdentifier, ttKeyword, ttNumber, ttString, ttOperator, ttSymbol, ttUnknown, ttEOF);

  Token = record
    TokenType: TokenType;
    Value: string;
  end;

var
  SourceCode: string;
  Position: Integer;

function IsAlpha(c: Char): Boolean;
begin
  IsAlpha := (c in ['a'..'z', 'A'..'Z', '_']);
end;

function IsDigit(c: Char): Boolean;
begin
  IsDigit := (c in ['0'..'9']);
end;

function IsWhitespace(c: Char): Boolean;
begin
  IsWhitespace := (c in [' ', #9, #10, #13]);
end;

function ReadNextToken: Token;
var
  CurrentChar: Char;
  TokenValue: string;
begin
  while (Position <= Length(SourceCode)) and IsWhitespace(SourceCode[Position]) do
    Inc(Position);

  if Position > Length(SourceCode) then
  begin
    Result.TokenType := ttEOF;
    Result.Value := '';
    Exit;
  end;

  CurrentChar := SourceCode[Position];

  if IsAlpha(CurrentChar) then
  begin
    TokenValue := '';
    while (Position <= Length(SourceCode)) and (IsAlpha(SourceCode[Position]) or IsDigit(SourceCode[Position])) do
    begin
      TokenValue := TokenValue + SourceCode[Position];
      Inc(Position);
    end;
    Result.TokenType := ttIdentifier;
    Result.Value := TokenValue;
    Exit;
  end;

  if IsDigit(CurrentChar) then
  begin
    TokenValue := '';
    while (Position <= Length(SourceCode)) and IsDigit(SourceCode[Position]) do
    begin
      TokenValue := TokenValue + SourceCode[Position];
      Inc(Position);
    end;
    Result.TokenType := ttNumber;
    Result.Value := TokenValue;
    Exit;
  end;

  Result.TokenType := ttUnknown;
  Result.Value := CurrentChar;
  Inc(Position);
end;

function Tokenize(Input: PChar): PChar; cdecl; export;
const
  MAX_OUTPUT_SIZE = 1024;
var
  TokenizedData: PChar;
  Token: Token;
begin
  SourceCode := string(Input);
  Position := 1;

  TokenizedData := StrAlloc(MAX_OUTPUT_SIZE);
  StrCopy(TokenizedData, '');

  repeat
    Token := ReadNextToken;
    StrCat(TokenizedData, PChar(Token.Value + ' '));
  until Token.TokenType = ttEOF;

  Result := TokenizedData;
end;

exports Tokenize;

begin
end.
