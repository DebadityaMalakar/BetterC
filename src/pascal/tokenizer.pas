library tokenizer;

{$mode objfpc}  // Enables `Result` usage

uses
  SysUtils;

type
  TokenType = (ttIdentifier, ttKeyword, ttNumber, ttString, ttOperator, ttSymbol, ttUnknown, ttEOF);
  
  Token = record
    TokenType: TokenType;
    Value: AnsiString;  // Use AnsiString for compatibility
  end;

var
  SourceCode: AnsiString;
  Position: Integer;

function IsAlpha(c: Char): Boolean;
begin
  IsAlpha := (c in ['a'..'z', 'A'..'Z', '_']);  // ✅ Use function name instead of Result
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
  TokenValue: AnsiString;
  TempToken: Token;
begin
  while (Position <= Length(SourceCode)) and IsWhitespace(SourceCode[Position]) do
    Inc(Position);
    
  if Position > Length(SourceCode) then
  begin
    TempToken.TokenType := ttEOF;
    TempToken.Value := '';
    ReadNextToken := TempToken;  // ✅ Assign to function name
    Exit;
  end;
  
  CurrentChar := SourceCode[Position];
  
  if IsAlpha(CurrentChar) then
  begin
    TokenValue := '';
    while (Position <= Length(SourceCode)) and 
          (IsAlpha(SourceCode[Position]) or IsDigit(SourceCode[Position])) do
    begin
      TokenValue := TokenValue + SourceCode[Position];
      Inc(Position);
    end;
    TempToken.TokenType := ttIdentifier;
    TempToken.Value := TokenValue;
    ReadNextToken := TempToken;
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
    TempToken.TokenType := ttNumber;
    TempToken.Value := TokenValue;
    ReadNextToken := TempToken;
    Exit;
  end;
  
  TempToken.TokenType := ttUnknown;
  TempToken.Value := CurrentChar;
  Inc(Position);
  ReadNextToken := TempToken;
end;

function Tokenize(Input: PChar): PChar; export;
const
  MAX_OUTPUT_SIZE = 1024;
var
  TokenizedData: array[0..MAX_OUTPUT_SIZE-1] of Char;
  Token: Token;
  OutputPos: Integer;
  i: Integer;
  TempStr: AnsiString;
begin
  SourceCode := AnsiString(StrPas(Input));  // ✅ Convert PChar to AnsiString
  Position := 1;
  OutputPos := 0;
  FillChar(TokenizedData, SizeOf(TokenizedData), 0);
  
  repeat
    Token := ReadNextToken;
    
    // Convert Token.Value to AnsiString before using PChar
    TempStr := Token.Value + ' ';
    
    if (OutputPos + Length(TempStr) + 1) < MAX_OUTPUT_SIZE then
    begin
      for i := 1 to Length(TempStr) do
      begin
        TokenizedData[OutputPos] := TempStr[i];
        Inc(OutputPos);
      end;
    end;
  until Token.TokenType = ttEOF;
  
  TokenizedData[OutputPos] := #0;  // Null terminate
  Tokenize := StrNew(PChar(TempStr));  // ✅ Convert AnsiString properly to PChar
end;

exports
  Tokenize;

begin
end.