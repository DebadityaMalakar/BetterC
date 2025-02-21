library FileOperations;

uses SysUtils;

function ReadFile(fileName: PChar): PChar; cdecl; export;
var
  fileHandle: TextFile;
  line, resultText: string;
  buffer: PChar;
begin
  resultText := '';
  AssignFile(fileHandle, fileName);
  {$I-} Reset(fileHandle); {$I+}

  if IOResult <> 0 then
  begin
    WriteLn('Error: Unable to open file ', fileName);
    Exit(nil);
  end;

  while not Eof(fileHandle) do
  begin
    ReadLn(fileHandle, line);
    resultText := resultText + line + LineEnding;
  end;

  CloseFile(fileHandle);

  buffer := StrAlloc(Length(resultText) + 1);
  StrPCopy(buffer, resultText);
  ReadFile := buffer;
end;

procedure WriteFile(fileName: PChar; content: PChar); cdecl; export;
var
  fileHandle: TextFile;
begin
  AssignFile(fileHandle, fileName);
  Rewrite(fileHandle);
  Write(fileHandle, content);
  CloseFile(fileHandle);
end;

procedure AppendFile(fileName: PChar; content: PChar); cdecl; export;
var
  fileHandle: TextFile;
begin
  AssignFile(fileHandle, fileName);
  {$I-} Append(fileHandle); {$I+}

  if IOResult <> 0 then
  begin
    Rewrite(fileHandle);
  end;

  Write(fileHandle, content);
  CloseFile(fileHandle);
end;

exports ReadFile, WriteFile, AppendFile;

begin
end.
