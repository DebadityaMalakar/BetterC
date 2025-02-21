program BetterC_Main;

uses
  SysUtils;

procedure Compile(FileName: PChar); cdecl; external 'compiler.o';

var
  choice: Integer;
  FileName: string;
begin
  repeat
    WriteLn('==== BetterC Compiler ====');
    WriteLn('1. Compile');
    WriteLn('2. Compile and Run');
    WriteLn('3. Exit');
    Write('Enter choice: ');
    ReadLn(choice);

    if choice = 1 then
    begin
      Write('Enter source file (.oc): ');
      ReadLn(FileName)
