program BuildBetterC;

uses SysUtils;

begin
  WriteLn('Building BetterC Compiler...');

  // Compile Pascal Files
  if Exec('fpc src\pascal\fileio.pas') <> 0 then
  begin
    WriteLn('Error: Failed to compile fileio.pas');
    Halt(1);
  end;

  if Exec('fpc src\pascal\tokenizer.pas') <> 0 then
  begin
    WriteLn('Error: Failed to compile tokenizer.pas');
    Halt(1);
  end;

  if Exec('fpc src\pascal\main.pas -o build\BetterC.exe') <> 0 then
  begin
    WriteLn('Error: Failed to compile main.pas');
    Halt(1);
  end;

  // Assemble & Link ASM Files
  if Exec('nasm -f win32 src\asm\compiler.asm -o src\asm\compiler.obj') <> 0 then
  begin
    WriteLn('Error: Failed to assemble compiler.asm');
    Halt(1);
  end;

  if Exec('gcc -m32 src\asm\compiler.obj -o build\compiler.exe -L. -lfileio -ltokenizer') <> 0 then
  begin
    WriteLn('Error: Failed to link compiler.asm');
    Halt(1);
  end;

  WriteLn('Build complete. Executable created in build\BetterC.exe');
end.
