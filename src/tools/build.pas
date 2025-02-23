program Build;
uses SysUtils;

procedure RunCommand(const Cmd: string; const Args: array of UnicodeString);
begin
  if ExecuteProcess(Cmd, Args) <> 0 then
  begin
    Writeln('Error running: ', Cmd);
    Halt(1);
  end;
end;

begin
  RunCommand('nasm', ['-f', 'elf64', 'src/asm/compiler.asm']);
  RunCommand('fpc', ['-Mtp', '-Sg', '-O2', 'src/pascal/tokenizer.pas']);
  RunCommand('fpc', ['-Mtp', '-Sg', '-O2', 'src/pascal/fileio.pas']);
  RunCommand('fpc', ['-Mtp', '-Sg', '-O2', 'src/tools/build.pas']);
  Writeln('Build completed.');
end.
