{$mode fpc}
unit uInternalProcs;

{$H+}

{ uInternalProcs.pp ; Implementations for DEASH internal commands/procedures }
{ Author: Marie Eckert                                                       }

interface
  uses Dos, SysUtils, Types, uExecutor, uHelpers, uPathResolve;  

  function DoInternalCmd(const AName: String; const AParams: TStringDynArray): TInvokeResult;
implementation
  uses uScriptEngine;

  function Cd(const AParams: TStringDynArray): TInvokeResult;
  var
    dir: String;
  begin
    if Length(AParams) = 0 then
    begin
      Cd.code := -1;
      Cd.message := 'cd expects 1 argument: <path>';
      exit;
    end;

    dir := ResolveEnvsInPath(AParams[0]);
    if dir[1] <> PathDelim then
      dir := ExpandFileName(GetCurrentDir()+PathDelim+AParams[0]);

    if not DirectoryExists(dir) then
    begin
      Cd.code := -1;
      Cd.message := 'No such directory: '+dir;
      exit;
    end;

    if not SetCurrentDir(dir) then
    begin
      Cd.code := -1;
      Cd.message := 'Could not change directory (an unknown error occured)';
      exit;
    end;

    Cd.code := 0;
  end;

  procedure Purr;
  const
    CAT = 'ᓚᘏᗢ';
  var
    proc: TProcedure;
    alias: TAlias;
  begin
    writeln(CAT, ' deash version: ', GetShellEnv('SH_VERSION'));
    writeln(CAT, ' active for: ', TimeToStr(ProgramUptime()));
    writeln(CAT, ' user: ', GetEnv('USERNAME'));
    
    write(CAT, ' preffered exported procedures: ');
    if Length(preffered_exported_procs) = 0 then
      write('[NONE]');
    for proc in preffered_exported_procs do
      write(sLineBreak, '   ', proc.name);
    writeln;
    
    write(CAT, ' Aliases: ');
    if Length(aliases) = 0 then
      write('[NONE]');
    for alias in aliases do
      write(sLineBreak, '   ', alias.name);
    writeln;

    write(CAT, ' exported procedures: ');
    if Length(exported_procs) = 0 then
      write('[NONE]');
    for proc in exported_procs do
      write(sLineBreak, '   ', proc.name);
    writeln;
  end;

  function DoInternalCmd(const AName: String; const AParams: TStringDynArray): TInvokeResult;
  begin
    DoInternalCmd.code := 0;

    case AName of
    'cd': DoInternalCmd := Cd(AParams);
    'purr': Purr;
    'exec': exit; { << IMPLEMENT SOON }
    else
      DoInternalCmd.code := -1;
      DoInternalCmd.message := 'No such internal cmd';
    end;
  end;
end.
