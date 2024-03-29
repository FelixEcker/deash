{$mode fpc}
unit uInternalProcs;

{$H+}

{ uInternalProcs.pp ; Implementations for internal commands/procedures }
{ Author: Marie Eckert                                                 }
interface
  uses Dos, SysUtils, Types, uExecutor, uHelpers, uTypes, uPathResolve,
       uXDebug;

  function DoInternalCmd(const AName: String;
                         const AParams: TStringDynArray;
                         var AScript: TScript): TInvokeResult;
implementation
  uses uScriptEngine;

  function Cd(const AParams: TStringDynArray): TInvokeResult;
  const
    ROOTDIR =
    {$IF defined(UNIX)}
      '/'
    {$ELSEIF defined(WINDOWS)}
      'C:\'
    {$ENDIF};
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
    debugwriteln('Resolved Envs in Path = '+dir);
    if pos(ROOTDIR, dir) <> 1 then
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

  procedure DebugCbTrace(var AScript: TScript);
  var
    i: Integer;
  begin
    writeln('Debug Codeblock Trace', sLineBreak);
    for i := HIGH(AScript.codeblocks) downto 0 do
      writeln(':: ', BlocktypeToStr(AScript.codeblocks[i]));
  end;

  function DoInternalCmd(const AName: String;
                         const AParams: TStringDynArray;
                         var AScript: TScript): TInvokeResult;
  begin
    DoInternalCmd.code := 0;

    case AName of
    'cd': DoInternalCmd := Cd(AParams);
    'purr': Purr;
    'debug_cbtrace': DebugCbTrace(AScript);
    'exec': exit; { << IMPLEMENT SOON }
    else
      DoInternalCmd.code := -1;
      DoInternalCmd.message := 'No such internal cmd';
    end;
  end;
end.
