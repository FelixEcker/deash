{$mode fpc}
unit uInternalProcs;

{$H+}

{ uInternalProcs.pp ; Implementations for DEASH internal commands/procedures }
{ Author: Marie Eckert                                                       }

interface
  uses Dos, SysUtils, Types, uExecutor, uHelpers;  

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

    if AParams[0][1] = PathDelim then
      dir := AParams[0]
    else
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
  begin
    writeln(CAT, ' deash version: ', GetShellEnv('SH_VERSION'));
    writeln(CAT, ' active for: ', TimeToStr(ProgramUptime()));
    writeln(CAT, ' user: ', GetEnv('USERNAME'));
    writeln(CAT, ' preffered exported procedures: ');
    writeln('    wip');
    writeln(CAT, ' Aliases:');
    writeln(CAT, ' exported procedures: ');
  end;

  function DoInternalCmd(const AName: String; const AParams: TStringDynArray): TInvokeResult;
  begin
    case AName of
    'cd': DoInternalCmd := Cd(AParams);
    'purr': Purr;
    else
      DoInternalCmd.code := -1;
      DoInternalCmd.message := 'No such internal cmd';
    end;
  end;
end.
