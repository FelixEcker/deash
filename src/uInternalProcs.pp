{$mode fpc}
unit uInternalProcs;

interface
  uses Dos, SysUtils, Types, uExecutor, uHelpers;  

  function DoInternalCmd(const AName: String; const AParams: TStringDynArray): TInvokeResult;
implementation
  uses uScriptEngine;

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

  function DoInternalCmd(const AName: String; const AParams:TStringDynArray): TInvokeResult;
  var
    tmp: String;
  begin
    DoInternalCmd.code := 0;
    DoInternalCmd.message := '';

    case AName of
    'purr': Purr;
    'cd': begin
      if AParams[0][1] <> PathDelim then
        tmp := ExpandFileName(GetCurrentDir()+PathDelim+AParams[0])
      else
        tmp := AParams[0];

      if not DirectoryExists(tmp) then
      begin
        DoInternalCmd.code := -1;
        DoInternalCmd.message := 'No such directory: '+tmp;
        exit;
      end;

      if not SetCurrentDir(tmp) then
      begin
        DoInternalCmd.code := -1;
        DoInternalCmd.message := 'Failed to change directory because of an unknown error';
      end;
    end;
    else
      DoInternalCmd.code := -1;
      DoInternalCmd.message := 'No such internal cmd';
    end;
  end;
end.
