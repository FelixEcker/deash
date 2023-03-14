{$mode fpc}
unit uInternalProcs;

{ uInternalProcs.pp ; Implementations for DEASH internal commands/procedures }
{ Author: Marie Eckert                                                       }

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
  begin
    case AName of
    'purr': Purr;
    else
      DoInternalCmd.code := -1;
      DoInternalCmd.message := 'No such internal cmd';
    end;
  end;
end.
