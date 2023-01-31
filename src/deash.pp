{$mode fpc}
program deash;

{ deash.pp ; Entry Point for deash }
{ author: Felix Eckert             }

{$H+}

uses SysUtils, uXDebug, uDEASHConsts, uInteractiveMode, uScriptEngine;

procedure DeashInfo;
begin
  writeln(':: deash version ', VERSION, '-', VERSION_TYPE);
  writeln(':: by ', DEV_INFO);
  writeln(':: git repository: https://github.com/FelixEcker/deash.git ');
  writeln(':: licensed under the bsd 3-claues license');
end;

begin
  if (ParamCount = 0) then
    LaunchShell
  else if (ParamStr(1) = '--info') then
    DeashInfo
  else
    DoScriptExec(ParamStr(1));
end.
