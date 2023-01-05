{$mode fpc}
program deash;

{$H+}

uses SysUtils, uXDebug, uInteractiveMode, uScriptEngine;

begin
  if (ParamCount = 0) then
    LaunchShell
  else
    DoScriptExec(ParamStr(1));
end.
