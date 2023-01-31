{$mode fpc}
unit uInteractiveMode;

{ uInteractiveMode.pp ; Interactive Shell for DEASH }
{ author: Felix Eckert                              }

{$H+}

interface
  uses SysUtils, StrUtils, Types, uXDebug, uScriptEngine, uPathResolve;

  procedure LaunchShell;
implementation
  procedure LaunchShell;
  begin
    debugwriteln('Launching shell');
    DoScriptExec(ResolveEnvsInPath('$HOME/.deashrc'));
  end;
end.
