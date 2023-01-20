{$mode fpc}
unit uInteractiveMode;

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
