{$mode fpc}
unit uInteractiveMode;

{ uInteractiveMode.pp ; Interactive Shell for DEASH }
{ author: Marie Eckert                              }

{$H+}

interface
  uses SysUtils, StrUtils, Types, uDEASHConsts, uHelpers, uXDebug, uScriptEngine, uPathResolve;

  procedure LaunchShell;
implementation
  procedure LaunchShell;
  var
    eval_result: TEvalResult;
    script: TScript;
  begin
    debugwriteln('Launching shell');
    DoScriptExec(ResolveEnvsInPath('$HOME/.deashrc'));

    script.exited := False;
    script.nline := 1;
    script.incomment := False;
    SetLength(script.codeblocks, 1);
    script.codeblocks[0] := BLOCKTYPE_NONE;

    while not script.exited do
    begin
      write('deash> ');
      readln(script.cline);

      eval_result := Eval(script);
      if not eval_result.success then
        DeashError(eval_result.message);

      script.nline := script.nline + 1;
    end;
  end;
end.
