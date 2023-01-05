{$mode fpc}
unit uScriptEngine;

{$H+}

interface
  uses SysUtils, StrUtils, uXDebug;

  type
    TVariable = record
      identifier, value: String;
    end;

    TVariableDynArray = array of TVariable;

    TScript = record
      scriptfile : TextFile;
      scriptpath : String;
      cline      : String;
      nline      : Integer;
      vars       : TVariableDynArray;
    end;

    TEvalResult = record
      success: Boolean;
      message: String;
    end;

  procedure DoScriptExec(const APath: String);
  function Eval(const ALine: String): TEvalResult;
implementation
  procedure DoScriptExec(const APath: String);
  var
    script: TScript;
  begin
    debugwriteln('Executing deash script '+APath);

    script.scriptpath := APath;
    Assign(script.scriptfile, script.scriptpath);
    ReSet(script.scriptfile);

    script.nline := 0;
    while not eof(script.scriptfile) do
    begin
      ReadLn(script.scriptfile, script.cline);
      script.nline := script.nline + 1;
      writeln(Format('%.3d %s', [script.nline, script.cline]));
    end;
  end;
end.
