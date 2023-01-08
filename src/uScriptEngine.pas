{$mode fpc}
unit uScriptEngine;

{ uScriptEngine.pas ; deash Script Execution Engine }
{ Author: Felix Eckert                              }

{$H+}

interface
  uses SysUtils, StrUtils, Types, uXDebug, uDEASHConsts, uHelpers;

  type
    { TODO: Put all these data structures into their own unit,
            this is getting too long... }

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

    TInvoke = record
      invoketype : Integer;
      location   : String;
    end;

    TParameter = record
      name    : String;
      ptype   : String;
      default : String;
    end;

    TParameterDynArray = array of TParameter;

    TProcedure = record
      name       : String;
      parameters : TParameterDynArray;
      lines      : String;
    end;

    TProcedureDynArray = array of TProcedure;

    TAlias = record
      name : String;
      cont : String;
    end;

    TAliasDynArray = array of TAlias;

  { Helper funcs }
  function FindExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  function FindPrefferedExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  function FindAlias(const AName: String; var AAliasRec: TAlias): Boolean;

  { Execution helper }
  procedure DoScriptExec(const APath: String);
  function GetInvoke(const AName: String; var ATargetRec: TInvoke): Boolean;
  function Eval(var AScript: TScript): TEvalResult;

  var
    preffered_exported_procs : TProcedureDynArray;
    exported_procs           : TProcedureDynArray;
    aliases                  : TAliasDynArray;
implementation
  function FindExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  begin
  end;
  
  function FindPrefferedExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  begin
  end;

  function FindAlias(const AName: String; var AAliasRec: TAlias): Boolean;
  begin
  end;

  procedure DoScriptExec(const APath: String);
  var
    script: TScript;
    evalres: TEvalResult;
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
      evalres := Eval(script);

      if not evalres.success then
      begin
        writeln(Format('deash <ERROR>:: eval for script %s failed at line %d:%s:: %s', 
              [script.scriptpath, script.nline, sLineBreak, evalres.message]));
        break;
      end;
    end;
  end;

  function GetInvoke(const AName: String; var ATargetRec: TInvoke): Boolean;
  var
    procrec: TProcedure;
    aliasrec: TAlias;
    path: String;
  begin
    GetInvoke := True;

    if FindPrefferedExpProc(AName, procrec) then
    begin
      ATargetRec.invoketype := INVOKETYPE_PREF_PROC;
      ATargetRec.location := AName;
      exit;
    end;

    if FindAlias(AName, aliasrec) then
    begin
      ATargetRec.invoketype := INVOKETYPE_ALIAS;
      exit;
    end;

    if BinaryExists(AName, path) then
    begin
      ATargetRec.invoketype := INVOKETYPE_BINARY;
      ATargetRec.location := path;
      exit;
    end;

    if FindExpProc(AName, procrec) then
    begin
      ATargetRec.invoketype := INVOKETYPE_PROC;
      ATargetRec.location := AName;
      exit;
    end;

    GetInvoke := False;
  end;

  function Eval(var AScript: TScript): TEvalResult;
  var
    tokens: TStringDynArray;
    invoke: TInvoke;
  begin
    Eval.success := True;
    Eval.message := 'succ';

    tokens := SplitString(AScript.cline, ' ');
    case tokens[0] of
      'if': exit;
      'elif': exit;
      'else': exit;
      'begin': exit;
      'end': exit;
    else begin
      if not GetInvoke(tokens[0], invoke) then
      begin
        Eval.success := False;
        Eval.message := 'Unrecognized identifier: '+tokens[0];
      end;
    end; end;
  end;
end.
