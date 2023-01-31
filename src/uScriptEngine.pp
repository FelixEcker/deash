{$mode fpc}
unit uScriptEngine;

{ uScriptEngine.pp ; deash Script Execution Engine }
{ Author: Felix Eckert                             }

{$H+}{$R+}

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
      incomment  : Boolean;
      codeblocks : TIntegerDynArray;
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

    TInvokeResult = record
      code: Integer;
      message: String;
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
  function ExtractProcName(const ASrc: String): String;
  function FindExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  function FindPrefferedExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  function FindAlias(const AName: String; var AAliasRec: TAlias): Boolean;
  function IsInternalCmd(const ACmd: String): Boolean;

  { Execution funcs }
  procedure DoScriptExec(const APath: String);
  function GetInvoke(const AName: String; var ATargetRec: TInvoke): Boolean;
  function DoInvoke(const AInvoke: TInvoke): TInvokeResult;
  function Eval(var AScript: TScript): TEvalResult;

  var
    preffered_exported_procs : TProcedureDynArray;
    exported_procs           : TProcedureDynArray;
    aliases                  : TAliasDynArray;
implementation
  { Helper funcs }
  function ExtractProcName(const ASrc: String): String;
  var
    i: Integer;
  begin
    ExtractProcName := '';
    for i := 1 to Length(ASrc) do
      if (ASrc[i] = ' ') or (ASrc[i] = '(') or (ASrc[i] = ';') then
        break
      else
        ExtractProcName := ExtractProcName + ASrc[i];   
  end;
  
  function FindExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  var
    i: Integer;
  begin
    FindExpProc := False;
    for i := 0 to Length(exported_procs)-1 do
    begin
      if (exported_procs[i].name = AName) then
      begin
        AProcRec := exported_procs[i];
        FindExpProc := True;
        exit;
       end;
    end;
  end;
  
  function FindPrefferedExpProc(const AName: String; var AProcRec: TProcedure): Boolean;
  var
    i: Integer;
  begin
    FindPrefferedExpProc := False;
    for i := 0 to Length(preffered_exported_procs)-1 do
    begin
      if (preffered_exported_procs[i].name = AName) then
      begin
        AProcRec := preffered_exported_procs[i];
        FindPrefferedExpProc := True;
        exit;
       end;
    end;
  end;

  function FindAlias(const AName: String; var AAliasRec: TAlias): Boolean;
  var
    i: Integer;
  begin
    FindAlias := False;
    for i := 0 to Length(aliases)-1 do
    begin
      if (aliases[i].name = AName) then
      begin
        AAliasRec := aliases[i];
        FindAlias := True;
        exit;
       end;
    end;
  end;

  function IsInternalCmd(const ACmd: String): Boolean;
  begin
    IsInternalCmd := True;

    case ACmd of
      'cd': exit;
      'purr': exit;
    end;

    IsInternalCmd := False;
  end;

  { Execution funcs }

  procedure DoScriptExec(const APath: String);
  var
    script: TScript;
    evalres: TEvalResult;
  begin
    debugwriteln('Executing deash script '+APath);

    if not FileExists(APath) then
    begin
      DeashError('File not Found: '+APath);
      exit;
    end;

    script.incomment := False;
    script.scriptpath := APath;
    Assign(script.scriptfile, script.scriptpath);
    ReSet(script.scriptfile);

    SetLength(script.codeblocks, 0);

    script.nline := 0;
    while not eof(script.scriptfile) do
    begin
      ReadLn(script.scriptfile, script.cline);
      debugwriteln('= '+script.cline);
      script.nline := script.nline + 1;
      evalres := Eval(script);
      if not evalres.success then
      begin
        DeashError(Format('eval for script %s failed at line %d:%s:: %s', 
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

    if IsInternalCmd(AName) then
    begin
      ATargetRec.invoketype := INVOKETYPE_INTERNAL;
      ATargeTRec.location := AName;
      exit;
    end;

    if FindPrefferedExpProc(ExtractProcName(AName), procrec) then
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

    if FindExpProc(ExtractProcName(AName), procrec) then
    begin
      ATargetRec.invoketype := INVOKETYPE_PROC;
      ATargetRec.location := AName;
      exit;
    end;
    GetInvoke := False;
  end;

  function DoInvoke(const AInvoke: TInvoke): TInvokeResult;
  begin
    DoInvoke.code := 0;
    DoInvoke.message := '';
  end;

  { Evaluates the cline of the given TScript record }
  function Eval(var AScript: TScript): TEvalResult;
  var
    tokens: TStringDynArray;
    invoke: TInvoke;
    inv_result: TInvokeResult;
    escaping: Boolean;
    i, j: Integer;
  begin
    Eval.success := True;
    Eval.message := 'succ';

    if (Length(AScript.cline) = 0) then exit;
    if (AScript.cline[1] = '#') then exit;

    tokens := SplitString(Trim(AScript.cline), ' ');

    { Skip until out of comment }
    i := 0;
    escaping := False;
    while AScript.incomment and (i < Length(tokens)) do
    begin
      for j := 1 to Length(tokens[i]) do
      begin
        if (tokens[i][j] = '}') and not escaping then
        begin
          tokens[i] := Copy(tokens[i], j, Length(tokens[i])-1);
          tokens := Copy(tokens, i, Length(tokens)-1);
          AScript.incomment := False;
          escaping := False;
          break;
        end;
        if (tokens[i][j] = '\') then
          escaping := not escaping;
      end;
      i := i + 1;
    end;
    if AScript.incomment then exit;
    if tokens[0] = '' then exit;
    if tokens[0] = 'end' then
    begin
      ArrPopInt(AScript.codeblocks);
      exit;
    end;
 
    { Only execute line if we are not in any declaration block }
    if (Length(AScript.codeblocks) = 0)
    or (AScript.codeblocks[HIGH(AScript.codeblocks)] < BLOCKTYPE_PROC)
    or (AScript.codeblocks[HIGH(AScript.codeblocks)] > BLOCKTYPE_VAR) then
    begin
      case tokens[0] of
        'if': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_IF); exit; end;
        'elif': exit;
        'else': exit;
        'begin': exit;
        'env': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_ENV); exit; end;
        'alias': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_ALIAS); exit; end;
        'var': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_VAR); exit; end;
        'proc': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_PROC); exit; end;
        '{': begin AScript.incomment := True; exit; end;
      else begin
        if not GetInvoke(tokens[0], invoke) then
        begin
          Eval.success := False;
          Eval.message := 'Unrecognized identifier: '+tokens[0];
          exit;
        end;

        inv_result := DoInvoke(invoke);
        if inv_result.code <> 0 then
        begin
          DeashError('Invoke finished with code '+IntToStr(inv_result.code));
          DeashError('Message: '+inv_result.message);
        end;
      end; end;
    end;
  end;
end.
