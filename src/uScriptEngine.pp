{$mode fpc}
unit uScriptEngine;

{ uScriptEngine.pp ; deash Script Execution Engine }
{ Author: Marie Eckert                             }

{$H+}{$R+}

interface
  uses Dos, SysUtils, StrUtils, Types, uXDebug, uDEASHConsts, uHelpers;

  type
    { TODO: Put all these data structures into their own unit,
            this is getting too long... }

    TVariable = record
      identifier, value: String;
      datatype: Integer;
    end;

    TVariableDynArray = array of TVariable;

    TScript = record
      scriptfile : TextFile;
      scriptpath : String;
      cline      : String;
      nline      : Integer;
      incomment  : Boolean;
      falseif    : Boolean;
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
  procedure SetShellEnv(const AName, AVal: String);
  function GetShellEnv(const AName: String): String;
  function ResolveVariable(const AName: String): TVariable;

  { Execution funcs }
  procedure DoScriptExec(const APath: String);
  function GetInvoke(const AName: String; var ATargetRec: TInvoke): Boolean;
  function DoInvoke(const AInvoke: TInvoke): TInvokeResult;
  function Eval(var AScript: TScript): TEvalResult;
  function EvalIf(var AScript: TScript; var AResult: TEvalResult): Boolean;

  var
    shell_env                : TVariableDynArray;
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

  procedure SetShellEnv(const AName, AVal: String);
  var
    i: Integer;
  begin
    for i := 0 to Length(shell_env)-1 do
    begin
      if shell_env[i].identifier = AName then
      begin
        shell_env[i].value := AVal;
        exit;
      end;
    end;

    SetLength(shell_env, Length(shell_env)+1);
    shell_env[HIGH(shell_env)].identifier := AName;
    shell_env[HIGH(shell_env)].value := AVal;
  end;

  function GetShellEnv(const AName: String): String;
  var
    sh_env: TVariable;
  begin
    GetShellEnv := '';

    for sh_env in shell_env do
    begin
      if sh_env.identifier = AName then
      begin
        GetShellEnv := sh_env.value;
        exit;
      end;
    end;
  end;

  function ResolveVariable(const AName: String): TVariable;
  begin
    ResolveVariable.datatype := -1;

    if AName[1] = '$' then // env variable
    begin
      ResolveVariable.identifier := Copy(AName, 2, Length(AName));
      ResolveVariable.value := GetEnv(ResolveVariable.identifier);
      ResolveVariable.datatype := DATATYPE_STRING;
      exit;
    end;

    if pos('SH_', AName) = 1 then
    begin
      ResolveVariable.identifier := AName;
      ResolveVariable.value := GetShellEnv(AName);
      ResolveVariable.datatype := DATATYPE_STRING;
    end;
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

    SetLength(script.codeblocks, 1);
    script.codeblocks[0] := BLOCKTYPE_NONE;

    script.nline := 0;
    while not eof(script.scriptfile) and (Length(script.codeblocks) > 0) do
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
    ifeval_res: TEvalResult;
    escaping: Boolean;
    curr_blocktype, i, j: Integer;
  begin
    Eval.success := True;
    Eval.message := 'succ';

    if (Length(AScript.cline) = 0) then exit;

    AScript.cline := Trim(AScript.cline);
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

    if Length(AScript.codeblocks) = 0 then exit;

    { Only execute line if we are not in any declaration block }

    curr_blocktype := AScript.codeblocks[HIGH(AScript.codeblocks)];
    if (curr_blocktype < BLOCKTYPE_PROC)
    or (curr_blocktype > BLOCKTYPE_VAR) then
    begin 
      case tokens[0] of
        'if': begin 
          ArrPushInt(AScript.codeblocks, BLOCKTYPE_IF);
          AScript.falseif := not EvalIf(AScript, ifeval_res);
          Eval := ifeval_res;
          exit; 
        end;
        'elif': begin
          if (curr_blocktype <> BLOCKTYPE_IF) then
          begin
            Eval.success := False;
            Eval.message := 'Invalid keyword "elif" outside of an IF-Block';
            exit;
          end;
          AScript.falseif := not EvalIf(AScript, ifeval_res);
          Eval := ifeval_res;
          exit;
        end;
        'else': begin
          if (curr_blocktype <> BLOCKTYPE_IF) then
          begin
            Eval.success := False;
            Eval.message := 'Invalid keyword "else" outside of an IF-Block';
            exit;
          end;
          AScript.falseif := not AScript.falseif;
          exit;
        end;
      end;
      
      if (curr_blocktype = BLOCKTYPE_IF) and AScript.falseif then exit;

      case tokens[0] of
        {'begin': exit; DEPRECATED LANG FEATURE }
        'env': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_ENV); exit; end;
        'alias': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_ALIAS); exit; end;
        'var': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_VAR); exit; end;
        'proc': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_PROC); exit; end;
        'return': begin ArrPopInt(AScript.codeblocks); exit; end;
        'exit': begin SetLength(AScript.codeblocks, 0); exit; end;
        '{': begin AScript.incomment := True; exit; end;
      else begin
        if not GetInvoke(tokens[0], invoke) then
        begin
          Eval.success := False;
          Eval.message := 'Unrecognized identifier: '+tokens[0];
          exit;
        end;
        debugwriteln('INVOKE LOCATION: '+invoke.location+' ; INVOKE TYPE: '+IntToStr(invoke.invoketype));
        inv_result := DoInvoke(invoke);
        if inv_result.code <> 0 then
        begin
          DeashError('Invoke finished with code '+IntToStr(inv_result.code));
          DeashError('Message: '+inv_result.message);
        end;
      end; end;
    end;
  end;

  function EvalIf(var AScript: TScript; var AResult: TEvalResult): Boolean;
  var
    i, skip, lefthandDT, righthandDT: Integer;
    lefthandVal, righthandVal: TVariable;
    operand, perform_bitwise: String;
    res_stash: Boolean;
    split: TStringDynArray;
  begin
    EvalIf := False;
    res_stash := False;
    AResult.success := True;

    perform_bitwise := '';
    split := SplitString(AScript.cline, ' ');
    skip := 0;
    for i := 1 to Length(split)-2 do
    begin
      if skip > 0 then
      begin
        skip := skip - 1;
        continue;
      end;

      case split[i] of
      'and': begin perform_bitwise := 'and'; continue; end;
      'or': begin perform_bitwise := 'or'; continue; end;
      end;

      { Determine Datatype, throw error if mismatch }
      if (i >= Length(split)) or (i + 2 >= Length(split))
      or (split[i+1] = 'and') or (split[i+1] = 'or') then
      begin
        { Do this to allow for checking if a boolean is true without a comparison }
        if (i < Length(split)) then 
        begin
          lefthandDT := DetermineDatatype(split[i]);
          if lefthandDT = DATATYPE_VARIABLE then
          begin
            lefthandVal := ResolveVariable(split[i]);
            lefthandDT := lefthandVal.datatype;
          end;
          
          if lefthandDT <> DATATYPE_BOOLEAN then
          begin
            AResult.success := False;
            AResult.message := 'malformed conditional: expected VALUE OPERATOR VALUE. This syntax is only allowed if the Variable/Value is of type Boolean!';
            exit;
          end;

          righthandDT := DATATYPE_BOOLEAN;
          operand := '=';
          lefthandVal.value := split[i];
          righthandVal.value := 'true';
        end else 
        begin
          debugwriteln(sLineBreak + 'Conditional evaluation failed on word: '+split[i]);
          AResult.success := False;
          AResult.message := 'malformed conditional: expected VALUE OPERATOR VALUE';
          exit;
        end;
      end else
      begin
        lefthandDT := DetermineDatatype(split[i]);
        righthandDT := DetermineDatatype(split[i+2]);
        operand := split[i+1];

        { Get actual variable datatype if its a variable, if its a string clean it up }
        if lefthandDT = DATATYPE_VARIABLE then
        begin
          lefthandVal := ResolveVariable(split[i]);
          lefthandDT := lefthandVal.datatype;
        end else
        begin
          lefthandVal.value := split[i];
          if lefthandDT = DATATYPE_STRING then
            lefthandVal.value := Copy(lefthandVal.value, 2, Length(lefthandVal.value)-1)
        end;

        if righthandDT = DATATYPE_VARIABLE then
        begin
          righthandVal := ResolveVariable(split[i+2]);
          righthandDT := righthandVal.datatype;
        end else
        begin
          righthandVal.value := split[i+2];
          if righthandDT = DATATYPE_STRING then
            righthandVal.value := Copy(righthandVal.value, 2, Length(righthandVal.value)-2)
        end;
        
        { Check if the two values can be compared }
        if lefthandDT <> righthandDT then
        begin
          AResult.success := False;
          AResult.message := Format('mismatched datatypes for comparison (%s and %s)', [DatatypeToStr(lefthandDT), DatatypeToStr(righthandDT)]);
          exit;
        end;
        skip := 2;
      end;

      { Stash the current result incase we do a bitwise }
      res_stash := EvalIf;
      
      { Do an actual comparison of the values }
      case operand of
        '=': EvalIf := righthandVal.value = lefthandVal.value;
        '<>': EvalIf := not (righthandVal.value = lefthandVal.value);
        '<', '>': begin
          if (righthandDT <> DATATYPE_INTEGER) or (lefthandDT <> DATATYPE_INTEGER) then
          begin
            AResult.success := False;
            AResult.message := Format('greater/lesser comparison only applicable for integers (got %s and %s)!', [DatatypeToStr(lefthandDT), DatatypeToStr(righthandDT)]);
            exit;
          end;

          EvalIf := StrToInt(lefthandVal.value) >  StrToInt(righthandVal.value);
          if operand = '<' then EvalIf := not EvalIf;
        end;
      end;

      case perform_bitwise of
      'and': begin EvalIf := EvalIf and res_stash; perform_bitwise := ''; end;
      'or': begin EvalIf := EvalIf or res_stash; perform_bitwise := ''; end;
      end;
    end;
  end;
end.
