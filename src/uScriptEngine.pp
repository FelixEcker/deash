{$mode fpc}
unit uScriptEngine;

{ uScriptEngine.pp ; deash Script Execution Engine }
{ Author: Marie Eckert                             }

{$H+}{$R+}

interface
  uses Dos, SysUtils, StrUtils, Types, uXDebug, uDEASHConsts, uExecutor, uHelpers, uInternalProcs, uTypes, uErrors;

  type
    TEvalResult = record
      success: Boolean;
      message: String;
    end;

    TAlias = record
      name : String;
      cont : String;
    end;

    TAliasDynArray = array of TAlias;

  { Helper funcs }

  (* Extract a Procedure Name out of ASrc *)
  function ExtractProcName(const ASrc: String): String;

  (* Find a Procedure using the normal Search order (Preffered, Exported, Local)  *)
  function FindProcedure(const AName: String; var AScript: TScript; var ATargetRec: TProcedure): Integer;

  (* Extract the Parameters of a Procedure Invoke *)
  function ExtractProcParams(const AInvoke: String): TStringDynArray;

  (* Find an Exported Procedure with name AName and put its record into AProcRec.
     Returns True when an Exported Procedure of given name was found, False if not. *)
  function FindExpProc(const AName: String; var AProcRec: TProcedure): Boolean;

  (* Find an Preffered Exported Procedure with name AName and put its record into AProcRec.
     Returns True when an Preffered Exported Procedure of given name was found, False if not. *)
  function FindPrefferedExpProc(const AName: String; var AProcRec: TProcedure): Boolean;  

  (* Find an Alias with name AName and put its record into AAliasRec.
     Returns True when an Alias of given name was found, False if not. *)
  function FindAlias(const AName: String; var AAliasRec: TAlias): Boolean;

  (* Returns True if the command name in ACmd is an integrated command, False if not *)
  function IsInternalCmd(const ACmd: String): Boolean;

  (* Set a Shell Environment Variable, see deash_spec.sad#builtin-cmds-vars *)
  procedure SetShellEnv(const AName, AVal: String);

  (* Get the value of a Shell Environment Variable *)
  function GetShellEnv(const AName: String): String;

  (* Resolve the variable name AName to a Variable Record (function result) in script AScript *)
  function ResolveVariable(var AScript: TScript; const AName: String): TVariable;

  { Execution funcs }

  (* Execute a deash script file at APath *)
  procedure DoScriptExec(const APath: String);

  (* Register a procedure in the script and start its loading *)
  procedure RegisterProc(const ADeclaration: String; var AScript: TScript);

  (* Execute the procedure AProcedure with the parameters AParameters *)
  function ExecProc(const AProcedure: TProcedure; const AParameters: TStringDynArray; var AScript: TScript): TProcedureResult;

  (* Get an Invoke with name AName and store it into ATargetRec.
     Returns True if the invoke exists, False if not. *)
  function GetInvoke(const AName: String; var AScript: TScript; var ATargetRec: TInvoke): Boolean;

  (* Execute the Invoke in AInvoke and return its InvokeResult. *)
  function DoInvoke(const AInvoke: TInvoke; var AScript: TScript): TInvokeResult;

  (* Evaluate the current line of the script AScript *)
  function Eval(var AScript: TScript): TEvalResult;

  (* Evaluate the current line of the script AScript as an If-Conditional *)
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

  function FindProcedure(const AName: String; var AScript: TScript; var ATargetRec: TProcedure): Integer;
  begin
    FindProcedure := INVOKETYPE_PREF_PROC;

    if FindPrefferedExpProc(AName, ATargetRec) then exit;

    FindProcedure := INVOKETYPE_EXP_PROC;
    if FindExpProc(AName, ATargetRec) then exit;

    FindProcedure := INVOKETYPE_PROC;
    for ATargetRec in AScript.procedures do
      if ATargetRec.name = AName then
        exit;

    FindProcedure := -1;
  end;

  function ExtractProcParams(const AInvoke: String): TStringDynArray;
  var
    open_pos, i: Integer;
    in_string, escaping: Boolean;
    param_string: String;
    params: TStringDynArray;
  begin
    open_pos := pos('(', AInvoke)+1;
    param_string := Copy(AInvoke, open_pos, Length(AInvoke)-open_pos);

    SetLength(params, 1);

    in_string := False;
    escaping := False;
    for i := 1 to Length(param_string) do
    begin
      if (param_string[i] = '\') and not escaping then 
      begin
        escaping := True;
        continue;
      end;

      if (param_string[i] = '''') and not escaping then
      begin
        in_string := not in_string;
        continue;
      end;

      if (param_string[i] = ',') and not in_string then
      begin
        SetLength(params, Length(params)+1);
        continue;
      end;

      params[HIGH(params)] := params[HIGH(params)] + param_string[i];
      escaping := False;
    end;

    for i := 0 to Length(params) - 1 do
      params[i] := trim(params[i]);
    
    ExtractProcParams := params;
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
      'debug_cbtrace': exit;
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

  function ResolveVariable(var AScript: TScript; const AName: String): TVariable;
  begin
    ResolveVariable.datatype := -1;

    if pos('$', AName) = 1 then // env variable
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
  procedure RegisterProc(const ADeclaration: String; var AScript: TScript);
  var
    split: TStringDynArray;
    i, stopped, ptype, proc_type, id: Integer;
    tmp: String;
    proc: TProcedure;
    has_params, skip_next, stop: Boolean;
  begin
    split := SplitString(ADeclaration, ' ');
    split := Copy(split, 1, Length(split)-1);

    has_params := pos('(', split[0]) <> 0;

    { Extract name }
    if has_params then
    begin
      proc.name := Copy(split[0], 1, pos('(', split[0])-1);
      split[0] := Copy(split[0], pos('(', split[0])+1, Length(split[0]));
    end else
      proc.name := Copy(split[0], 1, pos(';', split[0])-1);

    { Extract Parameters }
    if has_params then
    begin
      skip_next := False;
      for i := 0 to Length(split)-1 do
      begin
        if skip_next then
        begin
          skip_next := False;
          continue;
        end;

        SetLength(proc.parameters, Length(proc.parameters)+1);
        proc.parameters[HIGH(proc.parameters)].name := Copy(split[i], 1, Length(split[i])-1);

        tmp := Copy(split[i+1], 1, Length(split[i+1])-1);
        if (pos(')', tmp) <> 0) then 
        begin 
          tmp := Copy(tmp, 1, pos(')', tmp)-1);
          stop := True;
        end;

        ptype := StrToDatatype(tmp);
        if ptype = DATATYPE_UNREAL then
        begin
          ThrowError(ERR_SCRIPT_INVALID_DATATYPE, AScript, [Copy(split[i+1], 1, Length(split[i+1])-1)]);
          exit;
        end;

        proc.parameters[HIGH(proc.parameters)].ptype := ptype;

        if stop then begin stopped := i; break; end;
        skip_next := True;
      end;
    end;

    proc_type := PROCTYPE_PROC;
    if stopped+1 < Length(split) then
    begin
      for i := stopped + 1 to Length(split) - 1 do
      begin
        if pos('export', split[i]) <> 0 then proc_type := PROCTYPE_EXP;
        if pos('preffered', split[i]) <> 0 then proc_type := PROCTYPE_PREF;
      end;
    end;

    case proc_type of
    PROCTYPE_PROC: begin
      SetLength(AScript.procedures, Length(AScript.procedures)+1);
      AScript.procedures[HIGH(AScript.procedures)] := proc;
      id := HIGH(AScript.procedures); 
    end;
    PROCTYPE_EXP: begin
      SetLength(exported_procs, Length(exported_procs)+1);
      AScript.procedures[HIGH(exported_procs)] := proc;
      id := HIGH(exported_procs); 
    end;
    PROCTYPE_PREF: begin
      SetLength(preffered_exported_procs, Length(preffered_exported_procs)+1);
      AScript.procedures[HIGH(preffered_exported_procs)] := proc;
      id := HIGH(preffered_exported_procs); 
    end;
    end;
    
    AScript.registering_proc := id;
    AScript.registering_proc_type := proc_type;
  end;
  
  function ExecProc(const AProcedure: TProcedure; const AParameters: TStringDynArray; var AScript: TScript): TProcedureResult;
  var
    invoke_result: TInvokeResult;
    script: TScript;
    evalres: TEvalResult;
    parameter: String;
    i: Integer;
  begin
    if AProcedure.internal then
    begin
      invoke_result := DoInternalCmd(AProcedure.name, AParameters, AScript);
      ExecProc.success := invoke_result.code = 0;
      ExecProc.return_value := invoke_result.message;
      exit;
    end;

    script.incomment := False;
    script.scriptpath := 'procedure:'+AProcedure.name;

    SetLength(script.codeblocks, 1);
    script.codeblocks[0] := BLOCKTYPE_NONE;

    SetLength(script.vars, 1);
    script.vars[0].datatype := DATATYPE_STRING;
    script.vars[0].identifier := 'result';

    for parameter in AParameters do
    begin
      writeln(parameter);
    end;

    script.nline := 0;
    for i := 0 to Length(AProcedure.lines)-1 do
    begin
      debugwriteln(AProcedure.name+' = '+script.cline);
      script.nline := script.nline + 1;
      script.cline := AProcedure.lines[i];
      evalres := Eval(script);
      if not evalres.success then
      begin
        ExecProc.success := False;
        ExecProc.return_value := Format(ERR_PROC_EVAL_FAIL_LINE, 
              [AProcedure.name, script.nline, sLineBreak, evalres.message]);
        break;
      end;
    end;

    ExecProc.success := True;
    ExecProc.return_value := ResolveVariable(script, 'result').value;
  end;

  procedure DoScriptExec(const APath: String);
  var
    script: TScript;
    evalres: TEvalResult;
  begin
    debugwriteln('Executing deash script '+APath);

    if not FileExists(APath) then
    begin
      script.scriptpath := APath;
      script.nline := -1;
      ThrowError(ERR_GENERAL_FILE_NOT_FOUND, script, [APath]);
      exit;
    end;

    script.incomment := False;
    script.scriptpath := APath;
    Assign(script.scriptfile, script.scriptpath);
    ReSet(script.scriptfile);

    SetLength(script.codeblocks, 1);
    script.codeblocks[0] := BLOCKTYPE_NONE;
    script.registering_proc := -1;

    script.nline := 0;
    while not eof(script.scriptfile) and (Length(script.codeblocks) > 0) do
    begin
      ReadLn(script.scriptfile, script.cline);
      debugwriteln('= '+script.cline);
      script.nline := script.nline + 1;
      evalres := Eval(script);
      if not evalres.success then
        break;
    end;
  end;

  function GetInvoke(const AName: String; var AScript: TScript; var ATargetRec: TInvoke): Boolean;
  var
    procrec: TProcedure;
    aliasrec: TAlias;
    proc_type: Integer;
    path: String;
  begin
    GetInvoke := True;

    if IsInternalCmd(AName) then
    begin
      ATargetRec.invoketype := INVOKETYPE_INTERNAL;
      ATargeTRec.location := AName;
      exit;
    end;

    proc_type := FindProcedure(ExtractProcName(AName), AScript, procrec);
    if (proc_type >= 0) then
    begin
      ATargetRec.invoketype := proc_type;
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

    GetInvoke := False;
  end;

  function DoInvoke(const AInvoke: TInvoke; var AScript: TScript): TInvokeResult;
  begin
    DoInvoke.code := 0;
    DoInvoke.message := '';

    case AInvoke.invoketype of
    INVOKETYPE_BINARY: DoInvoke := ExecBin(AInvoke.location, AInvoke.parameters);
    INVOKETYPE_INTERNAL: DoInvoke := DoInternalCmd(AInvoke.location, AInvoke.parameters, AScript);
    end;
  end;

  { Evaluates the cline of the given TScript record }
  function Eval(var AScript: TScript): TEvalResult;
  var
    tokens: TStringDynArray;
    invoke: TInvoke;
    inv_result: TInvokeResult;
    ifeval_res: TEvalResult;
    escaping, stash, evalled_if: Boolean;
    curr_blocktype, i, j: Integer;
  begin
    Eval.success := True;
    Eval.message := 'succ';

    if (Length(AScript.cline) = 0) then exit;

    AScript.cline := Trim(AScript.cline);
    if (Length(AScript.cline) = 0) or (AScript.cline[1] = '#') then exit;

    tokens := SplitString(Trim(AScript.cline), ' ');

    { Skip until out of comment }

    i := 0;
    escaping := False;
    evalled_if := False;
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
    if Length(tokens) = 0 then exit;
    if tokens[0] = '' then exit;
    if tokens[0] = 'end' then
    begin
      if (Length(AScript.codeblocks) = 1) and (GetShellEnv('SH_MODE') = 'INTERACTIVE') then
      begin
        deasherror(ERR_INTERACTIVE_END_NOBLOCKS);
        exit;
      end;
      if (AScript.registering_proc <> -1) then
        if (AScript.codeblocks[HIGH(AScript.codeblocks)] <> BLOCKTYPE_PROC) then
          writeln('TODO: ADD PROCEDURE LINE "', AScript.cline, '"')
        else
          AScript.registering_proc := -1;
      ArrPopInt(AScript.codeblocks);
      exit;
    end;

    if Length(AScript.codeblocks) = 0 then exit;

    { Only execute line if we are not in any declaration block }

    curr_blocktype := AScript.codeblocks[HIGH(AScript.codeblocks)];

    if ((curr_blocktype = BLOCKTYPE_IF) and AScript.falseif)
    or (curr_blocktype = BLOCKTYPE_PROC) then
    begin
      case tokens[0] of
      'env',
      'alias',
      'var',
      'proc',
      'for',
      'if',
      'loop': ArrPushInt(AScript.codeblocks, BLOCKTYPE_IGNORE);
      end;

      if (curr_blocktype = BLOCKTYPE_PROC) and (AScript.registering_proc <> -1) then
      begin
        writeln('TODO: ADD PROCEDURE LINE "', AScript.cline, '"');
      end;

      exit;
    end;
    
    if (curr_blocktype = BLOCKTYPE_IGNORE) and (AScript.registering_proc <> -1) then
    begin
      writeln('TODO: ADD PROCEDURE LINE "', AScript.cline, '"');
      exit;
    end;

    if  (curr_blocktype >= BLOCKTYPE_PROC)
    and (curr_blocktype <= BLOCKTYPE_VAR) then
    begin
      exit;
    end;

    stash := AScript.falseif;
    case tokens[0] of
      'if': begin 
        ArrPushInt(AScript.codeblocks, BLOCKTYPE_IF);
        AScript.falseif := not EvalIf(AScript, ifeval_res);
        Eval := ifeval_res;
        evalled_if := True;
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
        evalled_if := True;
      end;
      'else': begin
        if (curr_blocktype <> BLOCKTYPE_IF) then
        begin
          Eval.success := False;
          Eval.message := 'Invalid keyword "else" outside of an IF-Block';
          exit;
        end;
        AScript.falseif := not AScript.falseif;
        evalled_if := True;
      end;
    end;
    
    if not Eval.success then
    begin
      if evalled_if then ArrPopInt(AScript.codeblocks);
      AScript.falseif := stash;
      exit;
    end;

    if (evalled_if and Eval.success) 
    or (AScript.codeblocks[HIGH(AScript.codeblocks)] = BLOCKTYPE_IGNORE) then 
      exit;
    
    case tokens[0] of
    'env': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_ENV); exit; end;
    'alias': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_ALIAS); exit; end;
    'var': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_VAR); exit; end;
    'proc': begin 
      RegisterProc(AScript.cline, AScript);
      ArrPushInt(AScript.codeblocks, BLOCKTYPE_PROC); 
      exit; 
    end;
    'for': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_LOOP_FOR); exit; end;
    'loop': begin ArrPushInt(AScript.codeblocks, BLOCKTYPE_LOOP_LOOP); exit; end;
    'exit': begin SetLength(AScript.codeblocks, 0); AScript.exited := True; exit; end;
    '{': begin AScript.incomment := True; exit; end;
    else begin
      if not GetInvoke(tokens[0], AScript, invoke) then
      begin
        ThrowError(ERR_SCRIPT_UNKNOWN_IDENTIFIER, AScript, [tokens[0]]);
        Eval.success := False;
        exit;
      end;
      invoke.parameters := Copy(tokens, 1, Length(tokens) - 1);
      debugwriteln('INVOKE LOCATION: '+invoke.location+' ; INVOKE TYPE: '+IntToStr(invoke.invoketype));
      inv_result := DoInvoke(invoke, AScript);
      if inv_result.code <> 0 then
      begin
        DeashError('Invoke finished with code '+IntToStr(inv_result.code));
        DeashError('Message: '+inv_result.message);
      end;
    end; { end else } end; { end case }
  end;
  
  (* Internal function to resolve the operand of a if-condition to a Variable record *)
  function ResolveOperand(var AScript: TScript; const AOperand: String; var ADestination: TVariable): TEvalResult;
  var
    datatype: Integer;
    proc: TProcedure;
  begin
    ADestination.identifier := '';
    datatype := DetermineDatatype(AOperand);
    
    ResolveOperand.success := True;
    ResolveOperand.message := '';
    
    if datatype = DATATYPE_VARIABLE then
    begin
      ADestination := ResolveVariable(AScript, AOperand);
      if ADestination.datatype = DATATYPE_UNREAL then
      begin
        ResolveOperand.success := False;
        ResolveOperand.message := 'No such variable: '+AOperand;
      end;
      exit;
    end else if datatype = DATATYPE_RETURNVAL then
    begin
      ADestination.datatype := DATATYPE_STRING;
      if (FindProcedure(ExtractProcName(AOperand), AScript, proc) = -1) then
      begin
        ResolveOperand.success := False;
        ResolveOperand.message := 'No such procedure';
      end;
      ADestination.value := ExecProc(proc, ExtractProcParams(AOperand), AScript).return_value;
      exit;
    end else
    begin
      ADestination.datatype := datatype;
      ADestination.value := AOperand;
      if datatype = DATATYPE_STRING then
        ADestination.value := Copy(ADestination.value, 2, Length(ADestination.value)-2);
      
      exit;
    end;
  end;

  function EvalIf(var AScript: TScript; var AResult: TEvalResult): Boolean;
  var
    i, skip, lefthand_end: Integer;
    lefthandVal, righthandVal: TVariable;
    _operator, perform_bitwise: String;
    res_stash: Boolean;
    split: TStringDynArray;
  begin
    EvalIf := False;
    res_stash := False;
    AResult.success := True;

    _operator := '';
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

      { Resolve Operands, throw error if datatypes mismatch }
      if (i >= Length(split)) or (i + 2 >= Length(split))
      or (split[i+1] = 'and') or (split[i+1] = 'or') then
      begin
        { Do this to allow for checking if a boolean is true without a comparison }
        if (i < Length(split)) then 
        begin
          AResult := ResolveOperand(AScript, split[i], lefthandVal);
          if not AResult.success then exit;
        
          if lefthandVal.datatype <> DATATYPE_BOOLEAN then
          begin
            ThrowError(ERR_SCRIPT_IF_MALFORMED_VALUE, AScript, []);
            AResult.success := False;
            exit;
          end;

          _operator := '=';
          righthandVal.datatype := DATATYPE_BOOLEAN;
          righthandVal.value := 'true';
        end else 
        begin
          debugwriteln(sLineBreak + 'Conditional evaluation failed on word: '+split[i]);
          ThrowError(ERR_SCRIPT_IF_MALFORMED_VALOPVAL, AScript, []);
          AResult.success := False;
          exit;
        end;
      end else
      begin
        AResult := ResolveOperand(AScript, split[i], lefthandVal);
        if not AResult.success then exit;

        { TODO: Properly evaluate end of lefthand operand and parse strings
                with spaces, IMPORTANT }
        lefthand_end := i+1;
        _operator := split[lefthand_end];

        AResult := ResolveOperand(AScript, split[lefthand_end+1], righthandVal);
        if not AResult.success then exit;
        
        { Check if the two values can be compared }
        if lefthandVal.datatype <> righthandVal.datatype then
        begin
          ThrowError(ERR_SCRIPT_IF_MISMATCHED_TYPES, AScript, [DatatypeToStr(lefthandVal.datatype), DatatypeToStr(righthandVal.datatype)]);
          AResult.success := False;
          exit;
        end;
        skip := 2;
      end;

      { Stash the current result incase we do a bitwise }
      res_stash := EvalIf;
     
      debugwritef('Comparison: %s (%s) %s'+sLineBreak, [lefthandVal.value, _operator, righthandVal.value]);
      { Do an actual comparison of the values }
      case _operator of
        '=': EvalIf := righthandVal.value = lefthandVal.value;
        '<>': EvalIf := not (righthandVal.value = lefthandVal.value);
        '<', '>': begin
          if (righthandVal.datatype <> DATATYPE_INTEGER) or (lefthandVal.datatype <> DATATYPE_INTEGER) then
          begin
            ThrowError(ERR_SCRIPT_IF_INVALID_OPERATOR, AScript, []);
            AResult.success := False;
            exit;
          end;

          EvalIf := StrToInt(lefthandVal.value) >  StrToInt(righthandVal.value);
          if _operator = '<' then EvalIf := not EvalIf;
        end;
      end;

      debugwritef('Comparison Result: %s'+sLineBreak, [BoolToStr(EvalIf, True)]);
      case perform_bitwise of
      'and': begin EvalIf := EvalIf and res_stash; perform_bitwise := ''; end;
      'or': begin EvalIf := EvalIf or res_stash; perform_bitwise := ''; end;
      end;
      debugwritef('Comparison Result after bitwise: %s'+sLineBreak, [BoolToStr(EvalIf, True)]);
    end;

    debugwritef('Evaluation Result: %s'+sLineBreak, [BoolToStr(EvalIf, True)]);
  end;
end.
