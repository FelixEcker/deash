{$mode fpc}
unit uInteractiveMode;

{ uInteractiveMode.pp ; Interactive Shell for DEASH }
{ author: Marie Eckert                              }

{$H+}

interface
  uses Dos, {$IF defined(LINUX)} BaseUnix, {$ENDIF} SysUtils, StrUtils, Types, uDEASHConsts, uHelpers, uXDebug, uScriptEngine, uPathResolve;

  procedure LaunchShell;

  var
    history          : TextFile;
    {$IF defined(LINUX)}
      sig_int_handler  : PSigActionRec;
      sig_quit_handler : PSigActionRec;
    {$ENDIF}
    should_quit      : Boolean;
implementation
  {$IF defined(LINUX)}
  procedure HandleSigInterrupt(sig: cint); cdecl;
  begin
  end;

  procedure HandleSigQuit(sig: cint); cdecl;
  begin
    should_quit := True;
  end;

  procedure InstallSignals;
  begin
    debugwriteln('Installing SIGINT handler...');
    new(sig_int_handler);
    sig_int_handler^.sa_Handler := SigActionHandler(@HandleSigInterrupt);
    fillchar(sig_int_handler^.Sa_Mask, sizeof(sig_int_handler^.sa_mask),#0);
    sig_int_handler^.Sa_Flags := 0;
    
    {$IF defined(LINUX)}
      sig_int_handler^.Sa_Restorer:=Nil;
    {$ENDIF}
    
    if fpSigAction(SIGINT, sig_int_handler, nil) <> 0 then
    begin
      deasherror('Error while installing SIGINT handler: ' + IntToStr(fpgeterrno) + '.');
      halt(1);
    end;

    debugwriteln('Installing SIGQUIT handler...');
    new(sig_quit_handler);
    sig_quit_handler^.sa_Handler := SigActionHandler(@HandleSigQuit);
    fillchar(sig_quit_handler^.Sa_Mask, sizeof(sig_quit_handler^.sa_mask), #0);
    sig_quit_handler^.Sa_Flags := 0;
    
    {$IF defined(LINUX)}
      sig_quit_handler^.Sa_Restorer:=Nil;
    {$ENDIF}
    
    if fpSigAction(SIGQUIT, sig_quit_handler, nil) <> 0 then
    begin
      deasherror('Error while installing SIGQUIT handler: ' + IntToStr(fpgeterrno) + '.');
      halt(1);
    end;
  end;
  {$ENDIF}

  procedure LaunchShell;
  var
    eval_result: TEvalResult;
    script: TScript;
    inbuff: String;
    rchar: Char;
  begin
    should_quit := False;

    debugwriteln('Launching shell');
    {$IF defined(LINUX)}
      InstallSignals;
    {$ENDIF}
    DoScriptExec(ResolveEnvsInPath('$HOME/.deashrc'));

    script.scriptpath := ResolveEnvsInPath('$HOME/.deash_history');
    script.exited := False;
    script.nline := 1;
    script.incomment := False;
    SetLength(script.codeblocks, 1);
    script.codeblocks[0] := BLOCKTYPE_NONE;

    Assign(history, script.scriptpath);
    if not FileExists(script.scriptpath) then
      ReWrite(history)
    else
      Append(history);

    while not script.exited and not should_quit do
    begin
      write('deash ', GetCurrentDir(), '> ');
      
      inbuff := '';
      repeat
        if eof() then
        begin
          should_quit := True;
          break;
        end;

        read(rchar);
        inbuff := inbuff + rchar;
      until (rchar = #10) or should_quit;
      if should_quit then break;

      script.cline := inbuff;
      write(history, script.cline);

      eval_result := Eval(script);
      if not eval_result.success then
        DeashError(eval_result.message);

      script.nline := script.nline + 1;
    end;
    
    Close(history);
  end;
end.
