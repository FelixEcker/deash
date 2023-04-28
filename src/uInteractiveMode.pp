{$mode fpc}
unit uInteractiveMode;

{ uInteractiveMode.pp ; Interactive Shell for DEASH }
{ author: Marie Eckert                              }

{$H+}

interface
  uses Keyboard, Dos,
{$IF defined(UNIX)}
       BaseUnix,
{$ENDIF}
       SysUtils, StrUtils, Types, uDEASHConsts, uHelpers, uInternalProcs,
       uXDebug, uScriptEngine, uPathResolve, uTypes;

  procedure LaunchShell;

  const
    { INPUT ACTIONS }
    IA_ENTER  = 0;
    IA_CLEFT  = 1;
    IA_CRIGHT = 2;

  var
    history          : TextFile;
  {$IF defined(UNIX)}
    sig_int_handler  : PSigActionRec;
    sig_quit_handler : PSigActionRec;
  {$ENDIF}
    should_quit      : Boolean;
implementation
{$IF defined(UNIX)}
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
      deasherror('Error while installing SIGINT handler: '
                    + IntToStr(fpgeterrno) + '.');
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
      deasherror('Error while installing SIGQUIT handler: '
                    + IntToStr(fpgeterrno) + '.');
      halt(1);
    end;
  end;
{$ENDIF}

  procedure HandleKeypress(AKey: Char; var AInputBuff: String;
                            var AAction: Integer);
  begin
    if AKey = #0 then AKey := GetKeyEventChar(TranslateKeyEvent(GetKeyEvent));

    case Integer(AKey) of
    13: AAction := IA_ENTER;
    else
    begin
      write(AKey);
      AInputBuff := AInputBuff + AKey;
    end; end;
  end;

  procedure HandleInputAction(const AAction: Integer; var AInputBuff: String);
  begin
    case AAction of
    IA_ENTER: write(#13#10);
    end;
  end;

  procedure LaunchShell;
  var
    eval_result: TEvalResult;
    script: TScript;
    inbuff: String;
    action: Integer;
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
    script.registering_proc := -1;

    Assign(history, script.scriptpath);
    if not FileExists(script.scriptpath) then
      ReWrite(history)
    else
      Append(history);

    while not script.exited and not should_quit do
    begin
      write('deash ', GetCurrentDir(), '> ');

      inbuff := '';
      InitKeyboard;
      repeat
        action := -1;
        if eof() then
        begin
          should_quit := True;
          break;
        end;

        rchar := GetKeyEventChar(TranslateKeyEvent(GetKeyEvent));
        HandleKeypress(rchar, inbuff, action);
        HandleInputAction(action, inbuff);
      until (action = IA_ENTER) or should_quit;
      DoneKeyboard;

      if should_quit then break;

      script.cline := inbuff;
      write(history, script.cline);

      if script.cline = 'debug_cbtrace' then
        DoInternalCmd(script.cline, [], script)
      else
        eval_result := Eval(script);

      script.nline := script.nline + 1;
    end;

    Close(history);
  end;
end.
