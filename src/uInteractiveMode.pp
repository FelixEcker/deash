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

  type
    TCursorPos = array[0..1] of Integer;

  procedure LaunchShell;

  const
    { KEY CODES NOT IN Keyboard UNIT }
    KEY_ANSI_ESCAPE = $3081A00;

    { INPUT ACTIONS }
    IA_ENTER  = 0;
    IA_DELETE = 1;
    IA_CLEFT  = 2;
    IA_CRIGHT = 3;

  var
    (* Column where the cursor originated from on the current line *)
    cursor_origin    : Integer;

    (* Current column of the cursor, relative to cursor_origin *)
    cursor_pos       : Integer;
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

  function GetCursorPos: TCursorPos;
  var
    r: LongWord;
    r_char: Char;
    hitescape: Boolean;
    report: ShortString;
  begin
    r_char := Char(0);
    hitescape := False;
    report := '';
    write(#27'[6n');

    { Read the Response }
    r := TranslateKeyEvent(GetKeyEvent);
    repeat
      if r = KEY_ANSI_ESCAPE then
        hitescape := True
      else if hitescape then
      begin
        r_char := GetKeyEventChar(r);
        if r_char = 'R' then break;

        report := report + r_char;
      end;

      r := TranslateKeyEvent(GetKeyEvent);
    until False;

    report := Copy(report, 2, Length(report));
    GetCursorPos[0] := StrToInt(Copy(report, 2, pos(';', report)-2));
    GetCursorPos[1] := StrToInt(Copy(report, pos(';', report)+1, Length(report)));
  end;

  procedure HandleKeypress(AKey: Longword; var AInputBuff: String;
                            var AAction: Integer);
  var
    as_char: Char;
  begin
    as_char := GetKeyEventChar(AKey);
    if as_char = #0 then
    begin
      case AKey of
      kbdLeft: AAction := IA_CLEFT;
      kbdRight: AAction := IA_CRIGHT;
      end;
      exit;
    end;

    case Integer(as_char) of
    13: AAction := IA_ENTER;
    16: AAction := IA_DELETE;
    else
    begin
      write(as_char);
      AInputBuff := AInputBuff + as_char;
    end; end;
  end;

  procedure HandleInputAction(const AAction: Integer; var AInputBuff: String);
  begin
    { TODO: Handle IA_DELETE, IA_CLEFT and IA_CRIGHT }
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
  begin
    should_quit := False;

    debugwriteln('Launching shell');
  {$IF defined(UNIX)}
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

      { Init cursor data for new line}
      cursor_origin := GetCursorPos[1];
      cursor_pos := 0;
      repeat
        action := -1;
        if eof() then
        begin
          should_quit := True;
          break;
        end;

        HandleKeypress(TranslateKeyEvent(GetKeyEvent), inbuff, action);
        if action <> -1 then
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
