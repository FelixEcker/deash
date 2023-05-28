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

    (* record type to store the current state of the input prompt *)
    TPrompt = record
      cursor_org : TCursorPos;
      cursor_pos : TCursorPos;
      action     : Integer;
      inbuff     : String;
    end;

  procedure LaunchShell;

  const
    { INPUT ACTIONS }
    IA_ENTER  = 0;
    IA_DELETE = 1;
    IA_CLEFT  = 2;
    IA_CRIGHT = 3;

    { CURSOR DIRECTIONS }
    CDIR_LEFT  = 0;
    CDIR_RIGHT = 1;
    CDIR_UP    = 2;
    CDIR_DOWN  = 3;

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
    fillchar(
      sig_quit_handler^.Sa_Mask,
      sizeof(sig_quit_handler^.sa_mask),
      #0
    );
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
    report: ShortString;
  begin
    r_char := Char(0);
    report := '';
    write(#27'[6n');

    { Read the Response }
    repeat
      r := TranslateKeyEvent(GetKeyEvent);
      r_char := ASCIIGetKeyEventChar(r);

      if ((Byte(r_char) > $39) or (Byte(r_char) < $30)) and (r_char <> ';')
      then
        if r_char = 'R' then
          break
        else
          continue;

      report := report + r_char;
    until False;

    GetCursorPos[0] := StrToInt(Copy(report, 1, pos(';', report)-2));
    GetCursorPos[1] := StrToInt(
                        Copy(report, pos(';', report)+1, Length(report))
                       );
  end;

  procedure MoveCursor(const AAmount: Integer; const ADirection: Integer);
  var
    dir_char: Char;
  begin
    case ADirection of
    CDIR_LEFT:  dir_char := 'D';
    CDIR_RIGHT: dir_char := 'C';
    CDIR_UP:    dir_char := 'A';
    CDIR_DOWN:  dir_char := 'B';
    end;

    write(#27'[', IntToStr(AAmount), dir_char);
  end;

  procedure HandleKeypress(const AKey: Longword; var APrompt: TPrompt);
  var
    as_char: Char;
  begin
    as_char := GetKeyEventChar(AKey);
    if as_char = #0 then
    begin
      case AKey of
      KEY_LEFT:  APrompt.action := IA_CLEFT;
      KEY_RIGHT: APrompt.action := IA_CRIGHT;
      end;
      exit;
    end;

    case Integer(as_char) of
    13: APrompt.action := IA_ENTER;
    16: APrompt.action := IA_DELETE;
    else
    begin
      write(as_char);
      APrompt.inbuff := APrompt.inbuff + as_char;
    end; end;
  end;

  procedure HandleInputAction(var APrompt: TPrompt);
  begin
    { TODO: Handle IA_DELETE, IA_CLEFT and IA_CRIGHT }
    case APrompt.action of
    IA_ENTER: write(#13#10);
    IA_DELETE: exit;
    IA_CLEFT: begin
      if APrompt.cursor_pos[1] = 0 then exit;
      APrompt.cursor_pos[1] := APrompt.cursor_pos[1]-1;
      MoveCursor(1, CDIR_LEFT);
    end;
    IA_CRIGHT: begin
      if APrompt.cursor_pos[1] = Length(APrompt.inbuff) then exit;
      APrompt.cursor_pos[1] := APrompt.cursor_pos[1]+1;
      MoveCursor(1, CDIR_RIGHT);
    end; { end IA_CRIGHT }
    end; { end case }
  end;

  procedure InitPrompt(var APrompt: TPrompt);
  begin
    APrompt.cursor_pos := GetCursorPos;
    APrompt.cursor_org := GetCursorPos;
    APrompt.action := -1;
    APrompt.inbuff := '';
  end;

  procedure LaunchShell;
  var
    prompt: TPrompt;
    eval_result: TEvalResult;
    script: TScript;
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

      InitKeyboard;

      { Init prompt record for new line }
      InitPrompt(prompt);

      repeat
        prompt.cursor_pos[1] := GetCursorPos[1] - prompt.cursor_org[1];
        if eof() then
        begin
          should_quit := True;
          break;
        end;

        HandleKeypress(TranslateKeyEvent(GetKeyEvent), prompt);
        if prompt.action <> -1 then
          HandleInputAction(prompt);
      until (prompt.action = IA_ENTER) or should_quit;
      DoneKeyboard;

      if should_quit then break;

      script.cline := prompt.inbuff;
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
