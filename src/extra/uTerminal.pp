{$mode fpc}
unit uTerminal;

{ uTerminal.pp ; Helper functions for interactions with a Terminal }
{ Author: Marie Eckert                                             }

{$H+}

interface
  uses SysUtils, StrUtils, uASCII, uHelpers, Keyboard;

  type
    TCursorPos = array[0..1] of Integer;

  (* Sets the state of the keyboard driver, use this function instead of
     Init- and DoneKeyboard *)
  procedure SetKeyboardDriverState(const AState: Boolean);

  (* Gets the current cursor position and returns it as a Integer array
     (size: 2, 0 = Line, 1 = Column *)
  function GetCursorPos: TCursorPos;

  (* Move the cursor by AAmount in direction ADirection, see constants
     in this unit for directions. *)
  procedure MoveCursor(const AAmount: Integer; const ADirection: Integer);

  (* Move the Cursor to the provided position *)
  procedure MoveCursorTo(const ACursorPos: TCursorPos);

  function GetWidth: Integer;

  (* Clears the current line *)
  procedure ClearLine;

  const
    { CURSOR DIRECTIONS }
    CDIR_LEFT  = 0;
    CDIR_RIGHT = 1;
    CDIR_UP    = 2;
    CDIR_DOWN  = 3;

  var
    (* Is the keyboard driver active or not *)
    keyboard_state: Boolean;
implementation
  uses uErrors;

  procedure SetKeyboardDriverState(const AState: Boolean);
  begin
    if keyboard_state = AState then exit;
    if AState then
      InitKeyboard
    else
      DoneKeyboard;

    keyboard_state := AState;
  end;

  function GetCursorPos: TCursorPos;
  var
    r: LongWord;
    r_char: Char;
    report: array[0..1] of ShortString;
    report_ix: Integer;
  begin
    r_char := Char(0);
    report[0] := '';
    report[1] := '';
    report_ix := 0;
    write(#27'[6n');

    { Read the Response }
    repeat
      r := TranslateKeyEvent(GetKeyEvent);
      r_char := ASCIIGetKeyEventChar(r);

      if ((Byte(r_char) > $39) or (Byte(r_char) < $30)) and (r_char <> ';')
      then
        if r_char = 'R' then
          break;

      if r_char = ';' then
      begin
        report_ix := 1;
        continue;
      end;

      if Byte(r_char) = 27 then
        continue;

      report[report_ix] := report[report_ix] + r_char;
    until False;

    GetCursorPos[0] := StrToInt(report[0]);
    GetCursorPos[1] := StrToInt(report[1]);
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
    else
    begin
      ThrowNonScriptError(ERR_INTERNAL_INVALID_CURSOR_DIRECTION, []);
      dir_char := 'D';
    end; { end else } end; { end case }

    write(#27'[', IntToStr(AAmount), dir_char);
  end;
  
  procedure MoveCursorTo(const ACursorPos: TCursorPos);
  begin
    write(#27'[', IntToStr(ACursorPos[0]), ';', IntToStr(ACursorPos[1]), 'f');
  end;

  function GetWidth: Integer;
  begin
    GetWidth := 100;
  end;

  procedure ClearLine;
  var
    ws_string: String;
    i: Integer;
  begin
    ws_string := '';

    { Originally used fillchar here, but because I don't exactly understand it
      and I want to go to sleep I have done this ugly for-loop instead
      - Marie }
    for i := 0 to GetWidth - 1 do
      ws_string := ws_string + ' ';
    write(#13, ws_string);
  end;
initialization
  keyboard_state := False;
end.
