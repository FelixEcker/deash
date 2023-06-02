{$mode fpc}
unit uTerminal;

{ uTerminal.pp ; Helper functions for interactions with a Terminal }
{ Author: Marie Eckert                                             }

{$H+}

interface
  uses SysUtils, StrUtils, uHelpers, Keyboard;

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
  procedure SetKeyboardDriverState(const AState: Boolean);
  begin
    if keyboard_state = AState then exit;
    if keyboard_state then
      InitKeyboard
    else
      DoneKeyboard;

    keyboard_state := AState;
  end;

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
initialization
  keyboard_state := False;
end.
