{$mode fpc}
program error_manual;

{ error_manual.pp ; Util for creating an error manual page for deash }
{ Author: Marie Eckert                                               }
{                                                                    }
{ This program will take input from either text files or stdin and   }
{ store it in a file as a serialized TDeashError record              }
{                                                                    }
{ Compile with: fpc error_manual -Fu"../src" -FE"out/"               }
{                                                                    }
{ Usage:                                                             }
{ error_manual <out file>                                            }

uses SysUtils, uTypes;

procedure Help;
begin
  writeln('This program will take input from either text files or stdin and ');
  writeln('store it in a file as a serialized TDeashError record');
  writeln;
  writeln('Usage: error_manual <out file>');
end;

var
  i: Integer;
  err: TDeashError;
  fl: file of TDeashError;
begin
  if ParamCount() = 0 then
  begin
    writeln('error_manual requires one argument: Output file!');
    halt;
  end;

  if ParamStr(1) = '--help' then
  begin
    Help;
    halt;
  end;

  write('Error ID: ');
  err.code := StrToInt(readln());

  writeln('Message (Press ESC to finish)');
  err.message := GetText;

  Assign(fl, ParamStr(1));
  ReWrite(fl);
  Write(fl, err);
  Close(fl);
end.
