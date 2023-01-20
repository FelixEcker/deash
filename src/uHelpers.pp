{$mode objfpc}
unit uHelpers;

{ uHelpers.pas ; Helper functions for deash }
{ Author: Felix Eckert                      }

{$H+}

interface
  procedure DeashError(const AMsg: String);
  function BinaryExists(const AName: String; var APath: String): Boolean;
  procedure ArrPush(var AArr: array of const; const AVal);
  function ArrPopInt(var AArr: array of const): Integer;
implementation
  procedure DeashError(const AMsg: String);
  begin
    writeln('deash <ERROR>:: ', AMsg);
  end;

  function BinaryExists(const AName: String; var APath: String): Boolean;
  begin
    BinaryExists := False;
  end;

  procedure ArrPush(var AArr: array of const; const AVal);
  begin
    SetLength(AArr, Length(AArr)+1);
    AArr[HIGH(AArr)] := AVal;
  end;

  function ArrPopInt(var AArr: array of const): Integer;
  begin
    ArrPopInt := AArr[HIGH(AArr)];
    SetLength(AArr, HIGH(AArr));
  end;
end.
