{$mode objfpc}
unit uHelpers;

{ uHelpers.pas ; Helper functions for deash }
{ Author: Felix Eckert                      }

{$H+}{$R+}

interface
  uses SysUtils, Types;

  procedure DeashError(const AMsg: String);
  function BinaryExists(const AName: String; var APath: String): Boolean;
  procedure ArrPushInt(var AArr: TIntegerDynArray; const AVal: Integer);
  function ArrPopInt(var AArr: TIntegerDynArray): Integer;
implementation
  procedure DeashError(const AMsg: String);
  begin
    writeln('deash <ERROR>:: ', AMsg);
  end;

  function BinaryExists(const AName: String; var APath: String): Boolean;
  const
    LOCATIONS : array of String = ('/bin', '/usr/bin', '/usr/local/bin');
  var
    location: String;
  begin
    BinaryExists := False;

    for location in LOCATIONS do
    begin
      if FileExists(location+'/'+AName) then
      begin
        APath := location+'/'+AName;
        BinaryExists := True;
        exit;
      end;
    end;
  end;

  procedure ArrPushInt(var AArr: TIntegerDynArray; const AVal: Integer);
  begin
    SetLength(AArr, Length(AArr)+1);
    AArr[HIGH(AArr)] := AVal;
  end;

  function ArrPopInt(var AArr: TIntegerDynArray): Integer;
  begin
    ArrPopInt := AArr[Length(AArr)-1];
    SetLength(AArr, Length(AArr)-1);
  end;
end.
