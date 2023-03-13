{$mode objfpc}
unit uHelpers;

{ uHelpers.pp ; Helper functions for deash }
{ Author: Marie Eckert                     }

{$H+}{$R+}

interface
  uses SysUtils, Types, uDEASHConsts;

  { Print a shell error }
  procedure DeashError(const AMsg: String);
  
  { Check if a binary with the name set in AName exists. If the result is True,
    its full path will be written to APath. }
  function BinaryExists(const AName: String; var APath: String): Boolean;
  
  { Add an int to given array }
  procedure ArrPushInt(var AArr: TIntegerDynArray; const AVal: Integer);
  
  { Get and remove the last index of given array }
  function ArrPopInt(var AArr: TIntegerDynArray): Integer;
  
  { Determine the datatype of a script statement / identifier }
  function DetermineDatatype(const AString: String): Integer;
  
  { Convert a internal Datatype code to its string name }
  function DatatypeToStr(const ADatatype: Integer): String;
implementation
  procedure DeashError(const AMsg: String);
  begin
    writeln(StdErr, 'deash <ERROR>:: ', AMsg);
  end;

  function BinaryExists(const AName: String; var APath: String): Boolean;
  const
    {$IF defined(LINUX)}
    LOCATIONS : array of String = ('/bin', '/usr/bin', '/usr/local/bin');
    {$ELSEIF defined(WINDOWS)}
    LOCATIONS : array of String = ('"C:\Program Files\"', '"C:\Program Files (x86)\"');
    {$ENDIF}
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

  function DetermineDatatype(const AString: String): Integer;
  begin
    DetermineDatatype := DATATYPE_VARIABLE;

    if (AString = BOOLEAN_STR_TRUE) or (AString = BOOLEAN_STR_FALSE) then
      DetermineDatatype := DATATYPE_BOOLEAN;

    if (Byte(AString[1]) >= Byte('0')) and (Byte(AString[1]) <= Byte('9')) then
      DetermineDatatype := DATATYPE_INTEGER;

    if AString[1] = '''' then
      DetermineDatatype := DATATYPE_STRING;
  end;

  function DatatypeToStr(const ADatatype: Integer): String;
  begin
    DatatypeToStr := 'unknown';

    case ADatatype of
      DATATYPE_VARIABLE: DatatypeToStr := 'Variable';
      DATATYPE_INTEGER: DatatypeToStr := 'Integer';
      DATATYPE_BOOLEAN: DatatypeToStr := 'Boolean';
      DATATYPE_STRING: DatatypeToStr := 'String';
    end;
  end;

initialization
  Randomize;
end.
