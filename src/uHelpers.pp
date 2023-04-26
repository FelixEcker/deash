{$mode objfpc}
unit uHelpers;

{ uHelpers.pp ; Helper functions for deash     }
{ Author: Marie Eckert                         }

{$R+}{$H+}

interface
  uses Dos, StrUtils, SysUtils, Types, uDEASHConsts,
       Resource, stringtableresource, 
       {$IF defined(UNIX)} ElfReader, {$ELSE} Windows, {$ENDIF}
       Classes;

  (* Print a shell error *)
  procedure DeashError(const AMsg: String);

  (* Check if a binary with the name set in AName exists. If the result is True,
    its full path will be written to APath. *)
  function BinaryExists(AName: String; var APath: String): Boolean;
  
  (* Add an int to given array *)
  procedure ArrPushInt(var AArr: TIntegerDynArray; const AVal: Integer);
  
  (* Get and remove the last index of given array *)
  function ArrPopInt(var AArr: TIntegerDynArray): Integer;
  
  (* Determine the datatype of a script statement / identifier *)
  function DetermineDatatype(const AString: String): Integer;
  
  (* Convert a internal Datatype code to its string name *)
  function DatatypeToStr(const ADatatype: Integer): String;
  
  (* Convert the string name of a Datatype to its internal code *)
  function StrToDatatype(const AStr: String): Integer;

  (* Convert an internal Blocktype code to its string name *)
  function BlocktypeToStr(const ABlocktype: Integer): String;

  (* Get the duration of the current session as a unix timestamp *)
  function ProgramUptime: TDateTime;

  (* Get a Resource String from the binary *)
  function GetResourceString(const AId: Integer): String;

  var
    program_start: TDateTime;
implementation
  procedure DeashError(const AMsg: String);
  begin
    writeln(StdErr, 'deash <ERROR>:: ', AMsg);
  end;

  function BinaryExists(AName: String; var APath: String): Boolean;
  var
    location: String;
  begin
    BinaryExists := False;

    { Check if binary is a path before checking dirs on PATH }
    if pos(PathDelim, AName) <> 0 then
    begin
      if FileExists(AName) then
      begin
        APath := AName;
        BinaryExists := True;
        exit;
      end;

      BinaryExists := False;
      exit;
    end;

    { Check dirs on PATH }

  {$IF defined(UNIX)}
    for location in SplitString(GetEnv('PATH'), ':') do
  {$ELSEIF defined(WINDOWS)}
    if pos('.exe', AName) <> Length(AName)-3 then
      AName := AName+'.exe';

    for location in SplitString(GetEnv('PATH'), ';') do
  {$ENDIF}
    begin
      if FileExists(location+PathDelim+AName) then
      begin
        APath := location+PathDelim+AName;
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
    ArrPopInt := 0;
    if Length(AArr) = 0 then exit;
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
      DetermineDatatype := DATATYPE_STRING
    else if pos('(', AString) > 0 then
      DetermineDatatype := DATATYPE_RETURNVAL;
  end;

  function DatatypeToStr(const ADatatype: Integer): String;
  begin
    DatatypeToStr := 'unknown';

    case ADatatype of
    DATATYPE_UNREAL:    DatatypeToStr := 'Unreal (unknown)';
    DATATYPE_VARIABLE:  DatatypeToStr := 'Variable';
    DATATYPE_INTEGER:   DatatypeToStr := 'Integer';
    DATATYPE_BOOLEAN:   DatatypeToStr := 'Boolean';
    DATATYPE_STRING:    DatatypeToStr := 'String';
    DATATYPE_RETURNVAL: DatatypeToStr := 'Procedure Return Value (String)';
    end;
  end;
  
  function StrToDatatype(const AStr: String): Integer;
  begin
    StrToDatatype := DATATYPE_UNREAL;
    
    case AStr of
    'Integer': StrToDatatype := DATATYPE_INTEGER;
    'Boolean': StrToDatatype := DATATYPE_BOOLEAN;
    'String':  StrToDatatype := DATATYPE_STRING;
    end;
  end;
  
  function BlocktypeToStr(const ABlocktype: Integer): String;
  begin
    BlocktypeToStr := 'unknown';

    case ABlocktype of
    BLOCKTYPE_NONE:      BlocktypeToStr := 'None';
    BLOCKTYPE_PROC:      BlocktypeToStr := 'Procedure';
    BLOCKTYPE_ALIAS:     BlocktypeToStr := 'Alias';
    BLOCKTYPE_ENV:       BlocktypeToStr := 'Environment';
    BLOCKTYPE_VAR:       BlocktypeToStr := 'Variable';
    BLOCKTYPE_IF:        BlocktypeToStr := 'If';
    BLOCKTYPE_IGNORE:    BlocktypeToStr := 'Ignore';
    BLOCKTYPE_LOOP_FOR:  BlocktypeToStr := 'For-Loop';
    BLOCKTYPE_LOOP_LOOP: BlocktypeToStr := 'Loop-Loop';
    end;
  end;

  function ProgramUptime: TDateTime;
  begin
    ProgramUptime := Time - program_start;
  end;

  procedure SetClearString(var AStr: String; const ASize: Integer);
  var
    i: Integer;
  begin
    AStr := '';
    for i := 1 to ASize do
      AStr := AStr + ' ';
  end;

  function GetResourceString(const AId: Integer): String;
{$IF defined(UNIX)}
  var
    res: TResources;
    stres: TStringTableResource;
{$ENDIF}
  begin
    result := '';

  {$IF defined(UNIX)}
    res := TResources.Create;
    try
      try
        res.loadfromfile(ParamStr(0));
        stres := res.find(RT_STRING, succ(AId shr 4)) as TStringTableResource;
        result := stres.strings[AId];
      except
        on e: EResourceReaderWrongFormatException do
        begin
          deasherror('An error occured when trying to load a resource: Wrong Format Exception');
          deasherror('Message: '+e.message);
          exit;
        end;
      end;
    finally
      res.Free
    end;
  {$ELSE}
    SetClearString(result, 8192);
    LoadString(FindResource(0, nil, RT_STRING), AId, @result[1], 8192);
    result := Trim(result);
  {$ENDIF}
  end;
end.
