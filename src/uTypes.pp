{$mode fpc}
unit uTypes;

{ uTypes.pp ; Shared types for deash }
{ Author: Marie Eckert               }

interface
  uses Types;

  type
    TVariable = record
      identifier, value: String;
      datatype: Integer;
    end;

    TVariableDynArray = array of TVariable;

    TScript = record
      scriptfile : TextFile;
      scriptpath : String;
      cline      : String;
      nline      : Integer;
      incomment  : Boolean;
      falseif    : Boolean;
      exited     : Boolean;
      codeblocks : TIntegerDynArray;
      vars       : TVariableDynArray;
    end;

implementation
end.
