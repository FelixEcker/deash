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

    TParameter = record
      name    : String;
      ptype   : String;
      default : String;
    end;

    TParameterDynArray = array of TParameter;

    TProcedure = record
      name       : String;
      internal   : Boolean;
      parameters : TParameterDynArray;
      lines      : String;
    end;

    TProcedureDynArray = array of TProcedure;

    TProcedureResult = record
      success      : Boolean;
      return_value : String;
    end;
 
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
      procedures : TProcedureDynArray;
    end;

    TDeashError = record
      code        : Integer;
      message     : String;
      description : String;
      fixes       : String;
    end;

implementation
end.
