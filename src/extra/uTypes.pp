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
      ptype   : Integer;
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
      (* The TextFile where the script is read from *)
      scriptfile       : TextFile;

      (* Path to the script file *)
      scriptpath       : String;

      (* Contents of the current line of the script *)
      cline            : String;

      (* Current Line Number *)
      nline            : Integer;

      (* Sets if the script is currently in a block comment *)
      incomment        : Boolean;

      (* If this is true, all lines will be ignored until the script leaves
         the current if-block *)
      falseif          : Boolean;

      (* Has the script stopped? *)
      exited           : Boolean;

      (* The codeblocks in which the script is currently in *)
      codeblocks       : TIntegerDynArray;

      (* The scripts local variables *)
      vars             : TVariableDynArray;

      (* The scripts local procedures *)
      procedures       : TProcedureDynArray;

      (* The index of the procedure the script is currently registering,
         -1 if no procedure is registering *)
      registering_proc : Integer;

      (* The type of procedure which is currently being registered *)
      registering_proc_type : Integer;
    end;

    TDeashError = record
      code        : Integer;
      message     : String;
      description : String;
      fixes       : String;
    end;

implementation
end.
