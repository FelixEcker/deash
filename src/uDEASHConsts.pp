{$mode fpc}
unit uDEASHConsts;

{ uDEASHConsts.pp ; Constants for deash }
{ Author: Marie Eckert                  }

{$H+}

interface
  const
    { Shell information }
    VERSION = '0.1.0';
    VERSION_TYPE = 'alpha-dev';
    DEV_INFO = 'Marie Eckert';

    { Interal codes for different types of Code-Blocks }
    BLOCKTYPE_NONE       = 0;
    BLOCKTYPE_PROC       = 1;
    BLOCKTYPE_ALIAS      = 2;
    BLOCKTYPE_ENV        = 3;
    BLOCKTYPE_VAR        = 4;
    BLOCKTYPE_IF         = 5;
    BLOCKTYPE_IGNORE     = 6;
    BLOCKTYPE_LOOP_FOR   = 7;
    BLOCKTYPE_LOOP_WHILE = 8;

    { Internal codes for Invoke types}
    INVOKETYPE_PREF_PROC = 0;
    INVOKETYPE_ALIAS     = 1;
    INVOKETYPE_BINARY    = 2;
    INVOKETYPE_PROC      = 3;
    INVOKETYPE_INTERNAL  = 4;

    { Internal codes for Datatypes }
    DATATYPE_UNREAL    = -1;
    DATATYPE_VARIABLE  = 0;
    DATATYPE_INTEGER   = 1;
    DATATYPE_STRING    = 2;
    DATATYPE_BOOLEAN   = 3;
    DATATYPE_RETURNVAL = 4;

    { String representations for Boolean values }
    BOOLEAN_STR_TRUE  = 'true';
    BOOLEAN_STR_FALSE = 'false';

    { Error Messages }
    ERR_EVAL_IF_MALOFRMED_VALUE    = 'malformed conditional: expected VALUE OPERATOR VALUE, got VALUE. This syntax is only allowed if the Variable/Value is of type Boolean!';
    ERR_EVAL_IF_MALFORMED_VALOPVAL = 'malformed conditional: expected VALUE OPERATOR VALUE';
    ERR_EVAL_IF_MISMATCHED_TYPES   = 'mismatched datatypes for comparison (%s and %s)';
    ERR_EVAL_IF_GREATER_NINT       = 'greater/lesser comparison only applicable for integers (got %s and %s)!';

implementation
end.
