{$mode fpc}
unit uDEASHConsts;

{ uDEASHConsts.pp ; Constants for deash }
{ Author: Marie Eckert                  }

{$H+}

interface
  const
    { Interal codes for different types of Code-Blocks }
    BLOCKTYPE_NONE      = 0;
    BLOCKTYPE_PROC      = 1;
    BLOCKTYPE_ALIAS     = 2;
    BLOCKTYPE_ENV       = 3;
    BLOCKTYPE_VAR       = 4;
    BLOCKTYPE_IF        = 5;
    BLOCKTYPE_IGNORE    = 6;
    BLOCKTYPE_LOOP_FOR  = 7;
    BLOCKTYPE_LOOP_LOOP = 8;

    { Internal codes for Invoke types}
    INVOKETYPE_PREF_PROC = 0;
    INVOKETYPE_EXP_PROC  = 1;
    INVOKETYPE_ALIAS     = 2;
    INVOKETYPE_BINARY    = 3;
    INVOKETYPE_PROC      = 4;
    INVOKETYPE_INTERNAL  = 5;

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
    ERR_EVAL_IF_MALOFRMED_VALUE    = '';
    ERR_EVAL_IF_MALFORMED_VALOPVAL = '';
    ERR_EVAL_IF_MISMATCHED_TYPES   = '';
    ERR_EVAL_IF_GREATER_NINT       = 'greater/lesser comparison only applicable for integers (got %s and %s)!';
    ERR_PROC_EVAL_FAIL_LINE        = 'eval for procedure %s failed at line %d:%s:: %s';
    ERR_SCRIPT_EVAL_FAIL_LINE      = 'eval for script %s failed at line %d:%s:: %s';
    ERR_INTERACTIVE_END_NOBLOCKS   = 'Can not leave last codeblock while shell is in interactive mode!';

implementation
end.
