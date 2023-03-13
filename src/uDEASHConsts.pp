{$mode fpc}
unit uDEASHConsts;

{ uDEASHConsts.pp ; Constants for deash }
{ Author: Marie Eckert                  }

{$H+}

interface
  const
    { Shell information }
    VERSION = '0.0.0';
    VERSION_TYPE = 'alpha-dev';
    DEV_INFO = 'Marie Eckert';

    { Interal codes for different types of Code-Blocks }
    BLOCKTYPE_NONE   = 0;
    BLOCKTYPE_PROC   = 1;
    BLOCKTYPE_ALIAS  = 2;
    BLOCKTYPE_ENV    = 3;
    BLOCKTYPE_VAR    = 4;
    BLOCKTYPE_IF     = 5;
    BLOCKTYPE_IGNORE = 6;

    { Internal codes for Invoke types}
    INVOKETYPE_PREF_PROC = 0;
    INVOKETYPE_ALIAS     = 1;
    INVOKETYPE_BINARY    = 2;
    INVOKETYPE_PROC      = 3;
    INVOKETYPE_INTERNAL  = 4;

    { Internal codes for Datatypes }
    DATATYPE_VARIABLE = 0;
    DATATYPE_INTEGER  = 1;
    DATATYPE_STRING   = 2;
    DATATYPE_BOOLEAN  = 3;

    { String representations for Boolean values }
    BOOLEAN_STR_TRUE  = 'true';
    BOOLEAN_STR_FALSE = 'false';
implementation
end.
