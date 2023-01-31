{$mode fpc}
unit uDEASHConsts;

{ uDEASHConsts.pp ; Constants for deash }
{ Author: Felix Eckert                  }

{$H+}

interface
  const
    VERSION = '0.0.0';
    VERSION_TYPE = 'alpha-dev';
    DEV_INFO = 'Felix Eckert';
    BLOCKTYPE_NONE   = 0;
    BLOCKTYPE_PROC   = 1;
    BLOCKTYPE_ALIAS  = 2;
    BLOCKTYPE_ENV    = 3;
    BLOCKTYPE_VAR    = 4;
    BLOCKTYPE_IF     = 5;
    BLOCKTYPE_IGNORE = 6;
    INVOKETYPE_PREF_PROC = 0;
    INVOKETYPE_ALIAS     = 1;
    INVOKETYPE_BINARY    = 2;
    INVOKETYPE_PROC      = 3;
    INVOKETYPE_INTERNAL  = 4;
implementation
end.
