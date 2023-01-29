{$mode fpc}
unit uDEASHConsts;

{ uDEASHConsts.pas ; Constants for deash }
{ Author: Felix Eckert                   }

{$H+}

interface
  const
    BLOCKTYPE_NONE  = 0;
    BLOCKTYPE_PROC  = 1;
    BLOCKTYPE_ALIAS = 2;
    BLOCKTYPE_ENV   = 3;
    BLOCKTYPE_VAR   = 4;
    BLOCKTYPE_IF    = 5;
    INVOKETYPE_PREF_PROC = 0;
    INVOKETYPE_ALIAS     = 1;
    INVOKETYPE_BINARY    = 2;
    INVOKETYPE_PROC      = 3;
    INVOKETYPE_INTERNAL  = 4;
implementation
end.
