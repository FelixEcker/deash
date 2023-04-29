{$mode objfpc}
unit uErrors;

{ uErrors.pp ; Error System for deash }
{ Author: Marie Eckert                }

{$R res/error_message.rc}
{$H+}

interface
  uses SysUtils, uTypes, uXDebug, uHelpers;

  const
    { Error Types }
    ERR_TYPE_GENERAL  = 0;
    ERR_TYPE_INTERNAL = 1000;
    ERR_TYPE_SCRIPT   = 2000;

    { Error Codes }
    ERR_GENERAL_FILE_NOT_FOUND   = 0001;
    ERR_GENERAL_NO_CONFIG        = 0002;
    ERR_GENERAL_MALFORMED_CONFIG = 0003;

    ERR_INTERNAL_UNKNOWN = 1001;

    ERR_SCRIPT_UNKNOWN_IDENTIFIER        = 2001;
    ERR_SCRIPT_IF_MALFORMED_VALUE        = 2003;
    ERR_SCRIPT_IF_MALFORMED_VALOPVAL     = 2003;
    ERR_SCRIPT_IF_MISMATCHED_TYPES       = 2004;
    ERR_SCRIPT_IF_INVALID_OPERATOR       = 2005;
    ERR_SCRIPT_INVALID_DATATYPE          = 2006;
    ERR_SCRIPT_PREFFERED_UNEXPORTED_PROC = 2007;

    MESSAGE_PREFIX     = 1000;
    DESCRIPTION_PREFIX = 10000;
    FIXES_PREFIX       = 20000;

  procedure ThrowError(const AError: Integer; var AScript: TScript;
                       const AFormats: array of const);
  procedure PrintErrorInfo(const AError: Integer);
implementation
  procedure ThrowError(const AError: Integer; var AScript: TScript;
                       const AFormats: array of const);
  begin
    deasherror(Format('eval for script %s failed at line %d:',
                  [
                    AScript.scriptpath,
                    AScript.nline
                  ]
                )
              );
    deasherror(Format(GetResourceString(AError+MESSAGE_PREFIX), AFormats));
    deasherror(Format
                (
                  'Error code E%.4d; Run "deash -e <error code>" for'+
                    'more information',
                  [
                    AError
                  ]
                )
              );
  end;

  function GetErrorCategory(const AError: Integer): String;
  begin
    GetErrorCategory := 'unknown';

    if (AError >= ERR_TYPE_SCRIPT)  then GetErrorCategory := 'Script';
    if (AError < ERR_TYPE_SCRIPT)   then GetErrorCategory := 'Internal';
    if (AError < ERR_TYPE_INTERNAL) then GetErrorCategory := 'General';
  end;

  procedure PrintErrorInfo(const AError: Integer);
  var
    fix_tmp: String;
  begin
    writeln(Format('Error manual for error E%.4d', [AError]));
    writeln;
    writeln('Category: ', GetErrorCategory(AError));
    writeln('Message:');
    writeln(GetResourceString(AError+MESSAGE_PREFIX));
    writeln;
    writeln('Description:');
    writeln(GetResourceString(AError+DESCRIPTION_PREFIX));
    writeln;
    writeln('Fixes:');
    fix_tmp := GetResourceString(AError+FIXES_PREFIX);
    if fix_tmp = '' then fix_tmp := 'Currently there are no fixes available'+
                                    'for this error, sorry :(';
    writeln(fix_tmp);
  end;
end.
