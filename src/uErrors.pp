{$mode objfpc}
unit uErrors;

{ uErrors.pp ; Error System for deash }
{ Author: Marie Eckert                }

{$R res/error_message.rc}

interface
  uses SysUtils, Resource, stringtableresource, ElfReader, Classes, uTypes, uXDebug, uHelpers;

  const
    { Error Types }
    ERR_TYPE_GENERAL  = 0;
    ERR_TYPE_INTERNAL = 1000;
    ERR_TYPE_SCRIPT   = 2000;

    { Error Codes }
    ERR_GENERAL_FILE_NOT_FOUND   = 0001;
    ERR_GENERAL_NO_CONFIG        = 00002;
    ERR_GENERAL_MALFORMED_CONFIG = 00003;

    ERR_INTERNAL_UNKNOWN = 1001;

    ERR_SCRIPT_UNKNOWN_IDENTIFIER    = 2001;
    ERR_SCRIPT_IF_MALFORMED_VALUE    = 2002;
    ERR_SCRIPT_IF_MALFORMED_VALOPVAL = 2003;
    ERR_SCRIPT_IF_MISMATCHED_TYPES   = 2004;
    ERR_SCRIPT_IF_INVALID_OPERATOR   = 2005;

    DESCRIPTION_PREFIX = 10000;
    FIXES_PREFIX       = 20000;

  function GetResourceString(const AId: Integer): String;
  procedure ThrowError(const AError: Integer; var AScript: TScript; const AFormats: array of const);
  procedure PrintErrorInfo(const AError: Integer);
implementation
  function GetResourceString(const AId: Integer): String;
  var
    res: TResources;
    stres: TStringTableResource;
  begin
    result := '';
    res := TResources.Create;
    try
      res.loadfromfile(ParamStr(0));
      stres := res.find(RT_STRING, succ(AId shr 4)) as TStringTableResource;
      result := stres.strings[AId];
    finally
      res.Free
    end;
  end;

  procedure ThrowError(const AError: Integer; var AScript: TScript; const AFormats: array of const);
  begin
    deasherror(Format('eval for script %s failed at line %d:', [AScript.scriptpath, AScript.nline]));
    deasherror(Format(GetResourceString(AError), AFormats));
    deasherror(Format('Error code E%.5d; Run "deash -e <error code>" for more information', [AError]));
  end;

  function GetErrorCategory(const AError: Integer): String;
  begin
    GetErrorCategory := 'unknown';

    if (AError >= ERR_TYPE_SCRIPT)  then GetErrorCategory := 'Script';
    if (AError < ERR_TYPE_SCRIPT)   then GetErrorCategory := 'Internal';
    if (AError < ERR_TYPE_INTERNAL) then GetErrorCategory := 'General';
  end;

  procedure PrintErrorInfo(const AError: Integer);
  begin
    writeln('Error manual for error E%.5d');
    writeln;
    writeln('Category: ', GetErrorCategory(AError));
    writeln('Message:');
    writeln(GetResourceString(AError));
    writeln;
    writeln('Description:');
    writeln(GetResourceString(AError+DESCRIPTION_PREFIX));
  end;
end.
