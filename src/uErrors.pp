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
    ERR_TYPE_INTERNAL = 10000;
    ERR_TYPE_SCRIPT   = 20000;

    { Error Codes }
    ERR_GENERAL_FILE_NOT_FOUND   = 00001;
    ERR_GENERAL_NO_CONFIG        = 00002;
    ERR_GENERAL_MALFORMED_CONFIG = 00003;

    ERR_INTERNAL_UNKNOWN = 10001;

    ERR_SCRIPT_UNKNOWN_IDENTIFIER    = 20001;
    ERR_SCRIPT_IF_MALFORMED_VALUE    = 20002;
    ERR_SCRIPT_IF_MALFORMED_VALOPVAL = 20003;
    ERR_SCRIPT_IF_MISMATCHED_TYPES   = 20004;
    ERR_SCRIPT_IF_INVALID_OPERATOR   = 20005;

  function GetResourceString(const AId: Integer): String;
  procedure ThrowError(const AError: Integer; var AScript: TScript; const AFormats: array of const);
  procedure PrintErrorInfo(const AError: Integer);
implementation
  function GetResourceString(const AId: Integer): String;
  var
    res: TResources;
    stres: TStringTableResource;
  begin
    result:= '';
    res:= TResources.Create;
    try
      res.loadfromfile(ParamStr(0));
      stres:= res.find(RT_STRING, succ(AId shr 4)) as TStringTableResource;
      result:= stres.strings[AId];
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

  procedure PrintErrorInfo(const AError: Integer);
  begin
  end;
end.
