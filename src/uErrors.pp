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

  function GetResourceString(const AId: String): String;
  procedure ThrowError(const AError: Integer; var AScript: TScript; const AFormats: array of const);
  procedure PrintErrorInfo(const AError: Integer);
implementation
  function GetResourceString(const AId: String): String;
  var
    ress: TResources;
    res: TAbstractResource;
    res_reader: TElfResourceReader;
    str_stream: TStringStream;
    _str: String;
  begin
    result := '';

    res_reader := TElfResourceReader.Create;
    ress := TResources.Create;
    ress.loadfromfile(ParamStr(0),res_reader);
    res_reader.Free;
    res := ress.find(RT_STRING, 1);
    ress.Free;

      writeln(TStringTableResource(res).strings[0]);
  end;

  procedure ThrowError(const AError: Integer; var AScript: TScript; const AFormats: array of const);
  begin
    deasherror(Format('eval for script %s failed at line %d:', [AScript.scriptpath, AScript.nline]));
    deasherror('message');
    deasherror(Format('Error code E%.5d; Run "deash -e <error code>" for more information', [AError]));
  end;

  procedure PrintErrorInfo(const AError: Integer);
  begin
  end;
end.
