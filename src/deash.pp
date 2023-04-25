{$mode fpc}
program deash;

{ deash.pp ; Entry Point for deash }
{ author: Marie Eckert             }

{$H+}
{$R res/info.rc}

uses SysUtils, StrUtils, Types, uXDebug, uDEASHConsts, uInteractiveMode, uScriptEngine, uHelpers, uErrors;

const
  RSTRING_LICENSE = 1;
  RSTRING_VERSION = 2;
  RSTRING_AUTHOR  = 3;

(* Write the version to stdout *)
procedure GiveVersion;
begin
  writeln(':: deash version ', GetResourceString(RSTRING_VERSION));
end;

(* Quick information about deash *)
procedure DeashInfo;
begin
  GiveVersion;
  writeln(':: by ', GetResourceString(RSTRING_AUTHOR));
  writeln(':: git repository: https://github.com/FelixEcker/deash.git ');
  writeln(':: licensed under the bsd 3-clause license');
end;

(* Display deash's license *)
procedure DeashLicense;
var
  lstrsplit: TStringDynArray;
  str: String;
begin
  lstrsplit := SplitString(GetResourceString(RSTRING_LICENSE), Char($0A));
  for str in lstrsplit do
    writeln(':: ', str);
end;

(* Display the help-text *)
procedure DeashHelp;
begin
  writeln(':: -- HELP --');
  GiveVersion;
  writeln(':: Usage: deash [script file, empty for interactive] [options]');
  writeln('::');
  writeln(':: Options:');
  writeln(':: -e --error     Display an error manual page');
  writeln(':: --info         Info text about deash');
  writeln(':: --license      deash''s license text');
  writeln(':: --help         This help-text');
  {writeln(':: --no-fallback  Run without a fallback shell to escape into');
  writeln('::                incase deash crashes');}
end;

(* Show the error manual entry for the error code in ParamStr(2) *)
procedure ErrorManual;
var
  code: String;
  acode: LongInt;
begin
  if ParamCount < 2 then
  begin
    deasherror('Provide an error code to show its manual page! (e.g. E0001');
    exit;
  end;
  
  code := Copy(ParamStr(2), 2, Length(ParamStr(2)));
  if not TryStrToInt(code, acode) then
  begin
    deasherror('The code ' + ParamStr(2) + ' is not a valid error-code!');
    exit;
  end;

  PrintErrorInfo(acode);
end;

begin
  SetShellEnv('SH_VERSION', GetResourceString(RSTRING_VERSION));
  SetShellEnv('SH_AUTHOR', GetResourceString(RSTRING_AUTHOR));
  SetShellEnv('SH_BINLOC', ParamStr(0));

  program_start := Time;
  if (ParamCount = 0) then
  begin
    SetShellEnv('SH_MODE', 'INTERACTIVE');
    LaunchShell;
    halt;
  end;
  
  case ParamStr(1) of
  '--info': DeashInfo;
  '--help': DeashHelp;
  '--license': DeashLicense;
  '-e', '--error': ErrorManual;
  else begin
    SetShellEnv('SH_MODE', 'SCRIPT_EXEC');
    DoScriptExec(ParamStr(1));
  end; end;
end.
