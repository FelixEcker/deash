{$mode fpc}
program deash;

{ deash.pp ; Entry Point for deash }
{ author: Felix Eckert             }

{$H+}

uses SysUtils, uXDebug, uDEASHConsts, uInteractiveMode, uScriptEngine, uHelpers;

procedure GiveVersion;
begin
  writeln(':: deash version ', VERSION, '-', VERSION_TYPE);
end;

procedure DeashInfo;
begin
  GiveVersion;
  writeln(':: by ', DEV_INFO);
  writeln(':: git repository: https://github.com/FelixEcker/deash.git ');
  writeln(':: licensed under the bsd 3-clause license');
end;

procedure DeashHelp;
begin
  writeln(':: -- HELP --');
  GiveVersion;
  writeln(':: Usage: deash [script file, empty for interactive] [options]');
  writeln('::');
  writeln(':: Options:');
  writeln(':: --info         Info text about deash');
  writeln(':: --help         This help-text');
  writeln(':: --no-fallback  Run without a fallback shell to escape into');
  writeln('::                incase deash crashes');
end;

begin
  SetShellEnv('SH_VERSION', VERSION + '-' + VERSION_TYPE);
  SetShellEnv('SH_AUTHOR', DEV_INFO);
  SetShellEnv('SH_BINLOC', ParamStr(0));

  program_start := Time;
  if (ParamCount = 0) then
  begin
    SetShellEnv('SH_MODE', 'INTERACTIVE');
    LaunchShell;
  end
  else if (ParamStr(1) = '--info') then
    DeashInfo
  else if (ParamStr(1) = '--help') then
    DeashHelp
  else begin
    SetShellEnv('SH_MODE', 'SCRIPT_EXEC');
    DoScriptExec(ParamStr(1));
  end;
end.
