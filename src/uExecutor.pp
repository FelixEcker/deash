{$mode fpc}
unit uExecutor;

{ uExectuor.pp ; Invoke execution handler for deash }
{ Author: Marie Eckert                              }

interface
  uses Classes, StrUtils, SysUtils, Types, Process, uDEASHConsts, uHelpers;

  type
    TInvoke = record
      invoketype : Integer;
      location   : String;
      parameters : TStringDynArray;
    end;

    TInvokeResult = record
      code: Integer;
      message: String;
    end;
  
  function ExecBin(const ABinaryLocation: String; const AParameters: TStringDynArray): TInvokeResult;

  const
    BUF_SIZE = 2048;
implementation
  function ExecBin(const ABinaryLocation: String; const AParameters: TStringDynArray): TInvokeResult;
  var
    process: TProcess;
    parameter: String;
  begin
    ExecBin.code := 0;
    ExecBin.message := '';

    process := TProcess.Create(nil);
    process.executable := ABinaryLocation;
    for parameter in AParameters do
      process.parameters.add(parameter);

    process.options := [poPassInput];
    process.execute;

    while process.running do
    begin
    end;

    process.free;
  end;
end.
