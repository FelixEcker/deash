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
    bytes_read : Longint;
    buffer : array[1..BUF_SIZE] of Byte;
    i: Integer;
  begin
    ExecBin.code := 0;
    ExecBin.message := '';

    process := TProcess.Create(nil);
    process.executable := ABinaryLocation;
    for parameter in AParameters do
      process.parameters.add(parameter);

    process.options := [poUsePipes];
    process.execute;

    repeat
      bytes_read := process.output.read(buffer, BUF_SIZE);
      for i := 1 to bytes_read do
        write(Char(buffer[i]));
    until bytes_read = 0;

    process.free;
  end;
end.
