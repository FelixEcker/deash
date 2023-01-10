{$mode fpc}
unit uHelpers;

{ uHelpers.pas ; Helper functions for deash }
{ Author: Felix Eckert                      }

{$H+}

interface
  procedure DeashError(const AMsg: String);
  function BinaryExists(const AName: String; var APath: String): Boolean;
implementation
  procedure DeashError(const AMsg: String);
  begin
    writeln('deash <ERROR>:: ', AMsg);
  end;

  function BinaryExists(const AName: String; var APath: String): Boolean;
  begin
  end;
end.
