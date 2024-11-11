program ComRobot;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, main, lnetvisual, tachartlazaruspkg, printer4lazarus, pascalscript,
  sdpofpseriallaz, sdpojoysticklaz, gridsheet, enum_serial, VerInfo, gchannels,
  dynmatrix, editor, uPSI_dynmatrix, uPSI_user_charts, user_charts;

{$R *.res}

begin
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFEditor, FEditor);
  Application.Run;
end.

