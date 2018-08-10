program Project1;

{$MODE Delphi}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms, Interfaces,
  Unit1 in 'Unit1.pas' {Form1},
  DefLayersFrm in 'DefLayersFrm.pas' {DefLayersForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
