program VizTable;

uses
  Forms,
  VizMakeTBL in 'VizMakeTBL.pas' {FormGesture};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Gesture Delphi Demo';
  Application.CreateForm(TFormGesture, FormGesture);
  Application.Run;
end.
