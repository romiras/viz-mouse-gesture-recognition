program gesture;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormGesture},
  GestureTypes in 'GestureTypes.pas',
  PointsOp in 'PointsOp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Gesture Delphi Demo';
  Application.CreateForm(TFormGesture, FormGesture);
  Application.Run;
end.
