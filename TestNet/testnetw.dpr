program testnetw;

uses
  Forms,
  netw2 in 'netw2.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
