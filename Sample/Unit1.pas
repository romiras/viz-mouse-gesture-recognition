unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FFNN;

type
  TForm1 = class(TForm)
    TrainB: TButton;
    LoadB: TButton;
    SaveB: TButton;
    ShowB: TButton;
    Memo1: TMemo;
    FFNN1: TFFNN;
    procedure TrainBClick(Sender: TObject);
    procedure LoadBClick(Sender: TObject);
    procedure SaveBClick(Sender: TObject);
    procedure ShowBClick(Sender: TObject);
  private
    { Private declarations }
    function func(x: Real): Real;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.TrainBClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=1 to 10000 do begin
    //We'll take constant number of iterations
    //In real application we would measure error and stop training when error is
    //small enough. This is only an example.

    FFNN1.Input[1]:=i mod 7; //Set input
    FFNN1.DesiredOutput[1]:=func( i mod 7 ); //Set what we would want on Output
    FFNN1.BackProp; //Train network with this pair (In, Out).

    //NOTE: When using vectors: Input, Output and DesiredOutput use index
    //between 1 and Count (Input[1], Input[2], Output[1],...
    //Input[0], Output[0] and DesiredOutput[0] are reserved.
  end;
end;

procedure TForm1.LoadBClick(Sender: TObject);
begin
  FFNN1.LoadFromFile('Linear.nn'); //Loads everything. This will overrun design time settings.
end;

procedure TForm1.SaveBClick(Sender: TObject);
begin
  FFNN1.SaveToFile('Linear.nn');
end;

procedure TForm1.ShowBClick(Sender: TObject);
var
  r, t: Real;
  i: Integer;
begin
  for i:=0 to 6 do begin
    r:=i;
    FFNN1.Input[1]:=r; //Set network input
    FFNN1.CalcOut; //Calculate output. After this line we can read Output.
    t:=FFNN1.Output[1];
    //We'll set DesiredOutput only becouse we would like to call GetError functions
    //In real life we don't know Desired out.
    FFNN1.DesiredOutput[1]:=func( i );
    Memo1.Lines.Add(Format('%f %f %f %f', [r, t, FFNN1.GetAvgError, FFNN1.GetMaxError]));
      //Both errors are same becouse we have 1 output (Avg = Max)
  end;
end;

function TForm1.func(x: Real): Real;
begin
  //We'll train network to reproduce this function
  Result:=10 + x;
end;

end.
