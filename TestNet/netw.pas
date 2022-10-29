
unit netw;

interface

uses
  Classes, Controls, Forms, StdCtrls,
  FFNN;

type
  TForm1 = class(TForm)
    netw: TFFNN;
    Memo1: TMemo;
    btTrain: TButton;
    btShow: TButton;
    procedure btShowClick(Sender: TObject);
    procedure btTrainClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation
uses SysUtils;

{$R *.dfm}


function func(x: Real): Real;
begin
  //We'll train network to reproduce this function
  Result := 10 + x;
end;


procedure TrainNetwork;
var
  i: Integer;
begin
  with Form1 do
  for i := 1 to 10000 do begin
    //We'll take constant number of iterations
    //In real application we would measure error and stop training when error is
    //small enough. This is only an example.

    Netw.Input[1] := i mod 7; //Set input
    Netw.DesiredOutput[1] := func( i mod 7 ); //Set what we would want on Output
    Netw.BackProp; //Train network with this pair (In, Out).

    //NOTE: When using vectors: Input, Output and DesiredOutput use index
    //between 1 and Count (Input[1], Input[2], Output[1],...
    //Input[0], Output[0] and DesiredOutput[0] are reserved.
  end;
end;


procedure TForm1.btTrainClick(Sender: TObject);
begin
     TrainNetwork;
end;


procedure TForm1.btShowClick(Sender: TObject);
var
  r, t: Real;

procedure CalculateOutput;
var
  i: Integer;
begin
  with Form1 do
  for i := 0 to 6 do begin
    r := i;
    Netw.Input[1] := r; //Set network input
    Netw.CalcOut; //Calculate output. After this line we can read Output.
    t := Netw.Output[1];

    //We'll set DesiredOutput only becouse we would like to call GetError functions
    //In real life we don't know Desired out.
    Netw.DesiredOutput[1] := func( i );

    Memo1.Lines.Add(Format('%f %f %f %f', [r, t, Netw.GetAvgError, Netw.GetMaxError]));
      //Both errors are same becouse we have 1 output (Avg = Max)
  end;
end;

begin
    Memo1.Lines.Clear;
    CalculateOutput;
end;

end.
