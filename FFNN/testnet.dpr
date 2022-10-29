program testnet;

{$APPTYPE CONSOLE}

uses
  FFNN, SysUtils;

var
   netw: TFFNN;
   HiddenLayer: TNLayer;
   i: Integer;
   t, r: real;

function Func (x: Real): Real;
begin
  //We'll train network to reproduce this function
  Result:=10 + x;
end;

begin
     netw := TFFNN.Create(Nil);
     with netw do
     begin
          InputCount := 1;
          OutputCount := 1;
          InputMin := -1;
          InputMax := 8;
          OutputMin := 9;
          OutputMax := 18;

          writeln (NLayers.Count);
          HiddenLayer := AddLayer;

          writeln (HiddenLayer.NeuronCount);
          NLayers.Items[0].NeuronCount := 10;

          writeln (NLayers.Count);
          writeln (NLayers.Items[0].NeuronCount);

          BPSpeed := 0.1;
          Inertion := 0.1;
     end;

     writeln ('Training net...');
     for i:=1 to 10000 do begin
        //We'll take constant number of iterations
        //In real application we would measure error and stop training when error is
        //small enough. This is only an example.

        netw.Input[1]:=i mod 7; //Set input
        netw.DesiredOutput[1]:=func( i mod 7 ); //Set what we would want on Output
        netw.BackProp; //Train network with this pair (In, Out).

        //NOTE: When using vectors: Input, Output and DesiredOutput use index
        //between 1 and Count (Input[1], Input[2], Output[1],...
        //Input[0], Output[0] and DesiredOutput[0] are reserved.
     end;
     writeln ('Training done.');

     for i:=0 to 6 do begin
        r:=i;
        netw.Input[1]:=r; //Set network input
        netw.CalcOut; //Calculate output. After this line we can read Output.
        t:=netw.Output[1];
        //We'll set DesiredOutput only becouse we would like to call GetError functions
        //In real life we don't know Desired out.
        netw.DesiredOutput[1]:=func( i );
        writeln (Format('%f %f %f %f', [r, t, netw.GetAvgError, netw.GetMaxError]));
          //Both errors are same becouse we have 1 output (Avg = Max)
     end;

     netw.Free;
     readln
end.
 