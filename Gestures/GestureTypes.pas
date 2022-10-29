unit GestureTypes;

interface
uses Classes, FFNN;

const
    ErrGain = 0.8;
type
    Arr = array[0..15] of real;

var
    Net: TFFNN;

    { Store calculated cosines and sinuses }
    m_cosines,
    m_sinuses : Arr;

procedure RecognizePoints (var imax: integer);
procedure Train (z : integer);


implementation
uses Math, PointsOp;


{-----------------------------------------------------------------------------
  Description: Train the network
-----------------------------------------------------------------------------}
procedure Train (z: integer);
var
   i : integer;
begin
   // Feed the cosinus and sinus values to the network
   for i:=0 to 15 do
   begin
       Net.Input[i+1] := m_cosines[i];
       Net.Input[i+1+16] := m_sinuses[i];
   end;

   // Set all outputs to zero
   for i := 1 to 28 do
      Net.DesiredOutput[i] := 0.01;

   // Set the desired output to one
   Net.DesiredOutput[z] := 0.9;

   // Train
   Net.BackProp;
end;

{-----------------------------------------------------------------------------
  Description: Feed the cosinus and sinus values to the FFNN and determine the max output neuron
-----------------------------------------------------------------------------}
procedure RecognizePoints (var imax: integer);
var
   i : integer;
   maxv: real;
begin
   // Add the values to the network
   for i:=0 to 15 do
   begin
       Net.Input[i+1] := m_cosines[i];
       Net.Input[i+1+16] := m_sinuses[i];
   end;

   // Calculate the output
   Net.CalcOut;

   // Find the max output neuron
   maxv := 0;
   imax := 0;

   for i:=1 to 28 do
   begin
       if Net.Output[i] > maxv then
       begin
           maxv := Net.Output[i];
           imax := i;
       end;
   end;
end;

end.
