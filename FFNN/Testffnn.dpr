{
This is example of using ffnn.
We'll create neural network with one input and one output and with one
hidden layer.
That neural network will give us quadrant based on input angle which will
we provide...
If angle is between 0 and pi/2 result (quadrant) is 0;
If angle is between pi/2 and pi result is 1;
If angle is between pi and 3*pi/2 result (quadrant) is 2;
If angle is between 3*pi/2 and 2*pi result (quadrant) is 3;
}

program Testffnn;
{$APPTYPE CONSOLE}
uses
  SysUtils, ffnn; //Of course we have to add "djNN".

var
  nn: Tffnn; //This will be our Neural Network.
  //Auxiliary variables
  i: Integer;
  angle, quadrant: Real;

begin
  nn:=Tffnn.Create(1); //We'll create NN with one input.
  {
  Now we will add two layers.
  Firs one will have 4 neurons.
  It will be hidden layer becouse we will add another one.
  Second will have 1 neuron.
  It will be the output layer becouse we'll not add any more layers.
  Other parameters are:
    InCenter - what will be average input in that layer
    InRadius - how much will input in that layer change (add 20-50%)
    OutMin - Minimal value of output from layer
    OutWidth - How much to maximal output
  Standard values:
    For input (InCenter, InRadius)=(0, 1) This is sigmoid between -1 and 1.
    For output (OutMin, OutWidth)=(-1, 2) This is that same sigmoid.
  On first hidden layer and on output layer we'll use different values
  to scale inputs and outputs to our problem. On other layers standard sigmoid
  will do the job.
  }
  nn.AddLayer(4, pi, 1.5*pi, -1, 2);
  nn.AddLayer(1, 0, 1, -0.5, 5);

  //Now we have the network and we will teach it.
  randomize;
  for i:=1 to 100000 do begin //With how many samples
    angle:=random(6280)/1000; //Get random angle between 0 and 2*pi.
    //Calculate output, so we could teach NN.
    quadrant:=Trunc(angle/(pi/2));

    //Set input and desired output
    nn.Input[1]:=angle;
    nn.DesiredOutput[1]:=quadrant;
    //Tell network to consider this input and desired output.
    nn.BackProp(0.1, 0.1);
    {
    Constants used here (0.1, 0.1)=(BPSpeed, Inertion) are parameters
    for Back Propagation algorithm. This values will be just fine.
    }
  end;

  //Now we'll see what NN know.
  for i:=1 to 20 do begin
    angle:=random(6280)/1000;
    //In next two lines we can see how NN could be used
    nn.Input[1]:=angle;
    nn.CalcOut;
    //After CalcOut we can read the output.
    {
    Input, DesiredOutput and Output are TNeuralVector-s.
    TNeuralVector = array of Real;
    Element with index 0 is reserved.
    First user value have index 1, and last have index equal to Count
      (Input.Count, DesiredOutput.Count or Output.Count).
    }
    quadrant:=nn.Output[1];
    writeln(Format('Angle=%5.3f Should be=%d Result=%d',
      [angle, Trunc(angle/(pi/2)), Round(quadrant)]));
  end;

  //Be annoying.
  writeln('Press ENTER');
  readln;

  //Free the object.
  nn.Free;

  {
  There are two more methods: SaveToFile(FileName) and ReadFromFile(FileName).
  We could use it to store and retrive all network data.
  }
end.


