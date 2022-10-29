{
Dataland Software Feed Forward Neural Network v2001.1
This is Delphi unit for using Feed Forward Neural Networks.

E-mail: office@datalandsoftware.com
Web: www.datalandsoftware.com

This software is OSI Certified Open Source Software.
OSI Certified is a certification mark of the Open Source Initiative.
You can look at www.opensource.org for information about "OSI Certified".

``The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); 
you may not use this file except in compliance with the License. 
You may obtain a copy of the License at http://www.mozilla.org/MPL/ 
Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF 
ANY KIND, either express or implied. See the License for the specific language governing rights and 
limitations under the License. 

The Original Code is created by Dragan Djapic. 

The Initial Developer of the Original Code is Dragan Djapic. Portions created by 
Dragan Djapic are Copyright (C) by Dragan Djapic. All Rights Reserved. 

Contributor(s): ______________________________________. 
}

unit ffnn0;

interface

type
TFunction = function (x: Real): Real;

TNeuralVector = array of Real;

TNLayer=class(TObject)
  private
  protected
    v: TNeuralVector;
  public
    weight, dw: array of array of Real;
    i: ^TNeuralVector;
    o: TNeuralVector;
    InCenter, InRadius,  OutMin, OutWidth: Real;
    constructor Create(NumOfNeurons: Word;
      aInCenter, aInRadius, aOutMin, aOutWidth: Real);
    procedure CalcOut;
    procedure BackProp(var d: TNeuralVector; BPSpeed, Inertion: Real);
    procedure InitWeight;
    function Sigmoid(x: Real): Real;
    function SigmoidDeriv(x: Real): Real;
end;

Tffnn=class(TObject)
  private
  protected
    NLayers: array of TNLayer;
  public
    Input, Output: TNeuralVector;
    DesiredOutput: TNeuralVector;
    constructor Create(NumOfInputs: Word);
    destructor Destroy; override;
    procedure EmptyLayers;
    procedure SaveToFile(FileName: string);
    procedure LoadFromFile(FileName: string);
    procedure AddLayer(NumOfNeurons: Word;
      InCenter, InRadius, OutMin, OutWidth: Real);
    procedure CalcOut;
    procedure BackProp(BPSpeed, Inertion: Real);
end;

implementation

constructor TNLayer.Create(NumOfNeurons: Word;
      aInCenter, aInRadius, aOutMin, aOutWidth: Real);
begin
  SetLength(o, NumOfNeurons+1);
  o[0]:=-1;
  SetLength(v, NumOfNeurons+1);
  InCenter:=aInCenter;
  InRadius:=aInRadius;
  OutMin:=aOutMin;
  OutWidth:=aOutWidth;
end;

procedure TNLayer.CalcOut;
var
  j,k: Word;
begin
  for j:=1 to Length(o)-1 do begin
    v[j]:=0;
    for k:=0 to Length(i^)-1 do
      v[j]:=v[j]+weight[j,k]*i^[k];
    o[j]:=Sigmoid(v[j]);
  end;
end;

procedure TNLayer.BackProp (var d: TNeuralVector; BPSpeed, Inertion: Real);
var
  j,k: Word;
begin
  for j:=1 to Length(o)-1 do
    d[j]:=d[j]*SigmoidDeriv(v[j]);
  for j:=1 to Length(o)-1 do
    for k:=0 to Length(i^)-1 do begin
      dw[j,k]:=dw[j,k]*Inertion + BPSpeed*d[j]*i^[k];
      weight[j,k]:=weight[j,k]+dw[j,k];
    end;
  for j:=2 to Length(o)-1 do
    d[1]:=d[1]+d[j];
  SetLength(d, Length(i^));
  for j:=2 to Length(d)-1 do
    d[j]:=d[1];
end;

procedure TNLayer.InitWeight;
var
  j,k: Word;
begin
  SetLength(weight, Length(o), Length(i^));
  SetLength(dw, Length(o), Length(i^));
  for j:=1 to Length(o)-1 do
    for k:=0 to Length(i^)-1 do begin
      weight[j,k]:=InRadius*(2*(2.4/(Length(i^)-1))*random(1000)/1000-1);
      dw[j,k]:=0;
    end;
end;

function TNLayer.Sigmoid(x: Real): Real;
begin
  result:=OutWidth/(1+exp(-(x-InCenter)/InRadius))+OutMin
end;

function TNLayer.SigmoidDeriv(x: Real): Real;
begin
  result:=(exp(-(x-InCenter)/InRadius)*OutWidth/InRadius)/
    sqr(1+exp(-(x-InCenter)/InRadius))
end;

constructor Tffnn.Create(NumOfInputs: Word);
begin
  SetLength(Input, NumOfInputs+1);
  Input[0]:=-1;
  SetLength(NLayers, 0);
  randomize;
end;

destructor Tffnn.Destroy;
begin
  EmptyLayers;
  Inherited Destroy;
end;

procedure Tffnn.EmptyLayers;
begin
  while Length(NLayers) > 0 do begin
    NLayers[Length(NLayers)-1].Free;
    SetLength(NLayers, Length(NLayers)-1);
  end;
end;

procedure Tffnn.SaveToFile(FileName: string);
var
  f: textfile;
  i,j,k: Word;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  writeln(f, 'Feed forward neural network file.');
  writeln(f, 'Do NOT edit this file!');
  writeln(f, Length(Input)-1);
  writeln(f, Length(NLayers));
  for i:=0 to Length(NLayers)-1 do begin
    with NLayers[i] do begin
      writeln(f, Length(o)-1);
      writeln(f, InCenter, ' ', InRadius, ' ',OutMin, ' ',OutWidth);
      for j:=1 to Length(o)-1 do begin
        for k:=0 to Length(i^)-1 do
          Write(f, weight[j,k], ' ');
        writeln(f);
      end;
    end;
  end;
  CloseFile(f);
end;

procedure Tffnn.LoadFromFile(FileName: string);
var
  f: textfile;
  i,j,k: Word;
  NumOfNeurons: Word;
  NumOfLayers: Word;
  aInCenter, aInRadius, aOutMin, aOutWidth: Real;
  TempInt: Integer;
begin
  AssignFile(f, FileName);
  Reset(f);
  EmptyLayers;
  readln(f);
  readln(f);
  readln(f, TempInt);
  SetLength(Input, TempInt+1);
  Input[0]:=-1;
  readln(f, NumOfLayers);
  for i:=0 to NumOfLayers-1 do begin
    readln(f, NumOfNeurons);
    readln(f, aInCenter, aInRadius, aOutMin, aOutWidth);
    AddLayer(NumOfNeurons, aInCenter, aInRadius, aOutMin, aOutWidth);
    with NLayers[i] do begin
      for j:=1 to Length(o)-1 do begin
        for k:=0 to Length(i^)-1 do
          Read(f, weight[j,k]);
        readln(f);
      end;
    end;
  end;
  CloseFile(f);
end;

procedure Tffnn.AddLayer(NumOfNeurons: Word;
  InCenter, InRadius, OutMin, OutWidth: Real);
begin
  SetLength(NLayers, Length(NLayers)+1);
  NLayers[Length(NLayers)-1]:=TNLayer.Create(NumOfNeurons,
    InCenter, InRadius, OutMin, OutWidth);
  if Length(NLayers)>1 then begin
    NLayers[Length(NLayers)-1].i:=@NLayers[Length(NLayers)-2].o;
  end else begin
    NLayers[Length(NLayers)-1].i:=@Input;
  end;
  NLayers[Length(NLayers)-1].InitWeight;
  //Next line is for case that just added layer is output layer.
  SetLength(DesiredOutput, NumOfNeurons+1);
end;

procedure Tffnn.CalcOut;
var
  j: Word;
begin
  for j:=0 to Length(NLayers)-1 do
    NLayers[j].CalcOut;
  Output:=NLayers[Length(NLayers)-1].o;
end;

procedure Tffnn.BackProp(BPSpeed, Inertion: Real);
var
  j: Word;
  d: TNeuralVector;
begin
  CalcOut;
  SetLength(d, Length(Output));
  for j:=1 to Length(Output)-1 do
    d[j]:=DesiredOutput[j]-Output[j];
  for j:=Length(NLayers)-1 downto 0 do
    NLayers[j].BackProp(d, BPSpeed, Inertion);
end;

end.