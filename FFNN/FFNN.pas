{
Dataland Software Feed Forward Neural Network v2.0
This is Delphi component which implements Feed Forward Neural Networks.

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

unit FFNN;

{$IFDEF FPC} {$Mode Delphi} {$ENDIF}

interface

uses
  Classes;

type

  TNeuralVector = array of Real;
  TFFNN = class;

  TNLayer=class(TCollectionItem)
  private
    function GetNeuronCount: Integer;
    procedure SetNeuronCount(const Value: Integer);
  protected
    v: TNeuralVector;
    InCenter, InRadius,  OutMin, OutWidth: Real;
  public
    weight, dw: array of array of Real;
    i: ^TNeuralVector;
    o: TNeuralVector;
    constructor Create(Collection: TCollection); override;
    procedure CalcOut;
    procedure BackProp(var d: TNeuralVector; BPSpeed, Inertion: Real);
    function Sigmoid(x: Real): Real;
    function Deriv(x: Real): Real;
    procedure InitWeight;
    procedure SetInputMinMax(x, y: Real);
    function GetInputMin: Real;
    function GetInputMax: Real;
    procedure SetOutputMinMax(x, y: Real);
    function GetOutputMin: Real;
    function GetOutputMax: Real;
  published
    property NeuronCount: Integer read GetNeuronCount write SetNeuronCount;
  end;

  TNLayerClass = class of TNLayer;

  TNLayers=class(TCollection)
  private
    FFFNN: TFFNN;
    function GetItem(Index: Integer): TNLayer;
    procedure SetItem(Index: Integer; Value: TNLayer);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(FFNN: TFFNN);
    function Add: TNLayer;
    function AddItem(Item: TNLayer; Index: Integer): TNLayer;
    function Insert(Index: Integer): TNLayer;
    property Items[Index: Integer]: TNLayer read GetItem write SetItem; default;
  end;

  TFFNN = class(TComponent)
  private
    function GetInputCount: Integer;
    procedure SetInputCount(const Value: Integer);
    function GetOutputCount: Integer;
    procedure SetOutputCount(const Value: Integer);
    procedure SetNLayers(const Value: TNLayers);
    function GetInputMax: Real;
    function GetInputMin: Real;
    function GetOutputMax: Real;
    function GetOutputMin: Real;
    procedure SetInputMax(const Value: Real);
    procedure SetInputMin(const Value: Real);
    procedure SetOutputMax(const Value: Real);
    procedure SetOutputMin(const Value: Real);
  protected
    FInputMin, FInputMax: Real;
    FOutputMin, FOutputMax: Real;
    FNLayers: TNLayers;//Hidden layers
    FOutLayer: TNLayer;//Output layer
    FBPSpeed, FInertion: Real;//Const's in BackProp algorithm
    function GetNLayerClass: TNLayerClass; virtual;
    function CreateNLayer: TNLayer; virtual;
    procedure ConnectLayers;
    procedure InitWeights;
    function GetInputLayer: TNLayer;
    function GetOutputLayer: TNLayer;
  public
    Input, Output: TNeuralVector;//Use this to access In and Out. Don't use [0]
    DesiredOutput: TNeuralVector;//Set this prior to calling BackProp
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearLayers;
    procedure SaveToFile(FileName: string);//Save entire network
    procedure LoadFromFile(FileName: string);//Load it
    function AddLayer: TNLayer; overload;
    procedure CalcOut;
    function BackProp: Real; overload;
    function GetAvgError: Real;
    function GetMaxError: Real;
  published
    property InputCount: Integer read GetInputCount write SetInputCount;
    property OutputCount: Integer read GetOutputCount write SetOutputCount;
    property InputMin: Real read GetInputMin write SetInputMin;
    property InputMax: Real read GetInputMax write SetInputMax;
    property OutputMin: Real read GetOutputMin write SetOutputMin;
    property OutputMax: Real read GetOutputMax write SetOutputMax;
    property NLayers: TNLayers read FNLayers write SetNLayers;
    property BPSpeed: Real read FBPSpeed write FBPSpeed;
    property Inertion: Real read FInertion write FInertion;
  end;

procedure Register;

implementation

{$R *.dcr}

procedure Register;
begin
  RegisterComponents('Dataland', [TFFNN]);
end;

constructor TNLayer.Create(Collection: TCollection);
begin
  NeuronCount:=3;
  InCenter:=0;
  InRadius:=1;
  OutMin:=-1;
  OutWidth:=2;
  inherited;//Update will be called in here so we have to set properties before.
end;

procedure TNLayer.SetInputMinMax(x, y: Real);
begin
  InCenter:=(x+y)/2;
  InRadius:=(y-x)/2;
end;

function TNLayer.GetInputMin: Real;
begin
  Result:=InCenter-InRadius;
end;

function TNLayer.GetInputMax: Real;
begin
  Result:=InCenter+InRadius;
end;

procedure TNLayer.SetOutputMinMax(x, y: Real);
begin
  OutMin:=x;
  OutWidth:=y-x;
end;

function TNLayer.GetOutputMin: Real;
begin
  Result:=OutMin;
end;

function TNLayer.GetOutputMax: Real;
begin
  Result:=OutMin+OutWidth;
end;

procedure TNLayer.CalcOut;
var
  j,k: Integer;
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
  j,k: Integer;
begin
  for j:=1 to Length(o)-1 do
    d[j]:=d[j]*Deriv(v[j]);
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
  j,k: Integer;
begin
  SetLength(weight, Length(o), Length(i^));
  SetLength(dw, Length(o), Length(i^));
  for j:=1 to Length(o)-1 do
    for k:=0 to Length(i^)-1 do begin
      weight[j,k]:=InRadius*(2*(2.4/(Length(i^)-1))*random(1000)/1000-1);
      dw[j,k]:=0;
    end;
end;

function TNLayer.Deriv(x: Real): Real;
begin
  result:=(exp(-(x-InCenter)/InRadius)*OutWidth/InRadius)/sqr(1+exp(-(x-InCenter)/InRadius));
end;

function TNLayer.Sigmoid(x: Real): Real;
begin
  result:=OutWidth/(1+exp(-(x-InCenter)/InRadius))+OutMin
end;

function TNLayers.Add: TNLayer;
begin
  Result := TNLayer(inherited Add);
end;

function TNLayers.AddItem(Item: TNLayer;
  Index: Integer): TNLayer;
begin
  if Item = nil then
    Result := FFFNN.CreateNLayer
  else
  begin
    Result := Item;
    if Assigned(Item) then
    begin
      Result.Collection := Self;
      if Index < 0 then
        Index := Count - 1;
      Result.Index := Index;
    end;
  end;
end;

function TNLayers.Insert(Index: Integer): TNLayer;
begin
  Result := AddItem(nil, Index);
end;

function TFFNN.GetNLayerClass: TNLayerClass;
begin
  Result:=TNLayer;
end;

constructor TNLayers.Create(FFNN: TFFNN);
begin
  if FFNN <> nil then
    inherited Create(FFNN.GetNLayerClass)
  else
    inherited Create(TNLayer);
  FFFNN := FFNN;
end;


procedure TNLayers.Update(Item: TCollectionItem);
begin
  inherited;
  FFFNN.ConnectLayers;
  FFFNN.InitWeights;
end;


procedure TFFNN.SetNLayers(const Value: TNLayers);
begin
  FNLayers.Assign( Value );
end;

{ TNLayers }

function TNLayers.GetItem(Index: Integer): TNLayer;
begin
  Result := TNLayer(inherited GetItem(Index));
end;

function TNLayers.GetOwner: TPersistent;
begin
  Result := FFFNN;
end;

procedure TNLayers.SetItem(Index: Integer; Value: TNLayer);
begin
  inherited SetItem(Index, Value);
end;

function TFFNN.GetInputCount: Integer;
begin
  Result:=Length(Input)-1;
end;

procedure TFFNN.SetInputCount(const Value: Integer);
begin
  SetLength(Input, Value+1);
end;

constructor TFFNN.Create(AOwner: TComponent);
begin
  Inherited;
  InputCount:=2;
  Input[0]:=-1;
  OutputCount:=1;
  Output[0]:=-1;
  FBPSpeed:=0.1;
  FInertion:=0.1;
  FNLayers:=TNLayers.Create( Self );
  FOutLayer:=TNLayer.Create(nil);
  ConnectLayers;
  randomize;
end;

destructor TFFNN.Destroy;
begin
  ClearLayers;
  FNLayers.Free;
  FOutLayer.Free;
  Inherited Destroy;
end;

procedure TFFNN.ClearLayers;
begin
  NLayers.Clear;
end;

procedure TFFNN.SaveToFile(FileName: string);
var
  f: textfile;
  i,j,k: Integer;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  writeln(f, 'Feed forward neural network file. v2.0.');
  writeln(f, 'Do NOT edit this file!');

  writeln(f, InputCount, ' ', InputMin, ' ', InputMax);
  writeln(f, OutputCount, ' ', OutputMin, ' ', OutputMax);

  writeln(f, FNLayers.Count);
  for i:=0 to FNLayers.Count-1
    do write(f, NLayers[i].NeuronCount, ' ');
  writeln(f, FOutLayer.NeuronCount);

  for i:=0 to FNLayers.Count-1 do with FNLayers[i] do begin
    for j:=1 to Length(o)-1 do begin
      for k:=0 to Length(i^)-1 do
        write(f, weight[j,k], ' ');
      writeln(f);
    end;
  end;

  with FOutLayer do begin
    for j:=1 to Length(o)-1 do begin
      for k:=0 to Length(i^)-1 do
        write(f, weight[j,k], ' ');
      writeln(f);
    end;
  end;

  CloseFile(f);
end;

procedure TFFNN.LoadFromFile(FileName: string);
var
  f: textfile;
  i,j,k, n: Integer;
  a, b: Real;
begin
  AssignFile(f, FileName);
  Reset(f);
  ClearLayers;
  readln(f);
  readln(f);

  readln(f, i, a, b);
  InputCount:=i;
  InputMin:=a;
  InputMax:=b;

  readln(f, i, a, b);
  OutputCount:=i;
  OutputMin:=a;
  OutputMax:=b;

  readln(f, n);
  for i:=0 to n-1 do begin
    NLayers.Add;
    read(f, j);
    NLayers[i].NeuronCount:=j;
  end;
  readln(f, j);
  FOutLayer.NeuronCount:=j;

  ConnectLayers;
  InitWeights;

  for i:=0 to FNLayers.Count-1 do with FNLayers[i] do begin
    for j:=1 to Length(o)-1 do begin
      for k:=0 to Length(i^)-1 do begin
        read(f, a);
        weight[j,k]:=a;
      end;
      readln(f);
    end;
  end;

  with FOutLayer do begin
    for j:=1 to Length(o)-1 do begin
      for k:=0 to Length(i^)-1 do begin
        read(f, a);
        weight[j,k]:=a;
      end;
      readln(f);
    end;
  end;

  CloseFile(f);
end;

procedure TFFNN.CalcOut;
var
  i, j: Integer;
begin
  for j:=0 to FNLayers.Count-1 do
    FNLayers.Items[j].CalcOut;
  FOutLayer.CalcOut;
  for i:=1 to Length(Output)-1 do
    Output[i]:=FOutLayer.o[i];
end;

procedure TFFNN.ConnectLayers;
var
  i: Integer;
begin
  GetInputLayer.SetInputMinMax(FInputMin, FInputMax);
  GetOutputLayer.SetOutputMinMax(FOutputMin, FOutputMax);

  GetInputLayer.i:=@Input;
  if NLayers.Count>0 then begin
    for i:=0 to NLayers.Count-2 do begin
      NLayers.Items[i+1].i:=@(NLayers.Items[i].o);
    end;
    FOutLayer.i:=@(NLayers.Items[NLayers.Count-1].o);
  end;
end;

function TFFNN.GetInputLayer: TNLayer;
begin
  if NLayers.Count>0
    then Result:=NLayers.Items[0]
    else Result:=FOutLayer;
end;

function TFFNN.GetOutputLayer: TNLayer;
begin
  Result:=FOutLayer;
end;

function TFFNN.GetInputMax: Real;
begin
  Result:=FInputMax;
end;

function TFFNN.GetInputMin: Real;
begin
  Result:=FInputMin;
end;

function TFFNN.GetOutputMax: Real;
begin
  Result:=FOutputMax;
end;

function TFFNN.GetOutputMin: Real;
begin
  Result:=FOutputMin;
end;

procedure TFFNN.SetInputMax(const Value: Real);
begin
  FInputMax:=Value;
  GetInputLayer.SetInputMinMax(GetInputLayer.GetInputMin, Value);
end;

procedure TFFNN.SetInputMin(const Value: Real);
begin
  FInputMin:=Value;
  GetInputLayer.SetInputMinMax(Value, GetInputLayer.GetInputMax);
end;

procedure TFFNN.SetOutputMax(const Value: Real);
begin
  FOutputMax:=Value;
  GetOutputLayer.SetOutputMinMax(GetOutputLayer.GetOutputMin, Value);
end;

procedure TFFNN.SetOutputMin(const Value: Real);
begin
  FOutputMin:=Value;
  GetOutputLayer.SetOutputMinMax(Value, GetOutputLayer.GetOutputMax);
end;

function TFFNN.CreateNLayer: TNLayer;
var
  LClass: TNLayerClass;
begin
  LClass := GetNLayerClass;
{
  if Assigned(FOnCreateNLayerClass) then
    FOnCreateNLayerClass(Self, LClass);
}
  Result := LClass.Create(NLayers);
end;

procedure TFFNN.InitWeights;
var
  i: Integer;
begin
  for i:=0 to NLayers.Count-1 do begin
    NLayers.Items[i].InitWeight;
  end;
  FOutLayer.InitWeight;
end;

function TFFNN.GetOutputCount: Integer;
begin
  Result:=Length(Output)-1;
end;

procedure TFFNN.SetOutputCount(const Value: Integer);
begin
  SetLength(Output, Value+1);
  SetLength(DesiredOutput, Value+1);
  if FOutLayer<> nil then FOutLayer.NeuronCount:=Value;//!!!
  if Length(Output)>0
    then Output[0]:=-1;
end;

function TNLayer.GetNeuronCount: Integer;
begin
  Result:=Length(o)-1;
end;

procedure TNLayer.SetNeuronCount(const Value: Integer);
begin
  SetLength(o, Value+1);
  o[0]:=-1;
  SetLength(v, Value+1);
end;

function TFFNN.AddLayer: TNLayer;
var
  NLayer: TNLayer;
begin
  NLayer:=NLayers.Add;
  ConnectLayers;
  NLayer.InitWeight;
  Result:=NLayer;
end;

function TFFNN.BackProp: Real;
var
  j: Integer;
  d: TNeuralVector;
begin
  CalcOut;
  SetLength(d, Length(Output));
  for j:=1 to Length(Output)-1 do
    d[j]:=DesiredOutput[j]-Output[j];
  FOutLayer.BackProp(d, FBPSpeed, FInertion);
  for j:=FNLayers.Count-1 downto 0 do
    FNLayers.Items[j].BackProp(d, FBPSpeed, FInertion);
  Result:=GetMaxError;
end;

function TFFNN.GetAvgError: Real;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to High(DesiredOutput) do
    Result:=Result+sqr(DesiredOutput[i]-Output[i]);
  Result:=sqrt(Result);
end;

function TFFNN.GetMaxError: Real;
var
  i: Integer;
begin
  Result:=0;
  for i:=1 to High(DesiredOutput) do
    if Abs(DesiredOutput[i]-Output[i]) > Result
      then Result:=Abs(DesiredOutput[i]-Output[i]);
end;

end.
