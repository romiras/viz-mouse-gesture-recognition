
unit VizMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Math, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, FFNN, IniFiles;

type
  TFormGesture = class(TForm)
    PaintBox: TPaintBox;
    pnlInfo: TPanel;
    FFNN: TFFNN;
    Log: TMemo;
    PnlCommands: TPanel;
    txtLearn: TEdit;
    lblTrain: TLabel;
    btnTrain: TButton;
    pnlText: TPanel;
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure txtLearnKeyPress(Sender: TObject; var Key: Char);
    procedure btnTrainClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    bDown : boolean;

    { FPointList stores all the points collected in a PPointItem }
    FPointList: Tlist;

    { Store calculated cosines and sinuses }
    m_cosines : array[0..15] of real;
    m_sinuses : array[0..15] of real;

    procedure AlignPoints();
    procedure DrawPoints();
    procedure ConvertPoints();
    procedure RecognizePattern();
    procedure Train(z : integer);
  public
    { Public declarations }
  end;

  { Store collected points }
  PPointItem = ^APointList;
  APointList = record
    X: Integer;
    Y: Integer;
  end;

var
  FormGesture: TFormGesture;

implementation

{$R *.dfm}

const MAXERR=0.01;

{-----------------------------------------------------------------------------
  Description: Set the paintbox and load existing network
-----------------------------------------------------------------------------}
procedure TFormGesture.FormCreate(Sender: TObject);
var path: string;
begin
  Paintbox.Canvas.Brush.color := clWhite;
  Paintbox.Canvas.FillRect(PaintBox.Canvas.ClipRect);
  FPointList := TList.Create;
{  path:=extractfilepath(application.ExeName) + 'network.ffn';
  if fileexists(path) then
     FFNN.LoadFromFile(path);}
end;

{-----------------------------------------------------------------------------
  Description: Close the application and save the networkdata
-----------------------------------------------------------------------------}
procedure TFormGesture.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//     FFNN.SaveToFile(extractfilepath(application.ExeName) + 'network.ffn');
end;

{-----------------------------------------------------------------------------
  Description: Start to capture points by setting bDown to true. Clear paintbox first
-----------------------------------------------------------------------------}
procedure TFormGesture.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Reset state
  Paintbox.Canvas.Brush.color := clWhite;
  Paintbox.Canvas.FillRect(PaintBox.Canvas.ClipRect);
  FPointList.Clear;
  // Clear log
  log.Clear;
  log.Lines.add('Ready to receive points');
  // Set bDown
  bDown := true;
end;

{-----------------------------------------------------------------------------
  Description: User stopped making a gesture. Check if enough points are provided
               and process those points
-----------------------------------------------------------------------------}
procedure TFormGesture.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Stop drawing
  bDown := false;
  // Check for min number of points
  if FPointList.count > 16 then
  begin
      AlignPoints;
      DrawPoints;
      ConvertPoints;
      RecognizePattern;
  end
  else
      Log.Lines.add('Not enough points. 16 points required.');

end;

{-----------------------------------------------------------------------------
  Description: Capture X and Y position and add to list
-----------------------------------------------------------------------------}
procedure TFormGesture.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
    ARecord: PPointItem;
begin
   // Check if we are draing
   if bDown then
   begin
      // Draw point
      Paintbox.Canvas.pen.Color := clblack;
      Paintbox.Canvas.Brush.Color := clBlue;
      Paintbox.Canvas.Ellipse(x, y, x+4, y+4);

      // Add point to list
      New(ARecord);
      ARecord^.X := X;
      ARecord^.Y := Y;
      FPointList.Add(ARecord);
   end;

end;

{-----------------------------------------------------------------------------
  Description: Bring the pointslist down to 16 points by smoothing the path
-----------------------------------------------------------------------------}
procedure TFormGesture.AlignPoints();
var
     d, d_min : double;
     PRecord, IRecord: PPointItem;
     p_min, p, i : integer;
begin
     log.Lines.add('Smoothing path');
     while (FPointList.count > 16) do
     begin
    	   d_min := 10000000;
        p_min := 1;

        p := p_min;
        PRecord := FPointList.Items[p];
        i := p_min;
        inc(i);

        while i <> FPointList.count do
        begin
             IRecord := FPointList.Items[i];
             d := sqrt(Sqr(PRecord^.X - IRecord^.X) + Sqr(PRecord^.y - IRecord^.y));
             if (d < d_min) then
             begin
                 d_min := d;
                 p_min := p;
             end;

             p := i;
             PRecord := FPointList.Items[p];
             inc(i);
         end;

         p := p_min;
         i := p_min-1;

         PRecord := FPointList.Items[p];
         IRecord := FPointList.Items[i];
         IRecord^.X := round((PRecord^.x + IRecord^.x) / 2);
         IRecord^.Y := round((PRecord^.Y + IRecord^.y) / 2);
         FPointList.Items[i] := IRecord;
         FPointList.Delete(p);

     end;

end;

{-----------------------------------------------------------------------------
  Description: Draw the points in the pointslist on the paintbox
-----------------------------------------------------------------------------}
procedure TFormGesture.DrawPoints();
var
  i : integer;
  ARecord: PPointItem;
begin
  log.lines.add('Drawing points');
  ARecord := FPointList.items[0];
  Paintbox.Canvas.Brush.Color := clRed;
  Paintbox.Canvas.MoveTo(ARecord^.X, ARecord^.Y);
  PaintBox.Canvas.Ellipse(ARecord^.X-4, ARecord^.y-4, ARecord^.x+8, ARecord^.y+8);
  for i:=0 to FPointList.count-1 do
  begin
      ARecord := FPointList.items[i];
      Paintbox.Canvas.pen.Color := clblack;
      Paintbox.Canvas.Ellipse(ARecord^.X-3, ARecord^.y-3, ARecord^.x+3, ARecord^.y+3);
      Paintbox.Canvas.pen.Color := clGray;
      Paintbox.Canvas.LineTo(ARecord^.X, ARecord^.Y);
  end;

end;

{-----------------------------------------------------------------------------
  Description: Convert the points to sinus and cosinus values and calculate the angle.
               Show this angle on the screen
-----------------------------------------------------------------------------}
procedure TFormGesture.ConvertPoints();
var
     i,n: integer;
     ARecord, BRecord : PPointItem;
     angl : real;
     pt1, pt2 :TPoint;
begin
    log.lines.add('Converting points');
    n:=0;
    Paintbox.Canvas.Font.Name := 'Verdana';
    Paintbox.Canvas.Font.Size := 6;
    Paintbox.Canvas.Brush.Color := Clwhite;
    for i:=0 to FPointList.Count - 2 do
    begin
        ARecord := FPointList.Items[i];
        BRecord := FPointList.Items[i+1];

        pt1.X := BRecord^.X;
        pt1.Y := BRecord^.Y;
        pt2.X := ARecord^.X;
        pt2.Y := ARecord^.Y;

       	pt2.x := pt2.x - pt1.x;
      	pt2.y := pt2.Y - pt1.y;

      	m_cosines[n] := pt2.y / sqrt(sqr(pt2.x) + sqr(pt2.y));
          m_sinuses[n] := sqrt(1. - sqr(m_cosines[n]));
        if (pt2.x < 0) then m_sinuses[n] := - m_sinuses[n];
        angl := (arccos(m_cosines[n]) * 180. / pi);
        Paintbox.Canvas.TextOut(ARecord^.X+3, Arecord^.Y+3, format('%d', [round(angl)]));
        inc(n);
    end;
end;

{-----------------------------------------------------------------------------
  Description: Feed the cosinus and sinus values to the FFNN and determine the max output neuron
-----------------------------------------------------------------------------}
procedure TFormGesture.RecognizePattern;
var
   i, v : integer;
   maxv:real;
begin
   // Calculate the output
   FFNN.CalcOut;

   log.lines.add(format('%.5f', [FFNN.Output[1]]));
   txtLearn.SetFocus;
end;

{-----------------------------------------------------------------------------
  Description: Train a gesture by using a key
-----------------------------------------------------------------------------}
procedure TFormGesture.txtLearnKeyPress(Sender: TObject; var Key: Char);
var
   z : integer;
begin
   // Convert character to number. A is 1 etc
   z := ord(key) - 96;

   // Train gesture
   Train(z);

   // Disregard key and set the focus back
   key := #0;
   txtLearn.SetFocus;
end;

{-----------------------------------------------------------------------------
  Description: Train the network
-----------------------------------------------------------------------------}
procedure TFormGesture.Train(z: integer);
var
   i : integer;
begin
   for i:=1 to 2 do
      FFNN.DesiredOutput[i] := 0;
      
   FFNN.DesiredOutput[z] := 1;

   // Train
   FFNN.BackProp;
end;

{-----------------------------------------------------------------------------
  Description: Train the network by feeding the samples
-----------------------------------------------------------------------------}
procedure TFormGesture.btnTrainClick(Sender: TObject);
var
  i, a: integer;
  count: word;
  Pat: TStringList;
begin
     Pat:=TStringList.Create;
     try
        Pat.LoadFromFile('recognito.txt');

        if Pat.Count<>0 then
        begin
             count:=StrToInt(Pat.Strings[0]);
             repeat
                  for a:=1 to count do
                  begin
                       for i:=1 to 16 do
                       begin
                            FFNN.Input[i]:=StrToFloat(Pat.Strings[a*i]);
                            FFNN.Input[i+16]:=StrToFloat(Pat.Strings[a*(i+16)]);
                       end;
                       Train(1);
                  end;
                  log.Clear;

                  pnlText.caption := format('Gesture : Ready training. Error: %.5f', [FFNN.GetMaxError]);
                  Application.ProcessMessages;
            until FFNN.GetmaxError < MAXERR;
        end;
     finally
        Pat.Free;
     end;
     log.Lines.add('Trained');
end;

end.
