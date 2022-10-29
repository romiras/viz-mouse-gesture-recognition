
unit VizMakeTBL;

interface

uses
  SysUtils, Classes, Forms, Graphics, Controls, StdCtrls, ExtCtrls, Types,
  Math;

type
  TFormGesture = class(TForm)
    PaintBox: TPaintBox;
    pnlInfo: TPanel;
    Log: TMemo;
    PnlCommands: TPanel;
    btnSave: TButton;
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
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
    procedure SavePattern();
  public
    { Public declarations }
  end;

  { Store collected points }
  type
  PPointItem = ^APointList;
  APointList = record
    X: Integer;
    Y: Integer;
  end;

var
  FormGesture: TFormGesture;

implementation

{$R *.dfm}

var
  f: textfile;

const
  QPoints = 16;


{-----------------------------------------------------------------------------
  Description: Set the paintbox and load existing network
-----------------------------------------------------------------------------}
procedure TFormGesture.FormCreate(Sender: TObject);
begin
  With Paintbox.Canvas do
  begin
    Brush.color := clWhite;
    FillRect(ClipRect);
  end;

  FPointList := TList.Create;

  AssignFile(f, 'recognito.txt');
  {$I-}
  Append (f);
  {$I+}
  if IOresult <> 0 then
    Rewrite (f);
end;

{-----------------------------------------------------------------------------
  Description: Close the application and save the networkdata
-----------------------------------------------------------------------------}
procedure TFormGesture.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  CloseFile(f)
end;

{-----------------------------------------------------------------------------
  Description: Start to capture points by setting bDown to true. Clear paintbox first
-----------------------------------------------------------------------------}
procedure TFormGesture.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Reset state
  With Paintbox.Canvas do
  begin
    Brush.color := clWhite;
    FillRect (ClipRect);
  end;
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
  if FPointList.count > QPoints then
  begin
      AlignPoints;
      DrawPoints;
      ConvertPoints;
  end
  else
      Log.Lines.add('Not enough points. ' + IntToStr (Qpoints) + ' points required.');
end;

{-----------------------------------------------------------------------------
  Description: Capture X and Y position and add to list
-----------------------------------------------------------------------------}
procedure TFormGesture.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
    ARecord: PPointItem;
begin
   // Check if we are drawing
   if bDown then
   begin
      // Draw point
      With Paintbox.Canvas do
      begin
        pen.Color := clblack;
        Brush.Color := clBlue;
        Ellipse(x, y, x+4, y+4);
      end;

      // Add point to list
      New(ARecord);
      ARecord^.X := X;
      ARecord^.Y := Paintbox.Canvas.ClipRect.Bottom-Y;
      FPointList.Add(ARecord);
   end;
end;

{-----------------------------------------------------------------------------
  Description: Bring the pointslist down to QPoints points by smoothing the path
-----------------------------------------------------------------------------}
procedure TFormGesture.AlignPoints();
var
     d, d_min : double;
     PRecord, IRecord: PPointItem;
     p_min, p, i : integer;
begin
     log.Lines.add('Smoothing path');

{     // clean out points, closer to first point
     PRecord := FPointList.Items[0];
     i := 0;
     repeat
       inc (i);
       if i >= FPointList.count then
          break;
       IRecord := FPointList.Items[i];
     until sqrt(Sqr(PRecord^.X - IRecord^.X) + Sqr(PRecord^.y - IRecord^.y)) > 8;

     for p := 1 to i do
         FPointList.Delete(p);}

     while (FPointList.count > QPoints) do
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
         i := Pred (p_min);

         PRecord := FPointList.Items[p];
         IRecord := FPointList.Items[i];
         IRecord^.X := (PRecord^.x + IRecord^.x) div 2;
         IRecord^.Y := (PRecord^.Y + IRecord^.y) div 2;
         FPointList.Items[i] := IRecord;
         FPointList.Delete(p);
     end;
//     for p := 1 to 5 do
//         FPointList.Delete(p);
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
  for i:=0 to Pred (FPointList.count) do
  With Paintbox.Canvas do
  begin
      ARecord := FPointList.items[i];
      pen.Color := clblack;
      Ellipse(ARecord^.X-3, ARecord^.y-3, ARecord^.x+3, ARecord^.y+3);
      pen.Color := clGray;
      LineTo(ARecord^.X, ARecord^.Y);
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
    Paintbox.Canvas.Font.Name := 'Verdana';
    Paintbox.Canvas.Font.Size := 6;
    Paintbox.Canvas.Brush.Color := Clwhite;

    n:=0;
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
        if (pt2.x < 0) then
           m_sinuses[n] := - m_sinuses[n];
        angl := arccos(m_cosines[n]) * 180.0 / pi;
        Paintbox.Canvas.TextOut(ARecord^.X+3, Arecord^.Y+3, format('%d', [round(angl)]));
        inc(n);
    end;
end;

procedure TFormGesture.SavePattern;
var
   i : integer;
   ARecord: PPointItem;
begin
  for i:=0 to Pred (FPointList.count) do
  With Paintbox.Canvas do
  begin
      ARecord := FPointList.items[i];
      writeln(f, ARecord.X);
      writeln(f, ARecord.Y);
  end;
{   for i := 1 to QPoints do
   begin
     writeln(f, m_cosines[i]);
     writeln(f, m_sinuses[i]);
   end;}
end;

{-----------------------------------------------------------------------------
  Description: Saves the current getsure to the ini file
-----------------------------------------------------------------------------}
procedure TFormGesture.btnSaveClick(Sender: TObject);
begin
     SavePattern;
end;

end.
