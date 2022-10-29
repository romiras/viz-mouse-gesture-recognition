{-----------------------------------------------------------------------------
            Gesture is a demo application to show you how to implement
            gesture recognision with a neural network in Delphi.

            It works by taking 16 points and by calculating the cosinus and sinus values
            of two corresponding points and feeding these values to the network.
            The FFNN determines the outputs and the highest output is most likely the
            character you want to recognised.

            You can train a gesture by drawing it and typing its corresponding letter in
            the box next to train:
            To save this gesture, press save and enter the character.
            The train button loads the gestures.ini file and trains the network.
            With Show, you can see a gesture in the gestures.ini file.
-----------------------------------------------------------------------------}
unit MainForm;

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, IniFiles,
  GestureTypes, PointsOp, FFNN;

type
  TFormGesture = class(TForm)
    PaintBox: TPaintBox;
    pnlInfo: TPanel;
    Log: TMemo;
    PnlCommands: TPanel;
    txtLearn: TEdit;
    lblTrain: TLabel;
    btnSave: TButton;
    btnTrain: TButton;
    btnShow: TButton;
    pnlText: TPanel;
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure txtLearnKeyPress(Sender: TObject; var Key: Char);
    procedure btnSaveClick(Sender: TObject);
    procedure btnTrainClick(Sender: TObject);
    procedure btnShowClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    bDown : boolean;
    procedure DrawPoints();
    procedure Recognize;

  public
    { Public declarations }
    procedure OutConv (CRecord: PPointItem; angle: real);
  end;

var
  FormGesture: TFormGesture;

implementation
uses Math;

{$R *.dfm}

var
   NetFile: string;

{-----------------------------------------------------------------------------
  Description: Set the paintbox and load existing network
-----------------------------------------------------------------------------}
procedure TFormGesture.FormCreate(Sender: TObject);
begin
     FPointList := TList.Create;
     NetFile := extractfilepath(application.ExeName) + 'network.ffn';
     if fileexists(NetFile) then
        Net.LoadFromFile(NetFile);

     Paintbox.Canvas.Brush.color := clWhite;
     Paintbox.Canvas.FillRect(PaintBox.Canvas.ClipRect);
end;

procedure TFormGesture.FormDestroy(Sender: TObject);
begin
     // save the networkdata
     Net.SaveToFile(NetFile);
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

procedure TFormGesture.OutConv (CRecord: PPointItem; angle: real);
begin
     Paintbox.Canvas.TextOut(CRecord^.X+3, CRecord^.Y+3, format('%d', [round(angle)]));
end;

{-----------------------------------------------------------------------------
  Description: Draw the points in the pointslist on the paintbox
-----------------------------------------------------------------------------}
procedure TFormGesture.DrawPoints();
var
  i : integer;
  ARecord: PPointItem;
begin
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

procedure TFormGesture.Recognize;
var
   i, v : integer;
   maxv: real;
begin
      log.Lines.add('Smoothing path');
      AlignPoints;

      log.lines.add('Drawing points');
      DrawPoints;

      log.lines.add('Converting points');
      with Paintbox.Canvas do
      begin
           Font.Name := 'Verdana';
           Font.Size := 6;
           brush.Color := clWhite;
      end;
      ConvertPoints;
      
      RecognizePoints (v);

      for i:=1 to 28 do
          log.lines.add(format('%s: %.5f', [chr(i+96), Net.Output[i]]));

      maxv := Net.Output[v];
      if maxv > ErrGain then
         log.Lines.add(format('Highest output is %f, which is the %dthe output neuron. Value is %s', [maxv, v, chr(v+96)]))
      else
         log.Lines.add('No letters match.');

      // Update textbar
      if v < 27 then
         pnlText.Caption := pnlText.Caption +chr(v+96)
      else if v = 27 then
         pnlText.Caption := pnlText.Caption +' '
      else if v = 28 then
         pnlText.Caption := copy(pnlText.Caption, 0, length(pnlText.caption)-1);

      txtLearn.SetFocus;
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
  if FPointList.count -1 > 15 then
     Recognize
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
   log.Lines.add('Trained');


   // Disregard key and set the focus back
   key := #0;
   txtLearn.SetFocus;
end;

{-----------------------------------------------------------------------------
  Description: Saves the current getsure to the ini file
-----------------------------------------------------------------------------}
procedure TFormGesture.btnSaveClick(Sender: TObject);
var
  AppIni: TIniFile;
  a : string;
  i : integer;
  ARecord: PPointItem;
begin
  // open ini file in the application dir
  AppIni := TIniFile.Create(extractfilepath(application.ExeName) + 'GESTURES.INI');

  // Check if we have enough points
  if FPointList.count -1 = 15 then
  begin
     // Ask the character to save
     a := Inputbox('Save gesture', 'Please enter the letter', '');
     // Write points to file
     for i:=0 to FPointList.count -1 do
     begin
        ARecord := FPointList.items[i];
        AppIni.WriteInteger(a, 'x'+inttostr(i), ARecord^.x);
        AppIni.WriteInteger(a, 'y'+inttostr(i), ARecord^.y);
     end;
  end
  else
     ShowMessage('No data to save');

  // Release ini file
  AppIni.Free;

end;

{-----------------------------------------------------------------------------
  Description: Train the network by feeding the samples
-----------------------------------------------------------------------------}
procedure TFormGesture.btnTrainClick(Sender: TObject);
var
  AppIni: TIniFile;
  i, a: integer;
  ARecord: PPointItem;
begin
  // Open ini file
  AppIni := TIniFile.Create(extractfilepath(application.ExeName) + 'GESTURES.INI');
             
  repeat
   // Load each character and train it
   for a:=1 to 28 do
   begin
     if AppIni.SectionExists(chr(a+96)) then
     begin
        FPointList.Clear;
        for i:=0 to 15 do
        begin
           new(ARecord);
           Arecord^.X := AppIni.ReadInteger(chr(a+96), 'x'+inttostr(i), 0);
           Arecord^.Y := AppIni.ReadInteger(chr(a+96), 'y'+inttostr(i), 0);
           FPointList.Add(ARecord);
        end;
        Paintbox.Canvas.Brush.color := clWhite;
        Paintbox.Canvas.FillRect(PaintBox.Canvas.ClipRect);
        AlignPoints;
        DrawPoints;
        ConvertPoints;
        Train(a);
        log.Clear;
     end;
   end;
   log.Clear;
   // Show max error
   caption := format('Gesture : Ready training. Error: %.5f', [Net.GetMaxError]);
   Application.ProcessMessages;
   // Continue till error is less then 0.01
  until Net.GetmaxError < 0.01;

  // Release ini
  AppIni.Free;

end;

{-----------------------------------------------------------------------------
  Description: Draw selected character
-----------------------------------------------------------------------------}
procedure TFormGesture.btnShowClick(Sender: TObject);
var
  AppIni: TIniFile;
  i: integer;
  ARecord: PPointItem;
  a : string;
begin
  // Open ini file
  AppIni := TIniFile.Create(extractfilepath(application.ExeName) + 'GESTURES.INI');

  // Ask which character
  a := inputbox('Show gesture', 'Enter the letter', 'a');

     if AppIni.SectionExists(a) then
     begin
        FPointList.Clear;
        // Load points
        for i:=0 to 15 do
        begin
           new(ARecord);
           Arecord^.X := AppIni.ReadInteger(a, 'x'+inttostr(i), 0);
           Arecord^.Y := AppIni.ReadInteger(a, 'y'+inttostr(i), 0);
           FPointList.Add(ARecord);
        end;
        Paintbox.Canvas.Brush.color := clWhite;
        Paintbox.Canvas.FillRect(PaintBox.Canvas.ClipRect);
        // Draw the points
        DrawPoints;
     end;

  // Release ini file
  AppIni.Free;

end;

end.
