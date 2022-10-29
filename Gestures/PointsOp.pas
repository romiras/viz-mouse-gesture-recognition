unit PointsOp;

interface
uses Classes;

type
  { Store collected points }
  PPointItem = ^APointList;
  APointList = record
    X: Integer;
    Y: Integer;
  end;

var
    { FPointList stores all the points collected in a PPointItem }
    FPointList: Tlist;

procedure AlignPoints();
procedure ConvertPoints();

implementation
uses
    Windows, Math,
    //MainForm,
    GestureTypes;

{-----------------------------------------------------------------------------
  Description: Bring the pointslist down to 16 points by smoothing the path
-----------------------------------------------------------------------------}
procedure AlignPoints();
var
     d, d_min : double;
     PRecord, IRecord: PPointItem;
     p_min, p, i : integer;
begin
     while (FPointList.count-1 > 15) do
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
		         d := sqrt(power(PRecord^.X - IRecord^.X, 2) + power(PRecord^.y - IRecord^.y, 2));
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
  Description: Convert the points to sinus and cosinus values and calculate the angle.
               Show this angle on the screen
-----------------------------------------------------------------------------}
procedure ConvertPoints();
var
     i,n: integer;
     ARecord, BRecord : PPointItem;
     angl : real;
     pt1, pt2 :TPoint;

begin
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

      	m_cosines[n] := (pt2.y / sqrt(Sqr(pt2.x) + Sqr(pt2.y)));
        m_sinuses[n] := sqrt(1.0 - Sqr(m_cosines[n]));
        if (pt2.x < 0) then m_sinuses[n] := - m_sinuses[n];
        angl := (arccos(m_cosines[n]) * 180.0 / pi);

        //FormGesture.OutConv (ARecord, angl);

        inc(n);
    end;
end;

end.
