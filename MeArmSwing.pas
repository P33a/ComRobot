const
  NumServos = 4;

type
  TRCServo = record
    pulse: integer;
    angle: double;
    centerPulse, pulsePerDeg: double;
  end;

  TRobotCommand = record
    line: integer;
    name: string;
    time, target_time: double;
  end;

// Global Variables Here
var
  pulse, delta: integer;
  Servos, Si, Sf: array [0..NumServos - 1] of TRCServo;

  Command: TRobotCommand;

procedure ReadCalib;
var i: integer;
begin
  for i := 0 to NumServos - 1 do begin
    Servos[i].centerPulse := GetRCValue(4, 2 + i);
    Servos[i].pulsePerDeg := GetRCValue(5, 2 + i);
  end;
end;

function AngleToPulse(Servo: TRCServo; Angle: double): double;
begin
  result := Servo.centerPulse + Servo.pulsePerDeg * Angle;
end;

procedure Swing;
begin
  SetRCValue(4, 10, format('%d', [pulse]));
  pulse := pulse + delta;
  if (delta > 0) and (pulse >  2000) then begin
    delta := -delta;
  end else if (delta < 0) and (pulse <  1000) then begin
    delta := -delta;
  end;
  gMessageInt('s0', pulse);
end;


// this procedure is called periodicaly (default: 40 ms)
procedure Control;
var i: integer;
begin
  if RCButtonPressed(4, 6) then begin  // Calib Pressed
    ReadCalib();
  end;

  if RCButtonPressed(6, 1) then begin
    for i := 0 to NumServos - 1 do begin
      Servos[i].angle := GetRCValue(6, 2 + i);
      Servos[i].Pulse := round(AngleToPulse(Servos[i], Servos[i].angle));
      gMessageInt('s' + inttostr(i), Servos[i].Pulse);

      SetRCValue(3, 2 + i, format('%d', [Servos[i].Pulse]));
    end;
  end;
  //gMessageInt('s0', pulse);

  if Command.line > 0 then begin

  end;

end;

// this procedure is called once when the script is started
procedure Initialize;
begin
 //SetRCValue(4, 10, 'hello world');
  ReadCalib();
  pulse := 1000;
  delta := 50;
end;

