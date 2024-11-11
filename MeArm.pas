const
  NumServos = 4;

type
  TRCServo = record
    pulse: integer;
    angle, target_angle, delta: double;
    centerPulse, pulsePerDeg: double;
    maxAngle, minAngle: double;
  end;

  TRobotCommand = record
    row, col: integer;
    name: string;
    cur_time, target_time: double;
    running: boolean;
  end;

// Global Variables Here
var
  Servos: array [0..NumServos - 1] of TRCServo;

  Command: TRobotCommand;
  delta_t: double;

function max(a, b: double): double;
begin
  result := a;
  if b > a then result := b;
end;

function min(a, b: double): double;
begin
  result := a;
  if b < a then result := b;
end;


procedure ReadCalib;
var i: integer;
begin
  for i := 0 to NumServos - 1 do begin
    Servos[i].centerPulse := GetRCValue(4, 2 + i);
    Servos[i].pulsePerDeg := GetRCValue(5, 2 + i);
    Servos[i].maxAngle := GetRCValue(3, 9 + i);
    Servos[i].minAngle := GetRCValue(4, 9 + i);
  end;
end;

function AngleToPulse(Servo: TRCServo; Angle: double): double;
begin
  result := Servo.centerPulse + Servo.pulsePerDeg * Angle;
end;


procedure InitCommand(var cmd: TRobotCommand);
var i: integer;
begin
  // Fill new command
  cmd.name := LowerCase(GetRCText(cmd.row, cmd.col));

  cmd.cur_time := 0;
  cmd.target_time := GetRCValue(cmd.row, cmd.col + NumServos + 1);

  for i := 0 to NumServos - 1 do begin
    if Command.name = 'ap' then begin
      Servos[i].target_angle := max(min(GetRCValue(cmd.row, cmd.col + 1 + i), Servos[i].maxAngle), Servos[i].minAngle);
    end else if Command.name = 'am' then begin
      Servos[i].target_angle := max(min(GetRCValue(cmd.row, cmd.col + 1 + i), Servos[i].maxAngle), Servos[i].minAngle);
      Servos[i].delta := delta_t * (Servos[i].target_angle - Servos[i].angle) / cmd.target_time;
    end;
  end;

  cmd.running := true;
end;


procedure ProcessCommand(var cmd: TRobotCommand);
var i: integer;
begin
  if not Command.running then exit;

  // Have we ended the current command
  if cmd.cur_time > cmd.target_time then begin  // Goto next Command
    cmd.cur_time := cmd.cur_time - cmd.target_time;
    inc(cmd.row);
    InitCommand(cmd);
    if not (Command.name in ['am', 'ap']) then begin
      Command.running := false;
      exit;
    end;
  end;

  // Update the angle acording to the command
  for i := 0 to NumServos - 1 do begin
    if Command.name = 'am' then begin
      Servos[i].angle := Servos[i].angle + Servos[i].delta;
      //Servos[i].angle := Servos[i].target_angle;
    end else if Command.name = 'ap' then begin
      Servos[i].angle := Servos[i].target_angle;
    end;

    Servos[i].Pulse := round(AngleToPulse(Servos[i], Servos[i].angle));
    //gMessageInt('d' + inttostr(i), round(Servos[i].delta * Servos[i].pulsePerDeg));
    gMessageInt('s' + inttostr(i), Servos[i].Pulse);
    SetRCValue(5, 9 + i, format('%d', [round(Servos[i].delta * Servos[i].pulsePerDeg)]));
  end;

end;

// this procedure is called periodicaly (typically: 40 ms)
procedure Control;
var i, command_line: integer;
    v, delta_v: double;
    ang_update: boolean;
begin
  if ButtonPressed('Calib') then begin  // Calib Pressed
    ReadCalib();
  end;

  ang_update := false;
  delta_v := GetRCValue(7, 1);
  for i := 2 to 5 do begin
    if RCButtonPressed(7, i) then begin
      v := GetRCValue(6, i) + delta_v;
      SetRCValue(6, i, format('%g', [v]));
      ang_update := true;
    end;
    if RCButtonPressed(8, i) then begin
      v := GetRCValue(6, i) - delta_v;
      SetRCValue(6, i, format('%g', [v]));
      ang_update := true;
    end;
  end;

  if ButtonPressed('Set Ang') or ang_update then begin  // Set angle for all Servos
    for i := 0 to NumServos - 1 do begin
      Servos[i].angle := GetRCValue(6, 2 + i);
      Servos[i].Pulse := round(AngleToPulse(Servos[i], Servos[i].angle));
      gMessageInt('s' + inttostr(i), Servos[i].Pulse);

      SetRCValue(3, 2 + i, format('%d', [Servos[i].Pulse]));
    end;
  end;

    // Start trajectory
  if ButtonPressed('Play1') then begin
    command_line := 10;
    Command.row := round(GetRCValue(command_line, 2));
    Command.col := round(GetRCValue(command_line, 3));
    InitCommand(Command);
  end;

  if ButtonPressed('Play2') then begin
    command_line := 10;
    Command.row := round(GetRCValue(command_line, 9));
    Command.col := round(GetRCValue(command_line, 10));
    InitCommand(Command);
  end;

  if Command.running then begin // Process trajectory
    ProcessCommand(Command);
    Command.cur_time := Command.cur_time + delta_t;
    SetRCValue(10, 5, IntToStr(command.row));
  end;

end;

// this procedure is called once when the script is started
procedure Initialize;
begin
  delta_t := 0.02;
  ReadCalib();
  Command.running := false;
end;

