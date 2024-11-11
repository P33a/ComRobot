//type

// Global Variables Here
var
  delta_t: double;
  i_sense, i_lambda: double;

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


// this procedure is called when an "End of Packet" command is received
procedure Control;
var i, command_line: integer;
    Tau, A, Tau_C: double;
    Kp, Td, Kd: double;
begin
  if ButtonPressed('Calc') then begin  // Calc Pressed
    Tau := GetRCValue(14, 2);
    A := GetRCValue(15, 2);
    Tau_C := GetRCValue(16, 2);

    Kp := 1.0 / (A * Tau_C);
    Td := Tau;
    Kd := Kp * Td;
    SetRCValue(18, 2, format('%.3f', [Kp]));
    SetRCValue(19, 2, format('%.3f', [Td]));
    SetRCValue(20, 2, format('%.3f', [Kd]));
  end;
  //i_lambda := GetRCValue(14, 12);
  //i_sense := i_sense * 0.9 + 0.1 * GetRCValue(11, 11);
  //SetRCValue(11, 12, format('%.3f', [i_sense]));
end;

// this procedure is called once when the script is started
procedure Initialize;
begin
  delta_t := 0.04;
end;

