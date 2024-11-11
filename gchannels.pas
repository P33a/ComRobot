unit gchannels;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

const
  TEXCHANNELS_BUF_IN_SIZE = 1024;

type
{
typedef struct {
  char* command;
  float value;
  char* text;
  int index;
  int command_is(const char* c);
} frame_data_t;}

{ TChannelData }

TChannelData = record
   command, text, index_str, array_command: string;
   value: single;
   index: integer;
 end;

 TProcessChannel = procedure(ChannelData: TChannelData; source: integer) of object;

 TGChannelState = (tcs_wait_for_command, tcs_reading_data);

{ Tgchannels }

TGChannels = class
private
  procedure ProcessChar(c: char);
public
  serialData: string;
  buffer: array[0..TEXCHANNELS_BUF_IN_SIZE - 1] of char;
  count: integer;
  frame_command, frame_text: pchar;

  ChannelData: TChannelData;
  state: TGChannelState;

  ProcessChannel: TProcessChannel;

  function ReceiveData(s: string): string;


  constructor Create(newProcessChannel: TProcessChannel);
  destructor Destroy; override;
end;


function BuildMessage(c: string; f: single): string;
function BuildMessage(c: string; i: integer): string;
function BuildMessage(c: string; s: string): string;


implementation

function IsAlpha(c: char): boolean;
begin
  result := c in ['a'..'z', 'A'..'Z', '_'];
end;

function IsAlphaNumeric(c: char): boolean;
begin
  result := c in ['0'..'9', 'a'..'z', 'A'..'Z', '_'];
end;

function IsNumber(c: char): boolean;
begin
  result := c in ['0'..'9'];
end;

function BuildMessage(c: string; f: single): string;
begin
  result := c + ' ' + format('%.6g', [f]) + ';'
end;

function BuildMessage(c: string; i: integer): string;
begin
  result := c + ' ' + format('%d', [i]) + ';'
end;

function BuildMessage(c: string; s: string): string;
begin
  // no escaping for space or ';', just avoid it!
  result := c + ' ' + s + ';'
end;




{ TGChannels }

constructor TGChannels.Create(newProcessChannel: TProcessChannel);
begin
  state := tcs_wait_for_command;

  ProcessChannel:= newProcessChannel;
end;

destructor TGChannels.Destroy;
begin
  inherited Destroy;
end;

procedure TGChannels.ProcessChar(c: char);
var i: integer;
    s: string;
    mask: TFPUExceptionMask;
begin
  //b := ord(c);
  if (state = tcs_wait_for_command) and isalpha(c) then begin // A command allways starts with a letter
    state := tcs_reading_data;
    buffer[0] := c;
    count := 1;

  end else if (state = tcs_reading_data) and (c = chr($08)) then begin // BS (Backspace key received)
    if (count > 0) then begin
      dec(count);
      buffer[count] := chr(0);
    end;

  end else if (state = tcs_reading_data) and ((c = chr($0A)) or (c = chr($0D)) or (c = ';')) then begin // LF or CR (enter key received) or ';'
    // Now we can process the buffer
    if (count <> 0) then begin
      buffer[count] := chr(0); // Guarantee to null terminate the string
      frame_command := buffer; // The command starts at the begining

      // Find the first space to separate the command from the text/value
      frame_text := buffer;
      while (frame_text^ <> chr(0)) do begin
        if (frame_text^ = ' ') or (frame_text^ = ':') then begin  // If the first space found or a ':'
          frame_text^ := chr(0); // Command ends here;
          inc(frame_text);    // "text" starts here
          while (frame_text^ = ' ') do inc(frame_text); // or not, we should skip spaces
          break;  // And we can stop scanning
        end;
        inc(frame_text);
      end;

      ChannelData.command := frame_command;
      ChannelData.text := frame_text;

      // Now test if there is an array index in the command
      {while(1)
        if (!isdigit(*(frame.index_text - 1))) break;
        if (frame.index_text <= buffer) break;  // this should never happen
        frame.index_text--;
      }

      ChannelData.index := 0;
      ChannelData.array_command := ChannelData.command;
      i := Length(ChannelData.command);
      while i > 0 do begin
        if (not IsNumber(ChannelData.command[i])) then break;
        dec(i);
      end;

      // If there is an array index in the command then extract the index value
      if (i < Length(ChannelData.command)) and (i > 0) then begin
        ChannelData.index_str := copy(ChannelData.command, i + 1, MaxInt);
        ChannelData.index := StrToIntDef( ChannelData.index_str, 0);
        ChannelData.array_command := copy(ChannelData.command, 1, i);
      end;

      // for the numerical parameter try to get the value from the text
      mask := GetExceptionMask();
      SetExceptionMask(mask + [exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
      ChannelData.value := StrToFloatDef(frame_text, 0);
      ClearExceptions(false);
      SetExceptionMask(mask);

      if Assigned(ProcessChannel) then begin  // If "process_command" is not null
        ProcessChannel(ChannelData, 0); // Do something with the pair (command, value)
      end;

      // Reset the buffer
      count := 0;
      FillChar(buffer[0], sizeof(buffer), 0);
      ChannelData.text := buffer;
    end;
    state := tcs_wait_for_command;


  end else if (state = tcs_reading_data) and (count < TEXCHANNELS_BUF_IN_SIZE - 1) then begin // A new char can be read
    buffer[count] := c;  // Store char in the buffer
    inc(count);
  end;

end;

function TGChannels.ReceiveData(s: string): string;
var i, b: integer;
    alt_s: string;
begin
  if s = '' then exit;

  alt_s := '';
  for i := 1 to Length(s) do begin
    b := ord(s[i]);
    if b < 128 then begin
      ProcessChar(s[i]);
    end else begin
      alt_s := alt_s + chr((b and $7F));
    end;
  end;

  result := alt_s;
end;

end.

