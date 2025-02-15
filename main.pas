unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, TARadialSeries, sdpofpserial,
  SdpoJoystick, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, VerInfo, JSONPropStorage, Grids, Menus, Buttons, math, clipbrd,
  lNetComponents, lNet, WinSock2, gchannels, enum_serial, TAChartUtils, SynEdit, fgl,
  IniFiles, LCLType, gridsheet, Types, LCLIntf, ColorBox, PairSplitter, editor,
  dynmatrix;

const
  TermFileName = 'terminal.txt';
  ParsFileName = 'pars';
  ParsSize = 32;
  crlf = #13 + #10;


type

  { TChannelItem }

  TChannelItem = class
  private

  public
    Data: TChannelData;

    constructor Create;
    destructor Destroy; override;
  end;

  TChannelItemList = specialize TFPGObjectList<TChannelItem>;


  { TChannelConfig }

  TChannelConfig = class
  private

  public
    command: string;
    Row, Col: integer;
    Input, Output: boolean;
    ButtonRow, ButtonCol: integer;

    constructor Create;
    destructor Destroy; override;
  end;

  TChannelConfigList = specialize TFPGObjectList<TChannelConfig>;

  TRobotSpeeds = record
    V, Vn, W: single;
    solenoid: integer;
  end;

  TAxisConfig = record
    offset, index: integer;
    factor: double;
    channel: string;
  end;

  TButtonConfig = record
    on, off, toggle: integer;
    factor: double;
    channel: string;
  end;


  { TFMain }

  TFMain = class(TForm)
    BFileChoose: TButton;
    BClearSheet: TButton;
    BScriptRun: TButton;
    BJoySetConfig: TButton;
    BJoystickStop: TButton;
    BLoadSheet: TButton;
    BScriptStop: TButton;
    BSendRaw: TButton;
    BSendDirect: TButton;
    BSendSelection: TButton;
    BSendLine: TButton;
    BSetDt: TButton;
    BCloseComPort: TButton;
    BOpenComPort: TButton;
    BRefreshComPorts: TButton;
    BSaveLog: TButton;
    BAbout: TButton;
    BUDPConnect: TButton;
    BUDPDisconnect: TButton;
    BEditor: TButton;
    BChartRefresh: TButton;
    BSaveSheet: TButton;
    BJoystickStart: TButton;
    Button_ClearChart: TButton;
    CBCellIsProtected: TCheckBox;
    CBDeadManSwitch: TCheckBox;
    CBDTR: TCheckBox;
    CBRawSend: TCheckBox;
    CBRawRecv: TCheckBox;
    CBRTS: TCheckBox;
    Chart: TChart;
    CBChartFreeze: TCheckBox;
    CBComPort: TComboBox;
    CBCellCharted: TCheckBox;
    CBSaveSheetOnExit: TCheckBox;
    CBOmniJoy: TCheckBox;
    CBRemoteActions: TCheckBox;
    CBSerialReconnect: TCheckBox;
    CBLocalFilter: TCheckBox;
    CBAutoPing: TCheckBox;
    ChartUser: TChart;
    ColorBoxSeries: TColorBox;
    ColorDialog: TColorDialog;
    CBDecimalSeparator: TComboBox;
    ComboBoxJoystick: TComboBox;
    ComboReadMode: TComboBox;
    Edit1: TMenuItem;
    EditAutoPingMessage: TEdit;
    EditMaxPoints: TEdit;
    EditNumSeries: TEdit;
    EditEndOfPacketCommand: TEdit;
    EditReconnectDelay: TEdit;
    EditRobotVLabel: TEdit;
    EditRobotVnLabel: TEdit;
    EditRobotWLabel: TEdit;
    EditSolenoidLabel: TEdit;
    EditSendRaw: TEdit;
    EditSolenoidChannel: TEdit;
    EditAxisFactorV: TEdit;
    EditAxisChannelV: TEdit;
    EditAxisFactorVn: TEdit;
    EditAxisChannelVn: TEdit;
    EditAxisFactorW: TEdit;
    EditAxisChannelW: TEdit;
    EditAxisIndexV: TEdit;
    EditAxisIndexVn: TEdit;
    EditAxisIndexW: TEdit;
    EditButtonSolenoidOn: TEdit;
    EditAxisOffsetV: TEdit;
    EditAxisOffsetVn: TEdit;
    EditAxisOffsetW: TEdit;
    EditButtonSolenoidToggle: TEdit;
    EditButtonSolenoidOff: TEdit;
    EditDebugChannels: TEdit;
    EditErrorChannels: TEdit;
    EditSolenoid: TEdit;
    EditRobotV: TEdit;
    EditRobotVn: TEdit;
    EditRobotW: TEdit;
    EditSheetFilename: TEdit;
    EditReadChannel: TEdit;
    EditDt: TEdit;
    EditFormula: TEdit;
    EditVmax: TEdit;
    EditVnmax: TEdit;
    EditWmax: TEdit;
    EditSolenoidMax: TEdit;
    EditWriteChannel: TEdit;
    EditRobotX1: TEdit;
    EditLogName: TEdit;
    EditRemoteIP: TEdit;
    EditDefaultWrite: TEdit;
    EditDefaultWriteTriggerCol: TEdit;
    EditWriteTriggerRow: TEdit;
    EditWriteTriggerCol: TEdit;
    EditDefaultWriteTriggerRow: TEdit;
    Find1: TMenuItem;
    FontDialog: TFontDialog;
    GoTo1: TMenuItem;
    Joystick: TSdpoJoystick;
    JSONPropStorage: TJSONPropStorage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    EditPacketCount: TEdit;
    EditDebug: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MemoDebug: TMemo;
    MenuCopy: TMenuItem;
    MenuCut: TMenuItem;
    MenuDelete: TMenuItem;
    MenuDeleteAll: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemAddRow: TMenuItem;
    MenuItemDelRow: TMenuItem;
    MenuPaste: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PageControlGraphics: TPageControl;
    PairSplitter: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PasteSpecial1: TMenuItem;
    PopupMenuSheet: TPopupMenu;
    PopupMenuGrid: TPopupMenu;
    Repeatcommand1: TMenuItem;
    Replace1: TMenuItem;
    Serial: TSdpofpSerial;
    SETermAlt: TSynEdit;
    SGJoyState: TStringGrid;
    SGSheet: TStringGrid;
    ShapeScriptState: TShape;
    ShapeLEDJoy: TShape;
    SpeedButtonBackColor: TSpeedButton;
    SpeedButtonFont: TSpeedButton;
    SpeedButtonOK: TSpeedButton;
    SplitterTerminals: TSplitter;
    StatusBar: TStatusBar;
    SGRobot: TStringGrid;
    SETerm: TSynEdit;
    TabGrid: TTabSheet;
    TabConfig: TTabSheet;
    TabChannels: TTabSheet;
    TabJoystick: TTabSheet;
    TabMap: TTabSheet;
    TabUserChart: TTabSheet;
    TabTimeSeries: TTabSheet;
    TabTerminal: TTabSheet;
    Timer: TTimer;
    UDP: TLUDPComponent;
    UDPActions: TLUDPComponent;
    Undo1: TMenuItem;
    procedure BAboutClick(Sender: TObject);
    procedure BChartRefreshClick(Sender: TObject);
    procedure BClearSheetClick(Sender: TObject);
    procedure BCloseComPortClick(Sender: TObject);
    procedure BFileChooseClick(Sender: TObject);
    procedure BGoClick(Sender: TObject);
    procedure BJoySetConfigClick(Sender: TObject);
    procedure BJoystickStartClick(Sender: TObject);
    procedure BJoystickStopClick(Sender: TObject);
    procedure BLoadSheetClick(Sender: TObject);
    procedure BOpenComPortClick(Sender: TObject);
    procedure BRefreshComPortsClick(Sender: TObject);
    procedure BSaveLogClick(Sender: TObject);
    procedure BSaveSheetClick(Sender: TObject);
    procedure BScriptRunClick(Sender: TObject);
    procedure BScriptStopClick(Sender: TObject);
    procedure BSendDirectClick(Sender: TObject);
    procedure BSendLineClick(Sender: TObject);
    procedure BSendRawClick(Sender: TObject);
    procedure BSendSelectionClick(Sender: TObject);
    procedure BSetDtClick(Sender: TObject);
    procedure BUDPConnectClick(Sender: TObject);
    procedure BUDPDisconnectClick(Sender: TObject);
    procedure BEditorClick(Sender: TObject);
    procedure Button_ClearChartClick(Sender: TObject);
    procedure CBDTRChange(Sender: TObject);
    procedure CBRemoteActionsChange(Sender: TObject);
    procedure CBRTSChange(Sender: TObject);
    procedure ChartMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure EditFormulaChange(Sender: TObject);
    procedure EditFormulaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemAddRowClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemDelRowClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure SerialRxData(Sender: TObject);
    procedure SETermKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SGSheetDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SGSheetKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure SGParsGetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure SGParsValidateEntry(sender: TObject; aCol, aRow: Integer;
      const OldValue: string; var NewValue: String);
    procedure SGRobotClick(Sender: TObject);
    procedure SGRobotMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGSheetKeyPress(Sender: TObject; var Key: char);
    procedure SGSheetKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SGSheetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SGSheetMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpeedButtonBackColorClick(Sender: TObject);
    procedure SpeedButtonOKClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure UDPActionsError(const msg: string; aSocket: TLSocket);
    procedure UDPActionsReceive(aSocket: TLSocket);
    procedure UDPError(const msg: string; aSocket: TLSocket);
    procedure UDPReceive(aSocket: TLSocket);
    procedure FormCreate(Sender: TObject);
  private
    procedure CellsRectToStringList(CellsRect: TGridRect; SL: TStringList);
    procedure ChartRefresh;
    procedure Debug(s: string);
    procedure Debugln(s: string);
    procedure JoyStickConfig;
    function  LoadSheet(fname: string): boolean;
    procedure processActionsFrame(ChannelData: TChannelData; source: integer);
    procedure processFrame(ChannelData: TChannelData; source: integer);
    procedure RefreshChart(ChartCheckGroup: TCheckGroup);
    function SaveLog(FileName: string): string;
    procedure SeriesAddPoint(Series: TLineSeries; Y: double);
    procedure SeriesClear;
    procedure SetComState(newState: boolean);
    procedure StringListToCellsRect(SL: TStringList; CellsRect: TGridRect);
    { private declarations }
  public
    UDPPort: integer;
    UDPChannels, ActionChannels: TGChannels;
    SerialStateBeforeUp, SerialShouldReconnect: boolean;
    SerialConnectTimeTarget: QWord;
    UDPPingDecimator: QWord;

    packetCount: integer;
    packetTime: QWord;

    OutMessageList: string;

    SerialGChannels: TGChannels;
    ChannelItemList: TChannelItemList;

    ChannelConfigInList: TChannelConfigList;
    ChannelConfigOutList: TChannelConfigList;

    dt: double;

    FrameTime, LastFrameTime: QWord;
    delta : double;

    SeriesMaxPoints : integer;

    Sheet: TSheet;
    Last_x, Last_y: integer;
    Last_r, Last_c: integer;
    MouseIsDown: boolean;
    //Escaping: boolean

    curParEditRow: integer;
    SGRobotSelRow: integer;

    EndOfPacketCommand: string;

    ParIndex: integer;
    Pars: array [0..ParsSize - 1] of single;

    parsSent: integer;

    SpeedReq, SpeedMax: TRobotSpeeds;
    solenoid_toggle, prev_solenoid_toggle: integer;

    JoystickFileName: string;
    AxisConfigV, AxisConfigVn, AxisConfigW: TAxisConfig;
    ButtonActConfig: TButtonConfig;

    //procedure SaveToIni(ChannelConfigList: TChannelConfigList; fname: string);
    //procedure LoadFromIni(ChannelConfigList: TChannelConfigList; fname: string);

    procedure WriteCellData(r, c: integer);
    procedure ReadCellData(r, c: integer);
  end;

procedure SetRCValue(r, c: integer; s: string);
function GetRCValue(r, c: integer): double;
function GetRCText(r, c: integer): string;

function ButtonPressed(ButtonName: string): boolean;
function RCButtonPressed(r, c: integer): boolean;
procedure ClearButtons;
function RangeToMatrix(r, c, rows, cols: integer): Matrix;
procedure MatrixToRange(r, c: integer; const M: Matrix);
procedure MatrixToRangeF(r, c: integer; const M: Matrix; FormatString: string);

var
  FMain: TFMain;

procedure gMessageFloat(c: string; f: single);
procedure gMessageInt(c: string; i: integer);
procedure gMessageStr(c: string; s: string);

//function BuildMessage(c: char; val: integer): string;
procedure SendMessage(c: char; val: integer);
procedure SendRaw(s: string);

function AddLineSeries(aChart: TChart; aTitle: String; LineColor: TColor): TLineSeries;

implementation

{$R *.lfm}

procedure gMessageFloat(c: string; f: single);
begin
  FMain.OutMessageList += BuildMessage(c, f);
end;

procedure gMessageInt(c: string; i: integer);
begin
  FMain.OutMessageList += BuildMessage(c, i);
end;

procedure gMessageStr(c: string; s: string);
begin
  FMain.OutMessageList += BuildMessage(c, s);
end;


procedure SendMessage(c: char; val: integer);
var mess: string;
begin
  mess := c + IntToHex(dword(Val) and $FFFFFFFF, 8);
  with Fmain do begin
    if Serial.Active then serial.WriteData(mess);
    if UDP.Connected then UDP.SendMessage(mess, EditRemoteIP.Text);
  end;
end;

procedure SendRaw(s: string);
begin
  with Fmain do begin
    if Serial.Active then serial.WriteData(s);
    if UDP.Connected then UDP.SendMessage(s, EditRemoteIP.Text);
  end;
end;

function FilterZ(Y: double; U: double; z: double): double;
begin
  result := Y * (1 - z) + U * z;
end;

procedure SetRCValue(r, c: integer; s: string);
var cell: TSheetCell;
begin
  cell := FMain.sheet.EditCell(r, c);
  cell.ParseText(s);
end;


function GetRCValue(r, c: integer): double;
begin
  result := FMain.Sheet.Cell(r, c).value;
end;

function GetRCText(r, c: integer): string;
begin
  result := FMain.sheet.SGrid.Cells[c, r];
end;


function RCButtonPressed(r, c: integer): boolean;
var SheetCell: TSheetCell;
begin
  result := false;
  SheetCell := FMain.sheet.Cell(r, c);

  if SheetCell.CellType = ctButton then begin
    if SheetCell.value > 0 then begin
      result := true;
      SheetCell.value := 0;
    end;
  end;
end;

function ButtonPressed(ButtonName: string): boolean;
var i: integer;
    SheetCell: TSheetCell;
begin
  result := false;

  for i := 0 to FMain.sheet.CellList.Count -1 do begin
    SheetCell := FMain.sheet.CellList[i];
    Fmain.Debug(SheetCell.text);
    if SheetCell.CellType = ctButton then begin
      if (SheetCell.expression = ButtonName) and (SheetCell.value > 0) then begin
        result := true;
        SheetCell.value := 0;
        exit;
      end;
    end;
  end;
end;


procedure ClearButtons;
var i: integer;
    SheetCell: TSheetCell;
begin
  for i := 0 to FMain.sheet.CellList.Count -1 do begin
    SheetCell := FMain.sheet.CellList[i];
    if SheetCell.CellType = ctButton then begin
      SheetCell.value := 0;
    end;
  end;
end;


function RangeToMatrix(r, c, rows, cols: integer): Matrix;
var ir, ic: integer;
begin
  result := Mzeros(rows, cols);
  for ir := 0 to rows - 1 do begin
    for ic := 0 to cols - 1 do begin
      Msetv(result, ir, ic, FMain.sheet.Cell(r + ir, c + ic).value);
    end;
  end;
end;

procedure MatrixToRange(r, c: integer; const M: Matrix);
begin
  MatrixToRangeF(r, c, M, '%.7g');
end;


procedure MatrixToRangeF(r, c: integer; const M: Matrix; FormatString: string);
var ir, ic: integer;
begin
  for ir := 0 to M.rows - 1 do begin
    for ic := 0 to M.cols - 1 do begin
      FMain.sheet.EditCell(r + ir, c + ic).ParseText(format(FormatString, [MGetv(M, ir ,ic)]));
    end;
  end;
end;




{ TChannelConfig }

constructor TChannelConfig.Create;
begin

end;

destructor TChannelConfig.Destroy;
begin
  inherited Destroy;
end;


{ TChartVaalue }

constructor TChannelItem.Create;
begin

end;

destructor TChannelItem.Destroy;
begin
  inherited Destroy;
end;

{ TFMain }

procedure TFMain.Debug(s: string);
begin
  MemoDebug.Lines.Add(s);
  while MemoDebug.Lines.Count > 20 do begin
    MemoDebug.Lines.Delete(0);
  end;
end;

procedure TFMain.Debugln(s: string);
begin
  Debug(s + chr(10));
end;

{
function BuildMessage(c: char; val: integer): string;
begin
  result := c + IntToHex(dword(Val) and $FFFFFFFF, 8);
end;


function BuildMessage(c: char; f: single): string;
begin
  result := c + IntToHex((PDWord(@f))^ and $FFFFFFFF, 8);
end;

function BuildMessage(c: char; word_H: word; word_L: word): string;
begin
  result := c + IntToHex(dword((word_H shl 16) or word_L) and $FFFFFFFF, 8);
end;
}

procedure TFMain.RefreshChart(ChartCheckGroup: TCheckGroup);
var i: integer;
    cs: TLineSeries;
    //df: TSeriesDef;
    rcs: TBasicChartSeries;
begin
  {SeriesNameList.Clear;

  with CGChart.Items do begin

    // clear chart series
    with Chart do begin
      While Series.Count>0 do begin
        rcs := Series[0];
        RemoveSeries(Series[0]);
        rcs.Free;
      end;
    end;

    //cnt:=0;
    for i:=0 to Count-1 do begin
      if tree.Items[i].ImageIndex=1 then begin
        //Inc(cnt);

        // create new chart series for each variable
        cs:=TLineSeries.Create(FChart);
        cs.SeriesColor:=RandColor(i);
        cs.Title := GetNodePathText(tree, tree.Items[i]);
        df := TSeriesDef(tree.Items[i].Data);
        cs.Tag := PtrUInt(df);

        Chart.AddSeries(cs);
        SeriesNameList.Add(cs.Title);
      end;
    end;
  end;}
end;


procedure TFMain.processActionsFrame(ChannelData: TChannelData; source: integer);
var s: string;
begin
   if (ChannelData.command = 'up') then begin
    s := ChannelData.command + ': ' + ChannelData.text;
    s[1] := CharUpper(s[1]);
    SETerm.Lines.Add(s);
    EditErrorChannels.Text := s;

    if ChannelData.text = 'begin' then begin
      SerialStateBeforeUp := Serial.Active;
      if Serial.Active then begin
        SetComState(false);
      end;
    end else if ChannelData.text = 'end' then begin
      if CBSerialReconnect.checked then begin
        SerialShouldReconnect := true;
        SerialConnectTimeTarget := GetTickCount64() + StrToIntDef(EditReconnectDelay.text, 3000);
      end;
    end;
  end else if (ChannelData.command = 'alt') then begin
    if ChannelData.text = 'clear' then begin
      SETermAlt.Clear;
    end;
  end;


end;


procedure TFMain.processFrame(ChannelData: TChannelData; source: integer);
var i, c: integer;
    lambda: double;
    parValue: single;
    s: string;
    ChannelUpdate: boolean;
    ChannelItem: TChannelItem;
    CurCell: TSheetCell;

    channel: char; value: integer;
begin
  //Debugln(format('%s: %s (%.6g)', [ChannelData.command, ChannelData.text, ChannelData.value]));

  if PageControl.ActivePage = TabChannels then begin
    SGRobot.BeginUpdate();
    i := SGRobot.Cols[0].IndexOf(ChannelData.command);
    if i = -1 then begin
      i := SGRobot.Cols[0].IndexOf('');
    end;
    if (i > 0) and (i < SGRobot.RowCount) then begin
      SGRobot.Cells[0, i] := ChannelData.command;
      SGRobot.Cells[1, i] := format('%.6g', [ChannelData.value]);
    end;
    SGRobot.EndUpdate(false);
  end;


  if (ChannelData.command = 'err') or (ChannelData.command = 'msg') then begin
    s := ChannelData.command + ': ' + ChannelData.text;
    s[1] := CharUpper(s[1]);
    SETerm.Lines.Add(s);
    EditErrorChannels.Text := s;
    exit;
  end;

  // add this channel value to the current list
  ChannelUpdate := false;
  for i := 0 to ChannelItemList.Count - 1 do begin
    if ChannelItemList[i].Data.command = ChannelData.command then begin
      ChannelItemList[i].Data.Value := ChannelData.value;
      ChannelUpdate := true;
      break;
    end;
  end;
  if not ChannelUpdate then begin
    ChannelItem := TChannelItem.Create;
    ChannelItem.Data := ChannelData;
    ChannelItemList.Add(ChannelItem);
  end;

  if ChannelData.command = EndOfPacketCommand then begin //'loop'
    // Process the writes ChannelItemList
    for c := 0 to Sheet.CellList.Count - 1 do begin
      CurCell := Sheet.CellList[c];
      if (CurCell.WriteChannel <> '') and
         (sheet.Cell(CurCell.WriteTriggerRow, CurCell.WriteTriggerCol).value > 0) then begin
        OutMessageList += BuildMessage(CurCell.WriteChannel, CurCell.text);
      end;
      if (CurCell.WriteChannel <> '') and
         (CurCell.DefaultWrite <> '') and
         (sheet.Cell(CurCell.DefaultWriteTriggerRow, CurCell.DefaultWriteTriggerCol).value > 0) then begin
        OutMessageList += BuildMessage(CurCell.WriteChannel, CurCell.DefaultWrite);
      end;
    end;


    // if the joystick is active
    if Joystick.Active then begin
      OutMessageList += BuildMessage(AxisConfigV.channel, SpeedReq.V);
      OutMessageList += BuildMessage(AxisConfigW.channel, SpeedReq.W);
      if CBOmniJoy.Checked then OutMessageList += BuildMessage(AxisConfigVn.channel, SpeedReq.Vn);
      OutMessageList += BuildMessage(ButtonActConfig.channel, SpeedReq.solenoid);
    end;

    // If there are pending messages, send them and then clear the list
    if OutMessageList <> '' then begin
      SendRaw(OutMessageList);
      if CBRawSend.Checked then begin
        Debugln(OutMessageList);
      end;
      OutMessageList := '';
    end;

    // Now process the ChannelItemList
    //sheet.SGrid.BeginUpdate();
    for i := 0 to ChannelItemList.Count - 1 do begin
      ChannelItem := ChannelItemList[i];
      for c := 0 to Sheet.CellList.Count - 1 do begin
        CurCell := Sheet.CellList[c];
        if CurCell.ReadChannel = '' then continue;
        if CurCell.ReadChannel = ChannelItem.Data.command then begin
          if CurCell.ReadChanneldMode = rcmFloat then begin
            CurCell.value := ChannelItem.Data.value;
            CurCell.text := format('%.6g', [ChannelItem.Data.value]);
          end else if CurCell.ReadChanneldMode = rcmInteger then begin
            CurCell.value := StrToIntDef(ChannelItem.Data.text, 0);
            CurCell.text := format('%.6g', [ChannelItem.Data.value]);
          end else if CurCell.ReadChanneldMode = rcmString then begin
            CurCell.value := 0;
            CurCell.text := ChannelItem.Data.text;
          end;
          sheet.SGrid.Cells[CurCell.col, CurCell.row] := CurCell.text;

          // Add to chart if it is active
          //if not CBChartFreeze.Checked and (CurCell.Charted) and Assigned(CurCell.Series) then begin
          if not CBChartFreeze.Checked and Assigned(CurCell.Series) then begin
            SeriesAddPoint(CurCell.Series, CurCell.value);
          end;
        end;
      end;
    end;
    //sheet.SGrid.EndUpdate();

    // Empty ChannelItemList
    ChannelItemList.Clear;

    if FEditor.ProgRunning then begin  // Script controller
      FEditor.RunOnce; // One call to the script
      //if FEditor.CloseRequested then begin
      //   close;
      //   exit;
      //end;
    end;

    // Clear Button Presses
    for c := 0 to Sheet.CellList.Count - 1 do begin
      CurCell := Sheet.CellList[c];
      if CurCell.CellType = ctButton then CurCell.value := 0;
    end;
  end;

end;


procedure TFMain.JoyStickConfig;
begin
  AxisConfigV.offset := StrToInt(EditAxisOffsetV.Text);
  AxisConfigVn.offset := StrToInt(EditAxisOffsetVn.Text);
  AxisConfigW.offset := StrToInt(EditAxisOffsetW.Text);

  AxisConfigV.index:= StrToInt(EditAxisIndexV.Text);
  AxisConfigVn.index:= StrToInt(EditAxisIndexVn.Text);
  AxisConfigW.index := StrToInt(EditAxisIndexW.Text);

  AxisConfigV.factor:= StrToInt(EditAxisFactorV.Text);
  AxisConfigVn.factor:= StrToInt(EditAxisFactorVn.Text);
  AxisConfigW.factor := StrToInt(EditAxisFactorW.Text);

  AxisConfigV.channel:= EditAxisChannelV.Text;
  AxisConfigVn.channel:= EditAxischannelVn.Text;
  AxisConfigW.channel := EditAxischannelW.Text;

  ButtonActConfig.factor :=  StrToInt(EditSolenoidMax.Text);
  ButtonActConfig.on := StrToInt(EditButtonSolenoidOn.Text);
  ButtonActConfig.off := StrToInt(EditButtonSolenoidOff.Text);
  ButtonActConfig.toggle := StrToInt(EditButtonSolenoidToggle.Text);
  ButtonActConfig.channel := EditSolenoidChannel.Text;

  SpeedMax.V := StrToFloat(EditVmax.Text);
  SpeedMax.Vn := StrToFloat(EditVnmax.Text);
  SpeedMax.W := StrToFloat(EditWmax.Text);

  SpeedReq.V := StrToFloat(EditRobotV .Text);
  SpeedReq.Vn := StrToFloat(EditRobotVn .Text);
  SpeedReq.W := StrToFloat(EditRobotW.Text);
end;

procedure TFMain.FormShow(Sender: TObject);
var i: integer;
begin
  MakeFullyVisible();
  EndOfPacketCommand := EditEndOfPacketCommand.Text;
  if EndOfPacketCommand = '' then EndOfPacketCommand := 'loop';

  JoystickFileName := 'joystick.cfg';

  if FileExists(JoystickFileName) then begin
    SGJoyState.LoadFromFile(JoystickFileName);
  end;

  if FileExists(TermFileName) then begin
    SETerm.lines.LoadFromFile(TermFileName);
  end;

  SeriesMaxPoints := StrToIntDef(EditMaxPoints.Text, 1024);

  LoadSheet(EditSheetFilename.Text);
  JoyStickConfig();

  if CBRemoteActions.Checked then begin
    UDPActions.Listen(UDPActions.Port);
  end;

  if ShapeScriptState.Shape = stTriangleRight then FEditor.MenuRunClick(sender);
  dt := StrToFloatDefSep(EditDt.Text, 0.04);

  ChartRefresh();
end;


procedure TFMain.CellsRectToStringList(CellsRect: TGridRect; SL: TStringList);
var r, c: integer;
    s: string;
begin
  Sl.Clear;
  //SL.Add('sheet_selection');
  //SL.Add(inttostr(CellsRect.top));
  //SL.Add(inttostr(CellsRect.Left));
  //SL.Add(inttostr(CellsRect.bottom));
  //SL.Add(inttostr(CellsRect.Right));
  for r := CellsRect.top to CellsRect.bottom do begin
    s := '';
    for c := CellsRect.Left to CellsRect.Right do begin
      //SL.Add(Sheet.Cell(r, c).DisplayText_);
      s := s + Sheet.Cell(r, c).DisplayText_ + SL.Delimiter;
    end;
    SL.Add(s);
  end;
end;

procedure TFMain.StringListToCellsRect(SL: TStringList; CellsRect: TGridRect);
var r, c, i: integer;
    r_offset, c_offset: integer;
    tmpSL: TStringList;
begin
  r := CellsRect.top;
  c := CellsRect.Left;
  tmpSL := TStringList.Create;

  try try
    for r_offset := 0 to SL.Count - 1 do begin
      tmpSL.Clear;
      Split(SL[r_offset], tmpSL, chr(9));
      for c_offset := 0 to tmpSL.Count - 1 do begin
        Sheet.EditCell(r + r_offset, c + c_offset).ParseText(tmpSL[c_offset]);
      end;
    end;

    finally
      tmpSL.free;
    end;

    except on E: Exception do begin
      StatusBar.Panels[3].Text := E.message;
      exit;
    end;
  end;
end;


procedure TFMain.MenuItemAddRowClick(Sender: TObject);
begin
  //SGRobot.InsertRowWithValues(SGRobotSelRow,['','']);
  //SGSheet.InsertColRow(false, Last_r);
end;

procedure TFMain.MenuItemCopyClick(Sender: TObject);
var SL: TStringList;
begin
  SL := TStringList.Create;
  SL.Delimiter := chr(9);
  try
    CellsRectToStringList(SGSheet.Selection, SL);
    clipboard.AsText := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TFMain.MenuItemDelRowClick(Sender: TObject);
var SG: TStringGrid;
begin
  //SG := PopupMenuGrid.PopupComponent as TStringGrid;
  //sg.DeleteRow(SGRobotSelRow);
end;

procedure TFMain.MenuItemPasteClick(Sender: TObject);
var SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := clipboard.AsText;
    StringListToCellsRect(SL, Sheet.SGrid.Selection);
  finally
    SL.Free;
  end;
end;

procedure TFMain.SerialRxData(Sender: TObject);
var s, alt_s: string;
begin
  s := Serial.ReadData;
  if s = '' then exit;
  alt_s := SerialGChannels.ReceiveData(s);
  if alt_s <> '' then begin
    SETermAlt.Text := SETermAlt.Text + alt_s;
  end;
  if CBRawRecv.Checked then begin
    MemoDebug.Clear;
    Debugln(s);
  end;
  EditDebugChannels.Text := s;
end;

procedure TFMain.SETermKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_F9 then BSendLine.Click();
end;

procedure TFMain.SGSheetDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Grid: TStringGrid;
  sText: String;
  myBackColor: TColor;
  myTextColor: TColor;
  myAlignment: TAlignment;
  SheetCell: TSheetCell;
begin
  Grid := TStringGrid(Sender);
  SheetCell := Sheet.Cell(Arow, Acol);
  // set default values for the parameters,
  // get the values depending on the grid settings.
  sText := Grid.Cells[ACol, ARow];

  if SheetCell.CellType = ctButton then begin
    sText := SheetCell.expression;
    myTextColor := clBtnText;
    if SheetCell.CellButtonState = cstButtonDown then begin
      DrawFrameControl(Grid.Canvas.Handle, aRect, DFC_BUTTON, DFCS_BUTTONPUSH or DFCS_PUSHED);
      DrawButtonText(Grid.Canvas, aRect, sText, Font, myBackColor, myTextColor, true);
    end else begin
      DrawFrameControl(Grid.Canvas.Handle, aRect, DFC_BUTTON, DFCS_BUTTONPUSH);
      DrawButtonText(Grid.Canvas, aRect, sText, Font, myBackColor, myTextColor, false);
    end;
  end else if SheetCell.Charted and (SheetCell.ReadChannel <> '') then begin
    DrawChartDot(Grid.Canvas, aRect, SheetCell.SeriesColor);
  end;
end;

procedure TFMain.SGSheetKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = VK_RETURN then EditFormula.SetFocus;
end;

procedure TFMain.SGParsGetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  curParEditRow := ARow;
end;

procedure TFMain.SGParsValidateEntry(sender: TObject; aCol, aRow: Integer;
  const OldValue: string; var NewValue: String);
begin
  curParEditRow := -1;
end;

procedure TFMain.SGRobotClick(Sender: TObject);
begin
 // StatusBar.SimpleText :=  'clik';
end;

procedure TFMain.SGRobotMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var r, c: integer;
begin
  SGRobot.MouseToCell(X, Y, c, SGRobotSelRow);
  //SGRobotSelRow := SGRobot.MouseToCell();
  //StatusBar.SimpleText := 'Row: ' + IntToStr(SGRobotSelRow);
end;

procedure TFMain.SGSheetKeyPress(Sender: TObject; var Key: char);
begin
  // To begin editing when a key is pressed in the Grid
  if ord(key) < ord(' ') then exit;  // Filter non printable codes
  EditFormula.SetFocus;
  EditFormula.Text := key;
  EditFormula.SelStart := maxint;
end;

procedure TFMain.SGSheetKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  Last_c := Sheet.SGrid.Selection.Left;
  Last_r := Sheet.SGrid.Selection.Top;
  StatusBar.SimpleText := format('%3d: %3d',[Last_r, Last_c]);

  //EditFormula.Text := Sheet.Cell(Last_r, Last_c).Text;
  ReadCellData(Last_r, Last_c);
end;

procedure TFMain.SGSheetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i, r, c: integer;
    Grid: TStringGrid;
    SheetCell: TSheetCell;
    o: Tobject;
begin
  MouseIsDown := true;
  Last_x := x;
  Last_y := y;

  Grid:= TStringGrid(Sender);
  Grid.MouseToCell(x, y, c, r);
  StatusBar.SimpleText := format('%3d: %3d',[r, c]);

  if (c < 0) or (r < 0) then exit;  //  The user's right-click was not within a cell.
  WriteCellData(Last_r, Last_c);

  Last_r := r;
  Last_c := c;
  SheetCell := Sheet.Cell(r, c);

  // Button was pushed
  if SheetCell.CellType = ctButton then begin
    SheetCell.CellButtonState := cstButtonDown;
    SheetCell.value := SheetCell.value + 1;
    grid.Invalidate;
  end;

  //EditFormula.Text := SheetCell.Text;
  ReadCellData(Last_r, Last_c);
end;

procedure TFMain.SGSheetMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var Grid: TStringGrid;
    SheetCell: TSheetCell;
begin
  MouseIsDown := false;
  Grid:= TStringGrid(Sender);
  SheetCell := Sheet.Cell(Last_r, Last_c);

  if SheetCell.CellType = ctButton then begin
    SheetCell.CellButtonState := cstButtonUp;
    Grid.Invalidate;
  end;

end;

procedure TFMain.SpeedButtonBackColorClick(Sender: TObject);
var SheetCell: TSheetCell;
begin
  SheetCell := Sheet.EditCell(Last_r, Last_c);
  if not Assigned(SheetCell.Series) then exit;
  ColorDialog.Color := SheetCell.Series.SeriesColor;
  if ColorDialog.Execute then
    SheetCell.Series.SeriesColor := ColorDialog.Color;
end;

procedure TFMain.SpeedButtonOKClick(Sender: TObject);
begin
  // 1st way on accepting an edited formula
  //Sheet.EnterFormula(Last_r, Last_c, EditFormula.Text);
  WriteCellData(Last_r, Last_c);
end;

procedure TFMain.TimerTimer(Sender: TObject);
var i: integer;
begin
  if UDP.Connected then begin
    if GetTickCount64() - packetTime < 1000 then begin
      EditRemoteIP.Color := clGreen
    end else begin
      EditRemoteIP.Color := clYellow;

      if CBAutoPing.Checked and (UDPPingDecimator mod 20 = 0) then begin
        UDPPort := UDP.Port + ComboBoxJoystick.ItemIndex;
        UDP.SendMessage(EditAutoPingMessage.Text, EditRemoteIP.Text);
      end;
      inc(UDPPingDecimator);
    end;
  end else EditRemoteIP.Color := clRed;

  if Serial.Active then begin
    CBComPort.Color := clGreen;
  end else CBComPort.Color := clRed;

  // Check if we should reopen the serial port after a firmware upload
  if CBSerialReconnect.checked and SerialShouldReconnect and (GetTickCount64() > SerialConnectTimeTarget) then begin
    SetComState(SerialStateBeforeUp);
    SerialShouldReconnect := false;
  end;

  if FEditor.ProgRunning then begin
    ShapeScriptState.Brush.Color := clGreen;
    ShapeScriptState.Shape := stTriangleRight;
  end else begin
    ShapeScriptState.Brush.Color := clRed;
    ShapeScriptState.Shape := stSquare;
  end;

  if not Joystick.Active then begin
    ShapeLEDJoy.Brush.Color := clRed;
    exit;
  end;

  ShapeLEDJoy.Brush.Color := clGreen;

  SGJoyState.BeginUpdate;
  for i := 0 to MaxButtons - 1 do begin
    SGJoyState.Cells[1, 1 + i] := IntToStr(Joystick.Buttons[i]);
  end;
  for i := 0 to MaxWinAxis - 1 do begin
    SGJoyState.Cells[3, 1 + i] := IntToStr(Joystick.Axis[i]);
  end;
  SGJoyState.EndUpdate();

  if not CBDeadManSwitch.Checked or (Joystick.Buttons[5] <> 0) then begin
    SpeedReq.V := AxisConfigV.factor * SpeedMax.V * (Joystick.Axis[AxisConfigV.index] + AxisConfigV.Offset) / 32768.0;
    SpeedReq.W := AxisConfigW.factor * SpeedMax.W * (Joystick.Axis[AxisConfigW.index] + AxisConfigW.Offset) / 32768.0;
    if CBOmniJoy.Checked then begin
      SpeedReq.Vn := AxisConfigVn.factor * SpeedMax.Vn * (Joystick.Axis[AxisConfigVn.index] + AxisConfigVn.Offset) / 32768.0;
    end;
    if Joystick.Buttons[ButtonActConfig.on] <> 0 then SpeedReq.solenoid := round(ButtonActConfig.factor);
    if Joystick.Buttons[ButtonActConfig.off] <> 0 then SpeedReq.solenoid := 0;

    prev_solenoid_toggle := solenoid_toggle;
    solenoid_toggle := Joystick.Buttons[ButtonActConfig.toggle];
    if (solenoid_toggle <> 0) and (prev_solenoid_toggle = 0) then begin
      SpeedReq.solenoid := round(ButtonActConfig.factor * ord(SpeedReq.solenoid = 0));
    end;
  end else begin
    SpeedReq.V := 0;
    SpeedReq.Vn:= 0;
    SpeedReq.W := 0;
  end;

  EditRobotV.Text := format('%.3f', [SpeedReq.V]);
  EditRobotVn.Text := format('%.3f', [SpeedReq.Vn]);
  EditRobotW.Text := format('%.3f', [SpeedReq.W]);
  EditSolenoid.Text := IntToStr(SpeedReq.solenoid);

end;

procedure TFMain.UDPActionsError(const msg: string; aSocket: TLSocket);
begin
  UDPActions.Disconnect();
  StatusBar.SimpleText := msg;
end;

procedure TFMain.UDPActionsReceive(aSocket: TLSocket);
var s: string;
begin
  UDPActions.GetMessage(s);
  if CBLocalFilter.Checked and (aSocket.PeerAddress <> '127.0.0.1') then exit;
  if s <> '' then begin
    packetTime := GetTickCount64();
    inc(packetCount);
    EditPacketCount.Text := IntToStr(packetCount);

    ActionChannels.ReceiveData(s);
    if CBRawRecv.Checked then begin
      MemoDebug.Clear;
      Debugln(s);
    end;
    EditDebugChannels.Text := s;
  end;
end;
procedure TFMain.BSaveLogClick(Sender: TObject);
begin
  StatusBar.SimpleText := SaveLog(EditLogName.text + '.txt');
end;

procedure TFMain.BSaveSheetClick(Sender: TObject);
begin
  sheet.SaveToFile(EditSheetFilename.Text);
end;

procedure TFMain.BScriptRunClick(Sender: TObject);
begin
  //if not FEditor.Visible then FEditor.Show;
  FEditor.MenuRunClick(Sender);
end;

procedure TFMain.BScriptStopClick(Sender: TObject);
begin
  //if not FEditor.Visible then FEditor.Show;
  FEditor.MenuStopClick(Sender);
end;

procedure TFMain.BSendDirectClick(Sender: TObject);
begin
  Serial.WriteData(EditSendRaw.Text);
end;

procedure TFMain.BSendLineClick(Sender: TObject);
var s: string;
    i: integer;
begin
  i := SETerm.CaretY - 1; // - 1 ????
  if i < 0 then exit;
  //SETermAlt.Lines.Add(SETerm.Lines[i]);
  OutMessageList += SETerm.Lines[i];
end;

procedure TFMain.BSendRawClick(Sender: TObject);
begin
  OutMessageList += EditSendRaw.Text;
end;

procedure TFMain.BSendSelectionClick(Sender: TObject);
begin
  if not SETerm.SelAvail then exit;
  OutMessageList += SETerm.SelText;
end;

procedure TFMain.BSetDtClick(Sender: TObject);
begin
  dt := StrToFloat(EditDt.Text);
end;

procedure TFMain.BChartRefreshClick(Sender: TObject);
begin
  ChartRefresh();
end;

procedure TFMain.BClearSheetClick(Sender: TObject);
begin
  Sheet.Clear();
end;


procedure TFMain.ChartRefresh;
var c: integer;
    CurCell: TSheetCell;
    num: integer;
begin
  num := 0;
  Chart.Series.Clear;
  for c := 0 to Sheet.CellList.Count - 1 do begin
    CurCell := Sheet.CellList[c];
    CurCell.Series := nil;
    if not CurCell.Charted then continue;
    if CurCell.ReadChannel = '' then continue;
    CurCell.Series := AddLineSeries(Chart, CurCell.ReadChannel, CurCell.SeriesColor);
    inc(num);
  end;
  EditNumSeries.Text := IntToStr(num);
  SeriesMaxPoints := StrToIntDef(EditMaxPoints.Text, 1024);
end;


procedure TFMain.BAboutClick(Sender: TObject);
begin
  ShowMessage('ComRobot v' + InfoData[3] + crlf + crlf+
              'Copyright (C) 2023-24 Paulo Costa' + crlf + crlf+
              'Compiled: ' + DateToStr(FileDateToDateTime(FileAge(Application.ExeName))));
end;

procedure TFMain.BCloseComPortClick(Sender: TObject);
begin
  SetComState(false);
end;


procedure TFMain.BGoClick(Sender: TObject);
var i: integer;
begin
  for i := 0 to Chart.Series.Count - 1 do begin
    (Chart.Series.Items[i] as TLineSeries).Clear;
  end;

  CBChartFreeze.Checked := false;
end;

procedure TFMain.BJoySetConfigClick(Sender: TObject);
begin
  JoyStickConfig();
end;

procedure TFMain.BJoystickStartClick(Sender: TObject);
begin
  if ComboBoxJoystick.ItemIndex = 0 then begin
    Joystick.DeviceWin := dwJoystickID1;
    Joystick.Active := true;
  end else if ComboBoxJoystick.ItemIndex = 1 then begin
    Joystick.DeviceWin := dwJoystickID2;
    Joystick.Active := true;
  end;

  if UDP.Active then begin  //refresh the connection
    BUDPDisconnectClick(Sender);
    BUDPConnectClick(Sender);
  end;
end;

procedure TFMain.BJoystickStopClick(Sender: TObject);
begin
  Joystick.Active := false;
end;

function TFMain.LoadSheet(fname: string): boolean;
var c: integer;
begin
  if FileExists(fname) then begin
    sheet.LoadFromFile(fname);
    StatusBar.SimpleText := sheet.LastError;// 'Loaded sheet: ' + fname;
    for c := 0 to Sheet.CellList.Count - 1 do begin
      if Sheet.CellList[c].CellType = ctButton then Sheet.CellList[c].value := 0;
    end;
    result := true;
  end else begin
    StatusBar.SimpleText := 'File: ' + fname + ' does not exist!';
    result := false;
  end;
end;

procedure TFMain.BFileChooseClick(Sender: TObject);
var Filename: string;
begin
  if OpenDialog.Execute then begin
    Filename := OpenDialog.FileName;
    if ExtractFilePath(FileName) = GetCurrentDir() + PathDelim then begin
      Filename := ExtractFileName(Filename);
    end;
    if LoadSheet(FileName) then begin
      EditSheetFilename.Text := Filename;
    end;
  end;
end;


procedure TFMain.BLoadSheetClick(Sender: TObject);
begin
  LoadSheet(EditSheetFilename.Text);
end;

procedure TFMain.BOpenComPortClick(Sender: TObject);
begin
  SetComState(true);
end;

procedure TFMain.BRefreshComPortsClick(Sender: TObject);
begin
  ListComPorts(CBComPort.Items);
  if CBComPort.Items.Count > 0 then begin
    CBComPort.ItemIndex := CBComPort.Items.Count - 1;
  end;
end;

procedure TFMain.SetComState(newState: boolean);
begin
  try
  if newState then begin
    Serial.Device := CBComPort.Text;
    Serial.Open();
    if not Serial.Active then
      raise Exception.Create('Could not open port ' + Serial.Device);
    //SerSetParams(SerialHandle, StrToInt(EditSerialBaudrate.Text), 8, NoneParity, 1, []);
    //Serial.SetDTR(true);
    Serial.SetRTS(CBRTS.Checked);
    Serial.SetDTR(CBDTR.Checked);
  end else begin
    Serial.Close();
  end;
  finally
    if Serial.Active then CBComPort.Color := clGreen
    else CBComPort.Color := clRed;
  end;
end;

{
procedure TFMain.SaveToIni(ChannelConfigList: TChannelConfigList; fname: string);
var Ini: TMemIniFile ;
    section: string;
    Config: TChannelConfig;
    i: integer;
begin
  Ini := TMemIniFile.Create('channels_in.ini');
  try
    for i := 0 to ChannelConfigList.Count - 1 do begin
      Config := ChannelConfigList[i];
      section := 'i' + IntToStr(i);
      Ini.WriteString(section, 'Command', Config.command);
      Ini.WriteInteger(section, 'Row', Config.Row);
      Ini.WriteInteger(section, 'Col', Config.Col);
      Ini.WriteBool(section, 'Input', Config.Input);
      if Config.Output then begin
        Ini.WriteInteger(section, 'ButtonRow', Config.ButtonRow);
        Ini.WriteInteger(section, 'ButtonCol', Config.ButtonCol);
        Ini.WriteBool(section, 'Output', Config.Output);
      end;
    end;
  finally
    Ini.Free;
  end;
end;


procedure TFMain.LoadFromIni(ChannelConfigList: TChannelConfigList; fname: string);
var Ini: TMemIniFile ;
    section: string;
    Config: TChannelConfig;
    Sections: TStringList;
    i: integer;
begin
  if not FileExists('channels_in.ini') then exit;

  Sections := TStringList.Create;
  Ini := TMemIniFile.Create('channels_in.ini');

  try
    ChannelConfigList.Clear;
    Ini.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      Config := TChannelConfig.Create;
      section := Sections[i];
      Config.command := Ini.ReadString(section, 'Command', Config.command);
      Config.Row := Ini.ReadInteger(section, 'Row', Config.Row);
      Config.Col := Ini.ReadInteger(section, 'Col', Config.Col);
      Config.Input := Ini.ReadBool(section, 'Input', Config.Input);
      Config.Output := Ini.ReadBool(section, 'Output', Config.Output);
      Config.ButtonRow := Ini.ReadInteger(section, 'ButtonRow', Config.ButtonRow);
      Config.ButtonCol := Ini.ReadInteger(section, 'ButtonCol', Config.ButtonCol);
    end;
  finally
    Ini.Free;
    Sections.Free;
  end;

end;
}
procedure TFMain.WriteCellData(r, c: integer);
var cell: TSheetCell;
begin
  cell := sheet.EditCell(r, c);
  cell.isProtected := CBCellIsProtected.Checked;
  if cell.isProtected then begin
    ReadCellData(r, c);
    StatusBar.SimpleText := 'Cell is protected!';
    exit;
  end;
  cell.ParseText(EditFormula.Text);

  cell.ReadChannel := EditReadChannel.Text;
  cell.ReadChanneldMode := TReadChanneldMode(ComboReadMode.ItemIndex);

  cell.WriteChannel := EditWriteChannel.Text;
  cell.WriteTriggerRow := StrToIntDef(EditWriteTriggerRow.Text, 0);
  cell.WriteTriggerCol := StrToIntDef(EditWriteTriggerCol.Text, 0);

  cell.DefaultWrite := EditDefaultWrite.Text;
  cell.DefaultWriteTriggerRow := StrToIntDef(EditDefaultWriteTriggerRow.Text, 0);
  cell.DefaultWriteTriggerCol := StrToIntDef(EditDefaultWriteTriggerCol.Text, 0);

  cell.Charted := CBCellCharted.Checked and (cell.ReadChannel <> '');
  cell.SeriesColor := ColorBoxSeries.Selected;
end;


procedure TFMain.ReadCellData(r, c: integer);
var cell: TSheetCell;
    s: string;
begin
  cell := sheet.EditCell(r, c);
  EditFormula.Text := cell.text;
  EditReadChannel.Text := cell.ReadChannel;
  ComboReadMode.ItemIndex := ord(cell.ReadChanneldMode);

  EditWriteChannel.Text := cell.WriteChannel;

  s := '';
  if cell.WriteTriggerRow >= 1 then s := IntToStr(cell.WriteTriggerRow);
  EditWriteTriggerRow.Text := s;

  s := '';
  if cell.WriteTriggerCol >= 1 then s := IntToStr(cell.WriteTriggerCol);
  EditWriteTriggerCol.Text := s;

  EditDefaultWrite.Text := cell.DefaultWrite;

  s := '';
  if cell.DefaultWriteTriggerRow >= 1 then s := IntToStr(cell.DefaultWriteTriggerRow);
  EditDefaultWriteTriggerRow.Text := s;

  s := '';
  if cell.DefaultWriteTriggerCol >= 1 then s := IntToStr(cell.DefaultWriteTriggerCol);
  EditDefaultWriteTriggerCol.Text := s;

  CBCellCharted.Checked := cell.Charted;
  ColorBoxSeries.Selected := cell.SeriesColor;

  CBCellIsProtected.Checked := cell.isProtected;
end;

{
procedure ClearCells(CellsRect: TGridRect; ClearFlags: TClearFlagsSet);
var r, c: integer;
//    SheetCell: TSheetCell;
begin
  for r := CellsRect.top to CellsRect.bottom do begin
    for c := CellsRect.Left to CellsRect.Right do begin
      if cfFormulas in ClearFlags then
        FSheets.ActSheet.EditCell(r, c).ParseText('');
      if cfFormat in ClearFlags then
        FSheets.ActSheet.EditCell(r, c).backColor := clWindow;
    end;
  end;
end;
}

procedure TFMain.BUDPConnectClick(Sender: TObject);
begin
  UDPPort := UDP.Port + ComboBoxJoystick.ItemIndex;
  if not UDP.Connected then begin
    UDP.Listen(UDPPort);
  end;
  {
  // https://stackoverflow.com/questions/34242622/windows-udp-sockets-recvfrom-fails-with-error-10054
  BOOL bNewBehavior = FALSE;
        DWORD dwBytesReturned = 0;
        WSAIoctl(clientSock, SIO_UDP_CONNRESET, &bNewBehavior, sizeof bNewBehavior, NULL, 0, &dwBytesReturned, NULL, NULL);
  }
  //WSAIoctl(UDP.Socks[0], SIO_UDP_CONNRESET, &bNewBehavior, sizeof bNewBehavior, NULL, 0, &dwBytesReturned, NULL, NULL);
  UDP.SendMessage(EditAutoPingMessage.Text, EditRemoteIP.Text);
end;

procedure TFMain.BUDPDisconnectClick(Sender: TObject);
begin
  if UDP.Connected then begin
    UDP.Disconnect();
  end;
end;

procedure TFMain.BEditorClick(Sender: TObject);
begin
  //StatusBar.SimpleText := IntToStr(SGRobot.Columns.Count);
  FEditor.Show;
end;

procedure TFMain.Button_ClearChartClick(Sender: TObject);
begin
  SeriesClear();
end;

procedure TFMain.CBDTRChange(Sender: TObject);
begin
  Serial.SetDTR(CBDTR.Checked);
end;

procedure TFMain.CBRemoteActionsChange(Sender: TObject);
begin
  if CBRemoteActions.Checked then begin
    if not UDPActions.Connected then UDPActions.Listen(UDPActions.Port);
  end else begin
    UDPActions.Disconnect();
  end;
end;

procedure TFMain.CBRTSChange(Sender: TObject);
begin
  Serial.SetRTS(CBRTS.Checked);
end;

procedure TFMain.ChartMouseMove(Sender: TObject; Shift: TShiftState; X,  Y: Integer);
var GP: TDoublePoint;
begin
  //if not Chart.ScalingValid then exit;
  //GP := Chart.ImageToGraph(point(X, Y));
  //StatusBar.SimpleText := format('%.4f, %.4f', [GP.x, GP.y]);
end;

procedure TFMain.EditFormulaChange(Sender: TObject);
begin
  if CBCellIsProtected.Checked then exit; // Avoid editing protected cells
  SGSheet.Cells[last_c, last_r] := EditFormula.Text;
end;

procedure TFMain.EditFormulaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var SelRect: TGridRect;
begin
  if key in [VK_RETURN, VK_DOWN, VK_UP] then begin // 2nd way of accepting an edited formula
    key := 0;
    WriteCellData(Last_r, Last_c);
    Sheet.SGrid.SetFocus;

    if (key = VK_DOWN) then begin
      SelRect := Sheet.SGrid.Selection;
      if SelRect.Top < Sheet.SGrid.RowCount then begin
        SelRect.Top := SelRect.Top + 1;
        SelRect.Bottom := SelRect.Bottom + 1;
        Sheet.SGrid.Selection := SelRect;
      end;
    end;

  end else if key = VK_ESCAPE then begin
    Sheet.SGrid.SetFocus;
  end;
end;


procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if UDP.Connected then begin
    UDP.Disconnect();
  end;

  if Serial.Active then begin
     Serial.Close;
  end;

  SETerm.Lines.SaveToFile(TermFileName);

  if CBSaveSheetOnExit.Checked then sheet.SaveToFile(EditSheetFilename.Text);
end;

procedure TFMain.SeriesClear;
var i: integer;
begin
  for i := 0 to Chart.Series.Count - 1 do begin
    (Chart.Series[i] as TLineSeries).Clear;
  end;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  ActionChannels.Free;
  UDPChannels.Free;
  SerialGChannels.Free;

  ChannelItemList.Free;
  ChannelConfigInList.Free;
  ChannelConfigOutList.Free;
  Sheet.Free;
end;

procedure TFMain.UDPError(const msg: string; aSocket: TLSocket);
var lasterror: integer;
begin
  lasterror := WSAGetLastError();
  if lasterror = 10054 then exit;  // Ignore send error
  // https://stackoverflow.com/questions/34242622/windows-udp-sockets-recvfrom-fails-with-error-10054

  UDP.Disconnect();
  StatusBar.SimpleText := msg;
end;

procedure TFMain.UDPReceive(aSocket: TLSocket);
var s, alt_s: string;
begin
  UDP.GetMessage(s);
  if s <> '' then begin

    packetTime := GetTickCount64();
    inc(packetCount);
    EditPacketCount.Text := IntToStr(packetCount);

    //UDPChannels.ReceiveData(s);
    alt_s := UDPChannels.ReceiveData(s);
    if alt_s <> '' then begin
      SETermAlt.Text := SETermAlt.Text + alt_s;
    end;

    if CBRawRecv.Checked then begin
      MemoDebug.Clear;
      Debugln(s);
    end;
    EditDebugChannels.Text := s;

  end;
end;

procedure TFMain.FormCreate(Sender: TObject);
begin
  // Lazarus catches WM_SETTINGCHANGE and calls Application.IntfSettingChange
  // which calls GetFormatSettings in SysUtils
  // It can be switched off by setting Application.UpdateFormatSettings := False;
  {$IFDEF WINDOWS}
  Application.UpdateFormatSettings := false;
  {$ENDIF}
  DefaultFormatSettings.DecimalSeparator := '.';

  GetVersionInfo();
  Caption := 'ComRobot v' + InfoData[3];

  ChannelItemList := TChannelItemList.Create();
  ChannelConfigInList := TChannelConfigList.Create();
  ChannelConfigOutList := TChannelConfigList.Create();

  UDPChannels := TGChannels.Create(@processFrame);
  packetCount := 0;

  ActionChannels := TGChannels.Create(@processActionsFrame);

  SerialGChannels := TGChannels.Create(@processFrame);

  FrameTime := GetTickCount64();
  LastFrameTime := FrameTime;
  SeriesMaxPoints := 1024;

  curParEditRow := -1;

  Sheet := TSheet.Create;
  Sheet.SGrid := SGSheet;
  Sheet.FillHeaders();
end;


function AddLineSeries(aChart: TChart; aTitle: String; LineColor: TColor): TLineSeries;
begin
  Result := TLineSeries.Create(AChart.Owner);
  with TLineSeries(Result) do
  begin
    // Series title for the legend
    Title := aTitle;
    // Show data point markers (red fill color, black border)
    ShowPoints := false;
    //Pointer.Brush.Color := clRed;
    //Pointer.Pen.Color := clBlack;
    //Pointer.Style := psCircle;
    // Show red line segments connecting the data points
    ShowLines := true;
    LinePen.Style := psSolid;
    SeriesColor := LineColor;
  end;
  // Add new series to the chart
  AChart.AddSeries(Result);
end;



procedure TFMain.SeriesAddPoint(Series: TLineSeries; Y: double);
var i: integer;
begin
  Series.BeginUpdate;
  Series.Add(Y);
  while Series.Count > SeriesMaxPoints do begin
    Series.Delete(0);
  end;
  for i := 0 to Series.Count - 1 do begin
    Series.SetXValue(i, i);
  end;
  Series.EndUpdate;
end;

function TFMain.SaveLog(FileName: string): string;
var i, c, NumPoints: integer;
    SL: TStringList;
    FormatSettings: TFormatSettings;
    s: string;
begin
  FormatSettings := DefaultFormatSettings;
  FormatSettings.TimeSeparator := '_';
  i := CBDecimalSeparator.ItemIndex;
  if i = 0 then FormatSettings.DecimalSeparator := '.';
  if 1 = 1 then FormatSettings.DecimalSeparator := ',';

  result := 'Could not save log file: ' + FileName;
  if Chart.Series.Count = 0 then exit;
  SL := TStringList.Create;
  try
    NumPoints := (Chart.Series[0] as TLineSeries).Count;

    for i := 0 to NumPoints - 1 do begin
      s := format('%g', [i * dt]);
      for c := 0 to Chart.Series.Count - 1 do begin
        s := s + format(' %g', [(Chart.Series[c] as TLineSeries).YValue[i]]);
      end;
      SL.Add(s);
    end;
    //SL.SaveToFile(EditLogName.text + DateTimeToStr(now(), FormatSettings) + '.txt');
    SL.SaveToFile(FileName);
    result := 'Saved log file: ' + FileName;
    SL.Clear;
  finally
    SL.Free;
    FormatSettings := DefaultFormatSettings;
  end;
end;


end.

