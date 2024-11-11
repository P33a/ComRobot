unit gridsheet;

{$mode objfpc}{$H+}

{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, fgl, Graphics, Grids, math, laz2_DOM, laz2_XMLRead,
  laz2_XMLWrite, TATypes, TASeries;


type
  TSheet = class;
  //TSheetCellList = class;

  TCellType = (ctText, ctFormula, ctButton);
  TCellButtonState = (cstButtonUp, cstButtonDown);
  TReadChanneldMode = (rcmFloat, rcmInteger, rcmString);

  TSheetCell = class
    sheet: TSheet;
    row, col: integer;
    text, name, expression: string;    // Text: the raw edit text
    value: double;
    NumberFormat: string;
    CellType: TCellType;
    CellButtonState: TCellButtonState;

    ReadChannel, WriteChannel: string;
    ReadChanneldMode: TReadChanneldMode;
    WriteTriggerRow, WriteTriggerCol: integer;
    DefaultWrite: string;
    DefaultWriteTriggerRow, DefaultWriteTriggerCol: integer;
    Charted: boolean;
    Series: TLineSeries;
    SeriesColor: TColor;

    //SourceCellsList: TStringList;
    level: integer;
    backColor: TColor;
    //Font: TFont;

    constructor Create;
    destructor Destroy; override;
    function DisplayText_: string;
    procedure ParseText(NewText: string);
  private
  end;


  TSheetCellList = specialize TFPGObjectList<TSheetCell>;
  {
  TSheetCellList = class(TList)
  private
  protected
    function GetItems(Index: Integer): TSheetCell;
    procedure SetItems(Index: Integer; ASheetCell: TSheetCell);
  public
    function Add(ASheetCell: TSheetCell): Integer;
    function Extract(Item: TSheetCell): TSheetCell;
    function Remove(ASheetCell: TSheetCell): Integer;
    function IndexOf(ASheetCell: TSheetCell): Integer;
    function First: TSheetCell;
    function Last: TSheetCell;
    procedure Insert(Index: Integer; ASheetCell: TSheetCell);
    property Items[Index: Integer]: TSheetCell read GetItems write SetItems; default;
    procedure ClearAll;
    function IndexFromName(aName: string): integer;
    function IndexFromRC(aRow, aCol: integer): integer;
  end;}

  TSheet = class
    CellList: TSheetCellList; // List of all, non empty, cells (owns each TSheetCell)
    SGrid: TSTringGrid;
    DefaultSheetCell: TSheetCell;  // Empty TSheetCell
    LastError: string;

    constructor Create;
    destructor Destroy; override;

    function Cell(r, c: integer): TSheetCell;
    function EditCell(r, c: integer): TSheetCell;

    //procedure EnterFormula(r, c: integer; newText: string);
    procedure FillHeaders;

    procedure LoadFromFile(XMLFile: string);
    procedure SaveToFile(XMLFile: string);
    procedure Clear;
  private
  end;


  TClearFlag = (cfFormat, cfFormulas);
  TClearFlagsSet = Set of TClearFlag;

procedure DrawButtonText(const Canvas: TCanvas; const Rect: TRect;
  const Text: String; const textFont: TFont; const BackColor, TextColor: TColor; Pushed: boolean);
procedure DrawChartDot(const Canvas: TCanvas; const Rect: TRect; dotColor: TColor);
procedure DrawNormalText(const Canvas: TCanvas; const Rect: TRect;
  const Text: String; const textFont: TFont; const BackColor, TextColor: TColor);


implementation


procedure DrawButtonText(const Canvas: TCanvas; const Rect: TRect;
  const Text: String; const textFont: TFont; const BackColor, TextColor: TColor; Pushed: boolean);
const
  X_BORDER_WIDTH = 2;
  Y_BORDER_WIDTH = 1;
var
  iLeftBorder: Integer;
begin
  Canvas.Font := textFont;
  // calculate the left border
  iLeftBorder := Rect.Left + (Rect.Right - Rect.Left - Canvas.TextWidth(Text)) div 2;
  // set colors
  Canvas.Font.Color := TextColor;
  Canvas.Brush.Color := BackColor;

  Canvas.Brush.Style := bsclear;
  // paint the text
  Canvas.TextRect(Rect, iLeftBorder + ord(Pushed), Rect.Top + Y_BORDER_WIDTH + ord(Pushed), Text);
end;

procedure DrawChartDot(const Canvas: TCanvas; const Rect: TRect; dotColor: TColor);
begin
  // calculate the left border
  //iLeftBorder := Rect.Left + (Rect.Right - Rect.Left - Canvas.TextWidth(Text)) div 2;
  // set colors
  //Canvas.pen.Color := dotColor;
  Canvas.Brush.Color := dotColor;
  Canvas.Brush.Style := bsSolid;
  // paint the text
  Canvas.Ellipse(Rect.Right, Rect.Top, Rect.Right - 7, Rect.Top + 7);
end;


procedure DrawNormalText(const Canvas: TCanvas; const Rect: TRect;
  const Text: String; const textFont: TFont; const BackColor, TextColor: TColor);
const
  X_BORDER_WIDTH = 2;
  Y_BORDER_WIDTH = 1;
var
  iLeftBorder: Integer;
begin
  Canvas.Font := textFont;
  // calculate the left border
  iLeftBorder := Rect.Left + (Rect.Right - Rect.Left - Canvas.TextWidth(Text)) div 2;
  // set colors
  Canvas.Font.Color := TextColor;
  Canvas.Brush.Color := BackColor;

  Canvas.Brush.Style := bsclear;
  // paint the text
  Canvas.TextRect(Rect, iLeftBorder, Rect.Top + Y_BORDER_WIDTH, Text);
end;


{ TSheet }


function TSheet.Cell(r, c: integer): TSheetCell;
begin
  result := DefaultSheetCell;
  if SGrid.Objects[c, r] = nil then exit;
  result := TSheetCell(SGrid.Objects[c, r]);
end;

constructor TSheet.Create;
begin
  DefaultSheetCell := TSheetCell.Create;
  CellList := TSheetCellList.Create;
  //Parser := TSimpleParser.Create;
  //Parser.RegisterFunction('RC', @RCValue, 2);
end;

destructor TSheet.Destroy;
begin
  //Parser.free;
  CellList.Clear;
  CellList.Free;
  DefaultSheetCell.Free;
  inherited;
end;

function TSheet.EditCell(r, c: integer): TSheetCell;
begin
  if SGrid.Objects[c, r] <> nil then begin
    // if it was already alocated use it
    Result := TSheetCell(SGrid.Objects[c, r]);
  end else begin
    // Must create a new one and add it to the list
    Result := TSheetCell.Create;
    CellList.Add(result);
    SGrid.Objects[c, r] := Result;
    Result.sheet := self;
    Result.row := r;
    Result.col := c;
    //Result.GridObject := SGrid.Objects[c, r];  // To know the corresponding GridObject
  end;
end;



{ TSheetCell }

constructor TSheetCell.Create;
begin
  row := -1;
  col := -1;
  NumberFormat := '%g';
  //SourceCellsList := TStringList.Create;
  backColor := clWindow;
  //font := TFont.Create;
end;

destructor TSheetCell.Destroy;
begin
  //font.Free;
  //SourceCellsList.Free;
  inherited;
end;

function TSheetCell.DisplayText_: string;
begin
  if CellType = ctFormula then begin
    result := '=' + expression;
  end else if CellType = ctButton then begin
    result := '[' + expression + ']';
  end else begin
    result := text;
  end;
end;

procedure TSheetCell.ParseText(NewText: string);
var s: string;
    len: integer;
begin
  if NewText = text then exit; //No change!
  text := NewText;
  s := trim(text);
  len := length(s);

  if (len > 0) and (s[1] = '=') then begin
    expression := copy(s, 2, maxint);
    Sheet.SGrid.Cells[col, row] := format(NumberFormat, [value]);
    CellType := ctFormula;
  end else if (len > 0) and (s[1] = '[') and (s[len] = ']') then begin
    expression := copy(s, 2, len - 2);
    value := 0;
    Sheet.SGrid.Cells[col, row] := text;
    CellType := ctButton;
  end else begin
    expression := s;
    value := strToFloatDef(s, 0);
    Sheet.SGrid.Cells[col, row] := text;
    CellType := ctText;
  end;
end;



function FontStylesToStr(FontStyles: TFontStyles): string;
begin
  result := '[    ]';
  if fsBold in FontStyles then result[2] := 'B';
  if fsItalic in FontStyles then result[3] := 'I';
  if fsUnderline in FontStyles then result[4] := 'U';
  if fsStrikeOut in FontStyles then result[5] := 'S';
end;

function StrToFontStyles(s: string; default: TFontStyles): TFontStyles;
begin
  result := default;
  if length(s) <> 6 then exit;
  result := [];
  if s[2] = 'B' then result := result + [fsBold];
  if s[3] = 'I' then result := result + [fsItalic];
  if s[4] = 'I' then result := result + [fsUnderline];
  if s[5] = 'S' then result := result + [fsStrikeOut];
end;


procedure TSheet.SaveToFile(XMLFile: string);
var XML: TXMLDocument;
    node, prop: TDOMNode;
    //PI: TDOMProcessingInstruction;
    i: integer;
    s, def_s: string;
begin
  XML := TXMLDocument.Create;
  try
    //PI := XML.CreateProcessingInstruction('xml', 'version="1.0"');
    //XML.InsertBefore(PI, XML.DocumentElement);

    node := XML.CreateElement('sheet');
    XML.AppendChild(node);

    // Save columns width if they are different from the default value
    node := XML.CreateElement('columns');
    XML.DocumentElement.AppendChild(node);

    prop := XML.CreateElement('defaultcol');
    TDOMElement(prop).SetAttribute('width', format('%d',[SGrid.DefaultColWidth]));
    node.AppendChild(prop);

    for i := 1 to SGrid.ColCount - 1 do begin
      if SGrid.ColWidths[i] = SGrid.DefaultColWidth then continue;
      prop := XML.CreateElement('col');
      TDOMElement(prop).SetAttribute('num', format('%d',[i]));
      TDOMElement(prop).SetAttribute('width', format('%d',[SGrid.ColWidths[i]]));
      node.AppendChild(prop);
    end;


    node := XML.CreateElement('cells');
    XML.DocumentElement.AppendChild(node);

    //def_s := FontStylesToStr(DefaultSheetCell.Font.Style);
    for i := 0 to CellList.Count - 1 do begin
      if (CellList[i].text = '') and
         (CellList[i].ReadChannel = '') and
         (CellList[i].WriteChannel = '') then continue;
      prop := XML.CreateElement('cell');
      TDOMElement(prop).SetAttribute('r', format('%d',[CellList[i].row]));
      TDOMElement(prop).SetAttribute('c', format('%d',[CellList[i].col]));
      TDOMElement(prop).SetAttribute('text', format('%s',[CellList[i].text]));

      if CellList[i].ReadChannel <> '' then
        TDOMElement(prop).SetAttribute('read_channel', format('%s',[CellList[i].ReadChannel]));
      if CellList[i].ReadChanneldMode <> DefaultSheetCell.ReadChanneldMode then
        TDOMElement(prop).SetAttribute('read_channel_mode', format('%d',[ord(CellList[i].ReadChanneldMode)]));

      if CellList[i].WriteChannel <> '' then
        TDOMElement(prop).SetAttribute('write_channel', format('%s',[CellList[i].WriteChannel]));
      if CellList[i].WriteTriggerRow <> 0 then
        TDOMElement(prop).SetAttribute('trigger_row', format('%d',[CellList[i].WriteTriggerRow]));
      if CellList[i].WriteTriggerCol <> 0 then
        TDOMElement(prop).SetAttribute('trigger_col', format('%d',[CellList[i].WriteTriggerCol]));

      if CellList[i].DefaultWrite <> '' then
        TDOMElement(prop).SetAttribute('default_write', format('%s',[CellList[i].DefaultWrite]));
      if CellList[i].DefaultWriteTriggerRow <> 0 then
        TDOMElement(prop).SetAttribute('default_trigger_row', format('%d',[CellList[i].DefaultWriteTriggerRow]));
      if CellList[i].DefaultWriteTriggerCol <> 0 then
        TDOMElement(prop).SetAttribute('default_trigger_col', format('%d',[CellList[i].DefaultWriteTriggerCol]));

      if CellList[i].Charted then
        TDOMElement(prop).SetAttribute('charted', format('%d',[ord(CellList[i].Charted)]));
      if CellList[i].SeriesColor <> clBlack then
        TDOMElement(prop).SetAttribute('series_color', format('%s',[ColorToString(CellList[i].SeriesColor)]));

      if CellList[i].backcolor <> clWindow then
        TDOMElement(prop).SetAttribute('backcolor', format('$%.6x',[integer(CellList[i].backcolor)]));
      //if CellList[i].Font.Name <> DefaultSheetCell.Font.Name then
      //  TDOMElement(prop).SetAttribute('font', format('%s',[CellList[i].Font.Name]));
      //if CellList[i].Font.Size <> DefaultSheetCell.Font.Size then
      //  TDOMElement(prop).SetAttribute('font_size', format('%d',[CellList[i].Font.Size]));
      //s := FontStylesToStr(CellList[i].Font.Style);
      //if s <> def_s then
      //  TDOMElement(prop).SetAttribute('style', s);
      node.AppendChild(prop);
    end;

    WriteXMLFile(XML, XMLFile);

  finally
    XML.Free;
  end;
end;


function GetNodeAttrInt(parentNode: TDOMNode; attrName: string; defaultValue: integer): integer;
var attibute: TDOMNode;
begin
  attibute := parentNode.Attributes.GetNamedItem(attrName);
  if assigned(attibute) then begin
    result := StrToIntDef(attibute.NodeValue, defaultValue);
  end else begin
    result := defaultValue;
  end;
end;


function GetNodeAttrStr(parentNode: TDOMNode; attrName: string; defaultValue: string): string;
var attibute: TDOMNode;
begin
  attibute := parentNode.Attributes.GetNamedItem(attrName);
  if assigned(attibute) then begin
    result := attibute.NodeValue;
  end else begin
    result := defaultValue;
  end;
end;


procedure TSheet.LoadFromFile(XMLFile: string);
var XML: TXMLDocument;
    root, node, prop: TDOMNode;
    w, num: integer;
    i, r, c: integer;
    s: string;
    bcolor: integer;
    SheetCell: TSheetCell;
begin
  ReadXMLFile(XML, XMLFile);
  if XML = nil then exit;

  root := XML.DocumentElement;
  if root.NodeName <> 'sheet' then begin
    LastError := 'Error: root element is <' + root.NodeName + '>';
    exit;
  end;

  for i := 0 to CellList.Count - 1 do begin
    r := CellList[i].row;
    c := CellList[i].col;
    SGrid.Objects[c, r] := nil;
  end;
  SGrid.Clean([gzNormal]);
  CellList.Clear;

  //root := XML.DocumentElement.FindNode('sheet');
  //if root = nil then exit;

  //def_s := FontStylesToStr(DefaultSheetCell.Font.Style);

  node := root.FirstChild;
  while node <> nil do begin
    if node.NodeName = 'columns' then begin
      prop := node.FirstChild;
      while prop <> nil do begin
        // default values

        if prop.NodeName = 'defaultcol' then begin
          w := Sgrid.ColWidths[0]; //Preserve fixed column width
          SGrid.DefaultColWidth := GetNodeAttrInt(prop, 'width', SGrid.DefaultColWidth);
          Sgrid.ColWidths[0] := w;
        end else if prop.NodeName = 'col' then begin
          num := GetNodeAttrInt(prop, 'num', -1);
          w := GetNodeAttrInt(prop, 'width', SGrid.DefaultColWidth);
          if num > 0 then begin
            SGrid.ColWidths[num] := w;
          end;
        end;

        prop := prop.NextSibling;
      end;

    end else if node.NodeName = 'cells' then begin
      prop := node.FirstChild;
      while prop <> nil do begin
        // default values

        if prop.NodeName = 'cell' then begin
          r := GetNodeAttrInt(prop, 'r', -1);
          c := GetNodeAttrInt(prop, 'c', -1);
          s := GetNodeAttrStr(prop, 'backcolor','');
          bcolor := strtointdef(s, clWindow);
          s := GetNodeAttrStr(prop, 'text', '');
          //bcolor := GetNodeAttr(prop, 'backcolor', clWindow);
          if (r >= 0) and (c >= 0) then begin
            SheetCell := EditCell(r, c);
            SheetCell.backColor := TColor(bcolor);
            SheetCell.ParseText(s);

            SheetCell.ReadChannel := GetNodeAttrStr(prop, 'read_channel', DefaultSheetCell.ReadChannel);
            SheetCell.ReadChanneldMode := TReadChanneldMode(GetNodeAttrInt(prop, 'read_channel_mode', ord(DefaultSheetCell.ReadChanneldMode)));

            SheetCell.WriteChannel := GetNodeAttrStr(prop, 'write_channel', DefaultSheetCell.WriteChannel);
            SheetCell.WriteTriggerRow := GetNodeAttrInt(prop, 'trigger_row', DefaultSheetCell.WriteTriggerRow);
            SheetCell.WriteTriggerCol := GetNodeAttrInt(prop, 'trigger_col', DefaultSheetCell.WriteTriggerCol);

            SheetCell.DefaultWrite := GetNodeAttrStr(prop, 'default_write', DefaultSheetCell.DefaultWrite);
            SheetCell.DefaultWriteTriggerRow := GetNodeAttrInt(prop, 'default_trigger_row', DefaultSheetCell.DefaultWriteTriggerRow);
            SheetCell.DefaultWriteTriggerCol := GetNodeAttrInt(prop, 'default_trigger_col', DefaultSheetCell.DefaultWriteTriggerCol);

            SheetCell.Charted := GetNodeAttrInt(prop, 'charted', ord(DefaultSheetCell.Charted)) <> 0;
            SheetCell.SeriesColor := StringToColorDef(GetNodeAttrStr(prop, 'series_color', ColorToString(DefaultSheetCell.SeriesColor)), clBlack);

            //SheetCell.Font.Name := GetNodeAttrStr(prop, 'font', DefaultSheetCell.Font.Name);
            //SheetCell.Font.Size := GetNodeAttrInt(prop, 'font_size', DefaultSheetCell.Font.Size);
            //SheetCell.Font.Style := StrToFontStyles(GetNodeAttrStr(prop, 'style', def_s), DefaultSheetCell.Font.Style);
          end;
        end;

        prop := prop.NextSibling;
      end;
    end;
    node := node.NextSibling;
  end;
  LastError := '';
end;


procedure TSheet.Clear;
var i, r, c: integer;
begin
  for i := 0 to CellList.Count - 1 do begin
    r := CellList[i].row;
    c := CellList[i].col;
    SGrid.Objects[c, r] := nil;
  end;
  SGrid.Clean([gzNormal]);
  CellList.Clear;

  LastError := '';
end;


//procedure TSheet.EnterFormula(r, c: integer; newText: string);
//begin
//  EditCell(r, c).ParseText(newText);
//end;


procedure TSheet.FillHeaders;
var i: integer;
begin
  for i := 0 to Sgrid.ColCount -1 do begin
    SGrid.Cells[i, 0] := inttostr(i);
  end;
  for i := 0 to Sgrid.RowCount -1 do begin
    SGrid.Cells[0, i] := inttostr(i);
  end;
end;


end.

