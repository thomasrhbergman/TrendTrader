unit Scanner.Filters;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, IABFunctions, IABSocketAPI,
  System.Generics.Collections, BrokerHelperAbstr, Winapi.msxml, Vcl.ExtCtrls, Vcl.Controls,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Graphics, Vcl.Samples.Spin, Vcl.ComCtrls, DebugWriter, DaModule, Scanner.Types,
  Scanner.FilterList, DaImages, Utils, Common.Types, Global.Types, Publishers;
{$ENDREGION}

type
  TFieldType = (ftAmountField, ftBooleanField, ftComboField, ftComboFieldConverted, ftConidField, ftDateField,
                ftDoubleField, ftEmbeddedFilterField, ftIntField, ftSeparatorField, ftSliderField, ftStringField);
  TRangeState = (rsUnknown, rsLess, rsGreater, rsRange);

  IScannerControl = interface
    ['{C70A782A-429C-4CA8-98DC-11D458829B45}']
    function GetEditValue: string;
    function GetFieldType: TFieldType;
    function GetVarName: string;
    function GetCode: string;
    procedure SetEditValue(const Value: string);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetVarName(const Value: string);
    procedure SetCode(const Value: string);
    property Code      : string     read GetCode      write SetCode;
    property EditValue : string     read GetEditValue write SetEditValue;
    property FieldType : TFieldType read GetFieldType write SetFieldType;
    property VarName   : string     read GetVarName   write SetVarName;
  end;

  TFilterControl = class;
  TFiltersController = class(TDictionary<string, TFilterControl>)
  private
    FFilterList: TArrayFilterData;
    FOwner: TWinControl;
  public const
    AnchorValues: array [0..6] of string = ('PRICE', 'VOLUME', 'CONVOPT', 'CPNRATE', 'MATDATE', 'SP', 'MOODY');
  public
    function AddFilter(aId: string): Boolean;
    function AddOrReplaceFilters: Boolean;
    procedure ClearAllFilters;
    procedure ClearFilterControl(aId: string);
    procedure DeleteFilterControl(aId: string);
    procedure RemoveAllFilters;

    property FilterList: TArrayFilterData read FFilterList write FFilterList;
    property Owner     : TWinControl      read FOwner      write FOwner;
  end;

  TFilterControl = class(TPanel)
  public const
    RangeStateString: array [TRangeState] of string = ('unknown', 'less than', 'greater than', 'in the range');

    C_TYPE_FIELD_AMOUNT          = 'scanner.filter.InvestmentAmountField';
    C_TYPE_FIELD_BOOLEAN         = 'scanner.filter.BooleanField';
    C_TYPE_FIELD_COMBO           = 'scanner.filter.ComboField';
    C_TYPE_FIELD_COMBO_CONVERTED = 'scanner.filter.ComboField$ConvertedComboField';
    C_TYPE_FIELD_CONID           = 'scanner.filter.ConidField';
    C_TYPE_FIELD_DATE            = 'scanner.filter.DateField';
    C_TYPE_FIELD_DOUBLE          = 'scanner.filter.DoubleField';
    C_TYPE_FIELD_EMBEDDED        = 'scanner.filter.EmbeddedFilterField';
    C_TYPE_FIELD_INTEGER         = 'scanner.filter.IntField';
    C_TYPE_FIELD_SEPARATOR       = 'scanner.filter.SeparatorField';
    C_TYPE_FIELD_SLIDER          = 'scanner.filter.SliderField';
    C_TYPE_FIELD_STRING          = 'scanner.filter.StringField';

    C_LEFT_MAX  = 280;
    C_LEFT_MIN  = 215;
    C_WIDTH_MAX = 50;
    C_WIDTH_MIN = 115;
    C_TOP       = 4;
  private
    FComboboxRange: TCombobox;
    FId: string;
    FParentController: TFiltersController;
    FVariableList: TDictionary<string, string>;
    FFilterData: TFilterData;
    function GetRangeState: TRangeState;
    function GetVariableList: TDictionary<string, string>;
    procedure OnDoubleEditKeyPress(Sender: TObject; var Key: Char);
    procedure OnRangeComboboxChange(Sender: TObject);
    procedure OnRemoveFilterClick(Sender: TObject);
    procedure SetRangeState(const Value: TRangeState);
  public
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(aFilterData: TFilterData);
    procedure SetVariableValue(const Key, Value: string);

    property FilterData       : TFilterData                 read FFilterData       write FFilterData;
    property Id               : string                      read FId               write FId;
    property ParentController : TFiltersController          read FParentController write FParentController;
    property RangeState       : TRangeState                 read GetRangeState     write SetRangeState;
    property VariableList     : TDictionary<string, string> read GetVariableList;
  end;

  TScannerEdit = class(TEdit, IScannerControl)
  private
    FCode: string;
    FFieldType: TFieldType;
    FVarName: string;
    function GetCode: string;
    function GetEditValue: string;
    function GetFieldType: TFieldType;
    function GetVarName: string;
    procedure SetCode(const Value: string);
    procedure SetEditValue(const Value: string);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetVarName(const Value: string);
  public
    property Code      : string     read GetCode      write SetCode;
    property EditValue : string     read GetEditValue write SetEditValue;
    property FieldType : TFieldType read GetFieldType write SetFieldType;
    property VarName   : string     read GetVarName   write SetVarName;
  end;

  TScannerSpinEdit = class(TSpinEdit, IScannerControl)
  private
    FCode: string;
    FFieldType: TFieldType;
    FVarName: string;
    function GetCode: string;
    function GetEditValue: string;
    function GetFieldType: TFieldType;
    function GetVarName: string;
    procedure SetCode(const Value: string);
    procedure SetEditValue(const Value: string);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetVarName(const Value: string);
  public
    property Code      : string     read GetCode      write SetCode;
    property EditValue : string     read GetEditValue write SetEditValue;
    property FieldType : TFieldType read GetFieldType write SetFieldType;
    property VarName   : string     read GetVarName   write SetVarName;
  end;

  TScannerCheckBox = class(TCheckBox, IScannerControl)
  private
    FCode: string;
    FFieldType: TFieldType;
    FVarName: string;
    function GetCode: string;
    function GetEditValue: string;
    function GetFieldType: TFieldType;
    function GetVarName: string;
    procedure SetCode(const Value: string);
    procedure SetEditValue(const Value: string);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetVarName(const Value: string);
  public
    property Code      : string     read GetCode      write SetCode;
    property EditValue : string     read GetEditValue write SetEditValue;
    property FieldType : TFieldType read GetFieldType write SetFieldType;
    property VarName   : string     read GetVarName   write SetVarName;
  end;

  TScannerDateTimePicker = class(TDateTimePicker, IScannerControl)
  private
    FCode: string;
    FFieldType: TFieldType;
    FVarName: string;
    function GetCode: string;
    function GetEditValue: string;
    function GetFieldType: TFieldType;
    function GetVarName: string;
    procedure SetCode(const Value: string);
    procedure SetEditValue(const Value: string);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetVarName(const Value: string);
  public
    property Code      : string     read GetCode      write SetCode;
    property EditValue : string     read GetEditValue write SetEditValue;
    property FieldType : TFieldType read GetFieldType write SetFieldType;
    property VarName   : string     read GetVarName   write SetVarName;
  end;

  TScannerComboBox = class(TComboBox, IScannerControl)
  private
    FCode         : string;
    FFieldType    : TFieldType;
    FVarName      : string;
    FDefaultIndex : Integer;
    function GetCode: string;
    function GetEditValue: string;
    function GetFieldType: TFieldType;
    function GetVarName: string;
    procedure SetCode(const Value: string);
    procedure SetEditValue(const Value: string);
    procedure SetFieldType(const Value: TFieldType);
    procedure SetVarName(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property Code         : string     read GetCode       write SetCode;
    property DefaultIndex : Integer    read FDefaultIndex write FDefaultIndex;
    property EditValue    : string     read GetEditValue  write SetEditValue;
    property FieldType    : TFieldType read GetFieldType  write SetFieldType;
    property VarName      : string     read GetVarName    write SetVarName;
  end;

implementation

{ TFiltersController }

function TFiltersController.AddOrReplaceFilters: Boolean;
var
  i, j: Integer;
  FilterControl: TFilterControl;
  CanDelete: Boolean;
begin
  Result := False;
  if (TfrmScannerFilterList.ShowDocument(@FFilterList) = mrOk) then
  begin
    SendMessage(Owner.Handle, WM_SETREDRAW, 0, 0);
    try
      for i := Low(FFilterList.Items) to High(FFilterList.Items) do
      begin
        TPublishers.LogPublisher.Write([ltLogWriter], ddText, Self, 'AddOrReplaceFilters', FFilterList[i].Id + ' = ' + BoolToStr(FFilterList[i].Checked, True));
        if FFilterList[i].Checked and (not FFilterList[i].Id.IsEmpty) and (not Self.ContainsKey(FFilterList[i].Id)) then
        begin
          FilterControl := TFilterControl.Create(Owner);
          FilterControl.ParentController := Self;
          FilterControl.Id := FFilterList[i].Id;
          FilterControl.Initialize(FFilterList[i]);
          Self.Add(FFilterList[i].Id, FilterControl);
          Result := True;
        end
        else
        begin
          CanDelete := True;
          for j := Low(AnchorValues) to High(AnchorValues) do
            if (FFilterList[i].Id = AnchorValues[j]) then
            begin
              CanDelete := False;
              Break;
            end;

          if CanDelete and (not FFilterList[i].Checked) and Self.ContainsKey(FFilterList[i].Id) then
            DeleteFilterControl(FFilterList[i].Id);
        end;
      end;
    finally
      SendMessage(Owner.Handle, WM_SETREDRAW, 1, 0);
      RedrawWindow(Owner.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
    end;
  end;
end;

function TFiltersController.AddFilter(aId: string): Boolean;
var
  i: Integer;
  FilterControl: TFilterControl;
begin
  Result := False;
  SendMessage(Owner.Handle, WM_SETREDRAW, 0, 0);
  try
    for i := Low(FFilterList.Items) to High(FFilterList.Items) do
      if (FFilterList[i].Id = aId) then
      begin
        if (not Self.ContainsKey(FFilterList[i].Id)) then
        begin
          FilterControl := TFilterControl.Create(Owner);
          FilterControl.ParentController := Self;
          FilterControl.Id := FFilterList[i].Id;
          FilterControl.Initialize(FFilterList[i]);
          FFilterList.Items[i].Checked := True;
          Self.Add(FFilterList[i].Id, FilterControl);
        end;
        Result := True;
        Break;
      end;
  finally
    SendMessage(Owner.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Owner.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
  end;
end;

procedure TFiltersController.RemoveAllFilters;
var
  i: Integer;
begin
  SendMessage(Owner.Handle, WM_SETREDRAW, 0, 0);
  try
    for i := High(FFilterList.Items) downto Low(FFilterList.Items) do
    begin
      FFilterList.Items[i].Checked := False;
      DeleteFilterControl(FFilterList[i].Id);
    end;
  finally
    SendMessage(Owner.Handle, WM_SETREDRAW, 1, 0);
    RedrawWindow(Owner.Handle, nil, 0, RDW_ERASE or RDW_INVALIDATE or RDW_FRAME or RDW_ALLCHILDREN);
  end;
  Self.Clear;
  Self.AddFilter('VOLUME');
end;

procedure TFiltersController.ClearAllFilters;
var
  i: Integer;
begin
  for i := High(FFilterList.Items) downto Low(FFilterList.Items) do
    ClearFilterControl(FFilterList.Items[i].Id);
end;

procedure TFiltersController.ClearFilterControl(aId: string);
var
  FilterControl: TFilterControl;
begin
  if Self.ContainsKey(aId) then
  begin
    FilterControl := Self.Items[aId];
    FilterControl.Clear;
  end;
end;

procedure TFiltersController.DeleteFilterControl(aId: string);
var
  FilterControl: TFilterControl;
begin
  if Self.ContainsKey(aId) then
  begin
    FilterControl := Self.Items[aId];
    FreeAndNil(FilterControl);
    Self.Remove(aId);
  end;
end;

{ TFilterControl }

constructor TFilterControl.Create(AOwner: TComponent);
begin
  inherited;
  if (AOwner is TWinControl) then
    Parent := TWinControl(AOwner);
  Align         := alTop;
  BevelInner    := bvNone;
  BevelOuter    := bvNone;
  ShowCaption   := False;
  Height        := 28;
  Top           := 1000;
  FVariableList := TDictionary<string, string>.Create;
end;

destructor TFilterControl.Destroy;
begin
  FVariableList.Clear;
  FreeAndNil(FVariableList);
  inherited;
end;

procedure TFilterControl.Clear;
var
  i: Integer;
  ScannerControl: IScannerControl;
begin
  for i := 0 to Self.ControlCount - 1 do
    if Supports(Self.Controls[i], IScannerControl, ScannerControl) then
      ScannerControl.EditValue := '';
end;

procedure TFilterControl.Initialize(aFilterData: TFilterData);
var
  Bmp                : TBitmap;
  ButtonRemoveFilter : TSpeedButton;
  CheckBox           : TScannerCheckBox;
  ComboBox           : TScannerComboBox;
  DateTimePicker     : TScannerDateTimePicker;
  DisplayName        : string;
  Edit               : TScannerEdit;
  i                  : Integer;
  IsCreateControl    : Boolean;
  LabelCaption       : TLabel;
  LabelTo            : TLabel;
  SpinEdit           : TScannerSpinEdit;
begin
  FFilterData := aFilterData;
  IsCreateControl := True;
  if (Length(aFilterData.AbstractField) > 0) then
  begin
    if (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_DOUBLE) then
    begin
      Edit := TScannerEdit.Create(Self);
      Edit.Parent     := Self;
      Edit.Text       := '0';
      Edit.Hint       := aFilterData.AbstractField[0].Tooltip;
      Edit.Width      := C_WIDTH_MIN;
      Edit.Left       := C_LEFT_MIN;
      Edit.Top        := C_TOP;
      Edit.OnKeyPress := OnDoubleEditKeyPress;
      Edit.VarName    := aFilterData.AbstractField[0].VarName;
      Edit.Code       :=  aFilterData.AbstractField[0].Code;
      Edit.FieldType  := ftDoubleField;
    end
    else if (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_INTEGER) then
    begin
      SpinEdit := TScannerSpinEdit.Create(Self);
      SpinEdit.Parent    := Self;
      SpinEdit.Hint      := aFilterData.AbstractField[0].Tooltip;
      SpinEdit.Width     := C_WIDTH_MIN;
      SpinEdit.Left      := C_LEFT_MIN;
      SpinEdit.Top       := C_TOP;
      SpinEdit.MaxValue  := aFilterData.AbstractField[0].MaxValue;
      SpinEdit.MinValue  := aFilterData.AbstractField[0].MinValue;
      SpinEdit.VarName   := aFilterData.AbstractField[0].VarName;
      SpinEdit.Code      := aFilterData.AbstractField[0].Code;
      SpinEdit.FieldType := ftIntField;
    end
    else if (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_STRING) then
    begin
      Edit := TScannerEdit.Create(Self);
      Edit.Parent    := Self;
      Edit.Text      := '0';
      Edit.Hint      := aFilterData.AbstractField[0].Tooltip;
      Edit.Width     := C_WIDTH_MIN;
      Edit.Left      := C_LEFT_MIN;
      Edit.Top       := C_TOP;
      Edit.VarName   := aFilterData.AbstractField[0].VarName;
      Edit.Code      := aFilterData.AbstractField[0].Code;
      Edit.FieldType := ftStringField;
    end
    else if (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_BOOLEAN) then
    begin
      CheckBox := TScannerCheckBox.Create(Self);
      CheckBox.Parent    := Self;
      CheckBox.Text      := aFilterData.AbstractField[0].DisplayName;
      CheckBox.Hint      := aFilterData.AbstractField[0].Tooltip;
      CheckBox.Width     := C_WIDTH_MIN;
      CheckBox.Left      := C_LEFT_MIN;
      CheckBox.Top       := C_TOP;
      CheckBox.VarName   := aFilterData.AbstractField[0].VarName;
      CheckBox.Code      := aFilterData.AbstractField[0].Code;
      CheckBox.FieldType := ftBooleanField;
      IsCreateControl    := False;
    end
    else if (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_DATE) then
    begin
      DateTimePicker := TScannerDateTimePicker.Create(Self);
      DateTimePicker.Parent    := Self;
      DateTimePicker.Date      := System.SysUtils.Date;
      DateTimePicker.Hint      := aFilterData.AbstractField[0].Tooltip;
      DateTimePicker.Width     := C_WIDTH_MIN;
      DateTimePicker.Left      := C_LEFT_MIN;
      DateTimePicker.Top       := C_TOP;
      DateTimePicker.VarName   := aFilterData.AbstractField[0].VarName;
      DateTimePicker.Code      := aFilterData.AbstractField[0].Code;
      DateTimePicker.FieldType := ftDateField;
    end
    else if (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_COMBO) or
            (aFilterData.AbstractField[0].AbstractFieldType = C_TYPE_FIELD_COMBO_CONVERTED) then
    begin
      ComboBox := TScannerComboBox.Create(Self);
      ComboBox.Parent    := Self;
      ComboBox.Hint      := aFilterData.AbstractField[0].Tooltip;
      ComboBox.Width     := C_WIDTH_MIN;
      ComboBox.Left      := C_LEFT_MIN;
      ComboBox.Top       := C_TOP;
      ComboBox.VarName   := aFilterData.AbstractField[0].VarName;
      ComboBox.Code      := aFilterData.AbstractField[0].Code;
      ComboBox.FieldType := ftComboField;
      for i := Low(aFilterData.AbstractField[0].ComboValues) to High(aFilterData.AbstractField[0].ComboValues) do
      begin
        ComboBox.Items.AddObject(aFilterData.AbstractField[0].ComboValues[i].DisplayName, TObject(aFilterData.AbstractField[0].ComboValues[i].Code));
        if aFilterData.AbstractField[0].ComboValues[i].Default then
        begin
          ComboBox.ItemIndex := i;
          ComboBox.DefaultIndex := i;
        end;
      end;
    end;
  end;

  if (aFilterData.TypeFilter = 'RangeFilter') and (Length(aFilterData.AbstractField) > 1) then
  begin
    FComboboxRange := TCombobox.Create(Self);
    FComboboxRange.Parent := Self;
    FComboboxRange.Items.Add(RangeStateString[rsLess]);
    FComboboxRange.Items.Add(RangeStateString[rsGreater]);
    FComboboxRange.Items.Add(RangeStateString[rsRange]);
    FComboboxRange.ItemIndex := 1;
    FComboboxRange.Width     := 80;
    FComboboxRange.Left      := 130;
    FComboboxRange.Top       := C_TOP;
    FComboboxRange.OnChange  := OnRangeComboboxChange;

    if (aFilterData.AbstractField[1].AbstractFieldType = C_TYPE_FIELD_DOUBLE) then
    begin
      Edit := TScannerEdit.Create(Self);
      Edit.Parent     := Self;
      Edit.Text       := '0';
      Edit.Hint       := aFilterData.AbstractField[1].Tooltip;
      Edit.Width      := C_WIDTH_MAX;
      Edit.Left       := C_LEFT_MAX;
      Edit.Top        := C_TOP;
      Edit.Visible    := False;
      Edit.OnKeyPress := OnDoubleEditKeyPress;
      Edit.VarName    := aFilterData.AbstractField[1].VarName;
      Edit.Code       := aFilterData.AbstractField[1].Code;
      Edit.FieldType  := ftDoubleField;
    end
    else if (aFilterData.AbstractField[1].AbstractFieldType = C_TYPE_FIELD_INTEGER) then
    begin
      SpinEdit := TScannerSpinEdit.Create(Self);
      SpinEdit.Parent    := Self;
      SpinEdit.Hint      := aFilterData.AbstractField[1].Tooltip;
      SpinEdit.Width     := C_WIDTH_MAX;
      SpinEdit.Left      := C_LEFT_MAX;
      SpinEdit.Top       := C_TOP;
      SpinEdit.Visible   := False;
      SpinEdit.MaxValue  := aFilterData.AbstractField[1].MaxValue;
      SpinEdit.MinValue  := aFilterData.AbstractField[1].MinValue;
      SpinEdit.VarName   := aFilterData.AbstractField[1].VarName;
      SpinEdit.Code      := aFilterData.AbstractField[1].Code;
      SpinEdit.FieldType := ftIntField;
    end
    else if (aFilterData.AbstractField[1].AbstractFieldType = C_TYPE_FIELD_STRING) then
    begin
      Edit := TScannerEdit.Create(Self);
      Edit.Parent    := Self;
      Edit.Text      := '0';
      Edit.Hint      := aFilterData.AbstractField[1].Tooltip;
      Edit.Width     := C_WIDTH_MAX;
      Edit.Left      := C_LEFT_MAX;
      Edit.Top       := C_TOP;
      Edit.Visible   := False;
      Edit.VarName   := aFilterData.AbstractField[1].VarName;
      Edit.Code      := aFilterData.AbstractField[1].Code;
      Edit.FieldType := ftStringField;
    end
    else if (aFilterData.AbstractField[1].AbstractFieldType = C_TYPE_FIELD_BOOLEAN) then
    begin
      CheckBox := TScannerCheckBox.Create(Self);
      CheckBox.Parent    := Self;
      CheckBox.Text      := aFilterData.AbstractField[1].DisplayName;
      CheckBox.Hint      := aFilterData.AbstractField[1].Tooltip;
      CheckBox.Width     := C_WIDTH_MAX;
      CheckBox.Left      := C_LEFT_MAX;
      CheckBox.Top       := C_TOP;
      CheckBox.Visible   := False;
      CheckBox.VarName   := aFilterData.AbstractField[1].VarName;
      CheckBox.Code      := aFilterData.AbstractField[1].Code;
      CheckBox.FieldType := ftBooleanField;
      IsCreateControl    := False;
    end
    else if (aFilterData.AbstractField[1].AbstractFieldType = C_TYPE_FIELD_DATE) then
    begin
      DateTimePicker := TScannerDateTimePicker.Create(Self);
      DateTimePicker.Parent    := Self;
      DateTimePicker.Date      := System.SysUtils.Date;
      DateTimePicker.Hint      := aFilterData.AbstractField[1].Tooltip;
      DateTimePicker.Width     := C_WIDTH_MAX;
      DateTimePicker.Left      := C_LEFT_MAX;
      DateTimePicker.Top       := C_TOP;
      DateTimePicker.Visible   := False;
      DateTimePicker.VarName   := aFilterData.AbstractField[1].VarName;
      DateTimePicker.Code      := aFilterData.AbstractField[1].Code;
      DateTimePicker.FieldType := ftDateField;
    end
    else if (aFilterData.AbstractField[1].AbstractFieldType = C_TYPE_FIELD_COMBO) then
    begin
      ComboBox := TScannerComboBox.Create(Self);
      ComboBox.Parent    := Self;
      ComboBox.Hint      := aFilterData.AbstractField[1].Tooltip;
      ComboBox.Width     := C_WIDTH_MAX;
      ComboBox.Left      := C_LEFT_MAX;
      ComboBox.Top       := C_TOP;
      ComboBox.Visible   := False;
      ComboBox.VarName   := aFilterData.AbstractField[1].VarName;
      ComboBox.Code      := aFilterData.AbstractField[1].Code;
      ComboBox.FieldType := ftBooleanField;
      for i := Low(aFilterData.AbstractField[1].ComboValues) to High(aFilterData.AbstractField[1].ComboValues) do
      begin
        ComboBox.Items.AddObject(aFilterData.AbstractField[1].ComboValues[i].DisplayName, TObject(aFilterData.AbstractField[1].ComboValues[i].Code));
        if aFilterData.AbstractField[1].ComboValues[i].Default then
        begin
          ComboBox.ItemIndex    := i;
          ComboBox.DefaultIndex := i;
        end;
      end;
    end;

    LabelTo := TLabel.Create(Self);
    LabelTo.Parent  := Self;
    LabelTo.Caption := 'to';
    LabelTo.Top     := 7;
    LabelTo.Left    := 268;
    LabelTo.Width   := 10;
  end;

  if IsCreateControl then
  begin
    DisplayName := '';
    for i := Low(aFilterData.AbstractField) to High(aFilterData.AbstractField) do
    begin
      DisplayName := aFilterData.AbstractField[i].DisplayName.Replace('Above', '').Replace('Below', '').TrimRight;
      if not DisplayName.IsEmpty then
        Break;
    end;
    if DisplayName.IsEmpty then
      for i := Low(aFilterData.Columns) to High(aFilterData.Columns) do
      begin
        DisplayName := aFilterData.Columns[i].Name.Replace('Above', '').Replace('Below', '').TrimRight;
        if not DisplayName.IsEmpty then
          Break;
      end;

    LabelCaption := TLabel.Create(Self);
    LabelCaption.Parent    := Self;
    LabelCaption.Alignment := taRightJustify;
    LabelCaption.Left      := 2;
    LabelCaption.Top       := 7;
    if (aFilterData.TypeFilter = 'SimpleFilter') then
    begin
      DisplayName := DisplayName + ' ' + RangeStateString[rsGreater];
      LabelCaption.Width := 200;
    end
    else
      LabelCaption.Width := 125;
    LabelCaption.Caption := DisplayName;
  end;

  IsCreateControl := True;
  for i := Low(FParentController.AnchorValues) to High(FParentController.AnchorValues) do
    if (aFilterData.Id = FParentController.AnchorValues[i]) then
    begin
      IsCreateControl := False;
      Break;
    end;

  if IsCreateControl then
  begin
    ButtonRemoveFilter := TSpeedButton.Create(Self);
    ButtonRemoveFilter.Parent  := Self;
    ButtonRemoveFilter.Flat    := True;
    ButtonRemoveFilter.Height  := 23;
    ButtonRemoveFilter.Left    := 330;
    ButtonRemoveFilter.Top     := C_TOP;
    ButtonRemoveFilter.Width   := 23;
    ButtonRemoveFilter.OnClick := OnRemoveFilterClick;
    Bmp := TBitmap.Create;
    try
      DMImage.vil16.GetBitmap(43, Bmp); // X
      ButtonRemoveFilter.Glyph.Assign(Bmp);
    finally
      FreeAndNil(Bmp);
    end;
  end;
end;

function TFilterControl.GetRangeState: TRangeState;
begin
  Result := rsUnknown;
  if Assigned(FComboboxRange) then
    case FComboboxRange.ItemIndex of
      0:
        Result := rsLess;
      1:
        Result := rsGreater;
      2:
        Result := rsRange;
    end;
end;

procedure TFilterControl.SetRangeState(const Value: TRangeState);
begin
  if Assigned(FComboboxRange) and (RangeState <> Value) then
  begin
    case Value of
      rsLess:
        FComboboxRange.ItemIndex := 0;
      rsGreater:
        FComboboxRange.ItemIndex := 1;
      rsRange:
        FComboboxRange.ItemIndex := 2;
    end;
    OnRangeComboboxChange(FComboboxRange);
  end;
end;

procedure TFilterControl.SetVariableValue(const Key, Value: string);
var
  ScannerControl: IScannerControl;
  i: Integer;
begin
  for i := 0 to Self.ControlCount - 1 do
    if Supports(Self.Controls[i], IScannerControl, ScannerControl) and (ScannerControl.Code = Key) then
    begin
      ScannerControl.EditValue := Value;
      Break;
    end;
end;

function TFilterControl.GetVariableList: TDictionary<string, string>;
var
  i: Integer;
  ScannerControl: IScannerControl;
begin
  FVariableList.Clear;
  for i := 0 to Self.ControlCount - 1 do
    if Supports(Self.Controls[i], IScannerControl, ScannerControl) then
    begin
      case RangeState of
        rsLess:
          if ScannerControl.VarName = 'max' then
            FVariableList.AddOrSetValue(ScannerControl.Code, ScannerControl.EditValue);
        rsGreater:
          if ScannerControl.VarName = 'min' then
            FVariableList.AddOrSetValue(ScannerControl.Code, ScannerControl.EditValue);
      else
        FVariableList.AddOrSetValue(ScannerControl.Code, ScannerControl.EditValue);
      end;
    end;
  Result := FVariableList;
end;

procedure TFilterControl.OnDoubleEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not(CharInSet(Key, ['0' .. '9', #08, FormatSettings.DecimalSeparator])) then
    Key := #0;
end;

procedure TFilterControl.OnRemoveFilterClick(Sender: TObject);
begin
  if Assigned(ParentController) then
    ParentController.DeleteFilterControl(Self.Id);
end;

procedure TFilterControl.OnRangeComboboxChange(Sender: TObject);
var
  i: Integer;
  ScannerControl: IScannerControl;
begin
  if Sender is TComboBox then
  begin
    if TComboBox(Sender).ItemIndex in [0, 1, 2] then
      Self.RangeState := TRangeState(TComboBox(Sender).ItemIndex + 1);

    for i := 0 to Self.ControlCount - 1 do
      if Supports(Self.Controls[i], IScannerControl, ScannerControl) then
      begin
        case Self.RangeState of
          rsLess:
            begin
              if (ScannerControl.VarName = 'max') then
              begin
                Self.Controls[i].Width   := C_WIDTH_MIN;
                Self.Controls[i].Left    := C_LEFT_MIN;
                Self.Controls[i].Visible := True;
              end
              else if (ScannerControl.VarName = 'min') then
              begin
                Self.Controls[i].Visible := False;
              end
            end;
          rsGreater:
            if (ScannerControl.VarName = 'max') then
            begin
              Self.Controls[i].Visible := False;
            end
            else if (ScannerControl.VarName = 'min') then
            begin
              Self.Controls[i].Width   := C_WIDTH_MIN;
              Self.Controls[i].Left    := C_LEFT_MIN;
              Self.Controls[i].Visible := True;
            end;
          rsRange:
            begin
              if (ScannerControl.VarName = 'min') then
              begin
                Self.Controls[i].Width   := C_WIDTH_MAX;
                Self.Controls[i].Left    := C_LEFT_MIN;
                Self.Controls[i].Visible := True;
              end
              else if (ScannerControl.VarName = 'max') then
              begin
                Self.Controls[i].Width   := C_WIDTH_MAX;
                Self.Controls[i].Left    := C_LEFT_MAX;
                Self.Controls[i].Visible := True;
              end;
            end;
        end;
      end;
  end;
end;

{ TScannerEdit }

function TScannerEdit.GetCode: string;
begin
  Result := FCode;
end;

function TScannerEdit.GetEditValue: string;
begin
  case Self.FieldType of
    ftStringField:
      Result := Self.Text;
    ftDoubleField:
      Result := FormatFloat('0.00####', SafeStrToFloat(Self.Text, 0)).Replace(FormatSettings.DecimalSeparator, '.');
  end;
end;

function TScannerEdit.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TScannerEdit.GetVarName: string;
begin
  Result := FVarName;
end;

procedure TScannerEdit.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TScannerEdit.SetEditValue(const Value: string);
begin
  if Value.IsEmpty and (Self.FieldType = ftStringField) then
    Self.Text := ''
  else if Value.IsEmpty and (Self.FieldType = ftDoubleField) then
    Self.Text := '0'
  else
    Self.Text := VarToStr(Value);
end;

procedure TScannerEdit.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TScannerEdit.SetVarName(const Value: string);
begin
  FVarName := Value;
end;

{ TScannerSpinEdit }

function TScannerSpinEdit.GetCode: string;
begin
  Result := FCode;
end;

function TScannerSpinEdit.GetEditValue: string;
begin
  Result := Self.Value.ToString;
end;

function TScannerSpinEdit.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TScannerSpinEdit.GetVarName: string;
begin
  Result := FVarName;
end;

procedure TScannerSpinEdit.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TScannerSpinEdit.SetEditValue(const Value: string);
begin
  if Value.IsEmpty then
    Self.Value := 0
  else
    Self.Value := StrToIntDef(VarToStr(Value), 0);
end;

procedure TScannerSpinEdit.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TScannerSpinEdit.SetVarName(const Value: string);
begin
  FVarName := Value;
end;

{ TScannerComboBox }

constructor TScannerComboBox.Create(AOwner: TComponent);
begin
  inherited;
  FDefaultIndex := -1;
  Style := csDropDownList;
end;

function TScannerComboBox.GetCode: string;
begin
  Result := FCode;
end;

function TScannerComboBox.GetEditValue: string;
begin
  if (Self.ItemIndex > -1) then
    Result := string(Self.Items.Objects[Self.ItemIndex])
  else if (FDefaultIndex > -1) then
    Result := string(Self.Items.Objects[FDefaultIndex]);
end;

function TScannerComboBox.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TScannerComboBox.GetVarName: string;
begin
  Result := FVarName;
end;

procedure TScannerComboBox.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TScannerComboBox.SetEditValue(const Value: string);
begin
  if Value.IsEmpty then
    Self.ItemIndex := FDefaultIndex
  else
  begin
    if Self.Items.IndexOf(Value) > -1 then
      Self.ItemIndex := Self.Items.IndexOf(Value)
    else
      Self.ItemIndex := FDefaultIndex;
  end;
end;

procedure TScannerComboBox.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TScannerComboBox.SetVarName(const Value: string);
begin
  FVarName := Value;
end;

{ TScannerCheckBox }

function TScannerCheckBox.GetCode: string;
begin
  Result := FCode;
end;

function TScannerCheckBox.GetEditValue: string;
begin
  Result := BoolToStr(Self.Checked, True).ToLower;
end;

function TScannerCheckBox.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TScannerCheckBox.GetVarName: string;
begin
  Result := FVarName;
end;

procedure TScannerCheckBox.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TScannerCheckBox.SetEditValue(const Value: string);
begin
  if Value.IsEmpty then
    Self.Checked := False
  else
    Self.Checked := StrToBoolDef(VarToStr(Value), False);
end;

procedure TScannerCheckBox.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TScannerCheckBox.SetVarName(const Value: string);
begin
  FVarName := Value;
end;

{ TScannerDateTimePicker }

function TScannerDateTimePicker.GetCode: string;
begin
  Result := FCode;
end;

function TScannerDateTimePicker.GetEditValue: string;
begin
  Result := FormatDateTime('YYYYMMDD', Self.Date);
end;

function TScannerDateTimePicker.GetFieldType: TFieldType;
begin
  Result := FFieldType;
end;

function TScannerDateTimePicker.GetVarName: string;
begin
  Result := FVarName;
end;

procedure TScannerDateTimePicker.SetCode(const Value: string);
begin
  FCode := Value;
end;

procedure TScannerDateTimePicker.SetEditValue(const Value: string);
begin
  if Value.IsEmpty then
    Self.Date := System.SysUtils.Date
  else
    Self.Date := StrToDateDef(VarToStr(Value), System.SysUtils.Date);
end;

procedure TScannerDateTimePicker.SetFieldType(const Value: TFieldType);
begin
  FFieldType := Value;
end;

procedure TScannerDateTimePicker.SetVarName(const Value: string);
begin
  FVarName := Value;
end;

end.
