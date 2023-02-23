unit Candidate.EmbargoColumn;

interface

{$REGION 'Region uses'}
uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Samples.Spin, System.Math, Vcl.ComCtrls,
  System.Generics.Collections, Common.Types, {$IFDEF USE_CODE_SITE}CodeSiteLogging, {$ENDIF} CustomForms, IABFunctions,
  IABSocketAPI, IABSocketAPI_const, Candidate.Types, DaImages, Utils, Vcl.NumberBox;
{$ENDREGION}

type
  TfrmCandidateEmbargoColumn = class(TCustomForm)
    btnAddColumn: TBitBtn;
    btnCancel: TBitBtn;
    cbColumns: TComboBox;
    cbColumnValue: TComboBox;
    cbHoldWorkingDays: TCheckBox;
    cbRankingPosition: TComboBox;
    cbRankingSum: TComboBox;
    cbReleaseWorkingDays: TCheckBox;
    cbTimeIntervalFromDataObtained: TComboBox;
    cbValueExistInColumn: TComboBox;
    edColumnValue1: TNumberBox;
    edColumnValue2: TNumberBox;
    edHoldTime: TDateTimePicker;
    edRankingPosition1: TNumberBox;
    edRankingPosition2: TNumberBox;
    edRankingSum1: TNumberBox;
    edRankingSum2: TNumberBox;
    edReleaseTime: TDateTimePicker;
    edTimeIntervalFromDataObtained: TNumberBox;
    lblColumnValueAnd: TLabel;
    lblRankingPositionAnd: TLabel;
    lblRankingSumAnd: TLabel;
    lblTimeIntervalMs: TLabel;
    pnlBottom: TPanel;
    pnlColumnValue: TPanel;
    pnlColumnValue2: TPanel;
    pnlColumnValueBetween: TPanel;
    pnlConditions: TPanel;
    pnlHoldTime: TPanel;
    pnlRankingPosition: TPanel;
    pnlRankingPositionBetween: TPanel;
    pnlRankingSum: TPanel;
    pnlRankingSumBetween: TPanel;
    pnlReleaseTime: TPanel;
    pnlTimeIntervalFromDataObtained: TPanel;
    pnlValueExistInColumn: TPanel;
    rbColumnValue: TRadioButton;
    rbHoldTime: TRadioButton;
    rbRankingPosition: TRadioButton;
    rbRankingSum: TRadioButton;
    rbReleaseTime: TRadioButton;
    rbTimeIntervalFromDataObtained: TRadioButton;
    rbValueExistInColumn: TRadioButton;
    procedure cbColumnValueChange(Sender: TObject);
    procedure cbRankingPositionChange(Sender: TObject);
    procedure cbRankingSumChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure OnChangeEnabled(Sender: TObject);
    procedure OnChangePanelColour(Sender: TObject);
  private
    FColumns: TObjectDictionary<Integer, TColumnsInfo>;
    FColumnsInfo: TColumnsInfo;
    function CheckData: Boolean;
  public
    class function ShowDocument(aDialogMode: TDialogMode; AColumns: TObjectDictionary<Integer, TColumnsInfo>; var ColumnsInfo: TColumnsInfo): TModalResult;
    procedure Initialize;
    procedure Denitialize;
  end;

implementation

{$R *.dfm}

class function TfrmCandidateEmbargoColumn.ShowDocument(aDialogMode: TDialogMode; AColumns: TObjectDictionary<Integer, TColumnsInfo>; var ColumnsInfo: TColumnsInfo): TModalResult;
begin
  Result := mrCancel;
  with TfrmCandidateEmbargoColumn.Create(nil) do
  try
    DialogMode := aDialogMode;
    FColumns := AColumns;
    if (aDialogMode = dmInsert) then
      FColumnsInfo := TColumnsInfo.Create(stEmbargoColumn, iabIdle)
    else
      FColumnsInfo := ColumnsInfo;
    Initialize;
    if (ShowModal = mrOk) and (aDialogMode in [dmInsert, dmUpdate]) then
    begin
      Denitialize;
      ColumnsInfo := FColumnsInfo;
      Result := mrOk;
    end;
  finally
    Free;
  end;
end;

procedure TfrmCandidateEmbargoColumn.Initialize;
var
  Inequality: TInequalityType;
  Pair: TPair<Integer, TColumnsInfo>;
  ColumnsInfo: TColumnsInfo;
begin
  cbColumns.Items.Clear;
  cbColumnValue.Items.Clear;
  cbRankingPosition.Items.Clear;
  cbRankingSum.Items.Clear;
  cbTimeIntervalFromDataObtained.Items.Clear;
  cbValueExistInColumn.Items.Clear;
  for Inequality := iqAbove to iqBetween do
  begin
    cbColumnValue.Items.Add(Inequality.ToString);
    cbRankingPosition.Items.Add(Inequality.ToString);
    cbRankingSum.Items.Add(Inequality.ToString);
  end;

  if Assigned(FColumns) then
    for Pair in FColumns do
      if Pair.Value.SourceType in [stStaticList, stCandidateMarket, stTickColumn, stCalcColumn] then
      begin
        cbColumns.Items.AddObject(Pair.Value.Caption, TObject(Pair.Key));
        cbTimeIntervalFromDataObtained.Items.AddObject(Pair.Value.Caption, TObject(Pair.Key));
        cbValueExistInColumn.Items.AddObject(Pair.Value.Caption, TObject(Pair.Key));
      end;

  case DialogMode of
    dmInsert:
      begin
        Self.Caption                := 'Add Embargo';
        cbColumnValue.ItemIndex     := Ord(TInequalityType.iqBelow);
        cbRankingPosition.ItemIndex := Ord(TInequalityType.iqBelow);
        cbRankingSum.ItemIndex      := Ord(TInequalityType.iqAbove);
      end;
    dmUpdate, dmView:
      begin
        if (DialogMode = dmUpdate) then
        begin
          Self.Caption          := 'Edit Embargo';
          btnAddColumn.Caption  := 'Save Column';
        end
        else
        begin
          Self.Caption          := 'View Embargo Info';
          btnAddColumn.Enabled  := False;
        end;

        rbColumnValue.Checked                  := FColumnsInfo.EmbargoColumn.EmbargoType = etColumnValue;
        rbHoldTime.Checked                     := FColumnsInfo.EmbargoColumn.EmbargoType = etHoldTime;
        rbRankingPosition.Checked              := FColumnsInfo.EmbargoColumn.EmbargoType = etRankingPosition;
        rbRankingSum.Checked                   := FColumnsInfo.EmbargoColumn.EmbargoType = etRankingSum;
        rbReleaseTime.Checked                  := FColumnsInfo.EmbargoColumn.EmbargoType = etReleaseTime;
        rbValueExistInColumn.Checked           := FColumnsInfo.EmbargoColumn.EmbargoType = etColumnValueExists;
        rbTimeIntervalFromDataObtained.Checked := FColumnsInfo.EmbargoColumn.EmbargoType = etTimeInterval;
        cbReleaseWorkingDays.Checked           := FColumnsInfo.EmbargoColumn.WorkingDays;
        cbHoldWorkingDays.Checked              := FColumnsInfo.EmbargoColumn.WorkingDays;

        cbColumnValue.ItemIndex     := Ord(FColumnsInfo.EmbargoColumn.InequalityType);
        cbRankingPosition.ItemIndex := Ord(FColumnsInfo.EmbargoColumn.InequalityType);
        cbRankingSum.ItemIndex      := Ord(FColumnsInfo.EmbargoColumn.InequalityType);

        edColumnValue1.ValueFloat               := FColumnsInfo.EmbargoColumn.Value1;
        edColumnValue2.ValueFloat               := FColumnsInfo.EmbargoColumn.Value2;
        edHoldTime.Time                         := FColumnsInfo.EmbargoColumn.TimeStamp;
        edRankingPosition1.ValueInt             := Trunc(FColumnsInfo.EmbargoColumn.Value1);
        edRankingPosition2.ValueInt             := Trunc(FColumnsInfo.EmbargoColumn.Value2);
        edRankingSum1.ValueFloat                := FColumnsInfo.EmbargoColumn.Value1;
        edRankingSum2.ValueFloat                := FColumnsInfo.EmbargoColumn.Value2;
        edReleaseTime.Time                      := FColumnsInfo.EmbargoColumn.TimeStamp;
        edTimeIntervalFromDataObtained.ValueInt := FColumnsInfo.EmbargoColumn.TimeInterval;

        for var i := 0 to cbColumns.Items.Count - 1 do
          if FColumns.ContainsKey(Integer(cbColumns.Items.Objects[i])) then
          begin
            ColumnsInfo := FColumns.Items[Integer(cbColumns.Items.Objects[i])];
            if ColumnsInfo.RecordId = FColumnsInfo.EmbargoColumn.RefRecordId then
            begin
              cbValueExistInColumn.ItemIndex           := i;
              cbTimeIntervalFromDataObtained.ItemIndex := i;
              cbColumns.ItemIndex                      := i;
              Break;
            end;
          end;

        cbColumnValueChange(nil);
        cbColumnValueChange(nil);
        cbRankingPositionChange(nil);
        cbRankingSumChange(nil);
        OnChangePanelColour(nil);
      end;
  end;
  OnChangeEnabled(nil);
end;

procedure TfrmCandidateEmbargoColumn.Denitialize;
var
  RefColumn: TColumnsInfo;
begin
  if rbReleaseTime.Checked then
  begin
    FColumnsInfo.EmbargoColumn.EmbargoType := etReleaseTime;
    FColumnsInfo.EmbargoColumn.TimeStamp   := edReleaseTime.Time;
    FColumnsInfo.EmbargoColumn.WorkingDays := cbReleaseWorkingDays.Checked;
  end
  else if rbHoldTime.Checked then
  begin
    FColumnsInfo.EmbargoColumn.EmbargoType := etHoldTime;
    FColumnsInfo.EmbargoColumn.TimeStamp   := edHoldTime.Time;
    FColumnsInfo.EmbargoColumn.WorkingDays := cbHoldWorkingDays.Checked;
  end
  else if rbRankingSum.Checked then
  begin
    FColumnsInfo.EmbargoColumn.EmbargoType    := etRankingSum;
    FColumnsInfo.EmbargoColumn.InequalityType := TInequalityType(cbRankingSum.ItemIndex);
    FColumnsInfo.EmbargoColumn.Value1         := edRankingSum1.ValueFloat;
    FColumnsInfo.EmbargoColumn.Value2         := edRankingSum2.ValueFloat;
  end
  else if rbRankingPosition.Checked then
  begin
    FColumnsInfo.EmbargoColumn.EmbargoType    := etRankingPosition;
    FColumnsInfo.EmbargoColumn.InequalityType := TInequalityType(cbRankingPosition.ItemIndex);
    FColumnsInfo.EmbargoColumn.Value1         := edRankingPosition1.ValueInt;
    FColumnsInfo.EmbargoColumn.Value2         := edRankingPosition2.ValueInt;
  end
  else if rbColumnValue.Checked then
  begin
    if (cbColumns.ItemIndex > -1) and FColumns.ContainsKey(Integer(cbColumns.Items.Objects[cbColumns.ItemIndex])) then
    begin
      RefColumn := FColumns.Items[Integer(cbColumns.Items.Objects[cbColumns.ItemIndex])];
      FColumnsInfo.EmbargoColumn.EmbargoType    := etColumnValue;
      FColumnsInfo.EmbargoColumn.InequalityType := TInequalityType(cbColumnValue.ItemIndex);
      FColumnsInfo.EmbargoColumn.Value1         := edColumnValue1.ValueFloat;
      FColumnsInfo.EmbargoColumn.Value2         := edColumnValue2.ValueFloat;
      FColumnsInfo.EmbargoColumn.RefRecordId    := RefColumn.RecordId;
      New(FColumnsInfo.EmbargoColumn.ColumnsInfo);
      FColumnsInfo.EmbargoColumn.ColumnsInfo^ := RefColumn;
    end;
  end
  else if rbValueExistInColumn.Checked then
  begin
    if (cbValueExistInColumn.ItemIndex > -1) and FColumns.ContainsKey(Integer(cbValueExistInColumn.Items.Objects[cbValueExistInColumn.ItemIndex])) then
    begin
      RefColumn := FColumns.Items[Integer(cbValueExistInColumn.Items.Objects[cbValueExistInColumn.ItemIndex])];
      FColumnsInfo.EmbargoColumn.EmbargoType := etColumnValueExists;
      FColumnsInfo.EmbargoColumn.RefRecordId := RefColumn.RecordId;
      New(FColumnsInfo.EmbargoColumn.ColumnsInfo);
      FColumnsInfo.EmbargoColumn.ColumnsInfo^ := RefColumn;
    end;
  end
  else if rbTimeIntervalFromDataObtained.Checked then
  begin
    if (cbTimeIntervalFromDataObtained.ItemIndex > -1) and FColumns.ContainsKey(Integer(cbTimeIntervalFromDataObtained.Items.Objects[cbTimeIntervalFromDataObtained.ItemIndex])) then
    begin
      RefColumn := FColumns.Items[Integer(cbTimeIntervalFromDataObtained.Items.Objects[cbTimeIntervalFromDataObtained.ItemIndex])];
      FColumnsInfo.EmbargoColumn.EmbargoType  := etTimeInterval;
      FColumnsInfo.EmbargoColumn.RefRecordId  := RefColumn.RecordId;
      FColumnsInfo.EmbargoColumn.TimeInterval := edTimeIntervalFromDataObtained.ValueInt;
      New(FColumnsInfo.EmbargoColumn.ColumnsInfo);
      FColumnsInfo.EmbargoColumn.ColumnsInfo^ := RefColumn;
    end;
  end;

  FColumnsInfo.Weight := 0;
end;

procedure TfrmCandidateEmbargoColumn.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if (ModalResult = mrOk) then
    CanClose := CheckData;
end;

function TfrmCandidateEmbargoColumn.CheckData: Boolean;
resourcestring
  rcSelectInequalityType = 'Please select "Type Of Inequality"' + sLineBreak;
  rcSelectColumnSpecific = 'Please select "Column With a Specific Value"' + sLineBreak;
  rcSelectColumnExist    = 'Please select "Value Exist In Column"' + sLineBreak;
var
  Problems: string;
begin
  Problems := '';
  if rbRankingSum.Checked and (cbRankingSum.ItemIndex = -1) then
  begin
    Problems := rcSelectInequalityType;
    SetFocusSafely(cbRankingSum);
  end
  else if rbRankingPosition.Checked and (cbRankingPosition.ItemIndex = -1) then
  begin
    Problems := rcSelectInequalityType;
    SetFocusSafely(cbRankingPosition);
  end
  else if rbColumnValue.Checked then
  begin
    if (cbColumns.ItemIndex = -1) then
    begin
      Problems := rcSelectColumnSpecific;
      SetFocusSafely(cbColumns);
    end;
    if (cbColumnValue.ItemIndex = -1) then
    begin
      Problems := rcSelectInequalityType;
      SetFocusSafely(cbColumnValue);
    end;
  end
  else if rbValueExistInColumn.Checked then
  begin
    if (cbValueExistInColumn.ItemIndex = -1) then
    begin
      Problems := rcSelectColumnExist;
      SetFocusSafely(cbValueExistInColumn);
    end;
  end
  else if rbTimeIntervalFromDataObtained.Checked then
  begin
    if (cbTimeIntervalFromDataObtained.ItemIndex = -1) then
    begin
      Problems := rcSelectColumnExist;
      SetFocusSafely(cbTimeIntervalFromDataObtained);
    end;
  end;

  Result := Problems.IsEmpty;
  if not Result then
    ShowMessage(Problems);
end;

procedure TfrmCandidateEmbargoColumn.OnChangeEnabled(Sender: TObject);
begin
  pnlColumnValue.Enabled                  := rbColumnValue.Checked;
  pnlHoldTime.Enabled                     := rbHoldTime.Checked;
  pnlRankingPosition.Enabled              := rbRankingPosition.Checked;
  pnlRankingSum.Enabled                   := rbRankingSum.Checked;
  pnlReleaseTime.Enabled                  := rbReleaseTime.Checked;
  pnlTimeIntervalFromDataObtained.Enabled := rbTimeIntervalFromDataObtained.Checked;
  pnlValueExistInColumn.Enabled           := rbValueExistInColumn.Checked;
end;

procedure TfrmCandidateEmbargoColumn.OnChangePanelColour(Sender: TObject);
begin
  if rbColumnValue.Checked then
    pnlColumnValue.Color := clInfoBk;
  if rbHoldTime.Checked then
    pnlHoldTime.Color := clInfoBk;
  if rbRankingPosition.Checked then
    pnlRankingPosition.Color := clInfoBk;
  if rbRankingSum.Checked then
    pnlRankingSum.Color := clInfoBk;
  if rbReleaseTime.Checked then
    pnlReleaseTime.Color := clInfoBk;
  if rbValueExistInColumn.Checked then
    pnlValueExistInColumn.Color := clInfoBk;
  if rbTimeIntervalFromDataObtained.Checked then
    pnlTimeIntervalFromDataObtained.Color := clInfoBk;
end;

procedure TfrmCandidateEmbargoColumn.cbColumnValueChange(Sender: TObject);
begin
  pnlColumnValueBetween.Visible := cbColumnValue.ItemIndex = Ord(TInequalityType.iqBetween);
end;

procedure TfrmCandidateEmbargoColumn.cbRankingPositionChange(Sender: TObject);
begin
  pnlRankingPositionBetween.Visible := cbRankingPosition.ItemIndex = Ord(TInequalityType.iqBetween);
end;

procedure TfrmCandidateEmbargoColumn.cbRankingSumChange(Sender: TObject);
begin
  pnlRankingSumBetween.Visible := cbRankingSum.ItemIndex = Ord(TInequalityType.iqBetween);
end;

end.
