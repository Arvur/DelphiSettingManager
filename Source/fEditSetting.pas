{-----------------------------------------------------------------------------
 Unit Name: fEditSetting
 Author:    Original code by Lachlan Gemmell (http://lachlan.gemmell.com/),
            maintained and improved by Erwien Saputra.
 Purpose:   Encapsulates all interaction with the Registry.

 History:
 12/27/04 - Received the file from Lachlan.
 01/02/05 - Updated to work with new ISettingCollection.
            A lot of updates. Refactored the logic and creates several helper
            classes.
 03/01/05 - Added buttons.
 04/08/05 - Updated this form, giving this form the ability to persist its
            setting. This form uses my new setting framework.
 06/18/05 - Updated SetTopPanelHeight and GetTopPanelHeight.
-----------------------------------------------------------------------------}
unit fEditSetting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, TreeViewController,
  DelphiSettingRegistry, Buttons, ImgList, ValueNamesProvider, ToolWin, ActnMan,
  ActnCtrls, XPStyleActnCtrls, ActnList, Menus, SettingPersistent;

type
  TEditSettingProperties = class;

  IEditSetting = interface
  ['{D8825E5B-D312-4277-9605-0DC278F65D7B}']
    function GetLeftSettingPath : string;
    function GetRightSettingPath : string;
    function GetLeftSettingName : string;
    function GetRightSettingName : string;
    procedure SetLeftSettingPath (const APath : string);
    procedure SetRightSettingPath (const APath : string);
    procedure SetLeftSettingName (const ASettingName : string);
    procedure SetRightSettingName (const ASettingName : string);

    procedure Execute;

    property LeftSettingPath : string read GetLeftSettingPath write
        SetLeftSettingPath;
    property LeftSettingName : string read GetLeftSettingName write
        SetLeftSettingName;
    property RightSettingPath : string read GetRightSettingPath write
        SetRightSettingPath;
    property RightSettingName : string read GetRightSettingName write
        SetRightSettingName;

  end;

  TfrmEditSetting = class(TForm, IEditSetting)
    pnlTop: TPanel;
    Splitter1: TSplitter;
    pnlValuesLeft: TPanel;
    pnlValuesRight: TPanel;
    pnlLeft: TPanel;
    spTree: TSplitter;
    pnlRight: TPanel;
    tvLeft: TTreeView;
    tvRight: TTreeView;
    lblLeft: TLabel;
    lblRight: TLabel;
    lbLeftValues: TListBox;
    lbRightValues: TListBox;
    pnlBottom: TPanel;
    pnlButton: TPanel;
    btnClose: TBitBtn;
    imgTree: TImageList;
    memValueLeft: TMemo;
    memValueRight: TMemo;
    actKeyValue: TActionList;
    actLeftCopyKeyTo: TAction;
    actLeftDeleteKey: TAction;
    actLeftCopyNameTo: TAction;
    actLeftDeleteName: TAction;
    actLeftClearValue: TAction;
    actRightCopyKeyTo: TAction;
    actRightCopyNameTo: TAction;
    actRightDeleteName: TAction;
    actRightClearValue: TAction;
    spValues: TSplitter;
    popLeftTree: TPopupMenu;
    actRightDeleteKey: TAction;
    LeftCopyKeyTo: TMenuItem;
    LeftDeleteKey: TMenuItem;
    PoRightTree: TPopupMenu;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    popLeftValue: TPopupMenu;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    popRightValues: TPopupMenu;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    popLeftMemo: TPopupMenu;
    MenuItem35: TMenuItem;
    popRightMemo: TPopupMenu;
    MenuItem50: TMenuItem;
    atbLeftNames: TActionToolBar;
    atbRightNames: TActionToolBar;
    acmKeyValue: TActionManager;
    imgKeyValues: TImageList;
    atbLeftKey: TActionToolBar;
    atbRightKey: TActionToolBar;
    procedure ValueNamesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure TreeViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure popLeftValuePopup(Sender: TObject);
    procedure actRightClearValueExecute(Sender: TObject);
    procedure actRightDeleteNameExecute(Sender: TObject);
    procedure actRightDeleteKeyExecute(Sender: TObject);
    procedure actLeftClearValueExecute(Sender: TObject);
    procedure actLeftDeleteNameExecute(Sender: TObject);
    procedure actLeftDeleteKeyExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure TreeMenuPopup(Sender: TObject);
    procedure actLeftCopyNameToExecute(Sender: TObject);
    procedure actRightCopyNameToExecute(Sender: TObject);
    procedure actRightCopyKeyToExecute(Sender: TObject);
    procedure actLeftCopyKeyToExecute(Sender: TObject);
    procedure lbLeftValuesClick(Sender: TObject);
    procedure tvLeftGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure tvLeftGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure spTreeCanResize(Sender: TObject; var NewSize: Integer;
      var Accept: Boolean);
    procedure spTreeMoved(Sender: TObject);
  private
    { Private declarations }
    FLeftSettingName : string;
    FLeftRegistry : IDelphiSettingRegistry;
    FLeftController : ITreeViewController;
    FLeftValueNamesProvider : IValueNamesProvider;

    FRightSettingName : string;
    FRightRegistry : IDelphiSettingRegistry;
    FRightController : ITreeViewController;
    FRightValueNamesProvider : IValueNamesProvider;

    FSynchronizer : IDelphiSettingSynchronizer;
    FLeftReadOnly: Boolean;
    FRightReadOnly: Boolean;

    FSettingProperties : TEditSettingProperties;

    function GetLeftSettingPath : string;
    function GetRightSettingPath : string;
    function GetLeftSettingName : string;
    function GetRightSettingName : string;
    procedure SetLeftSettingPath (const APath : string);
    procedure SetRightSettingPath (const APath : string);
    procedure SetLeftSettingName (const ASettingName : string);
    procedure SetRightSettingName (const ASettingName : string);

    procedure Execute;

    procedure BuildListBoxes (const ListA, ListB : TStrings);
    procedure UpdateListBox (const Intf : IInterface);
    procedure DisplayValue (const Intf : IInterface);
    procedure SyncListBox (const ANewSelection, ATopIndex : integer);

    procedure CopyValue (const SourceListBox : TListBox;
        const SourceProvider, TargetProvider : IValueNamesProvider);
    procedure DeleteCurrentKey (const DelphiRegistry : IDelphiSettingRegistry;
      const TreeView : TTreeView);
    procedure DeleteSelectedValue (const Provider : IValueNamesProvider;
      const SelectedItem, TopItem : integer);
    procedure ClearSelectedValue(const Provider: IValueNamesProvider;
      const SelectedItem, TopItem: integer);
  public
    { Public declarations }
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  end;

  TEditSettingProperties = class (TFormSetting)
  private
    function GetLeftPanelWidth: integer;
    function GetTopPanelHeight: integer;
    procedure SetPanelWidth(const Value: integer);
    procedure SetTopPanelHeight(const Value: integer);
  published
    property TopPanelHeight : integer read GetTopPanelHeight write SetTopPanelHeight;
    property LeftPanelWidth : integer read GetLeftPanelWidth write SetPanelWidth;
  end;

implementation

uses
  IntfObserver;

const
  //Captions for the menus.
  COPY_KEY_CAPTION    = 'Copy "%0:s" To "%1:s"';
  DELETE_KEY_CAPTION  = 'Delete Key "%s"';
  COPY_NAME_CAPTION   = 'Copy "%s" To "%s"';
  DELETE_NAME_CAPTION = 'Delete Name "%s"';
  CLEAR_VALUE         = 'Clear Value';

  READ_ONLY_SUFFIX    = ' (Read Only)';

  CAPTION_COLOR : array [false..true] of TColor = (clWindowText, clRed);
{$R *.dfm}

{ TForm1 }

constructor TfrmEditSetting.Create (AOwner : TComponent);
begin
  inherited;
  //Create the DelphiSettingRegistry and Disable the observer pattern here.
  FLeftRegistry := TDelphiSettingRegistry.Create;
  (FLeftRegistry as ISubject).Enabled := false;
  //Create the treeview controller, bind the DelphiSettingRegistry and the
  //TreeView.
  FLeftController := CreateTreeViewController;
  FLeftController.BindDelphiSettingRegistry(FLeftRegistry);
  FLeftController.BindTreeView (tvLeft);
  //Create the ValueNamesProvider. It is bound with the DelphiSettingRegistry.
  //Attach the events.
  FLeftValueNamesProvider := GetValueNamesProvider (FLeftRegistry);
  FLeftValueNamesProvider.OnValueNamesChanged := UpdateListBox;
  FLeftValueNamesProvider.OnAsStringChanged := DisplayValue;
  (FLeftRegistry as ISubject).Enabled := true;

  //Create the DelphiSettingRegistry and Disable the observer pattern here.
  FRightRegistry := TDelphiSettingRegistry.Create;
  (FRightRegistry as ISubject).Enabled := false;
  //Create the treeview controller, bind the DelphiSettingRegistry and the
  //TreeView.
  FRightController := CreateTreeViewController;
  FRightController.BindTreeView (tvRight);
  FRightController.BindDelphiSettingRegistry (FRightRegistry);
  //Create the ValueNamesProvider. It is bound with the DelphiSettingRegistry.
  //Attach the events.
  FRightValueNamesProvider := GetValueNamesProvider (FRightRegistry);
  FRightValueNamesProvider.OnValueNamesChanged := UpdateListBox;
  FRightValueNamesProvider.OnAsStringChanged := DisplayValue;
  (FRightRegistry as ISubject).Enabled := true;

  //Create the synchronizer.
  FSynchronizer := GetDelphiSettingSynchronizer;

  self.atbLeftNames.Orientation := boRightToLeft;
  self.atbLeftKey.Orientation   := boRightToLeft;

  FSettingProperties := TEditSettingProperties.Create (self);
  SettingPersistent.GetIniPersistentManager.LoadSetting (FSettingProperties);
end;

function TfrmEditSetting.GetLeftSettingPath: string;
begin
  Result := FLeftRegistry.SettingPath;
end;

function TfrmEditSetting.GetRightSettingPath: string;
begin
  Result := FRightRegistry.SettingPath;
end;

//Set the left setting path. It opens the FLeftRegistryProvider, setup the
//controller, add the treeview controller to the synchronizer.
procedure TfrmEditSetting.SetLeftSettingPath(const APath: string);
begin
  //Open the DelphiSettingRegistry, build the nodes based on the new path.
  FLeftRegistry.OpenSetting (APath);
  FLeftController.BuildNode (nil);
  //Add the controller to the synchronizer.
  FSynchronizer.AddTreeViewController (FLeftController);
  //Let the ValueNamesProvider knows what is the setting path of the
  //DelphiSettingRegistry. This value is necessary to get the relative path for
  //the ValueNamesProvider.
  FLeftValueNamesProvider.SetSettingPath (FLeftRegistry.SettingPath);

  //If the Path does not contain CustomSetting string, this path is the path to
  //the original Delphi IDE key. Disables the menus.
  FLeftReadOnly := Pos ('CustomSettings', APath) = 0;

  //Disables the menus when the custom setting is read-only.
  if FLeftReadOnly = true then begin
    self.actLeftDeleteKey.Enabled   := false;
    self.actLeftDeleteName.Enabled  := false;
    self.actLeftClearValue.Enabled  := false;
    self.actRightCopyKeyTo.Enabled  := false;
    self.actRightCopyNameTo.Enabled := false;

    SetLeftSettingName (FLeftSettingName);
  end;
end;

//Set the right pane. It opens the FRightRegistryProvider, setup the
//controller, add the treeview controller to the synchronizer.
procedure TfrmEditSetting.SetRightSettingPath(const APath: string);
begin
  FRightRegistry.OpenSetting (APath);
  FRightController.BuildNode (nil);
  FSynchronizer.AddTreeViewController (FRightController);
  //Let the ValueNamesProvider knows what is the setting path of the
  //DelphiSettingRegistry. This value is necessary to get the relative path for
  //the ValueNamesProvider.
  FRightValueNamesProvider.SetSettingPath (FRightRegistry.SettingPath);

  //If the Path does not contain CustomSetting string, this path is the path to
  //the original Delphi IDE key. Disables the menus.
  FRightReadOnly := Pos ('CustomSettings', APath) = 0;
  if FRightReadOnly = true then begin
    self.actRightDeleteKey.Enabled  := false;
    self.actRightDeleteName.Enabled := false;
    self.actRightClearValue.Enabled := false;
    self.actLeftCopyKeyTo.Enabled   := false;
    self.actLeftCopyNameTo.Enabled  := false;

    SetRightSettingName (FRightSettingName);
    lblRight.Caption := lblRight.Caption + ' (Read Only)';
  end;
end;

procedure TfrmEditSetting.Execute;
begin
  ShowModal;
end;

function TfrmEditSetting.GetLeftSettingName: string;
begin
  Result := FLeftSettingName;
end;

function TfrmEditSetting.GetRightSettingName: string;
begin
  Result := FRightSettingName;
end;

//UI stuff. Assign the name Delphi Setting Registry on the left pane.
procedure TfrmEditSetting.SetLeftSettingName(const ASettingName: string);
begin
  FLeftSettingName := ASettingName;

  if FLeftReadOnly = true then
    FLeftSettingName := ASettingName + READ_ONLY_SUFFIX;

  lblLeft.Caption    := FLeftSettingName;
  lblLeft.Font.Color := CAPTION_COLOR [FLeftReadOnly];

end;

//UI stuff. Assign the name Delphi Setting Registry on the right pane.
procedure TfrmEditSetting.SetRightSettingName(const ASettingName: string);
begin
  FRightSettingName := ASettingName;

  if FRightReadOnly = true then
    FRightSettingName := ASettingName + READ_ONLY_SUFFIX;

  lblRight.Caption    := FRightSettingName;
  lblRight.Font.Color := CAPTION_COLOR [FRightReadOnly];
end;

//This event is shared with spTree and spValues. pnlLeft is controlled by spTree
//while pnlValuesLeft is controller by spValues. This event handler synchronizes
//these two panels, that both will always be at the same size.
procedure TfrmEditSetting.spTreeMoved(Sender: TObject);
var
  ControlledPanel,
  OtherPanel : TPanel;
begin
  //Get the other panel that is not controlled by the Sender.
  if (Sender = spTree) then begin
    ControlledPanel := pnlLeft;
    OtherPanel := pnlValuesLeft;
  end
  else begin
    ControlledPanel := pnlValuesLeft;
    OtherPanel := pnlLeft;
  end;

  //Synchronizes both panel.
  OtherPanel.Width := ControlledPanel.Width;
end;

procedure TfrmEditSetting.spTreeCanResize(Sender: TObject; var NewSize: Integer;
  var Accept: Boolean);
begin
  Accept := (NewSize > 30);
end;

//Load the closed folder image if an item is drawn.
procedure TfrmEditSetting.tvLeftGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  Node.ImageIndex := 0;
end;

//Load the open folder image if an item is selected.
procedure TfrmEditSetting.tvLeftGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  Node.SelectedIndex := 1;
end;

//This method synchronizes the ListBox values. It compares two TStrings line by
//line, and if two lines are not identical, that means there is a gap. If a gap
//if found, this method will synchronizes the listbox by inserting an empty
//string. The end result, two string list that each line will have same values
//or the counterpart is blank.
//If one TStrings is empty, it will remain empty. It won't be filled with blank
//lines.
//Both string must not be auto sorted.
procedure TfrmEditSetting.BuildListBoxes(const ListA, ListB: TStrings);
var
  CompareResult,
  LoopA, LoopB : integer;
  StrA, StrB : string;
begin
  LoopA := 0;
  LoopB := 0;

  //Iterates both TStrings.
  while (LoopA < ListA.Count) and
        (LoopB < ListB.Count) do begin
    //Get the string for the first TString
    if LoopA < ListA.Count then
      StrA := ListA [LoopA]
    else
      StrA := EmptyStr;

    //Get the string for the second TString
    if LoopB < ListB.Count then
      StrB := ListB [LoopB]
    else
      StrB := EmptyStr;

    //Compare both strings. if the result is not identical, insert blank line
    //to at the location of the string that has greater value.
    CompareResult := CompareText (StrA, StrB);

    if (CompareResult > 0) then
      ListA.Insert (LoopA, EmptyStr)
    else if (CompareResult < 0) then
      ListB.Insert (LoopB, EmptyStr);

    Inc (LoopA);
    Inc (LoopB);
  end;

  //If both lines do not have same count, that means at the end of one TStrings,
  //there is no more text that can be compared. Add blank lines to the TStrings
  //that has less line items.
  while (ListA.Count <> ListB.Count) and
        ((ListA.Count <> 0) and (ListB.Count <> 0)) do
    if ListA.Count < ListB.Count then
      ListA.Append (EmptyStr)
    else
      ListB.Append (EmptyStr);
end;

//Event handler for ValueNamesProvider.OnValueNamesChanged. If the provider
//is changed, this method will populate the listbox. This event handler is
//shared by FLeftValueNamesProvider and FRightValueNamesProvider.
procedure TfrmEditSetting.UpdateListBox(const Intf: IInterface);
var
  NamesProvider,
  OtherProvider : IValueNamesProvider;
  StrActive, strOther : TStrings;
begin
  //Get the left and the right provider, based on Intf parameter.
  NamesProvider := (Intf as IValueNamesProvider);

  if (NamesProvider = FLeftValueNamesProvider) then begin
    StrActive := lbLeftValues.Items;
    StrOther  := lbRightValues.Items;
    OtherProvider := FRightValueNamesProvider;
  end
  else begin
    StrActive := lbRightValues.Items;
    StrOther  := lbLeftValues.Items;
    OtherProvider := FLeftValueNamesProvider;
  end;

  if OtherProvider = nil then
    Exit;

  StrActive.BeginUpdate;
  StrOther.BeginUpdate;
  try
    //If the LeftProvider and the RightProvider have identical RelativePath,
    //this means that both providers have that key and are synchronized. Refresh
    //the ListBoxes for both ListBoxes and then synchronize the list boxes.
    if NamesProvider.RelativePath = OtherProvider.RelativePath then begin
      StrActive.Assign (NamesProvider.ValueNames);
      StrOther.Assign (OtherProvider.ValueNames);
      BuildListBoxes (StrActive, StrOther);
    end
    else begin
      //If the LeftProvider and the RightProvider does not point to the same
      //relative path, assign it to one and clear the other list box. This can
      //be one of two things, the "Other" provider does not have the key, or
      //both provider are not synchronized yet.
      StrActive.Assign (NamesProvider.ValueNames);
      StrOther.Clear;
    end;

  finally
    StrActive.EndUpdate;
    StrOther.EndUpdate;
  end;
end;

//Event handler for ValueNamesProvider.OnAsStringChanged. If the names have
//values it will be displayed in the memo. If the left and the right are not
//match, it will be displayed in green. This event handler is shared by
//FLeftValueNamesProvider and FRightValueNamesProvider.
procedure TfrmEditSetting.DisplayValue(const Intf: IInterface);
var
  NamesProvider : IValueNamesProvider;
  Color : TColor;
begin
  NamesProvider := Intf as IValueNamesProvider;

  //Find out which TMemo it should update.
  if NamesProvider = FLeftValueNamesProvider then
    memValueLeft.Lines.Text := NamesProvider.AsString
  else
    memValueRight.Lines.Text := NamesProvider.AsString;

  //If both TMemos have same value, set the color to normal, otherwise make it
  //green.
  if SameText (memValueLeft.Text, memValueRight.Text) then
    Color := clWindowText
  else
    Color := clGreen;

  memValueLeft.Font.Color  := Color;
  memValueRight.Font.Color := Color;
end;

//Event handler when the list box is clicked. It will synchronizes both list
//boxes. This event handler is shared by lbLeftValues and lbRightValues.
procedure TfrmEditSetting.lbLeftValuesClick(Sender: TObject);
var
  ListBox : TListBox;
begin
  ListBox := (Sender as TListBox);
  SyncListBox (ListBox.ItemIndex, ListBox.TopIndex);
end;

//This method synchronizes the list box selection.
procedure TfrmEditSetting.SyncListBox(const ANewSelection, ATopIndex: integer);
  procedure SetSelection (ListBox : TListBox;
      const NamesProvider : IValueNamesProvider);
  var
    ClickEvent : TNotifyEvent;
    SelectedName : string;
  begin
    //Disable the event handler, otherwise it might trapped into infinite loop.
    ClickEvent := ListBox.OnClick;
    ListBox.OnClick := nil;
    SelectedName := EmptyStr;

    //Set the ItemIndex and get the name. The name can be an empty string. Empty
    //string means that the selected name does not exist in the NameProvider.
    if ListBox.Items.Count > ANewSelection then begin
      ListBox.ItemIndex := ANewSelection;
      SelectedName := ListBox.Items [ANewSelection];
    end;

    //Synchronizes the TopIndex. This will synchonizes not only the selection
    //but also the item positions.
    if ListBox.Items.Count > ATopIndex then
      ListBox.TopIndex := ATopIndex;

    //Update the NameProvider.SelectedIndex. Remember, if SelectedName is an
    //empty string, the selected index is -1.  The value name does not exist.
    NamesProvider.SelectedIndex := NamesProvider.GetNameIndex (SelectedName);

    //Restore the event handler.
    ListBox.OnClick := ClickEvent;
  end;
begin
  SetSelection (lbLeftValues, FLeftValueNamesProvider);
  SetSelection (lbRightValues, FRightValueNamesProvider);
end;

//Copy value copies a value name from the SourceProvider to the TargetProvider.
//ListBox parameter is needed, after copying the name/value, the ListBoxes need
//to be synchronized.
//If the Name exist on both SourceProvider, the value will be copied.
procedure TfrmEditSetting.CopyValue(const SourceListBox: TListBox;
  const SourceProvider, TargetProvider: IValueNamesProvider);
var
  TopIndex,
  SelectedIndex : integer;
begin
  TopIndex      := SourceListBox.TopIndex;
  SelectedIndex := SourceListBox.ItemIndex;
  TargetProvider.CopySelectedValue (SourceProvider);
  SyncListBox (SelectedIndex, TopIndex);
end;

//Delete a key. There is no check whether the treeview is read-only or not.
procedure TfrmEditSetting.DeleteCurrentKey(
  const DelphiRegistry: IDelphiSettingRegistry; const TreeView : TTreeView);
const
  CONFIRM_DELETE_KEY = 'Are you sure you want to delete "%s"?';
var
  Node : TTreeNode;
  KeyName : string;
begin
  Node := TreeView.Selected;
  KeyName := Node.Text;

  if (MessageDlg (Format (CONFIRM_DELETE_KEY, [KeyName]), mtConfirmation,
                  [mbYes, mbNo], 0) = mrNo) then
    Exit;

  //Delete the current key.
  DelphiRegistry.DeleteCurrentKey;
  //The Treeview Controller does not rebuild the nodes when a node is deleted.
  //It is better to delete the node rather than clears the treeview and rebuild
  //everything.
  Node.Delete;
end;

//Delete selected value name, and synchronize the list box selection and top
//position.
procedure TfrmEditSetting.DeleteSelectedValue(
  const Provider: IValueNamesProvider; const SelectedItem, TopItem : integer);
begin
  Provider.DeleteSelectedValue;

  if (SelectedItem < lbLeftValues.Items.Count) then begin
    lbLeftValues.Selected [SelectedItem] := true;
    lbLeftValues.TopIndex := TopItem;
  end;

  if (SelectedItem < lbRightValues.Items.Count) then begin
    lbRightValues.Selected [SelectedItem] := true;
    lbRightValues.TopIndex := TopItem;
  end;
end;

//Clear the selected value.
procedure TfrmEditSetting.ClearSelectedValue(
  const Provider: IValueNamesProvider; const SelectedItem, TopItem : integer);
begin
  Provider.ClearSelectedValue;

  if (SelectedItem < lbLeftValues.Items.Count) then begin
    lbLeftValues.Selected [SelectedItem] := true;
    lbLeftValues.TopIndex := TopItem;
  end;

  if (SelectedItem < lbRightValues.Items.Count) then begin
    lbRightValues.Selected [SelectedItem] := true;
    lbRightValues.TopIndex := TopItem;
  end;
end;

procedure TfrmEditSetting.actLeftCopyKeyToExecute(Sender: TObject);
begin
  //Add Key.  DelphiRegistrySetting will add the missing registry key, then it
  //will notify the changes and at the same time setting the newly created key
  //as the current key. TreeViewController as the observer will fail to find
  //related Node, as the key was just created. It then creates the node.
  //The OnChange event will get refired as side effect.
  FRightRegistry.AddKey (FLeftValueNamesProvider.RelativePath);
end;

procedure TfrmEditSetting.actRightCopyKeyToExecute(Sender: TObject);
begin
  //Look at comment at the actLeftCopyKeyToExecute event handler.
  FLeftRegistry.AddKey (FRightValueNamesProvider.RelativePath);
end;

procedure TfrmEditSetting.actLeftDeleteKeyExecute(Sender: TObject);
begin
  DeleteCurrentKey (FLeftRegistry, tvLeft);
end;

procedure TfrmEditSetting.actRightDeleteKeyExecute(Sender: TObject);
begin
  DeleteCurrentKey (FRightRegistry, tvRight);
end;

procedure TfrmEditSetting.actRightCopyNameToExecute(Sender: TObject);
begin
  CopyValue (lbRightValues, FRightValueNamesProvider, FLeftValueNamesProvider);
end;

procedure TfrmEditSetting.actLeftCopyNameToExecute(Sender: TObject);
begin
  CopyValue (lbLeftValues, FLeftValueNamesProvider, FRightValueNamesProvider);
end;

procedure TfrmEditSetting.actLeftDeleteNameExecute(Sender: TObject);
begin
  DeleteSelectedValue (FLeftValueNamesProvider, lbLeftValues.ItemIndex,
      lbLeftValues.TopIndex);
end;

procedure TfrmEditSetting.actRightDeleteNameExecute(Sender: TObject);
begin
  DeleteSelectedValue (FRightValueNamesProvider, lbRightValues.ItemIndex,
      lbRightValues.TopIndex);
end;

procedure TfrmEditSetting.actLeftClearValueExecute(Sender: TObject);
begin
  ClearSelectedValue (FLeftValueNamesProvider, lbLeftValues.ItemIndex,
                      lbLeftValues.TopIndex);
end;

procedure TfrmEditSetting.actRightClearValueExecute(Sender: TObject);
begin
  ClearSelectedValue (FRightValueNamesProvider, lbRightValues.ItemIndex,
                      lbRightValues.TopIndex);
end;

//This event handler is shared by popLeftTree and popRightTree.
//This menu does not enable or disable the menus based on the read-only property
//as the menus is already disabled on SetLeftSettingPath and SetRightSettingPath
procedure TfrmEditSetting.TreeMenuPopup(Sender: TObject);
begin
  if Sender = popLeftTree then begin
    actLeftCopyKeyTo.Caption := Format (COPY_KEY_CAPTION,
                                        [tvLeft.Selected.Text, FRightSettingName]);
    actLeftDeleteKey.Caption := Format (DELETE_KEY_CAPTION,
                                        [tvLeft.Selected.Text]);
  end
  else begin
    actRightCopyKeyTo.Caption := Format (COPY_KEY_CAPTION,
                                         [tvRight.Selected.Text, FLeftSettingName]);
    actRightDeleteKey.Caption := Format (DELETE_KEY_CAPTION,
                                         [tvRight.Selected.Text]);
  end;
end;

//This event handler is shared by popLeftValue and popRightValue.
//This controls what menus will be visible and what is the text for those menus.
procedure TfrmEditSetting.popLeftValuePopup(Sender: TObject);
var
  IsValueSelected : boolean;
  ValueName : string;
begin
  if Sender = popLeftValue then begin
    //The blank lines on the ListBox can be a blank line or a missing value name
    //If it is a missing value name, SelectedIndex will be -1.

    IsValueSelected := (FLeftValueNamesProvider.SelectedIndex <> -1);

    //Set captions for the menus here.
    if (IsValueSelected = true) then begin
      ValueName := FLeftValueNamesProvider.ValueNames [FLeftValueNamesProvider.SelectedIndex];
      actLeftCopyNameTo.Caption := Format (COPY_NAME_CAPTION,
                                           [ValueName, FRightSettingName]);
      actLeftDeleteName.Caption := Format (DELETE_NAME_CAPTION, [ValueName]);
    end;

    //Set whether the menus should be disabled or enabled.
    actLeftCopyNameTo.Enabled := IsValueSelected and (FRightReadOnly = false);
    actLeftDeleteName.Enabled := IsValueSelected and (FLeftReadOnly = false);
    actLeftClearValue.Enabled := IsValueSelected and (FLeftReadOnly = false);
  end
  else begin
    IsValueSelected := (FRightValueNamesProvider.SelectedIndex <> -1);

    if (IsValueSelected = true) then begin
      ValueName := FRightValueNamesProvider.ValueNames [FRightValueNamesProvider.SelectedIndex];
      actRightCopyNameTo.Caption := Format (COPY_NAME_CAPTION,
                                           [ValueName, FLeftSettingName]);
      actRightDeleteName.Caption := Format (DELETE_NAME_CAPTION, [ValueName]);
    end;

    actRightCopyNameTo.Enabled := IsValueSelected and (FLeftReadOnly = false);
    actRightDeleteName.Enabled := IsValueSelected and (FRightReadOnly = false);
    actRightClearValue.Enabled := IsValueSelected and (FRightReadOnly = false);

  end;
end;

procedure TfrmEditSetting.FormResize(Sender: TObject);
begin
  pnlValuesLeft.Width := pnlLeft.Width;
end;

procedure TfrmEditSetting.TreeViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key <> VK_DELETE) then
    Exit;

  if (Sender = tvLeft) and
     (FLeftReadOnly = false) then
    DeleteCurrentKey (FLeftRegistry, tvLeft);

  if (Sender = tvRight) and
     (FRightReadOnly = false) then
    DeleteCurrentKey (FRightRegistry, tvRight);
end;

procedure TfrmEditSetting.ValueNamesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key <> VK_DELETE) then
    Exit;

  if (Sender = lbLeftValues) and
     (FLeftReadOnly = false) then
    DeleteSelectedValue (FLeftValueNamesProvider, lbLeftValues.ItemIndex,
      lbLeftValues.TopIndex);

  if (Sender = lbRightValues) and
     (FRightReadOnly = false) then
    DeleteSelectedValue (FRightValueNamesProvider, lbRightValues.ItemIndex,
      lbRightValues.TopIndex);

end;

destructor TfrmEditSetting.Destroy;
begin
  if Assigned (FSettingProperties) then begin
    SettingPersistent.GetIniPersistentManager.SaveSetting (FSettingProperties);
    FreeAndNil (FSettingProperties);
  end;

  inherited;
end;

{ TEditSettingProperties }

function TEditSettingProperties.GetTopPanelHeight: integer;
begin
  Result := (Form as TfrmEditSetting).pnlTop.Height;
end;

procedure TEditSettingProperties.SetTopPanelHeight(const Value: integer);
begin
  (Form as TfrmEditSetting).pnlTop.Height := Value;
end;

function TEditSettingProperties.GetLeftPanelWidth: integer;
begin
  Result := (Form as TfrmEditSetting).pnlLeft.Width;
end;

procedure TEditSettingProperties.SetPanelWidth(const Value: integer);
begin
  (Form as TfrmEditSetting).pnlLeft.Width := Value;
end;

end.
