{-----------------------------------------------------------------------------
 Unit Name: frmSetting

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   UI to list, add, rename, copy, or delete custom settings.
            This frame is IDE agnostic, the main form must calls SetIDEVersion.
            The object of this class creates and maintains ISettingCollection
            and act as the user interface for ISettingCollection.
 History:
 12/31/2004 - Refactored some methods. New methods : CreateNewSetting,
              DeleteSelectedCustomSetting, RenameSelectedSetting, and
              CopySelectedSetting. These methods are public.
 01/02/2005 - Updated with the Edit form.
 02/21/2005 - Added keyboard support for the delete key.
 02/22/2005 - Fixed the problem with ItemIndex as the ListBoxes are now multi
              select.
 04/19/2005 - Fixed memory leak when creating the Edit Setting instance.
 06/18/2005 - Added support for applying template to the newly created form.
 06/22/2005 - Added GenerateNewSettingName method to generate a unique name
              when the user clicked the New Setting button.  
-----------------------------------------------------------------------------}

unit frmSetting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SettingCollection, ExtCtrls;

type
  TfrmSettingList = class(TFrame)
    lbSettings: TListBox;
    btnNew: TButton;
    btnDelete: TButton;
    btnRename: TButton;
    btnCopy: TButton;
    btnEdit: TButton;
    pnl_Buttons: TPanel;
    procedure lbSettingsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnEditClick(Sender: TObject);
    procedure lbSettingsDblClick(Sender: TObject);
    procedure lbSettingsClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnRenameClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
  private
    { Private declarations }
    FSettingCollection : ISettingCollection;

    function GetItemSelected : boolean;
    function GetSelectedSettingName : string;
    function GetIDEVersion : TIDEVersion;
    function GetSelectedRegistryKey : string;
    function GetSelectedRegistryPath : string;

    function GenerateNewSettingName : string;
    procedure RefreshListBox;
    procedure UpdateButtonState;
  public
    { Public declarations }
    procedure SelectSettingByName (const ASettingName : string);
    procedure SetIDEVersion (const IDEVersion : TIDEVersion);

    procedure CreateNewSetting;
    procedure DeleteSelectedCustomSetting;
    procedure RenameSelectedSetting;
    procedure CopySelectedSetting;
    procedure EditSetting;

    //These properties are necessary to tell other object what is the item that
    //the user is currently selected, the custom registry key, etc.
    //The purpose of these properties are to provide all necessary information
    //to create a shortcut.
    property ItemSelected : boolean read GetItemSelected;
    property SelectedSettingName : string read GetSelectedSettingName;
    property IDEVersion          : TIDEVersion read GetIDEVersion;
    //SelectedRegistryKey returns registry key starting with 'CustomSettings'.
    //Pass the value of this property to the -r parameter.
    //This one does not returns full registry path.
    property SelectedRegistryKey : string read GetSelectedRegistryKey;
    //SelectedRegistryPath returns the path of the selected custom setting under
    //HKEY_CURRENT_USER.  It starts with Software/Borland/CustomSettings
    property SelectedRegistryPath : string read GetSelectedRegistryPath;
  end;

implementation

uses fNewSetting, fRenameSetting, ShellUtilities, fEditSetting, SettingTemplate, dmGlyphs;

{$R *.dfm}

{ TfrmSettingList }

//This method must be called to set the ISettingCollection.
procedure TfrmSettingList.SetIDEVersion(const IDEVersion: TIDEVersion);
begin
  //Raise the exception if FSettingCollection has been assigned.
  //That means this method is called more than once.
  if Assigned (FSettingCollection) then
    raise Exception.Create ('Cannot call SetIDEVersion more than once.');

  //Creates the ISettingCollection.
  FSettingCollection := CreateSettingCollection(IDEVersion);
  lbSettings.Items.Clear;

  RefreshListBox;
end;

//Copy a selected setting to a new setting.
procedure TfrmSettingList.CopySelectedSetting;
var
  NewName: string;
  OldName: string;
begin
  if (lbSettings.ItemIndex <> -1) then begin
    OldName := lbSettings.Items[lbSettings.ItemIndex];
    NewName := EmptyStr;

    if TfrmRenameSetting.Execute(OldName, NewName, false) then begin
      FSettingCollection.CopySetting(OldName, NewName);
      RefreshListBox;
    end;
  end;
end;

procedure TfrmSettingList.UpdateButtonState;
begin
  btnDelete.Enabled := (lbSettings.ItemIndex <> -1) and (lbSettings.SelCount >= 1);
  btnRename.Enabled := (lbSettings.ItemIndex <> -1) and (lbSettings.SelCount = 1);
  btnCopy.Enabled   := (lbSettings.ItemIndex <> -1) and (lbSettings.SelCount = 1);
  btnEdit.Enabled   := (lbSettings.ItemIndex <> -1) and
                       (0 < lbSettings.SelCount) and (lbSettings.SelCount <= 2);
end;

//Rename the name of the selected setting.  If this method renames the IDE
//setting that is currently used by a running Delphi IDE, it can have
//unpredictable result.  My test shows that the IDE complains that it cannot
//save the changes to the registry.
procedure TfrmSettingList.RenameSelectedSetting;
var
  OldName: string;
  NewName: string;
begin
  if (lbSettings.ItemIndex <> -1) then begin
    OldName := lbSettings.Items[lbSettings.ItemIndex];
    NewName := EmptyStr;

    if TfrmRenameSetting.Execute(OldName, NewName) then begin
      FSettingCollection.RenameSetting(OldName, NewName);
      RefreshListBox;
    end;
  end;
end;

//Delete the selected setting. If this method delete a setting that is currently
//used by a running Delphi IDE, it can have unpredictable result.  Check the
//comment for RenameSelectedSetting.
procedure TfrmSettingList.DeleteSelectedCustomSetting;
var
  Index : integer;
  SelectedItems : array of integer;
const
  DELETE_CONFIRMATION_SINGULAR : array [false..true] of string =
    ('Are you sure you want to delete these settings?',
     'Are you sure you want to delete this setting?');
begin
  if (lbSettings.ItemIndex <> -1) then
    if MessageDlg (DELETE_CONFIRMATION_SINGULAR[lbSettings.SelCount = 1],
                   mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
      //Get the selected items.
      SetLength (SelectedItems, lbSettings.SelCount);
      lbSettings.Perform (LB_GETSELITEMS, lbSettings.SelCount,
                          LongInt (SelectedItems));

      for Index in SelectedItems do
        FSettingCollection.DeleteSetting (lbSettings.Items [Index]);

      RefreshListBox;
    end;
end;

//Create a new setting.
procedure TfrmSettingList.CreateNewSetting;
var
  RegKey,
  TemplateName,
  NewSettingName: string;
  UseDefaultSetting: Boolean;
  AppliedTemplate : TStringArray;
  Template : ISettingTemplate;
  TemplateCollection : ITemplateCollection;
begin
  NewSettingName := self.GenerateNewSettingName;

  if TfrmNewSetting.Execute (NewSettingName, FSettingCollection.IDEVersion,
                             UseDefaultSetting, AppliedTemplate) = true then
  begin
    Assert(NewSettingName <> '');
    FSettingCollection.AddNewSetting(NewSettingName, UseDefaultSetting);
    TemplateCollection := GetTemplateCollection;

    //If the user wants to apply a template, get the custom registry path, and
    //apply the template. This code makes sure that the TemplateCollection is
    //properly initialized.
    if (Length (AppliedTemplate) > 0) and
       (Assigned (TemplateCollection) = true) then begin
      RegKey := FSettingCollection.GetCustomRegistryPath (NewSettingName);
      Template := nil;

      for TemplateName in AppliedTemplate do begin
        if Assigned (Template) = false then
          Template := TemplateCollection.GetTemplate (
                          FSettingCollection.IDEVersion, TemplateName)
        else
          Template.MergeTemplate (TemplateCollection.GetTemplate (
                                      FSettingCollection.IDEVersion,
                                      TemplateName));
      end;

      TemplateCollection.ApplyTemplate (RegKey, Template);
    end;

    //Apply Template here.
    RefreshListBox;
  end;
end;

//Refresh the list of setting on the list box with the registry.
procedure TfrmSettingList.lbSettingsClick(Sender: TObject);
begin
  self.UpdateButtonState;
end;

procedure TfrmSettingList.btnNewClick(Sender: TObject);
begin
  CreateNewSetting;
end;

procedure TfrmSettingList.btnDeleteClick(Sender: TObject);
begin
  DeleteSelectedCustomSetting;
end;

procedure TfrmSettingList.btnRenameClick(Sender: TObject);
begin
  RenameSelectedSetting;
end;

procedure TfrmSettingList.btnCopyClick(Sender: TObject);
begin
  CopySelectedSetting;
end;

procedure TfrmSettingList.btnEditClick(Sender: TObject);
begin
  EditSetting;
end;

//Refactored method. This method refresh the items on the listbox based on the
//current information in the FSettingCollection, set the state of the buttons
//accordingly, and try to preserve the ItemIndex property of the listbox.
procedure TfrmSettingList.RefreshListBox;
var
  Loop : integer;
  SelectedItems : array of integer;
begin
  if lbSettings.SelCount > 0 then begin
    SetLength (SelectedItems, lbSettings.SelCount);
    lbSettings.Perform (LB_GETSELITEMS, lbSettings.SelCount,
                        LongInt (SelectedItems));
    FSettingCollection.ReadSettings (lbSettings.Items);

    for Loop := 0 to Length (SelectedItems) do
      if SelectedItems [Loop] < lbSettings.Count then
        lbSettings.Selected [SelectedItems [Loop]] := true;
  end
  else
    FSettingCollection.ReadSettings (lbSettings.Items);

  UpdateButtonState;
end;

//Returns the IDE version that was assigned through the SetIDEVersion.
function TfrmSettingList.GetIDEVersion: TIDEVersion;
begin
  Result := self.FSettingCollection.IDEVersion;
end;

//Returns true if there is at least one item selected.
function TfrmSettingList.GetItemSelected: boolean;
begin
  Result := self.lbSettings.ItemIndex <> -1;
end;

//Returns the name of the selected setting name.
function TfrmSettingList.GetSelectedSettingName: string;
begin
  if lbSettings.SelCount = 0 then
    Result := ''
  else
    Result := lbSettings.Items [lbSettings.ItemIndex];
end;

//Returns the key of the customized setting. This value can be used with the -r
//parameter.
function TfrmSettingList.GetSelectedRegistryKey: string;
begin
  Result := FSettingCollection.GetCustomRegistryKey (GetSelectedSettingName);
end;

procedure TfrmSettingList.lbSettingsDblClick(Sender: TObject);
begin
  Screen.Cursor := crHourglass;
  try
    RunDelphi (GetExecutablePath (IDEVersion),
               '-r"' + SelectedRegistryKey + '"');
  finally
    Screen.Cursor := crArrow;
  end;
end;

//Return the custom path of the selected custosm setting. The path is started
//with Software/Borland/CustomSettings + the custom setting key.
function TfrmSettingList.GetSelectedRegistryPath: string;
begin
  Result := FSettingCollection.GetCustomRegistryPath (GetSelectedSettingName);
end;

//Select a custom setting on the list by its name.
procedure TfrmSettingList.SelectSettingByName(
  const ASettingName: string);
begin
  if lbSettings.Items.IndexOf (ASettingName) <> -1 then begin
    lbSettings.Selected [lbSettings.Items.IndexOf (ASettingName)] := true;
    UpdateButtonState;
  end;
  //lbSettings.ItemIndex := lbSettings.Items.IndexOf (ASettingName);
end;

procedure TfrmSettingList.lbSettingsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) then
    DeleteSelectedCustomSetting;
end;

procedure TfrmSettingList.EditSetting;
var
  CompRef : IInterfaceComponentReference;
  I : IEditSetting;
  SelectedItems : array [0..1] of integer;
begin
  if (lbSettings.SelCount <= 0) or (2 < lbSettings.SelCount) then
    Exit;

  I := TfrmEditSetting.Create (self);
  try
    if (lbSettings.SelCount = 1) then begin
      I.LeftSettingPath := GetIDEKey (FSettingCollection.IDEVersion);
      I.LeftSettingName := 'Delphi Original Setting';
      I.RightSettingPath := FSettingCollection.GetCustomRegistryPath (
                                SelectedSettingName, true);
      I.RightSettingName := SelectedSettingName;
    end
    else begin
      lbSettings.Perform (LB_GETSELITEMS, lbSettings.SelCount,
                          LongInt (@SelectedItems));

      I.LeftSettingPath  := FSettingCollection.GetCustomRegistryPath (
                              lbSettings.Items [SelectedItems[0]], true);
      I.LeftSettingName  := lbSettings.Items [SelectedItems[0]];
      I.RightSettingPath := FSettingCollection.GetCustomRegistryPath (
                              lbSettings.Items [SelectedItems[1]], true);
      I.RightSettingName := lbSettings.Items [SelectedItems[1]];
    end;

    I.Execute;
  finally
    if Supports (I, IInterfaceComponentReference, CompRef) then
      CompRef.GetComponent.Free;
  end;
end;

//Generating a unique name for the new template, either returns a unique name or
//an empty string.
function TfrmSettingList.GenerateNewSettingName: string;
const
  NEW_SETTING    = 'New_Setting';
  NEW_SETTING_INC = NEW_SETTING + '_%d';
var
  Counter : integer;
  Found : boolean;
begin
  Result  := NEW_SETTING;
  Counter := 1;
  Found   := false;

  //Loop through the available setting names, if the constructed name does not
  //available return it.  After trying to find it 100 times, stop looking.  If
  //a unique name cannot be generated, returns an empty string.
  while (Found = false) or (Counter < 100) do begin
    Found := lbSettings.Items.IndexOf (Result) = -1;

    if Found = true then
      Break;

    Result := Format (NEW_SETTING_INC, [Counter]);
    Inc (Counter);
  end;

  if (Found = false) then
    Result := EmptyStr;
end;

end.
