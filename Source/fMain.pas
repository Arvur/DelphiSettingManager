{-----------------------------------------------------------------------------
 Unit Name: fMain

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   Main form

 History:
 11/22/2004 - Added menu, saving and restoring last position, last tab opened,
              last selected item.
 11/26/2004 - Added the ability to save a custom setting to a file.
 11/31/2004 - Updated this file with a bunch of comments, refactored some
              methods.
 02/22/2005 - Bug with the delete button for the BDS1, set Scaled to false.
 02/25/2005 - Fixed bug when trying to save .reg file with invalid character.
 04/05/2005 - Implemented load/save setting using setting persistent classes.
              Removed dependency on ConfigPersistent.pas, IMainProperties
              interface and its methods.
 06/18/2005 - Added SettingTemplate. This class is responsible for loading the
              template file.
-----------------------------------------------------------------------------}

unit fMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, frmSetting, SettingCollection, Buttons, ActnList,
  StdActns, Menus, ImgList, SettingPersistent;

type
  TSettingManagerForm = class;

  TfrmMain = class(TForm)
    Menu: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    mnuCustomSetting: TMenuItem;
    NewCustomSetting: TMenuItem;
    DeleteCustomSetting: TMenuItem;
    RenameCustomSetting: TMenuItem;
    CopyCustomSetting: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    N2: TMenuItem;
    CreateShortcut1: TMenuItem;
    RunDelphi1: TMenuItem;
    pcInstalledIDE: TPageControl;
    btnCreateShortcut: TBitBtn;
    btnClose: TBitBtn;
    dlgSave: TSaveDialog;
    btnRunDelphi: TBitBtn;
    ActionList: TActionList;
    actFileExit: TFileExit;
    actCreateShortcut: TAction;
    actRunDelphi: TAction;
    imgList: TImageList;
    actNewSetting: TAction;
    actDeleteSetting: TAction;
    actRenameSetting: TAction;
    actCopySetting: TAction;
    Options1: TMenuItem;
    RestoreDefaultSize1: TMenuItem;
    actRestoreSize: TAction;
    actSaveSetting: TAction;
    SaveSetting1: TMenuItem;
    N1: TMenuItem;
    actAbout: TAction;
    actEditSetting: TAction;
    Edit1: TMenuItem;
    procedure actEditSettingExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actSaveSettingExecute(Sender: TObject);
    procedure actRestoreSizeExecute(Sender: TObject);
    procedure actCopySettingExecute(Sender: TObject);
    procedure actRenameSettingExecute(Sender: TObject);
    procedure actDeleteSettingExecute(Sender: TObject);
    procedure actNewSettingExecute(Sender: TObject);
    procedure actRunDelphiExecute(Sender: TObject);
    procedure actCreateShortcutExecute(Sender: TObject);
  private
    { Private declarations }
    FSettingManagerForm : TSettingManagerForm;
    function CurrentFrame : TfrmSettingList;
    procedure CreateTabs;
    procedure LoadTabs;
    procedure SetTab (const IDE : TIDEVersion; const IDEInstalled : boolean);
    //IMainFormProperties
    function GetSelectedTabIndex : integer;
    function GetSelectedCustomSetting : string;
    procedure SetSelectedCustomSetting (const AValue : string);
    procedure SetSelectedTabIndex (const AValue : integer);

    function CorrectFileName (AFileName : string) : string;
  public
    { Public declarations }
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TSettingManagerForm = class (TFormSetting)
  private
    function GetSelectedCustomSetting: string;
    function GetSelectedTabIndex: integer;
    procedure SetSelectedCustomSetting(const Value: string);
    procedure SetSelectedTabIndex(const Value: integer);
  published
    property SelectedTabIndex : integer read GetSelectedTabIndex
        write SetSelectedTabIndex;
    property SelectedCustomSetting : string read GetSelectedCustomSetting
        write SetSelectedCustomSetting;
  end;

var
  frmMain: TfrmMain;

implementation

uses ShellUtilities, LoadSaveCustomSetting, fAbout, SettingTemplate;

const
  //Error message if no IDE is installed in the system.
  ERR_NO_SUPPORTED_IDE = 'There is no supported Borland IDE installed in this machine. ' +
                         '(Delphi 6, Delphi 7, C#Builder, Delphi 8).';
  //Default form width and height.
  FORM_WIDTH  = 550;
  FORM_HEIGHT = 370;

  //The template file name. It must be in the same location with the executable.
  GALILEO_TEMPLATE_NAME = 'TEMPLATE.XML';

{$R *.dfm}

{ TForm1 }

// Dynamically creating tabs for all IDEs
procedure TfrmMain.CreateTabs;
var
  IDELoop  : TIDEVersion;
  newTab   : TTabSheet;
  newFrame : TfrmSettingList;
begin
  for IDELoop := Low(TIDEVersion) to High(TIDEVersion) do begin
    try
      newTab             := TTabSheet.Create(Self);
      newTab.Caption     := IDEParams[IDELoop].DisplayName;
      newTab.Name        := 'tab_' + IDEParams[IDELoop].ShortName;
      newTab.PageControl := pcInstalledIDE;
      newFrame        := TfrmSettingList.Create(Self);
      newFrame.Name   := 'frm_' + IDEParams[IDELoop].ShortName;
      newFrame.Parent := newTab;
      newFrame.Align  := alClient;
    except
      FreeAndNil(newTab);
    end;
  end;
end;

//Retrieve a set of installed Delphi IDEs and then for each IDE version, set the
//correspoinding Tab Sheet.  The TabSheet which has no corresponding IDE will be
//hidden.
procedure TfrmMain.LoadTabs;
var
  InstalledIDE : TInstalledIDE;
  Loop         : TIDEVersion;
  TabLoop      : integer;
begin
  //If there is no supported IDE installed, display an information dialog and
  //terminate the app.
  //If there is at least one supported IDE installad, InstalledIDE will not be
  //an empty set.
  if GetInstalledIDEVersion (InstalledIDE) = false then begin
    MessageDlg (ERR_NO_SUPPORTED_IDE, mtInformation, [mbOK], 0);
    Application.Terminate;
  end
  else begin
    Assert (InstalledIDE <> []);
    //Iterate through the TIDEVersion constants and based on whether the IDE
    //is installed or not, set the tab.
    for Loop := Low (TIDEVersion) to High (TIDEVersion) do
      SetTab (Loop, (Loop in InstalledIDE));

    //At this point, all tabs which have corresponding IDE installed will be
    //enabled and visible. Set the first tab which is visible and enabled to be
    //the active tab.
    for TabLoop := 0 to pcInstalledIDE.PageCount - 1 do
      if pcInstalledIDE.Pages[TabLoop].TabVisible = true then begin
        pcInstalledIDE.ActivePageIndex := TabLoop;
        Break;
      end;
  end;
end;

//SetTab set the tab visible if the corresponding IDE is installed, and
//initialize the frame if the IDE is installed.
procedure TfrmMain.SetTab(const IDE: TIDEVersion; const IDEInstalled: boolean);
var
  Tab : TTabSheet;
  Frame : TfrmSettingList;
begin
  //Based on the IDE, get the corresponding Tab and Frame.
  Tab := TTabSheet(Self.FindComponent('tab_' + IDEParams[IDE].ShortName));
  Frame := TfrmSettingList(Self.FindComponent('frm_' + IDEParams[IDE].ShortName));

  //Disable or set the tab invisible if corresponding IDE is not installed.
  Tab.Visible    := IDEInstalled;
  Tab.TabVisible := IDEInstalled;
  Tab.Enabled    := IDEInstalled;

  if IDEInstalled = true then
    Frame.SetIDEVersion (IDE);
end;

//Returns the TfrmSettingList of the active frame.
function TfrmMain.CurrentFrame: TfrmSettingList;
var
  frmName: string;
begin
  //Return TfrmSettingLIst based on the index of the current page.
  frmName := StringReplace(pcInstalledIDE.ActivePage.Name, 'tab_', 'frm_', []);
  Result := TfrmSettingList(Self.FindComponent(frmName));
end;

//If CurrentFrame is a valid Frame and there is a selected item, open the file
//save dialog and save a shortcut.
procedure TfrmMain.actCreateShortcutExecute(Sender: TObject);
begin
  //If there is no item selected or there is no frame in the current tab, exit.
  if (CurrentFrame = nil) or
     (CurrentFrame.ItemSelected = false) then
    Exit;

  dlgSave.DefaultExt := 'lnk';
  dlgSave.FileName := CorrectFileName (CurrentFrame.SelectedSettingName);

  //Create the shortcut. Encluse the registry key within double quotes, to
  //anticipate registry key with space in it.
  if dlgSave.Execute = true then
    CreateLink (dlgSave.FileName,
                GetExecutablePath (CurrentFrame.IDEVersion),
                '-r"' + CurrentFrame.SelectedRegistryKey + '"');
end;

//Execute the selected IDE, if CurrentFrame is valid and an item is selected.
procedure TfrmMain.actRunDelphiExecute(Sender: TObject);
begin
  if (CurrentFrame = nil) or
     (CurrentFrame.ItemSelected = true) then begin
    Screen.Cursor := crHourglass;
    try
      RunDelphi (GetExecutablePath (CurrentFrame.IDEVersion),
                 '-r"' + CurrentFrame.SelectedRegistryKey + '"');
    finally
      Screen.Cursor := crArrow;
    end;
  end;
end;

procedure TfrmMain.actNewSettingExecute(Sender: TObject);
begin
  CurrentFrame.CreateNewSetting;
end;

procedure TfrmMain.actDeleteSettingExecute(Sender: TObject);
begin
  CurrentFrame.DeleteSelectedCustomSetting;
end;

procedure TfrmMain.actRenameSettingExecute(Sender: TObject);
begin
  CurrentFrame.RenameSelectedSetting;
end;

procedure TfrmMain.actCopySettingExecute(Sender: TObject);
begin
  CurrentFrame.CopySelectedSetting;
end;

procedure TfrmMain.actEditSettingExecute(Sender: TObject);
begin
  CurrentFrame.EditSetting;
end;

//Property getter, returns the selected custom setting, if there is one.  If no
//frame is active, this will return an empty string.
function TfrmMain.GetSelectedCustomSetting: string;
begin
  if (CurrentFrame <> nil) then
    Result := CurrentFrame.SelectedSettingName
  else
    Result := EmptyStr;
end;

//Property getter, Returns the index of the selected tab.
function TfrmMain.GetSelectedTabIndex: integer;
begin
  Result := pcInstalledIDE.ActivePageIndex;
end;

//Property setter, to select a setting on the CurrentFrame.
procedure TfrmMain.SetSelectedCustomSetting(const AValue: string);
begin
  if (CurrentFrame <> nil) then
    CurrentFrame.SelectSettingByName (AValue);
end;

//Property setter to select a tab index.
procedure TfrmMain.SetSelectedTabIndex(const AValue: integer);
begin
  //Make sure that the AValue is a valid tab index value and the tab sheet is
  //visible.
  if (AValue < pcInstalledIDE.PageCount) and
     (pcInstalledIDE.Pages [AValue].TabVisible = true) then
    pcInstalledIDE.ActivePageIndex := AValue;
end;

//Restore the form size and position. The default position is in the center of
//the screen.
procedure TfrmMain.actRestoreSizeExecute(Sender: TObject);
begin
  Width  := FORM_WIDTH;
  Height := FORM_HEIGHT;
  Left   := (Screen.Width - Width) div 2;
  Top    := (Screen.Height - Height) div 2;
end;

//Action item for saving the selected custom setting to a registry file.
procedure TfrmMain.actSaveSettingExecute(Sender: TObject);
var
  SaveSetting : ISaveSetting;
begin
  //Execute only if the current frame is a valid frame and an item is selected.
  if (CurrentFrame <> nil) and
     (CurrentFrame.ItemSelected = true) then begin
    dlgSave.DefaultExt := 'reg';
    //Since the exported file will have .reg extension, add the IDE name in
    //front of the default name. By having the IDE name in front of the registry
    //file, it will make it easy to recognize for which IDE is the .reg file.
    dlgSave.FileName := CorrectFileName (
                            pcInstalledIDE.ActivePage.Caption + ' ' +
                            CurrentFrame.SelectedSettingName + '.reg');

    if dlgSave.Execute = true then begin
      SaveSetting := GetSaveSetting;
      SaveSetting.FileName := dlgSave.FileName;
      SaveSetting.CustomSettingPath := CurrentFrame.SelectedRegistryPath;
      SaveSetting.SaveSettingToFile;
    end;
  end;
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;
  CreateTabs;
  LoadTabs;

  FSettingManagerForm := TSettingManagerForm.Create (self);
  SettingPersistent.GetIniPersistentManager.LoadSetting (FSettingManagerForm);
  SettingTemplate.CreateTemplateCollection (
      ExtractFilePath (Application.ExeName) + GALILEO_TEMPLATE_NAME);
end;

destructor TfrmMain.Destroy;
begin
  if Assigned (FSettingManagerForm) then begin
    SettingPersistent.GetIniPersistentManager.SaveSetting (FSettingManagerForm);
    SettingPersistent.GetIniPersistentManager.SaveSetting (FSettingManagerForm);
    FreeAndNil (FSettingManagerForm);
  end;

  inherited;
end;

procedure TfrmMain.actAboutExecute(Sender: TObject);
begin
  with TfrmAbout.Create (self) do
    try
      ShowModal;
    finally
      Release;
    end;
end;

function TfrmMain.CorrectFileName(AFileName: string): string;
const
  INVALID_CHAR_FILENAME : set of char = ['<', '>', '|', '"', '\', '/'];
var
  Loop : integer;
begin
  for Loop := 1 to Length (AFileName) do
    if AFileName[Loop] in INVALID_CHAR_FILENAME then
      AFileName[Loop] := '_';

  Result := AFileName;
end;

{ TSettingManagerForm }

function TSettingManagerForm.GetSelectedCustomSetting: string;
begin
  Result := (Form as TfrmMain).GetSelectedCustomSetting;
end;

procedure TSettingManagerForm.SetSelectedCustomSetting(const Value: string);
begin
  (Form as TfrmMain).SetSelectedCustomSetting (Value);
end;

function TSettingManagerForm.GetSelectedTabIndex: integer;
begin
  Result := (Form as TfrmMain).GetSelectedTabIndex;
end;

procedure TSettingManagerForm.SetSelectedTabIndex(const Value: integer);
begin
  (Form as TfrmMain).SetSelectedTabIndex (Value);
end;

end.

