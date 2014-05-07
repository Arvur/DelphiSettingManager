{-----------------------------------------------------------------------------
 Unit Name: SettingPersistent

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   A simple persistent framework using RTTI. The persistent framework
            works by reading the properties of TSetting. All properties that
            must be persisted must be declared in the published section.
            If the class that needs to be persisted cannot be inherited from
            TSetting, use Abstract Pattern.

            If the order of property is important during loading from persistent
            layer, write the property in the order of execution. The property on
            the top will be read/write first.

            Future improvements:
            - Add feature to capture Windows message when the user changed the
              screen size, add this code in the TFormSetting.
            - Implement the persistent manager as singleton.
            - Implement XML persistent.
 History:
 04/01/2005 - Initial version. Based on my original pet project with persistent
              framework that I did on 2001-2002.
 04/11/2005 - Added support for nested object at the ini persistent manager.
-----------------------------------------------------------------------------}
unit SettingPersistent;

interface

uses
  Classes, Windows, TypInfo, Forms, IniFiles;

type
  //TSetting is the base class. It exposes Name property. The persistent
  //framework uses this property as identifier. If the subclass does not
  //override GetName, this class will return the class name.
  //The SetDefault is a placeholder. This method is used to get the default
  //state of the object.
{$M+}
  TSetting = class
  protected
    function GetName : string; virtual;
  public
    procedure SetDefault; virtual;
  published
    property Name : string read GetName;
  end;
{$M-}

  TPropRec = record
    PropCount : integer;
    PropList : TPropList;
  end;

  //TFormSetting is used for TForm and its descendants. Use this class to works
  //with persistent framework. This class works as abstract pattern for TForm.
  //At very basic level, this class stores the desktop size, the original form
  //size and a reference to the form.
  //This class must be created by passing a TForm in the constructor.
  //This class exposes Top, Left, Height and Width, it must be executed in that
  //order.
  TFormSetting = class (TSetting)
  private
    FForm : TForm;
    FDesktopSize: TRect;
    FOriginalSize : TRect;

    function GetHeight: integer;
    function GetLeft: integer;
    function GetTop: integer;
    function GetWidth: integer;
    procedure SetHeight(const Value: integer);
    procedure SetLeft(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetWidth(const Value: integer);

    procedure GetWorkAreaSize;
  protected
    property Form : TForm read FForm write FForm;
    property DesktopSize : TRect read FDesktopSize write FDesktopSize;
    property OriginalSize : TRect read FOriginalSize write FOriginalSize;
  public
    constructor Create (const AForm : TForm);
    destructor Destroy; override;
    procedure ResetSizeAndPos;
    procedure SetDefault; override;
  published
    property Top : integer read GetTop write SetTop;
    property Left : integer read GetLeft write SetLeft;
    property Height : integer read GetHeight write SetHeight;
    property Width : integer read GetWidth write SetWidth;
  end;

  //Persistent manager interface. All concrete persistent manager must implement
  //this class.
  ISettingPersistentManager = interface
  ['{16FD2B74-2AED-4F3C-93FE-796AFA406C0A}']
    procedure SaveSetting (const ASetting : TSetting);
    procedure LoadSetting (const ASetting : TSetting);
  end;

  //Persistent manager that will read and save to an ini file.
  TINISettingPersistentManager = class (TInterfacedObject,
    ISettingPersistentManager)
  private
    FIniFileName : string;
    procedure LoadPropList (const ASetting : TSetting; var PropRec : TPropRec);
    procedure ReadSetting (const ASetting: TSetting;
        const APropRec : TPropRec; const AStrings : TStrings);

    procedure SaveSetting (const ASetting : TSetting; const AIniFile : TIniFile;
        SectionName : string = ''); overload;
    procedure LoadSetting(const ASetting: TSetting; const AIniFile : TIniFile;
        SectionName : string = ''); overload;

  public
    constructor Create (const AFileName : string; CanCreate : boolean = true);

    procedure SaveSetting (const ASetting : TSetting); overload;
    procedure LoadSetting (const ASetting : TSetting); overload;
  end;

//Factory funciton for INISettingManager.
function GetIniPersistentManager : ISettingPersistentManager;

implementation

uses
  SysUtils, ShlObj;

const
  //Supported property types.
  SUPPORTED_PROPERTY = [tkInteger, tkChar, tkEnumeration, tkFloat, tkString,
                        tkInt64, tkWString, tkLString, tkClass];

var
  PersistentManager : ISettingPersistentManager;

function GetSpecialFolderPath(FolderID: Integer; CanCreate: Boolean = True): string;
var
  FilePath: array [0..MAX_PATH] of Char;
begin
  SHGetSpecialFolderPath(0, @FilePath[0], FolderID, CanCreate);
  Result := IncludeTrailingPathDelimiter(FilePath);
end;

function GetIniPersistentManager : ISettingPersistentManager;
begin
  if Assigned (PersistentManager) = false then
    PersistentManager := TIniSettingPersistentManager.Create (
                             GetSpecialFolderPath(CSIDL_LOCAL_APPDATA) +
                             ChangeFileExt(ExtractFileName(Application.ExeName), '.ini'));

  Result := PersistentManager;
end;

{ TSetting }

function TSetting.GetName: string;
begin
  Result := ClassType.ClassName;
end;

//Place holder. It is not implemented to prevent Abstract error.
procedure TSetting.SetDefault;
begin
  //Not implemented.
end;

{ TINISettingPersistentManager }

constructor TINISettingPersistentManager.Create(const AFileName: string;
  CanCreate: boolean);
var
  FileHandle : integer;
begin
  inherited Create;
  FIniFileName := AFileName;

  if SameText (FIniFileName, EmptyStr) = true then
    raise Exception.Create ('File Name cannot be empty.');
  FIniFileName := AFileName;

  if FileExists (FIniFileName) = false then begin
    FileHandle := FileCreate (FIniFileName);

    if FileHandle <> -1 then
      FileClose (FileHandle);
  end;
end;

//Load the property list into internal list.
procedure TINISettingPersistentManager.LoadPropList(const ASetting: TSetting;
    var PropRec : TPropRec);
begin
  FillChar (PropRec.PropList, SizeOf (PropRec.PropList), 0);
  PropRec.PropCount := GetPropList (PTypeInfo (ASetting.ClassInfo),
                                    SUPPORTED_PROPERTY, @PropRec.PropList,
                                    false);
end;

//Load the setting from the peristent layer to the object. If the setting does
//not exist, there will be nothing.
procedure TINISettingPersistentManager.LoadSetting(const ASetting: TSetting;
  const AIniFile : TIniFile; SectionName : string = '');
var
  Loop : integer;
  Values : TStringList;
  PropRec : TPropRec;
  Obj : TObject;
begin
  Values := TStringList.Create;

  if SectionName = '' then
    SectionName := ASetting.Name;

  try
    //Load a section from the ini file to a stringlist.
    if AIniFile.SectionExists (SEctionName) = true then begin
      LoadPropList (ASetting, PropRec);
      AIniFile.ReadSectionValues (SectionName, Values);
      ReadSetting (ASetting, PropRec, Values);

      //For each nested object, recursively call this method.
      for Loop := 0 to PropRec.PropCount - 1 do
        if PropRec.PropList[Loop]^.PropType^.Kind = tkClass then begin
          Obj := GetObjectProp (ASetting, PropRec.PropList[Loop], TSetting);

          if Assigned (Obj) then
            LoadSetting (Obj as TSetting, AIniFile,
                         SectionName + '-' + PropRec.PropList[Loop]^.Name);
        end;
    end;
  finally
    Values.Free;
  end;
end;

//Verify the name value pairs in a strings and assign it to the object. The only
//exception is the Name property, it is used as identifier not as read-write
//property.
procedure TINISettingPersistentManager.LoadSetting(const ASetting: TSetting);
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create (FIniFileName);
  try
    LoadSetting (ASetting, IniFile, '');
  finally
    IniFile.Free;
  end;
end;

//Assign values from AStrings to the object of TSetting.
procedure TINISettingPersistentManager.ReadSetting(const ASetting: TSetting;
    const APropRec : TPropRec; const AStrings : TStrings);
var
  Loop : integer;
  Prop : TPropInfo;
  Value : variant;
begin
  //Iterate through the property of the object.
  for Loop := 0 to APropRec.PropCount - 1 do begin
    Prop := APropRec.PropList [Loop]^;

    //Do not set the name property, the property does not have Setter method, or
    //the property is an object.
    if SameText (Prop.Name, 'Name') or
       (Prop.SetProc = nil) or
       (AStrings.IndexOfName (Prop.Name) = -1) or
       (Prop.PropType^.Kind = tkClass) then
      Continue;

    Value := AStrings.Values [Prop.Name];
    SetPropValue (ASetting, Prop.Name, Value);
  end;
end;

//Persist an object.
{ TODO : Consider refactoring this method. }
procedure TINISettingPersistentManager.SaveSetting (const ASetting: TSetting;
    const AIniFile : TIniFile; SectionName : string = '');
var
  Values : TStringList;
  Loop : integer;
  Obj  : TObject;
  PropRec : TPropRec;
begin
  Values := TStringList.Create;

  if SameText (SectionName, EmptyStr) = true then
    SectionName := ASetting.Name;

  try
    //Read the property into the internal list.
    LoadPropList (ASetting, PropRec);

    //Write the property one by one. If the property is write-only, don't bother
    //to save it. :)
    for Loop := 0 to PropRec.PropCount - 1 do begin
      if (PropRec.PropList[Loop]^.PropType^.Kind <> tkClass) and
         (SameText (PropRec.PropList[Loop]^.Name, 'Name') or
          (PropRec.PropList[Loop]^.GetProc = nil)) then
        Continue;

        AIniFile.WriteString (SectionName, PropRec.PropList[Loop]^.Name,
                              GetPropValue (ASetting, PropRec.PropList[Loop]^.Name,
                              true))
    end;

    //Persist the nested object.
    for Loop := 0 to PropRec.PropCount - 1 do begin
      if PropRec.PropList[Loop]^.PropType^.Kind <> tkClass then
        Continue;

      Obj := GetObjectProp (ASetting, PropRec.PropList[Loop], TSetting);

      if Assigned (Obj) then begin
        AIniFile.WriteString (SectionName, PropRec.PropList[Loop]^.Name,
                              SectionName + '-' + PropRec.PropList[Loop]^.Name);
        SaveSetting (Obj as TSetting, AIniFile,
                     SectionName + '-' + PropRec.PropList[Loop]^.Name);
      end;
    end;

  finally
    Values.Free;
  end;
end;

//Persist a setting to an INI file.
procedure TINISettingPersistentManager.SaveSetting(const ASetting: TSetting);
var
  ini : TIniFile;
begin
  ini := TIniFile.Create (FIniFileName);
  try
    SaveSetting (ASetting, ini, '');
  finally
    ini.Free;
  end;
end;

{ TFormSetting }

function TFormSetting.GetWidth: integer;
begin
  Result := self.FForm.Width;
end;

//Set the form width. If it is off screen, adjust it.
procedure TFormSetting.SetWidth(const Value: integer);
begin
  FForm.Width := Value;

  //If the width is wider than the work are, the saved size will be reset to
  //the width of the work area.
  if self.FForm.Width > self.FDesktopSize.Right then
    self.FForm.Width := self.FDesktopSize.Right ;
end;

function TFormSetting.GetTop: integer;
begin
  Result := self.FForm.Top;
end;

//Set the form top position. If it is off screen, adjust it.
procedure TFormSetting.SetTop(const Value: integer);
begin
  FForm.Top := Value;

  //If the Top of the form is less than the top of the work area, reset it.
  if FForm.Top < FDesktopSize.Top then
    FForm.Top := FDesktopSize.Top
  else
    //If the top if bigger than the work area size, or part of the form is out
    //of the work area, reset the top so when the app is reloaded, the form
    //will be displayed entirely.
    if (FForm.Top > FDesktopSize.Bottom) or
       ((FForm.Top + FForm.Height) > FDesktopSize.Bottom) then
      FForm.Top := FDesktopSize.Bottom - FForm.Height;
end;

function TFormSetting.GetHeight: integer;
begin
  Result := self.FForm.Height;
end;

//Set the form height. If the form is off screen, adjust it.
procedure TFormSetting.SetHeight(const Value: integer);
begin
  FForm.Height := Value;

  //If the height is bigger than the work area, the saved size will be reset
  //to the height of the work area.
  if FForm.Height > FDesktopSize.Bottom then
    FForm.Height := FDesktopSize.Bottom;
end;

function TFormSetting.GetLeft: integer;
begin
  Result := self.FForm.Left;
end;

//Set the left position of the form.  If the form is position off the screen,
//adjust it.
procedure TFormSetting.SetLeft(const Value: integer);
begin
  FForm.Left := Value;

  //If the left is out of the work area, reset it.
  if FForm.Left < FDesktopSize.Left then
    FForm.Left := FDesktopSize.Left
  else
    //If the form is off to the right or some of the form is hidden to the
    //right, reset the left property.
    if (FForm.Left > FDesktopSize.Right) or
       ((FForm.Left + FForm.Width) > FDesktopSize.Right) then
      FForm.Left := FDesktopSize.Right - FForm.Width;
end;

//Get the desktop size.
procedure TFormSetting.GetWorkAreaSize;
begin
  SystemParametersInfo (SPI_GETWORKAREA, 0, @FDesktopSize, 0);
end;

constructor TFormSetting.Create (const AForm : TForm);
begin
  inherited Create;

  //Keep the reference of the AForm.
  self.FForm := AForm;
  //Get the deskop size,
  GetWorkAreaSize;

  //Get the original size.
  self.FOriginalSize.Left := FForm.Left;
  self.FOriginalSize.Top  := FForm.Top;
  self.FOriginalSize.Right := FForm.Width;
  self.FOriginalSize.Bottom := FForm.Height;
end;

destructor TFormSetting.Destroy;
begin

  inherited;
end;

//Reset the form position and size to the original position and size.
procedure TFormSetting.ResetSizeAndPos;
begin
  self.FForm.Top    := FOriginalSize.Top;
  self.FForm.Left   := FOriginalSize.Left;
  self.FForm.Height := FOriginalSize.Bottom;
  self.FForm.Width  := FOriginalSize.Right;
end;

procedure TFormSetting.SetDefault;
begin
  inherited;
  ResetSizeAndPos;
end;

end.
