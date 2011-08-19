{-----------------------------------------------------------------------------
 Unit Name: SettingCollection

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   Based on Corbin Dunn's blog, Delphi IDE recognizes -r parameter and
            it can be used to load a specific registry key under 'Software\Borland'
            This is a very useful feature, and I decided to manage it.

            First I stored all settings under Software\Borland, but it become
            convoluted, so I moved all custom settings under
            \Software\Borland\CustomSettings.  Last problem, I have three Delphi
            and Galileo based IDEs installed, so I added another sub key under
            CustomSettings.

            All custom registry settings will be stored at:
            Delphi 6  - \Software\Borland\CustomSettings\Delphi6
            Delphi 7  - \Software\Borland\CustomSettings\Delphi7
            C#Builder - \Software\Borland\CustomSettings\BDS1
            Delphi 8  - \Software\Borland\CustomSettings\BDS2

            This makes it more manageable.

            To use those settings, pass \CustomSetting\Delphi7\Barebone to the
            -r parameter.

            I created this class to manage IDE settings in the registry for a
            specific IDE. The supported IDEs are (Delphi 6, Delphi 7,
            BDS 1 (C#Builder), BDS2 (Delphi 8) and BDS3 (Delphi 2005).

            The class is exposed as interface, with a factory method which takes
            TIDEVersion enumerated type as parameter. The factory function will
            create the registry key automatically.

            The ISettingCollection manages all registry settings for a specific
            IDE.
            GetInstalledIDEVersion returns all detected IDE in a machine.
            GetExecutablePath returns the executable filename for anIDE.
 History:
 11/05/2004 - Initial version.
 12/07/2005 - Added support for BDS 3.
 12/24/2004 - Refactored the case statements.  All case statements are pulled
              out from the method and encapsulated as a function.
              Exposed one more function GetCustomRegistryPath. This function is
              used to save registry information to a file.
              Exposed GetCustomRegistryPath for the ability to save a custom
              setting to a .reg file.
 01/02/2004 - Two update for Lachlan Gemmell's Setting Editor.
              Updated GetCustomRegistryPath by adding a default constant
              parameter, IncludeIDEVersionSubKey. This option is to include the
              IDE version at the end of the string.  The default value for this
              parameter is false, it will not affect the existing code.
              Exposed the GetIDEKey from the implementation to interface
              section.
-----------------------------------------------------------------------------}


unit SettingCollection;

interface

uses
  Classes;

type
  //Enumerated type for all supported Delphi / Galileo based IDEs.
  TIDEVersion = (Delphi6,
                 Delphi7,
                 BDS1 {Borland C# Builder 1},
                 BDS2 {Borland Delphi 8 for .NET},
                 BDS3 {Borland Delphi 2005},
                 BDS4 {Borland Delphi 2006},
                 BDS5 {CodeGear Delphi 2007},
                 BDS6 {CodeGear Delphi 2009},
                 BDS7 {Embarcadero Delphi 2010},
                 BDS8 {Embarcadero Delphi XE});
  //Set of TIDEVersion, it is returned by GetInstalledIDEVersion.
  TInstalledIDE = set of TIDEVersion;

  TIDEConstants = record
    // IDE display name for main form
    DisplayName: string;
    // Short name used for naming and SettingsRoot
    ShortName: string;
    //Registry branch including company name
    IDERoot: string;
    //The key of each IDE.
    RegistryKey: string;
    //Whenever the IDE is started using -r<RegistryKey> parameter, Delphi/BDS will
    //create <RegistryKey>\IDE Version, and put all settings under that key, not
    //directly under the <RegistryKey>.
    //These constants are useful when the user wants to copy setting from their
    //existing IDE (the default) to the new custom setting.
    IDEVersion: string;
  end;

const
  IDEParams: array [TIDEVersion] of TIDEConstants = (
    (DisplayName:    'Delphi 6'; ShortName: 'Delphi6'; IDERoot:     'Software\Borland\'; RegistryKey: 'Delphi\6.0\'; IDEVersion: '6.0'),
    (DisplayName:    'Delphi 7'; ShortName: 'Delphi7'; IDERoot:     'Software\Borland\'; RegistryKey: 'Delphi\7.0\'; IDEVersion: '7.0'),
    (DisplayName:  'C# Builder'; ShortName:    'BDS1'; IDERoot:     'Software\Borland\'; RegistryKey:    'BDS\1.0\'; IDEVersion: '1.0'),
    (DisplayName:    'Delphi 8'; ShortName:    'BDS2'; IDERoot:     'Software\Borland\'; RegistryKey:    'BDS\2.0\'; IDEVersion: '2.0'),
    (DisplayName: 'Delphi 2005'; ShortName:    'BDS3'; IDERoot:     'Software\Borland\'; RegistryKey:    'BDS\3.0\'; IDEVersion: '3.0'),
    (DisplayName: 'Delphi 2006'; ShortName:    'BDS4'; IDERoot:     'Software\Borland\'; RegistryKey:    'BDS\4.0\'; IDEVersion: '4.0'),
    (DisplayName: 'Delphi 2007'; ShortName:    'BDS5'; IDERoot:     'Software\Borland\'; RegistryKey:    'BDS\5.0\'; IDEVersion: '5.0'),
    (DisplayName: 'Delphi 2009'; ShortName:    'BDS6'; IDERoot:    'Software\CodeGear\'; RegistryKey:    'BDS\6.0\'; IDEVersion: '6.0'),
    (DisplayName: 'Delphi 2010'; ShortName:    'BDS7'; IDERoot:    'Software\CodeGear\'; RegistryKey:    'BDS\7.0\'; IDEVersion: '7.0'),
    (DisplayName:   'Delphi XE'; ShortName:    'BDS8'; IDERoot: 'Software\Embarcadero\'; RegistryKey:    'BDS\8.0\'; IDEVersion: '8.0')
  );

type
  //This interface can be used to manage custom settings for a specific IDS.
  ISettingCollection = interface
  ['{24C9531A-A384-4F28-A35A-62CB058B4FE8}']
    //Add a new registry setting.  The name must be as descriptive as possible
    //The length of the overall registry key cannot exceed 255 characters.
    procedure AddNewSetting (const SettingName : string;
        const UseDefaultSetting : boolean);
    //Delete an existing key.
    procedure DeleteSetting (const SettingName : string);
    //Copy an existing key with its configuration as another setting. After this
    //operation those two keys will be identical.
    procedure CopySetting (const SourceName, DestName : string);
    //Rename an existing key.
    procedure RenameSetting (const OldName, NewName : string);
    //Return all custom settings for this IDE.
    procedure ReadSettings (const AStrings : TStrings);

    function GetCustomRegistryKey (const SettingName : string): string;
    function GetCustomRegistryPath (const SettingName : string;
        const IncludeIDEVersionSubKey: boolean = false): string;

    //Properties getters.
    function GetSettingCount : integer;
    function GetSettingName (Index : integer) : string;
    function GetIDEVersion : TIDEVersion;

    //Return the number of custom settings.
    property SettingCount : integer read GetSettingCount;
    property SettingName [Index : integer] : string read GetSettingName;
    property IDEVersion : TIDEVersion read GetIDEVersion;
  end;

//Factory function
function CreateSettingCollection (const AIDEVersion : TIDEVersion): ISettingCollection; forward;

//Returns all installed Delphi IDEs and Galileo based IDEs that this application
//supports.
function GetInstalledIDEVersion (out AInstalledIDE : TInstalledIDE) : boolean; forward;

//Returns the path and file name of an IDE based on the IDEVersion. If the IDE
//does not exist, this function returns an empty string.
function GetExecutablePath (const IDEVersion : TIDEVersion): string; forward;

//Returns the registry key for a specific IDE.
//For example, for Delphi 2005 it will be Software/Borland/BDS/3.0
function GetIDEKey (const AIDEVersion : TIDEVersion) : string; forward;

implementation

uses
  Registry, SysUtils, Windows;

const
  //The value under IDE key that contains the path and the filename of the
  //executable.
  APP_VALUE        = 'App';
  //Root for all IDE specific settings.
  SettingsRoot     = 'CustomSettings\';

type
  TSettingCollection = class (TInterfacedObject, ISettingCollection)
  private
    FSettingNames : TStrings;
    FReg : TRegistry;
    FIDEVersion : TIDEVersion;
  protected
    procedure AddNewSetting (const SettingName : string;
                             const UseDefaultSetting : boolean);
    procedure DeleteSetting (const SettingName : string);
    procedure CopySetting (const SourceName, DestName : string);
    procedure RenameSetting (const OldName, NewName : string);
    procedure ReadSettings (const AStrings : TStrings);

    function GetCustomRegistryKey (const SettingName : string): string;
    function GetCustomRegistryPath (const SettingName : string;
        const IncludeIDEVersionSubKey: boolean = false): string;

    procedure LoadSettingNames;

    function GetSettingCount : integer;
    function GetSettingName (Index : integer) : string;
    function GetIDERoot: string;
    function GetIDEVersion : TIDEVersion;

  public
    constructor Create (const IDEVersion : TIDEVersion);
    destructor Destroy; override;
  end;

//All of these functions below work return string, based on the parameter. These
//functions were created to remove all case statements from the class.
//Returns the registry key for Galileo based IDEs.
function GetIDEKey (const AIDEVersion : TIDEVersion) : string;
begin
  try
    Result := IDEParams[AIDEVersion].IDERoot + IDEParams[AIDEVersion].RegistryKey;
  except
    raise Exception.Create ('Unknown IDE.');
  end;
end;

function GetIDEVersionAsString (const AIDEVersion : TIDEVersion) : string;
begin
  try
    Result := IDEParams[AIDEVersion].IDEVersion;
  except
    raise Exception.Create ('Unknown IDE.');
  end;
end;

function GEtCustomSettingKey (const AIDEVersion : TIDEVersion) : string;
begin
  try
    Result := SettingsRoot + IDEParams[AIDEVersion].ShortName + '\';
  except
    raise Exception.Create ('Unknown IDE');
  end;
end;

//This function returns true if there is at least one IDE installed.
//The installed IDEs are returned as array.
function GetInstalledIDEVersion (out AInstalledIDE : TInstalledIDE) : boolean;
  function IDEInstalled (const IDEVersion : TIDEVersion): boolean;
  begin
    Result := SameText (GetExecutablePath (IDEVersion), EmptyStr) = false;
  end;

var
  Loop: TIDEVersion;
begin
  AInstalledIDE := [];

  for Loop := Low (TIDEVersion) to High (TIDEVersion) do
    if IDEInstalled (Loop) then
      Include (AInstalledIDE, Loop);

  Result := AInstalledIDE <> [];
end;

//Returns the path of the IDE executable.  Returns an empty string if the
//desired IDE is not installed.
function GetExecutablePath (const IDEVersion : TIDEVersion): string;
var
  Reg : TRegistry;
  Key : string;
begin
  Result := '';

  Key := GetIDEKey (IDEVersion);
  Reg := TRegistry.Create;

  try
    Reg.RootKey := HKEY_CURRENT_USER;

    if (Reg.OpenKey (Key, false)) and
       (FileExists (Reg.ReadString (APP_VALUE))) then
      Result := Reg.ReadString (APP_VALUE)
    else begin
      Reg.RootKey := HKEY_LOCAL_MACHINE;

      if (Reg.OpenKey (Key, false)) and
         (FileExists (Reg.ReadString (APP_VALUE))) then
        Result := Reg.ReadString (APP_VALUE);
    end;
  finally
    Reg.Free;
  end;
end;

function CreateSettingCollection (const AIDEVersion : TIDEVersion): ISettingCollection;
begin
  Result := TSettingCollection.Create (AIDEVersion);
end;

{ TSettingCollection }

//Add a new setting, if the UseDefaultSetting is set to true, it will use the
//default IDE setting.  The default setting is the IDE setting that got loaded
//without -r parameter.  If UseDefaultSetting is false, the custom registry key
//will be left blank, and let the IDE populates it with the factory setting.
procedure TSettingCollection.AddNewSetting(const SettingName: string;
    const UseDefaultSetting : boolean);
begin
  Assert (SameText (FReg.CurrentPath,
                    GetIDERoot + GetCustomSettingKey (FIDEVersion)));

  if FReg.KeyExists (SettingName) = true then
    raise Exception.Create ('Setting already exists.');

  //Enforce the length of the key.  Overall it should not exceed 255 characters,
  //I could not find whether the maximum length includes HKEY_CURRENT_USER or
  //not.  Just to be safe, limit the SettingName to 100.
  if (Length (SettingName)) > 100 then
    raise Exception.Create ('Setting name is too long.');

  //If the UseDefaultSetting is true, creates a new custom IDE setting with all
  //properties. This is done by copying that setting from the IDE key root.
  //Delphi/BDS will create use the setting not directly under the SettingName,
  //but it will use the setting under the SettingName\IDE Version.
  if (UseDefaultSetting = true) then
    FReg.MoveKey ('\' + GetIDEKey (FIDEVersion),
                  SettingName + '\' + GetIDEVersionAsString (FIDEVersion),
                  false)
  else
    //If user does not want the new setting to have settings that IDE has by
    //default, creates a blank registry key. IDE will populate it with factory
    //default.
    FReg.CreateKey (SettingName);

  LoadSettingNames;
end;

//Copy a setting to another setting.
procedure TSettingCollection.CopySetting(const SourceName,
  DestName: string);
begin
  Assert (SameText (FReg.CurrentPath,
                    GetIDERoot + GetCustomSettingKey (FIDEVersion)));

  if (Trim (SourceName) = EmptyStr) then
    raise Exception.Create ('Invalid parameter. Existing setting not defined.');

  if (Trim (DestName) = EmptyStr) then
    raise Exception.Create ('Invalid parameter. New key not defined.');

  if FReg.KeyExists (DestName) = true then
    raise Exception.Create (DestName + ' is already exists');

  FReg.MoveKey (SourceName, DestName, false);

  LoadSettingNames;
end;

constructor TSettingCollection.Create(const IDEVersion: TIDEVersion);
begin
  inherited Create;

  FReg := TRegistry.Create;
  FSettingNames := TStringList.Create;
  FIDEVersion := IDEVersion;

  if GetExecutablePath (IDEVersion) = EmptyStr then
    raise Exception.Create ('The desired IDE does not exists.');

  FReg.OpenKey (GetIDERoot + GetCustomSettingKey (FIDEVersion), true);

  LoadSettingNames;
end;

//Delete an existing setting.
procedure TSettingCollection.DeleteSetting(const SettingName: string);
begin
  if FReg.KeyExists (SettingName) then
    FReg.DeleteKey (SettingName);

  LoadSettingNames;
end;

destructor TSettingCollection.Destroy;
begin
  FReg.Free;
  FSettingNames.Free;
  inherited;
end;

//Returns the root key of the customized registry setting. The returned value of
//this function can be used for the -r parameter.
function TSettingCollection.GetCustomRegistryKey(
  const SettingName: string): string;
begin
  Result := GetCustomSettingKey (FIDEVersion) + SettingName;
end;

//Return the full path of the custom setting.
function TSettingCollection.GetCustomRegistryPath(const SettingName: string;
    const IncludeIDEVersionSubKey: boolean = false): string;
begin
  Result := GetIDERoot + GetCustomRegistryKey (SettingName);

  //If the IDE version is required, include it at the end of the string.
  if IncludeIDEVersionSubKey = true then
    Result := Result + '\' + GetIDEVersionAsString (FIDEVersion);
end;

function TSettingCollection.GetIDERoot: string;
begin
  Result := IDEParams[FIDEVersion].IDERoot;
end;

function TSettingCollection.GetIDEVersion: TIDEVersion;
begin
  Result := self.FIDEVersion;
end;

function TSettingCollection.GetSettingCount: integer;
begin
  Result := FSettingNames.Count;
end;

function TSettingCollection.GetSettingName (Index : integer): string;
begin
  if Index >= GetSettingCount then
    raise Exception.Create ('Index out of bounds.');

  Result := FSettingNames [Index];
end;

procedure TSettingCollection.LoadSettingNames;
begin
  FSettingNames.Clear;
  FReg.GetKeyNames (FSettingNames);
end;

procedure TSettingCollection.ReadSettings(const AStrings: TStrings);
begin
  LoadSettingNames;

  AStrings.Assign (FSettingNames);
end;

procedure TSettingCollection.RenameSetting(const OldName,
  NewName: string);
begin
  //Copy the setting, and delete the old key.
  CopySetting (OldName, NewName);
  DeleteSetting (OldName);

  //Refresh the key names.
  LoadSettingNames;
end;

end.
