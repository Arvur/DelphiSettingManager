{-----------------------------------------------------------------------------
 Unit Name: SaveCustomSetting

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   Logic to save a custom setting to a .reg file.

 Originally the idea is to have a functionality to save and to load custom
 setting to a .reg file. It is more difficult than I thought. I could not find
 any reference to save the registry key programmatically.

 Finally, I decided to use regedit command line parameter to save the custom
 path registry key.  My intention was to have a .reg key that I can create in
 one machine and moved to another machine, I do not think it is a good idea to
 keep the license information in the .reg file. Unfortunately, the .reg file for
 Windows XP and Win2K is a unicode file. I do not want this utility to depend on
 3rd party utility to read the .reg file, so the ability to load a custom
 setting is dropped.

 To remove the license information, the license values is removed ONLY FOR THE
 SELECTED CUSTOM SETTING.  IT DOES NOT REMOVE THE INFORMATION FROM THE BORLAND
 DELPHI REGISTRY KEY OR FROM THE LOCAL_MACHINE KEY.

 DELPHI SETTING MANAGER DOES NOT DELETE OR ALTER THE DEFAULT DELPHI IDE REGISTRY
 REGISTRY KEYS BELOW WILL NOT BE ALTERED IN ANY WAY.
   HKEY_CURRENT_USER/Software/Borland/Delphi/6.0
   HKEY_CURRENT_USER/Software/Borland/Delphi/7.0
   HKEY_CURRENT_USER/Software/Borland/BDS/1.0
   HKEY_CURRENT_USER/Software/Borland/BDS/2.0
   HKEY_CURRENT_USER/Software/Borland/BDS/3.0

 My test shows that deleting the license keys for the Custom Settings are OK,
 Delphi still can use the custom setting without any problem.

 The license keys are only exist for Delphi 6 and Delphi 7. The license keys
 does not exist for other IDEs.

 History:
 12/26/2004 - Initial version. The logic to save custom setting to a reg file,
              the license keys are not stored to the .reg file.
 01/05/2004 - Fixed a bug with the firewall, the string constant was incorrect
              and prevent saving a correct setting.
-----------------------------------------------------------------------------}
unit LoadSaveCustomSetting;

interface

type
  //Interface to save a custom setting. This interface requires the file name,
  //the path of the custom setting.  The path means the registry full registry
  //path without the HKEY_CURRENT_USER.
  //The result is a .reg file that can be imported with regedit. Before saving
  //the registry, the license information (serial number, license key, etc.)
  //will be deleted from the custom path. So far, deleting the license does not
  //harm the custom setting.
  //This setting shall not mess with the original setting, only the custom
  //setting shall be updated.
  ISaveSetting = interface
  ['{7981C60E-385D-4AEE-891B-CED674A7D7FE}']
    function GetFileName : string;
    function GetCustomSettingPath : string;
    procedure SetFileName (const AFileName : string);
    procedure SetCustomSettingPath (const AValue : string);

    procedure SaveSettingToFile;

    property FileName : string read GetFileName write SetFileName;
    property CustomSettingPath : string read GetCustomSettingPath
        write SetCustomSettingPath;
  end;

  TSaveSetting = class (TInterfacedObject, ISaveSetting)
  private
    FFileName : string;
    FCustomSettingPath : string;
    procedure DeleteLicenseInfo;

  protected
    function GetFileName : string;
    function GetCustomSettingPath : string;
    procedure SetFileName (const AFileName : string);
    procedure SetCustomSettingPath (const AValue : string);

    procedure SaveSettingToFile;
  public
    constructor Create;
  end;

function GetSaveSetting : ISaveSetting; forward;

implementation

uses
  ShellAPI, Windows, Classes, SysUtils, Registry;

const
  //One option that I considered was to have the functionality to save only the
  //package information and the library path.  The fact that Win2K and WinXP
  //regedit saves the .reg file as unicode make it difficult. Hopefully it can
  //be implemented in the future.
  KNOWN_ASSEMBLIES     = 'Known Assemblies';
  KNOWN_IDE_ASSEMBLIES = 'Known IDE Assemblies';
  KNOWN_IDE_PACKAGES   = 'Known IDE Packages';
  KNOWN_PACKAGES       = 'Known Packages';
  LIBRARY_KEY          = 'Library';
  //Registry keys for the license information.
  SERIAL_NUMBER        = 'LMLIC';
  LICENSE_KEY          = 'LMKEY';
  //Beside those two keys above, Delphi 6 has this key below.
  DELPHI6_LM           = 'LM';


function GetSaveSetting : ISaveSetting;
begin
  Result := TSaveSetting.Create;
end;

{ TSaveSetting }

constructor TSaveSetting.Create;
begin
  inherited Create;
  FFileName          := EmptyStr;
  FCustomSettingPath := EmptyStr;
end;

procedure TSaveSetting.SetFileName(const AFileName: string);
begin
  if SameText (FFileName, AFileName) = false then
    FFileName := AFileName;
end;

function TSaveSetting.GetCustomSettingPath: string;
begin
  Result := FCustomSettingPath;
end;


function TSaveSetting.GetFileName: string;
begin
  Result := FFileName;
end;

//Assign the custom setting. The custom setting must not started with backslash
//or HKEY_CURRRENT_USER string.
procedure TSaveSetting.SetCustomSettingPath(const AValue: string);
begin
  FCustomSettingPath := AValue;

  if Pos ('HKEY_CURRENT_USER', FCustomSettingPath) = 1 then
    FCustomSettingPath := Copy (FCustomSettingPath,
                                Length ('HKEY_CURRENT_USER'),
                                Length (FCustomSettingPath));

  if Pos ('\', FCustomSettingPath) = 1 then
    FCustomSettingPath := Copy (FCustomSettingPath, 2,
                                Length (FCustomSettingPath));

  if SameText (FCustomSettingPath, EmptyStr) then
    raise Exception.Create ('Invalid registry path.');
end;

//Use Regedit.exe to save the setting to a .reg file.
procedure TSaveSetting.SaveSettingToFile;
const
  REGEDIT_PARAM_FMT = '/e "%0:s" "HKEY_CURRENT_USER\%1:S"';
begin
  DeleteLicenseInfo;
  ShellExecute (0, nil, PChar ('regedit'),
                PChar (Format (REGEDIT_PARAM_FMT,
                               [FFileName, FCustomSettingPath])),
                nil, SW_HIDE);
end;

//Delete the license information from the custom setting.  Originally I wanted
//to remove it in the .reg file, but because for WinXP it is a unicode file,
//I cannot read and make modification. The license information are deleted from
//the custom registry keys.
//This method cannot delete license information other than in the custom setting
//registry keys.
procedure TSaveSetting.DeleteLicenseInfo;
var
  Reg : TRegistry;
  List : TStringList;
begin
  Reg  := TRegistry.Create;
  List := TStringList.Create;
  try
    if Reg.OpenKey (FCustomSettingPath, false) = false then
      raise Exception.Create ('Cannot read ' +  FCustomSettingPath);

    //Firewall.  Do not delete other license information if the path is not the
    //custom setting registry path.
    if Pos ('Software\Borland\CustomSettings\', FCustomSettingPath) <> 1 then
      raise Exception.Create ('This utility cannot modify registry keys other ' +
                              'than the custom setting registry keys.');

    Reg.GetKeyNames (List);

    if List.Count > 1 then
      raise Exception.Create ('Ambiguous settings.');

    Reg.OpenKey (List [0], false);

    if Reg.ValueExists (SERIAL_NUMBER) then
      Reg.DeleteValue (SERIAL_NUMBER);

    if Reg.ValueExists (LICENSE_KEY) then
      Reg.DeleteValue (LICENSE_KEY);

    if Reg.ValueExists (DELPHI6_LM) then
      Reg.DeleteValue (DELPHI6_LM);
  finally
    Reg.Free;
    List.Free;
  end;
end;

end.
