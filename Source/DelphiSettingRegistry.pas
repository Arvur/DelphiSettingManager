{-----------------------------------------------------------------------------
 Unit Name: DelphiSettingRegistry
 Author:    Erwien Saputra

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Purpose:   Encapsulates all interaction with the Registry.

 History:
 01/19/05 - Initial creation.
 02/20/05 - Fixed a bug when a key was missing on one treeview and exist at
            another, and that node is expanded, the treeview with missing key
            will expand the root node.
-----------------------------------------------------------------------------}
unit DelphiSettingRegistry;

interface

uses
  Classes, Registry, SysUtils, IntfObserver, Subject;

type
  IDelphiSettingRegistry = interface;

  TOnNodeChanged = procedure (const Sender : IDelphiSettingRegistry;
                              const CurrentPath : string;
                              const RefreshCurrentPath : boolean) of object;
  TOnValueNamesChanged = procedure (const Sender : IDelphiSettingRegistry;
                                    const CurrentPath : string;
                                    const Names : TStrings);

  TStringArray = array of string;

  //This interface will provide all necessary functionalities to open a setting.
  //Setting path is the full registry key to the Delphi Custom Setting.
  //This interface provides the ability to have selection, change the selection,
  //add a new key, delete existing key, and get children keys of a sub-key.
  IDelphiSettingRegistry = interface
  ['{B910A6AD-9BEB-4540-A17B-4D099E7B7AE9}']
    function GetSettingPath : string;
    function GetCurrentPath : string;

    function GetParent (APath : string) : string;
    function SetCurrentPath (const APath : string) : boolean;
    procedure GetChild (const APath : string; out StringArray : TStringArray);

    procedure OpenSetting (const SettingPath : string);

    function DeleteCurrentKey : string;
    procedure AddKey (const RelativePath : string);

    property SettingPath : string read GetSettingPath;
    property CurrentPath : string read GetcurrentPath;
  end;

  TDelphiSettingRegistry = class (TInterfacedObject, IDelphiSettingRegistry,
                                  ISubject)
  private
    FSettingPath : string;
    FReg : TRegistry;
    FCurrentIndex : integer;
    FValueNames : TStringList;
    FSubject : ISubject;
  protected
    function GetSettingPath : string;
    function GetCurrentPath : string;

    function SetCurrentPath (const APath : string) : boolean;
    function GetParent (APath : string) : string;
    procedure GetChild (const APath : string; out StringArray : TStringArray);

    procedure OpenSetting (const SettingPath : string);

    function DeleteCurrentKey : string;
    procedure AddKey (const RelativePath : string);

    property Subject : ISubject read FSubject implements ISubject;
  public
    constructor Create (const APath : string = '');
    destructor Destroy; override;
  end;

function GetLastDelimiterIndex (const AValue : string): integer; forward;
function GetFirstDelimiterIndex (const AValue : string): integer; forward;

implementation

uses
  Contnrs;

const
  NO_CURRENT = -2;
  ROOT       = -1;
  DELIMITER = '\';

function GetLastDelimiterIndex (const AValue : string): integer;
begin
  Result := LastDelimiter (DELIMITER, AValue);

  if Result > 1 then
    if AValue [Result - 1] = DELIMITER then
      Result := GetLastDelimiterIndex (Copy (AValue, 1, Result - 2));

end;

function GetFirstDelimiterIndex (const AValue : string): integer;
var
  ValueLength : integer;
begin
  ValueLength := Length (AValue);
  Result := Pos (DELIMITER, AValue);

  if Result < ValueLength then
    if AValue[Result + 1] = DELIMITER then
      Result := GetFirstDelimiterIndex (
                    Copy (AValue, Result + 1, ValueLength - (Result + 1)));
end;

{ TDelphiSettingKeys }

constructor TDelphiSettingRegistry.Create(const APath: string = '');
begin
  inherited Create;
  FReg          := TRegistry.Create;
  FValueNames   := TStringList.Create;
  FCurrentIndex := NO_CURRENT;
  FSubject      := TSubject.Create (self);

  if SameText (Trim (APath), EmptyStr) = false then
    OpenSetting (APath);
end;

destructor TDelphiSettingRegistry.Destroy;
begin
  FReg.Free;
  FValueNames.Free;
  FSubject.Enabled := false;
  FSubject := nil;
  inherited;
end;

function TDelphiSettingRegistry.GetSettingPath: string;
begin
  Result := FSettingPath;
end;

procedure TDelphiSettingRegistry.OpenSetting(const SettingPath: string);
begin
  FSettingPath := ExcludeTrailingBackslash (SettingPath);

  //It is important to include backslash at the beginning. The path is always a
  //full path under HKCU. Without backslash in the beginning, Regedit.OpenKey
  //may try to open a sub key under the current key.
  if SameText (FSettingPath[1], '\') = false then
    FSettingPath := '\' + FSettingPath;

  if FReg.OpenKey(FSettingPath, false) = false then
    raise Exception.Create (FSettingPath + ' does not exist.');

  FSubject.Notify;
end;

//Return the current path, start it with a backslash.
function TDelphiSettingRegistry.GetCurrentPath: string;
begin
  Result := '\' + IncludeTrailingBackslash (FReg.CurrentPath);
end;

//Open the current path. Any update to the selection will notify the subscriber
//of this class.
function TDelphiSettingRegistry.SetCurrentPath (const APath: string) : boolean;
begin
  Result := FReg.OpenKey (APath, false);

  if (Result = true) then
    FSubject.Notify;
end;

//Add a key. Relative Path is the path relative to the SettingPath. This method
//does not add any values or sub-keys.
procedure TDelphiSettingRegistry.AddKey(const RelativePath: string);
var
  CorrectedPath : string;
begin
  CorrectedPath := FSettingPath + '\' + ExcludeTrailingBackslash (RelativePath);

  if FReg.KeyExists (CorrectedPath) = true then
    Exit;

  if FReg.CreateKey (CorrectedPath) = false then
    raise Exception.Create('Cannot Create ' + CorrectedPath);

  FReg.OpenKey (CorrectedPath, false);

  //Notify all observer.
  FSubject.Notify;
end;

//Delete the current key. After deleting a key, this method does not broadcast
//messages to the subscriber. When a key is deleted, the selection is changed
//to the parent path of the deleted node (probably I should change it to nil).
//If this action notifies all subscribers it will move all TreeView selection.
//It looked weird.
function TDelphiSettingRegistry.DeleteCurrentKey : string;
var
  ParentPath : string;
begin
  if FReg.DeleteKey (GetCurrentPath) = false then
    raise Exception.Create ('Failed to delete ' + GetCurrentPath);

  ParentPath := self.GetParent (GetCurrentPath);
  Result := self.GetCurrentPath;

  SetCurrentPath (ParentPath);
end;

procedure TDelphiSettingRegistry.GetChild(const APath: string;
  out StringArray: TStringArray);
var
  CurrentPath : string;
  List : TStringList;
  Loop : integer;
begin
  CurrentPath := self.GetCurrentPath;
  List := TStringList.Create;
  try
    if FReg.OpenKey (APath, false) = false then
      raise Exception.Create ('Error opening ' + APath);

     FReg.GetKeyNames(List);
     SetLength (StringArray, List.Count);

     for Loop := 0 to List.Count - 1 do
       StringArray[Loop] := List[Loop];
  finally
    //Assign the CurrentPath directly to the FReg to prevent OnNodeChanged from
    //firing.
    FReg.OpenKey (CurrentPath, false);
    List.Free;
  end;

end;

function TDelphiSettingRegistry.GetParent(APath: string): string;
begin
  APath := ExcludeTrailingBackslash (APath);
  Result := Copy (APath, 1, GetLastDelimiterIndex (APath) - 1);
end;

end.
