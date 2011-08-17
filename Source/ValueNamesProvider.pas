{-----------------------------------------------------------------------------
 Unit Name: ValueNamesProvider

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   

 IValueNamesProvider is an observer of the IDelphiSettingRegistry. Whenever
 it reads new names from the registry keys, it triggers OnValueNamesChanged and
 OnStringChanged.

 This class is intended to be used with a TListBox, the form is responsible to
 arrange the names. If the user select an item on the listbox, the TListBox
 should set the SelectedIndex. Whenever the SelectedIndex is changed, it will
 trigger OnStringChanged.

 The main purpose for this class is as provider for the names under a key and
 to manage registry value names manipulation.

 The form that displays the ListBox which uses IValueNameProvider as data source
 displays two Listboxes and synchronizes the value names. The form may inserts
 blank lines between the ListBox items. For that reason ListBox.ItemIndex cannot
 be used directly with the IValueNamesProvider.SelectedItemIndex.

 Currently, this interface does not support multi-select items.

 The ListBox.ItemIndex
 may not be used directly to assign
 History:
 02/01/05 - Initial creation.
 02/17/05 - Implemented ClearSelectedValue. Updated with a lot of comments.
 02/19/05 - Update CopySelectedValue, it calls UpdateValue if the Name is
            already exist.
-----------------------------------------------------------------------------}
unit ValueNamesProvider;

interface

uses
  DelphiSettingRegistry, Classes;

type
  TIntfNotifyEvent = procedure (const Intf : IInterface) of object;

  IValueNamesProvider = interface
  ['{098F1A23-0169-4812-B00E-BD6FB2F77306}']
    function GetRelativePath : string;
    function GetSelectedIndex : integer;
    function GetValueNames : TStrings;
    function GetAsString : string;
    function GetOnValueNamesChanged : TIntfNotifyEvent;
    function GetOnAsStringChanged : TIntfNotifyEvent;
    procedure SetSelectedIndex (const Idx : integer);
    procedure SetOnValueNamesChanged (const Value : TIntfNotifyEvent);
    procedure SetOnAsStringChanged (const Value : TIntfNotifyEvent);

    function GetNameIndex (const AName : string): integer;
    function GetSettingPath : string;
    procedure SetSettingPath (const ASettingPath : string);
    procedure CopySelectedValue (const Source : IValueNamesProvider);
    procedure DeleteSelectedValue;
    procedure ClearSelectedValue;

    property RelativePath : string read GetRelativePath;
    property ValueNames : TStrings read GetValueNames;
    property AsString : string read GetAsString;

    property SelectedIndex : integer read GetSelectedIndex write SetSelectedIndex;
    property OnValueNamesChanged : TIntfNotifyEvent read GetOnValueNamesChanged
        write SetOnValueNamesChanged;
    property OnAsStringChanged : TIntfNotifyEvent read GetOnAsStringChanged
        write SetOnAsStringChanged;
  end;


function GetValueNamesProvider (const Intf : IDelphiSettingRegistry) : IValueNamesProvider;

implementation

uses
  SysUtils, Registry, IntfObserver;

type
  TValueNamesProvider = class (TInterfacedObject, IValueNamesProvider,
                               IObserver)
  private
    FOnValueNamesChanged : TIntfNotifyEvent;
    FOnAsStringChanged : TIntfNotifyEvent;
    FSelectedIndex : integer;
    FValueNames : TStringList;
    FValue : string;
    FReg : TRegistry;
    FSettingPath : string;

    procedure LoadValueNames;
    procedure ClearValueNames;
    procedure SetSelectedIndex (const Idx : integer);
    procedure UpdateValue;
  protected
    function GetRelativePath : string;
    function GetSelectedIndex : integer;
    function GetValueNames : TStrings;
    function GetAsString : string;
    function GetOnValueNamesChanged : TIntfNotifyEvent;
    function GetOnAsStringChanged : TIntfNotifyEvent;
    procedure SetOnValueNamesChanged (const Value : TIntfNotifyEvent);
    procedure SetOnAsStringChanged (const Value : TIntfNotifyEvent);

    function GetNameIndex (const AName : string): integer;
    function GetSettingPath : string;
    procedure SetSettingPath (const ASettingPath : string);
    procedure CopySelectedValue (const Source : IValueNamesProvider);
    procedure DeleteSelectedValue;
    procedure ClearSelectedValue;

    procedure Update (const Subject : ISubject; const AIntf : IInterface = nil);
  public
    constructor Create;
    destructor Destroy; override;

  end;

//Factory function.
function GetValueNamesProvider (const Intf : IDelphiSettingRegistry): IValueNamesProvider;
begin
  Result := TValueNamesProvider.Create;
  Result.SetSettingPath (Intf.SettingPath);
  (Intf as ISubject).AttachObserver (Result as IObserver);
end;

{ TValueNamesProvider }

//Returns the index of the selected value name.
function TValueNamesProvider.GetSelectedIndex: integer;
begin
  Result := self.FSelectedIndex;
end;

constructor TValueNamesProvider.Create;
begin
  inherited Create;
  FSelectedIndex := -1;
  FValueNames := TStringList.Create;
  FValueNames.Sorted := true;
  FReg := TRegistry.Create;
end;

//Event handler setter.
procedure TValueNamesProvider.SetOnAsStringChanged(
  const Value: TIntfNotifyEvent);
begin
  FOnAsStringChanged := Value;
end;

//Retrieve all value names.
function TValueNamesProvider.GetValueNames: TStrings;
begin
  Result := FValueNames;
end;

//Event handler getter.
function TValueNamesProvider.GetOnValueNamesChanged: TIntfNotifyEvent;
begin
  Result := FOnValueNamesChanged;
end;

//Event handler setter.
procedure TValueNamesProvider.SetOnValueNamesChanged(
  const Value: TIntfNotifyEvent);
begin
  FOnValueNamesChanged := Value;
end;

//Returns the value of the selected registry value name.
function TValueNamesProvider.GetAsString: string;
begin
  Result := FValue;
end;

//Event handler getter.
function TValueNamesProvider.GetOnAsStringChanged: TIntfNotifyEvent;
begin
  Result := FOnAsStringChanged;
end;

//Returns the relative path of the current Delph Registry Setting. This function
//should not be here, it is more appopriate to have it at the
//IDelphiSettingRegistry. Unfortunately it has to be here for now, as this class
//needs this information and this class does not have reference to the
//IDelphiSettingRegistry.
function TValueNamesProvider.GetRelativePath: string;
begin
  //Returns the path after the setting path. Setting Path is string from
 //'\Software' to the current setting name.
  Result := Copy (FReg.CurrentPath, Length (self.FSettingPath) + 1,
                  Length (FReg.CurrentPath));
end;

destructor TValueNamesProvider.Destroy;
begin
  FValueNames.Free;
  inherited;
end;

//Returns the internal list index for AName.
function TValueNamesProvider.GetNameIndex(const AName: string): integer;
begin
  Result := FValueNames.IndexOf (AName);
end;

//Set the currently selected index.
procedure TValueNamesProvider.SetSelectedIndex(const Idx: integer);
begin
  if (FSelectedIndex = Idx) then
    Exit;

  FSelectedIndex := Idx;
  UpdateValue;
end;

//Reads the string representatino of the selected value name.
procedure TValueNamesProvider.UpdateValue;
var
  DataType : TRegDataType;
  ValueName : string;
  ValueExist : boolean;
begin
  ValueExist := (FSelectedIndex <> -1) and
                FReg.ValueExists (FValueNames [FSelectedIndex]);

  if ValueExist then begin
    ValueName := FValueNames [FSelectedIndex];
    DataType := FReg.GetDataType (ValueName);

    //Read the value and stored it in FValue. If the key is a binary key, set
    //FValue as {Binary}.
    case DataType of
      rdString,
      rdExpandString : FValue := FReg.ReadString (ValueName);
      rdInteger      : FValue := IntToStr (FReg.ReadInteger (ValueName));
      rdBinary       : FValue := '(Binary)';
    else
      FValue := '(Unknown)';
    end;
  end
  else
    FValue := '';

  if Assigned (FOnAsStringChanged) = true then
    FOnAsStringChanged (self);
end;

//This method loads the FValueNames with the value names of the selected
//selected registry key.  FReg is alredy opened the 'current key'.
procedure TValueNamesProvider.LoadValueNames;
begin
  //Reset the selected index and load the value names for the current key.
  FSelectedIndex := -1;
  FReg.GetValueNames (FValueNames);

  if Assigned (FOnValueNamesChanged) then
    FOnValueNamesChanged (self);

  //Update the FValue. UpdateValue will trigger FOnAsStringChanged.
  UpdateValue;
end;

//Clear the value names.
procedure TValueNamesProvider.ClearValueNames;
begin
  //Clear the value names and reset the selected index.
  FValueNames.Clear;
  FSelectedIndex := -1;

  if Assigned (FOnValueNamesChanged) then
    FOnValueNamesChanged (self);

  //UpdateValue will clear the FValue by the virtue that FSelectedIndex is -1,
  //and trigger the FOnAsStringChanged.
  UpdateValue;
end;

//This method is called by the Subject, IDelphiSettingRegistry.
//This method reads the current registry key from IDelphiSettingRegistry, and
//then opens the same key with the internal FReg.
procedure TValueNamesProvider.Update(const Subject: ISubject;
  const AIntf: IInterface);
var
  Intf : IDelphiSettingRegistry;
begin
  Intf := Subject as IDelphiSettingRegistry;

  if (FReg.CurrentPath = Intf.CurrentPath) then
    Exit;

  //FReg should be able to open the current path, as both points to the same
  //registry key. If for some reason this key cannot be opened, reset the FReg
  //to open the root key and clear the value names.
  if (FReg.OpenKey (Intf.CurrentPath, false)) = false then begin
    FReg.OpenKey('\', false);
    ClearValueNames;
  end
  else
    //The key is found and was opened. Load the value names and notify the form.
    LoadValueNames;
end;

//This method sets the setting path. Setting path is the path from '\Software'
//to the delphi setting registry name. This information is required to get the
//relative path.
procedure TValueNamesProvider.SetSettingPath(const ASettingPath: string);
begin
  FSettingPath := ASettingPath;
end;

//Using other IValueNamesProvider as source, copy the selected value. This is
//where the GetRelativePath is needed. If the key from the source exist in this
//class, this method will copy the value.
procedure TValueNamesProvider.CopySelectedValue(
  const Source: IValueNamesProvider);
var
  ValueName, Path : string;
  Reg : TRegistry;
  Buffer : pointer;
  BytesRead,
  DataSize : integer;
  ValueExist : boolean;
begin
  if Source = nil then
    Exit;

  //The relative path of the source is different than the path of this object,
  //that means the source path does not exist.  Create it first.
  if Source.RelativePath <> self.GetRelativePath then
    raise Exception.Create ('Relative paths must be identical.');

  //Get the name of the selected key from the source provider and checks if the
  //same value name exist in this IValueNamesProvider. ValueExist determines
  //whether this class neeed to reload the value names or not.
  ValueName := Source.ValueNames [Source.SelectedIndex];
  ValueExist := FValueNames.IndexOf (ValueName) > -1;
  //Get the full path of the source key.
  Path := IncludeTrailingBackslash (Source.GetSettingPath) +
          Source.RelativePath;

  //Use Reg to read the source and FReg will write the value.
  Reg := TRegistry.Create;
  try
    Reg.OpenKeyReadOnly (Path);

    case Reg.GetDataType (ValueName) of
      rdString       : FReg.WriteString (ValueName, Reg.ReadString (ValueName));
      rdExpandString : FReg.WriteExpandString (ValueName,
                                               Reg.ReadString (ValueName));
      rdInteger      : FReg.WriteInteger (ValueName,
                                          Reg.ReadInteger (ValueName));
      rdBinary       : begin
                         dataSize := Reg.GetDataSize (ValueName);
                         GetMem (Buffer, DataSize);
                         try
                           BytesRead := Reg.ReadBinaryData (ValueName,
                                          Buffer^, dataSize);
                           FReg.WriteBinaryData (ValueName, Buffer^, BytesRead);
                         finally
                           FreeMem (Buffer, dataSize);
                         end;
                       end;
    end;
  finally
    Reg.Free;
  end;

  if ValueExist = false then begin
    //Load the value names, as the value did not exist before.
    LoadValueNames;
    //The SelectedIndex was -1, set it to points the newly created value.
    SetSelectedIndex (FValueNames.IndexOf (ValueName));
  end
  else
    //The value name, it exist but the value has been modified. The form must
    //update itself.
    UpdateValue;
end;

//Getter for the setting path.
function TValueNamesProvider.GetSettingPath: string;
begin
  Result := FSettingPath;
end;

//This method delete the selected value.
procedure TValueNamesProvider.DeleteSelectedValue;
begin
  if FSelectedIndex = -1 then
    Exit;

  if FReg.ValueExists (FValueNames [FSelectedIndex]) then
    FReg.DeleteValue (FValueNames [FSelectedIndex]);

  //Reload the value names and notify the view.
  LoadValueNames;
end;

//This method clears the value of a value name. The value name itself will not
//bd deleted.
procedure TValueNamesProvider.ClearSelectedValue;
var
  ValueName: string;
begin
  if FSelectedIndex = -1 then
    Exit;

  if FReg.ValueExists (FValueNames [FSelectedIndex]) then begin
    ValueName := FValueNames [FSelectedIndex];

    case FReg.GetDataType (ValueName) of
      rdString,
      rdExpandString : FReg.WriteExpandString (ValueName, EmptyStr);
      rdInteger      : FReg.WriteInteger (ValueName, 0);
      rdBinary       : FReg.WriteBinaryData (ValueName, Ptr (0)^, 0);
    end;

    //The value for the selected valuename has been changed, notify the view.
    UpdateValue;
  end;
end;

end.
