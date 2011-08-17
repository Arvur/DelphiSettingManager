unit DelphiSettingEditor;

interface

uses
  Classes, Registry, SysUtils;

type
  TStringArray = array of string;

  IDelphiSettingKeys = interface
  ['{B910A6AD-9BEB-4540-A17B-4D099E7B7AE9}']
    function GetSettingPath : string;
    function GetCurrentPath : string;
    function GetOnCurrentKeyChanged : TNotifyEvent;
    procedure SetOnCurrentKeyChanged (const Value : TNotifyEvent);
    procedure SetCurrentPath (const APath : string);

    function GetParent (const APath : string) : string;
    procedure GetChild (const APath : string; out StringArray : TStringArray);

    procedure OpenSetting (const SettingPath : string);
    procedure RefreshCurrentKey (const Recursive : boolean = false);

    function DeleteCurrentKey : string;
    procedure AddKey (const APath : string);

    property SettingPath : string read GetSettingPath;
    property CurrentPath : string read GetcurrentPath write SetCurrentPath;
    property OnCurrentKeyChanged : TNotifyEvent read GetOnCurrentKeyChanged
        write SetOnCurrentKeyChanged;
  end;

  TDelphiSettingKeys = class (TInterfacedObject, IDelphiSettingKeys)
  private
    FSettingPath : string;
    FNodes : TStringList;
    FReg : TRegistry;
    FCurrentIndex : integer;
    FOnCurrentKeyChanged : TNotifyEvent;

    procedure BuildNodes;
  protected
    function GetNodeCount : integer;
    function GetSettingPath : string;
    function GetCurrentPath : string;
    function GetOnCurrentKeyChanged : TNotifyEvent;
    procedure SetOnCurrentKeyChanged (const Value : TNotifyEvent);
    procedure SetCurrentPath (const APath : string);

    function GetParent (const APath : string) : string;
    procedure GetChild (const APath : string; out StringArray : TStringArray);

    procedure OpenSetting (const SettingPath : string);
    procedure RefreshCurrentKey (const Recursive : boolean = false);

    function DeleteCurrentKey : string;
    procedure AddKey (const APath : string);

  public
    constructor Create (const APath : string);
    destructor Destroy; override;
  end;

implementation

uses
  Contnrs;

const
  NO_CURRENT = -2;
  ROOT       = -1;

function GetLastDelimiterIndex (const AValue : string): integer;
const
  DELIMITER = '\';
begin
  Result := LastDelimiter (DELIMITER, AValue);

  if Result > 1 then
    if AValue [Result - 1] = DELIMITER then
      Result := GetLastDelimiterIndex (Copy (AValue, 1, Result - 2));

end;

function NameValueCompare(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText (List.Names[Index1], List.Names[Index2]);

  if Result = 0 then
    Result := AnsiCompareText (List.ValueFromIndex[Index1],
                               List.ValueFromIndex[Index2]);
end;

{ TDelphiSettingKeys }

constructor TDelphiSettingKeys.Create(const APath: string);
begin
  inherited Create;
  FNodes        := TStringList.Create;
  FReg          := TRegistry.Create;
  FCurrentIndex := NO_CURRENT;
  OpenSetting (APath);
end;

destructor TDelphiSettingKeys.Destroy;
begin
  if Assigned (FNodes) then
    FNodes.Free;

  inherited;
end;

function TDelphiSettingKeys.GetNodeCount: integer;
begin
  Result := FNodes.Count;

  if SameText (FSettingPath, EmptyStr) = false then
    Inc (Result);
end;

procedure TDelphiSettingKeys.SetOnCurrentKeyChanged(
  const Value: TNotifyEvent);
begin
  FOnCurrentKeyChanged := Value;
end;

function TDelphiSettingKeys.GetSettingPath: string;
begin
  Result := FSettingPath;
end;

function TDelphiSettingKeys.GetOnCurrentKeyChanged: TNotifyEvent;
begin
  Result := self.FOnCurrentKeyChanged;
end;

procedure TDelphiSettingKeys.OpenSetting(const SettingPath: string);
begin
  FSettingPath := ExcludeTrailingBackslash (SettingPath);

  //It is important to include backslash at the beginning. The path is always a
  //full path under HKCU. Without backslash in the beginning, Regedit.OpenKey
  //may try to open a sub key under the current key.
  if SameText (FSettingPath[1], '\') = false then
    FSettingPath := '\' + FSettingPath;

  if FReg.OpenKey(FSettingPath, false) = false then
    raise Exception.Create (FSettingPath + ' does not exist.');

  if Assigned (FOnCurrentKeyChanged) then
    FOnCurrentKeyChanged (self);
end;

procedure TDelphiSettingKeys.RefreshCurrentKey (
    const Recursive : boolean = false);
const
  NODE_FORMAT  = '%s\%s';
var
  KeyQueue,
  SubKeyList : TStringList;
  KeyToProcess : string;
  Loop : integer;
begin
  KeyQueue   := TStringList.Create;
  SubKeyList := TStringList.Create;
  FNodes.BeginUpdate;
  try
    Loop := self.FCurrentIndex + 1;

    while Pos (GetCurrentPath, FNodes [Loop]) = 1 do
      FNodes.Delete (Loop);

    KeyQueue.Add (GetCurrentPath);

    repeat
      KeyToProcess := KeyQueue[0];
      KeyQueue.Delete (0);

      if FReg.OpenKeyReadOnly (KeyToProcess) = false then
        raise Exception.Create ('Failed opening ' + KeyToProcess);

      FReg.GetKeyNames (SubKeyList);

      for Loop := 0 to SubKeyList.Count - 1 do begin
        KeyQueue.Add (Format (NODE_FORMAT, [KeyToProcess, SubKeyList[Loop]]));
        FNodes.Add (Format (NODE_FORMAT, [KeyToProcess, SubKeyList[Loop]]));
      end

    until (KeyQueue.Count = 0) or (Recursive = false);

  finally
    FNodes.EndUpdate;
    KeyQueue.Free;
    SubKeyList.Free;
  end;
end;

procedure TDelphiSettingKeys.BuildNodes;
begin
end;

function TDelphiSettingKeys.GetCurrentPath: string;
begin
  Result := '\' + self.FReg.CurrentPath;
end;

procedure TDelphiSettingKeys.SetCurrentPath(const APath: string);
var
  NodeIndex,
  LastDelimiterIndex : integer;
  str : string;
begin
  if FReg.OpenKey (APath, false) = false then
    raise Exception.Create (APath + ' not found.');

  if Assigned (self.FOnCurrentKeyChanged) then
    FOncurrentKeyChanged (self);

end;

procedure TDelphiSettingKeys.AddKey(const APath: string);
var
  CorrectedPath : string;
begin
  CorrectedPath := ExcludeTrailingBackslash (APath);

  if CorrectedPath[1] <> '\' then
    CorrectedPath := '\' + CorrectedPath;

  if FReg.KeyExists (CorrectedPath) = false then
    if FReg.CreateKey (CorrectedPath) = false then
      raise Exception.Create('Cannot Create ' + CorrectedPath);

  FReg.OpenKey (CorrectedPath, false);

  if Assigned (self.FOnCurrentKeyChanged) then
    FOncurrentKeyChanged (self);
end;

function TDelphiSettingKeys.DeleteCurrentKey : string;
var
  ParentPath : string;
  IsRootPath : boolean;
begin
  IsRootPath := SameText (GetCurrentPath, FSettingPath);

  if FReg.DeleteKey (GetCurrentPath) = false then
    raise Exception.Create ('Failed to delete ' + GetCurrentPath);

  ParentPath := self.GetParent (GetCurrentPath);
  self.SetCurrentPath (ParentPath);
  Result := self.GetCurrentPath;
end;

procedure TDelphiSettingKeys.GetChild(const APath: string;
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
    List.Free;
  end;

end;

function TDelphiSettingKeys.GetParent(const APath: string): string;
begin
  Result := Copy (APath, 1, GetLastDelimiterIndex (APath));
end;

end.

procedure TForm1.Button1Click(Sender: TObject);
var
  DSK : DelphiSettingEditor.IDelphiSettingKeys;
  Childs : TStringArray;
  List : TStringList;
  ParentNode : TTreeNode;
  ChildStr,
  str : string;
begin
  DSK := TDelphiSettingKeys.Create('Software\Borland\CustomSettings\BDS3\Barebone\3.0\');
  List := TStringList.Create;
  try
    ParentNode := self.TreeView1.Items.AddChild (nil, DSK.CurrentPath);
    List.AddObject(DSK.CurrentPath, ParentNode);

    while List.Count > 0 do begin
      str := List [0];
      ParentNode := List.Objects[0] as TTreeNode;
      List.Delete (0);
      DSK.GetChild (str, Childs);

      for ChildStr in Childs do begin
        List.AddObject (str + '\' + ChildStr,
                        TreeView1.Items.AddChild (ParentNode, ChildStr));
      end;
    end;
  finally
    List.Free;
  end;
end;

