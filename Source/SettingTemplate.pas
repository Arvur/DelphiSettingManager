{-----------------------------------------------------------------------------
 Unit Name: SettingTemplate

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   Interfaces and classes to save and restore registry template for
            Delphi Setting Manager.
            To use, an XML file with valid template must exist. The main form or
            main object must create the TemplateCollection using
            CreateTemplateCollection. The loaded template collection can be
            accessed through GetTemplateCollection.
 History:
 06/18/2005 - Initial version.
 06/25/2005 - Fixed bug with comments.  Added check for comments.  Updated code
              that writes the default value.
-----------------------------------------------------------------------------}
unit SettingTemplate;

interface

uses
  Classes, SettingCollection, XMLDoc, XMLIntf;

type
  //Interface that represent a registry key.
  IRegistryKey = interface
  ['{4D37A8EE-9A3E-42D9-9B37-C0E06A4626C7}']
    function GetDefaultValue: string;
    function GetDeleteTree: boolean;
    function GetEmptyTree: boolean;
    function GetItemCount: integer;
    function GetNames(Index: integer): string;
    function GetTreeName: string;
    function GetValues(Index: integer): string;

    procedure Assign (Source : IRegistryKey);
    procedure Merge (Source : IRegistryKey);

    property TreeName : string read GetTreeName;
    property DeleteTree : boolean read GetDeleteTree;
    property EmptyTree : boolean read GetEmptyTree;
    property ItemCount : integer read GetItemCount;
    property Names[Index : integer]: string read GetNames;
    property Values[Index : integer]: string read GetValues;
    property DefaultValue : string read GetDefaultValue;
  end;

  ISettingTemplate = interface
  ['{C3219481-A8CE-4D8D-B32F-FF8D9FA655F2}']
    function GetTemplateName : string;
    function GetRegistryKeyCount : integer;
    function GetRegistryKey(index : integer) : IRegistryKey;
    function GetIDEVersion: TIDEVersion;

    procedure LoadTemplate (const AFileName : string;
        const AIDEVersion : TIDEVersion; const ATemplateName : string);
    procedure MergeTemplate (const OtherTemplate : ISettingTemplate);

    property TemplateName : string read GetTemplateName;
    property RegistryKeyCount : integer read GetRegistryKeyCount;
    property RegistryKey[Index : integer] : IRegistryKey read GetRegistryKey;
    property IDEVersion : TIDEVersion read GetIDEVersion;// = (Delphi6, Delphi7, BDS1, BDS2, BDS3);
  end;

  //Registry key class represents all values under a certain tree. This class
  //holds information about what values under a certain tree.
  TRegistryKey = class (TInterfacedObject, IRegistryKey)
  private
    FDefaultValue : string;
    FDeleteTree : boolean;
    FEmptyTree : boolean;
    FTreeName : string;
    FValues : TStringList;

    function GetDefaultValue: string;
    function GetDeleteTree: boolean;
    function GetEmptyTree: boolean;
    function GetItemCount: integer;
    function GetNames(Index: integer): string;
    function GetTreeName: string;
    function GetValues(Index: integer): string;

    procedure Assign (Source : IRegistryKey);
    procedure Merge (Source : IRegistryKey);
  protected
    property TreeName : string read GetTreeName;
    property DeleteTree : boolean read GetDeleteTree;
    property EmptyTree : boolean read GetEmptyTree;
    property ItemCount : integer read GetItemCount;
    property Names[Index : integer]: string read GetNames;
    property Values[Index : integer]: string read GetValues;
    property DefaultValue : string read GetDefaultValue;
  public
    constructor Create; overload;
    constructor Create (const ANode : IXMLNode); overload;
    destructor Destroy; override;
    procedure LoadRegistryKey (const ANode : IXMLNode);
  end;

  //This class contains all trees and values for a specific configuration.
  TSettingTemplate = class (TInterfacedObject, ISettingTemplate)
  private
    FTemplateName: string;
    FRegistryKeys : IInterfaceList;
    FIDEVersion : TIDEVersion;
  protected
    function GetTemplateName : string;
    function GetRegistryKeyCount : integer;
    function GetRegistryKey(index : integer) : IRegistryKey;
    function GetIDEVersion: TIDEVersion;

    function GetTargetNode (const XMLDoc : IXMLDocument;
        const AIDEVersion : TIDEVersion; const ATemplateName : string): IXMLNode;

    procedure LoadTemplate (const AFileName : string;
        const AIDEVersion : TIDEVersion; const ATemplateName : string);
    procedure MergeTemplate (const OtherTemplate : ISettingTemplate);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  //ITemplateCollection provides the ability to get and access the Delphi
  //Setting Manager templates. This interface allows easy retrieval for the
  //templates.
  ITemplateCollection = interface
  ['{51278FEE-BBD5-48D1-A1E9-CA93E272CD36}']
    function TemplateCount (const AIDEVersion : TIDEVersion) : integer;
    function GetTemplate (const AIDEVersion : TIDEVersion;
        const TemplateName : string) : ISettingTemplate;
    procedure GetTemplateNames (const AIDEVersion : TIDEVersion;
        Strings : TStrings);
    procedure ApplyTemplate (Root : string;
        const ASettingTemplate : ISettingTemplate);

  end;

  TTemplateCollection = class (TInterfacedObject, ITemplateCollection)
  private
    FDelphi6,
    FDelphi7,
    FBDS1,
    FBDS2,
    FBDS3 : TStringList;
    FFileName : string;
  protected
    function GetStringList (const IDEVersion : string) : TStrings; overload;
    function GetStringList (const IDEVersion : TIDEVersion) : TStrings; overload;
    function TemplateCount (const AIDEVersion : TIDEVersion) : integer;
    function GetTemplate (const AIDEVersion : TIDEVersion;
        const TemplateName : string) : ISettingTemplate;
    procedure GetTemplateNames (const AIDEVersion : TIDEVersion;
        Strings : TStrings);
    procedure LoadTemplateNames (const FileName : string);
    procedure ApplyTemplate (Root : string;
        const ASettingTemplate : ISettingTemplate);
  public
    constructor Create (const TemplateFileName : string);
    destructor Destroy; override;
  end;

//This function and procedure allow a centralized access for the
//ITemplateCollection object.
function GetTemplateCollection : ITemplateCollection;
procedure CreateTemplateCollection (const TemplateFileName : string);

implementation

uses
  SysUtils, Registry, Dialogs;

const
  //These strings are the node in the XML file. Each string represents a Delphi
  //version.
  NODE_DELPHI6 = 'DELPHI6';
  NODE_DELPHI7 = 'DELPHI7';
  NODE_BDS1    = 'BDS1';
  NODE_BDS2    = 'BDS2';
  NODE_BDS3    = 'BDS3';
  ERR_LOAD_TEMPLATE_FAILED = 'Error loading Template file. Template is ' +
                             'not available.' + sLineBreak;

var
  TemplateCollection : ITemplateCollection;

function GetTemplateCollection : ITemplateCollection;
begin
  Result := TemplateCollection;
end;

//If the Template does not exist, or the XML is invalid, the exception will be
//supressed and the user will be notified with an error message. This is
//important. If the object is create in the main form constructor and this
//procedure failed, the application would die.
procedure CreateTemplateCollection (const TemplateFileName : string);
begin
  if FileExists (TemplateFileName) = true then
    try
      TemplateCollection := TTemplateCollection.Create (TemplateFileName);
    except
      on E : Exception do begin
        MessageDlg (ERR_LOAD_TEMPLATE_FAILED + E.Message, mtError, [mbOK], 0);
      end;
    end;
end;

{ TRegistryKey }

function TRegistryKey.GetDefaultValue: string;
begin
  Result := self.FDefaultValue;
end;

function TRegistryKey.GetDeleteTree: boolean;
begin
  Result := self.FDeleteTree;
end;

function TRegistryKey.GetEmptyTree: boolean;
begin
  Result := self.FEmptyTree;
end;

function TRegistryKey.GetTreeName: string;
begin
  Result := self.FTreeName;
end;

function TRegistryKey.GetNames(Index: integer): string;
begin
  Result := self.FValues.Names [Index];
end;

function TRegistryKey.GetItemCount: integer;
begin
  Result := self.FValues.Count;
end;

constructor TRegistryKey.Create;
begin
  inherited Create;
  self.FValues := TStringList.Create;
end;

function TRegistryKey.GetValues(Index: integer): string;
begin
  Result := self.FValues.ValueFromIndex[Index];
end;

procedure TRegistryKey.Assign(Source: IRegistryKey);
var
  Loop : integer;
begin
  if Assigned (Source) then begin
    FDefaultValue := Source.DefaultValue;
    FDeleteTree   := Source.DeleteTree;
    FEmptyTree    := Source.EmptyTree;
    FTreeName     := Source.TreeName;

    FValues.Clear;
    for Loop := 0 to Source.ItemCount - 1 do
      FValues.Add (Source.Names[Loop] + '=' + Source.Values[Loop]);
  end;
end;

procedure TRegistryKey.Merge (Source : IRegistryKey);
var
  Loop : integer;
begin
  if Assigned (Source) then begin
    if SameText (self.FTreeName, EmptyStr) then
      Assign (Source)
    else begin
      if SameText (FTreeName, Source.TreeName) = false then
        raise Exception.Create ('Cannot merge keys with different TreeName.');

      if Source.DeleteTree <> self.FDeleteTree then begin
        FDeleteTree := false;
        FEmptyTree  := true;
      end;

      if Source.EmptyTree <> self.FEmptyTree then
        self.FEmptyTree := true;

      for Loop := 0 to Source.ItemCount - 1 do
        if self.FValues.IndexOfName(Source.Names[Loop]) = -1 then
          FValues.Add (Source.Names[Loop] + '=' + Source.Values[Loop])
        else
          if SameText (FValues.Values[Source.Names[Loop]],
                       Source.Values [Loop]) = false then
            FValues.Values[Source.Names[Loop]] := Source.Values[Loop];
    end;
  end;
end;

destructor TRegistryKey.Destroy;
begin
  if Assigned (FValues) then
    FreeAndNil (FValues);

  inherited;
end;

//Load the registry key and values from ANode.  ANode must must be the Template
//Node.
procedure TRegistryKey.LoadRegistryKey(const ANode: IXMLNode);
var
  Node : IXMLNode;
  Name, Value : string;
begin
  if ANode.NodeType = ntComment then
    Exit;

  //Get the name of the node. This is the relative tree name.
  if ANode.HasAttribute ('name') then
    FTreeName := ANode.Attributes ['name'];

  //Is this tree must be deleted?
  FDeleteTree := (ANode.HasAttribute ('delete_tree') = true) and
                 (ANode.Attributes ['delete_tree'] = true);

  //Is this key must be emptied first?
  FEmptyTree := (ANode.HasAttribute ('empty_tree') = true) and
                (ANode.Attributes ['empty_tree']);

  //Iterate the name values for this key.
  if ANode.HasChildNodes = true then begin
    Node := ANode.ChildNodes.First;

    while Node <> nil do begin
      if Node.NodeType <> ntComment then begin
        Name := Node.Attributes['name'];
        Value := Node.Attributes['value'];

        if SameText (Name, 'default') then
          FDefaultValue := Value
        else
          FValues.Add (Name + '=' + Value);
      end;

      Node := Node.NextSibling;
    end;
  end;
end;

constructor TRegistryKey.Create(const ANode: IXMLNode);
begin
  inherited Create;

  self.FValues := TStringList.Create;

  LoadRegistryKey (ANode);
end;

{ TSettingTemplate }

//This method loads the text file, looks for a specific template by name for a
//specific Delphi version. The registry keys under that tempate is stored as a
//list of IRegistryKey.
procedure TSettingTemplate.LoadTemplate(const AFileName : string;
        const AIDEVersion : TIDEVersion; const ATemplateName : string);
var
  XML : IXMLDocument;
  TreeNode,
  TargetNode : IXMLNode;
  TreeLoop : integer;
  RegKey : IRegistryKey;
begin
  XML := TXMLDocument.Create (nil);
  XML.LoadFromFile (AFileName);

  //Get the Node of the Template Name.
  TargetNode := GetTargetNode(XML, AIDEVersion, ATemplateName);

  //Each children of the Template Node are an instance of IRegistryKey. Load
  //each children node to a list of IRegistryKey.
  for TreeLoop := 0 to TargetNode.ChildNodes.Count - 1 do begin
    TreeNode := TargetNode.ChildNodes.Get (TreeLoop);
    RegKey := TRegistryKey.Create (TreeNode);
    FRegistryKeys.Add (RegKey);
  end;
end;

procedure TSettingTemplate.MergeTemplate(const OtherTemplate: ISettingTemplate);
var
  Index,
  Loop : integer;
  NewKey,
  RegKey : IRegistryKey;
  Names : TStringList;
begin
  Names := TStringList.Create;
  try
    for Loop := 0 to self.FRegistryKeys.Count - 1 do begin
      RegKey := IRegistryKey (FRegistryKeys.Items[Loop]);
      Names.Add (RegKey.TreeName);
    end;

    for Loop := 0 to OtherTemplate.RegistryKeyCount - 1 do begin
      RegKey := OtherTemplate.RegistryKey [Loop];
      Index := Names.IndexOf(RegKey.TreeName);

      if (Index = -1) then begin
        NewKey := TRegistryKey.Create;
        NewKey.Assign (RegKey);
        self.FRegistryKeys.Add (NewKey);
        Names.Add (NewKey.TreeName);
      end
      else begin
        NewKey := (FRegistryKeys[Index]) as IRegistryKey;

        NewKey.Merge (RegKey);
      end;
    end;
  finally
    Names.Free;
  end;
end;

function TSettingTemplate.GetRegistryKey(index: integer): IRegistryKey;
begin
  Result := FRegistryKeys[index] as IRegistryKey;
end;

function TSettingTemplate.GetRegistryKeyCount: integer;
begin
  Result := self.FRegistryKeys.Count;
end;

function TSettingTemplate.GetTemplateName: string;
begin
  Result := self.FTemplateName;
end;

constructor TSettingTemplate.Create;
begin
  self.FRegistryKeys := TInterfaceList.Create;
end;

destructor TSettingTemplate.Destroy;
begin
  FRegistryKeys := nil;
  inherited;
end;

//Returns the root node of the template name on the IXMLDocument. In a XML file
//there can be more than one template with same name that belongs to different
//IDE version.
function TSettingTemplate.GetTargetNode(const XMLDoc : IXMLDocument;
  const AIDEVersion: TIDEVersion; const ATemplateName: string): IXMLNode;
var
  Node : IXMLNode;
  NodeName : string;
  ANode : IXMLNode;
  Loop : integer;
begin
  Result      := nil;
  Node        := XMLDoc.Node;
  FIDEVersion := AIDEVersion;

  //Get the node named ROOT.
  while SameText (Node.NodeName, 'ROOT') = false do
    Node := Node.ChildNodes.First;

  //Get the string representation of the desired IDE version and get that node.
  case AIDEVersion of
    Delphi6 : NodeName := NODE_DELPHI6;
    Delphi7 : NodeName := NODE_DELPHI7;
    BDS1    : NodeName := NODE_BDS1;
    BDS2    : NodeName := NODE_BDS2;
    BDS3    : NodeName := NODE_BDS3;
  end;

  Node := Node.ChildNodes.FindNode (NodeName);

  //Iterate through the node, stop when it finds the desired node.
  if Assigned (Node) = true then begin
    for Loop := 0 to Node.ChildNodes.Count - 1 do begin
      ANode := Node.ChildNodes.Get(Loop);

      if (ANode.NodeType = ntComment) then
        Continue;

      if (ANode.HasAttribute ('name') and
          (ANode.Attributes ['name'] = ATemplateName)) then begin
        Result := ANode;
        Break;
      end;
    end;
  end;
end;

//Returns the IDE Version of the Template.
function TSettingTemplate.GetIDEVersion: TIDEVersion;
begin
  Result := self.FIDEVersion;
end;

{ TTemplateCollection }

//Create the TemplateCollection and load the Templates.
constructor TTemplateCollection.Create (const TemplateFileName : string);
begin
  FFileName := TemplateFileName;
  //The StringList that hold the Template Name.
  FDelphi6  := TStringList.Create;
  FDelphi7  := TStringList.Create;
  FBDS1     := TStringList.Create;
  FBDS2     := TStringList.Create;
  FBDS3     := TStringList.Create;

  LoadTemplateNames(FFileName);
end;

//Load the Template names from the XML file to the string lists.
procedure TTemplateCollection.LoadTemplateNames(const FileName: string);
var
  XML : IXMLDocument;
  TemplateNode,
  Node : IXMLNode;
  List : TStrings;
begin
  XML := TXMLDocument.Create (nil);
  XML.LoadFromFile (FileName);
  Node := XML.ChildNodes.First;

  //Get the root.
  while SameText (Node.NodeName, 'ROOT') = false do
    Node := Node.ChildNodes.First;

  //Does the root has ide tmplate?
  if Node.HasChildNodes = true then begin
    //Get the IDE Version root.
    Node := Node.ChildNodes.First;

    while Node <> nil do begin
      //The IDE Version root has templates in it.
      if Node.HasChildNodes = true then begin
        List := self.GetStringList(Node.NodeName);

        if Assigned (List) = true then begin
          //Move pointer to the children node, the template names, and iterate
          //each template names.
          TemplateNode := Node.ChildNodes.First;

          while TemplateNode <> nil do begin
            if TemplateNode.NodeType <> ntComment then
              List.Add (TemplateNode.Attributes ['name']);

            TemplateNode := TemplateNode.NextSibling;
          end;
        end;
      end;

      Node := Node.NextSibling;
    end;
  end;
end;

//Returns the number of template for a Delphi IDE version.
function TTemplateCollection.TemplateCount(
    const AIDEVersion: TIDEVersion): integer;
var
  List : TStrings;
begin
  List := GetStringList (AIDEVersion);

  if Assigned (List) then
    Result := List.Count
  else
    Result := 0;
end;

//Returns all templates for a specific IDE.
procedure TTemplateCollection.GetTemplateNames(const AIDEVersion : TIDEVersion;
    Strings: TStrings);
var
  List : TStrings;
begin
  Strings.Clear;
  List := GetStringList (AIDEVersion);

  if Assigned (List) then
    Strings.Assign (List);
end;

destructor TTemplateCollection.Destroy;
begin
  FDelphi6.Free;
  FDelphi7.Free;
  FBDS1.Free;
  FBDS2.Free;
  FBDS3.Free;

  inherited;
end;

//Given a template name and an IDE version, this method returns an instance of
//ISettingTemplate.
function TTemplateCollection.GetTemplate(const AIDEVersion: TIDEVersion;
  const TemplateName: string): ISettingTemplate;
begin
  Result := TSettingTemplate.Create;
  Result.LoadTemplate(self.FFileName, AIDEVersion, TemplateName);
end;

//A helper function, returns the TStrings that is associated with the expected
//IDEVersion in string.
function TTemplateCollection.GetStringList(const IDEVersion: string): TStrings;
begin
  if SameText (IDEVersion, NODE_DELPHI6) then
    Result := FDelphi6
  else if SameText (IDEVersion, NODE_DELPHI7) then
    Result := FDelphi7
  else if SameText (IDEVersion, NODE_BDS1) then
    Result := FBDS1
  else if SameText (IDEVersion, NODE_BDS2) then
    Result := FBDS2
  else if SameText (IDEVersion, NODE_BDS3) then
    Result := FBDS3
  else
    Result := nil;
end;

function TTemplateCollection.GetStringList(
  const IDEVersion: TIDEVersion): TStrings;
begin
  case IDEVersion of
    Delphi6 : Result := FDelphi6;
    Delphi7 : Result := FDelphi7;
    BDS1    : Result := FBDS1;
    BDS2    : Result := FBDS2;
    BDS3    : Result := FBDS3;
  else
    Result := nil;
  end;
end;

//Apply the Template to a registry key (Root). If Root is a valid Delphi Setting
//Manager key, this will update that registry key to match the rules in the
//Template.  Root must be an absolute registry key under HKEY_CURRENT_USER.
procedure TTemplateCollection.ApplyTemplate(Root: string;
  const ASettingTemplate: ISettingTemplate);
var
  Reg : TRegistry;
  RegKeyLoop,
  ValueLoop : integer;
  RegKey : IRegistryKey;
begin
  //Make sure that Root has backslash.
  if (Root[1] <> '\') then
    Root := '\' + Root;

  Reg := TRegistry.Create;
  try
    if Reg.OpenKey(Root, false) = false then
      raise Exception.CreateFmt ('%s does not exist.', [Root]);

    //Loop through the registry key in the template.
    for RegKeyLoop := 0 to ASettingTemplate.RegistryKeyCount - 1 do begin
      //Create the key
      Reg.OpenKey (Root, false);
      RegKey := ASettingTemplate.RegistryKey[RegKeyLoop];

      //Delete the tree.
      if RegKey.DeleteTree = true then
        Reg.DeleteKey (RegKey.TreeName)
      else begin
        //Empty the tree, if the tree exist.
        if (RegKey.EmptyTree = true) and
           (Reg.KeyExists (RegKey.TreeName) = true) then begin
          Reg.DeleteKey (RegKey.TreeName);
          Reg.CreateKey (RegKey.TreeName);
        end;

        //Recreate the key.
        Reg.OpenKey (RegKey.TreeName, true);

        //Write the default value.
        if SameText (RegKey.DefaultValue, EmptyStr) = false then
          Reg.WriteString('', RegKey.DefaultValue);

        //Write each values.
        for ValueLoop := 0 to RegKey.ItemCount - 1 do
          Reg.WriteString (RegKey.Names[ValueLoop], RegKey.Values[ValueLoop]);
      end;
    end;
  finally
    Reg.Free;
  end;
end;

end.
