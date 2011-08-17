{-----------------------------------------------------------------------------
 Unit Name: ValueNamesProvider

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   Manages the complex interaction between IDelphiSettingRegistry and
            TTreeView.

 History:
 01/19/05 - Initial creation.
 02/20/05 - Fixed a bug when a key was missing on one treeview and exist at
            another, and that node is expanded, the treeview with missing key
            will expand the root node.
-----------------------------------------------------------------------------}
unit TreeViewController;

interface

uses
  ComCtrls, DelphiSettingRegistry, SysUtils, Classes;

type
  //Manages the interaction between the TTreeView and the IDelphiSettingRegistry
  //The class that implements thi interface handles the interaction from the
  //IDelphiSettingRegistry to the TreeView.
  ITreeViewController = interface
  ['{A6523793-38FB-4298-BF15-323D884055BA}']
    //Property getters/
    function GetDelphiSettingRegistry : IDelphiSettingRegistry;
    function GetTreeView : TTreeview;
    //Given a full path, returns a node.
    function FindNode(const FullPath: string; out Node: TTreeNode): boolean;

    //Bind the TTreeView and Bind the IDelphiSettingRegistry.
    procedure BindTreeView (const TreeView : TTreeview);
    procedure BindDelphiSettingRegistry (const Intf : IDelphiSettingRegistry);
    //Build the nodes in the TTreeView. If Node is nil, build it from the root.
    //otherwise, clear the Node and build all nodes underneath.
    procedure BuildNode (Node : TTreeNode);

    property TreeView : TTreeView read GetTreeView;
    property DelphiSettingRegistry : IDelphiSettingRegistry
        read GetDelphiSettingRegistry;
  end;

  //Synchronizes the selected key for both IDelphiSettingRegistry.
  //The object that implements this interface handles the interaction from the
  //treeview to IDelphiSettingRegistry.
  //This interface was refactored out from ITreeViewController.
  //Delphi Setting Manager have two IDelphiSettingManager, each is represented
  //in TreeView, selecting an item on the TreeView will change the
  //IDelphiSettingRegistry. There is a need to synchronize the selection of both
  //IDelphiSettingRegistry/TreeView. This class is also handles synchronizing
  //the IDelphiSettingRegistry.
  IDelphiSettingSynchronizer = interface
  ['{A2A45B82-E38C-451E-BF56-BEFD8CC38EEE}']
    procedure AddTreeViewController (const Controller : ITreeViewController);
    procedure ReleaseDelphiSettingKey (const Intf : IDelphiSettingRegistry);
    procedure ReleaseTreeView (const TreeView : TTreeView);
  end;

//Factory functions.
function CreateTreeViewController : ITreeViewController; forward;
function GetDelphiSettingSynchronizer : IDelphiSettingSynchronizer; forward;

implementation

uses
  Contnrs, IntfObserver;

type
  //Implements ITreeViewController
  TTreeViewController = class (TInterfacedObject, ITreeViewController, IObserver)
  private
    FTreeView : TTreeView;
    FDelphiSettingRegistry : IDelphiSettingRegistry;

    procedure Update (const Subject : ISubject; const AIntf : IInterface = nil);
    procedure GetNodesFromPath (Path : string; out Nodes : TStringArray);
  protected
    function GetTreeView : TTreeview;
    function GetDelphiSettingRegistry : IDelphiSettingRegistry;
    function FindNode(const FullPath: string; out Node: TTreeNode): boolean;

    procedure BindTreeView (const TreeView : TTreeview);
    procedure BindDelphiSettingRegistry (const Intf : IDelphiSettingRegistry);
    procedure BuildNode (Node : TTreeNode);

  public
  end;

  TDelphiSettingSynchronizer = class (TInterfacedObject, IDelphiSettingSynchronizer)
  private
    FList : TInterfaceList;
    FUpdating : boolean;

    function IndexOfTreeView (const TreeView : TTreeView): integer;

    procedure PropagateChange (TreeView: TTreeView; Path : string;
      const Selected, Expanded : boolean);
    procedure NodeChanged (Sender: TObject; Node: TTreeNode);
    procedure ReleaseControl (const Index : integer);
  protected
    procedure ReleaseDelphiSettingKey (const Intf : IDelphiSettingRegistry);
    procedure ReleaseTreeView (const TreeView : TTreeView);
    procedure AddTreeViewController (const Controller : ITreeViewController);
  public
    constructor Create;
    destructor Destroy; override;
  end;

function CreateTreeViewController : ITreeViewController;
begin
  Result := TTreeViewController.Create;
end;

function GetDelphiSettingSynchronizer : IDelphiSettingSynchronizer;
begin
  Result := TDelphiSettingSynchronizer.Create;
end;

//Given a node, get the full path of that node.
function GetPathFromNode (Node : TTreeNode) : string;
begin
  Result := '';

  while Node <> nil do begin
    Result := Node.Text + '\' + Result;
    Node := Node.Parent;
  end;
end;

//Get the text of the root node. The root node has the setting path in it.
function RootText (Node : TTreeNode) : string;
begin
  while Node.Parent <> nil do
    Node := Node.Parent;

  Result := Node.Text;
end;

//Get the relative path, it is the path without the root.
function GetRelativePath (Node : TTreeNode) : string;
begin
  Result := '';

  while Node.Parent <> nil do begin
    Result := Node.Text + '\' + Result;
    Node := Node.Parent;
  end;

  if SameText (Result, EmptyStr) = false then
    Result := '\' + Result;

end;

{ TTreeViewController }

//Given a path, parses it to a string of array. Each node is a key. The first
//item is the setting path, the second items to the last items are the nodes
//after the setting path (or relative path).
procedure TTreeViewController.GetNodesFromPath (Path : string;
  out Nodes : TStringArray);
var
  ArrayLength, Delimiter, Index : integer;
begin
  //Firewall if the path is empty string.
  if SameText (Path, EmptyStr) then begin
    SetLength (Nodes, 0);
    Exit;
  end;

  //Set the array length for starter. Make it large enough for most operation.
  ArrayLength := 32;
  SetLength (Nodes, ArrayLength);
  Index := 0;

  if Path[1] <> '\' then
    Path := '\' + Path;

  //Get the setting path as the root node.
  if Pos (FDelphiSettingRegistry.SettingPath, Path) = 1 then
    Path := Copy (Path, Length (FDelphiSettingRegistry.SettingPath) + 2,
                  Length (Path))
  else
    Path := Copy (Path, 2, Length (Path));

  //Iterates the rest of the path and parses it into array.
  while Path <> '' do begin
    Delimiter := GetFirstDelimiterIndex (Path);
    Nodes[Index] := Copy (Path, 1, Delimiter - 1);
    Inc (Index);

    if Delimiter = 0 then
      Path := ''
    else
      Path := Copy (Path, Delimiter + 1, Length (Path));

    //Resize the array if it is not large enough.
    if (Index = ArrayLength) and (Path <> '') then begin
      ArrayLength := ArrayLength * 2;
      SetLength (Nodes, ArrayLength);
    end;
  end;

  //Resize the array to the correct size.
  SetLength (Nodes, Index);
end;

//Bind the IDelphiSettingRegistry to this TreeViewController.
procedure TTreeViewController.BindDelphiSettingRegistry(
  const Intf: IDelphiSettingRegistry);
begin
  if Intf = FDelphiSettingRegistry then
    Exit;

  if Assigned (FDelphiSettingRegistry) then
    (FDelphiSettingRegistry as ISubject).DetachObserver (self);

  FDelphiSettingRegistry := Intf;

  if Assigned (FDelphiSettingRegistry) then
    (FDelphiSettingRegistry as ISubject).AttachObserver (self);
end;

//The IObserver method. It is get called by the Subject. When Subject current
//key is modified, this controller is notified. This method will select the
//TreeView node that correspond with the selected key.
procedure TTreeViewController.Update(const Subject: ISubject;
  const AIntf: IInterface);
var
  Node : TTreeNode;
  Intf : IDelphiSettingRegistry;
begin
  Intf := Subject as IDelphiSettingRegistry;

  //If the Node is not found, build the node.
  if FindNode (Intf.CurrentPath, Node) = false then begin
    BuildNode(Node);
    //After the node is rebuild, find it again.
    FindNode (Intf.CurrentPath, Node);
  end;

  if Assigned (Node) then
    FTreeView.Select (Node);

end;

//Given a full path, return the corresponding node on the treeview. If the node
//is not found, it returns false and the out parameter Node will be the closest
//node to the desired node.
//If the node is found, Node will be desired node and returns true.
function TTreeViewController.FindNode(const FullPath: string;
  out Node: TTreeNode): boolean;
var
  Nodes : TStringArray;
  Loop : integer;
  TempNode : TTreeNode;
begin
  Result := true;
  //Parses the path to array of string. Each element of the array is a registry
  //key in hiearchy order.
  GetNodesfromPath (FullPath, Nodes);

  FTreeView.Items.BeginUpdate;
  try
    FTreeView.Selected := nil;
    //This is the root.  It is always valid. The first path is the root, the
    //setting path.
    Node := FTreeView.Items.GetFirstNode;

    //Firewall if the TreeView does not have any node yet.
    if Node = nil then
      Exit;

    //Iterate through the array and try to find the target node.
    for Loop := Low (Nodes)  to High (Nodes) do begin
      //Iterate the children nodes and see if it matches the desired key.
      TempNode := Node.getFirstChild;

      while (Assigned (TempNode)) and
            (SameText (TempNode.Text, Nodes [Loop]) = false) do
        TempNode := TempNode.GetNextSibling;

      //If the desired key is not found, break. Node will be closest node to the
      //desired node.
      if (Assigned (TempNode) = false) or
         (SameText (TempNode.Text, Nodes [Loop]) = false) then begin
        Result := false;
        Break;
      end;

      //Set the Node with TempNode. TempNode is a valid node. Move on to the
      //children of this node.
      Node := TempNode;
    end;
  finally
    FTreeView.Items.EndUpdate;
  end;
end;

//Bind a TreeView to this class.
procedure TTreeViewController.BindTreeView(const TreeView: TTreeview);
begin
  if FTreeView = TreeView then
    Exit;

  FTreeView := TreeView;
end;

//Build Node builds TreeView with keys. It reads the keys from
//IDelphiSettingRegistry. If the Node parameter is nil, this method builds
//everything from the root. If Node is not nil, it will clears all nodes under
//that Node, and rebuild it.
procedure TTreeViewController.BuildNode(Node: TTreeNode);
var
  ChildKey : string;
  Path : string;
  List : TStringList;
  Children : TStringArray;
begin
  //Get the node and the full path to start. Path is the full path for each key.
  if Node = nil then begin
    Path := FDelphiSettingRegistry.SettingPath;
    Node := FTreeView.Items.AddChild (nil, Path);
  end
  else begin
    Path := GetPathFromNode (Node);
    Node.DeleteChildren;
  end;

  //List is a queue. It contains full path and the corresponding node on the
  //treeview. When processing each node, this method reads all sub keys using
  //the full path information and creates the nodes under the processed node.
  List := TStringList.Create;

  try
    //This is the beginning, insert the full path and the start node.
    List.AddObject (Path, Node);

    while List.Count > 0 do begin
      //Pop an item from the queue and process it.
      Path := List [0];
      Node := List.Objects[0] as TTreeNode;
      List.Delete (0);

      FDelphiSettingRegistry.GetChild(Path, Children);

      //Creates the nodes under the processed node and at the same time add
      //child node and the corresponding path to the queue to be processed.
      for ChildKey in Children do
        List.AddObject (Path + '\' + ChildKey,
                        FTreeView.Items.AddChild (Node, ChildKey));
    end;
  finally
    List.Free;
  end;
end;

function TTreeViewController.GetTreeView: TTreeview;
begin
  Result := self.FTreeView;
end;

function TTreeViewController.GetDelphiSettingRegistry: IDelphiSettingRegistry;
begin
  Result := FDelphiSettingRegistry;
end;


{ TDelphiSettingSynchronizer }

constructor TDelphiSettingSynchronizer.Create;
begin
  inherited;
  FList := TInterfaceList.Create;
end;

destructor TDelphiSettingSynchronizer.Destroy;
var
  Loop : integer;
begin
  for Loop := FList.Count - 1 downto 0 do
    ReleaseTreeView ((FList.Items[Loop] as ITreeViewController).TreeView);

  inherited;
end;

//This method is an event handler, executed every time the node in the treeview
//is expanded, collapsed, or selected.
procedure TDelphiSettingSynchronizer.NodeChanged(Sender: TObject;
  Node: TTreeNode);
var
  Path : string;
begin
  //Node can be nil if the Selected is set to nil, when the nodes are rebuild.
  //When the user add a key to the treeview, series of event handler, OnExpanded
  //and OnChanged are called. OnExpanded will be called with nil as Node.
  //FUpdating is a flag that prevents the event handler from being trapped into
  //an infinite loop. When the user change selection on the tree view, the
  //synchronizing code will trigger OnChanged event multiple times.
  if (FUpdating = true) or
     (Node = nil) then
    Exit;

  FUpdating := true;
  try
    Path := GetRelativePath (Node);

    //Tells all registered TreeViewController to update itself, using the
    //relative path information. Each IDelphiSettingRegistry in the system has
    //different setting path, so the relative path must be used instead of full
    //path.
    PropagateChange (Sender as TTreeView, Path, Node.Selected, Node.Expanded);
  finally
    FUpdating := false;
  end;
end;

procedure TDelphiSettingSynchronizer.ReleaseDelphiSettingKey(
  const Intf: IDelphiSettingRegistry);
var
  Loop : integer;
begin
  for Loop := 0 to FList.Count - 1 do
    if (FList.Items[Loop] as ITreeViewController).DelphiSettingRegistry = Intf then begin
      ReleaseControl (Loop);
      Break;
    end;
end;

procedure TDelphiSettingSynchronizer.ReleaseTreeView(const TreeView: TTreeView);
var
  Index : integer;
begin
  Index := IndexOfTreeView(TreeView);
  ReleaseControl (Index);
end;

//The core method in this class. This method updates all registered
//TreeViewController. It updates the
procedure TDelphiSettingSynchronizer.PropagateChange(TreeView : TTreeView;
  Path: string; const Selected, Expanded : boolean);
var
  Loop : integer;
  LocView : TTreeView;
  TopNode,
  Node : TTreeNode;
  FullTopNodePath,
  TopNodePath : string;
  TreeViewController : ITreeViewController;
begin
  if TreeView.TopItem <> nil then
    TopNodePath := GetRelativePath (TreeView.TopItem);

  for Loop := 0 to FList.Count - 1 do begin
    TreeViewController := FList.Items[Loop] as ITreeViewController;
    LocView := TreeViewController.TreeView;

    FullTopNodePath := TreeViewController.DelphiSettingRegistry.SettingPath +
                       TopNodePath;

    //Set the top node first.  If the SetCurrentPath is set first, setting the
    //TopNode may cause the TopNode to push the Selected node out of the
    //TreeView. The TreeView that initiates the change does not need update.
    if (LocView <> TreeView) then begin
      if TreeViewController.FindNode (FullTopNodePath, TopNode) then
        TreeViewController.TreeView.TopItem := TopNode;
    end;

    //Make sure the current setting path in the registry is up to date. This
    //call will set the DelphiSettingRegistry and TreeViewController will be
    //notified about the change and set the TreeView selection to reflect the
    //current selection of the corresponding DelphiSettingRegistry. Selecting a
    //node will trigger OnChange, but the FUpdating is currently true, it will
    //not trigger this method again.
    TreeViewController.DelphiSettingRegistry.SetCurrentPath (
          TreeViewController.DelphiSettingRegistry.SettingPath +
          Path);

    if LocView = TreeView then
      Continue;

    //Get the selected node, if the desired node does not exist in the target
    //treeview, continue.
    Node := LocView.Selected;

    if Assigned (Node) = false then
      Continue;

    //Check if the relative path of the node is equal with the path of the
    //treeview that initiates the change. If it is, synchronize the expanded or
    //collapsed state.
    if SameText (GetRelativePath (Node), Path) = true then begin
      if Expanded then
        Node.Expand(false)
      else
        Node.Collapse(false);
    end;

  end;
end;

//Add a treeview controller. Set the TreeView OnChange, OnCollapsed, and
//OnExpanded event handler.
procedure TDelphiSettingSynchronizer.AddTreeViewController(
  const Controller : ITreeViewController);
begin

  Controller.TreeView.OnChange    := NodeChanged;
  Controller.TreeView.OnCollapsed := NodeChanged;
  Controller.TreeView.OnExpanded  := NodeChanged;

  FList.Add (Controller);
end;

function TDelphiSettingSynchronizer.IndexOfTreeView(
  const TreeView: TTreeView): integer;
var
  Loop : integer;
begin
  Result := -1;

  for Loop := 0 to FList.Count - 1 do
    if (FList.Items[Loop] as ITreeViewController).TreeView = TreeView then begin
       Result := Loop;
       Break;
    end;
end;

procedure TDelphiSettingSynchronizer.ReleaseControl(const Index: integer);
var
  TreeViewController : ITreeViewController;
begin
  TreeViewController := FList.Items[Index] as ITreeViewController;

  TreeViewController.TreeView.OnChange    := nil;
  TreeViewController.TreeView.OnCollapsed := nil;
  TreeViewController.TreeView.OnExpanded  := nil;

  FList.Delete (Index);
end;

end.
