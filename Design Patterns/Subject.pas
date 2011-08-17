{-----------------------------------------------------------------------------
 Unit Name: Subject
 Author:    Erwien Saputra

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2002.

 Purpose:   Implements ISubjet, to be used as aggregated object, being exposed
            via property (delegated interface).
 History:
-----------------------------------------------------------------------------}


unit Subject;

interface
uses
  IntfObserver, Classes;

type
  //Class reference for TSubject
  TSubjectClass = class of TSubject;

  //Implements
  TSubject = class (TAggregatedObject, ISubject)
  private
    FUpdateCount  : integer;
    FObserverList : IInterfaceList;
    FEnabled      : boolean;

    //IObserver
    function GetEnabled : boolean;
    function GetIsUpdating : boolean;
    procedure SetEnabled (const AValue : boolean);
    procedure Notify; overload;
    procedure Notify (const AIntf : IInterface); overload;
    procedure AttachObserver (const AObserver : IObserver);
    procedure DetachObserver (const AObserver : IObserver);
    procedure BeginUpdate;
    procedure EndUpdate;
  protected
  public
    constructor Create (Controller : IInterface); reintroduce; virtual;
  end;

implementation

uses
  SysUtils;

{ TSubject }

{-----------------------------------------------------------------------------
  Procedure: TSubject.AttachObserver
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   Add the observer to the internal observer list.  Raise exception
             if AObserver is not assigned.  No duplicate is allowed.
  Arguments: AObserver: IObserver
  Result:    None
  Notes:
-----------------------------------------------------------------------------}
procedure TSubject.AttachObserver (const AObserver: IObserver);
begin
  if (Assigned (AObserver) = false) then
    raise Exception.Create ('Observer cannot be nil.');

  if (FObserverList.IndexOf (AObserver) < 0) then begin
    FObserverList.Add (AObserver);
    Notify;
  end;
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.BeginUpdate
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   Increase the FUpdateCount private counter.
  Arguments: None
  Result:    None
  Notes:
-----------------------------------------------------------------------------}
procedure TSubject.BeginUpdate;
begin
  if (FEnabled = true) then
    Inc (FUpdateCount);
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.Create
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   AggregatedObject constructor.  It calls TAggregatedObject's
             constructor.  Create IInterfaceList and initialize FUpdateCount.
  Arguments: Controller: IInterface
  Result:    None
  Notes:     TSubject must be implemented as aggregated interfaced object.
-----------------------------------------------------------------------------}
constructor TSubject.Create(Controller: IInterface);
begin
  inherited Create (Controller);

  FUpdateCount  := 0;
  FEnabled      := true;
  FObserverList := TInterfaceList.Create;
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.DetachObserver
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:
  Arguments: AObserver: IObserver
  Result:    None
  Notes:
-----------------------------------------------------------------------------}
procedure TSubject.DetachObserver(const AObserver: IObserver);
begin
  FObserverList.Remove (AObserver);
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.EndUpdate
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   Decrease the UpdateCount counter and if it reaches 0, call Notify.
             But if the FUpdateCount is already 0, don't call notify.
  Arguments: None
  Result:    None
  Notes:     Thanks to Joanna Carter from delphi OOD newsgroup, that informing
             me this BeginUpdate/EndUpdate technique.
-----------------------------------------------------------------------------}
procedure TSubject.EndUpdate;
begin
  if (FEnabled = false) or (FUpdateCount = 0) then
    Exit;

  Dec (FUpdateCount);

  if (FUpdateCount = 0) then
    Notify;
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.GetEnabled
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   This returns if this Subject is enabled.
  Arguments: None
  Result:    boolean
  Notes:
-----------------------------------------------------------------------------}
function TSubject.GetEnabled: boolean;
begin
  Result := FEnabled;
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.Notify
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   Notify all observers that subject has changed and probably they
             want to update themselves.
  Arguments: None
  Result:    None
  Notes:
-----------------------------------------------------------------------------}
procedure TSubject.Notify;
var
  iLoop : integer;
begin
  if (FEnabled = true) then
    for iLoop := 0 to (FObserverList.Count - 1) do
      IObserver (FObserverList.Items [iLoop]).Update (Controller as ISubject);
end;

//Notify all observers about change and notify what interface has been changed.
procedure TSubject.Notify (const AIntf : IInterface);
var
  iLoop : integer;
begin
  if (FEnabled = true) then
    for iLoop := 0 to (FObserverList.Count - 1) do
      IObserver (FObserverList.Items [iLoop]).Update (Controller as ISubject,
                 AIntf);
end;

function TSubject.GetIsUpdating: boolean;
begin
  Result := (FUpdateCount > 0);
end;

{-----------------------------------------------------------------------------
  Procedure: TSubject.SetEnabled
  Author:    Erwien Saputra
  Date:      19-Aug-2002
  Purpose:   Set this subject enabled or not.  Subject cannot be disabled
             in the middle of update (FUpdateCount <> 0);
  Arguments: const AValue: boolean
  Result:    None
  Notes:
-----------------------------------------------------------------------------}
procedure TSubject.SetEnabled(const AValue: boolean);
begin
  if (GetIsUpdating = true) then
    raise Exception.Create ('Subject is updating.');

  if (AValue <> FEnabled) then begin
    FEnabled := AValue;
    Notify;
  end;
end;

end.
