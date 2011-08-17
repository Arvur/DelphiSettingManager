{-----------------------------------------------------------------------------
 Unit Name: ObserverIntf
 Author:    Erwien Saputra

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2002.

 Purpose:   Provides ISubject and IObserver interfaces.
 History:   New methods added as I have been inspired from Joanna Carter's
            MVP paper.  Now I added BeginUpdate and EndUpdate method.
-----------------------------------------------------------------------------}

unit IntfObserver;

interface

type

  IObserver = interface;

  ISubject = interface
  ['{A9447E2C-64B9-4A17-9DC8-0C96B96F4598}']
    //Property methods
    function GetEnabled : boolean;
    function GetIsUpdating : boolean;
    procedure SetEnabled (const AValue : boolean);

    //Main method to notify all the observers.
    procedure Notify; overload;
    //Method to notify all observers and tell what interface has been changed.
    //This method cannot be used with BeginUpdate - EndUpdate.
    procedure Notify (const AIntf : IInterface); overload;
    //Attach observer to the Subject
    procedure AttachObserver (const AObserver : IObserver);
    //Detach observer from the Subject
    procedure DetachObserver (const AObserver : IObserver);
    //Start the update process of the Subject
    procedure BeginUpdate;
    //End the update process of the Subjet.
    procedure EndUpdate;

    //Set this observer enabled or disabled
    property Enabled : boolean read GetEnabled write SetEnabled;
    //Flag, returning this observer is currently updating or not.
    property IsUpdating : boolean read GetIsUpdating;
  end;

  IObserver = interface
  ['{F82F3F63-CA48-4F5C-BF3E-E4F17804BF4E}']
    procedure Update (const Subject : ISubject; const AIntf : IInterface = nil);
  end;

implementation

end.

