{-----------------------------------------------------------------------------
 Unit Name: ShellUtilities

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:
 History:
 02/18/2005 - Fixed the problem with RunDelphi. It incorrectly added double
              quotes around the parameter. It is not necessary and caused a bug
              when the custom setting has space in it.
-----------------------------------------------------------------------------}

unit ShellUtilities;

interface

uses
  ShlObj, ActiveX, ComObj, SysUtils, Windows, ShellAPI, Forms;

//Create a shortcut at the desired location. This procedure is adapted from
//Creating shortcuts with Delphi at delphi.about.com.
//ShortcutName = the path and the file name of the shortcut
//Target = The executable name.
//Parameter = parameter for the executable.
//Description (Optional) = set the description of the link.
procedure CreateLink (const ShortcutName, Target, Parameter : string;
    const Description : string = ''); forward;

procedure RunDelphi (const DelphiExecutable, Parameter : string); forward;
procedure GotoLink (const Link: string); forward;

implementation

procedure CreateLink (const ShortcutName, Target, Parameter : string;
    const Description : string = '');
var
  IObject : IUnknown;
  ISLink  : IShellLink;
  IPFile  : IPersistFile;
begin
  IObject := CreateComObject (CLSID_ShellLink);
  ISLink  := IObject as IShellLink;
  IPFile  := IObject as IPersistFile;

  if FileExists (Target) = false then
    raise Exception.Create ('Target does not exist.');

  ISLink.SetPath (PChar (Target));
  ISLink.SetWorkingDirectory (PChar (ExtractFilePath (Target)));

  if SameText (Trim (Parameter), EmptyStr) = false then
    ISLink.SetArguments (PChar (Parameter));

  if SameText (Trim (Description), EmptyStr) = false then
    ISLink.SetDescription (PCHar (Description));

  IPFile.Save (PWideChar (WideString (ShortcutName)), false);
end;

procedure RunDelphi (const DelphiExecutable, Parameter : string);
var
  StartupInfo : TStartupInfo;
  ProcessInformation : TProcessInformation;
begin
  FillChar (StartupInfo, SizeOf (TStartupInfo), #0);
  FillChar (ProcessInformation, SizeOf (TProcessInformation), #0);
  StartupInfo.cb := SizeOf (TStartupInfo);

  if not CreateProcess (nil, PChar ('"' + DelphiExecutable + '" ' + Parameter), nil, nil,
                        false, CREATE_NEW_PROCESS_GROUP + NORMAL_PRIORITY_CLASS,
                        nil, nil, StartupInfo, ProcessInformation) then
    raise Exception.Create ('Cannot run Delphi.');
end;

procedure GotoLink (const Link: string);
begin
  if SameText (Link, EmptyStr) = false then
    ShellExecute (0, 'Open', PChar (Link), nil, nil, SW_SHOWNORMAL);
end;

end.
