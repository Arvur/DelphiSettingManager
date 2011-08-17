{-----------------------------------------------------------------------------
 Unit Name: fRenameSetting

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   UI to rename or copy a setting.  This class has one entry point,
            returns true if the user assigned new name and clicked OK.
 History:
-----------------------------------------------------------------------------}

unit fRenameSetting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TfrmRenameSetting = class(TForm)
    lblOldName: TLabeledEdit;
    lblNewName: TLabeledEdit;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure lblNewNameKeyPress(Sender: TObject; var Key: Char);
    procedure lblNewNameChange(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
    class function Execute (const OldName : string; var NewName : string;
                            const Rename : boolean = true) : boolean;
  end;

var
  frmRenameSetting: TfrmRenameSetting;

implementation

{$R *.dfm}
const
  RENAME_CAPTION = 'Rename Setting';
  COPY_CAPTION   = 'Copy Setting';

{ TfrmRename }

//Entry point for this class. This class facilitates renaming an existing
//setting or copying a setting.
class function TfrmRenameSetting.Execute(const OldName: string;
  var NewName: string; const Rename: boolean = true): boolean;
var
  frm : TfrmRenameSetting;
begin
  frm := TfrmRenameSetting.Create (nil);

  try
    if Rename = true then
      frm.Caption := RENAME_CAPTION
    else
      frm.Caption := COPY_CAPTION;

    frm.lblOldName.Text := OldName;

    if (Trim (NewName) = EmptyStr) then
      frm.lblNewName.Text := EmptyStr
    else
      frm.lblNewName.Text := NewName;

    Result := (frm.ShowModal = mrOK) and
              (frm.lblNewName.Text <> NewName);

    if Result = true then
      NewName := frm.lblNewName.Text;

  finally
    frm.Release;
  end;
end;

procedure TfrmRenameSetting.lblNewNameChange(Sender: TObject);
begin
  btnOK.Enabled := Trim (lblNewName.Text) <> EmptyStr;
end;

procedure TfrmRenameSetting.lblNewNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = '\' then
    Key := #0;
end;

end.
