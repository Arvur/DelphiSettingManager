{-----------------------------------------------------------------------------
 Unit Name: fNewSetting
 Author:    Erwien Saputra
 Purpose:   User interface to add a new setting. If the class function returns
            true, the NewSettingName is meaningful.
 History:
 02/21/2005 - Updated the tab order.
 06/18/2005 - Added support for Setting Template.
 06/22/2005 - Modified the execute method, this form loads the templates first
              before assigning the new template name.
-----------------------------------------------------------------------------}

unit fNewSetting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ExtCtrls, SettingTemplate, SettingCollection,
  CheckLst;

type
  TStringArray = array of string;

  TfrmNewSetting = class(TForm)
    lblSettingName: TLabeledEdit;
    btnOK: TButton;
    btnCancel: TButton;
    cbCopyCurrentIDESetting: TCheckBox;
    clbTemplates: TCheckListBox;
    lblTemplates: TStaticText;
    procedure lblSettingNameKeyPress(Sender: TObject; var Key: Char);
    procedure lblSettingNameChange(Sender: TObject);
  private
    { Private declarations }
    procedure LoadIDETemplates (const AIDEVersion : TIDEVersion);
  public
    { Public declarations }
    constructor Create (AOwner: TComponent); override;
    class function Execute (var NewSettingName : string;
        const AIDEVersion : TIDEVersion; var UseCurrentIDESetting: boolean;
        var AppliedTemplates : TStringArray) : boolean;
  end;

var
  frmNewSetting: TfrmNewSetting;

implementation

uses
  dmGlyphs;

{$R *.dfm}

const
  DEFAULT_SETTING_HINT =
    'Check this if you want to use the default IDE setting to be copied to ' +
    'your new custom setting.' + sLineBreak +
    'If this option is unchecked, the new custom setting will have the ' +
    'default factory setting.';

{ TfrmNewSetting }
constructor TfrmNewSetting.Create(AOwner: TComponent);
begin
  inherited;
  cbCopyCurrentIDESetting.Hint := DEFAULT_SETTING_HINT;
  self.clbTemplates.MultiSelect := true;
end;

//Entry point for this form. If NewSettingName is initialized, it will be used
//on the edit box.
//This function returns true if the user clicks OK and the user set the new
//setting name.
class function TfrmNewSetting.Execute(var NewSettingName: string;
    const AIDEVersion : TIDEVersion;  var UseCurrentIDESetting: boolean;
    var AppliedTemplates : TStringArray) : boolean;
var
  frm : TfrmNewSetting;
  Index,
  Loop : integer;
begin
  frm := TfrmNewSetting.Create (nil);

  try
    //Load the template from the Template persistent. LoadIDETemplate must be
    //called before lblSettingName is set, as the lblSettingNameChange event
    //handler depends on the state of clbTemplate.  Whenever have time, remove
    //the coupling from the lblSettingNameChange, do not rely on the event
    //handler.
    frm.LoadIDETemplates (AIDEVersion);

    if Trim (NewSettingName) <> EmptyStr then
      frm.lblSettingName.Text := NewSettingName
    else
      frm.lblSettingName.Text := EmptyStr;

    frm.lblSettingName.SelectAll;
    Result := (frm.ShowModal = MrOK) and
              (frm.lblSettingName.Text <> EmptyStr);

    UseCurrentIDESetting := frm.cbCopyCurrentIDESetting.Checked;

    if (Result = true) then begin
      NewSettingName := frm.lblSettingName.Text;

      SetLength (AppliedTemplates, frm.clbTemplates.Count);
      Index := 0;

      //Return the selected template to the caller.
      for Loop := 0 to frm.clbTemplates.Count - 1 do begin
        if frm.clbTemplates.Checked[Loop] = true then begin
          AppliedTemplates[Index] := frm.clbTemplates.Items[Loop];
          Inc (Index);
        end;
      end;

      //Set the length of the Applied Templates array.
      SetLength (AppliedTemplates, Index);
    end;

  finally
    frm.Release;
  end;
end;

procedure TfrmNewSetting.lblSettingNameChange(Sender: TObject);
var
  EnableControl : boolean;
begin
  EnableControl := not SameText (Trim (lblSettingName.Text), EmptyStr);
  btnOK.Enabled                   := EnableControl;
  cbCopyCurrentIDESetting.Enabled := EnableControl;
  clbTemplates.Enabled            := EnableControl and (clbTemplates.Count > 0);
  lblTemplates.Enabled            := clbTemplates.Enabled;
end;

procedure TfrmNewSetting.lblSettingNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = '\' then
    Key := #0;
end;

procedure TfrmNewSetting.LoadIDETemplates(const AIDEVersion: TIDEVersion);
var
  TemplateCollection : ITemplateCollection;
begin
  clbTemplates.Items.Clear;
  TemplateCollection := SettingTemplate.GetTemplateCollection;

  if Assigned (TemplateCollection) then
    TemplateCollection.GetTemplateNames (AIDEVersion, clbTemplates.Items);
end;

end.
