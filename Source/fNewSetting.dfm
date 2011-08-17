object frmNewSetting: TfrmNewSetting
  Left = 258
  Top = 65
  Width = 404
  Height = 210
  Caption = 'New IDE Setting'
  Color = clBtnFace
  Constraints.MinHeight = 140
  Constraints.MinWidth = 380
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    396
    176)
  PixelsPerInch = 96
  TextHeight = 13
  object lblTemplates: TStaticText
    Left = 8
    Top = 56
    Width = 53
    Height = 17
    Caption = 'Templates'
    TabOrder = 5
  end
  object lblSettingName: TLabeledEdit
    Left = 8
    Top = 24
    Width = 380
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 64
    EditLabel.Height = 13
    EditLabel.Caption = 'Setting Name'
    MaxLength = 64
    TabOrder = 0
    OnChange = lblSettingNameChange
    OnKeyPress = lblSettingNameKeyPress
  end
  object btnOK: TBitBtn
    Left = 233
    Top = 144
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 313
    Top = 144
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 4
    Kind = bkCancel
  end
  object cbCopyCurrentIDESetting: TCheckBox
    Left = 232
    Top = 56
    Width = 145
    Height = 17
    Hint = 
      'Check this if you want to copy your default configuration to thi' +
      's new configuration'
    Alignment = taLeftJustify
    Anchors = [akTop, akRight]
    Caption = 'Copy Current IDE Setting'
    Checked = True
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 2
  end
  object clbTemplates: TCheckListBox
    Left = 8
    Top = 72
    Width = 217
    Height = 96
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
