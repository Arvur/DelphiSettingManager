object frmNewSetting: TfrmNewSetting
  Left = 258
  Top = 65
  Caption = 'New IDE Setting'
  ClientHeight = 172
  ClientWidth = 388
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 380
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    388
    172)
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
    Width = 372
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
  object btnOK: TButton
    Left = 222
    Top = 139
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    DisabledImageIndex = 25
    DoubleBuffered = True
    HotImageIndex = 26
    ImageIndex = 24
    ImageMargins.Left = 10
    Images = dm_Glyphs.iLst_Buttons
    ModalResult = 1
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object btnCancel: TButton
    Left = 305
    Top = 139
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    DisabledImageIndex = 4
    DoubleBuffered = True
    HotImageIndex = 5
    ImageIndex = 3
    ImageMargins.Left = 5
    Images = dm_Glyphs.iLst_Buttons
    ModalResult = 2
    ParentDoubleBuffered = False
    TabOrder = 4
  end
  object cbCopyCurrentIDESetting: TCheckBox
    Left = 222
    Top = 56
    Width = 158
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
    Width = 206
    Height = 92
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 1
  end
end
