object frmRenameSetting: TfrmRenameSetting
  Left = 477
  Top = 188
  Width = 460
  Height = 163
  ActiveControl = lblNewName
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    452
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object lblOldName: TLabeledEdit
    Left = 8
    Top = 24
    Width = 437
    Height = 21
    TabStop = False
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 47
    EditLabel.Height = 13
    EditLabel.Caption = 'Old Name'
    ReadOnly = True
    TabOrder = 0
  end
  object lblNewName: TLabeledEdit
    Left = 8
    Top = 64
    Width = 437
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'New Name'
    TabOrder = 1
    OnChange = lblNewNameChange
    OnKeyPress = lblNewNameKeyPress
  end
  object btnOK: TBitBtn
    Left = 281
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 2
    Kind = bkOK
  end
  object btnCancel: TBitBtn
    Left = 369
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    TabOrder = 3
    Kind = bkCancel
  end
end
