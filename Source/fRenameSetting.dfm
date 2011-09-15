object frmRenameSetting: TfrmRenameSetting
  Left = 477
  Top = 188
  ActiveControl = lblNewName
  BorderStyle = bsDialog
  ClientHeight = 135
  ClientWidth = 294
  Color = clBtnFace
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    294
    135)
  PixelsPerInch = 96
  TextHeight = 13
  object lblOldName: TLabeledEdit
    Left = 8
    Top = 24
    Width = 278
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
    Width = 278
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    EditLabel.Width = 53
    EditLabel.Height = 13
    EditLabel.Caption = 'New Name'
    TabOrder = 1
    OnChange = lblNewNameChange
    OnKeyPress = lblNewNameKeyPress
  end
  object btnOK: TButton
    Left = 128
    Top = 102
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
    TabOrder = 2
  end
  object btnCancel: TButton
    Left = 211
    Top = 102
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
    TabOrder = 3
  end
end
