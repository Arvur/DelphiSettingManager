object frmMain: TfrmMain
  Left = 330
  Top = 86
  Caption = 'Delphi Setting Manager'
  ClientHeight = 312
  ClientWidth = 534
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 350
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Padding.Left = 8
  Padding.Top = 8
  Padding.Right = 8
  Padding.Bottom = 8
  Menu = Menu
  OldCreateOrder = False
  Position = poDesigned
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object pcInstalledIDE: TPageControl
    Left = 8
    Top = 8
    Width = 518
    Height = 263
    Align = alClient
    Images = iLst_IDEs
    TabOrder = 0
  end
  object pnl_Buttons: TPanel
    Left = 8
    Top = 271
    Width = 518
    Height = 33
    Align = alBottom
    BevelOuter = bvNone
    DoubleBuffered = True
    Padding.Top = 8
    ParentDoubleBuffered = False
    TabOrder = 1
    object btnCreateShortcut: TButton
      AlignWithMargins = True
      Left = 0
      Top = 8
      Width = 120
      Height = 25
      Margins.Left = 0
      Margins.Top = 0
      Margins.Right = 8
      Margins.Bottom = 0
      Action = actCreateShortcut
      Align = alLeft
      DisabledImageIndex = 16
      HotImageIndex = 17
      ImageMargins.Left = 5
      Images = dm_Glyphs.iLst_Buttons
      TabOrder = 0
    end
    object btnClose: TButton
      Left = 443
      Top = 8
      Width = 75
      Height = 25
      Action = actFileExit
      Align = alRight
      DisabledImageIndex = 22
      HotImageIndex = 23
      ImageMargins.Left = 5
      Images = dm_Glyphs.iLst_Buttons
      TabOrder = 2
    end
    object btnRunDelphi: TButton
      Left = 128
      Top = 8
      Width = 100
      Height = 25
      Action = actRunDelphi
      Align = alLeft
      DisabledImageIndex = 19
      HotImageIndex = 20
      ImageMargins.Left = 5
      Images = dm_Glyphs.iLst_Buttons
      TabOrder = 1
    end
  end
  object Menu: TMainMenu
    Images = dm_Glyphs.iLst_Buttons
    Left = 200
    Top = 96
    object File1: TMenuItem
      Caption = 'File'
      object SaveSetting1: TMenuItem
        Action = actSaveSetting
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actFileExit
      end
    end
    object mnuCustomSetting: TMenuItem
      Caption = 'Custom Settings'
      object NewCustomSetting: TMenuItem
        Action = actNewSetting
      end
      object DeleteCustomSetting: TMenuItem
        Action = actDeleteSetting
      end
      object RenameCustomSetting: TMenuItem
        Action = actRenameSetting
      end
      object CopyCustomSetting: TMenuItem
        Action = actCopySetting
      end
      object Edit1: TMenuItem
        Action = actEditSetting
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object CreateShortcut1: TMenuItem
        Action = actCreateShortcut
      end
      object RunDelphi1: TMenuItem
        Action = actRunDelphi
      end
    end
    object Options1: TMenuItem
      Caption = 'Options'
      object RestoreDefaultSize1: TMenuItem
        Action = actRestoreSize
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object About1: TMenuItem
        Action = actAbout
      end
    end
  end
  object dlgSave: TSaveDialog
    DefaultExt = 'lnk'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 264
    Top = 96
  end
  object ActionList: TActionList
    Images = dm_Glyphs.iLst_Buttons
    Left = 264
    Top = 184
    object actFileExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 21
    end
    object actCreateShortcut: TAction
      Category = 'CustomSettings'
      Caption = 'Create S&hortcut'
      ImageIndex = 15
      ShortCut = 16456
      OnExecute = actCreateShortcutExecute
    end
    object actRunDelphi: TAction
      Category = 'CustomSettings'
      Caption = 'R&un Delphi'
      ImageIndex = 18
      ShortCut = 16469
      OnExecute = actRunDelphiExecute
    end
    object actNewSetting: TAction
      Category = 'CustomSettings'
      Caption = '&New'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = actNewSettingExecute
    end
    object actDeleteSetting: TAction
      Category = 'CustomSettings'
      Caption = '&Delete'
      ImageIndex = 3
      ShortCut = 16452
      OnExecute = actDeleteSettingExecute
    end
    object actRenameSetting: TAction
      Category = 'CustomSettings'
      Caption = '&Rename'
      ImageIndex = 6
      ShortCut = 16466
      OnExecute = actRenameSettingExecute
    end
    object actCopySetting: TAction
      Category = 'CustomSettings'
      Caption = '&Copy'
      ImageIndex = 9
      ShortCut = 16451
      OnExecute = actCopySettingExecute
    end
    object actRestoreSize: TAction
      Category = 'Options'
      Caption = 'Restore Default Form Size'
      OnExecute = actRestoreSizeExecute
    end
    object actSaveSetting: TAction
      Category = 'File'
      Caption = '&Save Setting to File'
      ImageIndex = 27
      ShortCut = 16467
      OnExecute = actSaveSettingExecute
    end
    object actAbout: TAction
      Category = 'Help'
      Caption = 'About'
      OnExecute = actAboutExecute
    end
    object actEditSetting: TAction
      Category = 'CustomSettings'
      Caption = '&Edit'
      ImageIndex = 12
      ShortCut = 16453
      OnExecute = actEditSettingExecute
    end
  end
  object iLst_IDEs: TImageList
    ColorDepth = cd32Bit
    DrawingStyle = dsTransparent
    Left = 132
    Top = 184
  end
end
