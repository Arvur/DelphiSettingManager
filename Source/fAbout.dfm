object frmAbout: TfrmAbout
  Left = 0
  Top = 0
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'About'
  ClientHeight = 192
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblBlog: TLabel
    Left = 8
    Top = 56
    Width = 189
    Height = 16
    Caption = 'http://blogs.slcdug.org/esaputra'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = lblBlogClick
  end
  object lblCodeline: TLabel
    Left = 8
    Top = 40
    Width = 135
    Height = 16
    Caption = 'http://www.codeline.net'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = lblCodelineClick
  end
  object lblGemmellCom: TLabel
    Left = 8
    Top = 112
    Width = 159
    Height = 16
    Caption = 'http://lachlan.gemmell.com'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    OnClick = lblGemmellComClick
  end
  object btn_Ok: TButton
    Left = 196
    Top = 159
    Width = 75
    Height = 25
    Cancel = True
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
    TabOrder = 0
  end
  object StaticText1: TStaticText
    Left = 8
    Top = 8
    Width = 179
    Height = 20
    Caption = 'Delphi Setting Manager 1.1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
  end
  object StaticText2: TStaticText
    Left = 8
    Top = 24
    Width = 208
    Height = 20
    Caption = 'Copyright '#169' Erwien Saputra (2005)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object StaticText3: TStaticText
    Left = 8
    Top = 80
    Width = 65
    Height = 20
    Caption = 'Thanks to:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
  end
  object StaticText4: TStaticText
    Left = 8
    Top = 96
    Width = 167
    Height = 20
    Caption = 'Lachlan Gemmell (Edit form)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
end
