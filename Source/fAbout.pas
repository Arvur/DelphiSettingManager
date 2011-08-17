{-----------------------------------------------------------------------------
 Unit Name: fAbout

 This software and source code are distributed on an as is basis, without
 warranty of any kind, either express or implied.

 This file can be redistributed, modified if you keep this header.

 Copyright © Erwien Saputra 2005  All Rights Reserved.

 Author:    Erwien Saputra
 Purpose:   About box.
 History:
 02/26/2005 - Updated the version number. Release 1.0.
-----------------------------------------------------------------------------}
unit fAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Buttons;

type
  TfrmAbout = class(TForm)
    BitBtn1: TBitBtn;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lblBlog: TLabel;
    lblCodeline: TLabel;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    lblGemmellCom: TLabel;
    procedure lblGemmellComClick(Sender: TObject);
    procedure lblBlogClick(Sender: TObject);
    procedure lblCodelineClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

uses ShellUtilities;

{$R *.dfm}

procedure TfrmAbout.lblCodelineClick(Sender: TObject);
begin
  GotoLink ('http:\\www.codeline.net');
end;

procedure TfrmAbout.lblBlogClick(Sender: TObject);
begin
  GotoLink ('http://blogs.slcdug.org/esaputra');
end;

procedure TfrmAbout.lblGemmellComClick(Sender: TObject);
begin
  GotoLink ('http://lachlan.gemmell.com');
end;

end.

