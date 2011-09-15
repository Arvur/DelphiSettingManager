unit dmGlyphs;

interface

uses
  SysUtils, Classes, ImgList, Controls;

type
  Tdm_Glyphs = class(TDataModule)
    iLst_Buttons: TImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  dm_Glyphs: Tdm_Glyphs;

implementation

{$R *.dfm}

end.
