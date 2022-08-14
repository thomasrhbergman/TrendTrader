unit DaImages;

interface

{$REGION 'Region uses'}
uses
  System.SysUtils, System.Classes, System.ImageList, Vcl.ImgList, Vcl.Controls, Vcl.BaseImageCollection,
  Vcl.ImageCollection, Vcl.VirtualImageList;
{$ENDREGION}

type
  TDMImage = class(TDataModule)
    ilCustomCheckImages: TImageList;
    ImCollection16: TImageCollection;
    ImCollection32: TImageCollection;
    imDocumentState: TImageCollection;
    vil16: TVirtualImageList;
    vil32: TVirtualImageList;
    vilDocumentState: TVirtualImageList;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  DMImage: TDMImage;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
