unit LayoutPersistent;

interface

type
  ILayout = interface
  ['{37C7A7D1-7F08-4D69-BDDE-E500AF42BE6A}']
    function GetLayoutName : string;
    procedure SetLayoutName (const ALayoutName : string);

    function GetValue (const AValueName) : Variant;
    procedure SetValue (const AValueName : string; const AValue : Variant);
    property LayoutName : string read GetLayoutName write SetLayoutName;
  end;

  IFormLayout = interface
  ['{70968724-5020-4E5E-9013-8154C46A5D8C}']
    function GetTop : integer;
    function GetLeft : integer;
    function GetWidth : integer;
    function GetHeight : integer;
    procedure SetTop (const AValue : integer);
    procedure SetLeft (const AValue : integer);
    procedure SetWidth (const AValue : integer);
    procedure SetHeight (const AValue : integer);

    property Top : integer read GetTop write SetTop;
    property Left : integer read GetLeft write SetLeft;
    property Width : integer read GetWidth write SetWidth;
    property Height : integer read GetHeight write SetHeight;
  end;

  ILayoutPersistentManager = interface
  ['{B910B21F-D2BC-4768-98FE-245A8C7E5200}']
    procedure SaveLayout (const Layout : ILayout);
    procedure ReadLayout (const LayoutName : string; out Layout : ILayout);
  end;

implementation

end.
