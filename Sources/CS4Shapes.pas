{: This help file explain all the entities classes defined in
   the CADSys 4.0 library for both the 2D and 3D use.

   These classes are defined in the CS4Shapes unit file
   that you must include in the <B=uses> clause in all of your units
   that access the types mentioned here.
}
unit CS4Shapes;

{$mode delphi}

interface

uses SysUtils, Classes, Graphics,
     CADSys4, CS4BaseTypes, LCLType, LCLIntf;

type
  {: This type defines the type used to specify the name of
     a <I=font type face> (like Times New Roman).
  }
  TFaceName = string[LF_FACESIZE];
  {: This class encapsulates the interface for the GDI font of
     Windows as defined by <Code=TLOGFONT> structure.

     This font is used by the library for the <See Class=TText2D>
     shape class. The use of it is somewhat difficult in the
     context of a 2D or 3D drawing so it is better to use
     the vectorial text shape <See Class=TJustifiedVectText2D> and
     <See Class=TJustifiedVectText3D>.
  }
  TExtendedFont = class(TObject)
  private
    LogFont: TLOGFONT;
    FHandle: HFONT;
    FCanvas: TCanvas;
    procedure SetNewValue;
    procedure SetCanvas(Cnv: TCanvas);
    procedure SetHeight(Value: Word);
    function GetHeight: Word;
    procedure SetWidth(Value: Word);
    function GetWidth: Word;
    procedure SetEscapement(Value: Word);
    function GetEscapement: Word;
    procedure SetWeight(Value: Word);
    function GetWeight: Word;
    procedure SetItalic(Value: Byte);
    function GetItalic: Byte;
    procedure SetUnderline(Value: Byte);
    function GetUnderline: Byte;
    procedure SetStrikeOut(Value: Byte);
    function GetStrikeOut: Byte;
    procedure SetCharSet(Value: Byte);
    function GetCharSet: Byte;
    procedure SetOutPrecision(Value: Byte);
    function GetOutPrecision: Byte;
    procedure SetClipPrecision(Value: Byte);
    function GetClipPrecision: Byte;
    procedure SetQuality(Value: Byte);
    function GetQuality: Byte;
    procedure SetPicthAndFamily(Value: Byte);
    function GetPicthAndFamily: Byte;
    procedure SetFaceName(Value: TFaceName);
    function GetFaceName: TFaceName;
  public
    {: This is the constructor that creates an instance of a
       font.

       When a new font is created it is set using the
       <I=DEFAULT_GUI_FONT> as defined in Windows specifications.
    }
    constructor Create;
    {: This destructor frees the font informations.

       It also detaches the font form a <I=Canvas>, if the font is
       currently in use by it.
    }
    destructor Destroy; override;
    {: This method assign the font data by using another font
       class as a prototype.

       Parameters:

       <LI=<I=Obj> is the font being used as a prototype.>
    }
    procedure Assign(Obj: TExtendedFont);
    {: This method saves the font informations into a stream.

       Parameters:

       <LI=<I=Strm> is the stream on which save the font structure.>
    }
    procedure SaveToStream(Strm: TStream);
    {: This method retrieves the font informations from a stream.

       Parameters:

       <LI=<I=Strm> is the stream from which retrieve the font
        structure.>
    }
    procedure LoadFromStream(Strm: TStream);
    {: This property attaches the font to a Canvas.

       If you want to use the font on a Canvas you must use this
       property. After you have setted this propery, to detach
       the font from the Canvas assign <B=nil> to this property.
    }
    property Canvas: TCanvas read FCanvas write SetCanvas;
    {: This property contains the handle for the
       <Code=TLOGFONT> structure.
    }
    property Handle: HFONT read FHandle;
    {: This property specifies the <I=lfHeight> field of <Code=TLOGFONT>.
    }
    property Height: Word read GetHeight write SetHeight;
    {: This property specifies the <I=lfWidth> field of <Code=TLOGFONT>.
    }
    property Width: Word read GetWidth write SetWidth;
    {: This property specifies the <I=lfEscapement> field of
       <Code=TLOGFONT>.
    }
    property Escapement: Word read GetEscapement write SetEscapement;
    {: This property specifies the <I=lfWeight> field of
       <Code=TLOGFONT>.
    }
    property Weight: Word read GetWeight write SetWeight;
    {: This property specifies the <I=lfItalic> field of
       <Code=TLOGFONT>.
    }
    property Italic: Byte read GetItalic write SetItalic;
    {: This property specifies the <I=lfUnderline> field of
       <Code=TLOGFONT>.
    }
    property Underline: Byte read GetUnderline write SetUnderline;
    {: This property specifies the <I=lfStrikeOut> field of
       <Code=TLOGFONT>.
    }
    property StrikeOut: Byte read GetStrikeOut write SetStrikeOut;
    {: This property specifies the <I=lfCharSet> field of
       <Code=TLOGFONT>.
    }
    property CharSet: Byte read GetCharSet write SetCharSet;
    {: This property specifies the <I=lfOutPrecision> field of
       <Code=TLOGFONT>.
    }
    property OutPrecision: Byte read GetOutPrecision write SetOutPrecision;
    {: This property specifies the <I=lfClipPrecision> field of
       <Code=TLOGFONT>.
    }
    property ClipPrecision: Byte read GetClipPrecision write SetClipPrecision;
    {: This property specifies the <I=lfQuality> field of
       <Code=TLOGFONT>.
    }
    property Quality: Byte read GetQuality write SetQuality;
    {: This property specifies the <I=lfPitchAndFamily> field of
       <Code=TLOGFONT>.
    }
    property PicthAndFamily: Byte read GetPicthAndFamily write SetPicthAndFamily;
    {: This property specify the <I=lfFaceName> field of
       <Code=TLOGFONT>.
    }
    property FaceName: TFaceName read GetFaceName write SetFaceName;
  end;

  {: This type defines the <I=saving mode> used by the <I=outline> primitive
     shapes.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     This type is used to specify beetwen the two modes:

     <LI=<I=stSpace> forces an <I=outline> to recompute the
      shape's points whenever it must be drawed.>
     <LI=<I=stTime> tell to the <I=outline> to store the
      shape's points and compute them only when the control
      points are changed.>
  }
  TPrimitiveSavingType = (stSpace, stTime);

  {: This type specifies how to draw an arc segment:

     <LI=in the <I=adClockwise> mode the arc segment is
      drawed clockwise>.
     <LI=in the <I=adCounterClockwise> mode the arc segment is
      drawed counter clockwise>.
  }
  TArcDirection = (adClockwise, adCounterClockwise);

  {: This is the class reference type for the
     <See Class=TPrimitive2D> shape class.
  }
  TPrimitive2DClass = class of TPrimitive2D;

  {: This handler can be used to modify a primitive by dragging its
     control points.

     See also <See Class=TObject2DHandler>.
  }
  TPrimitive2DHandler = class(TObject2DHandler)
  public
    procedure DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer); override;
    function OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=2D primitive>.

     A primitive shape is an entity which shape is controlled
     by a set of <I=control points>. This class stores and
     handles this set of point allowing the developer to
     focus on the way to draw the shape from these control
     points.

     This is the right class from which derive your own
     entity classes.

     See also <See Class=TOutline2D> and <See Class=TCurve2D>.

     <B=Warning>: This is an abstract class that cannot be used
     directly.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TPrimitive2D = class(TObject2D)
  private
    fPoints: TPointsSet2D;
  protected
    procedure _UpdateExtension; override;
    {: This method allows you to change the type of the set
       of points used to store the <I=control points> of the
       primitive.

       When the entity is created this method is called to
       create the set of <I=control points> that defines
       the shape of the entity.

       By default a <See Class=TPointsSet2D> instance is created
       to store a maximum of <I=Size> points.

       You may want to override this property to create
       a special set of points that is able to store more
       information than a simple point. For instance you can
       derive a new set from <See Class=TPointsSet2D> that for
       every point store the kind of the point. This is the
       first step to create a <I=path shape> that draw arc
       segment as well as straight lines.
    }
    function CreateVect(const Size: Integer): TPointsSet2D; dynamic;
  public
    {: This is the constructor of the class.

       The constructor need the identifier of the new graphic object.
       This <See Property=TGraphicObject@ID> will be used to
       identify the object in the <See Class=TCADCmp2D>.

       If the object is added with the method <See Method=TCADCmp@AddObject>
       and with the first parameter set to a number equal or greater that
       0, the <I=ID> given here will be overriden.

       If you derives from this class remember to call the
       inherited method. In this case pass the desired number
       of control points and set <See Property=TPointsSet2D@GrowingEnabled>
       of <See Property=TPrimitive2D@Points> to the desired value.

       Parameters:

       <LI=<I=ID> is the ID of the object.>
       <LI=<I=NPts> is the number of control points that the
        primitive can store without growing the vector.>
    }
    constructor Create(ID: LongInt; NPts: Integer);
    destructor Destroy; override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the set of <I=control points> used
       to define the shape of the entity.

       See the introduction of <See Class=TPrimitive2D> for details.
    }
    property Points: TPointsSet2D read fPoints write fPoints;
  end;

  {: This class defines a 2D line segment.

     The entity has two <I=control points> that are the extremes of
     the segment.
  }
  TLine2D = class(TPrimitive2D)
  public
    {: This constructor creates a new 2D line segment.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the starting point of the segment.>
       <LI=<I=P2> is the ending point of the segment.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a <I=2D outline>.

     An <I=outline> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     For an <I=outline> the control points are the same as
     the <I=profile points>.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     See also <See Class=TCurve2D>.

     <B=Note>: The control points are always in the object model
     coordinate system !
  }
  TOutline2D = class(TPrimitive2D)
  protected
    {: This method is called when the <I=profile points>
       are needed.

       It must returns the set of the <I=profile points>
       created by the class.

       <B=Warning>: You don't have to delete the set of points
       returned by the method.
    }
    function GetProfilePoints: TPointsSet2D; virtual; abstract;
    {: This method returns the number of <I=profile points>
       that is equal (for an <I=outline>) to the number
       of <I=control points>.
    }
    function GetNPts: Integer; virtual; abstract;
    {: This method returns <B=True> if the set of <I=profile points>
       is closed.

       The fact that the set is closed influences the way in which
       it is drawed and picked.
    }
    function GetIsClosed: Boolean; virtual;
  public
    {: This method initializes the profile points vector for using.

       You must call this method before any use of the methods that
       work on the set of <I=profile points>.

       It is better to call this method in a <Code=try-finally>
       block with <See Method=TOutline2D@EndUseProfilePoints>
       in the finally part.
    }
    procedure BeginUseProfilePoints; dynamic;
    {: This method finalizes the <I=profile points> when you
       finish to use them.

       You must call this method when you no longer need to use
       the set of <I=profile points>. This allow the library
       to eventually saves the memory used by the entity.

       It is better to call this method in a <Code=try-finally>
       block with this method in the finally part.

       <B=Note>: This method must be called after the
       <See Method=TOutline2D@BeginUseProfilePoints>.
    }
    procedure EndUseProfilePoints; dynamic;
    {: This is the set of <I=profile points> that is used to
       draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property ProfilePoints: TPointsSet2D read GetProfilePoints;
    {: This property contains the size of the set of
       <I=profile points> that is used to draw the entity.

       See the class description of <See Class=TOutline2D>.
    }
    property NumberOfProfilePts: Integer read GetNPts;
    {: This property is <B=True> when the shape of the
       entity must be considere as closed.
    }
    property IsClosed: Boolean read GetIsClosed;
  end;

  {: This class defines a 2D polyline.

     A polyline is obtained by connecting the <I=profile points> (in
     this case are the same as the <I=control points>) with straight
     line segments.
  }
  TPolyline2D = class(TOutline2D)
  protected
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    {: This constructor creates a new 2D polyline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the <I=control points> of
        the polyline. If you want to create a pointless polyline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction phase by using the method of
        <See Property=TPrimitive2D@Points>.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D polygon.

     A polygon is obtained by connecting the <I=profile points> (
     in this case they are the same as the <I=profile points>)
     with straight segments and filling the shape with the current
     brush of the Canvas.
  }
  TPolygon2D = class(TPolyline2D)
  protected
    function GetIsClosed: Boolean; override;
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D curve.

     A <I=curve> primitive shape is an entity that is drawed as
     a connected set of points (<I=profile points>) which displacement
     is controlled by a set of <I=control points>. The points used
     to draw the entity may be keept in memory or recomputed from
     the control points whenever they are needed to draw the
     shape. In the former a lot of memory may be used but the
     drawing is faster, in the latter the drawing of the shape
     may take a quite longer time but the memory is used
     efficently.

     A curve is a 2D polyline in which the points that define
     the shape (<I=profile points>)are not the same as the
     <I=control points> of the entity. In this case the control
     points only control the shape of the curve.

     For this class the <See property=TCurve2D@SavingType> property
     defines the mode of saving used by the entity.

     Before using the <I=profile points> you must call the
     <See Method=TOutline2D@BeginUseProfilePoints> and after the
     using you must call the
     <See Method=TOutline2D@EndUseProfilePoints> method.

     You have to put the code that defines the <I=profile points>
     from the <I=control points> in the
     <See Method=TCurve2D@PopulateCurvePoints> method.

     See also <See Class=TOutline2D>.

     <B=Note>: The control points and the profile points are
     always in the object model coordinate system !
  }
  TCurve2D = class(TOutline2D)
  private
    fSavingType: TPrimitiveSavingType;
    fCurvePrecision: Word;
    fCurvePoints: TPointsSet2D;
    fCountReference: Integer;

    procedure SetCurvePrecision(N: Word);
    procedure SetPrimitiveSavingType(S: TPrimitiveSavingType);
    procedure FreeCurvePoints;
  protected
    procedure _UpdateExtension; override;
    {: This method is called whenever the <I=profile points>
       must be computed.

       You must redefine this method to fill the set of
       <I=control points> by using the actual set of
       <I=control points>. In defining this method you have
       to call the inherited method passing it the
       right number of <I=profile points> used by the
       entity as the <I=N> parameter.

       In the method you may use the
       <See Property=TOutline2D@ProfilePoints> to add the
       points to the set of <I=profile points>.

       <B=Warning>: Don't call
       <See Method=TOutline2D@BeginUseProfilePoints> nor
       <See Method=TOutline2D@EndUseProfilePoints> in this method.
       Also don't access the <See Property=TOutline2D@NumberOfProfilePts>
       property but use the number of points that you have
       computed.
    }
    function PopulateCurvePoints(N: Word): TRect2D; dynamic;
    function GetProfilePoints: TPointsSet2D; override;
    function GetNPts: Integer; override;
  public
    constructor Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
    procedure BeginUseProfilePoints; override;
    procedure EndUseProfilePoints; override;
    {: This property may be used in the
       <See Method=TCurve2D@PopulateCurvePoints> as a parameters
       to control the precision (ie the number of <I=profile points>)
       used to draw the curve profile.

       By default it is 50.
    }
    property CurvePrecision: Word read fCurvePrecision write SetCurvePrecision;
    {: This property specify the saving mode used by the
       curve.

       By default it is set to <I=stTime>.

       See also <See Type=TPrimitiveSavingType>.
    }
    property SavingType: TPrimitiveSavingType read fSavingType write SetPrimitiveSavingType;
  end;

  {: This class defines a 2D rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle.
  }
  TFrame2D = class(TCurve2D)
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D frame.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> is the bottom-left corner of the frame.>
       <LI=<I=P2> is the upper-right corner of the frame.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  {: This class defines a 2D filled rectangle.

     The entity has two <I=control points> that are the corner
     points of the rectangle (these are in the object model space).
  }
  TRectangle2D = class(TFrame2D)
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
  end;

  {: This class defines an arc segment of a 2D ellipse.

     The arc is defined by the two corner of the box that
     contains the arc's ellipse, and the starting and ending
     angles of the arc.
  }
  TArc2D = class(TCurve2D)
  private
    FStartAngle, FEndAngle: TRealType;
    FDirection: TArcDirection;

    procedure SetStartAngle(A: TRealType);
    procedure SetEndAngle(A: TRealType);
    procedure SetArcDirection(D: TArcDirection);
    procedure GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor  creates a new arc of a 2D ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that defines the arc's ellipse.>
       <LI=<I=SA> is the starting angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>
       <LI=<I=SA> is the ending angle (in radiants) of the
        arc. The angle that correspond to zero radiants is
        along the positive x-axis (if no transformation is applied
        to the object).>

       Note: Once created, the arc has four control points. The
       first two are <I=P1> and <I=P2>; the third is the point
       that lies on the segment from the center of the arc's ellipse
       and the starting point of the arc; the fourth is the point
       that lies on the segment from the center of the arc's ellipse and
       the ending point of the arc.
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the starting angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property StartAngle: TRealType read FStartAngle write SetStartAngle;
    {: This property contains the ending angle of the arc in radiants.

       The angle that correspond to zero radiants is along the
       positive x-axis (if no transformation is applied to the
       object).
    }
    property EndAngle: TRealType read FEndAngle write SetEndAngle;
    {: This property contains the direction used to draw the arc.

       See <See Type=TArcDirection> for details.
    }
    property Direction: TArcDirection read FDirection write SetArcDirection;
  end;

  {: This class defines a 2D ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }
  TEllipse2D = class(TCurve2D)
  private
    procedure GetEllipseParams(var CX, CY, RX, RY: TRealType);
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new ellipse.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=P1> and <I=P2> are the corner points of the frame
        that contains the ellipse.>
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
  end;

  {: This class defines a 2D filled ellipse.

     The ellipse is defined by the two corner of the box that
     contains it.
  }
  TFilledEllipse2D = class(TEllipse2D)
  public
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType;
                  var Distance: TRealType): Integer; override;
  end;

  {: This class defines a 2D B-Spline curve.

     The B-Spline is defined by its control points.
     The order of the spline is 3 but you can change it.
  }
  TBSpline2D = class(TCurve2D)
  private
    fOrder: Byte;
  protected
    function PopulateCurvePoints(N: Word): TRect2D; override;
  public
    {: This constructor creates a new 2D spline.

       Parameters:

       <LI=<I=ID> is the object identifier.>
       <LI=<I=Pts> is an array that contains the control points
        of the spline. If you want to create a pointless spline
        (because you want to add the points after the construction)
        you must pass an array of only one point (an empty array is
        not allowed by Delphi) and delete the point after the
        construction.>
    }
    constructor Create(ID: LongInt; const Pts: array of TPoint2D);
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    {: This property contains the order of the spline.

       By default it is three (cubic spline).
    }
    property Order: Byte read FOrder write FOrder;
  end;

  {: This class defines a 2D text object that uses the Windows
     font for drawing.

     I created a new text object that is more suitable for a
     2D/3D CAD programs. By the fact that any TTF may be
     converted into the new font format, I discourage the use
     of this object that is keept in the library only for
     backward compatibility.

     See <See Class=TVectFont> for information on the
     new type of Text object.

     <B=Note>: The new text object is not able to fill the
      interior of characters. If you need this capability you
      still have to use this kind of Text object.
  }
  TText2D = class(TPrimitive2D)
  private
    fText: AnsiString;
    fHeight: TRealType;
    fExtFont: TExtendedFont;
    fDrawBox, fRecalcBox: Boolean;
    fClippingFlags: Integer; // Win32s DrawText flags.
  public
    {: Create a new text entity in the rectangle <I=Rect1>, with
       the given <I=Height> and <I=Text>.

       Points[0] will be the left-bottom corner of the text
       bounding box (also used as clipping box for the text)
       and Points[1] the right-up corner.

       The <I=width> of the text is set to the width of <I=Rect1>.
       If you dont know the dimension of the Text on screen, set
       the <See Property=TText2D@AutoSize> property to <B=True>.
       The first time the text will be drawed the bounding box will
       be adjusted automatically.

       The rectangle will be drawed in the current brush and pen
       if <See Property=TText2D@DrawBox> property is <B=True>.
    }
    constructor Create(ID: LongInt; Rect1: TRect2D; Height: TRealType; Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the heigth of the text in world
       units.

       This is different from the Heigth of font used to render
       the text object.
    }
    property Height: TRealType read fHeight write fHeight;
    {: This property contains the <See Class=TExtendedFont>
       instance used to render the text.

       Use it to change the font.
    }
    property LogFont: TExtendedFont read FExtFont;
    {: If this property is set to <B=True> the text box is
       drawed below the text. Otherwise only the text is drawed.

       By default it is <B=False>.
    }
    property DrawBox: Boolean read FDrawBox write FDrawBox;
    {: If this property is <B=True>, the bounding box is changed
       when the object is drawed so it contains the whole text.

       By default it is <B=False>.
    }
    property AutoSize: Boolean read fRecalcBox write fRecalcBox;
    {: This property contains the text string used by the
       text entity.

       You may include more than one line of text simply
       adding <Code=#10#13> beetwen lines.
    }
    property Text: AnsiString read FText write FText;
    {: This property contains the <I=clipping flags> used
       by drawing the text with the <I=DrawText> API function.

       By default the are setted to <I=DT_NOCLIP>.
    }
    property ClippingFlags: Integer read FClippingFlags write FClippingFlags;
  end;

  {: This class rapresents a scalable raster bitmap.

     This object is useful when you want a bitmap with world
     dimension that is scaled when you zoom in a portion of the
     drawing. For instance this can be useful for GIS
     applications. However this object isnt fast and sometimes
     the bitmap is not drawed. Maybe the problem is the Windows
     95 bitmap support. I will add faster and reliable
     raster capability to the library as soon as I have time.

     You can however use a thirdy part library to enhance this
     object by using this class as a blueprint for your
     specific bitmap entity.
  }
  TBitmap2D = class(TPrimitive2D)
  private
    fBitmap: TBitmap;
    fScaleFactor: TRealType;
    fAspectRatio: TRealType;
    fCopyMode: TCopyMode;

    procedure SetScaleFactor(SF: TRealType);
    procedure SetAspectRatio(AR: TRealType);
  public
    {: This constructor creates a new bitmap object.

       <I=Bmp> is the bitmap to be drawed and it will be freed
        by the object.

       <I=P1> and <I=P2> are the corner points of the bitmap
       in world coordinates (and the bitmap will be stretched
       to fit in).

       <B=Note>: The bitmap cannot be rotated !
    }
    constructor Create(ID: LongInt; const P1, P2: TPoint2D; Bmp: TBitmap);
    destructor Destroy; override;
    procedure Assign(const Obj: TGraphicObject); override;
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    {: This property contains the bitmap to be drawed.

       It will be freed by the object.
    }
    property Bitmap: TBitmap read FBitmap;
    {: This property may contains the scale factor to be used for the
       bitmap.

       If you set this property the bounding box is recomputed and
       the bitmap is not stretched. The first Points will remain
       in the position specified but the second point will be
       repositioned using the ScaleFactor.

       The value rapresent how many drawing unit correspond to
       one pixel in the image. So an image of 50x50 pixels with
       a ScaleFactor of 2.0 will be 100x100 drawing units large.
       How many application units (cm, inch, etc) is a drawing
       unit is left to you.
       
       Setting it to zero (the default) means that no scale is
       needed.
    }
    property ScaleFactor: TRealType read fScaleFactor write SetScaleFactor;
    {: This property may contains the aspect ratio (Width/Heigth) to
       be used for the bitmap.

       This property is used only if ScaleFactor is not 0.0

       Setting it to zero (the default) means that no aspect ratio is
       needed.
    }
    property AspectRatio: TRealType read fAspectRatio write SetAspectRatio;
    {: This property contains the CopyMode used to copy the
       bitmap.
    }
    property CopyMode: TCopyMode read fCopyMode write fCopyMode;
  end;

  {: This class defines a 2D/3D vectorial char as a set of
     polylines.

     The number of polylines that can be used to define a char
     is limited by the size given at the moment of creation.

     In the char definition the polylines must be defined in the
     unitary square that ranges from X:0,1 and Y:0,1, so that
     the char may be scaled with a scale transformation.

     When the char is drawed inside a string, the real dimension
     of the char is used (the font is a proportional one).

     <B=Note>: The vectorial character may be created with
     conversion from Windows True Type Font with a
     separate utility given with the library.
  }
  TVectChar = class(TObject)
  private
    fSubVects: TIndexedObjectList;
    fExtension: TRect2D;

    function GetVect(Idx: Integer): TPointsSet2D;
    function GetVectCount: Integer;
  public
    {: This constructor creates an new instance of the vectorial char.

       Parameters:

       <LI=<I=NSubVect> is the number of polylines that defines
        the char.>
    }
    constructor Create(NSubVect: Integer);
    destructor Destroy; override;
    {: This method create a new instance of a char definition by
       retrieves its datas from a stream.

       Parameters:

       <LI=<I=Stream> is the stream that contains the char
        definition (previously saved with
        <See Method=TVectChar@SaveToStream>).>
    }
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion);
    {: This method saves a char definition into a stream.

       Parameters:

       <LI=<I=Stream> is the stream into which save the char
        definition (that can be retrived with
        <See Method=TVectCharCreateFromStream>).>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method is used computes the real dimension of the
       char.

       This method <B=MUST> be called when the definition of
       the char is finished (that is when you finish to
       create the polylines of the char).
    }
    procedure UpdateExtension(Sender: TObject);
    {: This property contains the set of polylines that defines
       the char.

       The polylines are stored in instances of
       <See Class=TPointsSet2D>.

       <I=Idx> is the index of the polyline in the set.
    }
    property Vectors[Idx: Integer]: TPointsSet2D read GetVect;
    {: This property contains the number of polylines that
       defines the char.

       This property cannot be changed.
    }
    property VectorCount: Integer read GetVectCount;
    {: This property contains the bounding box of the char.

       This value is updated by calling
       <See Method=TVectChar@UpdateExtension>. You must call
       that method before use this property.
    }
    property Extension: TRect2D read fExtension;
  end;

  {: This class defines a 2D vectorial font typeface.

     The typeface is made up of 256 instances of
     <See Class=TVectChar> (so no unicode may be used). The font
     may be stored and retrived from a font file.

     The easy way to define a font typeface is by using the
     <I=True Type Font> converter that create it from a TTF font.

     The chars are indexed by their <I=ASCII> value. If a char
     is not defined, it will not be drawed and an underline will
     be shown.

     The space and carriage returns are handled automatically,
     but you can change their shape.

     Any font used by the application is registered in the
     system with an <I=Index>, that is used to save and retrieve
     the font from the disk and associate the correct font to
     the texts.

     If you change the indexes among different drawings, you
     could load the wrong file. In this case you will notice that
     the text is drawed with a different font. If the index
     doesn't correspond to any font and no default font is
     defined an exception will be raised and the drawing will not
     be loaded.

     See <See Function=CADSysSetDefaultFont> and the other
     registration functions for details.
  }
  TVectFont = class(TObject)
  private
    fVects: TIndexedObjectList;

    function GetChar(Ch: Char): TVectChar;
  public
    constructor Create;
    destructor Destroy; override;
    {: This method creates an instance of the font and retrieves
       its definition from a Stream.

       Parameters:

       <LI=<I=Stream> is the stream from which retrieve the
        font definition.>
    }
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion);
    {: This method saves an instance of the font into a Stream.

       Parameters:

       <LI=<I=Stream> is the stream to which saves the font
        definition.>
    }
    procedure SaveToStream(const Stream: TStream);
    {: This method draws a char of the font on a canvas using a
       transform mapping from a 2D viewing system.

       Parameters:

       <LI=<I=Ch> is the index (ASCII) of the char to be drawed.>
       <LI=<I=DrawPoint> is the 2D point in world coordinates at
        which draw the char (it is the botton-left corner of the
        bounding box of the char). The point is changed by the
        method into the position for the next char on the same
        text line.>
       <LI=<I=H> is the size of the char in world units. This value
        is used to scale the char definition.>
       <LI=<I=ICS> is the space between two chars (in normalized
        units). For example a value of 0.2 means a space of
        20% of H.>
       <LI=<I=VT> is the mapping transform from world to screen.
        It may be obtained from the
        <See Property=TCADViewport@ViewportToScreenTransform>
        property.>
       <LI=<I=Cnv> is the canvas on which draw the char.>
    }
    procedure DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv: TDecorativeCanvas);
    {: This method returns the bounding box of a vectorial text string
       when the current font is used to draw it.

       Parameters:

       <LI=<I=Str> is the string.>
       <LI=<I=H> is the size of the char in world units.>
       <LI=<I=InterChar> is the space beetwen two chars
        (in normalized units). For example a value of 0.2 means
        a space of 20% of H.>
       <LI=<I=InterLine> is the space beetwen two lines of the
        text (in normalizaed units). For example a value of 0.2
        means a space of 20% of H.>
    }
    function GetTextExtension(Str: AnsiString; H, InterChar, InterLine: TRealType): TRect2D;
    {: This method creates a new char definition and returns the
       <See Class=TVectChar> that rapresents it.

       You must use the returned value to define the char.

       If the char is already present it will be deleted.

       Parameters:

       <LI=<I=Ch> is the characted to be defined.>
       <LI=<I=N> is the number of polylines used to draw the char.>
    }
    function CreateChar(Ch: Char; N: Integer): TVectChar;
    {: This property contains the set of chars of the font.

       The characters are indexed by their <I=ASCII> code.
    }
    property Chars[Ch: Char]: TVectChar read GetChar;
  end;

  {: This type defines the horizontal justification mode for a
     2D/3D vectorial text:

     <LI=<I=jhLeft> means left justification.>
     <LI=<I=jhRight> means right justification.>
     <LI=<I=jhCenter> means center justification.>
  }
  THJustification = (jhLeft, jhRight, jhCenter);
  {: This type defines the vertical justification mode for a
     2D/3D vectorial text:

     <LI=<I=jvTop> means top justification.>
     <LI=<I=jvBottom> means bottom justification.>
     <LI=<I=jvCenter> means center justification.>
  }
  TVJustification = (jvTop, jvBottom, jvCenter);

  {: This class defines the 2D vectorial text.

     The text may be multilines and justified. It uses an
     instance of <See Class=TVectFont> to extract the typeface
     to be used.
  }
  TJustifiedVectText2D = class(TPrimitive2D)
  private
    fVectFont: TVectFont;
    fText: AnsiString;
    fHJustification: THJustification;
    fVJustification: TVJustification;
    fBasePoint: TPoint2D;
    fHeight, fCharSpace, fInterLine: TRealType;
    fDrawBox: Boolean;

    procedure SetHeight(H: TRealType);
    procedure SetCharSpace(S: TRealType);
    procedure SetInterLine(S: TRealType);
    procedure SetText(T: String);
    function GetTextExtension: TRect2D;
    procedure DrawText(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  protected
    procedure _UpdateExtension; override;
  public
    {: This constructor creates a new instance of the class.

       Parameters:

       <LI=<I=ID> is identifier that univocally identify the
        object in the CAD. By means of the method used to add the
        object to the CAD, the <I=ID> of the object might be different
        from the one supplied here. See <See Method=TCADCmp@AddObject>
        for details.>
       <LI=<I=FontVect> is the font typeface. Use
        <See Function=CADSysFindFontByIndex> and
        <See Function=CADSysFindFontIndex> for details.>
       <LI=<I=TextBox> is the rectangle used to justify the text.
        The string is drawed from the upper-left corner of this
        box.>
       <LI=<I=Height> is the size of the font in world units.>
       <LI=<I=Txt> is the text to be drawed.>
    }
    constructor Create(ID: LongInt; FontVect: TVectFont; TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
    constructor CreateFromStream(const Stream: TStream; const Version: TCADVersion); override;
    procedure Assign(const Obj: TGraphicObject); override;
    procedure SaveToStream(const Stream: TStream); override;
    procedure Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer); override;
    function OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer; override;
    {: This property contains the size of the font in world unit.
    }
    property Height: TRealType read fHeight write SetHeight;
    {: This property contains the spacing beetwen chars in
       normalized unit.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property CharSpace: TRealType read fCharSpace write SetCharSpace;
    {: This property contains the spacing beetwen lines of the
       text.

       For example a value of 0.2 means a space of 20% of the
       <See Property=TJustifiedVectText2D@Height>.
    }
    property InterLine: TRealType read fInterLine write SetInterLine;
    {: This property contains the instance of the font that is
       used to draw the text.
    }
    property VectFont: TVectFont read fVectFont write fVectFont;
    {: If this property is <B=True>, a frame is drawed around
       the text.
    }
    property DrawBox: Boolean read fDrawBox write fDrawBox;
    {: This property contains the text to be drawed.
    }
    property Text: AnsiString read FText write SetText;
    {: This property specifies the horizontal justification.
    }
    property HorizontalJust: THJustification read fHJustification write fHJustification;
    {: This property specifies the vertical justification.
    }
    property VerticalJust: TVJustification read fVJustification write fVJustification;
  end;

  {: This procedure sets the default font.
  }
  procedure CADSysSetDefaultFont(const Font: TVectFont);
  {: This function returns the default font.
  }
  function  CADSysGetDefaultFont: TVectFont;
  {: This function initializes the list of registered fonts.
  }
  procedure CADSysInitFontList;
  {: This function clears the list of registered fonts.
  }
  procedure CADSysClearFontList;
  {: This function returns the index of a font.

     If the font is not registered an exception will be raised.

     Parameters:

     <LI=<I=Font> is the font to be searched.>
  }
  function  CADSysFindFontIndex(const Font: TVectFont): Word;
  {: This function returns the font that was registered with
     the specified index.

     If the index is not used and a default font is defined,
     the function returns the default font. Otherwise an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the index of the font to be searched for.>
  }
  function  CADSysFindFontByIndex(Index: Word): TVectFont;
  {: This function register a font by retriving it from
     a file.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=FileName> is name of the file that contains the
      font.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
  procedure CADSysRegisterFontFromFile(Index: Word; const FileName: String);
  {: This function register a font.

     If the index of registration is already in use an
     exception will be raised.

     Parameters:

     <LI=<I=Index> is the registration index.>
     <LI=<I=Font> is the font to be registered.>

     <B=Note>: There are <I=MAX_REGISTERED_FONTS> slots in the
     registration list.
  }
  procedure CADSysRegisterFont(Index: Word; const Font: TVectFont);
  {: This function clear the registration of a font.

     Parameters:

     <LI=<I=Index> is the registration index.>
  }
  procedure CADSysUnregisterFont(Index: Word);

const
  {: This constats rapresent the maximum number of vectorial fonts
     that may be used in the library.
  }
  MAX_REGISTERED_FONTS = 512;

implementation

uses Math, Dialogs;

var
  VectFonts2DRegistered: array[0..MAX_REGISTERED_FONTS] of TVectFont;
  _NullChar: TVectChar;
  _DefaultFont: TVectFont;
  _DefaultHandler2D: TPrimitive2DHandler;

// =====================================================================
// TExtendedFont
// =====================================================================

procedure TExtendedFont.SetHeight(Value: Word);
begin
  LogFont.lfHeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetHeight: Word;
begin
  Result := LogFont.lfHeight;
end;

procedure TExtendedFont.SetWidth(Value: Word);
begin
  LogFont.lfWidth := Value;
  SetNewValue;
end;

function TExtendedFont.GetWidth: Word;
begin
  Result := LogFont.lfWidth;
end;

procedure TExtendedFont.SetEscapement(Value: Word);
begin
  LogFont.lfEscapement := Value;
  SetNewValue;
end;

function TExtendedFont.GetEscapement: Word;
begin
  Result := LogFont.lfEscapement;
end;

procedure TExtendedFont.SetWeight(Value: Word);
begin
  LogFont.lfWeight := Value;
  SetNewValue;
end;

function TExtendedFont.GetWeight: Word;
begin
  Result := LogFont.lfWeight;
end;

procedure TExtendedFont.SetItalic(Value: Byte);
begin
  LogFont.lfItalic := Value;
  SetNewValue;
end;

function TExtendedFont.GetItalic: Byte;
begin
  Result := LogFont.lfItalic;
end;

procedure TExtendedFont.SetUnderline(Value: Byte);
begin
  LogFont.lfUnderline := Value;
  SetNewValue;
end;

function TExtendedFont.GetUnderline: Byte;
begin
  Result := LogFont.lfUnderline;
end;

procedure TExtendedFont.SetStrikeOut(Value: Byte);
begin
  LogFont.lfStrikeOut := Value;
  SetNewValue;
end;

function TExtendedFont.GetStrikeOut: Byte;
begin
  Result := LogFont.lfStrikeOut;
end;

procedure TExtendedFont.SetCharSet(Value: Byte);
begin
  LogFont.lfCharSet := Value;
  SetNewValue;
end;

function TExtendedFont.GetCharSet: Byte;
begin
  Result := LogFont.lfCharSet;
end;

procedure TExtendedFont.SetOutPrecision(Value: Byte);
begin
  LogFont.lfOutPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetOutPrecision: Byte;
begin
  Result := LogFont.lfOutPrecision;
end;

procedure TExtendedFont.SetClipPrecision(Value: Byte);
begin
  LogFont.lfClipPrecision := Value;
  SetNewValue;
end;

function TExtendedFont.GetClipPrecision: Byte;
begin
  Result := LogFont.lfClipPrecision;
end;

procedure TExtendedFont.SetQuality(Value: Byte);
begin
  LogFont.lfQuality := Value;
  SetNewValue;
end;

function TExtendedFont.GetQuality: Byte;
begin
  Result := LogFont.lfQuality;
end;

procedure TExtendedFont.SetPicthAndFamily(Value: Byte);
begin
  LogFont.lfPitchAndFamily := Value;
  SetNewValue;
end;

function TExtendedFont.GetPicthAndFamily: Byte;
begin
  Result := LogFont.lfPitchAndFamily;
end;

procedure TExtendedFont.SetFaceName(Value: TFaceName);
var
  Cont: Byte;
begin
  for Cont := 1 to Length(Value) do
   LogFont.lfFaceName[Cont - 1] := Value[Cont];
  LogFont.lfFaceName[Length(Value)] := #0;
  SetNewValue;
end;

function TExtendedFont.GetFaceName: TFaceName;
begin
  Result := LogFont.lfFaceName;
end;

procedure TExtendedFont.SetNewValue;
var
  TmpHandle: HFONT;
begin
  TmpHandle := CreateFontIndirect(LogFont);
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, TmpHandle);
  DeleteObject(FHandle);
  FHandle := TmpHandle;
end;

procedure TExtendedFont.SetCanvas(Cnv: TCanvas);
begin
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, FCanvas.Font.Handle);
  FCanvas := Cnv;
  if Assigned(FCanvas) then SelectObject(FCanvas.Handle, FHandle);
end;

constructor TExtendedFont.Create;
begin
  inherited Create;
  GetObject(GetStockObject(DEFAULT_GUI_FONT), SizeOf(LogFont), @LogFont);
  LogFont.lfFaceName := 'Small Font';
  FHandle := CreateFontIndirect(LogFont);
end;

procedure TExtendedFont.Assign(Obj: TExtendedFont);
begin
  if Obj = Self then
   Exit;
  LogFont := TExtendedFont(Obj).LogFont;
  SetNewValue;
end;

destructor TExtendedFont.Destroy;
begin
  if Assigned(FCanvas) then
   SelectObject(FCanvas.Handle, FCanvas.Font.Handle);
  DeleteObject(FHandle);
  inherited Destroy;
end;

procedure TExtendedFont.SaveToStream(Strm: TStream);
begin
  with Strm do
   Write(LogFont, SizeOf(LogFont));
end;

procedure TExtendedFont.LoadFromStream(Strm: TStream);
begin
  with Strm do
   begin
     Read(LogFont, SizeOf(LogFont));
     SetNewValue;
   end;
end;

// =====================================================================
// TPrimitive2DHandler
// =====================================================================

procedure TPrimitive2DHandler.DrawControlPoints(const Sender: TObject2D; const VT: TTransf2D; const Cnv: TDecorativeCanvas; const Width: Integer);
var
  TmpPt: TPoint2D;
  Cont: Integer;
begin
  if Sender is TPrimitive2D then
   with TPrimitive2D(Sender) do
    if not HasTransform then
     for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], VT);
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y), Width);
      end
    else
     for Cont := 0 to Points.Count - 1 do
      begin
        TmpPt := TransformPoint2D(Points[Cont], MultiplyTransform2D(ModelTransform, VT));
        DrawPlaceHolder(Cnv, Round(TmpPt.X), Round(TmpPt.Y), Width);
      end;
end;

function TPrimitive2DHandler.OnMe(const Sender: TObject2D; Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  Cont: Integer;
  ResDist: TRealType;
begin
  Result := PICK_NOOBJECT;
  if Sender is TPrimitive2D then
   with TPrimitive2D(Sender) do
    if HasTransform then
     begin
       for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(Pt, TransformPoint2D(Points[Cont], ModelTransform), Aperture, ResDist) and
           (ResDist <= Distance) then
         begin
           Result := Cont;
           Distance := ResDist;
         end;
     end
    else
     begin
       for Cont := 0 to Points.Count - 1 do
        if NearPoint2D(Pt, Points[Cont], Aperture, ResDist) and
           (ResDist <= Distance) then
         begin
           Result := Cont;
           Distance := ResDist;
         end;
     end;
end;

// =====================================================================
// TPrimitive2D
// =====================================================================

procedure TPrimitive2D._UpdateExtension;
begin
  if not Assigned(fPoints) or (fPoints.Count = 0) then
   WritableBox := Rect2D(0, 0, 0, 0)
  else
   { Change the extension. }
   WritableBox := TransformBoundingBox2D(fPoints.Extension, ModelTransform);
end;

function TPrimitive2D.CreateVect(const Size: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D.Create(Size);
end;

constructor TPrimitive2D.Create(ID: LongInt; NPts: Integer);
begin
  inherited Create(ID);
  { Create the internal vector. }
  fPoints := CreateVect(NPts);
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

procedure TPrimitive2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TPrimitive2D) then
   begin // Per default non aggiunge i punti.
     if not Assigned(fPoints) then
      begin
        fPoints := CreateVect(0);
        fPoints.GrowingEnabled := True;
        fPoints.OnChange := UpdateExtension;
      end;
     fPoints.Clear;
   end;
end;

constructor TPrimitive2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     Read(TmpWord, SizeOf(TmpWord));
     fPoints := CreateVect(TmpWord);
     { Read all the points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        Read(TmpPt, SizeOf(TmpPt));
        fPoints.Points[Cont] := TmpPt;
      end;
     Read(TmpBoolean, SizeOf(TmpBoolean));
     fPoints.GrowingEnabled := TmpBoolean;
   end;
  fPoints.OnChange := UpdateExtension;
  SetSharedHandler(_DefaultHandler2D);
end;

procedure TPrimitive2D.SaveToStream(const Stream: TStream);
var
  TmpWord: Word;
  Cont: Integer;
  TmpPt: TPoint2D;
  TmpBoolean: Boolean;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpWord := fPoints.Count;
     Write(TmpWord, SizeOf(TmpWord));
     { Write all points. }
     for Cont := 0 to TmpWord - 1 do
      begin
        TmpPt := fPoints.Points[Cont];
        Write(TmpPt, SizeOf(TmpPt));
      end;
     TmpBoolean := fPoints.GrowingEnabled;
     Write(TmpBoolean, SizeOf(TmpBoolean));
   end;
end;

destructor TPrimitive2D.Destroy;
begin
  if Assigned(fPoints) then
   fPoints.Free;
  inherited Destroy;
end;

// =====================================================================
// TLine2D
// =====================================================================

constructor TLine2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TLine2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

procedure TLine2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  if not HasTransform then
   DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, VT)
  else
   DrawLine2D(Cnv, Points[0], Points[1], ClipRect2D, MultiplyTransform2D(ModelTransform, VT));
end;

function TLine2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                      var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform, False)]);
     Distance := MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TOutline2D
// =====================================================================

function TOutline2D.GetIsClosed: Boolean;
begin
  BeginUseProfilePoints;
  try
   Result := (ProfilePoints.Count > 2) and
              IsSamePoint2D(ProfilePoints[0], ProfilePoints[ProfilePoints.Count - 1]);
  finally
   EndUseProfilePoints;
  end;
end;

procedure TOutline2D.BeginUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do initialization here.
end;

procedure TOutline2D.EndUseProfilePoints;
begin
  // Due to the fact that ControlPoints are equal to ProfilePoints
  // there is no need to do finalization here.
end;

// =====================================================================
// TPolyline2D
// =====================================================================

constructor TPolyline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1);
  Points.AddPoints(Pts);
end;

procedure TPolyLine2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TLine2D) or (Obj is TPolyline2D) or (Obj is TPolygon2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, TPrimitive2D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

procedure TPolyLine2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  if not HasTransform then
   Points.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
  else
   Points.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
end;

function TPolyline2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                          var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform, False)]);
     Distance := MinValue([Aperture, TmpDist]);
   end;
end;

function TPolyline2D.GetProfilePoints: TPointsSet2D;
begin
  Result := Points;
end;

function TPolyline2D.GetNPts: Integer;
begin
  Result := Points.Count;
end;

// =====================================================================
// TPolygon2D
// =====================================================================

function TPolygon2D.GetIsClosed: Boolean;
begin
  Result := True; // Sempre chiuso.
end;

procedure TPolygon2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  { Draw the polygon. }
  if not HasTransform then
   Points.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
  else
   Points.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT))
end;

function TPolygon2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                         var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) then
   begin
     Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(Points.PointsReference, Points.Count, Pt, TmpDist, Aperture, ModelTransform)]);
     Distance := MinValue([Aperture, TmpDist]);
   end;
end;

// =====================================================================
// TCurve2D
// =====================================================================

procedure TCurve2D.SetPrimitiveSavingType(S: TPrimitiveSavingType);
begin
  if S <> fSavingType then
   begin
     fSavingType := S;
     UpdateExtension(Self);
   end;
end;

procedure TCurve2D.SetCurvePrecision(N: Word);
begin
  if fCurvePrecision <> N then
   fCurvePrecision := N;
end;

function TCurve2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if not Assigned(fCurvePoints) then
   fCurvePoints := TPointsSet2D.Create(N)
  else
   fCurvePoints.Clear;
  Inc(fCountReference);
  fCurvePoints.GrowingEnabled := True;
  Result := Rect2D(0, 0, 0, 0);
end;

procedure TCurve2D.FreeCurvePoints;
begin
  Dec(fCountReference);
  if fCountReference <= 0 then
   begin
     fCurvePoints.Free;
     fCurvePoints := nil;
     fCountReference := 0;
   end;
end;

constructor TCurve2D.Create(ID: LongInt; NPts: Integer; CurvePrec: Word);
begin
  inherited Create(ID, NPts);

  fCurvePrecision := CurvePrec;
  fCurvePoints := nil;
  fCountReference := 0;
  fSavingType := stTime;
end;

destructor TCurve2D.Destroy;
begin
  fCountReference := 0;
  FreeCurvePoints;
  inherited;
end;

procedure TCurve2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if Obj is TCurve2D then
   begin
     CurvePrecision := TCurve2D(Obj).fCurvePrecision;
     SavingType := TCurve2D(Obj).fSavingType;
   end;
end;

constructor TCurve2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  inherited;
  with Stream do
   begin
     Read(fCurvePrecision, SizeOf(fCurvePrecision));
     Read(fSavingType, SizeOf(fSavingType));
     fCountReference := 0;
   end;
end;

procedure TCurve2D.SaveToStream(const Stream: TStream);
begin
  inherited;
  with Stream do
   begin
     Write(fCurvePrecision, SizeOf(fCurvePrecision));
     Write(fSavingType, SizeOf(fSavingType));
   end;
end;

procedure TCurve2D._UpdateExtension;
begin
  if not Assigned(Points) or (Points.Count = 0) then
   Exit;
  case fSavingType of
   stSpace: begin
     WritableBox := PopulateCurvePoints(0);
     FreeCurvePoints;
   end;
   stTime: begin
     FreeCurvePoints;
     WritableBox := PopulateCurvePoints(0);
   end;
  end;
end;

procedure TCurve2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
    if Assigned(fCurvePoints) then
     begin
      if not HasTransform then
       fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
      else
       fCurvePoints.DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
     end;
  finally
    EndUseProfilePoints;
  end;
end;

function TCurve2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                       var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     BeginUseProfilePoints;
     try
       if not Assigned(fCurvePoints) then
        Exit;
       Result := MaxIntValue([PICK_INBBOX, IsPointOnPolyLine2D(fCurvePoints.PointsReference, fCurvePoints.Count, Pt, TmpDist, Aperture, ModelTransform, False)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
       EndUseProfilePoints;
     end;
   end;
end;

function TCurve2D.GetProfilePoints: TPointsSet2D;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints;
end;

function TCurve2D.GetNPts: Integer;
begin
  if not Assigned(fCurvePoints) then
   Raise ECADSysException.Create('TCurve2D: Call BeginUseProfilePoints before accessing the curve points.');
  Result := fCurvePoints.Count;
end;

procedure TCurve2D.BeginUseProfilePoints;
begin
  if fSavingType = stSpace then
   WritableBox := PopulateCurvePoints(0);
end;

procedure TCurve2D.EndUseProfilePoints;
begin
  if fSavingType = stSpace then
   FreeCurvePoints;
end;

// =====================================================================
// TArc2D
// =====================================================================

procedure TArc2D.GetArcParams(var CX, CY, RX, RY, SA, EA: TRealType);
var
  P1, P0: TPoint2D;
begin
  P0:= CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
  RX := Abs(P1.X - P0.X) / 2.0;
  RY := Abs(P1.Y - P0.Y) / 2.0;
  if Points.Count < 3 then
   Exit;
  P0:= CartesianPoint2D(Points[2]);
  P1 := CartesianPoint2D(Points[3]);
  case FDirection of
   adClockwise: begin
     SA := ArcTan2(CY - P0.Y, P0.X - CX);
     EA := ArcTan2(CY - P1.Y, P1.X - CX);
   end;
   adCounterClockwise: begin
     SA := ArcTan2(P0.Y - CY, P0.X - CX);
     EA := ArcTan2(P1.Y - CY, P1.X - CX);
   end;
  end;
end;

procedure TArc2D.SetStartAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fStartAngle <> A then
   begin
     fStartAngle := A;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[2] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TArc2D.SetEndAngle(A: TRealType);
var
  CX, RX, CY, RY, SA, EA: TRealType;
begin
  if fEndAngle <> A then
   begin
     fEndAngle := A;
     GetArcParams(CX, CY, RX, RY, SA, EA);
     Points[3] := Point2D(CX + RX * Cos(A), CY + RY * Sin(A));
   end;
end;

procedure TArc2D.SetArcDirection(D: TArcDirection);
begin
  if D <> FDirection then
   begin
     FDirection := D;
     UpdateExtension(Self);
   end;
end;

function TArc2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont, NPts: Integer;
  CX, RX, CY, RY: TRealType;
  Delta, CurrAngle: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  GetArcParams(CX, CY, RX, RY, fStartAngle, fEndAngle);
  // Calcola il numero di punti effetivi nella curva
  NPts := CurvePrecision;
  // Calcola il delta angolare tra due punti
  if fStartAngle < fEndAngle then
   Delta := (fEndAngle - fStartAngle) / (NPts + 1)
  else
   Delta := (TWOPI - fStartAngle + fEndAngle) / (NPts + 1);
  // Crea il vettore curvilineo.
  inherited PopulateCurvePoints(NPts + 1);
  // Popola il vettore curvilineo.
  if fDirection = adClockwise then
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY - RY * Sin(fEndAngle)));
   end
  else
   begin
     CurrAngle := fStartAngle;
     for Cont := 0 to NPts - 1 do
      begin
        ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY + RY * Sin(CurrAngle)));
        CurrAngle := CurrAngle + Delta
      end;
     ProfilePoints.Add(Point2D(CX + RX * Cos(fEndAngle), CY + RY * Sin(fEndAngle)));
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

{ Angles are in radiants. }
constructor TArc2D.Create(ID: LongInt; const P1, P2: TPoint2D; SA, EA: TRealType);
begin
  inherited Create(ID, 4, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.Add(Point2D(0, 0));
    Points.Add(Point2D(0, 0));
    fDirection := adClockwise;
    StartAngle := SA;
    EndAngle := EA;
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TArc2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) then
   begin
     fStartAngle := 0;
     fEndAngle := TWOPI;
     Points.DisableEvents := True;
     try
       Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
       Points.Add(Point2D(0, 0));
       Points.Add(Point2D(0, 0));
       Points.GrowingEnabled := False;
     finally
       Points.DisableEvents := False;
       UpdateExtension(Self);
     end;
   end
  else if Obj is TArc2D then
   begin
     fStartAngle := (Obj as TArc2D).StartAngle;
     fEndAngle := (Obj as TArc2D).EndAngle;
     fDirection := (Obj as TArc2D).Direction;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 3);
     Points.GrowingEnabled := False;
   end;
end;

constructor TArc2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  Points.DisableEvents := True;
  with Stream do
   try
     Read(FDirection, SizeOf(FDirection));
   finally
     Points.DisableEvents := False;
   end;
end;

procedure TArc2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fDirection, SizeOf(FDirection));
end;

// =====================================================================
// TFrame2D
// =====================================================================

function TFrame2D.PopulateCurvePoints(N: Word): TRect2D;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);

  ProfilePoints.Add(Points[0]);
  ProfilePoints.Add(Point2D(Points[0].X, Points[1].Y));
  ProfilePoints.Add(Points[1]);
  ProfilePoints.Add(Point2D(Points[1].X, Points[0].Y));
  ProfilePoints.Add(Points[0]);
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

constructor TFrame2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TFrame2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TFrame2D) or (Obj is TEllipse2D) or (Obj is TArc2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TRectangle2D
// =====================================================================

procedure TRectangle2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
   if not HasTransform then
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
   else
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
  finally
   EndUseProfilePoints;
  end;
end;

function TRectangle2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                           var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     { Consider all segments in the arc. }
     BeginUseProfilePoints;
     try
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(ProfilePoints.PointsReference, ProfilePoints.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
      EndUseProfilePoints;
     end;
   end;
end;

// =====================================================================
// TEllipse2D
// =====================================================================

procedure TEllipse2D.GetEllipseParams(var CX, CY, RX, RY: TRealType);
var
  P1, P0: TPoint2D;
begin
  P0:= CartesianPoint2D(Points[0]);
  P1 := CartesianPoint2D(Points[1]);
  RX := Abs(P1.X - P0.X) / 2.0 ;
  RY := Abs(P1.Y - P0.Y) / 2.0;
  CX := (P1.X + P0.X) / 2.0;
  CY := (P1.Y + P0.Y) / 2.0;
end;

function TEllipse2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
  Delta, CurrAngle, CX, RX, CY, RY: TRealType;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;

  inherited PopulateCurvePoints(CurvePrecision + 1);

  GetEllipseParams(CX, CY, RX, RY);
  Delta := TWOPI / CurvePrecision;
  ProfilePoints.Add(Point2D(CX + RX, CY));
  CurrAngle := Delta;
  for Cont := 1 to CurvePrecision - 1 do
   begin
     ProfilePoints.Add(Point2D(CX + RX * Cos(CurrAngle), CY - RY * Sin(CurrAngle)));
     CurrAngle := CurrAngle + Delta
   end;
  ProfilePoints.Add(Point2D(CX + RX, CY));
  Result := TransformBoundingBox2D(Points.Extension, ModelTransform);
end;

constructor TEllipse2D.Create(ID: LongInt; const P1, P2: TPoint2D);
begin
  inherited Create(ID, 2, 50);
  Points.DisableEvents := True;
  try
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TEllipse2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TEllipse2D) or (Obj is TFrame2D) or (Obj is TArc2D) then
   begin
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TFilledEllipse2D
// =====================================================================

procedure TFilledEllipse2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  BeginUseProfilePoints;
  try
   if not HasTransform then
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, VT)
   else
    ProfilePoints.DrawAsPolygon(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), Box, MultiplyTransform2D(ModelTransform, VT));
  finally
   EndUseProfilePoints;
  end;
end;

function TFilledEllipse2D.OnMe(Pt: TPoint2D; Aperture: TRealType;
                     var Distance: TRealType): Integer;
var
  TmpDist: TRealType;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   begin
     { Consider all segments in the arc. }
     BeginUseProfilePoints;
     try
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(ProfilePoints.PointsReference, ProfilePoints.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
      EndUseProfilePoints;
     end;
   end;
end;

// =====================================================================
// TBSpline2D
// =====================================================================

function Knot(I, K, N: Integer): Integer;
begin
  if I < K then
   Result := 0
  else if I > N then
   Result := N - K + 2
  else
   Result := I - K + 1;
end;

function NBlend(I, K, OK, N: Integer; U: TRealType): TRealType;
var
  T: Integer;
  V: TRealType;
begin
  if K = 1 then
   begin
     V := 0;
     if (Knot(i, OK, N) <= u) and (u < Knot(i + 1, OK, N)) then
      V := 1;
   end
  else
   begin
     V := 0;
     T := Knot(I + K - 1, OK, N) - Knot(I, OK, N);
     if T <> 0 then
      V := (U - Knot(I, OK, N)) * NBlend(I, K -1, OK, N, U) / T;
     T := Knot(I + K, OK, N) - Knot(I + 1, OK, N);
     if T <> 0 then
      V := V + (Knot(I + K, OK, N) - U) * NBlend(I + 1, K - 1, OK, N, U) / T;
   end;
  Result := V;
end;

function BSpline2D(U: TRealType; N, K: Integer; const Points: TPointsSet2D): TPoint2D;
var
  I: Integer;
  B: TRealType;
  TmpPt: TPoint2D;
begin
  Result := Point2D(0.0, 0.0);
  for I := 0 to N do
   begin
     B := NBlend(I, K, K, N, U);
     TmpPt := CartesianPoint2D(Points[I]);
     Result.X := Result.X + TmpPt.X * B;
     Result.Y := Result.Y + TmpPt.Y * B;
     Result.W := 1.0;
   end;
end;

function TBSpline2D.PopulateCurvePoints(N: Word): TRect2D;
var
  Cont: Integer;
begin
  if CurvePrecision = 0 then
   begin
     Result := Rect2D(0, 0, 0, 0);
     Exit;
   end;
  if Points.Count < FOrder then
   begin
     inherited PopulateCurvePoints(Points.Count);
     ProfilePoints.Copy(Points, 0, Points.Count - 1);
   end
  else
   begin
     inherited PopulateCurvePoints(CurvePrecision + 1);
     for Cont := 0 to CurvePrecision - 1 do
      ProfilePoints.Add(BSpline2D(Cont / CurvePrecision * (Points.Count - 2),
                                Points.Count - 1, FOrder, Points));
     ProfilePoints.Add(Points[Points.Count - 1]);
   end;
  Result := TransformBoundingBox2D(ProfilePoints.Extension, ModelTransform);
end;

constructor TBSpline2D.Create(ID: LongInt; const Pts: array of TPoint2D);
begin
  inherited Create(ID, High(Pts) - Low(Pts) + 1, 50);
  fOrder := 3; { The default spline is cubic. }
  Points.AddPoints(Pts);
  Points.GrowingEnabled := True;
end;

constructor TBSpline2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  with Stream do
   { Load the particular properties. }
   Read(fOrder, SizeOf(fOrder));
end;

procedure TBSpline2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   Write(fOrder, SizeOf(fOrder));
end;

procedure TBSpline2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TBSpline2D) then
   begin
     fOrder := TBSpline2D(Obj).fOrder;
     Points.Copy(TPrimitive2D(Obj).Points, 0, TPrimitive2D(Obj).Points.Count - 1);
     Points.GrowingEnabled := True;
   end;
end;

// =====================================================================
// TText2D
// =====================================================================

constructor TText2D.Create(ID: LongInt; Rect1: TRect2D; Height: TRealType;
                           Txt: AnsiString);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    fHeight := Height;
    fText := Txt;
    fRecalcBox := False;
    fDrawBox := False;
    fExtFont := TExtendedFont.Create;
    WritableBox := Rect1;
    fClippingFlags := 0;
    Points.Add(Rect1.FirstEdge);
    Points.Add(Rect1.SecondEdge);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

destructor TText2D.Destroy;
begin
  fExtFont.Free;
  inherited Destroy;
end;

procedure TText2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpBox: TRect2D;
  TmpHeight: Integer;
  TmpRect: TRect;
  TmpTransf: TTransf2D;
begin
  { Find the correct size. }
  TmpBox.FirstEdge := Point2D(0, 0);
  TmpBox.SecondEdge := Point2D(0, fHeight);
  TmpBox := TransformRect2D(TmpBox, VT);
  TmpHeight := Round(PointDistance2D(TmpBox.FirstEdge, TmpBox.SecondEdge));
  if TmpHeight <= 0 then
   Exit;
  { Build up the DrawText rect. }
  TmpRect := Rect2DToRect(TransformRect2D(Box, VT));
  FExtFont.Canvas := Cnv.Canvas;
  try
    FExtFont.Height := TmpHeight;
    if fRecalcBox then
     begin
       LCLIntf.DrawText(Cnv.Canvas.Handle, PChar(FText), Length(FText), TmpRect, DT_CALCRECT);
       if not HasTransform then
        TmpTransf := VT
       else
        TmpTransf := MultiplyTransform2D(ModelTransform, VT);
       TmpBox := TransformBoundingBox2D(RectToRect2D(TmpRect), InvertTransform2D(TmpTransf));
       Points.DisableEvents := True;
       try
         Points[0] := TmpBox.FirstEdge;
         Points[1] := TmpBox.SecondEdge;
       finally
         Points.DisableEvents := False;
         UpdateExtension(Self);
       end;
     end;
    if (Cnv.Canvas.Pen.Mode <> pmXor) then
     LCLIntf.DrawText(Cnv.Canvas.Handle, PChar(FText), Length(FText), TmpRect, FClippingFlags);
    if FDrawBox or (Cnv.Canvas.Pen.Mode = pmXor) then
     DrawRect2DAsPolyline(Cnv, Box, RectToRect2D(Cnv.Canvas.ClipRect), IdentityTransf2D, VT);
  finally
    FExtFont.Canvas := nil;
  end;
end;

function TText2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if Result = PICK_INBBOX then
   Result := PICK_INOBJECT;
end;

constructor TText2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     Read(TmpInt, SizeOf(TmpInt));
     SetString(FText, nil, TmpInt);
     Read(Pointer(FText)^, TmpInt);
     FExtFont := TExtendedFont.Create;
     FExtFont.LoadFromStream(Stream);
     Read(FClippingFlags, SizeOf(FClippingFlags));
     Read(FDrawBox, SizeOf(FDrawBox));
     Read(fHeight, SizeOf(fHeight))
   end;
end;

procedure TText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  with Stream do
   begin
     TmpInt := Length(FText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(FText)^, Length(FText));
     FExtFont.SaveToStream(Stream);
     Write(FClippingFlags, SizeOf(FClippingFlags));
     Write(FDrawBox, SizeOf(FDrawBox));
     Write(fHeight, SizeOf(fHeight));
   end;
end;

procedure TText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TText2D) or (Obj is TFrame2D) then
   begin
     if Obj is TText2D then
      begin
        fText := (Obj as TText2D).Text;
        fHeight := (Obj as TText2D).Height;
        fDrawBox := (Obj as TText2D).DrawBox;
        fRecalcBox := (Obj as TText2D).fRecalcBox;
        fClippingFlags := (Obj as TText2D).ClippingFlags;
        if not Assigned(FExtFont) then
         FExtFont := TExtendedFont.Create;
        fExtFont.Assign(TText2D(Obj).fExtFont);
      end;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := False;
   end;
end;

// =====================================================================
// TBitmap2D
// =====================================================================

procedure TBitmap2D.SetScaleFactor(SF: TRealType);
var
  TmpPt: TPoint2D;
begin
  if( fScaleFactor <> SF ) then
   begin
     fScaleFactor := SF;
     if( fScaleFactor <> 0.0 ) then
      begin
        if( fAspectRatio <> 0.0 ) then
         TmpPt.X := Points[0].X + fBitmap.Height * fScaleFactor / fAspectRatio
        else
         TmpPt.X := Points[0].X + fBitmap.Width * fScaleFactor;
        TmpPt.Y := Points[0].Y + fBitmap.Height * fScaleFactor;
        TmpPt.W := 1.0;
        Points[1] := TmpPt;
      end;
   end;
end;

procedure TBitmap2D.SetAspectRatio(AR: TRealType);
var
  TmpPt: TPoint2D;
begin
  if( fAspectRatio <> AR ) then
   begin
     fAspectRatio := AR;
     if( fScaleFactor <> 0.0 ) then
      begin
        if( fAspectRatio <> 0.0 ) then
         TmpPt.X := Points[0].X + fBitmap.Height * fScaleFactor / fAspectRatio
        else
         TmpPt.X := Points[0].X + fBitmap.Width * fScaleFactor;
        TmpPt.Y := Points[0].Y + fBitmap.Height * fScaleFactor;
        TmpPt.W := 1.0;
        Points[1] := TmpPt;
      end;
   end;
end;

constructor TBitmap2D.Create(ID: LongInt; const P1, P2: TPoint2D; Bmp: TBitmap);
begin
  inherited Create(ID, 2);
  fScaleFactor := 0.0;
  fAspectRatio := 0.0;
  fCopyMode := cmSrcCopy;
  Points.DisableEvents := True;
  try
    fBitmap := TBitmap.Create;
    fBitmap.Assign(Bmp);
    Points.Add(P1);
    Points.Add(P2);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
    UpdateExtension(Self);
  end;
end;

procedure TBitmap2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited;
  if (Obj is TBitmap2D) or (Obj is TFrame2D) then
   begin
     fScaleFactor := TBitmap2D(Obj).ScaleFactor;
     fAspectRatio := TBitmap2D(Obj).AspectRatio;
     fCopyMode := TBitmap2D(Obj).CopyMode;
     if Obj is TBitmap2D then
      fBitmap.Assign(TBitmap2D(Obj).fBitmap);
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
     Points.GrowingEnabled := True;
   end;
end;

destructor TBitmap2D.Destroy;
begin
  fBitmap.Free;
  inherited Destroy;
end;

constructor TBitmap2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
begin
  { Load the standard properties }
  inherited;
  fBitmap := TBitmap.Create;
  fBitmap.LoadFromStream(Stream);
  if( Version >= 'CAD422' ) then
   begin
     Stream.Read(fScaleFactor, SizeOf(fScaleFactor));
     Stream.Read(fAspectRatio, SizeOf(fAspectRatio));
     Stream.Read(fCopyMode, SizeOf(fCopyMode));
   end;
end;

procedure TBitmap2D.SaveToStream(const Stream: TStream);
begin
  { Save the standard properties }
  inherited SaveToStream(Stream);
  fBitmap.SaveToStream(Stream);
  Stream.Write(fScaleFactor, SizeOf(fScaleFactor));
  Stream.Write(fAspectRatio, SizeOf(fAspectRatio));
  Stream.Write(fCopyMode, SizeOf(fCopyMode));
end;

procedure TBitmap2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
var
  TmpPt1, TmpPt2: TPoint2D;
  TmpTransf: TTransf2D;
  TmpRect: TRect;
  OldMode: TCopyMode;
begin
  if not HasTransform then
   TmpTransf := VT
  else
   TmpTransf := MultiplyTransform2D(ModelTransform, VT);
  TmpPt1 := TransformPoint2D(Points[0], TmpTransf);
  TmpPt2 := TransformPoint2D(Points[1], TmpTransf);
  TmpRect := Rect2DToRect(Rect2D(TmpPt1.X, TmpPt1.Y, TmpPt2.X, TmpPt2.Y));
  OldMode := Cnv.Canvas.CopyMode;
  Cnv.Canvas.CopyMode := fCopyMode;
  Cnv.Canvas.StretchDraw(TmpRect, fBitmap);
  Cnv.Canvas.CopyMode := OldMode;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectChar.GetVect(Idx: Integer): TPointsSet2D;
begin
  Result := TPointsSet2D(fSubVects[Idx]);
end;

function TVectChar.GetVectCount: Integer;
begin
  Result := fSubVects.NumberOfObjects;
end;

constructor TVectChar.Create(NSubVect: Integer);
var
  Cont: Integer;
begin
  inherited Create;

  fSubVects := TIndexedObjectList.Create(NSubVect);
  fSubVects.FreeOnClear := True;
  for Cont := 0 to NSubVect - 1 do
   fSubVects.Objects[Cont] := TPointsSet2D.Create(0);
end;

destructor TVectChar.Destroy;
begin
  fSubVects.Free;

  inherited;
end;

procedure TVectChar.UpdateExtension;
var
  Cont: Integer;
begin
  fExtension := Rect2D(0.0, 0.0, 0.0, 0.0);
  for Cont := 0 to fSubVects.NumberOfObjects - 1 do
   fExtension := BoxOutBox2D(fExtension, TPointsSet2D(fSubVects.Objects[Cont]).Extension);
end;

constructor TVectChar.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
begin
  inherited Create;
  if( Version >= 'CAD4  ' ) then
   with Stream do
    begin
      Read(TmpInt, SizeOf(TmpInt));
      fSubVects := TIndexedObjectList.Create(TmpInt);
      // Lettura vettori.
      for Cont := 0 to fSubVects.NumberOfObjects - 1 do
       begin
         Read(TmpWord, SizeOf(TmpWord));
         fSubVects.Objects[Cont] := TPointsSet2D.Create(TmpWord);
         while TmpWord > 0 do
          begin
            Read(TmpPt, SizeOf(TmpPt));
            TPointsSet2D(fSubVects.Objects[Cont]).Add(TmpPt);
            Dec(TmpWord);
          end;
       end;
      UpdateExtension(Self);
    end;
end;

procedure TVectChar.SaveToStream(const Stream: TStream);
var
  TmpInt, Cont: Integer;
  TmpWord: Word;
  TmpPt: TPoint2D;
begin
  with Stream do
   begin
     TmpInt := fSubVects.NumberOfObjects;
     Write(TmpInt, SizeOf(TmpInt));
     // Scrittura vettori.
     for Cont := 0 to fSubVects.NumberOfObjects - 1 do
      begin
        TmpWord := TPointsSet2D(fSubVects.Objects[Cont]).Count;
        Write(TmpWord, SizeOf(TmpWord));
        while TmpWord > 0 do
         begin
           TmpPt := TPointsSet2D(fSubVects.Objects[Cont]).Points[TmpWord - 1];
           Write(TmpPt, SizeOf(TmpPt));
           Dec(TmpWord);
         end;
      end;
   end;
end;

// =====================================================================
// TVectFont
// =====================================================================

function TVectFont.GetChar(Ch: Char): TVectChar;
begin
  Result := TVectChar(fVects[Ord(Ch)]);
end;

constructor TVectFont.Create;
begin
  inherited;

  // Al massimo ci sono 256 caratteri.
  fVects := TIndexedObjectList.Create(256);
  fVects.FreeOnClear := True;
end;

destructor TVectFont.Destroy;
begin
  fVects.Free;

  inherited;
end;

constructor TVectFont.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  inherited Create;
  with Stream do
   begin
     fVects := TIndexedObjectList.Create(256);
     // Lettura caratteri.
     while True do
      begin
        // Lettura numero carattere.
        Read(TmpInt, SizeOf(TmpInt));
        if TmpInt = -1 then
         Break; // Fine.
        fVects[TmpInt] := TVectChar.CreateFromStream(Stream, Version);
      end;
   end;
end;

procedure TVectFont.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  with Stream do
   begin
     // Scrittura caratteri.
     for TmpInt := 0 to fVects.NumberOfObjects - 1 do
      if fVects[TmpInt] <> nil then
       begin
         // Scrittura numero carattere.
         Write(TmpInt, SizeOf(TmpInt));
         TVectChar(fVects[TmpInt]).SaveToStream(Stream);
       end;
     // Fine
     TmpInt := -1;
     Write(TmpInt, SizeOf(TmpInt));
   end;
end;

function TVectFont.CreateChar(Ch: Char; N: Integer): TVectChar;
begin
  if fVects[Ord(Ch)] <> nil then
   fVects[Ord(Ch)].Free;
  Result := TVectChar.Create(N);
  fVects[Ord(Ch)] := Result;
end;

procedure TVectFont.DrawChar2D(Ch: Char; var DrawPoint: TPoint2D; const H, ICS: TRealType; const VT: TTransf2D; Cnv: TDecorativeCanvas);
var
  Cont: Integer;
  TmpTransf: TTransf2D;
  TmpExt: TRect2D;
  TmpCh: TVectChar;
begin
  TmpCh := nil;
  if (Ch = ' ') then
   begin
     DrawPoint.X := DrawPoint.X + (0.4 + ICS) * H;
     Exit;
   end
  else if fVects[Ord(Ch)] <> nil then
   TmpCh := GetChar(Ch)
  else if (Ch <> #13) then
   TmpCh := _NullChar;
  if TmpCh <> nil then
   begin
     TmpTransf := IdentityTransf2D;
     TmpTransf[1, 1] := H;
     TmpTransf[2, 2] := H;
     TmpTransf[3, 1] := DrawPoint.X;
     TmpTransf[3, 2] := DrawPoint.Y;
     TmpTransf[3, 3] := DrawPoint.W;
     with TmpCh do
      begin
        TmpExt := Extension;
        for Cont := 0 to VectorCount - 1 do
         Vectors[Cont].DrawAsPolyline(Cnv, RectToRect2D(Cnv.Canvas.ClipRect), TmpExt, MultiplyTransform2D(TmpTransf, VT));
        DrawPoint.X := DrawPoint.X + (TmpExt.Right + ICS) * H;
      end;
   end;
end;

function TVectFont.GetTextExtension(Str: AnsiString; H, InterChar, InterLine: TRealType): TRect2D;
var
  Cont: Integer;
  RowLen, RowHeight, MaxRowLen: TRealType;
begin
  // Per i caratteri con la gambetta (come g) parto con Bottom = 1.0
  Result := Rect2D(0.0, 1.0, 0.0, H);
  MaxRowLen := 0.0;
  RowLen := 0.0;
  RowHeight := 0.0;
  for Cont := 1 to Length(Str) do
   begin
     if fVects[Ord(Str[Cont])] <> nil then
      with GetChar(Str[Cont]).Extension do
       begin
         RowLen := RowLen + (Right + InterChar);
         // Bottom contiene l'eventuale gambetta. 
         RowHeight := MaxValue([RowHeight, 1.0 - Bottom]);
       end
     else if Str[Cont] = ' ' then
      // Space.
      RowLen := RowLen + (0.5 + InterChar)
     else if Str[Cont] <> #13 then
      // Carattere nullo.
      with _NullChar.Extension do
       RowLen := RowLen + (Right + InterChar);
     if Str[Cont] = #13 then
      begin
        // New line. L'altezza è 1.3 per via delle gambette.
        MaxRowLen := MaxValue([MaxRowLen, RowLen - InterChar]);
        Result.Bottom := Result.Bottom - (InterLine + RowHeight);
        RowLen := 0.0;
        RowHeight := 0.0;
      end;
   end;
  MaxRowLen := MaxValue([MaxRowLen, RowLen - InterChar]);
  Result.Left := 0.0;
  Result.Bottom := (Result.Bottom - RowHeight) * H;
  Result.Right := MaxRowLen * H;
end;

// =====================================================================
// Registration functions
// =====================================================================

function CADSysFindFontIndex(const Font: TVectFont): Word;
var
  Cont: Integer;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   if Assigned(VectFonts2DRegistered[Cont]) and
      (VectFonts2DRegistered[Cont] = Font) then
    begin
      Result := Cont;
      Exit;
    end;
  Raise ECADObjClassNotFound.Create('CADSysFindFontIndex: Font not found');
end;

function CADSysFindFontByIndex(Index: Word): TVectFont;
begin
  if not Assigned(VectFonts2DRegistered[Index]) then
   begin
     if Assigned(_DefaultFont) then
      Result := _DefaultFont
     else
      Raise ECADObjClassNotFound.Create('CADSysFindFontByIndex: Font not registered');
   end
  else
   Result := VectFonts2DRegistered[Index];
end;

procedure CADSysRegisterFont(Index: Word; const Font: TVectFont);
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysRegisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFont: Font index already allocated');
  VectFonts2DRegistered[Index] := Font;
end;

procedure CADSysUnregisterFont(Index: Word);
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysUnregisterFont: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   begin
     VectFonts2DRegistered[Index].Free;
     VectFonts2DRegistered[Index] := nil;
   end;
end;

procedure CADSysRegisterFontFromFile(Index: Word; const FileName: String);
var
  TmpStream: TFileStream;
begin
  if Index > MAX_REGISTERED_FONTS then
   Raise ECADOutOfBound.Create('CADSysRegisterFontFromFile: Out of bound registration index');
  if Assigned(VectFonts2DRegistered[Index]) then
   Raise ECADObjClassNotFound.Create('CADSysRegisterFontFromFile: Font index already allocated');
  TmpStream := TFileStream.Create(FileName, fmOpenRead);
  try
   VectFonts2DRegistered[Index] := TVectFont.CreateFromStream(TmpStream, CADSysVersion);
  finally
   TmpStream.Free;
  end;
end;

procedure CADSysInitFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   VectFonts2DRegistered[Cont] := nil;
end;

procedure CADSysClearFontList;
var
  Cont: Word;
begin
  for Cont := 0 to MAX_REGISTERED_FONTS do
   if Assigned(VectFonts2DRegistered[Cont]) then
    VectFonts2DRegistered[Cont].Free;
end;

function CADSysGetDefaultFont: TVectFont;
begin
  Result := _DefaultFont;
end;

procedure CADSysSetDefaultFont(const Font: TVectFont);
begin
  _DefaultFont := Font;
end;

// =====================================================================
// TJustifiedVectText2D
// =====================================================================

procedure TJustifiedVectText2D.SetHeight(H: TRealType);
begin
  fHeight := H;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetCharSpace(S: TRealType);
begin
  fCharSpace := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetInterLine(S: TRealType);
begin
  fInterLine := S;
  UpdateExtension(Self);
end;

procedure TJustifiedVectText2D.SetText(T: String);
begin
  fText := T;
  UpdateExtension(Self);
end;

function TJustifiedVectText2D.GetTextExtension: TRect2D;
var
  TmpRect: TRect2D;
  CurrRect: TRect2D;
  DX, TX, TY: TRealType;
begin
  if Assigned(fVectFont) then
   TmpRect := fVectFont.GetTextExtension(fText, fHeight, fCharSpace, fInterLine)
  else
   TmpRect := Rect2D(0, 0, 0, 0);
  CurrRect.FirstEdge := Points[0];
  CurrRect.SecondEdge := Points[1];
  CurrRect := ReorderRect2D(CurrRect);
  fBasePoint := Point2D(CurrRect.Left, CurrRect.Top - TmpRect.Top);
  DX := TmpRect.Right - TmpRect.Left;
  case fHJustification of
   jhLeft: TX := CurrRect.Left;
   jhRight: TX := CurrRect.Right - DX;
   jhCenter: TX := (CurrRect.Left + CurrRect.Right - DX) / 2.0;
  else
   TX := CurrRect.Left;
  end;
  case fVJustification of
   jvTop: TY := CurrRect.Top - TmpRect.Top;
   jvBottom: TY := CurrRect.Bottom - TmpRect.Bottom;
   jvCenter: TY := (CurrRect.Top + CurrRect.Bottom) / 2.0;
  else
   TY := CurrRect.Top - TmpRect.Top;
  end;
  Result := TransformRect2D(TmpRect, Translate2D(TX, TY));
end;

procedure TJustifiedVectText2D.DrawText(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const DrawMode: Integer);
  procedure DrawTextLine(BasePt: TPoint2D; Str: String; ObjTransf: TTransf2D);
  var
    Cont: Integer;
  begin
    for Cont := 1 to Length(Str) do
     // Disegno il carattere.
     fVectFont.DrawChar2D(Str[Cont], BasePt, fHeight, fCharSpace, ObjTransf, Cnv);
  end;
var
  TmpTransf: TTransf2D;
  TmpStr, TmpRow: String;
  CurrBasePt: TPoint2D;
  TmpPos: Integer;
  TmpTExt: TRect2D;
begin
  if not Assigned(fVectFont) then
   Exit;
  // sposto il testo, applico la trasformazione oggetto al testo e trasformo nel viewport.
  TmpTransf := MultiplyTransform2D(ModelTransform, VT);
  try
    TmpTExt := GetTextExtension;
    if fDrawBox then
     DrawRect2DAsPolyline(Cnv, TmpTExt, RectToRect2D(Cnv.Canvas.ClipRect), ModelTransform, VT);
    if DrawMode and DRAWMODE_VECTTEXTONLYBOX = DRAWMODE_VECTTEXTONLYBOX then
     Exit;
    CurrBasePt.X := TmpTExt.Left;
    CurrBasePt.Y := TmpTExt.Top - fHeight;
    CurrBasePt.W := 1.0;
    // Estraggo le righe.
    TmpStr := fText;
    TmpPos := Pos(#13, TmpStr);
    while TmpPos > 0 do
     begin
       TmpRow := Copy(TmpStr, 1, TmpPos - 1);
       Delete(TmpStr, 1, TmpPos);
       if TmpStr[1] = #10 then
        Delete(TmpStr, 1, 1);
       TmpPos := Pos(#13, TmpStr);
       // Draw the string.
       TmpTExt := fVectFont.GetTextExtension(TmpRow, fHeight, fCharSpace, fInterLine);
       DrawTextLine(CurrBasePt, TmpRow, TmpTransf);
       CurrBasePt.Y := CurrBasePt.Y - (TmpTExt.Top - TmpTExt.Bottom) - fHeight * fInterLine;
     end;
    // Draw the string.
    DrawTextLine(CurrBasePt, TmpStr, TmpTransf);
  finally
  end;
end;

constructor TJustifiedVectText2D.Create(ID: LongInt; FontVect: TVectFont; TextBox: TRect2D; Height: TRealType; Txt: AnsiString);
begin
  inherited Create(ID, 2);
  Points.DisableEvents := True;
  try
    Points.Add(TextBox.FirstEdge);
    Points.Add(TextBox.SecondEdge);
    Points.GrowingEnabled := False;
  finally
    Points.DisableEvents := False;
  end;

  fHeight := Height;
  fCharSpace := 0.1;
  fInterLine := 0.02;
  fText := Txt;
  fDrawBox := False;
  fVectFont := FontVect;
  fHJustification := jhLeft;
  fVJustification := jvTop;

  UpdateExtension(Self);
end;

constructor TJustifiedVectText2D.CreateFromStream(const Stream: TStream; const Version: TCADVersion);
var
  TmpInt: Integer;
begin
  { Load the standard properties }
  inherited;
  with Stream do
   begin
     Read(TmpInt, SizeOf(TmpInt));
     SetString(fText, nil, TmpInt);
     Read(Pointer(fText)^, TmpInt);
     // Lettura indice font.
     Read(TmpInt, SizeOf(TmpInt));
     try
      fVectFont := CADSysFindFontByIndex(TmpInt);
     except
      on ECADObjClassNotFound do
       begin
         ShowMessage('Font class not found. Font not assigned');
         fVectFont := nil;
       end;
     end;
     Read(fHJustification, SizeOf(fHJustification));
     Read(fVJustification, SizeOf(fVJustification));
     Read(fDrawBox, SizeOf(fDrawBox));
     Read(fHeight, SizeOf(fHeight));
     Read(fInterLine, SizeOf(fInterLine));
     Read(fCharSpace, SizeOf(fCharSpace));
   end;
end;

procedure TJustifiedVectText2D.Assign(const Obj: TGraphicObject);
begin
  if (Obj = Self) then
   Exit;
  inherited Assign(Obj);
  if (Obj is TJustifiedVectText2D) or (Obj is TText2D) then
   begin
     if not Assigned(Points) then
      begin
        Points := CreateVect(2);
        Points.GrowingEnabled := False;
        Points.OnChange := UpdateExtension;
      end;
     if Obj is TJustifiedVectText2D then
      begin
        fText := (Obj as TJustifiedVectText2D).Text;
        fHeight := (Obj as TJustifiedVectText2D).Height;
        fInterLine := (Obj as TJustifiedVectText2D).InterLine;
        fCharSpace := (Obj as TJustifiedVectText2D).CharSpace;
        fDrawBox := (Obj as TJustifiedVectText2D).DrawBox;
        fVectFont := (Obj as TJustifiedVectText2D).fVectFont;
        fHJustification := (Obj as TJustifiedVectText2D).fHJustification;
        fVJustification := (Obj as TJustifiedVectText2D).fVJustification;
      end
     else if Obj is TText2D then
      begin
        fText := (Obj as TText2D).Text;
        fHeight := (Obj as TText2D).Height;
        fDrawBox := (Obj as TText2D).DrawBox;
      end;
     Points.Clear;
     Points.Copy(TPrimitive2D(Obj).Points, 0, 1);
   end;
end;

procedure TJustifiedVectText2D.SaveToStream(const Stream: TStream);
var
  TmpInt: Integer;
begin
  { Save the standard properties }
  inherited;
  with Stream do
   begin
     TmpInt := Length(fText);
     Write(TmpInt, SizeOf(TmpInt));
     Write(Pointer(fText)^, TmpInt);
     // Scrittura indice font.
     TmpInt := CADSysFindFontIndex(fVectFont);
     Write(TmpInt, SizeOf(TmpInt));
     Write(fHJustification, SizeOf(fHJustification));
     Write(fVJustification, SizeOf(fVJustification));
     Write(fDrawBox, SizeOf(fDrawBox));
     Write(fHeight, SizeOf(fHeight));
     Write(fInterLine, SizeOf(fInterLine));
     Write(fCharSpace, SizeOf(fCharSpace));
   end;
end;

procedure TJustifiedVectText2D.Draw(const VT: TTransf2D; const Cnv: TDecorativeCanvas; const ClipRect2D: TRect2D; const DrawMode: Integer);
begin
  DrawText(VT, Cnv, DrawMode);
end;

function TJustifiedVectText2D.OnMe(Pt: TPoint2D; Aperture: TRealType; var Distance: TRealType): Integer;
var
  TmpVect: TPointsSet2D;
  TmpDist: TRealType;
  TmpBox: TRect2D;
begin
  Result := inherited OnMe(Pt, Aperture, Distance);
  if (Result = PICK_INBBOX) then
   begin
     TmpBox := GetTextExtension;
     TmpVect := TPointsSet2D.Create(4);
     try
       TmpVect.Add(TmpBox.FirstEdge);
       TmpVect.Add(Point2D(TmpBox.Left, TmpBox.Top));
       TmpVect.Add(TmpBox.SecondEdge);
       TmpVect.Add(Point2D(TmpBox.Right, TmpBox.Bottom));
       Result := MaxIntValue([PICK_INBBOX, IsPointInPolygon2D(TmpVect.PointsReference, TmpVect.Count, Pt, TmpDist, Aperture, ModelTransform)]);
       Distance := MinValue([Aperture, TmpDist]);
     finally
       TmpVect.Free;
     end;
   end;
end;

procedure TJustifiedVectText2D._UpdateExtension;
begin
  inherited;
  WritableBox := TransformBoundingBox2D(GetTextExtension, ModelTransform);
end;

initialization
  CADSysRegisterClass(3, TLine2D);
  CADSysRegisterClass(4, TPolyline2D);
  CADSysRegisterClass(5, TPolygon2D);
  CADSysRegisterClass(6, TRectangle2D);
  CADSysRegisterClass(7, TArc2D);
  CADSysRegisterClass(8, TEllipse2D);
  CADSysRegisterClass(9, TFilledEllipse2D);
  CADSysRegisterClass(10, TText2D);
  CADSysRegisterClass(11, TFrame2D);
  CADSysRegisterClass(12, TBitmap2D);
  CADSysRegisterClass(13, TBSpline2D);
  CADSysRegisterClass(14, TJustifiedVectText2D);

  _DefaultHandler2D := TPrimitive2DHandler.Create(nil);

  // Vectorial fonts
  CADSysInitFontList;

  _NullChar := TVectChar.Create(1);
  _NullChar.Vectors[0].Add(Point2D(0.0, 0.0));
  _NullChar.Vectors[0].Add(Point2D(0.8, 0.0));
  _NullChar.UpdateExtension(nil);
  _DefaultFont := nil;
finalization
  CADSysClearFontList;
  _NullChar.Free;
  _DefaultHandler2D.Free;
end.

