unit cadsysreg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

uses
  CADSys4;

{$R cadsys.dcr}

procedure Register;
begin
  RegisterComponents('CADSys 4.2', [TRuler, TCADCmp2D, TCADViewport2D, TCADPrg2D]);
end;

end.

