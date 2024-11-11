unit uPSI_dynmatrix;

{$mode objfpc}{$H+}

{
This file has been generated by UnitParser v0.7, written by M. Knight
and updated by NP. v/d Spek and George Birbilis. 
Source Code from Carlo Kok has been used to implement various sections of
UnitParser. Components of ROPS are used in the construction of UnitParser,
code implementing the class wrapper is taken from Carlo Kok's conv utility

}
interface
 
uses
   SysUtils
  ,Classes
  ,uPSComponent
  ,uPSRuntime
  ,uPSCompiler
  ;
 
type 
(*----------------------------------------------------------------------------*)
  TPSImport_dynmatrix = class(TPSPlugin)
  protected
    procedure CompileImport1(CompExec: TPSScript); override;
    procedure ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter); override;
  end;
 
 
{ compile-time registration functions }
procedure SIRegister_dynmatrix(CL: TPSPascalCompiler);

{ run-time registration functions }
procedure RIRegister_dynmatrix_Routines(S: TPSExec);

procedure Register;

implementation


uses
   dynmatrix
  ;
 
 
procedure Register;
begin
  RegisterComponents('Pascal Script', [TPSImport_dynmatrix]);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_dynmatrix(CL: TPSPascalCompiler);
begin
  CL.AddTypeS('Matrix', 'record data : array of double; rows : Longword; cols :'
   +' Longword; end');
 CL.AddDelphiFunction('Procedure MInit( var Mat : Matrix; newrows, newcols : Longword)');
 CL.AddDelphiFunction('Procedure MTestData( const Mat : Matrix; out NRows, NCols : Longword)');
 CL.AddDelphiFunction('Procedure MSetSize( var Mat : Matrix; newrows, newcols : Longword)');
 CL.AddDelphiFunction('Procedure Msetv( var Mat : Matrix; r, c : Longword; v : double)');
 CL.AddDelphiFunction('Function Mgetv( const Mat : Matrix; r, c : Longword) : double');
 CL.AddDelphiFunction('Procedure MUsetv( var Mat : Matrix; r, c : Longword; v : double)');
 CL.AddDelphiFunction('Function MUgetv( const Mat : Matrix; r, c : Longword) : double');
 CL.AddDelphiFunction('Function MIsGood( const Mat : Matrix) : boolean');
 CL.AddDelphiFunction('Function MNumCols( const Mat : Matrix) : Longword');
 CL.AddDelphiFunction('Function MNumRows( const Mat : Matrix) : Longword');
 CL.AddDelphiFunction('Function Mzeros( numrows, numcols : LongWord) : Matrix');
 CL.AddDelphiFunction('Function Meye( n : Longword) : Matrix');
 CL.AddDelphiFunction('Function Mrandom( numrows, numcols : LongWord) : Matrix');
 CL.AddDelphiFunction('Function Minc( numrows, numcols : LongWord) : Matrix');
 CL.AddDelphiFunction('Procedure ArrayToMatrix( var M : Matrix; const D : array of double)');
 CL.AddDelphiFunction('Function Madd( const A, B : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MaddReal( const A : Matrix; k : double) : Matrix');
 CL.AddDelphiFunction('Function Mneg( const A : Matrix) : Matrix');
 CL.AddDelphiFunction('Function Msub( const A, B : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MsubReal( const A : Matrix; k : double) : Matrix');
 CL.AddDelphiFunction('Function MmultReal( const A : Matrix; k : double) : Matrix');
 CL.AddDelphiFunction('Function Mmult( const A, B : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MPower( const M : Matrix; const n : integer) : Matrix');
 CL.AddDelphiFunction('Function Mtran( const M : Matrix) : Matrix');
 CL.AddDelphiFunction('Function Minv( const M : Matrix) : Matrix');
 CL.AddDelphiFunction('Function Minv_fast( const M : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MelementMult( const A, B : Matrix) : Matrix');
 CL.AddDelphiFunction('Function Mmin( const M : Matrix) : double');
 CL.AddDelphiFunction('Function Mmax( const M : Matrix) : double');
 CL.AddDelphiFunction('Function MmaxAbs( const M : Matrix) : double');

 CL.AddDelphiFunction('function MallNorm(const M: Matrix): double');

 CL.AddDelphiFunction('Function MHflip( const M : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MConv( const A, B : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MCrop( const M : Matrix; uprow, leftcol, downrow, rightcol : Longword) : Matrix');
 CL.AddDelphiFunction('Function MOneCol( const M : Matrix; col : Longword) : Matrix');
 CL.AddDelphiFunction('Function MOneRow( const M : Matrix; row : Longword) : Matrix');
 CL.AddDelphiFunction('Function MStamp( const M, S : Matrix; drow, dcol : Longword) : Matrix');
 CL.AddDelphiFunction('Function MStampCol( const M, S : Matrix; col : Longword) : Matrix');
 CL.AddDelphiFunction('Function MStampRow( const M, S : Matrix; row : Longword) : Matrix');
 CL.AddDelphiFunction('Function MColsum( const M : Matrix) : Matrix');
 CL.AddDelphiFunction('Function MRowsum( const M : Matrix) : Matrix');
 CL.AddDelphiFunction('Function Mload( const fname : string) : Matrix');
 CL.AddDelphiFunction('Procedure Msave( M : Matrix; const fname : string)');
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure RIRegister_dynmatrix_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@MInit, 'MInit', cdRegister);
 S.RegisterDelphiFunction(@MTestData, 'MTestData', cdRegister);
 S.RegisterDelphiFunction(@MSetSize, 'MSetSize', cdRegister);
 S.RegisterDelphiFunction(@Msetv, 'Msetv', cdRegister);
 S.RegisterDelphiFunction(@Mgetv, 'Mgetv', cdRegister);
 S.RegisterDelphiFunction(@MUsetv, 'MUsetv', cdRegister);
 S.RegisterDelphiFunction(@MUgetv, 'MUgetv', cdRegister);
 S.RegisterDelphiFunction(@MIsGood, 'MIsGood', cdRegister);
 S.RegisterDelphiFunction(@MNumCols, 'MNumCols', cdRegister);
 S.RegisterDelphiFunction(@MNumRows, 'MNumRows', cdRegister);
 S.RegisterDelphiFunction(@Mzeros, 'Mzeros', cdRegister);
 S.RegisterDelphiFunction(@Meye, 'Meye', cdRegister);
 S.RegisterDelphiFunction(@Mrandom, 'Mrandom', cdRegister);
 S.RegisterDelphiFunction(@Minc, 'Minc', cdRegister);
 S.RegisterDelphiFunction(@ArrayToMatrix, 'ArrayToMatrix', cdRegister);
 S.RegisterDelphiFunction(@Madd, 'Madd', cdRegister);
 S.RegisterDelphiFunction(@MaddReal, 'MaddReal', cdRegister);
 S.RegisterDelphiFunction(@Mneg, 'Mneg', cdRegister);
 S.RegisterDelphiFunction(@Msub, 'Msub', cdRegister);
 S.RegisterDelphiFunction(@MsubReal, 'MsubReal', cdRegister);
 S.RegisterDelphiFunction(@MmultReal, 'MmultReal', cdRegister);
 S.RegisterDelphiFunction(@Mmult, 'Mmult', cdRegister);
 S.RegisterDelphiFunction(@MPower, 'MPower', cdRegister);
 S.RegisterDelphiFunction(@Mtran, 'Mtran', cdRegister);
 S.RegisterDelphiFunction(@Minv, 'Minv', cdRegister);
 S.RegisterDelphiFunction(@Minv_fast, 'Minv_fast', cdRegister);
 S.RegisterDelphiFunction(@MelementMult, 'MelementMult', cdRegister);
 S.RegisterDelphiFunction(@Mmin, 'Mmin', cdRegister);
 S.RegisterDelphiFunction(@Mmax, 'Mmax', cdRegister);
 S.RegisterDelphiFunction(@MmaxAbs, 'MmaxAbs', cdRegister);

 S.RegisterDelphiFunction(@MallNorm, 'MallNorm', cdRegister);

 S.RegisterDelphiFunction(@MHflip, 'MHflip', cdRegister);
 S.RegisterDelphiFunction(@MConv, 'MConv', cdRegister);
 S.RegisterDelphiFunction(@MCrop, 'MCrop', cdRegister);
 S.RegisterDelphiFunction(@MOneCol, 'MOneCol', cdRegister);
 S.RegisterDelphiFunction(@MOneRow, 'MOneRow', cdRegister);
 S.RegisterDelphiFunction(@MStamp, 'MStamp', cdRegister);
 S.RegisterDelphiFunction(@MStampCol, 'MStampCol', cdRegister);
 S.RegisterDelphiFunction(@MStampRow, 'MStampRow', cdRegister);
 S.RegisterDelphiFunction(@MColsum, 'MColsum', cdRegister);
 S.RegisterDelphiFunction(@MRowsum, 'MRowsum', cdRegister);
 S.RegisterDelphiFunction(@Mload, 'Mload', cdRegister);
 S.RegisterDelphiFunction(@Msave, 'Msave', cdRegister);
end;

 
 
{ TPSImport_dynmatrix }
(*----------------------------------------------------------------------------*)
procedure TPSImport_dynmatrix.CompileImport1(CompExec: TPSScript);
begin
  SIRegister_dynmatrix(CompExec.Comp);
end;
(*----------------------------------------------------------------------------*)
procedure TPSImport_dynmatrix.ExecImport1(CompExec: TPSScript; const ri: TPSRuntimeClassImporter);
begin
//  RIRegister_dynmatrix(ri);
  RIRegister_dynmatrix_Routines(CompExec.Exec); // comment it if no routines
end;
(*----------------------------------------------------------------------------*)
 
 
end.
