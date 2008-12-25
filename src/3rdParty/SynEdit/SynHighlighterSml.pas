{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterSML.pas, released 2000-04-17.
The Original Code is based on the dmMLSyn.pas file from the
mwEdit component suite by Martin Waldenburg and other developers, the Initial
Author of this file is David H. Muir.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: SynHighlighterSml.pas,v 1.1.1.1 2004/02/20 08:18:25 anoncvs Exp $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}
{
@abstract(Provides SynEdit with a Standard ML syntax highlighter, with extra options for the standard Basis library.)
@author(David H Muir <dhm@dmsoftware.co.uk>)
@created(1999)
@lastmod(2000-06-23)
The SynHighlighterSML.pas unit provides SynEdit text control with a Standard ML highlighter.  Many formatting attributes can
be specified, and there is an option to include extra keywords and operators only found in the Basis library, this option can
be disabled for backwards compatibility with older ML compilers that do not have support for the Basis Library.
}
unit SynHighlighterSml;

{$I SynEdit.inc}

interface

uses
  SysUtils, Classes,
  {$IFDEF SYN_CLX}
  Qt, QControls, QGraphics,
  {$ELSE}
  Windows, Messages, Controls, Graphics, Registry,
  {$ENDIF}
  SynEditTypes, SynEditHighlighter;

Type
  TtkTokenKind = (tkCharacter, tkComment, tkIdentifier, tkKey, tkNull, tkNumber,
    tkOperator, tkSpace, tkString, tkSymbol, tkSyntaxError, tkUnknown);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function: TtkTokenKind of object;

type
  TSynSMLSyn = class(TSynCustomHighlighter)
  private
    fBasis: Boolean;
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..145] of TIdentFuncTableFunc;
    fCharacterAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fOperatorAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fSyntaxErrorAttri: TSynHighlighterAttributes;
    function IsValidMLCharacter(Ch: Char): Boolean;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: String): Boolean;
    function Func15: TtkTokenKind;
    function Func19: TtkTokenKind;
    function Func20: TtkTokenKind;
    function Func21: TtkTokenKind;
    function Func23: TtkTokenKind;
    function Func26: TtkTokenKind;
    function Func28: TtkTokenKind;
    function Func31: TtkTokenKind;
    function Func35: TtkTokenKind;
    function Func37: TtkTokenKind;
    function Func41: TtkTokenKind;
    function Func43: TtkTokenKind;
    function Func44: TtkTokenKind;
    function Func47: TtkTokenKind;
    function Func50: TtkTokenKind;
    function Func52: TtkTokenKind;
    function Func57: TtkTokenKind;
    function Func59: TtkTokenKind;
    function Func60: TtkTokenKind;
    function Func62: TtkTokenKind;
    function Func66: TtkTokenKind;
    function Func68: TtkTokenKind;
    function Func74: TtkTokenKind;
    function Func76: TtkTokenKind;
    function Func80: TtkTokenKind;
    function Func82: TtkTokenKind;
    function Func88: TtkTokenKind;
    function Func92: TtkTokenKind;
    function Func97: TtkTokenKind;
    function Func101: TtkTokenKind;
    function Func111: TtkTokenKind;
    function Func114: TtkTokenKind;
    function Func126: TtkTokenKind;
    function Func145: TtkTokenKind;
    procedure AsciiCharProc;
    procedure CRProc;
    procedure CharacterProc;
    procedure ColonProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OperatorProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    procedure BasisOpProc;
    procedure RoundBracketOpen;
    function AltFunc: TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TtkTokenKind;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                         //mh 2000-07-14
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
  published
    property CharacterAttri: TSynHighlighterAttributes read fCharacterAttri
      write fCharacterAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property OperatorAttri: TSynHighlighterAttributes read fOperatorAttri
      write fOperatorAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property SyntaxErrorAttri: TSynHighlighterAttributes read fSyntaxErrorAttri
      write fSyntaxErrorAttri;
    property Basis: Boolean read FBasis write FBasis default True;
  end;

implementation

uses
  SynEditStrConst;

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    Case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
    else Identifiers[I] := False;
    end;
    J := UpCase(I);
    Case I in ['_', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J) - 64
    else mHashTable[I] := 0;
    end;
  end;
end;

function TSynSMLSyn.IsValidMLCharacter(Ch: Char): Boolean;
begin
  if (Ch in ['A'..'Z', 'a'..'z', '0'..'9']) then Result := True
  else case Ch of
    '\','|',',','.','<','>','/','?',':',';','@','#','~','[','{',']','}','_','-','=','+',
    '!','"','�','$','%','^','&','*','(',')': Result := True
    else Result := False;
  end;
end; { IsValidMLCharacter }

procedure TSynSMLSyn.InitIdent;
var
  I: Integer;
  pF: PIdentFuncTableFunc;
begin
  pF := PIdentFuncTableFunc(@fIdentFuncTable);
  for I := Low(fIdentFuncTable) to High(fIdentFuncTable) do begin
    pF^ := AltFunc;
    Inc(pF);
  end;
  fIdentFuncTable[15] := Func15;
  fIdentFuncTable[19] := Func19;
  fIdentFuncTable[20] := Func20;
  fIdentFuncTable[21] := Func21;
  fIdentFuncTable[23] := Func23;
  fIdentFuncTable[26] := Func26;
  fIdentFuncTable[28] := Func28;
  fIdentFuncTable[31] := Func31;
  fIdentFuncTable[35] := Func35;
  fIdentFuncTable[37] := Func37;
  fIdentFuncTable[41] := Func41;
  fIdentFuncTable[43] := Func43;
  fIdentFuncTable[44] := Func44;
  fIdentFuncTable[47] := Func47;
  fIdentFuncTable[50] := Func50;
  fIdentFuncTable[52] := Func52;
  fIdentFuncTable[57] := Func57;
  fIdentFuncTable[59] := Func59;
  fIdentFuncTable[60] := Func60;
  fIdentFuncTable[62] := Func62;
  fIdentFuncTable[66] := Func66;
  fIdentFuncTable[68] := Func68;
  fIdentFuncTable[74] := Func74;
  fIdentFuncTable[76] := Func76;
  fIdentFuncTable[80] := Func80;
  fIdentFuncTable[82] := Func82;
  fIdentFuncTable[88] := Func88;
  fIdentFuncTable[92] := Func92;
  fIdentFuncTable[97] := Func97;
  fIdentFuncTable[101] := Func101;
  fIdentFuncTable[111] := Func111;
  fIdentFuncTable[114] := Func114;
  fIdentFuncTable[126] := Func126;
  fIdentFuncTable[145] := Func145;
end;

function TSynSMLSyn.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while ToHash^ in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do
  begin
    inc(Result, mHashTable[ToHash^]);
    inc(ToHash);
  end;
  fStringLen := ToHash - fToIdent;
end;

function TSynSMLSyn.KeyComp(const aKey: String): Boolean;
var
  I: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end else Result := False;
end;

function TSynSMLSyn.Func15: TtkTokenKind;
begin
  if KeyComp('if') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func19: TtkTokenKind;
begin
  if KeyComp('do') then Result := tkKey else
    if KeyComp('and') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func20: TtkTokenKind;
begin
  if KeyComp('as') then Result := tkKey else
    if KeyComp('fn') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func21: TtkTokenKind;
begin
  if KeyComp('of') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func23: TtkTokenKind;
begin
  if KeyComp('in') then Result := tkKey else
    if KeyComp('end') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func26: TtkTokenKind;
begin
  if KeyComp('rec') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func28: TtkTokenKind;
begin
  if KeyComp('case') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func31: TtkTokenKind;
begin
  if KeyComp('op') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func35: TtkTokenKind;
begin
  if KeyComp('val') then Result := tkKey else
    if KeyComp('sig') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func37: TtkTokenKind;
begin
  if KeyComp('let') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func41: TtkTokenKind;
begin
  if KeyComp('fun') then Result := tkKey else
    if KeyComp('else') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func43: TtkTokenKind;
begin
  if KeyComp('local') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func44: TtkTokenKind;
begin
  if KeyComp('handle') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func47: TtkTokenKind;
begin
  if KeyComp('then') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func50: TtkTokenKind;
begin
  if KeyComp('open') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func52: TtkTokenKind;
begin
  if KeyComp('raise') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func57: TtkTokenKind;
begin
  if KeyComp('while') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func59: TtkTokenKind;
begin
  if KeyComp('where') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func60: TtkTokenKind;
begin
  if KeyComp('with') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func62: TtkTokenKind;
begin
  if KeyComp('infix') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func66: TtkTokenKind;
begin
  if KeyComp('andalso') then Result := tkKey else
    if KeyComp('type') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func68: TtkTokenKind;
begin
  if KeyComp('include') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func74: TtkTokenKind;
begin
  if KeyComp('orelse') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func76: TtkTokenKind;
begin
  if KeyComp('sharing') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func80: TtkTokenKind;
begin
  if KeyComp('infixr') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func82: TtkTokenKind;
begin
  if KeyComp('nonfix') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func88: TtkTokenKind;
begin
  if KeyComp('abstype') then Result := tkKey else
    if KeyComp('eqtype') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func92: TtkTokenKind;
begin
  if KeyComp('datatype') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func97: TtkTokenKind;
begin
  if KeyComp('functor') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func101: TtkTokenKind;
begin
  if KeyComp('struct') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func111: TtkTokenKind;
begin
  if KeyComp('exception') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func114: TtkTokenKind;
begin
  if KeyComp('signature') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func126: TtkTokenKind;
begin
  if KeyComp('withtype') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.Func145: TtkTokenKind;
begin
  if KeyComp('structure') then Result := tkKey else Result := tkIdentifier;
end;

function TSynSMLSyn.AltFunc: TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynSMLSyn.IdentKind(MayBe: PChar): TtkTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 146 then Result := fIdentFuncTable[HashKey] else Result := tkIdentifier;
end;

procedure TSynSMLSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #39: fProcTable[I] := AsciiCharProc;
      #13: fProcTable[I] := CRProc;
      '#': fProcTable[I] := CharacterProc;
      ':': fProcTable[I] := ColonProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := IdentProc;
      #10: fProcTable[I] := LFProc;
      #0: fProcTable[I] := NullProc;
      '0'..'9': fProcTable[I] := NumberProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := SpaceProc;
      '"': fProcTable[I] := StringProc;
      '@', '^': fProcTable[I] := BasisOpProc;
      '(': fProcTable[I] := RoundBracketOpen;
      '+', '-', '~', '*', '/', '=', '<', '>':  fProcTable[i] := OperatorProc;
      ',', '.',  ';': fProcTable[I] := SymbolProc;
    else
      fProcTable[I] := UnknownProc;
    end;
end;

constructor TSynSMLSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCharacterAttri := TSynHighlighterAttributes.Create(SYNS_AttrCharacter);
  fCharacterAttri.Foreground := clBlue;
  AddAttribute(fCharacterAttri);
  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clNavy;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Style := [fsBold];
  fKeyAttri.Foreground := clGreen;
  AddAttribute(fKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clRed;
  AddAttribute(fNumberAttri);
  fOperatorAttri := TSynHighlighterAttributes.Create(SYNS_AttrOperator);
  fOperatorAttri.Foreground := clMaroon;
  AddAttribute(fOperatorAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clBlue;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);
  fSyntaxErrorAttri := TSynHighlighterAttributes.Create(SYNS_AttrSyntaxError);
  fSyntaxErrorAttri.Foreground := clRed;
  fSyntaxErrorAttri.Style := [fsBold];
  AddAttribute(fSyntaxErrorAttri);
  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  MakeMethodTables;
  fDefaultFilter := SYNS_FilterSML;
  Basis := True;
end;

procedure TSynSMLSyn.SetLine(NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSMLSyn.AsciiCharProc;
begin
  fTokenID := tkString;
  repeat
    Inc(Run);
  until fLine[Run] in [#0, #10, #13, #39];
  if fLine[Run] = #39 then inc(Run);
end;

procedure TSynSMLSyn.CRProc;
begin
  fTokenID := tkSpace;
  Case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynSMLSyn.CharacterProc;
begin
  if (FLine[Run+1] = '"') and IsValidMLCharacter(FLine[Run+2]) and (FLine[Run+3] = '"') then begin
    fTokenID := tkCharacter;
    inc(Run, 4);
  end
  else begin
    fTokenID := tkSyntaxError;
    inc(Run);
    if (FLine[Run] = '"') then Inc(Run);
    if IsValidMLCharacter(FLine[Run]) then Inc(Run);
    if (FLine[Run] = '"') then Inc(Run);
  end;
end;

procedure TSynSMLSyn.ColonProc;
begin
  inc(Run);
  if Basis and (fLine[Run] = ':') then begin
    fTokenID := tkOperator;
    inc(Run);
  end
  else fTokenID := tkSymbol;
end;

procedure TSynSMLSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  inc(Run, fStringLen);
  while Identifiers[fLine[Run]] do inc(Run);
end;

procedure TSynSMLSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSMLSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSMLSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in
      ['0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F'] do
  begin
    case FLine[Run] of
      '.':  if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynSMLSyn.OperatorProc;
begin
  inc(Run);
  fTokenID := tkOperator;
end;

procedure TSynSMLSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynSMLSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynSMLSyn.SymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSMLSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run,2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSMLSyn.BasisOpProc;
begin
  inc(Run);
  if Basis then fTokenID := tkOperator else fTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.RoundBracketOpen;
begin
  Inc(Run);
  if (FLine[Run] = '*') then begin
    fTokenID := tkComment;
    inc(Run);
    while fLine[Run] <> #0 do
      case fLine[Run] of
        '*': if fLine[Run + 1] = ')' then begin
               inc(Run, 2);
               break;
             end
             else inc(Run);
        #10: break;
        #13: break;
        else inc(Run);
      end
  end
  else fTokenID := tkIdentifier;
end;

procedure TSynSMLSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynSMLSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynSMLSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynSMLSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynSMLSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSMLSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case GetTokenID of
    tkCharacter: Result := fCharacterAttri;
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkOperator: Result := fOperatorAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkSyntaxError: Result := fSyntaxErrorAttri;
    tkUnknown: Result := fIdentifierAttri;
    else Result := nil;
  end;
end;

function TSynSMLSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSMLSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

function TSynSMLSyn.GetIdentChars: TSynIdentChars;
begin
  Result := TSynValidStringChars;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}                                             //mh 2000-07-14
function TSynSMLSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSML;
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}                                                            //mh 2000-07-14
  RegisterPlaceableHighlighter(TSynSMLSyn);
{$ENDIF}
end.
