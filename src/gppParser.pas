(*:GpProfile Delphi pseudo-parser.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2006, Primoz Gabrijelcic
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
- The name of the Primoz Gabrijelcic may not be used to endorse or promote
  products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   Author            : Primoz Gabrijelcic
   Creation date     : 2006-06-17
   Last modification : 2006-06-17
   Version           : 1.0
</pre>*)(*
   History:
     1.0: 2006-06-17
       - Imported into CVS.
*)

unit gppParser;

{$B-,H+,J+,Q-,T-,X+} //don't change!

interface

uses
  Classes,
  Contnrs,
  mPasLex,
  gppIDT;

type
  TNotifyProc     = procedure(unitName: string) of object;
  TNotifyInstProc = procedure(fullName, unitName: string; parse: boolean) of object;

  TUnitList = class;
  TProcList = class;
  TAPIList  = class;
  TProject  = class;
  TProc     = class;

  TParserObj = class
  strict private
    psFileName: string;
    psParser  : TmwPasLex;
    psStream  : TMemoryStream;
  public
    constructor Create(parser: TmwPasLex; stream: TMemoryStream; fileName: string);
    property FileName: string read psFileName;
    property Lexer: TmwPasLex read psParser;
    property Stream: TMemoryStream read psStream;
  end; { TParserObj }

  TDefineList = class
  strict private
    dlDefines: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Assign(conditionals: string);
    procedure Define(conditional: string);
    function  IsDefined(conditional: string): boolean;
    procedure Undefine(conditional: string);
  end; { TDefineList }

  TStatement = class
  strict private
    stEnd         : integer;    // statement end, could be -1
    stInstrumented: boolean;    // is statement instrumented
    stLine        : integer;    // first statement line
    stList        : TList;      // list of statements in a block
    stSource      : string;     // original line, set only if stEnd <> -1
    stStart       : integer;    // statement start
    stToken       : TTokenKind; // statement token (tkWhile, tkIf etc)
  strict protected
    function GetStatement(idx: integer): TStatement;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Add(statement: TStatement);
    function  Count: integer;
    property FirstLine: integer read stLine write stLine;
    property Instrumented: boolean read stInstrumented write stInstrumented;
    property Source: string read stSource write stSource;
    property Statement[idx: integer]: TStatement read GetStatement; default;
    property StatementEnd: integer read stEnd write stEnd;
    property StatementStart: integer read stStart write stStart;
    property Token: TTokenKind read stToken write stToken;
  end; { TStatement }

  TStatements = class
  strict private
    stOpen     : TList;      // stack of open statements
    stStatement: TStatement;
  public
    constructor Create;
    destructor  Destroy; override;
    function  AddStatement(aToken: TTokenKind; parser: TmwPasLex;
      stmtLine, stmtStart, stmtEnd: integer): TStatement;
    function  CloseStatement: boolean;
    procedure PrepareStatement(token: TTokenKind; parser: TmwPasLex;
      stmtLine, tokenPos: integer);
    procedure Push(st: TStatement);
    property Statement: TStatement read stStatement;
  end; { TStatements }

  TUnit = class
  strict private
    unAllInst        : boolean;
    unAnyTrueUnit    : boolean;
    unAPIs           : TAPIList;
    unEndUses        : integer;
    unExcluded       : boolean;
    unFileDate       : integer;
    unImplementOffset: integer;
    unName           : string;
    unNoneInst       : boolean;
    unParsed         : boolean;
    unProcs          : TProcList;
    unProjectDir     : boolean;
    unQual           : string;
    unStartUses      : integer;
    unUnitID         : integer;
    unUnits          : TUnitList;
    unUsesOffset     : integer;
  public
    constructor Create(unitName: string; unitLocation: string = ''; excluded: boolean = false);
    destructor  Destroy; override;
    function  AnyChange: boolean;
    function  AnyInstrumented: boolean;
    procedure CheckInstrumentedProcs;
    procedure ConstructNames(idt: TIDTable);
    procedure Instrument(project: TProject; idt: TIDTable; keepDate: boolean);
    function  LocateProc(procName: string): TProc;
    function  LocateUnit(unitName: string): TUnit;
    function  Parse(project: TProject; const exclUnits, searchPath, defaultDir,
      conditionals: string; rescan, parseAssembler, D4syntax: boolean): boolean;
    property AllInstrumented: boolean read unAllInst write unAllInst;
    property Excluded: boolean read unExcluded;
    property FileDate: integer read unFileDate;
    property Name: string read unName;
    property NoneInstrumented: boolean read unNoneInst write unNoneInst;
    property Parsed: boolean read unParsed;
    property ProcedureList: TProcList read unProcs;
    property ProjectDir: boolean read unProjectDir write unProjectDir;
    property QualifiedName: string read unQual;
    property UnitID: integer read unUnitID write unUnitID;
  end; { TUnit }

  TProc = class
  strict private
    prCmtEnterBegin : integer;
    prCmtEnterEnd   : integer;
    prCmtExitBegin  : integer;
    prCmtExitEnd    : integer;
    prEndLineNum    : integer;
    prEndOffset     : integer;
    prHeaderLineNum : integer;
    prInitial       : boolean;
    prInstrumented  : boolean;
    prIntroToken    : string;
    prName          : string;
    prPureAsm       : boolean;
    prStartLineNum  : integer;
    prStartOffset   : integer;
    prStatement     : TStatements;
  public
    constructor Create(procName: string; offset: integer = 0; lineNum: integer = 0;
      headerLineNum: integer = 0; token: string = '');
    destructor  Destroy; override;
    function  ClassName: string;
    function  IsMethod: boolean;
    function  MethodName: string;
    procedure UninstrumentAllStatements;
    property CmtEnterBegin: integer read prCmtEnterBegin write prCmtEnterBegin;
    property CmtEnterEnd: integer read prCmtEnterEnd write prCmtEnterEnd;
    property CmtExitBegin: integer read prCmtExitBegin write prCmtExitBegin;
    property CmtExitEnd: integer read prCmtExitEnd write prCmtExitEnd;
    property EndLineNum: integer read prEndLineNum write prEndLineNum;
    property EndOffset: integer read prEndOffset write prEndOffset;
    property HeaderLineNum : integer read prHeaderLineNum;
    property Initial: boolean read prInitial write prInitial;
    property Instrumented: boolean read prInstrumented write prInstrumented;
    property IntroToken: string read prIntroToken;
    property Name: string read prName write prName;
    property PureAsm: boolean read prPureAsm write prPureAsm;
    property StartOffset: integer read prStartOffset;
    property Statement: TStatements read prStatement;
  end; { TProc }

  TAPI = class
  strict private
    apiBeginOffs: integer;
    apiCommands : string;
    apiEndOffs  : integer;
    apiExitBegin: integer;
    apiExitEnd  : integer;
    apiMeta     : boolean;
  public
    constructor Create(apiCmd: string; apiBegin, apiEnd, apiExStart, apiExEnd: integer;
      apiIsMetaComment: boolean);
    property BeginOffs: integer read apiBeginOffs;
    property Commands: string read apiCommands;
    property EndOffs: integer read apiEndOffs;
    property ExitBegin: integer read apiExitBegin;
    property ExitEnd: integer read apiExitEnd;
    property Meta: boolean read apiMeta;
  end; { TAPI }

  TUnitEnumerator = record
  strict private
    ueIndex   : integer;
    ueUnitList: TStringList;
  public
    constructor Create(unitList: TStringList);
    function GetCurrent: TUnit;
    function MoveNext: boolean;
    property Current: TUnit read GetCurrent;
  end; { TUnitEnumerator }

  TUnitList = class
  strict private
    ulList: TStringList;
  strict protected
    function GetUnits(idxUnit: integer): TUnit;
  public
    constructor Create; 
    destructor  Destroy; override;
    procedure Add(anUnit: TUnit);
    function  Count: integer;
    function  GetEnumerator: TUnitEnumerator;
    function  Locate(const unitName: string): TUnit;
    property Units[idxUnit: integer]: TUnit read GetUnits; default;
  end; { TUnitList }

  TGlbUnitList = class
  strict private
    gulList: TStringList;
  strict protected
    function GetUnits(idxUnit: integer): TUnit;
  public
    constructor Create; 
    destructor  Destroy; override;
    procedure Clear;
    function  Count: integer;
    function  GetEnumerator: TUnitEnumerator;
    function  Locate(unitName: string): TUnit;
    function  LocateCreate(unitName, unitLocation: string; excluded: boolean): TUnit;
    property Units[idxUnit: integer]: TUnit read GetUnits; default;
  end; { TGlbUnitList }

  TProcEnumerator = record
  strict private
    peIndex   : integer;
    peUnitList: TStringList;
  public
    constructor Create(unitList: TStringList);
    function GetCurrent: TProc;
    function MoveNext: boolean;
    property Current: TProc read GetCurrent;
  end; { TProcEnumerator }

  TProcList = class
  strict private
    plList       : TStringList;
    plOwnsObjects: boolean;
  strict protected
    function  GetProc(idxProc: integer): TProc;
  public
    constructor Create(ownsObjects: boolean = true); reintroduce;
    destructor  Destroy; override;
    function  Add(var procName: string; pureAsm: boolean; offset, lineNum,
      headerLineNum: integer; token: string): TProc; overload;
    function  Add(proc: TProc): integer; overload;
    procedure AddEnd(procName: string; offset, linenum: integer);
    procedure AddInstrumented(procName: string; cmtEnterBegin, cmtEnterEnd, cmtExitBegin,
      cmtExitEnd: integer);
    function  AnyChange: boolean;
    function  AnyInstrumented: boolean;
    procedure CheckInstrumented(var allInstrumented, noneInstrumented: boolean);
    procedure Clear;
    procedure ConstructNames(idt: TIDTable; const unitName, unitFullName: string);
    function  Count: integer;
    function  GetEnumerator: TProcEnumerator;
    function  Locate(const procName: string): TProc;
    property Proc[idxProc: integer]: TProc read GetProc; default;
  end; { TProcList }

  TAPIList = class
  strict private
    alList: TObjectList;
  protected
    function  GetAPI(idxAPI: integer): TAPI;
  public
    constructor Create; reintroduce;
    destructor  Destroy; override;
    procedure AddExpanded(apiEnterBegin,apiEnterEnd,apiExitBegin,apiExitEnd: integer);
    procedure AddMeta(apiCmd: string; apiBegin, apiEnd: integer);
    function Count: integer;
    property API[idxAPI: integer]: TAPI read GetAPI; default;
  end; { TAPIList }

  TProject = class
  strict private
    prAPIIntro        : string;
    prAppendUses      : string;
    prConditEnd       : string;
    prConditEndAPI    : string;
    prConditEndUses   : string;
    prConditStart     : string;
    prConditStartAPI  : string;
    prConditStartUses : string;
    prCreateUses      : string;
    prName            : string;
    prProfileAPI      : string;
    prProfileEnterAsm : string;
    prProfileEnterProc: string;
    prProfileExitAsm  : string;
    prProfileExitProc : string;
    prUnit            : TUnit;
    prUnits           : TGlbUnitList;
  strict protected
    function  AllInstrumented(projectDirOnly: boolean): boolean;
    function  AnyChange(projectDirOnly: boolean): boolean;
    function  AnyInstrumented(projectDirOnly: boolean): boolean;
    procedure CheckUnitID(chkUn: TUnit);
    function  GetFirstLine(unitName, procName: string): integer;
    function  GetHighestUnusedID: integer;
    function  GetInstrumentedStr: string;
    function  GetUnitPath(unitName: string): string;
    function  GetUnitProc(unitName, procName: string): TProc;
    function  NoneInstrumented(projectDirOnly: boolean): boolean;
    procedure PrepareComments(commentType: integer; gpprofName: string);
    procedure Rescan(exclUnits, searchPath, conditionals: string; notify: TNotifyProc;
      commentType: integer; ignoreFileDate: boolean; parseAssembler, D4syntax: boolean;
      gpprofName: string);
    procedure SetInstrumentedStr(instr: string);
  public
    constructor Create(projName: string);
    destructor  Destroy; override;
    procedure GetProcList(un: TUnit; pl: TProcList);
    procedure GetUnitList(ul: TObjectList; projectDirOnly: boolean);
    procedure Instrument(projectDirOnly: boolean; notify: TNotifyInstProc; commentType:
      integer; keepDate: boolean; const conditionals, searchPath: string; parseAssembler,
      D4syntax: boolean; gptSettings: TStringList; const gptUnitName: string; resourceName:
      string);
    procedure InstrumentAll(instrument, projectDirOnly: boolean);
    procedure InstrumentClass(unitData: TUnit; className: string; instrument: boolean);
    procedure InstrumentProc(unitData: TUnit; procData: TProc; instrument: boolean);
    procedure InstrumentUnit(anUnit: TUnit; instrument: boolean);
    procedure Parse(excludedUnits: TStrings; searchPath, conditionals: string; notify:
      TNotifyProc; commentType: integer; parseAssembler, D4syntax: boolean; gpprofName:
      string);
    property APIIntro: string read prAPIIntro;
    property AppendUses: string read prAppendUses;
    property ConditEnd: string read prConditEnd;
    property ConditEndAPI: string read prConditEndAPI;
    property ConditEndUses: string read prConditEndUses;
    property ConditStart: string read prConditStart;
    property ConditStartAPI: string read prConditStartAPI;
    property ConditStartUses: string read prConditStartUses;
    property CreateUses: string read prCreateUses;
    property ProfileAPI: string read prProfileAPI;
    property ProfileEnterAsm: string read prProfileEnterAsm;
    property ProfileEnterProc: string read prProfileEnterProc;
    property ProfileExitAsm: string read prProfileExitAsm;
    property ProfileExitProc: string read prProfileExitProc;
    property ProjectUnit: TUnit read prUnit;
    property Name: string read prName;
    property Units: TGlbUnitList read prUnits;
  end; { TProject }

implementation

uses
  Windows,
  SysUtils,
  Forms,
  GpProf_Collector,
  GpString,
  GpStuff,
  GpLists,
  GpResourceWriter,
  gppCommon,
  gppFileEdit;

type
  TClass = class
    clAllInst: boolean;
    clInstrum: string;
  end; { TClass }

{ globals }

function FindOnPath(uName, pSearchPath, pDefaultDir: string): string;
var
  i: integer;
  s: string;
  p: string;
begin
  if Pos('.',UpperCase(uName)) = 0 then
    uName := uName + '.pas';
  p := MakeBackslash(pDefaultDir);
  if FileExists(p+uName) then
    FindOnPath := LowerCase(p+uName)
  else if FileExists(uName) then begin
    if ExtractFilePath(uName) = '' then
      FindOnPath := LowerCase(MakeBackslash(GetCurrentDir)+uName)
    else
      FindOnPath := LowerCase(uName);
  end
  else begin
    for i := 1 to NumElements(pSearchPath,';',-1) do begin
      s := Compress(NthEl(pSearchPath,i,';',-1));
      if Pos('\',s) = 0 then
        s := p + s;
      s := MakeSmartBackslash(s)+uName;
      if FileExists(s) then begin
        FindOnPath := LowerCase(s);
        Exit;
      end;
    end;
    FindOnPath := '';
  end;
end; { FindOnPath }

{ TDefineList }

constructor TDefineList.Create;
begin
  inherited Create;
  dlDefines := TStringList.Create;
  dlDefines.Sorted := true;
end; { TDefineList.Create }

procedure TDefineList.Define(conditional: string);
begin
  if not IsDefined(conditional) then
    dlDefines.Add(UpperCase(conditional));
end; { TDefineList.Define }

destructor TDefineList.Destroy;
begin
  FreeAndNil(dlDefines);
  inherited;
end; { TDefineList.Destroy }

function TDefineList.IsDefined(conditional: string): boolean;
begin
  Result := (dlDefines.IndexOf(UpperCase(conditional)) >= 0);
end; { TDefineList.IsDefined }

procedure TDefineList.Undefine(conditional: string);
var
  idx: integer;
begin
  idx := dlDefines.IndexOf(UpperCase(conditional));
  if idx >= 0 then
    dlDefines.Delete(idx);
end; { TDefineList.Undefine }

procedure TDefineList.Assign(conditionals: string);
var
  i: integer;
begin
  dlDefines.Clear;
  for i := 1 to NumElements(conditionals,';',-1) do
    Define(NthEl(conditionals,i,';',-1));
end; { TDefineList.Assign }

{ TUnitEnumerator }

constructor TUnitEnumerator.Create(unitList: TStringList);
begin
  ueUnitList := unitList;
  ueIndex := -1;
end; { TUnitEnumerator.Create }

function TUnitEnumerator.GetCurrent: TUnit;
begin
  Result := TUnit(ueUnitList.Objects[ueIndex]);
end; { TUnitEnumerator.GetCurrent }

function TUnitEnumerator.MoveNext: boolean;
begin
  Result := ueIndex < ueUnitList.Count - 1;
  if Result then
    Inc(ueIndex);
end; { TUnitEnumerator.MoveNext }

{ TUnitList }

constructor TUnitList.Create;
begin
  inherited;
  ulList := TStringList.Create;
end; { TUnitList.Create }

destructor TUnitList.Destroy;
begin
  FreeAndNil(ulList);
  inherited;
end; { TUnitList.Destroy }

procedure TUnitList.Add(anUnit: TUnit);
begin
  ulList.AddObject(anUnit.Name, anUnit);
end; { TUnitList.Add }

function TUnitList.Count: integer;
begin
  Result := ulList.Count;
end; { TUnitList.Count }

function TUnitList.GetEnumerator: TUnitEnumerator;
begin
  Result := TUnitEnumerator.Create(ulList);
end; { TUnitList.GetEnumerator }

function TUnitList.GetUnits(idxUnit: integer): TUnit;
begin
  Result := TUnit(ulList.Objects[idxUnit]);
end; { TUnitList.GetUnits }

function TUnitList.Locate(const unitName: string): TUnit;
begin
  Result := TUnit(ulList.FetchObject(unitName));
end; { TUnitList.Locate }

{ TGlbUnitList }

constructor TGlbUnitList.Create;
begin
  inherited Create;
  gulList := TStringList.Create;
end; { TGlbUnitList.Create }

destructor TGlbUnitList.Destroy;
var
  i: integer;
begin
  for i := 0 to gulList.Count - 1 do
    gulList.Objects[i].Free;
  FreeAndNil(gulList);
  inherited;
end; { TGlbUnitList.Destroy }

function TGlbUnitList.Count: integer;
begin
  Result := gulList.Count;
end; { TGlbUnitList.Count }

procedure TGlbUnitList.Clear;
begin
  gulList.Clear;
end; { TGlbUnitList.Empty }

function TGlbUnitList.GetEnumerator: TUnitEnumerator;
begin
  Result := TUnitEnumerator.Create(gulList);
end; { TGlbUnitList.GetEnumerator }

function TGlbUnitList.GetUnits(idxUnit: integer): TUnit;
begin
  Result := TUnit(gulList.Objects[idxUnit]);
end; { TGlbUnitList.GetUnits }

function TGlbUnitList.Locate(unitName: string): TUnit;
begin
  Result := TUnit(gulList.FetchObject(unitName));
end; { TGlbUnitList.Locate }

function TGlbUnitList.LocateCreate(unitName, unitLocation: string; excluded: boolean): TUnit;
var
  idxUnit: integer;
begin
  idxUnit := gulList.IndexOf(unitName);
  if idxUnit >= 0 then
    Result := TUnit(gulList.Objects[idxUnit])
  else begin
    Result := TUnit.Create(unitName, unitLocation, excluded);
    gulList.AddObject(unitName, Result);
  end;
end; { TGlbUnitList.LocateCreate }

{ TProcEnumerator }

constructor TProcEnumerator.Create(unitList: TStringList);
begin
  peUnitList := unitList;
  peIndex := -1;
end; { TProcEnumerator.Create }

function TProcEnumerator.GetCurrent: TProc;
begin
  Result := TProc(peUnitList.Objects[peIndex]);
end; { TProcEnumerator.GetCurrent }

function TProcEnumerator.MoveNext: boolean;
begin
  Result := peIndex < peUnitList.Count - 1;
  if Result then
    Inc(peIndex);
end; { TProcEnumerator.MoveNext }

{ TProcList }

constructor TProcList.Create(ownsObjects: boolean);
begin
  inherited Create;
  plList := TStringList.Create;
  plOwnsObjects := ownsObjects;
end; { TProcList.Create }

destructor TProcList.Destroy;
var
  i: integer;
begin
  if plOwnsObjects then
    for i := 0 to Count - 1 do
      Proc[i].Free;
  FreeAndNil(plList);
  inherited Destroy;
end; { TProcList.Destroy }

function TProcList.Add(var procName: string; pureAsm: boolean;
  offset, lineNum, headerLineNum: integer; token: string): TProc;
var
  idxProc   : integer;
  overloaded: boolean;
  post      : integer;
begin
  idxProc := plList.IndexOf(procName);
  if idxProc >= 0 then begin
    Proc[idxProc].Name := Proc[idxProc].Name + ':1';
    overloaded := true;
  end
  else
    overloaded := (plList.IndexOf(procName+':1') >= 0);
  if overloaded then begin // fixup for overloaded procedures
    post := 2;
    while plList.IndexOf(procName + ':' + IntToStr(post)) >= 0 do
      Inc(post);
    procName := procName + ':' + IntToStr(post)
  end;
  Result := TProc.Create(procName, offset, linenum, headerLineNum, token);
  with Result do begin
    CmtEnterBegin := -1;
    CmtEnterEnd   := -1;
    CmtExitBegin  := -1;
    CmtExitEnd    := -1;
    PureAsm       := pureAsm;
  end;
  plList.AddObject(procName, Result);
end; { TProcList.Add }

function TProcList.Add(proc: TProc): integer;
begin
  Result := plList.AddObject(proc.Name, proc);
end; { TProcList.Add }

procedure TProcList.AddEnd(procName: string; offset, linenum: integer);
var
  idxProc: integer;
begin
  idxProc := plList.IndexOf(procName);
  if idxProc >= 0 then
    with Proc[idxProc] do begin
      EndOffset  := offset;
      EndLineNum := linenum;
      Initial    := false;
    end;
end; { TProcList.AddEnd }

procedure TProcList.AddInstrumented(procName: string; cmtEnterBegin, cmtEnterEnd,
  cmtExitBegin, cmtExitEnd: integer);
var
  idxProc : integer;
  procData: TProc;
begin
  idxProc := plList.IndexOf(procName);
  if idxProc >= 0 then begin
    procData := Proc[idxProc];
    procData.CmtEnterBegin := cmtEnterBegin;
    procData.CmtEnterEnd   := cmtEnterEnd;
    procData.CmtExitBegin  := cmtExitBegin;
    procData.CmtExitEnd    := cmtExitEnd;
    procData.Instrumented  := true;
    procData.Initial       := true;
  end;
end; { TProcList.AddInstrumented }

function TProcList.AnyChange: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to Count - 1 do
    with Proc[i] do
      if Instrumented <> Initial then begin
        Result := true;
        Exit;
      end;
end; { TProcList.AnyChange }

function TProcList.AnyInstrumented: boolean;
var
  i: integer;
begin
  Result := false;
  for i := 0 to Count - 1 do
    if Proc[i].Instrumented then begin
      Result := true;
      Exit;
    end;
end; { TProcList.AnyInstrumented }

procedure TProcList.CheckInstrumented(var allInstrumented, noneInstrumented: boolean);
var
  i: integer;
begin
  allInstrumented := true;
  noneInstrumented := true;
  for i := 0 to Count - 1 do
    if not Proc[i].Instrumented then
      allInstrumented := false
    else
      noneInstrumented := false;
end; { TProcList.CheckInstrumented }

procedure TProcList.Clear;
begin
  plList.Clear;
end; { TProcList.Clear }

procedure TProcList.ConstructNames(idt: TIDTable; const unitName, unitFullName: string);
var
  i : integer;
  pr: TProc;
begin
  for i := 0 to Count - 1 do begin
    pr := Proc[i];
    if pr.Instrumented then
      idt.ConstructName(unitName, unitFullName, pr.Name, pr.HeaderLineNum);
  end;
end; { TProcList.ConstructNames }

function TProcList.Count: integer;
begin
  Result := plList.Count;
end; { TProcList.Count }

function TProcList.GetEnumerator: TProcEnumerator;
begin
  Result := TProcEnumerator.Create(plList);
end; { TProcList.GetEnumerator }

function TProcList.GetProc(idxProc: integer): TProc;
begin
  Result := TProc(plList.Objects[idxProc]);
end; { TProcList.GetProc }

function TProcList.Locate(const procName: string): TProc;
begin
  Result := TProc(plList.FetchObject(procName));
end; { TProcList.Locate }

{ TProc }

constructor TProc.Create(procName: string; offset, lineNum, headerLineNum: integer; token: string);
begin
  inherited Create;
  prName          := procName;
  prHeaderLineNum := headerLineNum;
  prStartOffset   := offset;
  prStartLineNum  := linenum;
  prInstrumented  := false;
  prStatement     := TStatements.Create;
  prIntroToken    := token;
end; { TProc.Create }

destructor TProc.Destroy;
begin
  FreeAndNil(prStatement);
  inherited;
end; { TProc.Destroy }

function TProc.ClassName: string;
begin
  if not IsMethod then
    Result := ''
  else
    Result := FirstEl(prName, '.', -1);
end; { TProc.ClassName }

function TProc.IsMethod: boolean;
begin
  Result := (Pos('.', prName) > 0);
end; { TProc.IsMethod }

function TProc.MethodName: string;
begin
  if not IsMethod then
    Result := prName
  else
    Result := ButFirstEl(prName, '.', -1);
end; { TProc.MethodName }

procedure TProc.UninstrumentAllStatements;

  procedure Recurse(st: TStatement);
  var
    i: integer;
  begin
    for i := 0 to st.Count-1 do begin
      st[i].Instrumented := true;
      Recurse(st[i]);
    end; //for
  end; { Recurse }

begin { TProc.UninstrumentAllStatements }
  Recurse(prStatement.Statement);
end; { TProc.UninstrumentAllStatements }

{ TUnit }

constructor TUnit.Create(unitName: string; unitLocation: string = ''; excluded: boolean = false);
begin
  unName            := ExtractFileName(unitName);
  if unitLocation = '' then
    unQual := unitName
  else
    unQual := unitLocation;
  unUnits           := TUnitList.Create;
  unProcs           := TProcList.Create;
  unAPIs            := TAPIList.Create;
  unParsed          := false;
  unExcluded        := excluded;
  unAllInst         := false;
  unNoneInst        := true;
  unUsesOffset      := -1;
  unImplementOffset := -1;
  unStartUses       := -1;
  unEndUses         := -1;
  unFileDate        := -1;
  unAnyTrueUnit     := false;
end; { TUnit.Create }

destructor TUnit.Destroy;
begin
  FreeAndNil(unUnits);
  FreeAndNil(unProcs);
  FreeAndNil(unAPIs);
  inherited;
end; { TUnit.Destroy }

function TUnit.AnyChange: boolean;
begin
  Result := unProcs.AnyChange;
end; { TUnit.AnyChange }

function TUnit.AnyInstrumented: boolean;
begin
  Result := unProcs.AnyInstrumented;
end; { TUnit.AnyInstrumented }

procedure TUnit.CheckInstrumentedProcs;
begin
  unProcs.CheckInstrumented(unAllInst, unNoneInst);
end; { TUnit.CheckInstrumentedProcs }

procedure TUnit.ConstructNames(idt: TIDTable);
begin
  unProcs.ConstructNames(idt, unName, unQual);
end; { TUnit.ConstructNames }

procedure TUnit.Instrument(project: TProject; idt: TIDTable; keepDate: boolean);
var
  any     : boolean;
  api     : TAPI;
  ed      : TFileEdit;
  haveInst: boolean;
  haveUses: boolean;
  i       : integer;
  justName: string;
  name    : integer;
  pr      : TProc;
begin
  if unImplementOffset = -1 then
    raise Exception.Create('No implementation part defined in unit '+unName+'!');
  justName := ButLastEl(unQual, '.', Ord(-1));
  DeleteFile(justName+'.bk2');
  RenameFile(justName+'.bk1', justName+'.bk2');
  CopyFile(PChar(unQual), PChar(justName+'.bk1'), false);
  ed := TFileEdit.Create(unQual);
  try
    any := AnyInstrumented;
    haveUses := (unStartUses >= 0) and (unEndUses > unStartUses);
    if haveUses then
      ed.Remove(unStartUses,unEndUses+Length(project.ConditEndUses)-1);
    if any then begin
      if unAnyTrueUnit then
        ed.Insert(unUsesOffset+Length('uses'),Format(project.AppendUses,[unUnitID]))
      else
        ed.Insert(unImplementOffset+Length('implementation'),Format(project.CreateUses,[unUnitID]));
    end;

    for i := 0 to unAPIs.Count - 1 do begin
      api := unAPIs[i];
      if any then begin
        if api.Meta then begin
          ed.Remove(api.BeginOffs, api.EndOffs);
          ed.Insert(api.BeginOffs, Format(project.ProfileAPI, [api.Commands]));
        end
      end
      else begin
        if not api.Meta then begin
          ed.Remove(api.BeginOffs, api.EndOffs);
          ed.Insert(api.BeginOffs, '{'+project.APIIntro);
          ed.Remove(api.ExitBegin, api.ExitEnd);
          ed.Insert(api.ExitBegin, '}');
        end;
      end;
    end; //for i

    for i := 0 to unProcs.Count - 1 do begin
      pr := unProcs[i];
      haveInst := (pr.CmtEnterBegin >= 0);
      if not pr.Instrumented then begin
        if haveInst then begin // remove instrumentation
          ed.Remove(pr.CmtEnterBegin, pr.CmtEnterEnd + Length(project.ConditEnd) - 1);
          ed.Remove(pr.CmtExitBegin, pr.CmtExitEnd + Length(project.ConditEnd) - 1);
        end;
      end
      else begin
        name := idt.ConstructName(unName, unQual, pr.Name, pr.HeaderLineNum);
        if haveInst then
          ed.Remove(pr.CmtEnterBegin, pr.CmtEnterEnd + Length(project.ConditEnd) - 1);
        if pr.PureAsm then
          ed.Insert(pr.StartOffset + Length(pr.IntroToken),
            Format(project.ProfileEnterAsm, [unUnitID, name]))
        else
          ed.Insert(pr.StartOffset + Length(pr.IntroToken),
            Format(project.ProfileEnterProc, [unUnitID, name]));
        if haveInst then
          ed.Remove(pr.CmtExitBegin, pr.CmtExitEnd + Length(project.ConditEnd) - 1);
        if pr.PureAsm then
          ed.Insert(pr.EndOffset, Format(project.ProfileExitAsm, [unUnitID, name]))
        else
          ed.Insert(pr.EndOffset, Format(project.ProfileExitProc, [unUnitID, name]));
      end;
    end; //for i
    ed.Execute(keepDate);
  finally FreeAndNil(ed); end;
end; { TUnit.Instrument }

function TUnit.LocateProc(procName: string): TProc;
begin
  Result := unProcs.Locate(procName);
end; { TUnit.LocateProc }

function TUnit.LocateUnit(unitName: string): TUnit;
begin
  Result := unUnits.Locate(unitName);
end; { TUnit.LocateUnit }

function TUnit.Parse(project: TProject; const exclUnits, searchPath, defaultDir,
  conditionals: string; rescan, parseAssembler, D4syntax: boolean): boolean;
type
  TStates = (stScan, stParseUses, stScanProc, stScanProc3, stScanProc4, stWaitSemi);
  TProcStates = (stWaitAny, stWaitStatement, stWaitDo, stWaitThen, stWaitOf);
  TCommentStates = (stWaitEnterBegin, stWaitEnterEnd, stWaitExitBegin,
    stWaitExitEnd, stWaitExitBegin2, stNone);
var
  APIcmd               : string;
  apiStart             : integer;
  apiStartEnd          : integer;
  block                : integer;
  cmtEnterBegin        : integer;
  cmtEnterEnd          : integer;
  cmtExitBegin         : integer;
  cmtExitEnd           : integer;
  conds                : TDefineList;
  curProc              : TProc;
  currentFile          : string;
  direct               : string;
  expectUnitID         : boolean;
  ignoreNextSemi       : boolean;
  implement            : boolean;
  inAsmBlock           : boolean;
  incName              : string;
  inRecordDef          : boolean;
  isInstrumentationFlag: boolean;
  lexer                : TmwPasLex;
  lexerStack           : TList;
  lnumstk              : string;
  prevTokenID          : TTokenKind;
  proclnum             : integer;
  procn                : string;
  procname             : string;
  skipList             : TList;
  skipping             : boolean;
  startStmtPos         : integer;
  state                : TStates;
  stateComment         : TCommentStates;
  stateProc            : TProcStates;
  stk                  : string;
  stream               : TMemoryStream;
  tokenData            : string;
  tokenID              : TTokenKind;
  tokenLN              : integer;
  tokenPos             : integer;
  unLocation           : string;
  unName               : string;
  uun                  : string;

  function IsBlockStartToken(token: TTokenKind): boolean;
  begin
    if inRecordDef and (token = tkCase) then
      Result := false
    else
      Result := (token = tkBegin) or (token = tkRepeat) or
        (token = tkCase) or (token = tkTry) or
        (token = tkAsm) or (token = tkRecord);
    if token = tkAsm then
      inAsmBlock := true;
    if token = tkRecord then
      inRecordDef := true;
  end; { IsBlockStartToken }

  function IsBlockEndToken(token: TTokenKind): boolean;
  begin
    IsBlockEndToken := (token = tkEnd) or (token = tkUntil);
    if Result then begin
      inAsmBlock  := false;
      inRecordDef := false;
    end;
  end; { IsBlockEndToken }

  function IsNonblank(token: TTokenKind): boolean;
  begin
    Result := (token <> tkSpace) and (token <> tkCRLF) and
      (token <> tkBorComment) and (token <> tkAnsiComment) and
      (token <> tkSlashesComment);
  end; { IsNonblank }

  function ExpandLocation(location: string): string;
  begin
    if location = '' then
      Result := ''
    else begin
      location := Copy(location,2,Length(location)-2);
      if ((Length(location) >= 2) and (location[2] = ':')) or
         (location[1] = '\') then
        Result := location
      else
        Result := ExpandFileName(MakeBackslash(ExtractFilePath(project.ProjectUnit.QualifiedName))+location);
    end;
  end; { ExpandLocation }

  procedure CreateNewParser(fName: string; searchOnPath: string);
  var
    zero: char;
    pstk: TParserObj;
  begin
    if lexer <> nil then begin
      pstk := TParserObj.Create(lexer,stream,currentFile);
      lexerStack.Add(pstk);
    end;
    if not FileExists(fName) then begin
      fName := FindOnPath(fName, searchOnPath, defaultDir);
      if not FileExists(fName) then
        Abort;
    end;
    lexer := TmwPasLex.Create(D4syntax);
    stream := TMemoryStream.Create;
    if FileExists(fName) then begin
      try
        stream.LoadFromFile(fName);
      except
        FreeAndNil(stream);
        Abort;
      end;
    end;
    stream.Position := stream.Size;
    zero := #0;
    stream.Write(zero,1);
    lexer.Origin := Stream.Memory;
    lexer.RunPos := 0;
    currentFile := fName;
  end; { CreateNewParser }

  function RemoveLastParser: boolean;
  var
    pstk: TParserObj;
  begin
    FreeAndNil(lexer);
    FreeAndNil(stream);
    if lexerStack.Count > 0 then begin
      pstk := TParserObj(lexerStack[lexerStack.Count-1]);
      lexer := pstk.Lexer;
      stream := pstk.Stream;
      currentFile := pstk.FileName;
      FreeAndNil(pstk);
      lexerStack.Delete(lexerStack.Count-1);
      lexer.Next;
      Result := true;
    end
    else begin
      lexer := nil;
      stream := nil;
      Result := false;
    end;
  end; { RemoveLastParser }

  function ExtractCommentBody(comment: string): string;
  begin
    if comment = '' then
      Result := ''
    else if comment[1] = '{' then
      Result := Copy(comment,2,Length(comment)-2)
    else
      Result := Copy(comment,3,Length(comment)-4);
  end; { ExtractCommentBody }

  function ExtractDirective(comment: string): string;
  begin
    Result := ButFirst(UpperCase(FirstEl(ExtractCommentBody(comment),' ',-1)),1);
    // Fix Delphi stupidity - Delphi parses {$ENDIF.} (where '.' is any
    // non-ident character (alpha, number, underscore)) as {$ENDIF}
    while (Result <> '') and (not (Result[Length(Result)] in ['a'..'z','A'..'Z','0'..'9','_','+','-'])) do
      Delete(Result,Length(Result),1);
  end; { ExtractDirective }

  function ExtractParameter(comment: string; parameter: integer): string;
  begin
    Result := NthEl(ExtractCommentBody(comment),parameter+1,' ',-1);
  end; { ExtractParameter }

  procedure PushSkippingState(skipping: boolean; isIFOPT: boolean);
  begin
    skipList.Add(pointer(skipping));
    skipList.Add(pointer(isIFOPT));
  end; { PushSkippingState }

  function WasSkipping: boolean;
  begin
    if skipList.Count = 0 then
      Result := false
    else
      Result := boolean(skipList[skipList.Count-2]);
  end; { WasSkipping }

  function InIFOPT: boolean;
  begin
    if skipList.Count = 0 then
      Result := false // should not happen, but ...
    else
      Result := boolean(skipList[skipList.Count-1]);
  end; { TUnit.InIFOPT }

  function PopSkippingState: boolean;
  begin
    if skipList.Count = 0 then
      Result := true //source damaged - skip the rest
    else begin
      skipList.Delete(skipList.Count-1);
      Result := boolean(skipList[skipList.Count-1]);
      skipList.Delete(skipList.Count-1);
    end;
  end; { PopSkippingState }

  function IsOneOf(key: string; compareWith: array of string): boolean;
  var
    i: integer;
  begin
    Result := false;
    for i := Low(compareWith) to High(compareWith) do begin
      if key = compareWith[i] then begin
        Result := true;
        break;
      end;
    end; //for
  end; { IsOneOf }

  procedure M_Initialize;
  begin
    if not rescan then begin
      unQual := FindOnPath(unQual, searchPath, defaultDir);
      if not FileExists(unQual) then
        Abort;
      unProjectDir :=
        (self = project.ProjectUnit) or
        (ExtractFilePath(unQual) = ExtractFilePath(project.ProjectUnit.QualifiedName));
    end
    else begin
//        FreeAndNil(unProcs);
//        unProcs := TProcList.Create;
//        FreeAndNil(unUnits);
//        unUnits := TUnitList.Create;
    end;
    FreeAndNil(unAPIs);
    unAPIs := TAPIList.Create;
    lexer := nil;
    CreateNewParser(unQual,'');
    unFileDate        := FileAge(unQual); 
    state             := stScan;
    stateComment      := stNone;  
    implement         := false;
    block             := 0;
    procname          := '';
    proclnum          := -1;
    stk               := '';
    lnumstk           := '';
    cmtEnterBegin     := -1;
    cmtEnterEnd       := -1;
    cmtExitBegin      := -1;
    cmtExitEnd        := -1;
    unStartUses       := -1;
    unEndUses         := -1;
    unName            := '';
    unLocation        := '';
    unUsesOffset      := -1;
    unImplementOffset := -1;
    unStartUses       := -1;
    unEndUses         := -1;
    unAnyTrueUnit     := false;
    inAsmBlock        := false;
    inRecordDef       := false;
    prevTokenID       := tkNull;
    apiStart          := -1;
    apiStartEnd       := -1;
    skipping          := false;
    curProc           := nil;
  end; { M_Initialize }

  procedure M_PrepareToken;
  begin
    tokenID   := lexer.TokenID;
    tokenData := lexer.Token;
    tokenPos  := lexer.TokenPos;
    tokenLN   := lexer.LineNumber;
  end; { M_PrepareToken }

  procedure M_ProcessCondComp;
  begin
    // Don't process conditional compilation directive if it is
    // actually an instrumentation flag!
    with project do
      isInstrumentationFlag :=
        IsOneOf(tokenData,[ConditStart, ConditStartUses, ConditStartAPI,
                           ConditEnd, ConditEndUses, ConditEndAPI]);
    if not isInstrumentationFlag then begin
      direct := ExtractDirective(tokenData);
      if direct = 'IFDEF' then begin //process $IFDEF
        PushSkippingState(skipping,false);
        skipping := skipping or (not conds.IsDefined(ExtractParameter(tokenData,1)));
      end
      else if direct = 'IFOPT' then // process $IFOPT
        PushSkippingState(skipping,true)
      else if direct = 'IFNDEF' then begin //process $IFNDEF
        PushSkippingState(skipping,false);
        skipping := skipping or conds.IsDefined(ExtractParameter(tokenData,1));
      end
      else if direct = 'ENDIF' then //process $ENDIF
        skipping := PopSkippingState
      else if direct = 'ELSE' then begin //process $ELSE
        if (not InIFOPT) and (not WasSkipping) then
          skipping := not skipping;
      end;
    end;
  end; { M_ProcessCondComp }

  function M_ProcessCompDirect: boolean;
  begin
    Result := true;
    // direct := ExtractDirective(tokenData); // Already done
    if (direct = 'INCLUDE') or (direct = 'I') then begin //process $INCLUDE
      // process {$I *.INC}
      incName := ExtractParameter(tokenData,1);
      if FirstEl(incName,'.',-1) = '*' then
        incName := FirstEl(ExtractFileName(unQual),'.',-1)+'.'+ButFirstEl(incName,'.',-1);
      CreateNewParser(incName,searchPath);
      Result := false;
    end
    else if direct = 'DEFINE' then //process $DEFINE
      conds.Define(ExtractParameter(tokenData,1))
    else if direct = 'UNDEF' then //process $UNDEF
      conds.Undefine(ExtractParameter(tokenData,1));
  end; { M_ProcessCompDirect }

  procedure M_FixLexerFeatures;
  begin
    if inAsmBlock and
       ((prevTokenID = tkAddressOp) or (prevTokenID = tkDoubleAddressOp)) and
       (tokenID <> tkAddressOp) and
       (tokenID <> tkDoubleAddressOp) then
      tokenID := tkIdentifier;
    //fix mwPasParser's feature - these are not reserved words!
    if (tokenID = tkRead)      or (tokenID = tkWrite)     or
       (tokenID = tkName)      or (tokenID = tkIndex)     or
       (tokenID = tkStored)    or (tokenID = tkReadonly)  or
       (tokenID = tkResident)  or (tokenID = tkNodefault) or
       (tokenID = tkAutomated) or (tokenID = tkWriteonly) then
      tokenID := tkIdentifier;
  end; { M_FixLexerFeatures }

  procedure M_MetaComments;
  var
    unID: integer;
  begin
    if expectUnitID then begin
      expectUnitID := false;
      unID := StrToIntDef(ButFirst(ButLast(tokenData,1),1),-1);
      if unID > 0 then begin
        unUnitID := unID;
        Exit;
      end;
    end;
    if tokenData = project.ConditStartUses then begin
      unStartUses := tokenPos;
      expectUnitID := true;
    end
    else if tokenData = project.ConditEndUses then begin
      unEndUses := tokenPos;
      expectUnitID := false;
    end
    else if ((tokenID = tkBorComment) and (Copy(tokenData,1,1+Length(project.APIIntro))='{'+project.APIIntro)) or
            ((tokenID = tkAnsiComment) and (Copy(tokenData,1,2+Length(project.APIIntro))='(*'+project.APIIntro)) then
    begin
      if tokenID = tkBorComment then
        APIcmd := TrimL(Trim(ButLast(ButFirst(tokenData,1+Length(project.APIIntro)),1)))
      else
        APIcmd := TrimL(Trim(ButLast(ButFirst(tokenData,2+Length(project.APIIntro)),2)));
      if lexerStack.Count = 0 then
        unAPIs.AddMeta(APIcmd,tokenPos,tokenPos+Length(tokenData)-1);
    end
    else if tokenData = project.ConditStartAPI then begin
      apiStart := tokenPos;
      apiStartEnd := tokenPos+Length(tokenData)-1;
    end
    else if tokenData = project.ConditEndAPI then
      if lexerStack.Count = 0 then
        unAPIs.AddExpanded(apiStart, apiStartEnd, tokenPos, tokenPos+Length(tokenData)-1);
  end; { M_MetaComments }

  procedure M_ParseUses;
  begin
    if (tokenID = tkSemicolon) or (tokenID = tkComma) then begin
      if tokenID = tkSemicolon then
        state := stScan;
      if unName <> '' then begin
        uun := UpperCase(unName);
        if (unImplementOffset >= 0) and
           ((unStartUses < 0) and (unEndUses < 0)) or
           ((unStartUses >= 0) and (unEndUses >= unStartUses)) then
          unAnyTrueUnit := true;
        unUnits.Add(project.Units.LocateCreate(unName, ExpandLocation(unLocation),
                    (Pos(#13#10+uun+#13#10,exclUnits) <> 0) or
                    ((unStartUses >= 0) and (unEndUses < 0))));
      end;
      unName := '';
      unLocation := '';
    end
    else if tokenID = tkIdentifier then
      unName := tokenData
    else if tokenID = tkString then
      unLocation := tokenData;
  end; { M_ParseUses }

  procedure M_InitializationFinalization; forward;

  procedure M_ProcedureScanner;

    procedure M_ScanProc;
    begin
      if (tokenID = tkIdentifier) or (tokenID = tkPoint) or
         (tokenID = tkRegister) then begin
        procname := procname + tokenData;
        proclnum := tokenLN;
      end
      else if ((tokenID = tkSpace) and (procname <> '')) or
              (tokenID = tkSemicolon) or
              (tokenID = tkRoundOpen) or
              (tokenID = tkColon) then
        state := stScanProc3;
    end; { M_ScanProc }

    procedure M_ScanProc3;
    begin
      if ((tokenID = tkProcedure) or (tokenID = tkFunction) or
          (tokenID = tkConstructor) or (tokenID = tkDestructor)) and implement then
      begin
        state := stScanProc;
        block := 0;
        if procname <> '' then begin
          stk := stk + '/' + procname;
          lnumstk := lnumstk + '/' + IntToStr(proclnum);
        end;
        procname := '';
        proclnum := -1;
      end
      else if (tokenID = tkForward) or (tokenID = tkExternal) then begin
        procname := '';
        proclnum := -1;
      end
      else begin
        if IsBlockStartToken(tokenID) then
          Inc(block)
        else if IsBlockEndToken(tokenID) then
          Dec(block);
        if block < 0 then begin
          state    := stScan;
          stk      := '';
          lnumstk  := '';
          procname := '';
          proclnum := -1;
        end
        else if (block > 0) and (not inRecordDef) then begin
          if stk <> '' then
            procn := ButFirst(stk,1)+'/'+procname
          else
            procn := procname;
          if lexerStack.Count = 0 then
            if (tokenID <> tkAsm) or parseAssembler then
              curProc := unProcs.Add(procn,(tokenID = tkAsm),tokenPos,tokenLN,proclnum,tokenData);
          state := stScanProc4;
          stateComment := stWaitEnterBegin;
          stateProc := stWaitAny;
          startStmtPos := -1;
          ignoreNextSemi := false;
        end;
      end;
    end; { M_ScanProc3 }

    procedure M_ScanProc4;

      procedure M_ScanMetaComment;
      begin
        if tokenData = project.ConditStart then begin
          if stateComment = stWaitEnterBegin then begin
            cmtEnterBegin := tokenPos;
            stateComment := stWaitEnterEnd;
          end
          else if (stateComment = stWaitExitBegin) or
                  (stateComment = stWaitExitBegin2) then
          begin
            cmtExitBegin := tokenPos;
            stateComment := stWaitExitEnd;
          end;
        end
        else if tokenData = project.ConditEnd then begin
          if stateComment = stWaitEnterEnd then begin
            cmtEnterEnd := tokenPos;
            stateComment := stWaitExitBegin;
          end
          else if stateComment = stWaitExitEnd then begin
            cmtExitEnd := tokenPos;
            stateComment := stWaitExitBegin2;
          end;
        end;
      end; { M_ScanMetaComment }

      procedure M_EndProcedure;
      begin
        if lexerStack.Count = 0 then begin
          unProcs.AddEnd(procn,tokenPos,tokenLN);
          if stateComment = stWaitExitBegin2 then begin
            unProcs.AddInstrumented(procn,cmtEnterBegin,cmtEnterEnd,cmtExitBegin,cmtExitEnd);
            CheckInstrumentedProcs;
          end;
        end;
        stateComment := stNone;
        if stk = '' then begin
          procname := '';
          proclnum := -1;
          state    := stScan;
        end
        else begin
          procname := LastEl(stk,'/',-1);
          proclnum := StrToInt(LastEl(lnumstk,'/',-1));
          stk      := ButLastEl(stk,'/',-1);
          lnumstk  := ButLastEl(lnumstk,'/',-1);
          state    := stScanProc3;
        end;
        if tokenID = tkFinalization then
          M_InitializationFinalization;
      end; { M_EndProcedure }

      procedure M_ParseProcedure;

        procedure AddStatement(token: TTokenKind; start,stop: integer);
        begin
          curProc.Statement.AddStatement(token,lexer,tokenLN,start,stop);
          startStmtPos := -1;
        end; { AddStatement }

        procedure CheckMissingSemi;
        var
          tmpPos: integer;
        begin
          if (startStmtPos >= 0) and (startStmtPos < tokenPos) then begin // missing semicolon before 'end'
            tmpPos := tokenPos-1;
            while (tmpPos > startStmtPos) and (Ord((lexer.Origin+tmpPos)^) < 33) do
              Dec(tmpPos);
            if tmpPos > startStmtPos then
              AddStatement(tkSemicolon,startStmtPos,tmpPos);
          end;
        end; { CheckMissingSemi }

        function CloseStatement: boolean;
        begin
          CheckMissingSemi;
          Result := curProc.Statement.CloseStatement;
          if not Result then // parsing error
            curProc := nil;
          startStmtPos := -1;
        end; { CloseStatement }

        procedure PrepareStatement(newState: TProcStates);
        var
          st: TStatement;
        begin
          CheckMissingSemi;
          if stateProc = stWaitStatement then begin
            st := curProc.Statement.AddStatement(tokenID,lexer,tokenLN,tokenPos,-1);
            if not CloseStatement then
              Exit;
            curProc.Statement.Push(st);
          end
          else
            curProc.Statement.PrepareStatement(tokenID,lexer,tokenLN,tokenPos);
          stateProc := newState;
          startStmtPos := -1;
          ignoreNextSemi := false;
        end; { PrepareStatement }

      begin { M_ParseProcedure }
        if not assigned(curProc) then
          Exit;
        if ((stateProc = stWaitDo)   and (tokenID = tkDo)) or
           ((stateProc = stWaitThen) and (tokenID = tkThen)) or
           ((stateProc = stWaitOf)   and (tokenID = tkOf)) then
        begin
          if stateProc = stWaitOf then
            stateProc := stWaitAny
          else
            stateProc := stWaitStatement;
          startStmtPos := -1;
        end
        else begin
          if (startStmtPos = -1) and IsNonblank(tokenID) then
            startStmtPos := tokenPos;
          if (tokenID = tkFor) or (tokenID = tkWhile) or (tokenID = tkWith) then
            PrepareStatement(stWaitDo)
          else if tokenID = tkRepeat then
            PrepareStatement(stWaitAny)
          else if tokenID = tkIf then
            PrepareStatement(stWaitThen)
          else if tokenID = tkElse then
            PrepareStatement(stWaitStatement)
          else if tokenID = tkTry then begin
            PrepareStatement(stWaitAny);
            PrepareStatement(stWaitAny); // #ToDoH this requires double end - how?
          end
          else if (tokenID = tkFinally) or (tokenID = tkExcept) then begin
            CloseStatement;
            ignoreNextSemi := true;
            PrepareStatement(stWaitAny);
          end
          else if tokenID = tkCase then
            PrepareStatement(stWaitOf)
          else if tokenID = tkSemicolon then begin
            if ignoreNextSemi then
              ignoreNextSemi := false
            else begin
              AddStatement(tokenID,startStmtPos,tokenPos);
              if stateProc = stWaitStatement then begin
                CloseStatement;
                stateProc := stWaitAny;
              end;
            end;
            startStmtPos := -1;
          end
          else if (tokenID = tkBegin) or (tokenID = tkAsm) then begin
            if stateProc = stWaitStatement then
              stateProc := stWaitAny
            else
              curProc.Statement.PrepareStatement(tokenID,lexer,tokenLN,tokenPos);
            startStmtPos := -1;
          end
          else if (tokenID = tkEnd) or (tokenID = tkUntil) then begin
            CloseStatement;
            ignoreNextSemi := true;
          end;
        end;
      end; { M_ParseProcedure }

    begin { M_ScanProc4 }
      if IsBlockStartToken(tokenID) then
        Inc(block)
      else if IsBlockEndToken(tokenID) or (tokenID = tkFinalization) then
        Dec(block);
      if (tokenID = tkBorComment) or (tokenID = tkCompDirect) then
        M_ScanMetaComment;
      if block = 0 then
        M_EndProcedure
      else
        M_ParseProcedure;
    end; { M_ScanProc4 }

  begin { M_ProcedureScanner }
    if state = stScanProc then
      M_ScanProc
    else if state = stScanProc3 then
      M_ScanProc3
    else if state = stScanProc4 then
      M_ScanProc4
  end; { M_ProcedureScanner }

  procedure M_PrepareProc;
  begin
    state := stScanProc;
    block := 0;
    if procname <> '' then begin
      stk := stk + '/' + procname;
      lnumstk := lnumstk + '/' + IntToStr(proclnum);
    end;
    procname := '';
    proclnum := -1;
  end; { M_PrepareProc }

  procedure M_InitializationFinalization;
  begin
    procname := LowerCase(tokenData);
    procn    := procname;
    block := 1;
    curProc := unProcs.Add(procname,false,tokenPos,tokenLN,tokenLN,tokenData);
    state := stScanProc4;
    stateComment := stWaitEnterBegin;
    stateProc := stWaitAny;
    startStmtPos := -1;
    ignoreNextSemi := false;
  end; { M_InitializationFinalization }

begin { TUnit.Parse }
  unParsed := true;
  unUnitID := -1;
  Parse := true;
  try
    lexerStack := TList.Create;
    try
      M_Initialize;
      skipList := TList.Create;
      try
        conds := TDefineList.Create;
        conds.Assign(conditionals);
        try
          repeat
            while lexer.TokenID <> tkNull do begin
              M_PrepareToken;
              if tokenID = tkCompDirect then
                M_ProcessCondComp;
              if not skipping then begin //we're not in the middle of conditionally removed block
                if (tokenID = tkPoint) and (prevTokenID = tkEnd) then
                  break; //while - final end.
                if tokenID = tkCompDirect then
                  if not M_ProcessCompDirect then
                    continue; //while
                M_FixLexerFeatures;
                if (tokenID = tkBorComment) or (tokenID = tkAnsiComment) or
                   (tokenID = tkCompDirect) then
                  M_MetaComments;
                if state = stWaitSemi then begin
                  if tokenID = tkSemicolon then begin
                    unImplementOffset := tokenPos+1;
                    state := stScan;
                  end;
                end
                else if state = stParseUses then
                  M_ParseUses
                else if state in [stScanProc,stScanProc3,stScanProc4] then
                  M_ProcedureScanner
                else if (tokenID = tkUses) or
                        ((tokenID = tkIdentifier) and (UpperCase(tokenData) = 'CONTAINS'){not recognized by the parser}) then
                begin
                  state := stParseUses;
                  if implement then begin
                    if lexerStack.Count > 0 then begin // unable to instrument if 'uses' is in include file
                      implement := false;
                      unImplementOffset := -1;
                    end
                    else
                      unUsesOffset := tokenPos;
                  end;
                end
                else if (tokenID = tkImplementation) and (lexerStack.Count = 0) then begin
                  implement := true;
                  unImplementOffset := tokenPos;
                end
                else if tokenID = tkProgram then begin
                  implement := true;
                  state := stWaitSemi;
                end
                else if implement and (tokenID = tkInitialization) then
                  M_InitializationFinalization
                else if implement and (tokenID = tkFinalization) then
                  M_InitializationFinalization
                else if ((tokenID = tkProcedure) or (tokenID = tkFunction) or
                         (tokenID = tkConstructor) or (tokenID = tkDestructor)) and implement then
                  M_PrepareProc;
              end; //if not skipping
              prevTokenID := tokenID;
              lexer.Next;
            end; //while
          until not RemoveLastParser;
        finally FreeAndNil(conds); end;
      finally FreeAndNil(skipList); end;
    finally FreeAndNil(lexerStack); end;
  except Parse := false; end;
end; { TUnit.Parse }

{ TProject }

constructor TProject.Create(projName: string);
begin
  prUnits:= TGlbUnitList.Create;
  prName := projName;                             
  prUnit := nil;
end; { TProject.Create }

destructor TProject.Destroy;
begin
  FreeAndNil(prUnits);
  inherited;
end; { TProject.Destroy }

procedure TProject.CheckUnitID(chkUn: TUnit);
var
  un: TUnit;
begin
  if chkUn.UnitID <= 0 then
    Exit;
  for un in prUnits do begin
    if un.UnitID = chkUn.UnitID then begin
      chkUn.UnitID := -1;
      Exit;
    end;
  end;
end; { TProject.CheckUnitID }

procedure TProject.Parse(excludedUnits: TStrings; searchPath, conditionals: string;
  notify: TNotifyProc; commentType: integer; parseAssembler, D4syntax: boolean;
  gpprofName: string);
var
  curDir   : string;
  exclUnits: string;
  u1       : TUnit;
  un       : TUnit;
begin
  PrepareComments(commentType,gpprofName);
  exclUnits := excludedUnits.Text;
  if Last(exclUnits,2) <> #13#10 then
    exclUnits := exclUnits + #13#10;
  if First(exclUnits,2) <> #13#10 then
    exclUnits := #13#10 + exclUnits;
  prUnits.Clear;
  prUnit := prUnits.LocateCreate(prName, '', false);
  prUnit.ProjectDir := true;
  GetDir(0,curDir);
  ChDir(ExtractFilePath(prUnit.QualifiedName));
  try
    un := prUnit;
    repeat
      if @Notify <> nil then
        Notify(un.Name);
      un.Parse(self,exclUnits,searchPath,ExtractFilePath(prName),conditionals,false,parseAssembler,D4syntax);
      CheckUnitID(un);
      un := nil;
      for u1 in prUnits do begin
        if not (u1.Parsed or u1.Excluded) then begin
          un := u1;
          break; //for
        end;
      end;
    until (un = nil);
  finally ChDir(curDir); end;
end; { TProject.Parse }

procedure TProject.GetUnitList(ul: TObjectList; projectDirOnly: boolean);
var
  un: TUnit;
begin
  ul.Clear;
  ul.OwnsObjects := false;
  for un in prUnits do begin
    if (not un.Excluded) and (un.ProcedureList.Count > 0) and
       ((not projectDirOnly) or un.ProjectDir)
    then
      ul.Add(un);
  end;
end; { TProject.GetUnitList }

procedure TProject.GetProcList(un: TUnit; pl: TProcList);
var
  pr: TProc;
begin
  pl.Clear;
  if un <> nil then begin
    for pr in un.ProcedureList do 
      pl.Add(pr);
  end;
end; { TProject.GetProcList }

procedure TProject.InstrumentUnit(anUnit: TUnit; instrument: boolean);
var
  pr: TProc;
begin
  anUnit.AllInstrumented := instrument;
  anUnit.NoneInstrumented := not instrument;
  for pr in anUnit.ProcedureList do
    pr.Instrumented := instrument;
end; { TProject.InstrumentTUnit }

procedure TProject.InstrumentAll(instrument, projectDirOnly: boolean);
var
  un: TUnit;
begin
  for un in prUnits do begin
    if (not un.Excluded) and (un.ProcedureList.Count > 0) then
      if (not projectDirOnly) or un.ProjectDir then
        InstrumentUnit(un,instrument);
  end;
end; { TProject.InstrumentAll }

procedure TProject.InstrumentClass(unitData: TUnit; className: string; instrument:
  boolean);
var
  procData: TProc;
  procs   : TProcList;
begin
  procs := TProcList.Create(false);
  try
    GetProcList(unitData, procs);
    for procData in procs do begin
      if ((className = '') and (not procData.IsMethod)) or
         ((className <> '') and (AnsiSameText(className, procData.ClassName)))
      then
        InstrumentProc(unitData, procData, instrument);
    end; //for
  finally FreeAndNil(procs); end;
end; { TProject.InstrumentClass }

procedure TProject.InstrumentProc(unitData: TUnit; procData: TProc; instrument: boolean);
begin
  procData.UninstrumentAllStatements;
  procData.Instrumented := instrument;
  unitData.CheckInstrumentedProcs;
end; { TProject.InstrumentProc }

function TProject.AllInstrumented(projectDirOnly: boolean): boolean;
var
  un: TUnit;
begin
  Result := false;
  for un in prUnits do begin
    if (not un.Excluded) and (un.ProcedureList.Count > 0) and
       (un.ProjectDir or (not projectDirOnly)) then
      if not un.AllInstrumented then
        Exit;
  end;
  Result := true;
end; { TProject.AllInstrumented }

function TProject.GetInstrumentedStr: string;

  procedure AddResult(var instr: string; s: string);
  begin
    if s <> '' then begin
      if instr <> '' then
        instr := instr + ';';
      instr := instr + s;
    end;
  end; { AddResult }
  
  function AddUnit(un: TUnit): string;
  var
    cidx   : integer;
    clas   : string;
    classes: TStringList;
    cobj   : TClass;
    i      : integer;
    p      : integer;
    pr     : TProc;
    proc   : string;
    procs  : TStringList;
  begin
    Result := '';
    procs := TStringList.Create;
    try
// TODO 1 -oPrimoz Gabrijelcic : implement: AddUnit
raise Exception.Create('Not implemented');
//        GetProcList(un.unName,procs{,true});
      if procs.Count > 0 then begin
        classes := TStringList.Create;
        try
          for i := 0 to procs.Count-1 do begin
            proc := procs[i];
            pr := TProc(procs.Objects[i]);
            p := Pos('.',proc);
            if p > 0 then begin
              clas := First(proc,p-1);
              proc := ButFirst(proc,p);
            end
            else
              clas := '<>';
            cidx := classes.IndexOf(clas);
            if cidx = -1 then begin
              cobj := TClass.Create;
              cobj.clAllInst := pr.Instrumented;
              if pr.Instrumented then
                AddResult(cobj.clInstrum,'P'+proc);
              classes.AddObject(clas,cobj);
            end
            else begin
              cobj := TClass(classes.Objects[cidx]);
              if pr.Instrumented then
                AddResult(cobj.clInstrum,'P'+proc)
              else
                cobj.clAllInst := false;
            end;
          end; //for
          for i := 0 to classes.Count-1 do begin
            cobj := TClass(classes.Objects[i]);
            if cobj.clAllInst then
              AddResult(Result,'CA'+classes[i])
            else if cobj.clInstrum <> '' then
              AddResult(Result,'C?'+classes[i]+';'+cobj.clInstrum);
            FreeAndNil(cobj);
            classes.Objects[i] := nil;
          end; //for
        finally FreeAndNil(classes); end;
      end;
    finally FreeAndNil(procs); end;
  end; { AddUnit }

var
  allpr: boolean;
  instr: string;
  un   : TUnit;

begin { TProject.GetInstrumentedStr }
  instr := '';
  if AnyInstrumented(false) then begin
    if AllInstrumented(false) then
      AddResult(instr,'A')
    else begin
      allpr := AllInstrumented(true);
      if allpr then
        AddResult(instr,'P');
      for un in prUnits do begin
        if ((not allpr) or (not un.ProjectDir)) and (not un.NoneInstrumented) then begin
          AddResult(instr,'U'+IFF(un.AllInstrumented,'A','?')+un.Name);
          if (not un.Excluded) and (un.ProcedureList.Count > 0) then begin
            if (not un.AllInstrumented) and (not un.NoneInstrumented) then
              AddResult(instr,AddUnit(un));
          end;
        end;
      end; //for iUnit
    end;
  end;
  Result := instr;
end; { TProject.GetInstrumentedStr }

procedure TProject.SetInstrumentedStr(instr: string);
var
  el      : string;
  curUnit : string;
  curClass: string;
  curProc : string;
begin
  if instr = '' then
    InstrumentAll(false,false)
  else if instr[1] = 'A' then
    InstrumentAll(true,false)
  else begin
    if instr[1] = 'P' then begin
      InstrumentAll(false,false);
      InstrumentAll(true,true);
      instr := ButFirstEl(instr,';',-1);
    end;
    curUnit  := '';
    curClass := '';
    while NumElements(instr,';',-1) > 0 do begin
      SplitAtNthEl(instr,1,';',-1,el,instr);
      if el <> '' then begin
        if (el[1] = 'U') and (Length(el) > 2) then begin
          curUnit  := ButFirst(el,2);
          curClass := '';
// TODO 1 -oPrimoz Gabrijelcic : implement: TProject.SetInstrumentedStr
raise Exception.Create('Not implemented');
//          InstrumentUnit(curUnit, el[2] = 'A');
        end
        else if (el[1] = 'C') and (Length(el) > 2) and (curUnit <> '') then begin
          curClass := ButFirst(el,2);
// TODO 1 -oPrimoz Gabrijelcic : implement: TProject.SetInstrumentedStr
raise Exception.Create('Not implemented');
//            InstrumentClass(curUnit, curClass, el[2] = 'A');
        end
        else if (el[1] = 'P') and (curUnit <> '') and (curClass <> '') then begin
          curProc := ButFirst(el,1);
          if curProc <> '' then begin
            if curClass <> '<>' then
              curProc := curClass+'.'+curProc;
// TODO 1 -oPrimoz Gabrijelcic : implement: TProject.SetInstrumentedStr
raise Exception.Create('Not implemented');          
//              InstrumentProc(curUnit,curProc,true);
          end;
        end
        else
          ; // unknown, ignore
      end;
    end; //while
  end;
end; { TProject.SetInstrumentedStr }

function TProject.GetHighestUnusedID: integer;
var
  un: TUnit;
  iUnit: integer;
begin
  Result := 1;
  for iUnit := 0 to prUnits.Count - 1 do begin
    un := prUnits[iUnit];
    if un.UnitID >= Result then
      Result := un.UnitID;
  end;
end; { TProject.GetHighestUnusedID }

procedure TProject.Instrument(projectDirOnly: boolean; notify: TNotifyInstProc;
  commentType: integer; keepDate: boolean; const conditionals, searchPath: string;
  parseAssembler, D4syntax: boolean; gptSettings: TStringList; const gptUnitName: string;
  resourceName: string);
var
  anyInst        : boolean;
  curDir         : string;
  firstUnusedID  : integer;
  gptResourceData: TMemoryStream;
  i              : integer;
  idt            : TIDTable;
  rescan         : TList;
  resStr         : TGpResourceWriter;
  un             : TUnit;
  unAny          : boolean;
begin
  PrepareComments(commentType, gptUnitName);
  rescan := TList.Create;
  try
    gptResourceData := TMemoryStream.Create;
    try
      idt := TIDTable.Create;
      try
        GetDir(0,curDir);
        ChDir(ExtractFilePath(prUnit.QualifiedName));
        try
          firstUnusedID := GetHighestUnusedID;
          with prUnits do begin
            anyInst := false;
            for un in prUnits do begin
              if (not un.Excluded) and (un.ProcedureList.Count > 0) then begin
                if @Notify <> nil then
                  Notify(un.QualifiedName, un.Name, false);
                unAny := un.AnyInstrumented;
                if unAny then
                  anyInst := true;
                //re-instrument: changed units; units without unit id and at least one instrumented procedure
                if un.AnyChange or (unAny and (un.UnitID <= 0)) then begin
                  if un.UnitID <= 0 then begin
                    un.UnitID := firstUnusedID;
                    Inc(firstUnusedID);
                  end;
                  un.Instrument(self,idt,keepDate);
                  rescan.Add(un);
                end
                else
                  un.ConstructNames(idt);
              end;
            end; //for un in prUnits
          end;
          idt.Dump(gptResourceData);
        finally ChDir(curDir); end;
      finally FreeAndNil(idt); end;
      if not anyInst then
        DeleteFile(resourceName)
      else begin
        resStr := TGpResourceWriter.Create(resourceName);
        if assigned(resStr) then begin
          try
            resStr.WriteRCDataFromString(RES_SETTINGS, gptSettings.Text);
            resStr.WriteRCDataFromStream(RES_DATA, gptResourceData);
          finally FreeAndNil(resStr); end;
        end
        else
          Application.MessageBox(PChar('Cannot create resource file '+resourceName),'Instrumentation error',MB_OK);
      end;
      for i := 0 to rescan.Count-1 do begin
        if @Notify <> nil then
          Notify(TUnit(rescan[i]).QualifiedName,TUnit(rescan[i]).Name,true);
        // TODO 1 -oPrimoz Gabrijelcic : this rescan only causes 'instrumented' flag to change and must not destroy TProcs
        TUnit(rescan[i]).Parse(self, '', searchPath, ExtractFilePath(prName),
          conditionals, true, parseAssembler, D4syntax);
      end;
    finally FreeAndNil(gptResourceData); end;
  finally FreeAndNil(rescan); end;
end; { TProject.Instrument }

function TProject.NoneInstrumented(projectDirOnly: boolean): boolean;
var
  un: TUnit;
begin
  Result := false;
  for un in prUnits do begin
    if (not un.Excluded) and (un.ProcedureList.Count > 0) and
       (un.ProjectDir or (not projectDirOnly)) then
      if not un.NoneInstrumented then
        Exit;
  end;
  Result := true;
end; { TProject.NoneInstrumented }

function TProject.GetUnitPath(unitName: string): string;
var
  un: TUnit;
begin
  un := prUnits.Locate(unitName);
  if un = nil then
    raise Exception.Create('Trying to get name of unexistent unit!')
  else
    Result := un.QualifiedName;
end; { TProject.GetUnitPath }

procedure TProject.PrepareComments(commentType: integer; gpprofName: string);
begin
  gpprofName := ExtractFileNameOnly(gpprofName);
  case commentType of
    0:
      begin
        prConditStart     := '{>>GpProfile}';
        prConditStartUses := '{>>GpProfile U}';
        prConditStartAPI  := '{>>GpProfile API}';
        prConditEnd       := '{GpProfile>>}';
        prConditEndUses   := '{GpProfile U>>}';
        prConditEndAPI    := '{GpProfile API>>}';
      end; //0
    1:
      begin
        prConditStart     := '{$IFDEF GpProfile}';
        prConditStartUses := '{$IFDEF GpProfile U}';
        prConditStartAPI  := '{$IFDEF GpProfile API}';
        prConditEnd       := '{$ENDIF GpProfile}';
        prConditEndUses   := '{$ENDIF GpProfile U}';
        prConditEndAPI    := '{$ENDIF GpProfile API}';
      end; //1
  end; //case
  prAppendUses      := prConditStartUses + '{%d} ' + gpprofName + ', ' + prConditEndUses;
  prCreateUses      := prConditStartUses + '{%d} uses ' + gpprofName + '; ' + prConditEndUses;
  prProfileEnterProc:= prConditStart + ' ' + 'ProfilerEnterProc(%d,%d); try ' + prConditEnd;
  prProfileExitProc := prConditStart + ' finally ProfilerExitProc(%d,%d); end; ' + prConditEnd;
  prProfileEnterAsm := prConditStart + ' pushad; mov eax, %d; mov edx, %d; call ProfilerEnterProc; popad ' + prConditEnd;
  prProfileExitAsm  := prConditStart + ' push eax; mov eax, %d; mov edx, %d; call ProfilerExitProc; pop eax ' + prConditEnd;
  prProfileAPI      := prConditStartAPI + '%s' + prConditEndAPI;
  prAPIIntro        := 'GPP:';
end; { TProject.PrepareComments }

function TProject.GetFirstLine(unitName, procName: string): integer;
var
  pr: TProc;
begin
  pr := GetUnitProc(unitName,procName);
  if assigned(pr) then
    Result := pr.HeaderLineNum
  else
    Result := -1;
end; { TProject.GetFirstLine }

function TProject.GetUnitProc(unitName, procName: string): TProc;
var
  un: TUnit;
  pr: TProc;
begin
  un := prUnits.Locate(unitName);
  if un = nil then
    Result := nil
  else begin
    pr := un.LocateProc(procName);
    Result := pr;
  end;
end; { TProject.GetFirstLine }

function TProject.AnyInstrumented(projectDirOnly: boolean): boolean;
var
  un: TUnit;
begin
  Result := true;
  for un in prUnits do begin
    if (not un.Excluded) and (un.ProcedureList.Count > 0) and
       (un.ProjectDir or (not projectDirOnly)) then
      if un.AnyInstrumented then
        Exit;
  end;
  Result := false;
end; { TProject.AnyInstrumented }

procedure TProject.Rescan(exclUnits, searchPath, conditionals: string;
  notify: TNotifyProc; commentType: integer; ignoreFileDate: boolean;
  parseAssembler, D4syntax: boolean; gpprofName: string);
var
  un    : TUnit;
  curDir: string;
begin
  PrepareComments(commentType,gpprofName);
  GetDir(0,curDir);
  ChDir(ExtractFilePath(prUnit.QualifiedName));
  try
    if Last(exclUnits,2) <> #13#10 then
      exclUnits := exclUnits + #13#10;
    for un in prUnits do begin
      if ((not un.Excluded) and (un.ProcedureList.Count > 0)) and
          (ignoreFileDate or (un.FileDate <> FileAge(un.QualifiedName))) then
        un.Parse(self,exclUnits,searchPath,ExtractFilePath(prName),conditionals,true,parseAssembler,D4syntax);
    end;
  finally ChDir(curDir); end;
end; { TProject.Rescan }

function TProject.AnyChange(projectDirOnly: boolean): boolean;
var
  un: TUnit;
begin
  Result := true;
  for un in prUnits do begin
    if (not un.Excluded) and (un.ProcedureList.Count > 0) and
       (un.ProjectDir or (not projectDirOnly)) then
      if un.FileDate <> FileAge(un.QualifiedName) then
        Exit;
  end;
  Result := false;
end; { TProject.AnyChange }

{ TAPI }

constructor TAPI.Create(apiCmd: string; apiBegin, apiEnd, apiExStart, apiExEnd: integer; apiIsMetaComment: boolean);
begin
  inherited Create;
  apiCommands  := apiCmd;
  apiBeginOffs := apiBegin;
  apiEndOffs   := apiEnd;
  apiExitBegin := apiExStart;
  apiExitEnd   := apiExEnd;
  apiMeta      := apiIsMetaComment;
end; { TAPI.Create }

{ TAPIList }

constructor TAPIList.Create;
begin
  inherited Create;
  alList := TObjectList.Create;
end; { TAPIList.Create }

destructor TAPIList.Destroy;
begin
  FreeAndNil(alList);
  inherited;
end; { TAPIList.Destroy }

procedure TAPIList.AddExpanded(apiEnterBegin, apiEnterEnd, apiExitBegin,
  apiExitEnd: integer);
begin
  alList.Add(TAPI.Create('', apiEnterBegin, apiEnterEnd, apiExitBegin, apiExitEnd, false));
end; { TAPIList.AddExpanded }

procedure TAPIList.AddMeta(apiCmd: string; apiBegin, apiEnd: integer);
begin
  alList.Add(TAPI.Create(apiCmd, apiBegin, apiEnd, -1, -1, true));
end; { TAPIList.AddMeta }

function TAPIList.Count: integer;
begin
  Result := alList.Count;
end; { TAPIList.Count }

function TAPIList.GetAPI(idxAPI: integer): TAPI;
begin
  Result := TAPI(alList[idxAPI]);
end; { TAPIList.GetAPI }

{ TStatement }

constructor TStatement.Create;
begin
  inherited;
  stList := TList.Create;
end; { TStatement.Create }

destructor TStatement.Destroy;
var
  i: integer;
begin
  for i := 0 to stList.Count-1 do begin
    TStatement(stList[i]).Free;
    stList[i] := nil;
  end; //for
  FreeAndNil(stList);
  inherited;
end; { TStatement.Destroy }

procedure TStatement.Add(statement: TStatement);
begin
  stList.Add(statement);
end; { TStatement.Add }

function TStatement.Count: integer;
begin
  Result := stList.Count;
end; { TStatement.Count }

function TStatement.GetStatement(idx: integer): TStatement;
begin
  Result := TStatement(stList[idx]);
end; { TStatement.GetStatement }

{ TStatements }

function TStatements.AddStatement(aToken: TTokenKind; parser: TmwPasLex;
  stmtLine, stmtStart, stmtEnd: integer): TStatement;
var
  st: string;
begin
  Result := TStatement.Create;
  with Result do begin
    Token := aToken;
    StatementStart := stmtStart;
    StatementEnd := stmtEnd;
    FirstLine := stmtLine;
    if stmtEnd < 0 then
      Source := ''
    else begin
      SetLength(st, StatementEnd-StatementStart+1);
      Move(pointer(integer(parser.Origin)+StatementStart)^, st[1], Length(st));
      Source := st;
    end;
  end;
  TStatement(stOpen[stOpen.Count-1]).Add(Result);
end; { TStatements.AddStatement }
  
function TStatements.CloseStatement: boolean;
begin
  Result := stOpen.Count > 0;
  if Result then
    stOpen.Delete(stOpen.Count-1);
end; { TStatements.CloseStatement }
  
constructor TStatements.Create;
begin
  inherited;
  stOpen := TList.Create;
  stStatement := TStatement.Create;
  stOpen.Add(stStatement);
end; { TStatements.Create }
  
destructor TStatements.Destroy;
begin
  FreeAndNil(stStatement);
  FreeAndNil(stOpen);
  inherited;
end; { TStatements.Destroy }

procedure TStatements.PrepareStatement(token: TTokenKind; parser: TmwPasLex;
  stmtLine, tokenPos: integer);
begin
  Push(AddStatement(token,parser,stmtLine,tokenPos,-1));
end; { TStatements.PrepareStatement }

procedure TStatements.Push(st: TStatement);
begin
  stOpen.Add(st);
end; { TStatements.Push }

{ TParserObj }

constructor TParserObj.Create(parser: TmwPasLex; stream: TMemoryStream;
  fileName: string);
begin
  psParser   := parser;
  psStream   := stream;
  psFileName := fileName;
end; { TParserObj.Create }

end.

