(*:GpProfile preferences model.
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

unit gppPreferencesModel;

interface

uses
  Classes,
  GpDelphiInfo,
  GpCompositeModel,
  gppCommon;

type
  TGppAnalysisOption = (aoHideInactiveMethods);
  TGppAnalysisOptions = set of TGppAnalysisOption;

  TGppPrefAnalysis = class(TGpModelComponent)
  private
    FOptions: TGppAnalysisOptions;
  public
    constructor Create(const parent: IGpModelComponent); override;
  published
    property Options: TGppAnalysisOptions read FOptions write FOptions default
      [aoHideInactiveMethods];
  end; { TGppPrefAnalysis }

  TGppConditionalDefine = (cdCompiler, cdConsoleApp, cdProject, cdUser);
  TGppConditionalDefines = set of TGppConditionalDefine;

  TGppConditionalDefineInfo = record
    Tag  : TGppConditionalDefine;
    Value: string;
  end; { TGppConditionalDefineInfo }

  TGppPrefConditionalDefines = class;

  ///<summary>Enumerator listing all active conditional defines in TGppPrefConditionalDefines.</summary>
  ///<since>2007-02-26</since>
  TGppActiveConditionalDefinesEnumerator = class
  private
    acdeDefines    : TGppPrefConditionalDefines;
    acdeDefineIndex: integer;
  public
    constructor Create(defines: TGppPrefConditionalDefines);
    function  GetCurrent: string;
    function  MoveNext: boolean;
    property Current: string read GetCurrent;
  end; { TGppActiveConditionalDefinesEnumerator }

  ///<summary>Interface describing provider for enumerator listing all active conditional defines in TGppPrefConditionalDefines.</summary>
  ///<since>2007-02-26</since>
  IGppActiveConditionalDefines = interface
    function  AsString: string;
    function  GetEnumerator: TGppActiveConditionalDefinesEnumerator;
  end; { IGppActiveConditionalDefines }

  ///<summary>Provider for enumerator listing all active conditional defines in TGppPrefConditionalDefines.</summary>
  ///<since>2007-02-26</since>
  TGppActiveConditionalDefines = class(TInterfacedObject, IGppActiveConditionalDefines)
  private
    acdDefines: TGppPrefConditionalDefines;
  public
    constructor Create(defines: TGppPrefConditionalDefines);
    function  AsString: string;
    function  GetEnumerator: TGppActiveConditionalDefinesEnumerator;
  end; { TGppActiveConditionalDefines }

  TGppPrefConditionalDefines = class(TGpModelComponent)
  private
    FConditionalDefines: TStringList;
    FDefines           : TGppConditionalDefines;
    FDelphiVersion     : TGpKnownDelphiVersions;
    FUserDefines       : TStringList;
  protected
    procedure Add(tag: char; const value: string);
    procedure AddProjectDefines;
    procedure AddStandardCompilerDefines;
    procedure RebuildDefineList;
    function  GetActiveDefines: IGppActiveConditionalDefines;
    function  GetConditionalDefine(idxCondDef: integer): TGppConditionalDefineInfo;
    procedure NotifyUserDefinesChanged(Sender: TObject);
    procedure SetDefines(value: TGppConditionalDefines);
    procedure SetDelphiVersion(value: TGpKnownDelphiVersions);
    procedure SetUserDefines(const value: TStringList);
  public
    constructor Create(const parent: IGpModelComponent); override;
    destructor  Destroy; override;
    function Count: integer;
    function IndexOf(const value: string): integer;
    property ActiveDefines: IGppActiveConditionalDefines read GetActiveDefines;
    property ConditionalDefine[idxCondDef: integer]: TGppConditionalDefineInfo
      read GetConditionalDefine; default;
  published
    property DelphiVersion: TGpKnownDelphiVersions read FDelphiVersion
      write SetDelphiVersion default dvDelphi2007;
    property Defines: TGppConditionalDefines read FDefines write SetDefines
      default [cdCompiler, cdProject];
    property UserDefines: TStringList read FUserDefines write SetUserDefines;
  end; { TGppPrefConditionalDefines }

  TGppPrefExcludedUnits = class(TGpModelComponent)
  private
    FUnits: TStringList;
  protected
    procedure NotifyUnitsChanged(Sender: TObject);
    procedure SetUnits(const value: TStringList);
  public
    constructor Create(const parent: IGpModelComponent); override;
    destructor  Destroy; override;
  published
    property Units: TStringList read FUnits write SetUnits;
  end; { TGppPrefExcludedUnits }

  TGppBrowsingOption = (boShowAllFolders);
  TGppBrowsingOptions = set of TGppBrowsingOption;

  TGppInstrumentationOption = (ioAutoStartProfiling, ioInstrumentAssembler,
    ioKeepFileDateUnchanged);
  TGppInstrumentationOptions = set of TGppInstrumentationOption;

  TGppMarkerStyle = (msUnconditional, msConditional);

  TGppPrefInstrumentation = class(TGpModelComponent)
  private
    FBrowsingOptions       : TGppBrowsingOptions;
    FInstrumentationOptions: TGppInstrumentationOptions;
    FMarkerStyle           : TGppMarkerStyle;
  protected
    procedure SetBrowsingOptions(value: TGppBrowsingOptions);
    procedure SetInstrumentationOptions(value: TGppInstrumentationOptions);
  public
    constructor Create(const parent: IGpModelComponent); override;
  published
    property BrowsingOptions: TGppBrowsingOptions read FBrowsingOptions write
      SetBrowsingOptions;
    property MarkerStyle: TGppMarkerStyle read FMarkerStyle write FMarkerStyle default
      msUnconditional;
    property InstrumentationOptions: TGppInstrumentationOptions read FInstrumentationOptions
      write SetInstrumentationOptions default [ioAutoStartProfiling];
  end; { TGppPrefInstrumentation }

  TGppPreferences = class(TGpModelComponent)
  private
    gpAnalysis          : TGppPrefAnalysis;
    gpConditionalDefines: TGppPrefConditionalDefines;
    gpExcludedUnits     : TGppPrefExcludedUnits;
    gpInstrumentation   : TGppPrefInstrumentation;
  public
    constructor Create;
    destructor  Destroy; override;
  published
    property Analysis: TGppPrefAnalysis read gpAnalysis;
    property ConditionalDefines: TGppPrefConditionalDefines read gpConditionalDefines;
    property ExcludedUnits: TGppPrefExcludedUnits read gpExcludedUnits;
    property Instrumentation: TGppPrefInstrumentation read gpInstrumentation;
  end; { TGppPreferences }

implementation

uses
  Windows,
  SysUtils,
  HVStringBuilder, 
  GpStuff,
  GpStreams;

const
  DEF_DELPHI  = 'D';
  DEF_CONSOLE = 'C';
  DEF_PROJECT = 'P';
  DEF_USER    = 'U';

{ TGppPrefAnalysis }

constructor TGppPrefAnalysis.Create(const parent: IGpModelComponent);
begin
  inherited;
  FOptions := [aoHideInactiveMethods];
end; { TGppPrefAnalysis.Create }

{ TGppActiveConditionalDefinesEnumerator }

constructor TGppActiveConditionalDefinesEnumerator.Create(
  defines: TGppPrefConditionalDefines);
begin
  acdeDefines := defines;
  acdeDefineIndex := -1;
end; { TGppActiveConditionalDefinesEnumerator.Create }

function TGppActiveConditionalDefinesEnumerator.GetCurrent: string;
begin
  Result := acdeDefines[acdeDefineIndex].Value;
end; { TGppActiveConditionalDefinesEnumerator.GetCurrent }

function TGppActiveConditionalDefinesEnumerator.MoveNext: boolean;
begin
  repeat
    Inc(acdeDefineIndex);
  until (acdeDefineIndex >= acdeDefines.Count) or
        (acdeDefines[acdeDefineIndex].Tag in acdeDefines.Defines);
  Result := (acdeDefineIndex < acdeDefines.Count);
end; { TGppActiveConditionalDefinesEnumerator.MoveNext }

{ TGppActiveConditionalDefines }

function TGppActiveConditionalDefines.AsString: string;
var
  sb          : StringBuilder;
  sConditional: string;
begin
  sb := StringBuilder.Create;
  try
    for sConditional in Self do
      sb.Append(sConditional);
    Result := sb.ToString;
  finally FreeAndNil(sb); end;
end; { TGppActiveConditionalDefines.AsString }

constructor TGppActiveConditionalDefines.Create(defines: TGppPrefConditionalDefines);
begin
  acdDefines := defines;
end; { TGppActiveConditionalDefines.Create }

function TGppActiveConditionalDefines.GetEnumerator: TGppActiveConditionalDefinesEnumerator;
begin
  Result := TGppActiveConditionalDefinesEnumerator.Create(acdDefines);
end; { TGppActiveConditionalDefines.GetEnumerator }

{ TGppPrefConditionalDefines }

constructor TGppPrefConditionalDefines.Create(const parent: IGpModelComponent);
begin
  inherited;
  FDefines := [cdCompiler, cdProject];
  FUserDefines := TStringList.Create;
  FUserDefines.OnChange := NotifyUserDefinesChanged;
  FConditionalDefines := TStringList.Create;
  FConditionalDefines.Sorted := true;
  FConditionalDefines.Duplicates := dupIgnore;
  FDelphiVersion := NewestSupportedDelphi;
  RebuildDefineList;
end; { TGppPrefConditionalDefines.Create }

destructor TGppPrefConditionalDefines.Destroy;
begin
  FreeAndNil(FConditionalDefines);
  FreeAndNil(FUserDefines);
  inherited Destroy;
end; { TGppPrefConditionalDefines.Destroy }

function TGppPrefConditionalDefines.GetActiveDefines: IGppActiveConditionalDefines;
begin
  Result := TGppActiveConditionalDefines.Create(Self); 
end; { TGppPrefConditionalDefines.GetActiveDefines }

function TGppPrefConditionalDefines.GetConditionalDefine(idxCondDef: integer):
  TGppConditionalDefineInfo;
begin
  Result.Value := FConditionalDefines[idxCondDef];
  case Result.Value[Length(Result.Value)] of
    DEF_DELPHI:  Result.Tag := cdCompiler;
    DEF_CONSOLE: Result.Tag := cdConsoleApp;
    DEF_PROJECT: Result.Tag := cdProject;
    DEF_USER:    Result.Tag := cdUser;
    else raise Exception.CreateFmt('Invalid conditional define tag: %s', [Result.Value]);
  end;
  Delete(Result.Value, Length(Result.Value), 1);
end; { TGppPrefConditionalDefines.GetConditionalDefine }

procedure TGppPrefConditionalDefines.Add(tag: char; const value: string);
begin
  FConditionalDefines.Add(value+tag);
end; { TGppPrefConditionalDefines.Add }

procedure TGppPrefConditionalDefines.AddProjectDefines;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TGppPrefConditionalDefines.AddProjectDefines
//    projcond := ReplaceAll(frmGpProfileMain.GetDOFSetting('Directories','Conditionals',''),',',';');
//    for i := 1 to NumElements(projcond,';',-1) do
//      AddDefine(NthEl(projcond,i,';',-1),DEF_PROJECT);
end; { TGppPrefConditionalDefines.AddProjectDefines }

procedure TGppPrefConditionalDefines.AddStandardCompilerDefines;
begin
//  if DelphiVersion = '' then
//    Exit;
  BeginUpdate;
  try
    Add(DEF_DELPHI, 'WIN32');
    Add(DEF_DELPHI, 'CPU386');
    case DelphiVersion of
      dvDelphi2:    Add(DEF_DELPHI, 'VER90');
      dvDelphi3:    Add(DEF_DELPHI, 'VER100');
      dvDelphi4:    Add(DEF_DELPHI, 'VER120');
      dvDelphi5:    Add(DEF_DELPHI, 'VER130');
      dvDelphi6:    Add(DEF_DELPHI, 'VER140');
      dvDelphi7:    Add(DEF_DELPHI, 'VER150');
      dvBDS2005:    Add(DEF_DELPHI, 'VER170');
      dvBDS2006:    Add(DEF_DELPHI, 'VER180');
      dvDelphi2007: Add(DEF_DELPHI, 'VER185');
    end;
    if DelphiVersion >= dvDelphi6 then begin
      Add(DEF_DELPHI, 'MSWINDOWS');
      Add(DEF_DELPHI, 'CONDITIONALEXPRESSIONS');
    end;
  finally EndUpdate; end;
end; { TGppPrefConditionalDefines.AddStandardCompilerDefines }

function TGppPrefConditionalDefines.Count: integer;
begin
  Result := FConditionalDefines.Count;
end; { TGppPrefConditionalDefines.Count }

function TGppPrefConditionalDefines.IndexOf(const value: string): integer;
begin
  for Result := 0 to Count - 1 do
    if SameText(ConditionalDefine[Result].Value, value) then
      Exit;
  Result := -1;
end; { TGppPrefConditionalDefines.IndexOf }

procedure TGppPrefConditionalDefines.NotifyUserDefinesChanged(Sender: TObject);
begin
  RebuildDefineList;
  MarkChanged;
end; { TGppPrefConditionalDefines.NotifyUserDefinesChanged }

procedure TGppPrefConditionalDefines.RebuildDefineList;
var
  sDefine: string;
begin
  FConditionalDefines.Clear;
  if cdCompiler in FDefines then
    AddStandardCompilerDefines;
  if cdConsoleApp in FDefines then
    Add(DEF_CONSOLE, 'CONSOLE');
  if cdProject in FDefines then
    AddProjectDefines;
  for sDefine in FUserDefines do
    Add(DEF_USER, sDefine);
end; { TGppPrefConditionalDefines.RebuildDefineList }

procedure TGppPrefConditionalDefines.SetDefines(value: TGppConditionalDefines);
begin 
  if FDefines <> value then begin
    FDefines := value;
    RebuildDefineList;
    MarkChanged;
  end;
end; { TGppPrefConditionalDefines.SetDefines }

procedure TGppPrefConditionalDefines.SetDelphiVersion(value: TGpKnownDelphiVersions);
begin               
  if FDelphiVersion <> value then begin
    FDelphiVersion := value;
    RebuildDefineList;
    MarkChanged;
  end;
end; { TGppPrefInstrumentation.SetDelphiVersion }

procedure TGppPrefConditionalDefines.SetUserDefines(const value: TStringList);
begin
  FUserDefines.Assign(value);
end; { TGppPrefConditionalDefines.SetUserDefines }

{ TGppPrefExcludedUnits }

constructor TGppPrefExcludedUnits.Create(const parent: IGpModelComponent);
begin
  inherited;
  FUnits := TStringList.Create;
  FUnits.OnChange := NotifyUnitsChanged;
end; { TGppPrefExcludedUnits.Create }

destructor TGppPrefExcludedUnits.Destroy;
begin
  FreeAndNil(FUnits);
  inherited Destroy;
end; { TGppPrefExcludedUnits.Destroy }

procedure TGppPrefExcludedUnits.NotifyUnitsChanged(Sender: TObject);
begin
  MarkChanged;
end; { TGppPrefExcludedUnits.NotifyUnitsChanged }

procedure TGppPrefExcludedUnits.SetUnits(const value: TStringList);
begin
  BeginUpdate;
  try
    FUnits.Clear;
    FUnits.AddStrings(value);
  finally EndUpdate; end;
end; { TGppPrefExcludedUnits.SetUnits }

{ TGppPrefInstrumentation }

constructor TGppPrefInstrumentation.Create(const parent: IGpModelComponent);
begin
  inherited;
  FMarkerStyle := msUnconditional;
  FInstrumentationOptions := [ioAutoStartProfiling];
end; { TGppPrefInstrumentation.Create }

procedure TGppPrefInstrumentation.SetBrowsingOptions(value: TGppBrowsingOptions);
begin
  if FBrowsingOptions <> value then begin
    FBrowsingOptions := value;
    MarkChanged;
  end;
end; { TGppPrefInstrumentation.SetBrowsingOptions }

procedure TGppPrefInstrumentation.SetInstrumentationOptions(value:
  TGppInstrumentationOptions);
begin
  if FInstrumentationOptions <> value then begin
    FInstrumentationOptions := value;
    MarkChanged;
  end;
end; { TGppPrefInstrumentation.SetInstrumentationOptions }

{ TGppPreferences }

constructor TGppPreferences.Create;
begin
  inherited Create(nil);
  gpAnalysis := TGppPrefAnalysis.Create(Self);
  gpInstrumentation := TGppPrefInstrumentation.Create(Self);
  gpConditionalDefines := TGppPrefConditionalDefines.Create(Self);
  gpExcludedUnits := TGppPrefExcludedUnits.Create(Self);
end; { TGppPreferences.Create }

destructor TGppPreferences.Destroy;
begin
  FreeAndNil(gpExcludedUnits);
  FreeAndNil(gpConditionalDefines);
  FreeAndNil(gpInstrumentation);
  FreeAndNil(gpAnalysis);
  inherited Destroy;
end; { TGppPreferences.Destroy }

end.
