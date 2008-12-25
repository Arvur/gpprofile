(*:GpProfile project model.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2007, Primoz Gabrijelcic
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
   Last modification : 2007-07-12
   Version           : 1.01
</pre>*)(*
   History:
     1.01: 2007-07-12
       - Added 'Instrumentation' aspect to the composite model.
*)

unit gppProjectModel;

interface

uses
  GpCompositeModel,
  GpManagedClass,
  gppParser,
  gppPreferencesModel;

type
  TGppProjectModelInstrumentingEvent = procedure(Sender: TObject; const unitName:
    string) of object;

  TGppProjectModelParsingEvent = procedure(Sender: TObject; const unitName:
    string) of object;

  IGppProjectInstrumentationAspect = interface(IGpModelComponent)
  ['{C4049CC5-173A-4049-B20F-53FD28E006A6}']
  end; { IGppProjectInstrumentationAspect }

  IGppProjectModel = interface(IGpModelComponent)
  ['{E3D19087-AACD-43D9-BC83-EBC077AE7D66}']
    function  GetName: string;
    function  GetProject: TProject;
    function  GetProjectPreferences: TGppPreferences;
    procedure SetName(const value: string);
    function  GetOnInstrumenting: TGppProjectModelInstrumentingEvent;
    function  GetOnParsing: TGppProjectModelParsingEvent;
    procedure SetOnInstrumenting(value: TGppProjectModelInstrumentingEvent);
    procedure SetOnParsing(value: TGppProjectModelParsingEvent);
  //
    procedure BeginUpdate;
    procedure ChangeClassInstrumentation(unitData: TUnit; const className: string; instrument: boolean);
    procedure ChangeInstrumentation(instrument: boolean);
    procedure ChangeMethodInstrumentation(unitData: TUnit; procData: TProc; instrument: boolean);
    procedure ChangeUnitInstrumentation(unitData: TUnit; instrument: boolean);
    procedure CloseProject;
    procedure EndUpdate;
    procedure InstrumentProjectSource(instrumentProjectFolderOnly: boolean);
    function  IsOpen: boolean;
    procedure OpenProject(const fileName: string);
    function  Parse: boolean;
    procedure SyncSource(unitData: TUnit; procData: TProc);
    property Name: string read GetName write SetName;
    property Project: TProject read GetProject;
    property ProjectPreferences: TGppPreferences read GetProjectPreferences;
    property OnInstrumenting: TGppProjectModelInstrumentingEvent read GetOnInstrumenting
      write SetOnInstrumenting;
    property OnParsing: TGppProjectModelParsingEvent read GetOnParsing
      write SetOnParsing;
  end; { IGppProjectModel }

function CreateModel: IGppProjectModel;

implementation

uses
  SysUtils,
  Contnrs,
  Classes,
  GpStuff,
  GpString,
  GpDelphiInfo,
  HVStringBuilder,
  gppCommon;

type
  TGppProjectInstrumentationAspect = class(TGpModelComponent, IGppProjectInstrumentationAspect)
  end; { TGppProjectInstrumentationAspect }

  TGppProjectModel = class(TGpModelComponent, IGppProjectModel, IGpManagedErrorHandling)
  private
    gpmErrorHandling        : IGpManagedErrorHandling;
    gpmInstrumentationAspect: IGppProjectInstrumentationAspect;
    gpmName                 : string;
    gpmOnInstrumenting      : TGppProjectModelInstrumentingEvent;
    gpmOnParsing            : TGppProjectModelParsingEvent;
    gpmProject              : TProject;
    gpmProjectPreferences   : TGppPreferences;
//    gpmUpdateCount          : integer;
  protected
    function  ExtractDefines: string;
    procedure ForwardInstrumentationEvents(fullName, unitName: string; parse: boolean);
    procedure ForwardParsingEvents(unitName: string);
    function  GetName: string;
    function  GetOnInstrumenting: TGppProjectModelInstrumentingEvent;
    function  GetOnParsing: TGppProjectModelParsingEvent;
    function  GetProject: TProject;
    function  GetProjectPreferences: TGppPreferences;
    function  GetSearchPath: string;
    procedure SetName(const value: string);
    procedure SetOnInstrumenting(value: TGppProjectModelInstrumentingEvent);
    procedure SetOnParsing(value: TGppProjectModelParsingEvent);
    property Error: IGpManagedErrorHandling read gpmErrorHandling
      implements IGpManagedErrorHandling;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure BeginUpdate;
    procedure ChangeClassInstrumentation(unitData: TUnit; const className: string; instrument: boolean);
    procedure ChangeInstrumentation(instrument: boolean);
    procedure ChangeMethodInstrumentation(unitData: TUnit; procData: TProc; instrument: boolean);
    procedure ChangeUnitInstrumentation(unitData: TUnit; instrument: boolean);
    procedure CloseProject;
    procedure EndUpdate;
    procedure InstrumentProjectSource(instrumentProjectFolderOnly: boolean);
    function  IsOpen: boolean;
    procedure OpenProject(const fileName: string);
    function  Parse: boolean;
    procedure SyncSource(unitData: TUnit; procData: TProc);
    property Name: string read GetName write SetName;
    property Project: TProject read GetProject;
    property ProjectPreferences: TGppPreferences read GetProjectPreferences;
    property OnInstrumenting: TGppProjectModelInstrumentingEvent read GetOnInstrumenting
      write SetOnInstrumenting;
    property OnParsing: TGppProjectModelParsingEvent read GetOnParsing
      write SetOnParsing;
  end; { TGppProjectModel }

{ globals }

function CreateModel: IGppProjectModel;
begin
  Result := TGppProjectModel.Create;
end; { CreateModel }

{ TGppProjectModel }

constructor TGppProjectModel.Create;
begin
  inherited Create(nil);
  gpmInstrumentationAspect := TGppProjectInstrumentationAspect.Create(Self as IGpModelComponent);
  gpmErrorHandling := TGpManagedError.Create;
  gpmProjectPreferences := TGppPreferences.Create;
end; { TGppProjectModel.Create }

destructor TGppProjectModel.Destroy;
begin
  FreeAndNil(gpmProjectPreferences);
  FreeAndNil(gpmProject);
  inherited Destroy;
end; { TGppProjectModel.Destroy }

procedure TGppProjectModel.BeginUpdate;
begin
  inherited;
  gpmInstrumentationAspect.BeginUpdate;
end; { TGppProjectModel.BeginUpdate }

procedure TGppProjectModel.ChangeClassInstrumentation(unitData: TUnit; const className: string;
  instrument: boolean);
begin
  Project.InstrumentClass(unitData, className, instrument);
  gpmInstrumentationAspect.MarkChanged;
end; { TGppProjectModel.InstrumentClass }

procedure TGppProjectModel.ChangeInstrumentation(instrument: boolean);
var
  pUnit   : pointer;
  unitList: TObjectList;
begin
  BeginUpdate;
  try
    unitList := TObjectList.Create;
    try
      Project.GetUnitList(unitList, false);
      for pUnit in unitList do
        ChangeUnitInstrumentation(TUnit(pUnit), instrument);
    finally FreeAndNil(unitList); end;
  finally EndUpdate; end;
end; { TGppProjectModel.ChangeInstrumentation }

procedure TGppProjectModel.ChangeMethodInstrumentation(unitData: TUnit; procData: TProc;
  instrument: boolean);
begin
  Project.InstrumentProc(unitData, procData, instrument);
  gpmInstrumentationAspect.MarkChanged;
end; { TGppProjectModel.InstrumentProc }

procedure TGppProjectModel.ChangeUnitInstrumentation(unitData: TUnit; instrument: boolean);
begin
  Project.InstrumentUnit(unitData, instrument);
  gpmInstrumentationAspect.MarkChanged;
end; { TGppProjectModel.InstrumentUnit }

procedure TGppProjectModel.CloseProject;
begin
  if assigned(gpmProject) then begin
    FreeAndNil(gpmProject);
    MarkChanged;
  end;
end; { TGppProjectModel.CloseProject }

procedure TGppProjectModel.EndUpdate;
begin
  gpmInstrumentationAspect.EndUpdate;
  inherited;
end; { TGppProjectModel.EndUpdate }

function TGppProjectModel.ExtractDefines: string;
var
  defines: StringBuilder;
  sDefine: string;
begin
  defines := StringBuilder.Create;
  try
    for sDefine in ProjectPreferences.ConditionalDefines.ActiveDefines do begin
      if defines.Length > 0 then
        defines.Append(';');
      defines.Append(sDefine);
    end;
    Result := defines.ToString;
  finally FreeAndNil(defines); end;
end; { TGppProjectModel.ExtractDefines }

procedure TGppProjectModel.ForwardInstrumentationEvents(fullName, unitName: string;
  parse: boolean);
begin
  if assigned(OnInstrumenting) then
    OnInstrumenting(Self, unitName);
end; { TGppProjectModel.ForwardInstrumentationEvents }

procedure TGppProjectModel.ForwardParsingEvents(unitName: string);
begin
  if assigned(OnParsing) then
    OnParsing(Self, unitName);
end; { TGppProjectModel.ForwardParsingEvents }

function TGppProjectModel.GetName: string;
begin
  Result := gpmName;
end; { TGppProjectModel.GetName }

function TGppProjectModel.GetOnInstrumenting: TGppProjectModelInstrumentingEvent;
begin
  Result := gpmOnInstrumenting;
end; { TGppProjectModel.GetOnInstrumenting }

function TGppProjectModel.GetOnParsing: TGppProjectModelParsingEvent;
begin
  Result := gpmOnParsing;
end; { TGppProjectModel.GetOnParsing }

function TGppProjectModel.GetProject: TProject;
begin
  Result := gpmProject;
end; { TGppProjectModel.GetProject }

function TGppProjectModel.GetProjectPreferences: TGppPreferences;
begin
  Result := gpmProjectPreferences;
end; { TGppProjectModel.GetProjectPreferences }

function TGppProjectModel.GetSearchPath: string;
var
  path: string;
begin
  path := '';
  with CreateDelphiProjectOptionsReader do
    if OpenProject(Name) then
      path := SearchPath;
  with CreateDelphiSettingsReader do
    if OpenSettings(ProjectPreferences.ConditionalDefines.DelphiVersion) then
      path := path + ';' + LibraryPath;
  Result := ReplaceMacros(path);
  if First(Result, 1) = ';' then
    Delete(Result, 1, 1);
  if Last(Result, 1) = ';' then
    Delete(Result, Length(Result), 1);
end; { TGppProjectModel.GetSearchPath }

procedure TGppProjectModel.InstrumentProjectSource(instrumentProjectFolderOnly: boolean);
var
  gptSettings: TStringList;
  gptUnitName: string;
  outDir     : string;
begin
  // TODO 1 -oPrimoz Gabrijelcic : Change all this string constants into constants
  // TODO 1 -oPrimoz Gabrijelcic : This is wrong!
  outDir := ExtractFilePath(Name);
  // Instead of this, output folder set in Delphi's project options must be used!
  // TODO 5 -oPrimoz Gabrijelcic : Is this the right 'module name'?
  gptUnitName := IncludeTrailingPathDelimiter(outDir) +
    ChangeFileExt(ExtractFileName(Name), '_gpprofile.pas');
  //!! gpprofname must be generated on the fly from the resource in baggage.rc
  gptSettings := TStringList.Create;
  try
    gptSettings.Values['Performance/ProfilingAutostart'] := IntToStr(Ord(
      ioAutoStartProfiling in ProjectPReferences.Instrumentation.InstrumentationOptions));
    gpmProject.Instrument(instrumentProjectFolderOnly, ForwardInstrumentationEvents,
      Ord(ProjectPreferences.Instrumentation.MarkerStyle),
      ioKeepFileDateUnchanged in ProjectPreferences.Instrumentation.InstrumentationOptions,
      ProjectPreferences.ConditionalDefines.ActiveDefines.AsString,
      '', // TODO 1 -oPrimoz Gabrijelcic : Search path must be passed here!
      ioInstrumentAssembler in ProjectPreferences.Instrumentation.InstrumentationOptions,
      false,
      gptSettings,
      gptUnitName,
      ChangeFileExt(gptUnitName, '.res')
    );
  finally FreeAndNil(gptSettings); end;
  // gpmProject.Instrument causes reparse which causes objects pointers stored in the
  // virtual tree node data to become invalid and that's why trees must be reloaded
  MarkChanged; // TODO 1 -oPrimoz Gabrijelcic : This is bad solution because it causes big change in GUI
  // the real question is why does .Instrument cause reparse
end; { TGppProjectModel.InstrumentProjectSource }

function TGppProjectModel.IsOpen: boolean;
begin
  Result := assigned(gpmProject);
end; { TGppProjectModel.IsOpen }

procedure TGppProjectModel.OpenProject(const fileName: string);
begin
  BeginUpdate;
  try
    CloseProject;
    gpmProject := TProject.Create(fileName);
    gpmName := fileName;
    MarkChanged;
  finally EndUpdate; end;
end; { TGppProjectModel.OpenProject }

function TGppProjectModel.Parse: boolean;
begin
// TODO 3 -oPrimoz Gabrijelcic : check gppMain.LoadDefaultPreferences
// TODO 3 -oPrimoz Gabrijelcic : RebuildDefines;
  gpmProject.Parse(ProjectPreferences.ExcludedUnits.Units, GetSearchPath,
    ExtractDefines, ForwardParsingEvents,
    IFF(ProjectPreferences.Instrumentation.MarkerStyle = msUnconditional, 0, 1),
    ioInstrumentAssembler in ProjectPreferences.Instrumentation.InstrumentationOptions,
    not (ProjectPreferences.ConditionalDefines.DelphiVersion in [dvDelphi2, dvDelphi3]),
    CProfUnitName);
  MarkChanged;
  Result := true; 
end; { TGppProjectModel.Parse }

procedure TGppProjectModel.SetName(const value: string);
begin
  gpmName := value;
end; { TGppProjectModel.SetName }

procedure TGppProjectModel.SetOnInstrumenting(value: TGppProjectModelInstrumentingEvent);
begin
  gpmOnInstrumenting := value;
end; { TGppProjectModel.SetOnInstrumenting }

procedure TGppProjectModel.SetOnParsing(value: TGppProjectModelParsingEvent);
begin
  gpmOnParsing := value;
end;

procedure TGppProjectModel.SyncSource(unitData: TUnit; procData: TProc);
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TGppProjectModel.SyncSource
end; { TGppProjectModel.SyncSource }

end.
