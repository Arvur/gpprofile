(*:Hardecoded info about Delphi/BDS (paths, versions, project storage ...).
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
   Creation date     : 2007-02-25
   Last modification : 2007-02-25
   Version           : 0.1
</pre>*)(*
   History:
     0.1: 2007-02-25
       - Created.
*)

unit GpDelphiInfo;

interface

type
  TGpKnownDelphiVersions = (dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5,
    dvDelphi6, dvDelphi7, dvDelphi8, dvBDS2005, dvBDS2006, dvTurbo2006, dvDelphi2007);
  TGpKnownDelphiVersionSet = set of TGpKnownDelphiVersions; 

const
  CGpKnownDelphiNames: array [TGpKnownDelphiVersions] of string = ('Delphi 2',
    'Delphi 3', 'Delphi 4', 'Delphi 5', 'Delphi 6', 'Delphi 7', 'Delphi 8',
    'BDS 2005', 'BDS 2006', 'Turbo Delphi 2006', 'Delphi 2007');

var
  CGpAllKnownDelphiVersions: TGpKnownDelphiVersionSet; 

  function DelphiNameToVersion(const name: string): TGpKnownDelphiVersions;

type
  {Project options reader. Only basic functionality is implemented.}
  IGpDelphiProjectOptionsReader = interface ['{A13DE12F-B204-429C-82BB-61C03D546DBB}']
    function  GetSearchPath: string;
  //
    function  OpenProject(const projectName: string): boolean;
    procedure CloseProject;
    function  ReadSetting(const section, key: string; defaultValue: string = ''): string;
    property SearchPath: string read GetSearchPath;
  end; { IGpDelphiProjectOptionsReader }

  {Delphi settings reader. Only basic functionality is implemented.}
  IGpDelphiSettingsReader = interface ['{0505BB67-9EFB-4D71-A744-A9CD0B6A6133}']
    function  GetLibraryPath: string;
    function  GetRootPath: string;
    function  GetRootRegistryPath: string;
  //
    function  OpenSettings(delphiVersion: TGpKnownDelphiVersions): boolean;
    procedure CloseSettings;
    function  ReadSetting(const key, value: string; defaultData: string = ''): string;
    property LibraryPath: string read GetLibraryPath;
    property RootPath: string read GetRootPath;
    property RootRegistryPath: string read GetRootRegistryPath;
  end;

  function CreateDelphiProjectOptionsReader: IGpDelphiProjectOptionsReader;
  function CreateDelphiSettingsReader: IGpDelphiSettingsReader; overload;
  function CreateDelphiSettingsReader(delphiVersion: TGpKnownDelphiVersions): IGpDelphiSettingsReader; overload;
  function ReplaceMacro(const path, macro: string): string;
  function ReplaceMacros(const path: string): string;

implementation

uses
  Windows,
  SysUtils,
  StrUtils,
  IniFiles,
  DSiWin32,
  GpStuff;

const
  CGpDelphiFamily: TGpKnownDelphiVersionSet = [dvDelphi2, dvDelphi3,
    dvDelphi4, dvDelphi5, dvDelphi6, dvDelphi7];

  CGpBDSFamily: TGpKnownDelphiVersionSet = [dvDelphi8, dvBDS2005, dvBDS2006, dvTurbo2006,
    dvDelphi2007];

type
  TGpDelphiProjectOptionsReader = class(TInterfacedObject, IGpDelphiProjectOptionsReader)
  private
    orDofFile: TIniFile;
  protected
    function  GetSearchPath: string;
  public
    destructor  Destroy; override;
    procedure CloseProject;
    function  OpenProject(const projectName: string): boolean;
    function  ReadSetting(const section: string; const key: string;
      defaultValue: string = ''): string;
  end; { TGpDelphiProjectOptionsReader }

  TGpDelphiSettingsReader = class(TInterfacedObject, IGpDelphiSettingsReader)
  private
    srDelphiVersion: TGpKnownDelphiVersions;
    srRegistry     : TDSiRegistry;
  protected
    function  GetLibraryPath: string;
    function  GetRootPath: string;
    function  GetRootRegistryPath: string;
  public
    destructor  Destroy; override;
    procedure CloseSettings;
    function  OpenSettings(delphiVersion: TGpKnownDelphiVersions): boolean;
    function  ReadSetting(const key: string; const name: string;
      defaultValue: string = ''): string;
    property RootPath: string read GetRootPath;
    property RootRegistryPath: string read GetRootRegistryPath;
  end;

{ globals }

function CreateDelphiProjectOptionsReader: IGpDelphiProjectOptionsReader;
begin
  Result := TGpDelphiProjectOptionsReader.Create;
end;

function CreateDelphiSettingsReader: IGpDelphiSettingsReader;
begin
  Result := TGpDelphiSettingsReader.Create;
end; { CreateDelphiSettingsReader }

function CreateDelphiSettingsReader(delphiVersion: TGpKnownDelphiVersions):
  IGpDelphiSettingsReader;
begin
  Result := CreateDelphiSettingsReader;
  if assigned(Result) then
    Result.OpenSettings(delphiVersion);
end; { CreateDelphiSettingsReader }

function DelphiNameToVersion(const name: string): TGpKnownDelphiVersions;
begin
  for Result := Low(TGpKnownDelphiVersions) to High(TGpKnownDelphiVersions) do
    if SameText(CGpKnownDelphiNames[Result], name) then
      Exit;
  raise Exception.CreateFmt('Unknown Delphi: %s', [name]);
end; { DelphiNameToVersion }

function ReplaceMacro(const path, macro: string): string;
begin
  Result := StringReplace(path, '$('+macro+')', DSiGetEnvironmentVariable(macro),
    [rfReplaceAll, rfIgnoreCase]);
end; { ReplaceMacro }

function ReplaceMacros(const path: string): string;
var
  posMacro: integer;
  posRight: integer;
begin
  Result := path;
  while (Asgn(posMacro, Pos('$(', Result)) > 0) and
        (Asgn(posRight, PosEx(')', Result, posMacro)) > 0)
  do
    Result := ReplaceMacro(Result, Copy(Result, posMacro + 2, posRight - posMacro - 2)); 
end; { ReplaceMacros }

{ TGpDelphiProjectOptionsReader }

procedure TGpDelphiProjectOptionsReader.CloseProject;
begin
  FreeAndNil(orDofFile);
end;

destructor TGpDelphiProjectOptionsReader.Destroy;
begin
  CloseProject;
  inherited;
end;

function TGpDelphiProjectOptionsReader.GetSearchPath: string;
begin
  Result := ReadSetting('Directories', 'SearchPath');
end;

function TGpDelphiProjectOptionsReader.OpenProject(const projectName: string):
  boolean;
var
  settingsFile: string;
begin
  CloseProject;
  settingsFile := ChangeFileExt(projectName, '.dof');
  if Asgn(Result, FileExists(settingsFile)) then
    orDofFile := TIniFile.Create(settingsFile);
end;

function TGpDelphiProjectOptionsReader.ReadSetting(const section, key: string;
  defaultValue: string): string;
begin
  if not assigned(orDofFile) then
    Result := ''
  else
    Result := orDofFile.ReadString(section, key, defaultValue);
end;

{ TGpDelphiSettingsReader }

procedure TGpDelphiSettingsReader.CloseSettings;
begin
  FreeAndNil(srRegistry);
end;

destructor TGpDelphiSettingsReader.Destroy;
begin
  CloseSettings;
  inherited;
end;

function TGpDelphiSettingsReader.GetLibraryPath: string;
var
  path1: string;
  path2: string;
begin
  path1 := ReadSetting('Library', 'SearchPath');  // older Delphis
  path2 := ReadSetting('Library', 'Search Path'); // newer Delphis
  if (path1 <> '') and (path2 <> '') then
    Result := path1 + ';' + path2
  else if path1 <> '' then
    Result := path1
  else
    Result := path2;
end;

function TGpDelphiSettingsReader.GetRootPath: string;
begin
  Result := ReadSetting('', 'RootDir');
end;

function TGpDelphiSettingsReader.GetRootRegistryPath: string;
begin
  case srDelphiVersion of
    dvDelphi2, dvDelphi3, dvDelphi4, dvDelphi5, dvDelphi6, dvDelphi7, dvDelphi8:
      Result := Format('\SOFTWARE\Borland\Delphi\%d.0',
        [Ord(srDelphiVersion) - Ord(dvDelphi2) + 2]);
    dvBDS2005:
       Result := '\SOFTWARE\Borland\BDS\3.0';
    dvBDS2006: 
       Result := '\SOFTWARE\Borland\BDS\4.0';
    dvDelphi2007:
       Result := '\SOFTWARE\Borland\BDS\5.0';
  end;
end;

function TGpDelphiSettingsReader.OpenSettings(delphiVersion:
  TGpKnownDelphiVersions): boolean;
begin
  CloseSettings;
  srDelphiVersion := delphiVersion;
  srRegistry := TDSiRegistry.Create;
  srRegistry.RootKey := HKEY_CURRENT_USER;
  if not Asgn(Result, srRegistry.OpenKey(RootRegistryPath, false)) then
    FreeAndNil(srRegistry)
  else
    srRegistry.CloseKey;
end;

function TGpDelphiSettingsReader.ReadSetting(const key, name: string;
  defaultValue: string): string;
begin
  if not (assigned(srRegistry) and srRegistry.OpenKey(RootRegistryPath + '\' + key, false))
  then
    Result := defaultValue
  else begin
    Result := srRegistry.ReadString(name, defaultValue);
    srRegistry.CloseKey;
  end;
end;

var
  delphi: TGpKnownDelphiVersions;

initialization
  CGpAllKnownDelphiVersions := [];
  for delphi := Low(TGpKnownDelphiVersions) to High(TGpKnownDelphiVersions) do
    Include(CGpAllKnownDelphiVersions, delphi);
end.
