(*:GpProfile ID table.
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

unit gppIDT;

{$B-,H+,J+,Q-,T-,X+} //don't change!

interface

uses Classes;

type
  TIDTable = class
  private
    idClass: pointer;
    idProcs: pointer;
    idUnits: pointer;
  public
    constructor Create;
    destructor  Destroy; override;
    function    ConstructName(unitName, unitFullName, procName: string; firstLn: integer): integer;
    procedure Dump(strData: TStream);
  end;

implementation

uses
  Windows,
  SysUtils,
  IniFiles,
  GpHugeF,
  GpString,
  GpLists,
  GpStringHash,
  gppCommon,
  GpProf_Collector;

type
  TIDTE = class
    eName: PChar;
    eID  : integer;
    constructor Create(name: string; id: integer);
    destructor  Destroy; override;
  end; { TIDTE }

  TIDTUE = class(TIDTE)
    eQual: PChar;
    constructor Create(name, qual: string; id: integer); reintroduce;
    destructor  Destroy; override;
  end; { TIDTUE }

  TIDTCE = class(TIDTE)
    eUID: integer;
    constructor Create(name: string; id, uid: integer);
  end; { TIDTCE }

  TIDTPE = class(TIDTE)
    eUID    : integer;
    eCID    : integer;
    eFirstLn: integer;
    constructor Create(name: string; id, uid, cid, firstLn: integer);
  end; { TIDTPE }

  TIDTU = class
  strict private
    idDict: TGpStringDictionary;
  public
    constructor Create; 
    destructor  Destroy; override;
    function Insert(key, qual: string): cardinal;
    procedure Dump(var strData: TStream);
  end; { TIDTU }

  TIDTC = class
  strict private
    idDict: TGpStringDictionary;
  public
    constructor Create;
    destructor  Destroy; override;
    function Insert(key: string; uid: integer): cardinal;
    procedure Dump(var strData: TStream);
  end; { TIDTC }

  TIDTP = class
  strict private
    idDict: TGpStringDictionary;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Count: integer;
    function  Insert(key: string; uid, cid, firstLn: integer): cardinal;
    procedure Dump(var strData: TStream);
  end; { TIDTP }

{ globals }

procedure WriteTag(strData: TStream; tag: byte);
begin
  strData.Write(Tag, Sizeof(Byte));
end; { WriteTag }

procedure WriteInt(strData: TStream; int: integer);
begin
  strData.Write(Int, Sizeof(Integer));
end; { WriteInt }

procedure WriteString(strData: TStream; str: string);
begin
  WriteInt(strData, Length(str));
  if Length(str) > 0 then
    strData.Write(str[1], Length(str)+1); // write zero-terminated
end; { WriteString }

{ TIDTable }

constructor TIDTable.Create;
begin
  idClass := pointer(TIDTC.Create);
  idProcs := pointer(TIDTP.Create);
  idUnits := pointer(TIDTU.Create);
  inherited Create;
end; { TIDTable.Create }

destructor TIDTable.Destroy;
begin
  inherited Destroy;
  TIDTC(idClass).Free;
  TIDTP(idProcs).Free;
  TIDTU(idUnits).Free;
end; { TIDTable.Destroy }

function TIDTable.ConstructName(unitName, unitFullName, procName: string; firstLn: integer): integer;
var
  unitID: integer;
  clasID: integer;
  p     : integer;
begin
  unitID := TIDTU(idUnits).Insert(unitName, unitFullName);
  p := Pos('.',procName);
  if p > 0 then clasID := TIDTC(idClass).Insert(unitName+'.'+First(procName,p-1),unitID)
           else clasID := TIDTC(idClass).Insert(unitName+'.<>',unitID); // classless
  Result := TIDTP(idProcs).Insert(unitName+'.'+procName,unitID,clasID,firstLn);
end; { TIDTable.ConstructName }

procedure TIDTable.Dump(strData: TStream);
begin
  strData.Size := 0;
  WriteTag(strData, PR_PROCCOUNT);
  WriteInt(strData, TIDTP(idProcs).Count);
  TIDTU(idUnits).Dump(strData);
  TIDTC(idClass).Dump(strData);
  TIDTP(idProcs).Dump(strData);
end; { TIDTable.Dump }

{ TIDTU }

constructor TIDTU.Create;
begin
  inherited Create;
  idDict := TGpStringDictionary.Create(1000);
end; { TIDTU.Create }

destructor TIDTU.Destroy;
var
  iDict: integer;
  key  : string;
  pQual: int64;
begin
  for iDict := 0 to idDict.Count - 1 do begin
    idDict.Get(iDict, key, pQual);
    TGpString(pQual).Free;
  end;
  FreeAndNil(idDict);
  inherited Destroy;
end; { TIDTU.Destroy }

procedure TIDTU.Dump(var strData: TStream);
var
  iDict: integer;
  key  : string;
  pQual: int64;
begin
  WriteTag(strData, PR_UNITTABLE);
  WriteInt(strData, idDict.Count);
  for iDict := 0 to idDict.Count - 1 do begin
    idDict.Get(iDict, key, pQual);
    WriteString(strData, key);
    WriteString(strData, TGpString(pQual).Value);
  end;
end; { TIDTU.Dump }

function TIDTU.Insert(key, qual: string): cardinal;
var
  pQual: int64;
begin
  if not idDict.Find(key, Result, pQual) then
    Result := idDict.Add(key, int64(TGpString.Create(qual)));
end; { TIDTU.Insert }

{ TIDTC }

constructor TIDTC.Create;
begin
  inherited Create;
  idDict := TGpStringDictionary.Create(100);
end; { TIDTC.Create }

destructor TIDTC.Destroy;
begin
  FreeAndNil(idDict);
  inherited Destroy;
end; { TIDTC.Destroy }

procedure TIDTC.Dump(var strData: TStream);
var
  iDict: integer;
  key  : string;
  uid64: int64;
begin
  WriteTag(strData, PR_CLASSTABLE);
  WriteInt(strData, idDict.Count);
  for iDict := 0 to idDict.Count - 1 do begin
    idDict.Get(iDict, key, uid64);
    WriteString(strData, key);
    WriteInt(strData, uid64);
  end;
end; { TIDTC.Dump }

function TIDTC.Insert(key: string; uid: integer): cardinal;
var
  uid64: int64;
begin
  if not idDict.Find(key, Result, uid64) then
    Result := idDict.Add(key, uid);
end; { TIDTC.Insert }

{ TIDTP }

constructor TIDTP.Create;
begin
  inherited Create;
  idDict := TGpStringDictionary.Create(10000);
end; { TIDTP.Create }

destructor TIDTP.Destroy;
var
  iDict : integer;
  key   : string;
  pIDTPE: int64;
begin
  for iDict := 0 to idDict.Count - 1 do begin
    idDict.Get(iDict, key, pIDTPE);
    TIDTPE(pIDTPE).Free;
  end;
  FreeAndNil(idDict);
  inherited Destroy;
end; { TIDTP.Destroy }

function TIDTP.Count: integer;
begin
  Result := idDict.Count;
end; { TIDTP.Count }

procedure TIDTP.Dump(var strData: TStream);
var
  iDict : integer;
  key   : string;
  p     : TIDTPE;
  pIDTPE: int64;
begin
  WriteTag(strData, PR_PROCTABLE);
  WriteInt(strData, Count);
  for iDict := 0 to idDict.Count - 1 do begin
    idDict.Get(iDict, key, pIDTPE);
    p := TIDTPE(pIDTPE);
    WriteString(strData, key);
    WriteInt(strData, p.eUID);
    WriteInt(strData, p.eCID);
    WriteInt(strData, p.eFirstLn);
  end;
end; { TIDTP.Dump }

function TIDTP.Insert(key: string; uid, cid, firstLn: integer): cardinal;
var
  pIDTPE: int64;
begin
  if not idDict.Find(key, Result, pIDTPE) then
    Result := idDict.Add(key, int64(TIDTPE.Create('', 0, uid, cid, firstLn)));
end; { TIDTP.Insert }

{ TIDTE }

constructor TIDTE.Create(name: string; id: integer);
begin
  inherited Create;
  GetMem(eName,Length(name)+1);
  StrPCopy(eName,name);
  eID := id;
end; { TIDTE.Create }

destructor TIDTE.Destroy;
begin
  FreeMem(eName);
  inherited Destroy;
end; { TIDTE.Destroy }

{ TIDTPE }

constructor TIDTPE.Create(name: string; id, uid, cid, firstLn: integer);
begin
  inherited Create(name,id);
  eUID     := uid;
  eCID     := cid;
  eFirstLn := firstLn;
end; { TIDTPE.Create }

{ TIDTUE }

constructor TIDTUE.Create(name, qual: string; id: integer);
begin
  GetMem(eQual,Length(qual)+1);
  StrPCopy(eQual,qual);
  inherited Create(name, id);
end; { TIDTUE.Create }

destructor TIDTUE.Destroy;
begin
  inherited Destroy;
  FreeMem(eQual);
end; { TIDTUE.Destroy }

{ TIDTCE }

constructor TIDTCE.Create(name: string; id, uid: integer);
begin
  eUID := uid;
  inherited Create(name, id);
end; { TIDTCE.Create }

end.
