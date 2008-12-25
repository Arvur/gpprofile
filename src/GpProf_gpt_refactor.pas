{$IFDEF MSWindows}{$WARN SYMBOL_PLATFORM OFF}{$WARN UNIT_PLATFORM OFF}{$ENDIF MSWindows}
{$IFNDEF MSWindows}{$MESSAGE FATAL 'GpProfile only supports Win32'}{$ENDIF MSWindows}
{$IFNDEF ConditionalExpressions}{$MESSAGE FATAL 'GpProfile is only compatible with Delphi 2007 and newer'}{$ENDIF ConditionalExpressions}
{$IF CompilerVersion < 18}{$MESSAGE FATAL 'GpProfile is only compatible with Delphi 2007 and newer'}{$IFEND CompilerVersion}

unit GpProf_gpt_refactor;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Contnrs;
  
//<GpProfH.pas:
const
  PR_DIGENDCG    = -1;
  PR_FREQUENCY   =  0; // .prf tags, never reuse them, always add new!
  PR_ENTERPROC   =  1;
  PR_EXITPROC    =  2;
  PR_UNITTABLE   =  3;
  PR_CLASSTABLE  =  4;
  PR_PROCTABLE   =  5;
  PR_PROCSIZE    =  6;
  PR_ENDDATA     =  7;
  PR_STARTDATA   =  8;
  PR_ENDHEADER   =  9;
  PR_COMPTICKS   = 10;
  PR_COMPTHREADS = 11;
  PR_PRFVERSION  = 12;
  PR_STARTCALIB  = 13;
  PR_ENDCALIB    = 14;
  PR_DIGEST      = 15;
  PR_DIGTHREADS  = 16;
  PR_DIGUNITS    = 17;
  PR_DIGCLASSES  = 18;
  PR_DIGPROCS    = 19;
  PR_DIGFREQ     = 20;
  PR_ENDDIGEST   = 21;
  PR_DIGESTVER   = 22;
  PR_DIGCALLG    = 23;
  PR_PROCCOUNT   = 24;

  CALIB_CNT = 1000;

  CMD_MESSAGE = 'GPPROFILE_COMMAND';
  CMD_DONE    = 0;

  PRF_VERSION   = 5;
  PRF_DIGESTVER = 3;

  RES_SETTINGS = 'gpp_instrumentation_settings';
  RES_DATA     = 'gpp_instrumentation_data';
//>

const
  BUF_SIZE     = 64 * 1024; //64*1024;

  MAX_THREADS  = 1024;

type
  Pint64 = ^int64;

  TResPacket = record
    rpTag         : byte;
    rpThread      : integer;
    rpUnitID      : integer;
    rpProcID      : integer;
    rpMeasure1    : int64;
    rpMeasure2    : int64;
    rpNullOverhead: int64;
  end; { TResPacket }

  TProcProxy = class
  private
    ppUnitID   : integer;
    ppProcID   : integer;
    ppDeadTime : int64;
    ppStartTime: int64;
    ppTotalTime: int64;
    ppChildTime: int64;
  public
    constructor Create(unitID, procID: integer);
    destructor  Destroy; override;
    procedure   Start(pkt: TResPacket);
    procedure   Stop(var pkt: TResPacket);
    procedure   UpdateDeadTime(pkt: TResPacket);
  end; { TProcProxy }

  TBaseArray = class
  private
    baItems: TObjectList;
  protected
    function  GetBaseItem(idxItem: integer): TObject;
    function  GetLength: integer;
    procedure SetBaseItem(idxItem: integer; value: TObject);
    procedure SetLength(Value: integer);
  public
    constructor Create;
    destructor  Destroy; override;
    property Items[idxItem: integer]: TObject
      read GetBaseItem write SetBaseItem; default;
    property Length: integer read GetLength write SetLength;
  end; { TBaseArray }

  TProcProxyArr = class(TBaseArray)
  protected
    function  GetItem(idxItem: integer): TProcProxy;
    procedure SetItem(idxItem: integer; Value: TProcProxy);
  public
    property Items[idxItem: integer]: TProcProxy read GetItem write SetItem; default;
  end; { TProcProxyArr }

  TActiveProcList = class
  private
    aplList : TProcProxyArr;
    aplCount: integer;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   Append(proxy: TProcProxy);
    procedure   LocateLast(unitID, procID: integer; var this,parent: TProcProxy);
    procedure   Remove(proxy: TProcProxy);
    procedure   UpdateDeadTime(pkt: TResPacket);
  end; { TActiveProcList }

  TProcEntry = class
    peProcTime     : int64;
    peProcTimeMin  : int64;
    peProcTimeMax  : int64;
    peProcChildTime: int64;
    peProcCnt      : integer;
    peRecLevel     : integer;
    constructor Create;
  end; { TProcEntry }

  TCallGraphEntry = class
    cgeProcTime     : int64;
    cgeProcTimeMin  : int64;
    cgeProcTimeMax  : int64;
    cgeProcChildTime: int64;
    cgeProcCnt      : integer;
    constructor Create;
  end; { TCallGraphEntry }

  TColumn = class
    clIndex: integer;
    clEntry: TCallGraphEntry;
    clNext : TColumn;
  end; { TColumn }

  TColumnHeader = class
    chFirst : TColumn;
    chCached: TColumn;
    destructor Destroy;
  end; { TColumnHeader }

  TColumnHeaderArr = class(TBaseArray)
  protected
    function  GetItem(idxItem: integer): TColumnHeader;
    procedure SetItem(idxItem: integer; value: TColumnHeader);
  public
    property Items[idxItem: integer]: TColumnHeader
      read GetItem write SetItem; default;
  end; { TColumnHeaderArr }

  TCallGraph = class
  private
    cgRows: TColumnHeaderArr;
  protected
    function  GetItem(row, col: integer): TCallGraphEntry;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure Allocate(row,col: integer);
    procedure Clear(newRows: integer);
    function  FirstColumn(row: integer): TColumn;
    function  HighRow: integer;
    property Items[row, col: integer]: TCallGraphEntry read GetItem; default;
  end; { TCallGraph }

  TProcEntryArr = class(TBaseArray)
  protected
    function  GetItem(idxItem: integer): TProcEntry;
    procedure SetItem(idxItem: integer; value: TProcEntry);
  public
    property Items[idxItem: integer]: TProcEntry
      read GetItem write SetItem; default;
  end; { TProcEntryArr }

  // Results for one thread
  TResults = class
  private
    resActiveProcs : TActiveProcList;
    resCalCounter  : integer;
    resCalDTime    : int64;
    resCalibration : boolean;
    resCalibrCnt   : integer;
    resCallGraph   : TCallGraph;
    resCalMax      : int64;
    resCalPkt      : TResPacket;
    resCalTime     : int64;
    resCalTime2    : int64;
    resCurCalibr   : integer;
    resName        : string;
    resNullError   : integer;
    resNullErrorAcc: integer;
    resNullOverhead: int64;
    resOldTicks    : int64;
    resProcedures  : TProcEntryArr; // used when profiling - keep it quick
    resProcSize    : integer;
  protected
    procedure AllocCGEntry(i,j: integer);
  public
    constructor Create(procCount: integer);
    destructor  Destroy; override;
    procedure AddEnterProc(pkt: TResPacket);
    procedure AddExitProc(pkt: TResPacket);
    procedure CalibrationStep(pkt1, pkt2: TResPacket);
    procedure EnterProc(proxy: TProcProxy; pkt: TResPacket);
    procedure ExitProc(proxy,parent: TProcProxy; pkt: TResPacket);
    procedure StartCalibration(calibCnt: integer);
    procedure StopCalibration;
    procedure UpdateRunningTime(proxy, parent: TProcProxy);
    property Name: string read resName;
  end; { TResults }

  TEnhStream = class
  private
    esStream: TStream;
  protected
    destructor Destroy; override;
    procedure  CheckTag(tag: byte);
    procedure  ReadInt(var int: integer);
    procedure  ReadShortstring(var str: string);
    procedure  ReadString(var str: string);
    procedure  ReadTag(var tag: byte);
    procedure  WriteInt(int: integer);
    procedure  WriteInt64(i64: int64);
    procedure  WriteString(str: string);
    procedure  WriteTag(tag: byte);
    property Stream: TStream read esStream write esStream;
  end; { TEnhStream }

  TThreadEntry = class
    teThread   : DWORD;
    teName     : string; // someday will be setable by API
    teTotalTime: int64;
    teTotalCnt : integer;
    teResults  : TResults;
    teCounter  : int64;
    teTagBuf   : byte;
    tePktBuf   : TResPacket;
    constructor Create(threadID: DWORD);       // needs to be executed inside critical section
    destructor  Destroy; override;
    procedure   FlushCounter;
    procedure   Initialize(procCount: integer); // can be executed outside critical section
  end; { TThreadEntry }

TThreadList = class
  private
    function GetItems(idx: byte): TThreadEntry;
  public
    tlThreads: array [0..255] of TThreadEntry;
    tlCount  : byte;
    tlSync   : TRTLCriticalSection;
    tlProcCnt: integer;
    constructor Create(procCount: integer);
    destructor  Destroy; override;
    function    Add(thread: DWORD): TThreadEntry;
    function    Locate(thread: DWORD): TThreadEntry;
    function    LocateAdd(thread: DWORD): TThreadEntry;
    property    Items[idx: byte]: TThreadEntry read GetItems; default;
  end; { TThreadList }

  TProfiler = class
    prfFreq              : int64;
    prfDoneMsg           : integer;
    prfModuleName        : string;
    prfName              : string;
    prfRunning           : boolean;
    prfLastTick          : Comp;
    prfOnlyThread        : DWORD;
    prfInitialized       : boolean;
    prfDisabled          : boolean;
    prfProfilingAutostart: boolean;
    prfTable             : TEnhStream;
    prfProcCount         : integer;
    prfThreads           : TThreadList;
    destructor Destroy; override;
    function EnterProc(unitID, procID: integer): PInt64;
    procedure  ExitProc(unitID, procID: integer; counter: int64){: Pint64};
    procedure  FlushCounters;
    function   Initialize: boolean;
    procedure  ReadIncSettings;
    procedure  SaveProfile(fileName: string);
    procedure  WriteCalibration;
  end; { TProfiler }

implementation

const
  CAPLQuantum = 100;
  CNullAccuracy = 1000;

function CombineNames(fName, newExt: string): string;
begin
  Result := Copy(fName,1,Length(fName)-Length(ExtractFileExt(fName)))+'.'+newExt;
end; { CombineNames }

{ TProcProxy }

constructor TProcProxy.Create(unitID, procID: integer);
begin
  inherited Create;
  ppUnitID    := unitID;
  ppProcID    := procID;
  ppDeadTime  := 0;
  ppStartTime := 0;
  ppTotalTime := 0;
  ppChildTime := 0;
end; { TProcProxy.Create }

destructor TProcProxy.Destroy;
begin
  inherited Destroy;
end; { TProcProxy.Destroy }

procedure TProcProxy.Start(pkt: TResPacket);
begin
  ppStartTime := pkt.rpMeasure2;
end; { TProcProxy.Start }

procedure TProcProxy.Stop(var pkt: TResPacket);
begin
  ppTotalTime := pkt.rpMeasure1-ppStartTime - ppDeadTime - ppChildTime - pkt.rpNullOverhead;
  pkt.rpNullOverhead := 2*pkt.rpNullOverhead;
  if ppTotalTime < 0 then begin // overcorrected
    ppTotalTime := 0;
    pkt.rpNullOverhead := pkt.rpNullOverhead + ppTotalTime;
  end;
end; { TProcProxy.Stop }

procedure TProcProxy.UpdateDeadTime(pkt: TResPacket);
begin
  ppDeadTime := ppDeadTime + (pkt.rpMeasure2-pkt.rpMeasure1) + pkt.rpNullOverhead;
end; { TProcProxy.UpdateDeadTime }

{ TBaseArray| }

constructor TBaseArray.Create;
begin
  inherited;
  baItems := TObjectList.Create;
end; { TBaseArray.Create }

destructor TBaseArray.Destroy;
begin
  FreeAndNil(baItems);
  inherited;
end; { TBaseArray.Destroy }

function TBaseArray.GetBaseItem(idxItem: integer): TObject;
begin
  Result := baItems[idxItem];
end; { TBaseArray.GetBaseItem }

function TBaseArray.GetLength: integer;
begin
  Result := baItems.Count;
end; { TBaseArray.GetLength }

procedure TBaseArray.SetBaseItem(idxItem: integer; value: TObject);
begin
  baItems[idxItem] := value;
end; { TBaseArray.SetBaseItem }

procedure TBaseArray.SetLength(Value: integer);
begin
  while baItems.Count < Value do
    baItems.Add(nil);
  while baItems.Count > Value do
    baItems.Delete(baItems.Count-1);
end; { TBaseArray.SetLength }

{ TProcProxyArr }

function TProcProxyArr.GetItem(idxItem: integer): TProcProxy;
begin
  Result := TProcProxy(inherited Items[idxItem]);
end; { TProcProxyArr.GetItem }

procedure TProcProxyArr.SetItem(idxItem: integer; Value: TProcProxy);
begin
  inherited Items[idxItem] := Value;
end; { TProcProxyArr.SetItem }

{ TActiveProcList }

procedure TActiveProcList.Append(proxy: TProcProxy);
begin
  if aplCount >= aplList.Length then
    aplList.Length := aplList.Length + CAPLQuantum;
  aplList[aplCount] := proxy;
  Inc(aplCount);
end; { TActiveProcList.Append }

constructor TActiveProcList.Create;
begin
  inherited;
  aplList := TProcProxyArr.Create;
  aplList.Length := CAPLQuantum;
  aplCount := 0;
end; { TActiveProcList.Create }

destructor TActiveProcList.Destroy;
begin
  FreeAndNil(aplList);
  inherited;
end; { TActiveProcList.Destroy }

procedure TActiveProcList.LocateLast(unitID, procID: integer;
  var this, parent: TProcProxy);
var
  i: integer;
begin
  for i := aplCount-1 downto 0 do begin
    if (aplList[i].ppUnitID = unitID) and (aplList[i].ppProcID = procID) then
    begin
      this := aplList[i];
      if i > 0 then
        parent := aplList[i-1]
      else
        parent := nil;
      Exit;
    end;
  end;
  this   := nil;
  parent := nil;
end; { TActiveProcList.LocateLast }

procedure TActiveProcList.Remove(proxy: TProcProxy);
var
  i: integer;
begin
  for i := aplCount-1 downto 0 do begin // should be the last, but ...
    if aplList[i] = proxy then begin
      aplCount := i;
      Exit;
    end;
  end;
  raise Exception.Create('gppResults.TActiveProcList.Remove: Entry not found!');
end; { TActiveProcList.Remove }

procedure TActiveProcList.UpdateDeadTime(pkt: TResPacket);
var
  i: integer;
begin
  for i := aplCount-1 downto 0 do
    aplList[i].UpdateDeadTime(pkt);
end; { TActiveProcList.UpdateDeadTime }

{ XProcEntry }

constructor TProcEntry.Create;
begin
  inherited;
  peProcTimeMin := High(int64);
end; { XProcEntry.Create }

{ XCallGraphEntry }

constructor TCallGraphEntry.Create;
begin
  inherited;
  cgeProcTimeMin := High(int64);
end; { XCallGraphEntry.Create }

{ XColumnHeader }

destructor TColumnHeader.Destroy;
var
  q: TColumn;
  p: TColumn;
begin
  p := chFirst;
  while assigned(p) do begin
    q := p.clNext;
    p.clEntry.Free;
    p.Free;
    p := q;
  end; //while
  inherited;
end; { XColumnHeader.Destroy }

{ TColumnHeaderArr }

function TColumnHeaderArr.GetItem(idxItem: integer): TColumnHeader;
begin
  Result := TColumnHeader(inherited Items[idxItem]);
end; { TColumnHeaderArr.GetItem }

procedure TColumnHeaderArr.SetItem(idxItem: integer; value: TColumnHeader);
begin
  inherited Items[idxItem] := value;
end; { TColumnHeaderArr.SetItem }

{ TCallGraph }

procedure TCallGraph.Allocate(row, col: integer);
var
  cell: TColumn;

  procedure InsertFrom(p: TColumn);
  begin
    while assigned(p.clNext) and (p.clNext.clIndex < col) do
      p := p.clNext;
    cell.clNext := p.clNext;
    p.clNext := cell;
  end; { InsertFrom }

begin { TCallGraph.Allocate }
  cell := TColumn.Create;
  cell.clIndex := col;
  cell.clEntry := TCallGraphEntry.Create;
  with cgRows[row] do begin
    if not assigned(chFirst) then
      chFirst := cell
    else begin
      if chFirst.clIndex > col then begin
        cell.clNext := chFirst;
        chFirst := cell;
      end
      else if chCached.clIndex < col then
        InsertFrom(chCached)
      else
        InsertFrom(chFirst);
    end;
    chCached := cell;
  end; //with
end; { TCallGraph.Allocate }

procedure TCallGraph.Clear(newRows: integer);
var
  iRow: integer;
begin
  for iRow := 0 to HighRow do
    cgRows[iRow].Free;
  cgRows.Length := newRows;
  for iRow := 0 to HighRow do 
    cgRows[iRow] := TColumnHeader.Create;
end; { TCallGraph.Clear }

constructor TCallGraph.Create;
begin
  inherited;
  cgRows := TColumnHeaderArr.Create;
end; { TCallGraph.Create }

destructor TCallGraph.Destroy;
begin
  Clear(0);
  FreeAndNil(cgRows);
  inherited;
end; { TCallGraph.Destroy }

function TCallGraph.FirstColumn(row: integer): TColumn;
begin
  Result := cgRows[row].chFirst;
end; { TCallGraph.FirstColumn }

function TCallGraph.GetItem(row, col: integer): TCallGraphEntry;
var
  header: TColumnHeader;

  function SearchFrom(p: TColumn): TCallGraphEntry;
  begin
    while assigned(p.clNext) and (p.clNext.clIndex <= col) do
      p := p.clNext;
    if p.clIndex = col then
      Result := p.clEntry
    else
      Result := nil;
    header.chCached := p;
  end; { TCallGraph.SearchFrom }

begin
  header := cgRows[row];
  if not assigned(header.chFirst) then
    Result := nil
  else begin
    if header.chCached.clIndex = col then
      Result := header.chCached.clEntry
    else if header.chCached.clIndex < col then
      Result := SearchFrom(header.chCached)
    else
      Result := SearchFrom(header.chFirst);
  end;
end; { TCallGraph.GetItem }

function TCallGraph.HighRow: integer;
begin
  Result := cgRows.Length-1;
end; { TCallGraph.HighRow }

{ TProcEntryArr }

function TProcEntryArr.GetItem(idxItem: integer): TProcEntry;
begin
  Result := TProcEntry(inherited Items[idxItem]);
end; { TProcEntryArr.GetItem }

procedure TProcEntryArr.SetItem(idxItem: integer; value: TProcEntry);
begin
  inherited Items[idxItem] := value;
end; { TProcEntryArr.SetItem }

{ TResults }

procedure TResults.AddEnterProc(pkt: TResPacket);
var
  proxy: TProcProxy;
begin
  if resCalibration then
    pkt := resCalPkt
  else begin
    proxy := TProcProxy.Create(pkt.rpUnitID, pkt.rpProcID);
    EnterProc(proxy,pkt);
  end;
end; { TResults.AddEnterProc }

procedure TResults.AddExitProc(pkt: TResPacket);
var
  proxy : TProcProxy;
  parent: TProcProxy;
begin
  if resCalibration then
    CalibrationStep(resCalPkt, pkt)
  else begin
    resActiveProcs.LocateLast(pkt.rpUnitID, pkt.rpProcID, proxy, parent);
    if not assigned(proxy) then
      raise Exception.Create('gppResults.TResults.AddExitProc: Entry not found!');
    ExitProc(proxy, parent, pkt);
    proxy.Destroy;
  end;
end; { TResults.AddExitProc }

procedure TResults.AllocCGEntry(i, j: integer);
begin
  if resCallGraph[i,j] = nil then
    resCallGraph.Allocate(i,j);
end; { TResults.AllocCGEntry }

procedure TResults.CalibrationStep(pkt1, pkt2: TResPacket);
begin
  Inc(resCurCalibr);
  if resCurCalibr < resCalibrCnt then begin
    resCalDTime := pkt2.rpMeasure1-pkt1.rpMeasure2;
    if resCalDTime < resCalTime then
      resCalTime := resCalDTime;
  end
  else begin
    if resCurCalibr = resCalibrCnt then begin
      resCalMax := 2*resCalTime+1;
      resCalCounter := 0;
      resCalTime := 0;
      resCalTime2 := 0;
    end;
    resCalDTime := pkt2.rpMeasure1-pkt1.rpMeasure2;
    if resCalDTime <= resCalMax then begin
      Inc(resCalCounter);
      resCalTime := resCalTime + resCalTime2;
      resCalTime2 := resCalDTime;
    end;
  end;
end; { TResults.CalibrationStep }

constructor TResults.Create(procCount: integer);
begin
  inherited Create;
  resOldTicks := -1;
  resProcedures := TProcEntryArr.Create;
  resProcedures.Length := procCount;
  resCallGraph := TCallGraph.Create;
  resActiveProcs := TActiveProcList.Create;
  resCallGraph.Clear(procCount+1);
end; { TResults.Create }

destructor TResults.Destroy;
begin
  FreeAndNil(resActiveProcs);
  FreeAndNil(resProcedures);
  FreeAndNil(resCallGraph);
  inherited;
end; { TResults.Destroy }

procedure TResults.EnterProc(proxy: TProcProxy; pkt: TResPacket);
begin
  // update dead time in all active procedures
  // insert proxy object into active procedure queue
  // increment recursion level
  pkt.rpNullOverhead := 0;
  resActiveProcs.UpdateDeadTime(pkt);
  proxy.Start(pkt);
  resActiveProcs.Append(proxy);
  Inc(resProcedures[proxy.ppProcID].peRecLevel);
end; { TResults.EnterProc }

procedure TResults.ExitProc(proxy,parent: TProcProxy; pkt: TResPacket);
begin
  // decrement recursion level
  // remove proxy object from active procedure queue
  // update proxy end time
  // update time in active procedures from the same thread
  // update dead time in all active procedures
  Dec(resProcedures[proxy.ppProcID].peRecLevel);
  resActiveProcs.Remove(proxy);
  pkt.rpNullOverhead := resNullOverhead;
  resNullErrorAcc := resNullErrorAcc + resNullError;
  if resNullErrorAcc > CNullAccuracy then begin
    pkt.rpNullOverhead := pkt.rpNullOverhead + 1;
    Dec(resNullErrorAcc,CNullAccuracy);
  end;
  proxy.Stop(pkt);
  UpdateRunningTime(proxy, parent);
  resActiveProcs.UpdateDeadTime(pkt);
end; { TResults.ExitProc }

procedure TResults.StartCalibration(calibCnt: integer);
begin
  resCalibrCnt := calibCnt div 50;
  resCurCalibr := 0;
  resCalibration := true;
  resCalTime := High(int64);
end; { TResults.StopCalibration }

procedure TResults.StopCalibration;
begin
  resCalibration  := false;
  resNullOverhead := Round(CNullAccuracy*resCalTime/(resCalCounter-1));
  resNullError    := resNullOverhead mod CNullAccuracy;
  resNullOverhead := resNullOverhead div CNullAccuracy;
  resNullErrorAcc := 0;
end; { TResults.StopCalibration }

procedure TResults.UpdateRunningTime(proxy, parent: TProcProxy);
begin
  with resProcedures[proxy.ppProcID] do begin
    if assigned(parent) then
      parent.ppChildTime := parent.ppChildTime + proxy.ppTotalTime + proxy.ppChildTime;
    peProcTime := peProcTime + proxy.ppTotalTime;
    if proxy.ppTotalTime < peProcTimeMin then
      peProcTimeMin := proxy.ppTotalTime;
    if proxy.ppTotalTime > peProcTimeMax then
      peProcTimeMax := proxy.ppTotalTime;
    if peRecLevel = 0 then begin
      peProcChildTime := peProcChildTime + proxy.ppChildTime;
      peProcChildTime := peProcChildTime + proxy.ppTotalTime;
    end;
    Inc(peProcCnt);
  end;
  if assigned(parent) then begin
    AllocCGEntry(parent.ppProcID, proxy.ppProcID);
    with resCallGraph[parent.ppProcID, proxy.ppProcID] do begin
      cgeProcTime := cgeProcTime + proxy.ppTotalTime;
      if proxy.ppTotalTime < cgeProcTimeMin then
        cgeProcTimeMin := proxy.ppTotalTime;
      if proxy.ppTotalTime > cgeProcTimeMax then
        cgeProcTimeMax := proxy.ppTotalTime;
      if resProcedures[proxy.ppProcID].peRecLevel = 0 then begin
        cgeProcChildTime := cgeProcChildTime + proxy.ppChildTime;
        cgeProcChildTime := cgeProcChildTime + proxy.ppTotalTime;
      end
      else if (resProcedures[proxy.ppProcID].peRecLevel = 1) and
              (parent.ppProcID = proxy.ppProcID) then
      begin
        cgeProcChildTime := cgeProcChildTime + proxy.ppChildTime;
        cgeProcChildTime := cgeProcChildTime + proxy.ppTotalTime;
      end;
      Inc(cgeProcCnt);
    end;
  end;
end; { TResults.UpdateRunningTime }

{ TEnhStream }

procedure TEnhStream.CheckTag(tag: byte);
var
  fileTag: byte;
begin
  ReadTag(fileTag);
  if tag <> fileTag then raise Exception.Create('File corrupt!');
end; { TEnhStream.CheckTag }

destructor TEnhStream.Destroy;
begin
  esStream.Free;
end; { TEnhStream.Destroy }

procedure TEnhStream.ReadInt(var int: integer);
begin
  esStream.Read(int,SizeOf(integer));
end; { TEnhStream.ReadInt }

procedure TEnhStream.ReadShortstring(var str: string);
var
  s: shortstring;
begin
  esStream.Read(s[0],1);
  esStream.Read(s[1],Ord(s[0]));
  str := s;
end; { TEnhStream.ReadShortstring }

procedure TEnhStream.ReadString(var str: string);
var
  bStr: TBytes;
  len : integer;
begin
  ReadInt(len);
  if len = 0 then
    str := ''
  else begin
    SetLength(str, len);
    esStream.Read(str[1], len+1); // read zero-termination char too
  end;
end; { TEnhStream.ReadString }

procedure TEnhStream.ReadTag(var tag: byte);
begin
  esStream.Read(tag,SizeOf(byte));
end; { TEnhStream.ReadTag }

procedure TEnhStream.WriteInt(int: integer);
begin
  esStream.Write(int,SizeOf(integer));
end; { TEnhStream.WriteInt }

procedure TEnhStream.WriteInt64(i64: int64);
begin
  esStream.Write(i64,SizeOf(int64));
end; { TEnhStream.WriteInt64 }

procedure TEnhStream.WriteString(str: string);
var
  bStr: TBytes;
begin
  WriteInt(Length(str));
  if Length(str) > 0 then 
    esStream.Write(str[1],Length(str)+1); // write zero-terminated
end; { TEnhStream.WriteString }

procedure TEnhStream.WriteTag(tag: byte);
var
  bStr: array [1..1] of byte;
begin
  bStr[1] := tag;
  esStream.Write(bStr, 1);
end; { TEnhStream.WriteTag }

{ TThreadEntry }

constructor TThreadEntry.Create(threadID: DWORD);
begin
  inherited Create;
  teThread := threadID;
end; { TThreadEntry.Create }

destructor TThreadEntry.Destroy;
begin
  teResults.Free;
end; { TThreadEntry.Destroy }

procedure TThreadEntry.FlushCounter;
begin
  if teCounter <> 0 then begin
    tePktBuf.rpMeasure2 := teCounter;
    if teTagBuf = PR_ENTERPROC then
      teResults.AddEnterProc(tePktBuf)
    else
      teResults.AddExitProc(tePktBuf); 
    teCounter := 0;
  end;
end; { TThreadEntry.FlushCounter }

procedure TThreadEntry.Initialize(procCount: integer);
begin
  teResults := TResults.Create(procCount);
end; { TThreadEntry.Initialize }

{ TThreadList }

function TThreadList.Add(thread: DWORD): TThreadEntry;
begin
  if tlCount = 255 then
    raise Exception.Create('GpProfile can profile only up to 256 threads!');
  EnterCriticalSection(tlSync);
  try
    Inc(tlCount);
    tlThreads[tlCount] := TThreadEntry.Create(thread);
    Result := tlThreads[tlCount];
  finally LeaveCriticalSection(tlSync); end;
  Result.Initialize(tlProcCnt);
end; { TThreadList.Add }

constructor TThreadList.Create(procCount: integer);
begin
  inherited Create;
  InitializeCriticalSection(tlSync);
  tlProcCnt := procCount; 
  // add current thread - we'll definitely need it at least for calibration
  tlThreads[0] := TThreadEntry.Create(GetCurrentThreadID);
  tlThreads[0].Initialize(tlProcCnt);
end; { TThreadList.Create }

destructor TThreadList.Destroy;
var
  i: integer;
begin
  for i := 0 to tlCount do
    tlThreads[i].Free;
  DeleteCriticalSection(tlSync);
end; { TThreadList.Destroy }

function TThreadList.GetItems(idx: byte): TThreadEntry;
begin
  Result := tlThreads[idx];
end; { TThreadList.GetItems }

function TThreadList.Locate(thread: DWORD): TThreadEntry;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to tlCount do begin
    if tlThreads[i].teThread = thread then begin
      Result := tlThreads[i];
      Exit;
    end;
  end;
end; { TThreadList.Locate }

function TThreadList.LocateAdd(thread: DWORD): TThreadEntry;
begin
  Result := Locate(thread);
  if not assigned(Result) then
    Result := Add(thread);
end; { TThreadList.LocateAdd }

{ TProfiler }

destructor TProfiler.Destroy;
begin
  prfThreads.Free;
  prfTable.Free;
end; { TProfiler.Destroy }

function TProfiler.EnterProc(unitID, procID: integer): PInt64;
var
  ct: DWORD;
  te: TThreadEntry;
begin
  Result := nil;
  ct := GetCurrentThreadID;
  if prfRunning and ((prfOnlyThread = 0) or (prfOnlyThread = ct)) then begin
    te := prfThreads.LocateAdd(ct);
    with te do begin
      FlushCounter;
      teTagBuf := PR_ENTERPROC;
      tePktBuf.rpThread   := ct;
      tePktBuf.rpUnitID   := unitID;
      tePktBuf.rpProcID   := procID;
//      tePktBuf.rpMeasure1 := counter;
      Result := @tePktBuf.rpMeasure1;
    end; //with
  end
end; { TProfiler.EnterProc }

//function TProfiler.ExitProc(unitID, procID: integer; counter: int64): Pint64;
procedure TProfiler.ExitProc(unitID, procID: integer; counter: int64){: Pint64};
var
  ct: DWORD;
  te: TThreadEntry;
begin
  ct := GetCurrentThreadID;
  if prfRunning and ((prfOnlyThread = 0) or (prfOnlyThread = ct)) then begin
    te := prfThreads.LocateAdd(ct);
    with te do begin
      FlushCounter;
      teTagBuf := PR_EXITPROC;
      tePktBuf.rpThread   := ct;
      tePktBuf.rpUnitID   := unitID;
      tePktBuf.rpProcID   := procID;
      tePktBuf.rpMeasure1 := counter;
//      Result := @teCounter;
    end; //with
  end
//  else
//    Result := nil;
end; { TProfiler.ExitProc }

procedure TProfiler.FlushCounters;
var
  i: integer;
begin
  for i := 0 to prfThreads.tlCount do
    prfThreads.tlThreads[i].FlushCounter;
end; { TProfiler.FlushCounters }

function TProfiler.Initialize: boolean;
begin
  ReadIncSettings;
  if not prfDisabled then begin
    prfRunning      := prfProfilingAutostart;
    prfOnlyThread   := 0;
    prfLastTick     := -1;
    prfDoneMsg      := RegisterWindowMessage(CMD_MESSAGE);
    prfName         := CombineNames(prfModuleName, 'prf2');
    QueryPerformanceFrequency(prfFreq);
    Result := true;
  end
  else Result := false;
end; { TProfiler.Initialize }

procedure TProfiler.ReadIncSettings;
var
  buf    : array [0..256] of char;
  resStr : TResourceStream;
  resStrg: TStringList;
begin
  GetModuleFileName(HInstance, buf, 256);
  prfModuleName := string(buf);
  prfDisabled := true;
  prfProfilingAutostart := false;
  prfTable := TEnhStream.Create;
  try
    prfTable.Stream := TResourceStream.Create(HInstance, RES_DATA, RT_RCDATA);
  except prfTable.Stream := nil; end;
  if not assigned(prfTable.Stream) then begin
    MessageBox(0, PChar(Format('Cannot find %s resource! '+
      'Profiling will be disabled.', [RES_DATA])), 'GpProfile',
      MB_OK + MB_ICONERROR);
  end
  else begin
    prfTable.CheckTag(PR_PROCCOUNT);
    prfTable.ReadInt(prfProcCount);
    prfThreads := TThreadList.Create(prfProcCount);
    resStr := TResourceStream.Create(HInstance, RES_SETTINGS, RT_RCDATA);
    if not assigned(resStr) then
      MessageBox(0, PChar(Format('Cannot find %s resource! '+
        'Profiling will be disabled.', [RES_SETTINGS])), 'GpProfile',
        MB_OK + MB_ICONERROR)
    else begin
      try
        resStrg := TStringList.Create;
        try
          resStrg.LoadFromStream(resStr);
          prfProfilingAutostart := (StrToIntDef(resStrg.Values['ProfilingAutostart'],1) <> 0);
        finally resStrg.Free; end;
        prfDisabled := false;
      finally resStr.Free; end;
    end;
  end;
end; { TProfiler.ReadIncSettings }

procedure TProfiler.SaveProfile(fileName: string);
var
  p  : TColumn;
  i  : integer;
  j  : integer;
  prf: TEnhStream;
begin 
  prf := TEnhStream.Create;
  try
    with prf do begin
      Stream := TFileStream.Create(fileName,fmCreate);
      WriteTag(PR_PRFVERSION);
      WriteInt(PRF_VERSION);
      WriteTag(PR_FREQUENCY);
      WriteInt64(prfFreq);
      WriteTag(PR_ENDHEADER);
      prfTable.Stream.Position := 0;
      Stream.CopyFrom(prfTable.Stream, prfTable.Stream.Size);
      WriteTag(PR_DIGPROCS);
      WriteInt(prfThreads.tlCount+1);
      WriteInt(prfProcCount);
      for j := 1 to prfProcCount do begin
        for i := 0 to prfThreads.tlCount do begin
          with prfThreads[i].teResults.resProcedures[j] do begin
            WriteInt64(peProcTime);
            WriteInt64(peProcTimeMin);
            WriteInt64(peProcTimeMax);
            WriteInt64(peProcChildTime);
            WriteInt(peProcCnt);
          end; //with
        end; //for
      end; //for
      WriteTag(PR_DIGCALLG);
      for j := 0 to prfThreads.tlCount do begin
        with prfThreads[j].teResults do begin
          for i := 0 to resCallGraph.HighRow do begin
            p := resCallGraph.FirstColumn(i);
            if assigned(p) then begin
              WriteInt(i);
              while assigned(p) do begin
                WriteInt(p.clIndex);
                with p.clEntry do begin
                  WriteInt64(cgeProcTime);
                  WriteInt64(cgeProcTimeMin);
                  WriteInt64(cgeProcTimeMax);
                  WriteInt64(cgeProcChildTime);
                  WriteInt(cgeProcCnt);
                end;
                p := p.clNext;
              end; //while
              WriteInt(PR_DIGENDCG);
            end; //if
          end; //for
          WriteInt(PR_DIGENDCG);
        end; //with
      end; //for
      WriteTag(PR_ENDDIGEST);
    end; //with
  finally prf.Free; end;
end; { TProfiler.SaveProfile }

procedure TProfiler.WriteCalibration;         
var
  i  : integer;
  run: boolean;
begin
  run := prfRunning;
  prfRunning := true;
  prfThreads.tlThreads[0].teResults.StartCalibration(CALIB_CNT);
  for i := 1 to CALIB_CNT + 1 do begin
    EnterProc(0, 0);
    ExitProc(0, 0, 0);
  end;
  prfThreads.tlThreads[0].FlushCounter;
  prfThreads.tlThreads[0].teResults.StopCalibration;
  prfRunning := run;
end; { TProfiler.WriteCalibration }

end.

