{$MESSAGE WARN 'Internal version, do not redistribute!'}

{:GpProfile 2.0 timing module.
  @author Primoz Gabrijelcic
  @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2003, Primoz Gabrijelcic
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

  Tested with Delphi 2007.

  Author           : Primoz Gabrijelcic
  Creation date    : 1999
  Last modification: 2008
  Version          : 3.0

  </pre>}{

  History:
    3.0: 
      - Rebuild for GpProfile 2.
      - Dropped support for everything before Delphi 2007.
}

{$IFDEF MSWindows}{$WARN SYMBOL_PLATFORM OFF}{$WARN UNIT_PLATFORM OFF}{$ENDIF MSWindows}
{$IFNDEF MSWindows}{$MESSAGE FATAL 'GpProfile only supports Win32'}{$ENDIF MSWindows}
{$IFNDEF ConditionalExpressions}{$MESSAGE FATAL 'GpProfile is only compatible with Delphi 2007 and newer'}{$ENDIF ConditionalExpressions}
{$IF CompilerVersion < 18}{$MESSAGE FATAL 'GpProfile is only compatible with Delphi 2007 and newer'}{$IFEND CompilerVersion}

{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O-,P+,Q-,R-,S-,T-,U-,V+,W+,X+,Y+,Z1}

unit GpProf_gpt;

interface

procedure ProfilerStart;
procedure ProfilerStop;
procedure ProfilerStartThread;
procedure ProfilerEnterProc(unitID, procID: integer);
procedure ProfilerExitProc(unitID, procID: integer);
procedure ProfilerTerminate;

implementation

uses
  Windows,
  SysUtils,
  IniFiles,
  Classes,
  Contnrs,
  GpProf_gpt_refactor;

{$R *.res}

// type

var
  profiler: TProfiler;

procedure ProfilerEnterProc(unitID, procID: integer);
var
//  cnt : int64;
  pcnt: PInt64;
begin
//  QueryPerformanceCounter(cnt);
  pcnt := profiler.EnterProc(unitID, procID);
  if assigned(pcnt) then // profiling enabled etc
    QueryPerformanceCounter(pcnt^);
end; { ProfilerEnterProc }

procedure ProfilerExitProc(unitID, procID: integer);
var
  cnt: int64;
begin
  QueryPerformanceCounter(cnt);
  profiler.ExitProc(unitID, procID, cnt);
end; { ProfilerExitProc }

procedure ProfilerStart;
begin
  if not profiler.prfDisabled then
    profiler.prfRunning := true;
end; { ProfilerStart }

procedure ProfilerStop;
begin
  if not profiler.prfDisabled then
    profiler.prfRunning := false;
end; { ProfilerStop }

procedure ProfilerStartThread;
begin
  profiler.prfOnlyThread := GetCurrentThreadID;
  profiler.prfRunning := true;
end; { ProfilerStartThread }

procedure ProfilerTerminate;
begin
  if not profiler.prfInitialized then
    Exit;
  ProfilerStop;
  with profiler do begin
    prfInitialized := false;
    FlushCounters;
    SaveProfile(prfName);
    PostMessage(HWND_BROADCAST, prfDoneMsg, CMD_DONE, 0);
    Free;
  end;
end; { ProfilerTerminate }

initialization
  profiler := TProfiler.Create;
  if profiler.Initialize then begin // not disabled
    profiler.WriteCalibration;
    profiler.prfInitialized := true;
  end;
finalization
  ProfilerTerminate;
end.

