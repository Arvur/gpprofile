(*:GpProfile source file editor.
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

unit gppFileEdit;

{$B-,H+,J+,Q-,T-,X+} //don't change!

// not really suitable for large scale file editing because of stupid
// underlaying data structure; that can be easiliy changed, though

interface

uses
  Classes;

type
  TFileEdit = class
    constructor Create(fileName: string);
    destructor  Destroy; override;
    procedure   Insert(atOffset: integer; what: string);
    procedure   Remove(fromOffset, toOffset: integer);
    procedure   Execute(keepDate: boolean);
  private
    editFile: string;
    editList: TStringList;
    procedure   Schedule(cmd: pointer);
  end;

implementation

uses
  Windows,
{$IFDEF DDP}
  GpExcept,
{$ENDIF}
  SysUtils;

type
  PFECmd = ^TFECmd;
  TFECmd = record
    fecCmd : (cmdInsert, cmdRemove);
    fecOfs1: integer;
    fecOfs2: integer;
    fecTxt : string;
  end;

{ TFileEdit }

  constructor TFileEdit.Create(fileName: string);
  begin
    editFile := fileName;
    editList := TStringList.Create;
    editList.Sorted := true;
  end; { TFileEdit.Create }

  destructor TFileEdit.Destroy;
  var
    i: integer;
  begin
    for i := 0 to editList.Count-1 do
      Dispose(PFECmd(editList.Objects[i]));
    editList.Free;
  end; { TFileEdit.Destroy }

  procedure TFileEdit.Execute(keepDate: boolean);
  var
    stream  : TMemoryStream;
    f       : file;
    i       : integer;
    fileDate: integer;

    function MakeCurP: pointer;
    begin
      Result := pointer(integer(stream.Memory)+stream.Position);
    end; { MakeCurP }

    procedure Remove(first,lastp1: integer);
    begin
      if first > stream.Position then BlockWrite(f,MakeCurP^,first-stream.Position);
      stream.Position := lastp1;
    end; { Remove }

    procedure Insert(position: integer; key: string);
    begin
      if position > stream.Position then begin
        BlockWrite(f,MakeCurP^,position-stream.Position);
        stream.Position := position;
      end;
      BlockWrite(f,key[1],Length(key));
    end; { Insert }

  begin { TFileEdit.Execute }
    fileDate := 0; // to keep compiler happy
    stream := TMemoryStream.Create;
    try
      stream.LoadFromFile(editFile);
      stream.Position := 0;
      Assign(f,editFile);
      Reset(f,1);
      if keepDate then
        fileDate := FileGetDate(TFileRec(f).Handle);
      Rewrite(f,1);
      try
        for i := 0 to editList.Count-1 do
          with PFECmd(editList.Objects[i])^ do begin
            if fecCmd = cmdInsert
              then Insert(fecOfs1,fecTxt)
              else Remove(fecOfs1,fecOfs2+1);
          end;
        BlockWrite(f,MakeCurP^,stream.Size-stream.Position);
        if keepDate then
          FileSetDate(TFileRec(f).Handle,fileDate);
      finally Close(f); end;
    finally stream.Free; end;
  end; { TFileEdit.Execute }

  procedure TFileEdit.Insert(atOffset: integer; what: string);
  var
    cmd: PFECmd;
  begin
    New(cmd);
    with cmd^ do begin
      fecCmd := cmdInsert;
      fecOfs1:= atOffset;
      fecTxt := what;
    end;
    Schedule(cmd);
  end; { TFileEdit.Insert }

  procedure TFileEdit.Remove(fromOffset, toOffset: integer);
  var
    cmd: PFECmd;
  begin
    New(cmd);
    with cmd^ do begin
      fecCmd := cmdRemove;
      fecOfs1:= fromOffset;
      fecOfs2:= toOffset;
    end;
    Schedule(cmd);
  end; { TFileEdit.Remove }

  procedure TFileEdit.Schedule(cmd: pointer);
  begin
    with PFECmd(cmd)^ do begin
      editList.AddObject(Format('%10d%1d',[fecOfs1,Ord(fecCmd = cmdInsert)]),cmd);
    end;
  end; { TFileEdit.Schedule }

end.
