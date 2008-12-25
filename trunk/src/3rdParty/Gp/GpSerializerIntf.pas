///<summary>Base serializer interface.
///<author>Primoz Gabrijelcic</author>
///<license>
///This software is distributed under the BSD license.
///
///Copyright (c) 2007, Primoz Gabrijelcic
///All rights reserved.
///
///Redistribution and use in source and binary forms, with or without modification,
///are permitted provided that the following conditions are met:
///- Redistributions of source code must retain the above copyright notice, this
///  list of conditions and the following disclaimer.
///- Redistributions in binary form must reproduce the above copyright notice,
///  this list of conditions and the following disclaimer in the documentation
///  and/or other materials provided with the distribution.
///- The name of the Primoz Gabrijelcic may not be used to endorse or promote
///  products derived from this software without specific prior written permission.
///
///THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
///ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
///WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
///DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
///ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
///(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
///LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
///ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
///(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
///SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
///</license>
///<remarks><para>
///   Author            : Primoz Gabrijelcic
///   Creation date     : 2007-12-06
///   Last modification : 2007-12-06
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2007-12-06
///       - Released.
///</para></remarks>

unit GpSerializerIntf;

interface

uses
  SysUtils,
  Classes;

type
  EGpSerializer = class(Exception);

  IGpSerializer = interface['{0538599C-676B-443B-B208-BCCF76313DF6}']
    procedure Copy(sourceObj, targetObj: TPersistent);
    procedure Restore(targetObj: TPersistent; storage: TStream); overload;
    procedure Restore(targetObj: TPersistent; storage: string); overload;
    procedure Serialize(sourceObj: TPersistent; storage: TStream); overload;
    function  Serialize(sourceObj: TPersistent): string; overload;
  end; { IGpSerializer }

  TGpSerializer = class abstract(TInterfacedObject, IGpSerializer)
    procedure Copy(sourceObj, targetObj: TPersistent); virtual;
    procedure Restore(targetObj: TPersistent; storage: TStream); overload; virtual; abstract;
    procedure Restore(targetObj: TPersistent; storage: string); overload; virtual;
    procedure Serialize(sourceObj: TPersistent; storage: TStream); overload; virtual; abstract;
    function  Serialize(sourceObj: TPersistent): string; overload; virtual;
  end; { TGpSerializer }

implementation

{ TGpSerializer }

procedure TGpSerializer.Copy(sourceObj, targetObj: TPersistent);
var
  storage: TMemoryStream;
begin
  storage := TMemoryStream.Create;
  try
    Serialize(sourceObj, storage);
    storage.Position := 0;
    Restore(targetObj, storage);
  finally FreeAndNil(storage); end;
end; { TGpSerializer.Copy }

procedure TGpSerializer.Restore(targetObj: TPersistent; storage: string);
var
  strStorage: TStringStream;
begin
  strStorage := TStringStream.Create(storage);
  try
    Restore(targetObj, strStorage);
  finally FreeAndNil(strStorage); end;
end; { TGpSerializer.Restore }

function TGpSerializer.Serialize(sourceObj: TPersistent): string;
var
  strStorage: TStringStream;
begin
  strStorage := TStringStream.Create('');
  try
    Serialize(sourceObj, strStorage);
    Result := strStorage.DataString;
  finally FreeAndNil(strStorage); end;
end; { TGpSerializer.Serialize }

end.
