///<summary>Simple TPersistent-XML-TPersistent serialized.</summary>
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
///   Creation date     : 2006-09-01
///   Last modification : 2007-10-26
///   Version           : 1.0
///</para><para>
///   History:
///     1.0: 2007-10-26
///       - Released.
///</para></remarks>

unit GpXMLSerializer;

interface

uses
  Classes,
  SysUtils,
  GpSerializerIntf;

type
  EGpXMLSerializer = class(EGpSerializer);

  IGpXMLSerializer = interface(IGpSerializer) ['{2D834F2C-DCA1-4432-A670-4434FE10B527}']
  end; { IGpXMLSerializer }

  function CreateGpXMLSerializer: IGpXMLSerializer;

implementation

uses
  OmniXML,
  OmniXMLUtils,
  OmniXMLPersistent;

type
  TGpXMLSerializer = class(TGpSerializer, IGpXMLSerializer)
  public
    procedure Restore(targetObj: TPersistent; storage: TStream); overload; override;
    procedure Serialize(sourceObj: TPersistent; storage: TStream); overload; override;
  end; { TGpXMLSerializer }

{ globals }

function CreateGpXMLSerializer: IGpXMLSerializer;
begin
  Result := TGpXMLSerializer.Create;
end; { CreateGpXMLSerializer }

{ TGpXMLSerializer }

procedure TGpXMLSerializer.Restore(targetObj: TPersistent; storage: TStream);
var
  reader: TOmniXMLReader;
  xmlDoc: IXMLDocument;
begin
  xmlDoc := CreateXMLDoc;
  reader := TOmniXMLReader.Create(pfNodes);
  try
    if (not XMLLoadFromStream(xmlDoc, storage)) or (xmlDoc.DocumentElement = nil) then
      raise EGpXMLSerializer.CreateFmt('Invalid data passed to Restore(%s)',
        [targetObj.ClassName])
    else if not SameText(xmlDoc.DocumentElement.NodeName, targetObj.ClassName) then
      raise EGpXMLSerializer.CreateFmt(
        'Data for a different object (%s) passed to Restore(%s)',
        [xmlDoc.DocumentElement.NodeName, targetObj.ClassName])
    else
      reader.Read(targetObj, xmlDoc.DocumentElement);
  finally FreeAndNil(reader); end;
end; { TGpXMLSerializer.Restore }

procedure TGpXMLSerializer.Serialize(sourceObj: TPersistent; storage: TStream);
var
  writer: TOmniXMLWriter;
  xmlDoc: IXMLDocument;
begin
  xmlDoc := CreateXMLDoc;
  EnsureNode(xmlDoc, sourceObj.ClassName);
  writer := TOmniXMLWriter.Create(xmlDoc, pfNodes);
  try
    writer.Write(sourceObj, xmlDoc.DocumentElement, false, true, true);
    XMLSaveToStream(xmlDoc, storage);
  finally FreeAndNil(writer); end;
end; { TGpXMLSerializer.Serialize }

end.
