(*:GpProfile profile model.
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

unit gppProfileModel;

interface

uses
  GpCompositeModel,
  GpManagedClass;
  
type
  IGppProfileModel = interface(IGpModelComponent)
    ['{3DD32049-484C-468C-BE2B-065A1D9314F6}']
    function  GetName: string;
    procedure SetName(const value: string);
  //
    procedure BeginUpdate;
    procedure CloseProfile;
    procedure EndUpdate;
    function  IsOpen: boolean;
    procedure OpenProfile(const fileName: string);
    function  Load: boolean;
    property Name: string read GetName write SetName;
  end; { IGppProfileModel }

function CreateModel: IGppProfileModel;

implementation

uses
  SysUtils,
  gppProfileData;

type
  TGppProfileModel = class(TGpModelComponent, IGppProfileModel, IGpManagedErrorHandling)
  private
    gpmErrorHandling: IGpManagedErrorHandling;
    gpmName         : string;
    gpmProfileData  : TGppProfileData;
  protected
    function  GetName: string;
    procedure SetName(const value: string);
    property Error: IGpManagedErrorHandling read gpmErrorHandling
      implements IGpManagedErrorHandling;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CloseProfile;
    function  IsOpen: boolean;
    function  Load: boolean;
    procedure OpenProfile(const fileName: string);
    property Name: string read GetName write SetName;
  end; { TGppProfileModel }

{ globals }

function CreateModel: IGppProfileModel;
begin
  Result := TGppProfileModel.Create;
end; { CreateModel }

{ TGppProfileModel }

constructor TGppProfileModel.Create;
begin
  inherited;
  gpmErrorHandling := TGpManagedError.Create;
end; { TGppProfileModel.Create }

destructor TGppProfileModel.Destroy;
begin
  FreeAndNil(gpmProfileData);
  inherited;
end; { TGppProfileModel.Destroy }

procedure TGppProfileModel.CloseProfile;
begin
  FreeAndNil(gpmProfileData);
  MarkChanged;
end; { TGppProfileModel.CloseProfile }

function TGppProfileModel.GetName: string;
begin
  Result := gpmName;
end; { TGppProfileModel.GetName }

function TGppProfileModel.IsOpen: boolean;
begin
  Result := assigned(gpmProfileData);
end; { TGppProfileModel.IsOpen }

function TGppProfileModel.Load: boolean;
begin
  // TODO 1 -oPrimoz Gabrijelcic : implement: TGppProfileModel.Load
  Result := false;
end; { TGppProfileModel.Load }

procedure TGppProfileModel.OpenProfile(const fileName: string);
begin
  BeginUpdate;
  try
    CloseProfile;
    gpmProfileData := TGppProfileData.Create;
    gpmName := fileName;
    MarkChanged;
  finally EndUpdate; end;
end; { TGppProfileModel.OpenProfile }

procedure TGppProfileModel.SetName(const value: string);
begin
  gpmName := value;
end; { TGppProfileModel.SetName }

end.

