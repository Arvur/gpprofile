(*:GpProfile 'instrumentation' preferences frame.
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
   Last modification : 2007-11-03
   Version           : 1.01
</pre>*)(*
   History:
     1.01: 2007-11-03
       - Added instrumentation option 'Keep file date/time unchanged'.
     1.0: 2006-06-17
       - Imported into CVS.
*)

unit gppFramePreferencesInstrumentation;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Contnrs,
  gppPreferencesModel;

type
  TfrmPreferencesInstrumentation = class(TFrame)
    bvlSettings          : TBevel;
    cbInstrumentAssembler: TCheckBox;
    cbProfilingAutostart : TCheckBox;
    cbShowAllFolders     : TCheckBox;
    cbxMarker            : TComboBox;
    lblMarkerStyle       : TLabel;
    lblSettings          : TLabel;
    pnlInstrumentation   : TPanel;
    cbKeepFileDateUnchanged: TCheckBox;
    procedure cbInstrumentAssemblerClick(Sender: TObject);
    procedure cbKeepFileDateUnchangedClick(Sender: TObject);
    procedure cbProfilingAutostartClick(Sender: TObject);
    procedure cbShowAllFoldersClick(Sender: TObject);
    procedure cbxMarkerChange(Sender: TObject);
  private
    FModel: TGppPrefInstrumentation;
  protected
    procedure HandleModelModified(Sender: TObject; modifiedComponents: TObjectList);
    procedure SetModel(value: TGppPrefInstrumentation);
    procedure SubscribeToModel;
    procedure UnsubscribeFromModel; 
  public
    destructor Destroy; override;
    procedure Reload;
    property Model: TGppPrefInstrumentation read FModel write SetModel;
  end; { TfrmPreferencesInstrumentation }

implementation

{$R *.dfm}

destructor TfrmPreferencesInstrumentation.Destroy;
begin
  UnsubscribeFromModel;
  inherited;
end; { TfrmPreferencesInstrumentation.Destroy }

procedure TfrmPreferencesInstrumentation.cbInstrumentAssemblerClick(Sender: TObject);
begin
  if cbInstrumentAssembler.Checked then
    Model.InstrumentationOptions := Model.InstrumentationOptions + [ioInstrumentAssembler]
  else
    Model.InstrumentationOptions := Model.InstrumentationOptions - [ioInstrumentAssembler];
end; { TfrmPreferencesInstrumentation.cbInstrumentAssemblerClick }

procedure TfrmPreferencesInstrumentation.cbKeepFileDateUnchangedClick(Sender: TObject);
begin
  if cbKeepFileDateUnchanged.Checked then
    Model.InstrumentationOptions := Model.InstrumentationOptions + [ioKeepFileDateUnchanged]
  else
    Model.InstrumentationOptions := Model.InstrumentationOptions - [ioKeepFileDateUnchanged];
end; { TfrmPreferencesInstrumentation.cbKeepFileDateUnchangedClick }

procedure TfrmPreferencesInstrumentation.cbProfilingAutostartClick(Sender: TObject);
begin
  if cbProfilingAutostart.Checked then
    Model.InstrumentationOptions := Model.InstrumentationOptions + [ioAutoStartProfiling]
  else
    Model.InstrumentationOptions := Model.InstrumentationOptions - [ioAutoStartProfiling];
end; { TfrmPreferencesInstrumentation.cbProfilingAutostartClick }

procedure TfrmPreferencesInstrumentation.cbShowAllFoldersClick(Sender: TObject);
begin
  if cbShowAllFolders.Checked then
    Model.BrowsingOptions := Model.BrowsingOptions + [boShowAllFolders]
  else
    Model.BrowsingOptions := Model.BrowsingOptions - [boShowAllFolders];
end; { TfrmPreferencesInstrumentation.cbShowAllFoldersClick }

procedure TfrmPreferencesInstrumentation.cbxMarkerChange(Sender: TObject);
begin
  Model.MarkerStyle := TGppMarkerStyle(cbxMarker.ItemIndex);
end; { TfrmPreferencesInstrumentation.cbxMarkerChange }

procedure TfrmPreferencesInstrumentation.HandleModelModified(Sender: TObject;
  modifiedComponents: TObjectList);
begin
  Reload;
end; { TfrmPreferencesInstrumentation.HandleModelModified }

procedure TfrmPreferencesInstrumentation.Reload;
begin
  cbxMarker.ItemIndex := Ord(Model.MarkerStyle);
  cbShowAllFolders.Checked := (boShowAllFolders in Model.BrowsingOptions);
  cbProfilingAutostart.Checked := (ioAutoStartProfiling in Model.InstrumentationOptions);
  cbInstrumentAssembler.Checked := (ioInstrumentAssembler in Model.InstrumentationOptions);
  cbKeepFileDateUnchanged.Checked := (ioKeepFileDateUnchanged in Model.InstrumentationOptions);
end; { TfrmPreferencesInstrumentation.Reload }

procedure TfrmPreferencesInstrumentation.SetModel(value: TGppPrefInstrumentation);
begin
  if FModel <> value then begin
    UnsubscribeFromModel;
    FModel := value;
    SubscribeToModel;
  end;
end; { TfrmPreferencesInstrumentation.SetModel }

procedure TfrmPreferencesInstrumentation.SubscribeToModel;
begin
  if assigned(Model) then begin
    Model.Subscribe(HandleModelModified);
    Reload;
  end;
end; { TfrmPreferencesInstrumentation.SubscribeToModel }

procedure TfrmPreferencesInstrumentation.UnsubscribeFromModel;
begin
  if assigned(Model) then
    Model.Unsubscribe(HandleModelModified);
end; { TfrmPreferencesInstrumentation.UnsubscribeFromModel }

end.
