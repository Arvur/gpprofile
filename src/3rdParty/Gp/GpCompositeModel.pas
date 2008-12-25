(*:Base class for a composite model, implementing semi-smart hierarchical notification
   mechanism.
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
   Creation date     : 2006-05-15
   Last modification : 2006-06-10
   Version           : 1.0
</pre>*)(*
   History:
     1.0: 2006-05-15
       - Created.
*)

unit GpCompositeModel;

interface

uses
  Classes,
  Contnrs,
  GpLists;

type
  TGpComponentModified = procedure(Sender: TObject; modifiedComponents: TObjectList) of
    object;

  IGpModelComponent = interface;

  IGpCompositeModel = interface['{9558C96F-7AE4-4926-8148-2BDE5CE309BC}']
    function RefCount: integer;
  end; { IGpCompositeModel }

  IGpModelComponent = interface['{0ADC255E-E34F-4135-B54C-00C69EA69159}']
  //accessors
    function  GetModel: IGpCompositeModel;
    function  GetParent: IGpModelComponent;
  //for use inside the component
    procedure MarkChanged;
  //for use from inside and outside of the component
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Subscribe(subscriber: TGpComponentModified);
    procedure Unsubscribe(subscriber: TGpComponentModified);
    property Model: IGpCompositeModel read GetModel;
    property Parent: IGpModelComponent read GetParent;
  end; { IGpModelComponent }

  TGpModelComponent = class(TInterfacedPersistent, IGpModelComponent)
  private
    FModel : IGpCompositeModel;
    FParent: IGpModelComponent;
  protected
    function  GetModel: IGpCompositeModel;
    function  GetParent: IGpModelComponent;
    procedure MarkChanged;
  public
    constructor Create(const parent: IGpModelComponent); overload; virtual;
    constructor Create(const parent: IGpModelComponent; refObject: TObject); overload; virtual;
    destructor  Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Subscribe(subscriber: TGpComponentModified);
    procedure Unsubscribe(subscriber: TGpComponentModified);
    property Model: IGpCompositeModel read GetModel;
    property Parent: IGpModelComponent read GetParent;
  end; { TGpModelComponent }

function  AttachToModel(component: TObject; refObject: TObject = nil): IGpCompositeModel;
procedure DetachFromModel(component: TObject);

implementation

uses
  Windows,
  SysUtils;

type
  IGpManagedComponent = interface['{79AC0916-6D8E-48B4-8257-06529ADB710E}']
  //accessors
    function  GetDirtyList: TInterfaceList;
    function  GetProxiedComponent: TObject;
    function  GetProxiedComponentIntf: IGpModelComponent;
    function  GetSubscribers: TGpTMethodList;
    function  GetUpdateCount: integer;
    procedure SetUpdateCount(value: integer);
  //managing interface
    function  ModifiedSinceLastUpdate(subscriber: TGpComponentModified): boolean;
    procedure SubcomponentSentNotifications(dirtyList: TInterfaceList);
    procedure UpdateLastUpdate(subscriber: TGpComponentModified);
    property DirtyList: TInterfaceList read GetDirtyList;
    property ProxiedComponent: TObject read GetProxiedComponent;
    property ProxiedComponentIntf: IGpModelComponent read GetProxiedComponentIntf;
    property Subscribers: TGpTMethodList read GetSubscribers;
    property UpdateCount: integer read GetUpdateCount write SetUpdateCount;
  end; { IGpManagedComponent }

  IGpCompositeModelManager = interface['{F44D5993-B21E-44B9-B08F-E7F814E88C6D}']
  //IGpModelComponent proxy forwarders
    procedure BeginUpdate(component: IGpModelComponent);
    procedure EndUpdate(component: IGpModelComponent);
    procedure MarkChanged(component: IGpModelComponent);
    procedure Subscribe(component: IGpModelComponent; subscriber: TGpComponentModified);
    procedure Unsubscribe(component: IGpModelComponent; subscriber: TGpComponentModified);
  //managing interface
    procedure RegisterComponent(component, refObject: TObject);
    procedure ResendPendingNotifications(component: IGpManagedComponent);
    procedure UnregisterComponent(component: TObject);
  end; { IGpCompositeModelManager }

  TGpSubscriberWrapper = class
  private
    FLastUpdateCount: integer;
    FSubscriber     : TGpComponentModified;
  public
    constructor Create(subscriber: TGpComponentModified; lastUpdateCount: integer);
    property LastUpdateCount: integer read FLastUpdateCount write FLastUpdateCount;
    property Subscriber: TGpComponentModified read FSubscriber write FSubscriber;
  end; { TGpSubscriberWrapper }

  TGpSubscriberList = class
  private
    FSubscriberList: TObjectList;
  strict protected
    function  GetItems(idxItem: integer): TGpSubscriberWrapper;
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(subscriber: TGpComponentModified): integer;
    function  Count: integer;
    function  IndexOf(subscriber: TGpComponentModified): integer;
    procedure Remove(subscriber: TGpComponentModified);
    property Items[idxItem: integer]: TGpSubscriberWrapper read GetItems; default;
  end; { TGpSubscriberList }

  TGpComponentProxy = class(TInterfacedObject, IGpModelComponent, IGpManagedComponent)
  private
    FDirtyList           : TInterfaceList;
    FModel               : IGpCompositeModel;
    FModelManager        : IGpCompositeModelManager;
    FParent              : IGpModelComponent;
    FProxiedComponent    : TObject;
    FProxiedComponentIntf: IGpModelComponent;
    FSubscribers         : TGpTMethodList;
    FSubscriberUpdates   : TGpSubscriberList;
    FUpdateCount         : integer;
    FUpdateLockCount     : integer;
  protected
    procedure AppendToDirtyList(modifiedComponents: TInterfaceList);
    function  GetDirtyList: TInterfaceList;
    function  GetModel: IGpCompositeModel;
    function  GetParent: IGpModelComponent;
    function  GetProxiedComponent: TObject;
    function  GetProxiedComponentIntf: IGpModelComponent;
    function  GetSubscribers: TGpTMethodList;
    function  GetUpdateCount: integer;
    procedure SetUpdateCount(value: integer);
    property ModelManager: IGpCompositeModelManager read FModelManager;
  public
    constructor Create(const model: IGpCompositeModel; parent, proxiedInterface:
      IGpModelComponent; proxiedComponent: TObject);
    destructor  Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure MarkChanged;
    function  ModifiedSinceLastUpdate(subscriber: TGpComponentModified): boolean;
    procedure SubcomponentSentNotifications(modifiedComponents: TInterfaceList);
    procedure Subscribe(subscriber: TGpComponentModified);
    procedure Unsubscribe(subscriber: TGpComponentModified);
    procedure UpdateLastUpdate(subscriber: TGpComponentModified);
  end; { TGpComponentProxy }
                                            
  TGpComponentProxyList = class
  private
    FModel    : IGpCompositeModel;
    FProxyList: TInterfaceList;
  protected
    function CreateProxy(component, refObject: TObject): IGpModelComponent;
    function GetProxy(idxProxy: integer): IGpModelComponent;
    function LocateParent(component: TObject): IGpModelComponent;
  public
    constructor Create(const model: IGpCompositeModel);
    destructor  Destroy; override;
    procedure Add(component, refObject: TObject);
    function  Locate(component: TObject): IGpModelComponent;
    function  LocateIntf(component: IGpModelComponent): IGpModelComponent;
    procedure Remove(component: TObject);
    property Proxy[idxProxy: integer]: IGpModelComponent read GetProxy;
  end; { TGpComponentProxyList }

  TGpCompositeModel = class(TInterfacedObject, IGpCompositeModel, IGpCompositeModelManager)
  private
    FComponentProxyList: TGpComponentProxyList;
  protected
    procedure GetCleanedDirtyList(componentProxy: IGpManagedComponent; subscriber:
      TGpComponentModified; cleanedDirtyList: TInterfaceList);
    function GetProxy(component: IGpModelComponent): IGpModelComponent;
    procedure MapProxiesToObjects(dirtyList: TInterfaceList; modifiedComponents: TObjectList);
    procedure UpdateCounters(subscriber: TGpComponentModified; dirtyList: TInterfaceList);
    property Proxy[component: IGpModelComponent]: IGpModelComponent read GetProxy;
  public
    constructor Create;
    destructor Destroy; override;
  //IGpCompositeModelManager
    procedure RegisterComponent(component, refObject: TObject);
    procedure ResendPendingNotifications(componentProxy: IGpManagedComponent);
    procedure UnregisterComponent(component: TObject);
  //IGpModelComponent proxy forwarders
    procedure BeginUpdate(component: IGpModelComponent);
    procedure EndUpdate(component: IGpModelComponent);
    procedure MarkChanged(component: IGpModelComponent);
    function  RefCount: integer;
    procedure Subscribe(component: IGpModelComponent; subscriber: TGpComponentModified);
    procedure Unsubscribe(component: IGpModelComponent; subscriber: TGpComponentModified);
  end; { TGpCompositeModel }

{ globals }

function AttachToModel(component, refObject: TObject): IGpCompositeModel;
var
  compIntf: IGpModelComponent;
begin
  if not Supports(component, IGpModelComponent, compIntf) then
    Result := nil
  else begin
    if not assigned(compIntf.Parent) then
      Result := TGpCompositeModel.Create
    else
      Result := compIntf.Parent.Model;
    if refObject = nil then
      refObject := component;
    (Result as IGpCompositeModelManager).RegisterComponent(component, refObject);
  end;
end; { TGpModelComponent.AttachToModel }

procedure DetachFromModel(component: TObject);
var
  compIntf: IGpModelComponent;
begin
  if Supports(component, IGpModelComponent, compIntf) then
    (compIntf.Model as IGpCompositeModelManager).UnregisterComponent(component);
end; { DetachFromModel }

{ TGpSubscriberWrapper }

constructor TGpSubscriberWrapper.Create(subscriber: TGpComponentModified;
  lastUpdateCount: integer);
begin
  inherited Create;
  FSubscriber := subscriber;
  FLastUpdateCount := lastUpdateCount;
end; { TGpSubscriberWrapper.Create }

{ TGpSubscriberList }

constructor TGpSubscriberList.Create;
begin
  inherited;
  FSubscriberList := TObjectList.Create;
end; { TGpSubscriberList.Create }

destructor TGpSubscriberList.Destroy;
begin
  FreeAndNil(FSubscriberList);
  inherited;
end; { TGpSubscriberList.Destroy }

function TGpSubscriberList.Add(subscriber: TGpComponentModified): integer;
begin
  Result := FSubscriberList.Add(TGpSubscriberWrapper.Create(subscriber, 0));
end; { TGpSubscriberList.Add }

function TGpSubscriberList.Count: integer;
begin
  Result := FSubscriberList.Count;
end; { TGpSubscriberList.Count }

function TGpSubscriberList.GetItems(idxItem: integer): TGpSubscriberWrapper;
begin
  Result := TGpSubscriberWrapper(FSubscriberList[idxItem]);
end; { TGpSubscriberList.GetItems }

function TGpSubscriberList.IndexOf(subscriber: TGpComponentModified): integer;
var
  iSubscriber: TMethod;
begin
  for Result := 0 to FSubscriberList.Count - 1 do begin
    iSubscriber := TMethod(Items[Result].Subscriber);
    if (iSubscriber.Code = TMethod(subscriber).Code) and
       (iSubscriber.Data = TMethod(subscriber).Data)
    then
      Exit;
  end;
  Result := -1;
end; { TGpSubscriberList.IndexOf }

procedure TGpSubscriberList.Remove(subscriber: TGpComponentModified);
var
  idxSubscriber: integer;
begin
  idxSubscriber := IndexOf(subscriber);
  if idxSubscriber >= 0 then
    FSubscriberList.Delete(idxSubscriber);
end; { TGpSubscriberList.Remove }

{ TGpModelComponent }

constructor TGpModelComponent.Create(const parent: IGpModelComponent; refObject: TObject);
begin
  FParent := parent;
  FModel := AttachToModel(Self, refObject);
end; { TGpModelComponent.Create }

constructor TGpModelComponent.Create(const parent: IGpModelComponent);
begin
  Create(parent, nil);
end; { TGpModelComponent.Create }

destructor TGpModelComponent.Destroy;
begin
  DetachFromModel(Self);
//  FModel := nil;
  inherited;
end; { TGpModelComponent.Destroy }

procedure TGpModelComponent.BeginUpdate;
begin
  (Model as IGpCompositeModelManager).BeginUpdate(Self);
end; { TGpModelComponent.BeginUpdate }

procedure TGpModelComponent.EndUpdate;
begin
  (Model as IGpCompositeModelManager).EndUpdate(Self);
end; { TGpModelComponent.EndUpdate }

function TGpModelComponent.GetModel: IGpCompositeModel;
begin
  Result := FModel;
end; { TGpModelComponent.GetModel }

function TGpModelComponent.GetParent: IGpModelComponent;
begin
  Result := FParent;
end; { TGpModelComponent.GetParent }

procedure TGpModelComponent.MarkChanged;
begin
  (Model as IGpCompositeModelManager).MarkChanged(Self);
end; { TGpModelComponent.MarkChanged }

procedure TGpModelComponent.Subscribe(subscriber: TGpComponentModified);
begin
  (Model as IGpCompositeModelManager).Subscribe(Self, subscriber);
end; { TGpModelComponent.Subscribe }

procedure TGpModelComponent.Unsubscribe(subscriber: TGpComponentModified);
begin
  (Model as IGpCompositeModelManager).Unsubscribe(Self, subscriber);
end; { TGpModelComponent.Unsubscribe }

{ TGpComponentProxy }

constructor TGpComponentProxy.Create(const model: IGpCompositeModel; parent,
  proxiedInterface: IGpModelComponent; proxiedComponent: TObject);
begin
  inherited Create;
  FSubscribers := TGpTMethodList.Create;
  FModel := model;
  FModelManager := model as IGpCompositeModelManager;
  FParent := parent;
  FProxiedComponent := proxiedComponent;
  FProxiedComponentIntf := proxiedInterface;
  FDirtyList := TInterfaceList.Create;
  FSubscriberUpdates := TGpSubscriberList.Create;
end; { TGpComponentProxy.Create }

destructor TGpComponentProxy.Destroy;
begin
  FreeAndNil(FSubscriberUpdates);
  FreeAndNil(FDirtyList);
  FreeAndNil(FSubscribers);
  inherited Destroy;
end; { TGpComponentProxy.Destroy }

procedure TGpComponentProxy.AppendToDirtyList(modifiedComponents: TInterfaceList);
var
  iComponent: integer;
begin
  FDirtyList.Capacity := FDirtyList.Count + modifiedComponents.Count;
  for iComponent := 0 to modifiedComponents.Count - 1 do begin
    if FDirtyList.IndexOf(modifiedComponents[iComponent]) < 0 then begin
      FDirtyList.Count := FDirtyList.Count + 1;
      FDirtyList[FDirtyList.Count-1] := modifiedComponents[iComponent];
    end;
  end;
end; { TGpCompositeObjectImpl.AppendToDirtyList }

procedure TGpComponentProxy.BeginUpdate;
begin
  Inc(FUpdateLockCount);
end; { TGpComponentProxy.BeginUpdate }

procedure TGpComponentProxy.EndUpdate;
begin
  Dec(FUpdateLockCount);
  if FUpdateLockCount < 0 then
    raise Exception.CreateFmt('%s: Update count < 0', [ClassName])
  else if FUpdateLockCount = 0 then
    ModelManager.ResendPendingNotifications(Self);
end; { TGpComponentProxy.EndUpdate }

function TGpComponentProxy.GetDirtyList: TInterfaceList;
begin
  Result := FDirtyList;
end; { TGpComponentProxy.GetDirtyList }

function TGpComponentProxy.GetModel: IGpCompositeModel;
begin
  Result := FModel;
end; { TGpComponentProxy.GetModel }

function TGpComponentProxy.GetParent: IGpModelComponent;
begin
  Result := FParent;
end; { TGpComponentProxy.GetParent }

function TGpComponentProxy.GetProxiedComponent: TObject;
begin
  Result := FProxiedComponent;
end; { TGpComponentProxy.GetProxiedComponent }

function TGpComponentProxy.GetProxiedComponentIntf: IGpModelComponent;
begin
  Result := FProxiedComponentIntf;
end; { TGpComponentProxy.GetProxiedComponentIntf }

function TGpComponentProxy.GetSubscribers: TGpTMethodList;
begin
  Result := FSubscribers;
end; { TGpComponentProxy.GetSubscribers }

function TGpComponentProxy.GetUpdateCount: integer;
begin
  Result := FUpdateCount;
end; { TGpComponentProxy.GetUpdateCount }

procedure TGpComponentProxy.MarkChanged;
var
  idxObject: integer;
begin
  idxObject := FDirtyList.IndexOf(Self);
  if idxObject < 0 then
    FDirtyList.Add(Self);
  if FUpdateLockCount = 0 then
    ModelManager.ResendPendingNotifications(Self);
end; { TGpComponentProxy.MarkChanged }

function TGpComponentProxy.ModifiedSinceLastUpdate(subscriber: TGpComponentModified):
  boolean;
var
  idxSubscriberWrapper: integer;
begin                              
  idxSubscriberWrapper := FSubscriberUpdates.IndexOf(subscriber);
  if idxSubscriberWrapper < 0 then
    Result := true
  else
    Result := (FSubscriberUpdates[idxSubscriberWrapper].LastUpdateCount < FUpdateCount);
end; { TGpComponentProxy.ModifiedSinceLastUpdate }

procedure TGpComponentProxy.SetUpdateCount(value: integer);
begin
  FUpdateCount := value;
end; { TGpComponentProxy.SetUpdateCount }

procedure TGpComponentProxy.SubcomponentSentNotifications(modifiedComponents:
  TInterfaceList);
begin
  AppendToDirtyList(modifiedComponents);
  if FUpdateLockCount = 0 then
    ModelManager.ResendPendingNotifications(Self);
end; { TGpComponentProxy.SubcomponentSentNotifications }

procedure TGpComponentProxy.Subscribe(subscriber: TGpComponentModified);
begin
  FSubscribers.Ensure(TMethod(subscriber));
end; { TGpComponentProxy.Subscribe }

procedure TGpComponentProxy.Unsubscribe(subscriber: TGpComponentModified);
begin
  FSubscribers.Remove(TMethod(subscriber));
end; { TGpComponentProxy.Unsubscribe }

procedure TGpComponentProxy.UpdateLastUpdate(subscriber: TGpComponentModified);
var
  idxSubscriberWrapper: integer;
begin
  idxSubscriberWrapper := FSubscriberUpdates.IndexOf(subscriber);
  if idxSubscriberWrapper < 0 then
    idxSubscriberWrapper := FSubscriberUpdates.Add(subscriber);
  FSubscriberUpdates[idxSubscriberWrapper].LastUpdateCount := FUpdateCount;
end; { TGpComponentProxy.UpdateLastUpdate }

{ TGpComponentProxyList }

constructor TGpComponentProxyList.Create(const model: IGpCompositeModel);
begin
  inherited Create;
  FProxyList := TInterfaceList.Create;
  //this component is owned by the model itself and this is only a backpointer
  //if we create a true interface reference, model will never be destroyed
  pointer(FModel) := pointer(model);
end; { TGpComponentProxyList.Create }

destructor TGpComponentProxyList.Destroy;
begin
  FreeAndNil(FProxyList);
  pointer(FModel) := nil;
  inherited Destroy;
end; { TGpComponentProxyList.Destroy }

procedure TGpComponentProxyList.Add(component, refObject: TObject);
begin
  if Locate(component) = nil then
    FProxyList.Add(CreateProxy(component, refObject));
end; { TGpComponentProxyList.Add }

function TGpComponentProxyList.CreateProxy(component, refObject: TObject): IGpModelComponent;
var
  compIntf: IGpModelComponent;
begin
  Supports(component, IGpModelComponent, compIntf);
  Result := TGpComponentProxy.Create(FModel, LocateParent(component), compIntf, refObject);
end; { TGpComponentProxyList.CreateProxy }

function TGpComponentProxyList.GetProxy(idxProxy: integer): IGpModelComponent;
begin
  Result := FProxyList[idxProxy] as IGpModelComponent;
end; { TGpComponentProxyList.GetProxy }

function TGpComponentProxyList.Locate(component: TObject): IGpModelComponent;
var
  iProxy: integer;
begin
  if assigned(component) then begin
    for iProxy := 0 to FProxyList.Count - 1 do begin
      Result := Proxy[iProxy];
      if (Result as IGpManagedComponent).ProxiedComponent = component then
        Exit;
    end;
  end;
  Result := nil;
end; { TGpComponentProxyList.Locate }

function TGpComponentProxyList.LocateIntf(component: IGpModelComponent):
  IGpModelComponent;
var
  iProxy: integer;
begin
  if assigned(component) then begin
    for iProxy := 0 to FProxyList.Count - 1 do begin
      Result := Proxy[iProxy];
      if (Result as IGpManagedComponent).ProxiedComponentIntf = component then
        Exit;
    end;
  end;
  Result := nil;
end; { TGpComponentProxyList.Locate }

function TGpComponentProxyList.LocateParent(component: TObject): IGpModelComponent;
var
  compIntf  : IGpModelComponent;
  compParent: IGpModelComponent;
  iProxy    : integer;
  refIntf: IGpModelComponent;
begin
  if assigned(component) then begin
    Supports(component, IGpModelComponent, compIntf);
    compParent := compIntf.Parent;
    if assigned(compParent) then begin
      for iProxy := 0 to FProxyList.Count - 1 do begin
        Result := Proxy[iProxy];
        Supports((Result as IGpManagedComponent).ProxiedComponent, IGpModelComponent,
          refIntf);
        if refIntf = compParent then
          Exit;
      end;
    end;
  end;
  Result := nil;
end; { TGpComponentProxyList.LocateParent }

procedure TGpComponentProxyList.Remove(component: TObject);
var
  proxy: IGpModelComponent;
begin
  proxy := Locate(component);
  if assigned(proxy) then
    FProxyList.Remove(proxy);
end; { TGpComponentProxyList.Remove }

{ TGpCompositeModel }

constructor TGpCompositeModel.Create;
begin
  inherited Create;
  FComponentProxyList := TGpComponentProxyList.Create(Self);
end; { TGpCompositeModel.Create }

destructor TGpCompositeModel.Destroy;
begin
  FreeAndNil(FComponentProxyList);
  inherited Destroy;
end; { TGpCompositeModel.Destroy }

procedure TGpCompositeModel.BeginUpdate(component: IGpModelComponent);
begin
  Proxy[component].BeginUpdate;
end; { TGpCompositeModel.BeginUpdate }

procedure TGpCompositeModel.EndUpdate(component: IGpModelComponent);
begin
  Proxy[component].EndUpdate;
end; { TGpCompositeModel.EndUpdate }

procedure TGpCompositeModel.GetCleanedDirtyList(componentProxy: IGpManagedComponent;
  subscriber: TGpComponentModified; cleanedDirtyList: TInterfaceList);
var
  component : IGpManagedComponent;
  iComponent: integer;
begin
  cleanedDirtyList.Clear;
  cleanedDirtyList.Capacity := componentProxy.DirtyList.Count;
  for iComponent := 0 to componentProxy.DirtyList.Count - 1 do begin
    component := componentProxy.DirtyList[iComponent] as IGpManagedComponent;
    if (component = componentProxy) or component.ModifiedSinceLastUpdate(subscriber) then
      cleanedDirtyList.Add(component as IGpModelComponent);
  end;
end; { TGpCompositeModel.GetCleanedDirtyList }

function TGpCompositeModel.GetProxy(component: IGpModelComponent): IGpModelComponent;
begin
  Result := FComponentProxyList.LocateIntf(component);
end; { TGpCompositeModel.GetProxy }

procedure TGpCompositeModel.MapProxiesToObjects(dirtyList: TInterfaceList;
  modifiedComponents: TObjectList);
var
  iComponent: integer;
begin
  modifiedComponents.Clear;
  for iComponent := 0 to dirtyList.Count - 1 do 
    modifiedComponents.Add((dirtyList[iComponent] as IGpManagedComponent).ProxiedComponent);
end; { TGpCompositeModel.MapProxiesToObjects }

procedure TGpCompositeModel.MarkChanged(component: IGpModelComponent);
begin
  Proxy[component].MarkChanged;
end; { TGpCompositeModel.MarkChanged }

function TGpCompositeModel.RefCount: integer;
begin
  Result := FRefCount;
end; { TGpCompositeModel.RefCount }

procedure TGpCompositeModel.RegisterComponent(component, refObject: TObject);
begin
  FComponentProxyList.Add(component, refObject);
end; { TGpCompositeModel.RegisterComponent }

procedure TGpCompositeModel.ResendPendingNotifications(componentProxy:
  IGpManagedComponent);
var
  cleanedDirtyList  : TInterfaceList;
  component         : IGpModelComponent;
  iSubscriberWrapper: integer;
  modifiedComponents: TObjectList;
  subscriber        : TGpComponentModified;
begin
  cleanedDirtyList := TInterfaceList.Create;
  try
    componentProxy.UpdateCount := componentProxy.UpdateCount + 1;
    for iSubscriberWrapper := 0 to componentProxy.Subscribers.Count - 1 do begin
      subscriber := TGpComponentModified(componentProxy.Subscribers[iSubscriberWrapper]);
      GetCleanedDirtyList(componentProxy, subscriber, cleanedDirtyList);
      if cleanedDirtyList.Count > 0 then begin
        modifiedComponents := TObjectList.Create(false);
        try
          MapProxiesToObjects(cleanedDirtyList, modifiedComponents);
          subscriber(componentProxy.ProxiedComponent, modifiedComponents);
        finally FreeAndNil(modifiedComponents); end;
      end;
      UpdateCounters(subscriber, cleanedDirtyList);
    end;
    component := componentProxy as IGpModelComponent;
    if assigned(component.Parent) then
      (component.Parent as IGpManagedComponent).SubcomponentSentNotifications(
        componentProxy.DirtyList);
    componentProxy.DirtyList.Clear;
  finally FreeAndNil(cleanedDirtyList); end;
end; { TGpCompositeModel.ResendPendingNotifications }

procedure TGpCompositeModel.Subscribe(component: IGpModelComponent;
  subscriber: TGpComponentModified);
begin
  Proxy[component].Subscribe(subscriber);
end; { TGpCompositeModel.Subscribe }

procedure TGpCompositeModel.UnregisterComponent(component: TObject);
begin
  FComponentProxyList.Remove(component);
end; { TGpCompositeModel.UnregisterComponent }

procedure TGpCompositeModel.Unsubscribe(component: IGpModelComponent;
  subscriber: TGpComponentModified);
begin
  Proxy[component].Unsubscribe(subscriber);
end; { TGpCompositeModel.Unsubscribe }

procedure TGpCompositeModel.UpdateCounters(subscriber: TGpComponentModified; dirtyList:
  TInterfaceList);
var
  iComponent: integer;
begin
  for iComponent := 0 to dirtyList.Count - 1 do
    (dirtyList[iComponent] as IGpManagedComponent).UpdateLastUpdate(subscriber);
end; { TGpCompositeModel.UpdateCounters }

end.
