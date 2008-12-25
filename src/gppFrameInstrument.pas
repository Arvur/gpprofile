(*:GpProfile 'instrument' frame.
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
   Last modification : 2006-06-17
   Version           : 1.0
</pre>*)(*
   History:
     1.0: 2006-06-17
       - Imported into CVS.
*)

unit gppFrameInstrument;

// TODO 1 -oPrimoz Gabrijelcic : OnCheck handler for vtClasses

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VirtualTrees, StdCtrls, ExtCtrls, JLLabel, Contnrs, 
  gppProjectModel,
  gppParser;

type
  TUnitViewType = (uvtAllUnits, uvtUnit);
  TClassViewType = (cvtAllClasses, cvtClassless, cvtClass);
  TMethodViewType = (mvtAllMethods, mvtMethod);

  TfrmInstrument = class(TFrame)
    lblClasses     : TRotateLabel;
    lblProcs       : TRotateLabel;
    lblUnits       : TRotateLabel;
    pbGradientUnits: TPaintBox;
    pnlClasses     : TPanel;
    pnlProcs       : TPanel;
    pnlUnits       : TPanel;
    splitMethods   : TSplitter;
    vtClasses      : TVirtualStringTree;
    vtMethods      : TVirtualStringTree;
    vtUnits        : TVirtualStringTree;
    procedure vtClassesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtClassesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtClassesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
      TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vtMethodsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMethodsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtMethodsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
      TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
    procedure vtUnitsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtUnitsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure vtUnitsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column:
      TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
  private
    FClassList          : TStringList;
    FHasGlobalProcedures: boolean;
    FModel              : IGppProjectModel;
  protected
    function  GetClassIndex(node: PVirtualNode = nil): integer;
    procedure GetClassList(unitData: TUnit; classList: TStringList;
      var hasGlobalProcedures: boolean);
    function  GetClassViewType(node: PVirtualNode = nil): TClassViewType;
    function  GetMethodProcData(node: PVirtualNode = nil): TProc;
    function  GetMethodViewType(node: PVirtualNode = nil): TMethodViewType;
    function  GetUnitUnitData(node: PVirtualNode = nil): TUnit;
    function  GetUnitViewType(node: PVirtualNode = nil): TUnitViewType;
    procedure HandleModelModified(Sender: TObject; modifiedComponents: TObjectList);
    procedure RecheckClassesState;
    procedure RecheckMethodsState;
    procedure RecheckStateIcons;
    procedure RecheckUnitsState;
    procedure ReloadClassesFromModel(unitData: TUnit);
    procedure ReloadEverything;
    procedure ReloadMethodsFromModule(unitData: TUnit; classViewType: TClassViewType;
      idxClass: integer);
    procedure ReloadUnitsFromModel;
    procedure SetClassData(node: PVirtualNode; classViewType: TClassViewType; classIndex:
      integer = -1);
    procedure SetMethodData(node: PVirtualNode; methodViewType: TMethodViewType;
      methodData: TProc = nil);
    procedure SetModel(const value: IGppProjectModel);
    procedure SetUnitData(node: PVirtualNode; unitViewType: TUnitViewType; unitData: TUnit =
      nil);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    property Model: IGppProjectModel read FModel write SetModel;
  end; { TfrmInstrument }

implementation

uses
  GpString,
  GpVirtualTree;

{$R *.dfm}

{ TfrmInstrument }

constructor TfrmInstrument.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FClassList := TStringList.Create;
end; { TfrmInstrument.Create }

destructor TfrmInstrument.Destroy;
begin
  if assigned(FModel) then
    FModel.Unsubscribe(HandleModelModified);
  FreeAndNil(FClassList);
  inherited;
end; { TfrmInstrument.Destroy }

function TfrmInstrument.GetClassIndex(node: PVirtualNode): integer;
begin
  Result := VTGetNodeDataInt(vtClasses, node, 1);
end; { TfrmInstrument.GetClassIndex }

procedure TfrmInstrument.GetClassList(unitData: TUnit; classList: TStringList; var
  hasGlobalProcedures: boolean);
var
  className: string;
  pData    : pointer;
  pl       : TProcList;
  procData : TProc;
begin
  hasGlobalProcedures := false;
  pl := TProcList.Create(false);
  try
    FModel.Project.GetProcList(unitData, pl);
    classList.Clear;
    classList.Sorted := true;
    for pData in pl do begin
      procData := TProc(pData);
      if not procData.IsMethod then
        hasGlobalProcedures := true
      else begin
        className := procData.ClassName;
        if classList.IndexOf(className) < 0 then
          classList.Add(className);
      end;
    end;
  finally FreeAndNil(pl); end;
end; { TfrmInstrument.GetClassList }

function TfrmInstrument.GetClassViewType(node: PVirtualNode): TClassViewType;
begin
  Result := TClassViewType(VTGetNodeData(vtClasses, node));
end; { TfrmInstrument.GetClassViewType }

function TfrmInstrument.GetMethodProcData(node: PVirtualNode = nil): TProc;
begin
  Result := TProc(VTGetNodeData(vtMethods, node, 1));
end; { TfrmInstrument.GetMethodProcData }

function TfrmInstrument.GetMethodViewType(node: PVirtualNode): TMethodViewType;
begin
  Result := TMethodViewType(VTGetNodeData(vtMethods, node));
end; { TfrmInstrument.GetMethodViewType }

function TfrmInstrument.GetUnitUnitData(node: PVirtualNode): TUnit;
begin
  Result := TUnit(VTGetNodeData(vtUnits, node, 1));
end; { TfrmInstrument.GetUnitUnitData }

function TfrmInstrument.GetUnitViewType(node: PVirtualNode): TUnitViewType;
begin
  Result := TUnitViewType(VTGetNodeDataInt(vtUnits, node));
end; { TfrmInstrument.GetUnitViewType }

procedure TfrmInstrument.HandleModelModified(Sender: TObject;
  modifiedComponents: TObjectList);
var
  iModified: integer;
begin
  for iModified := 0 to modifiedComponents.Count - 1 do
    if Supports(modifiedComponents[iModified], IGppProjectInstrumentationAspect) then
      RecheckStateIcons
    else if Supports(modifiedComponents[iModified], IGppProjectModel) then
      ReloadEverything;
end; { TfrmInstrument.HandleModelModified }

procedure TfrmInstrument.RecheckClassesState;
var
  allInstrumented : boolean;
  classList       : TStringList;
  classViewType   : TClassViewType;
  idxClass        : integer;
  idxProc         : integer;
  iProcData       : integer;
  node            : PVirtualNode;
  nodeAll         : PVirtualNode;
  noneInstrumented: boolean;
  pl              : TProcList;
  procData        : TProc;
begin
  classList := TStringList.Create;
  try
    classList.Assign(FClassList);
    classList.Insert(0, ''); // 'global methods' placeholder
    pl := TProcList.Create(false);
    try
      FModel.Project.GetProcList(GetUnitUnitData, pl);
      for iProcData := 0 to pl.Count - 1 do begin
        procData := TProc(pl[iProcData]);
        if not procData.IsMethod then
          idxProc := 0
        else
          idxProc := classList.IndexOf(procData.ClassName);
        if procData.Instrumented then
          classList.Objects[idxProc] := TObject(cardinal(classList.Objects[idxProc]) OR $01)
        else
          classList.Objects[idxProc] := TObject(cardinal(classList.Objects[idxProc]) OR $02);
      end;
    finally FreeAndNil(pl); end;
    nodeAll := nil;
    allInstrumented := true;
    noneInstrumented := true;
    vtClasses.BeginUpdate;
    try
      node := vtClasses.GetFirst;
      while assigned(node) do begin
        classViewType := GetClassViewType(node);
        if classViewType = cvtAllClasses then
          nodeAll := node
        else begin
          if classViewType = cvtClassless then
            idxClass := 0
          else
            idxClass := GetClassIndex(node)+1;
          case cardinal(classList.Objects[idxClass]) AND $03 of
            1: begin
              vtClasses.CheckState[node] := csCheckedNormal;
              noneInstrumented := false;
            end;
            0, 2: begin
              vtClasses.CheckState[node] := csUncheckedNormal;
              allInstrumented := false;
            end;
            3: begin
              vtClasses.CheckState[node] := csMixedNormal;
              allInstrumented := false;
              noneInstrumented := false;
            end;
          end; //case
        end;
        node := vtClasses.GetNext(node);
      end; //while
      if assigned(nodeAll) then
        if allInstrumented then
          vtClasses.CheckState[nodeAll] := csCheckedNormal
        else if noneInstrumented then
          vtClasses.CheckState[nodeAll] := csUncheckedNormal
        else
          vtClasses.CheckState[nodeAll] := csMixedNormal;
    finally vtClasses.EndUpdate; end;
  finally FreeAndNil(classList); end;
end; { TfrmInstrument.RecheckClassesState }

procedure TfrmInstrument.RecheckMethodsState;
var
  allInstrumented : boolean;
  node            : PVirtualNode;
  nodeAll         : PVirtualNode;
  noneInstrumented: boolean;
  procData        : TProc;
begin
  vtMethods.BeginUpdate;
  try
    nodeAll := nil;
    allInstrumented := true;
    noneInstrumented := true;
    node := vtMethods.GetFirst;
    while assigned(node) do begin
      if GetMethodViewType(node) = mvtAllMethods then
        nodeAll := node
      else begin
        procData := GetMethodProcData(node);
        if procData.Instrumented then begin
          vtMethods.CheckState[node] := csCheckedNormal;
          noneInstrumented := false;
        end
        else begin
          vtMethods.CheckState[node] := csUncheckedNormal;
          allInstrumented := false;
        end;
      end;
      node := vtMethods.GetNext(node);
    end;
    if assigned(nodeAll) then
      if allInstrumented then
        vtMethods.CheckState[nodeAll] := csCheckedNormal
      else if noneInstrumented then
        vtMethods.CheckState[nodeAll] := csUncheckedNormal
      else
        vtMethods.CheckState[nodeAll] := csMixedNormal;
  finally vtMethods.EndUpdate; end;
end; { TfrmInstrument.RecheckMethodsState }

procedure TfrmInstrument.RecheckStateIcons;
begin
  RecheckUnitsState;
  RecheckClassesState;
  RecheckMethodsState;
end; { TfrmInstrument.RecheckStateIcons }

procedure TfrmInstrument.RecheckUnitsState;
var
  allInstrumented : boolean;
  node            : PVirtualNode;
  nodeAll         : PVirtualNode;
  noneInstrumented: boolean;
  unitData        : TUnit;
begin
  vtUnits.BeginUpdate;
  try
    nodeAll := nil;
    allInstrumented := true;
    noneInstrumented := true;
    node := vtUnits.GetFirst;
    while assigned(node) do begin
      if GetUnitViewType(node) = uvtAllUnits then
        nodeAll := node
      else begin
        unitData := GetUnitUnitData(node);
        if unitData.AllInstrumented then begin
          vtUnits.CheckState[node] := csCheckedNormal;
          noneInstrumented := false;
        end
        else if unitData.NoneInstrumented then begin
          vtUnits.CheckState[node] := csUncheckedNormal;
          allInstrumented := false;
        end
        else begin
          vtUnits.CheckState[node] := csMixedNormal;
          allInstrumented := false;
          noneInstrumented := false;
        end;
      end;
      node := vtUnits.GetNext(node);
    end;
    if assigned(nodeAll) then
      if allInstrumented then
        vtUnits.CheckState[nodeAll] := csCheckedNormal
      else if noneInstrumented then
        vtUnits.CheckState[nodeAll] := csUncheckedNormal
      else
        vtUnits.CheckState[nodeAll] := csMixedNormal;
  finally vtUnits.EndUpdate; end;
end; { TfrmInstrument.RecheckUnitsState }

procedure TfrmInstrument.ReloadClassesFromModel(unitData: TUnit);
var
  iClass : integer;
  node   : PVirtualNode;
  nodeAll: PVirtualNode;
begin
  vtClasses.BeginUpdate;
  try
    vtClasses.Clear;
    FClassList.Clear;
    GetClassList(unitData, FClassList, FHasGlobalProcedures);
    nodeAll := vtClasses.AddChild(nil);
    SetClassData(nodeAll, cvtAllClasses);
    vtClasses.CheckType[nodeAll] := ctTriStateCheckBox;
    if FHasGlobalProcedures then begin
      node := vtClasses.AddChild(nil);
      SetClassData(node, cvtClassless, 0);
      vtClasses.CheckType[node] := ctTriStateCheckBox;
    end;
    for iClass := 0 to FClassList.Count - 1 do begin
      node := vtClasses.AddChild(nil);
      SetClassData(node, cvtClass, iClass);
      vtClasses.CheckType[node] := ctTriStateCheckBox;
    end;
    VTSelectNode(vtClasses, nodeAll);
    RecheckClassesState;
  finally vtClasses.EndUpdate; end;
end; { TfrmInstrument.ReloadClassesFromModel }

procedure TfrmInstrument.ReloadEverything;
begin
  vtUnits.BeginUpdate;
  vtClasses.BeginUpdate;
  vtMethods.BeginUpdate;
  try
    vtUnits.Clear;
    vtClasses.Clear;
    vtMethods.Clear;
    ReloadUnitsFromModel;
  finally
    vtUnits.EndUpdate;
    vtClasses.EndUpdate;
    vtMethods.EndUpdate;
  end;
end; { TfrmInstrument.ReloadEverything }

procedure TfrmInstrument.ReloadMethodsFromModule(unitData: TUnit; classViewType:
  TClassViewType; idxClass: integer);
var
  node    : PVirtualNode;
  nodeAll : PVirtualNode;
  pl      : TProcList;
  pProc   : pointer;
  procData: TProc;
begin
  pl := TProcList.Create(false);
  try
    FModel.Project.GetProcList(unitData, pl);
    vtMethods.BeginUpdate;
    try
      vtMethods.Clear;
      nodeAll := vtMethods.AddChild(nil);
      SetMethodData(nodeAll, mvtAllMethods);
      vtMethods.CheckType[nodeAll] := ctTriStateCheckBox;
      for pProc in pl do begin
        procData := TProc(pProc);
        if (classViewType = cvtAllClasses) or
           ((classViewType = cvtClassless) and (not procData.IsMethod) or
           ((idxClass >= 0) and SameText(FClassList[idxClass], procData.ClassName)))
        then begin
          node := vtMethods.AddChild(nil);
          SetMethodData(node, mvtMethod, procData);
          vtMethods.CheckType[node] := ctCheckBox;
        end;
      end;
      VTSelectNode(vtMethods, nodeAll);
      RecheckMethodsState;
      vtMethods.SortTree(0, sdAscending);
    finally vtMethods.EndUpdate; end;
  finally FreeAndNil(pl); end;
end; { TfrmInstrument.ReloadMethodsFromModule }

procedure TfrmInstrument.ReloadUnitsFromModel;
var
  i       : integer;
  node    : PVirtualNode;
  nodeAll : PVirtualNode;
  ul      : TObjectList;
  unitData: TUnit;
begin
  ul := TObjectList.Create(false);
  try
    vtUnits.BeginUpdate;
    try
      vtUnits.Clear;
      if assigned(FModel.Project) then begin
        FModel.Project.GetUnitList(ul, true{projectDirOnly}); // TODO 1 -oPrimoz Gabrijelcic : reimplement 'show all folders'
        nodeAll := vtUnits.AddChild(nil);
        SetUnitData(nodeAll, uvtAllUnits);
        vtUnits.CheckType[nodeAll] := ctTriStateCheckBox;
        for i := 0 to ul.Count-1 do begin
          unitData := TUnit(ul[i]);
          node := vtUnits.AddChild(nil);
          SetUnitData(node, uvtUnit, unitData);
          vtUnits.CheckType[node] := ctTriStateCheckBox;
          VTSetCheck(vtUnits, node, unitData.AllInstrumented, unitData.NoneInstrumented);
        end;
        VTSelectNode(vtUnits, nodeAll);
        RecheckUnitsState;
      end;
      vtUnits.SortTree(0, sdAscending);
    finally vtUnits.EndUpdate; end;
  finally FreeAndNil(ul); end;
end; { TfrmInstrument.ReloadUnitsFromModel }

procedure TfrmInstrument.SetClassData(node: PVirtualNode; classViewType: TClassViewType;
  classIndex: integer);
begin
  VTSetNodeDataInt(vtClasses, Ord(classViewType), node);
  VTSetNodeDataInt(vtClasses, classIndex, node, 1);
end; { TfrmInstrument.SetClassData }

procedure TfrmInstrument.SetMethodData(node: PVirtualNode; methodViewType:
  TMethodViewType; methodData: TProc);
begin
  VTSetNodeDataInt(vtUnits, Ord(methodViewType), node);
  VTSetNodeData(vtUnits, methodData, node, 1);
end; { TfrmInstrument.SetMethodData }

procedure TfrmInstrument.SetModel(const value: IGppProjectModel);
begin
  if assigned(FModel) then
    FModel.Unsubscribe(HandleModelModified);
  FModel := value;
  if assigned(FModel) then
    FModel.Subscribe(HandleModelModified);
end; { TfrmInstrument.SetModel }

procedure TfrmInstrument.SetUnitData(node: PVirtualNode; unitViewType: TUnitViewType;
  unitData: TUnit);
begin
  VTSetNodeDataInt(vtUnits, Ord(unitViewType), node);
  VTSetNodeData(vtUnits, unitData, node, 1);
end; { TfrmInstrument.SetUnitData }

procedure TfrmInstrument.vtClassesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if vtClasses.UpdateCount = 0 then begin
    if GetUnitViewType <> uvtAllUnits then
      ReloadMethodsFromModule(GetUnitUnitData, GetClassViewType(Node), GetClassIndex(Node));
  end;
end; { TfrmInstrument.vtClassesChange }

procedure TfrmInstrument.vtClassesChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  classViewType: TClassViewType;
  doInstrument : boolean;
begin
  if vtClasses.UpdateCount > 0 then
    Exit;
  if Node <> vtClasses.FocusedNode then
    VTSelectNode(vtClasses, Node);
  vtClasses.BeginUpdate;
  try
    doInstrument := (Sender.CheckState[node] = csCheckedNormal);
    classViewType := GetClassViewType(Node);
    if classViewType = cvtAllClasses then begin
      FModel.BeginUpdate;
      try
        repeat
          node := Sender.GetNext(node);
          if not assigned(node) then
            break; //repeat
          if GetClassViewType(Node) = cvtClassless then
            FModel.ChangeClassInstrumentation(GetUnitUnitData, '', doInstrument)
          else
            FModel.ChangeClassInstrumentation(GetUnitUnitData, FClassList[GetClassIndex(Node)],
              doInstrument);
        until false;
      finally FModel.EndUpdate; end;
    end
    else if classViewType = cvtClassless then
      FModel.ChangeClassInstrumentation(GetUnitUnitData, '', doInstrument)
    else
      FModel.ChangeClassInstrumentation(GetUnitUnitData, FClassList[GetClassIndex(Node)],
        doInstrument);
  finally vtClasses.EndUpdate; end;
end; { TfrmInstrument.vtClassesChecked }

procedure TfrmInstrument.vtClassesGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  case GetClassViewType(Node) of
    cvtAllClasses: CellText := '<all classes>';
    cvtClassless:  CellText := '<global procedures>';
    cvtClass:      CellText := FClassList[GetClassIndex(Node)];
  end;
end; { TfrmInstrument.vtClassesGetText }

procedure TfrmInstrument.vtMethodsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  // TODO 1 -oPrimoz Gabrijelcic : Show source line in the bottom panel
  if GetMethodViewType(node) = mvtMethod then 
    FModel.SyncSource(GetUnitUnitData, GetMethodProcData(node));
end; { TfrmInstrument.vtMethodsChange }

procedure TfrmInstrument.vtMethodsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  doInstrument: boolean;
begin
  if vtMethods.UpdateCount > 0 then
    Exit;
  vtMethods.BeginUpdate;
  try
    doInstrument := (Sender.CheckState[node] = csCheckedNormal);
    if GetMethodViewType(node) = mvtAllMethods then begin
      FModel.BeginUpdate;
      try
        repeat
          node := Sender.GetNext(node);
          if not assigned(node) then
            break; //repeat
          FModel.ChangeMethodInstrumentation(GetUnitUnitData, GetMethodProcData(node),
            doInstrument);
        until false;
      finally FModel.EndUpdate; end;
    end
    else
      FModel.ChangeMethodInstrumentation(GetUnitUnitData, GetMethodProcData(node),
        doInstrument);
  finally vtMethods.EndUpdate; end;
end; { TfrmInstrument.vtMethodsChecked }

procedure TfrmInstrument.vtMethodsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
var
  classViewType: TClassViewType;
  procData     : TProc;
begin
  if GetMethodViewType(node) = mvtAllMethods then begin
    classViewType := GetClassViewType;
    if classViewType = cvtAllClasses then begin
      if FHasGlobalProcedures then
        CellText := '<all procedures>'
      else
        CellText := '<all methods>';
    end
    else if classViewType = cvtClassless then
      CellText := '<all global procedures>'
    else
      CellText := Format('<all %s methods>', [VTGetText(vtClasses)]);
  end
  else begin
    procData := GetMethodProcData(node);
    if (GetClassViewType = cvtAllClasses) or (Pos('.', procData.Name) = 0) then
      CellText := procData.Name
    else
      CellText := ButFirstEl(procData.Name, '.', -1);
  end;
end; { TfrmInstrument.vtMethodsGetText }

procedure TfrmInstrument.vtUnitsChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  if vtUnits.UpdateCount = 0 then begin
    if GetUnitViewType(Node) = uvtAllUnits then begin
      vtClasses.Clear;
      vtMethods.Clear;
    end
    else
      ReloadClassesFromModel(GetUnitUnitData(Node));
  end;
end; { TfrmInstrument.vtUnitsChange }

procedure TfrmInstrument.vtUnitsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  doInstrument: boolean;
begin
  if vtUnits.UpdateCount > 0 then
    Exit;
  if Node <> vtUnits.FocusedNode then
    VTSelectNode(vtUnits, Node);
  vtUnits.BeginUpdate;
  try
    doInstrument := (Sender.CheckState[node] = csCheckedNormal);
    if GetUnitViewType(Node) = uvtAllUnits then 
      FModel.ChangeInstrumentation(doInstrument)
    else
      FModel.ChangeUnitInstrumentation(GetUnitUnitData, doInstrument);
  finally vtUnits.EndUpdate; end;
end; { TfrmInstrument.vtUnitsChecked }

procedure TfrmInstrument.vtUnitsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: WideString);
begin
  case GetUnitViewType(Node) of
    uvtAllUnits: CellText := '<all units>';
    uvtUnit:     CellText := GetUnitUnitData(Node).Name;
  end;
end; { TfrmInstrument.vtUnitsGetText }

end.
