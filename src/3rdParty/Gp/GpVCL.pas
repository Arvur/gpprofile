(*:VCL helper library.
   @author Primoz Gabrijelcic
   @desc <pre>
   (c) 2008 Primoz Gabrijelcic
   Free for personal and commercial use. No rights reserved.

   Author            : Primoz Gabrijelcic
   Creation date     : 2003-12-11
   Last modification : 2008-02-06
   Version           : 1.09a
</pre>*)(*
   History:
     1.09a: 2008-02-06
       - Changed TControlEnumeratorFactory to a record. That way, no reference counting on
         the interface is required and code is shorter. Thanks to Fredrik Loftheim for
         the suggestion.
     1.09: 2008-01-23
       - Added TWinControl.Controls enumerator.
     1.08: 2007-10-26
       - Added methods ControlByClass and ControlsByClass.
     1.07: 2004-04-19
       - Added methods DisableChildControls, EnableChildControls, DisableControls,
         EnableControls.
     1.06: 2004-04-08
       - Added parameter SkipFirstN to ControlByTag and ComponentByTag.
     1.05: 2004-03-10
       - Added function CanAllocateHandle.
     1.04: 2004-03-09
       - Added support for .Lines property. 
     1.03: 2004-02-08
       - Added support for .Checked property.
     1.02: 2004-01-17
       - Added two overloaded methods ComponentByTag. 
     1.01: 2003-12-24
       - Added methods ContainsNonemptyControl and SameControlsAndProperties.  
     1.0: 2003-12-11
       - Released.
*)

unit GpVCL;

interface

uses
  Classes,
  Controls,
  Contnrs;

type
  TControlEnumerator = record
  strict private
    ceClass : TClass;
    ceIndex : integer;
    ceParent: TWinControl;
  public
    constructor Create(parent: TWinControl; matchClass: TClass);
    function GetCurrent: TControl;
    function MoveNext: boolean;
    property Current: TControl read GetCurrent;
  end; { TControlEnumerator }

  TControlEnumeratorFactory = record
  strict private
    cefClass : TClass;
    cefParent: TWinControl;
  public
    constructor Create(parent: TWinControl; matchClass: TClass);
    function  GetEnumerator: TControlEnumerator;
  end; { TControlEnumeratorFactory }

function  ContainsNonemptyControl(controlParent: TWinControl;
  const requiredControlNamePrefix: string; const ignoreControls: string = ''): boolean;
procedure CopyControlsToProperties(sourceParent: TWinControl;
  targetObj: TPersistent; const removePrefixFromControls: string);
procedure CopyPropertiesToControls(sourceObj: TPersistent;
  targetParent: TWinControl; const prefixControlsWith: string);
function  SameControlsAndProperties(controlParent: TWinControl;
  compareObj: TPersistent; const removePrefixFromControls: string): boolean;

function  ControlByTag(parent: TWinControl; tag: integer;
  searchInSubcontrols: boolean = true): TControl; overload;
function  ControlByTag(parent: TWinControl; tag: integer; searchInSubcontrols: boolean;
  allowOnly: array of TControlClass): TControl; overload;
function  ControlByTag(parent: TWinControl; tag: integer; searchInSubcontrols: boolean;
  allowOnly: array of TControlClass; skipFirstN: integer): TControl; overload;

function  ControlByClass(parent: TWinControl; controlClass: TClass;
  controlIndex: integer = 1): TControl;

procedure ControlsByClass(parent: TWinControl; controlClass: TClass;
  controls: TObjectList);

function  ComponentByTag(parent: TComponent; tag: integer;
  searchInSubcomponents: boolean = true): TComponent; overload;
function  ComponentByTag(parent: TComponent; tag: integer; searchInSubcomponents: boolean;
  allowOnly: array of TComponentClass): TComponent; overload;
function  ComponentByTag(parent: TComponent; tag: integer; searchInSubcomponents: boolean;
  allowOnly: array of TComponentClass; skipFirstN: integer): TComponent; overload;

function  CanAllocateHandle(control: TWinControl): boolean;

procedure DisableChildControls(parent: TWinControl; disabledList: TList = nil);
procedure EnableChildControls(parent: TWinControl; enabledList: TList = nil);
procedure DisableControls(controlList: TList);
procedure EnableControls(controlList: TList);

function  EnumControls(parent: TWinControl; matchClass: TClass = nil): TControlEnumeratorFactory;

implementation

uses
  TypInfo,
  SysUtils, 
  GpProperty;

function Unwrap(s: string; child: TControl): string;
begin
  (* Probably not a good idea to do this for all applications:
  if (not IsPublishedProp(child, 'WordWrap')) or (GetOrdProp(child, 'WordWrap') = 0) then
    Result := s
  else
    Result := TrimRight(StringReplace(s, #13#10, ' ', [rfReplaceAll]));
  *)
  Result := s;
end; { Unwrap }

{:Checks if any control (with name starting with 'requiredControlNamePrefix' and
  not included in the #13#10-delimited 'ignoreControls' list) contains non-empty
  .Text or .Lines property.
  @since   2003-12-24
}
function ContainsNonemptyControl(controlParent: TWinControl;
  const requiredControlNamePrefix: string;
  const ignoreControls: string = ''): boolean;
var
  child   : TControl;
  iControl: integer;
  ignored : TStringList;
  obj     : TObject;
begin
  Result := true;
  if ignoreControls = '' then
    ignored := nil
  else begin
    ignored := TStringList.Create;
    ignored.Text := ignoreControls;
  end;
  try
    for iControl := 0 to controlParent.ControlCount-1 do begin
      child := controlParent.Controls[iControl];
      if (requiredControlNamePrefix = '') or
         SameText(requiredControlNamePrefix, Copy(child.Name, 1, Length(requiredControlNamePrefix))) then
      if (not assigned(ignored)) or (ignored.IndexOf(child.Name) < 0) then
      if IsPublishedProp(child, 'Text') and (GetStrProp(child, 'Text') <> '') then
        Exit
      else if IsPublishedProp(child, 'Lines') then begin
        obj := TObject(cardinal(GetOrdProp(child, 'Lines')));
        if (obj is TStrings) and (Unwrap(TStrings(obj).Text, child) <> '') then
          Exit;
      end;
    end; //for iControl
  finally FreeAndNil(ignored); end;
  Result := false;
end; { ContainsNonemptyControl }

{:Copies data from .Text and .Checked properties to published object properties with the
  same name as the control names (after prefix is removed from the control name).
  @since   2003-12-11
}
procedure CopyControlsToProperties(sourceParent: TWinControl;
  targetObj: TPersistent; const removePrefixFromControls: string);
var
  child     : TControl;
  iProperty : integer;
  obj       : TObject;
  sourceProp: IGpProperty;
begin
  sourceProp := CreateGpProperty(targetObj);
  for iProperty := 0 to sourceProp.Count-1 do begin
    child := sourceParent.FindChildControl(removePrefixFromControls+sourceProp.Name[iProperty]);
    if assigned(child) then begin
      if IsPublishedProp(child, 'Text') then
        sourceProp.StringValue[iProperty] := GetStrProp(child, 'Text')
      else if IsPublishedProp(child, 'Checked') then
        sourceProp.BooleanValue[iProperty] := (GetOrdProp(child, 'Checked') <> 0)
      else if IsPublishedProp(child, 'Lines') then begin
        obj := TObject(cardinal(GetOrdProp(child, 'Lines')));
        if obj is TStrings then
          sourceProp.StringValue[iProperty] := Unwrap(TStrings(obj).Text, child);
      end;
    end;
  end; //for
end; { CopyControlsToProperties }

{:Copies data from published object properties to .Text and .Checked properties of
  controls with the same name as the object properties (after prefix is removed from the
  control name).
  @since   2003-12-11
}
procedure CopyPropertiesToControls(sourceObj: TPersistent;
  targetParent: TWinControl; const prefixControlsWith: string);
var
  child     : TControl;
  iProperty : integer;
  obj       : TObject;
  sourceProp: IGpProperty;
begin
  sourceProp := CreateGpProperty(sourceObj);
  for iProperty := 0 to sourceProp.Count-1 do begin
    child := targetParent.FindChildControl(prefixControlsWith+sourceProp.Name[iProperty]);
    if assigned(child) then begin
      if IsPublishedProp(child, 'Text') then
        SetStrProp(child, 'Text', sourceProp.StringValue[iProperty])
      else if IsPublishedProp(child, 'Checked') then
        SetOrdProp(child, 'Checked', Ord(sourceProp.BooleanValue[iProperty]))
      else if IsPublishedProp(child, 'Lines') then begin
        obj := TObject(cardinal(GetOrdProp(child, 'Lines')));
        if (obj is TStrings) then
          TStrings(obj).Text := sourceProp.StringValue[iProperty];
      end;
    end;
  end; //for
end; { CopyPropertiesToControls }

{:Checks if data in .Text and .Checked properties is the same as in published object
  properties with the same name as the control names (after prefix is removed
  from the control name).
  @since   2003-12-24
}
function SameControlsAndProperties(controlParent: TWinControl;
  compareObj: TPersistent; const removePrefixFromControls: string): boolean;
var
  child     : TControl;
  iProperty : integer;
  obj       : TObject;
  sourceProp: IGpProperty;
begin
  Result := false;
  sourceProp := CreateGpProperty(compareObj);
  for iProperty := 0 to sourceProp.Count-1 do begin
    child := controlParent.FindChildControl(removePrefixFromControls+sourceProp.Name[iProperty]);
    if assigned(child) then begin
      if IsPublishedProp(child, 'Text') and
         (sourceProp.StringValue[iProperty] <> GetStrProp(child, 'Text'))
      then
        Exit;
      if IsPublishedProp(child, 'Checked') and
         (sourceProp.BooleanValue[iProperty] <> (GetOrdProp(child, 'Checked') <> 0))
      then
        Exit;
      if IsPublishedProp(child, 'Lines') then begin
        obj := TObject(cardinal(GetOrdProp(child, 'Lines')));
        if (obj is TStrings) then
          if sourceProp.StringValue[iProperty] <> Unwrap(TStrings(obj).Text, child) then
            Exit;
      end;
    end;
  end; //for
  Result := true;
end; { SameControlsAndProperties }

function ControlByTag(parent: TWinControl; tag: integer; 
  searchInSubcontrols: boolean): TControl;
begin
  Result := ControlByTag(parent, tag, searchInSubcontrols, [nil]);
end; { ControlByTag }

function ControlByTag(parent: TWinControl; tag: integer; searchInSubcontrols: boolean;
  allowOnly: array of TControlClass): TControl; 
begin
  Result := ControlByTag(parent, tag, searchInSubcontrols, allowOnly, 0);
end; { ControlByTag }

function ControlByTag(parent: TWinControl; tag: integer; searchInSubcontrols: boolean;
  allowOnly: array of TControlClass; skipFirstN: integer): TControl;

  function IsAllowed(control: TControl): boolean;
  var
    iAllowed: integer;
  begin
    if (Length(allowOnly) = 0) or ((Length(allowOnly) = 1) and (allowOnly[Low(allowOnly)] = nil)) then
      Result := true
    else begin
      Result := true;
      for iAllowed := Low(allowOnly) to High(allowOnly) do
        if allowOnly[iAllowed] = control.ClassType then
          Exit;
      Result := false;
    end;
  end; { IsAllowed }

var
  iControl: integer;
  
begin { ControlByTag }
  Result := nil;
  for iControl := 0 to parent.ControlCount-1 do begin
    if (parent.Controls[iControl].Tag = tag) and IsAllowed(parent.Controls[iControl]) then
    begin
      if skipFirstN > 0 then
        Dec(skipFirstN)
      else begin
        Result := parent.Controls[iControl];
        break; //for iControl
      end;
    end;
  end; //for iControl
  if (not assigned(Result)) and searchInSubcontrols then begin
    for iControl := 0 to parent.ControlCount-1 do begin
      if parent.Controls[iControl] is TWinControl then
        Result := ControlByTag(parent.Controls[iControl] as TWinControl, tag, true,
                    allowOnly);
      if assigned(Result) then
        break; //for iControl
    end; //for iControl
  end;
end; { ControlByTag }

function ControlByClass(parent: TWinControl; controlClass: TClass;
  controlIndex: integer): TControl;
var
  iControl : integer;
  occurence: integer;
begin
  Result := nil;
  occurence := 0;
  for iControl := 0 to parent.ControlCount - 1 do begin
    if parent.Controls[iControl] is controlClass then begin
      Inc(occurence);
      if occurence = controlIndex then begin
        Result := parent.Controls[iControl];
        break; //for
      end;
    end;
  end;
end; { ControlByClass }

procedure ControlsByClass(parent: TWinControl; controlClass: TClass;
  controls: TObjectList);
var
  iControl: integer;
begin
  controls.OwnsObjects := false; //better safe than sorry
  controls.Clear;
  for iControl := 0 to parent.ControlCount - 1 do
    if parent.Controls[iControl] is controlClass then
      controls.Add(parent.Controls[iControl]);
end; { ControlsByClass }

function ComponentByTag(parent: TComponent; tag: integer;
  searchInSubcomponents: boolean): TComponent;
begin
  Result := ComponentByTag(parent, tag, searchInSubcomponents, [nil]);
end; { ComponentByTag }

function ComponentByTag(parent: TComponent; tag: integer; searchInSubcomponents: boolean;
  allowOnly: array of TComponentClass): TComponent; 
begin
  Result := ComponentByTag(parent, tag, searchInSubcomponents, allowOnly, 0);
end; { ComponentByTag }

function ComponentByTag(parent: TComponent; tag: integer; searchInSubcomponents: boolean;
  allowOnly: array of TComponentClass; skipFirstN: integer): TComponent;

  function IsAllowed(control: TComponent): boolean;
  var
    iAllowed: integer;
  begin
    if (Length(allowOnly) = 0) or ((Length(allowOnly) = 1) and
       (allowOnly[Low(allowOnly)] = nil))
    then
      Result := true
    else begin
      Result := true;
      for iAllowed := Low(allowOnly) to High(allowOnly) do
        if allowOnly[iAllowed] = control.ClassType then
          Exit;
      Result := false;
    end;
  end; { IsAllowed }

var
  iComponent: integer;

begin { ComponentByTag }
  Result := nil;
  for iComponent := 0 to parent.ComponentCount-1 do begin
    if (parent.Components[iComponent].Tag = tag) and
       IsAllowed(parent.Components[iComponent]) then
    begin
      if skipFirstN > 0 then
        Dec(skipFirstN)
      else begin
        Result := parent.Components[iComponent];
        break; //for iComponent
      end;
    end;
  end; //for iControl
  if (not assigned(Result)) and searchInSubcomponents then begin
    for iComponent := 0 to parent.ComponentCount-1 do begin
      Result := ComponentByTag(parent.Components[iComponent], tag, true, allowOnly);
      if assigned(Result) then
        break; //for iComponent
    end; //for iComponent
  end;
end; { ComponentByTag }

function CanAllocateHandle(control: TWinControl): boolean;
begin
  Result := false;
  repeat
    if control.HandleAllocated then begin
      Result := true;
      break; //repeat
    end;
    control := control.Parent;
  until control = nil;
end; { CanAllocateHandle }

procedure DisableChildControls(parent: TWinControl; disabledList: TList = nil);
var
  iControl: integer;
begin
  for iControl := 0 to parent.ControlCount-1 do
    if parent.Controls[iControl].Enabled then begin
      parent.Controls[iControl].Enabled := false;
      disabledList.Add(parent.Controls[iControl]);
    end;
end; { DisableChildControls }

procedure EnableChildControls(parent: TWinControl; enabledList: TList = nil);
var
  iControl: integer;
begin
  for iControl := 0 to parent.ControlCount-1 do
    if not parent.Controls[iControl].Enabled then begin
      parent.Controls[iControl].Enabled := true;
      enabledList.Add(parent.Controls[iControl]);
    end;
end; { EnableChildControls }

procedure DisableControls(controlList: TList);
var
  iControl: integer;
begin
  for iControl := 0 to controlList.Count-1  do
    TControl(controlList[iControl]).Enabled := false;
end; { DisableControls }

procedure EnableControls(controlList: TList);
var
  iControl: integer;
begin
  for iControl := 0 to controlList.Count-1  do
    TControl(controlList[iControl]).Enabled := true;
end; { EnableControls }

function EnumControls(parent: TWinControl; matchClass: TClass = nil): TControlEnumeratorFactory;
begin
  Result := TControlEnumeratorFactory.Create(parent, matchClass);
end; { EnumControls }

{ TControlEnumerator }

constructor TControlEnumerator.Create(parent: TWinControl; matchClass: TClass);
begin
  ceParent := parent;
  ceClass := matchClass;
  ceIndex := -1;
end; { TControlEnumerator.Create }

function TControlEnumerator.GetCurrent: TControl;
begin
  Result := ceParent.Controls[ceIndex];
end; { TControlEnumerator.GetCurrent }

function TControlEnumerator.MoveNext: boolean;
begin
  Result := false;
  while ceIndex < (ceParent.ControlCount - 1) do begin
    Inc(ceIndex);
    if (ceClass = nil) or (ceParent.Controls[ceIndex].InheritsFrom(ceClass)) then begin
      Result := true;
      break; //while
    end;
  end; //while
end; { TControlEnumerator.MoveNext }

{ TControlEnumeratorFactory }

constructor TControlEnumeratorFactory.Create(parent: TWinControl; matchClass: TClass);
begin
  cefParent := parent;
  cefClass := matchClass;
end; { TControlEnumeratorFactory.Create }

function TControlEnumeratorFactory.GetEnumerator: TControlEnumerator;
begin
  Result := TControlEnumerator.Create(cefParent, cefClass);
end; { TControlEnumeratorFactory.GetEnumerator }

end.
