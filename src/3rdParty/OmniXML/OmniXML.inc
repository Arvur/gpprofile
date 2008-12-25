{ $OmniXML: OmniXML/OmniXML.inc,v 1.3 2006/02/14 15:47:39 mremec Exp $ }

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF Declared(RTLVersion) and (RTLVersion >= 14)}
    {$DEFINE HAS_UNIT_VARIANTS}
    {$DEFINE DELPHI6_UP}
  {$IFEND}
{$ENDIF}

{$I OmniXML_JEDI.inc}
