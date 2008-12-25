(*:GpProfile common look and feel management.
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

unit gppLookAndFeel;

interface

uses
  ExtCtrls;

var
  CGppMenuColor         : integer;
  CGppHighlightMenuColor: integer;
  CGppShadowMenuColor   : integer;

procedure GppPaintHorizontalGradient(paintBox: TPaintBox);
procedure GppPaintVerticalGradient(paintBox: TPaintBox);

implementation

uses
  GraphUtil,
  Graphics,
  Controls,
  Classes;

procedure GppPaintHorizontalGradient(paintBox: TPaintBox);
begin
  GradientFillCanvas(paintBox.Canvas, CGppMenuColor, clWhite, paintBox.ClientRect,
    gdHorizontal);
end; { GppPaintHorizontalGradient }

procedure GppPaintVerticalGradient(paintBox: TPaintBox);
begin
  if paintBox.Align = alTop then
    GradientFillCanvas(paintBox.Canvas, CGppMenuColor, clWhite, paintBox.ClientRect,
      gdVertical)
  else
    GradientFillCanvas(paintBox.Canvas, clWhite, CGppMenuColor, paintBox.ClientRect,
      gdVertical);
end; { GppPaintVerticalGradient }

{ TGradientPanel }

initialization
  CGppMenuColor := $ACE3FA;
  CGppHighlightMenuColor := GetHighlightColor(CGppMenuColor);
  CGppShadowMenuColor := GetShadowColor(CGppMenuColor, -150);
end.
