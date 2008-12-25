(*:Partial replacement for the C's trinary ?: operator.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2003, Primoz Gabrijelcic
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
   Creation date     : unknown
   Last modification : 2003-06-19
   Version           : 1.01
</pre>*)(*
   History:
     1.01: 2003-06-19
       - BSD license added.
*)

unit GpIFF;

interface

  function IFF(condit: boolean; iftrue, iffalse: string): string; overload;
  function IFF(condit: boolean; iftrue, iffalse: integer): integer; overload;
  function IFF(condit: boolean; iftrue, iffalse: real): real; overload;
  function IFF(condit: boolean; iftrue, iffalse: boolean): boolean; overload;
  function IFF(condit: boolean; iftrue, iffalse: pointer): pointer; overload;

implementation

  function IFF(condit: boolean; iftrue, iffalse: string): string;
  begin
    if condit then
      Result := iftrue
    else
      Result := iffalse;
  end; { IFF }

  function IFF(condit: boolean; iftrue, iffalse: integer): integer;
  begin
    if condit then
      Result := iftrue
    else
      Result := iffalse;
  end; { IFF }

  function IFF(condit: boolean; iftrue, iffalse: real): real;
  begin
    if condit then
      Result := iftrue
    else
      Result := iffalse;
  end; { IFF }

  function IFF(condit: boolean; iftrue, iffalse: boolean): boolean;
  begin
    if condit then
      Result := iftrue
    else
      Result := iffalse;
  end; { IFF }

  function IFF(condit: boolean; iftrue, iffalse: pointer): pointer;
  begin
    if condit then
      Result := iftrue
    else
      Result := iffalse;
  end; { IFF }

end.
