{ putcode.pas }
{<<<}
{ NOTICE OF COPYRIGHT AND OWNERSHIP OF SOFTWARE:

  Copyright (C) 1984 Oregon Software, Inc.
  All Rights Reserved.

  This program is the property of Oregon Software.  The program or
  parts of it may be copied and used only as provided under a signed
  license agreement with Oregon Software.  Any support purchased from
  Oregon Software does not apply to user-modified programs.  All copies
  of this program must display this notice and all copyright notices.


  Release version: 0045  Level: 1
  Processor: ~processor~
  System: ~system~

  Pascal-2 compiler object file generator.

 Last modified by KRIS on 21-Nov-1990 15:31:24
 Purpose:
Update release version for PC-VV0-GS0 at 2.3.0.1

}
{>>>}
{$nomain}
%include 'ident.def';
%include 'hdr.def';
%include 'hdrprc.def';
%include 'hdrc.def';
%include 'commc.def';

{<<<}
procedure writech(ch: char);

  begin
    write(MacFile, ch);
    column := column + 1;
  end {writech};
{>>>}
{<<<}
procedure writestr(s: packed array[low..high: integer] of char);

  var i, j: integer;

  begin
    j := high;
    while (j > 0) and (s[j] = ' ') do j := j - 1;
    column := column + j;
    for i := low to j do
      write(MacFile, s[i]);
  end {writestr};
{>>>}
{<<<}
procedure writeint(v: integer);

  var
    bufptr: 0..9;
    buffer: array [1..20] of char;
    u: unsigned;

  begin
    bufptr := 0;
    if v < 0 then
      writech('-');
    u := abs(v);

    repeat
      bufptr := bufptr + 1;
      buffer[bufptr] := chr (u mod 10 + ord('0'));
      u := u div 10;
    until u = 0;

    repeat
      writech(buffer[bufptr]);
      bufptr := bufptr - 1;
    until bufptr = 0;
  end; {writeint}
{>>>}
{<<<}
procedure WriteHex(v: unsigned {value to write} );

{ Write an unsigned value to the macro file as a hexadecimal number.
  16 bit values only.
}
  const
    maxhex = 4;

  var
    hexbuf: packed array [1..maxhex] of char;
    i: 1..maxhex;    { induction var for filling hexbuf }
    j: 0..15;        { numeric value of one hex digit }

  begin
    v := v and 65535; {** a kludge **}
    for i := maxhex downto 1 do begin
      j  := v mod 16;
      v := v div 16;
      if j <= 9 then
        hexbuf[i] := chr(ord('0') + j)
      else hexbuf[i] := chr(ord('A') + j - 10);
      end; { for i }
    write(MacFile, hexbuf);
    column := column + 4;
  end {WriteHex} ;
{>>>}
{<<<}
procedure WriteHexLong(v: unsigned {value to write} );

{ Write an unsigned 32 bit value to the macro file as a hexadecimal number.
}
  const
    maxhex = 8;

  var
    hexbuf: packed array [1..maxhex] of char;
    i: 1..maxhex;    { induction var for filling hexbuf }
    j: 0..15;        { numeric value of one hex digit }

  begin {WriteHexLong}
    for i := maxhex downto 1 do begin
      j  := v mod 16;
      v := v div 16;
      if j <= 9 then
        hexbuf[i] := chr(ord('0') + j)
      else hexbuf[i] := chr(ord('A') + j - 10);
      end; { for i }
    write(MacFile, hexbuf);
    column := column + 4;
  end; {WriteHexLong}
{>>>}
{<<<}
procedure reposition(col: columnrange);

  begin
    if (column >= col) {we're at or past the desired column}
       then writech(' ');  { emit one space at least }

    while column < col do
      writech(' ');
  end {reposition};
{>>>}
{<<<}
procedure writeline;

  begin
    writeln(MacFile);
    column := 1;
  end {writeline};
{>>>}
{<<<}
function uppercase(ch: char): char;

  begin
    if (ch >= 'a') and (ch <= 'z') then
      uppercase := chr(ord(ch) - ord('a') + ord('A'))
    else uppercase := ch;
  end; {uppercase}
{>>>}
{<<<}
procedure allocfixup;

  begin
    if fixuphead = nil then begin { first time }
      new(fixuphead);
      fixuptail := fixuphead;
      end

    else begin { tack new node on end of list }
      new(fixuptail^.fixuplink);
      fixuptail := fixuptail^.fixuplink;
      end;  { tack new node }

    with fixuptail^ do begin
      fixuplink := nil;  { new end of list }
      fixupaddr := undefinedaddr;
      fixupobjpc := currentpc;  { this data is only required by the test
                                  dumper }
      end;
  end;  { allocfixup }
{>>>}
{<<<}
  procedure absfixup(var ref: fixupptr; len: integer);

{ Generate a fixup record for an absolute fixup.  This is expected to
  be called just before the word is generated, and "ref" will be used
  to provide the absolute value at some later time
}

  begin
    allocfixup;
    ref := fixuptail;
    with ref^ do
      begin
      fixupkind := fixupabs;
      fixupfileloc := tempfilesize + nexttempbuffer + 4;
      fixupobjpc := currentpc;
      fixuplen := len;
      end;
  end; {absfixup}
{>>>}
{<<<}
  procedure insertnewESD;

{ The global variable "newESD" is to be inserted into the ESDtable.
  Additionally, we must check for table overflow (abort if so).
}
    begin
      if nextESD = lastESD then abort(manyexterns);

      ESDtable[nextESD] := newESD;  { that's easy enough }
      nextESD := nextESD + 1;   { may actually be beyond linker's range }
    end;  { insertnewESD }
{>>>}
{<<<}
procedure findESDid;

{ Compute the ESDid for the given entry.  If not found, insert it.

  Note that the ESDid is not just the index into the ESDtable because
  XDEF's and "standard load sections" (for instance) are not assigned ESDid's.
}
  var
    ESD: ESDrange;

  begin
    ESD := firstESD;
    ESDid := firstESD;
    found := false;

    while (ESD < nextESD) and not found do begin

      with ESDtable[ESD] do
        if ESDkind = ESDsupport then

          if (newESD.ESDkind = ESDsupport) and
             (newESD.suppno = suppno) then
            found := true
          else ESDid := ESDid + 1

        else if ESDkind = ESDexternal then

          if (newESD.ESDkind = ESDexternal) and
             (newESD.exproc = exproc) then
            found := true
          else ESDid := ESDid + 1

        else if ESDkind = ESDdefine then
          begin
          { ESDdefine is odd because there is no ESDid assigned, but the check
            is here so xdefs are not added to the table more than once.
          }
          if (newESD.ESDkind = ESDdefine) and
             (newESD.vartabindex = vartabindex) then
            found := true
          end

        else if ESDkind in [ESDuse, ESDshrvar] then

          if (newESD.ESDkind = ESDkind) and
             (newESD.vartabindex = vartabindex) then
            found := true
          else ESDid := ESDid + 1

        else if ESDkind in [ESDcommon, ESDdiag] then
          if newESD.ESDkind = ESDkind then found := true
          else ESDid := ESDid + 1;

      ESD := ESD + 1;
      end; {while}

    if not found then
      insertnewESD;   { nextESD bumps, but not ESDid }
  end; { findESDid }
{>>>}
{<<<}
procedure putrelfile(data: unsigned);

  begin
    if nextrelfile = maxrelfile then begin
      put(relfile);
      nextrelfile := 0;
      end
    else nextrelfile := nextrelfile + 1;
    relfile^[nextrelfile] := data;
  end; {puttemp}
{>>>}
{<<<}
procedure flushtempbuffer;

{ Write the current contents of the tempbuffer to the temporary object
  file.
}
  var
    i : 0..31;  { induction var for buffer copying }
    packbits: unsigned;  { pseudo packed array of boolean }


  begin
    if nexttempbuffer > 0 then
      begin
      putrelfile((sectionno[currentsect] + 1) * 256 +
                     nexttempbuffer);
      for i := nexttemprelocn to 31 do temprelocn[i] := false;
      packbits := 0;
      for i := 0 to 15 do begin
        packbits := packbits * 2;
        if temprelocn[i] then
          packbits := packbits + 1;
        end;
      putrelfile(packbits);

      packbits := 0;
      for i := 16 to 31 do begin
        packbits := packbits * 2;
        if temprelocn[i] then
          packbits := packbits + 1;
        end;
      putrelfile(packbits);

      for i := 0 to nexttempbuffer - 1 do
        putrelfile(tempbuffer[i]);

      tempfilesize := tempfilesize + nexttempbuffer + 3;
      nexttempbuffer := 0;
      nexttemprelocn:= 0;
      end;
  end; {flushtempbuffer}
{>>>}
{<<<}
procedure putdata(data: unsigned);

  begin
    tempbuffer[nexttempbuffer] := data and 65535;
    nexttempbuffer := nexttempbuffer + 1;
    if nexttempbuffer >= maxtempbuffer then flushtempbuffer;
  end;
{>>>}
{<<<}
procedure putbuffer(data: unsigned; reloc: boolean);

  begin
    temprelocn[nexttemprelocn] := reloc;
    nexttemprelocn := nexttemprelocn + 1;
    putdata(data);
  end;  {putbuffer}
{>>>}
{<<<}
procedure newsection(newsect: section {section to switch to} );

{ Change sections to "newsect".  If there is any data accumulated for the
  object file, that buffer is written.
}

  begin
    if newsect <> currentsect then
      begin
      if switcheverplus[outputmacro] then
        begin
        reposition(opcolumn);
        writestr('SECTION');
        if newsect = codesect then writestr(sectiontail);
        reposition(opndcolumn);
        writeint(sectionno[newsect]);
        writeline;
        end;
      if switcheverplus[outputobj] then flushtempbuffer;
      sectionpc[currentsect] := currentpc;
      currentsect := newsect;
      currentpc := sectionpc[currentsect];
      end;
  end; {newsection}
{>>>}
{<<<}
procedure seekstringfile(n: integer {byte to access});

{ Do the equivalent of a "seek" on the string file.  This sets the
  file and "nextstringfile" to access byte "n" of the stringfile.
}

  var
    newblock: 1..maxstringblks; { block to which seeking }


  begin {seekstringfile}
    newblock := n div (diskbufsize + 1) + 1;
    if newblock <> curstringblock then
      begin
      if needcaching then
        begin
        seek(stringfile, newblock);
        curstringblock := newblock;
        end
      else
        begin
        curstringblock := newblock;
        stringblkptr := stringblkptrtbl[newblock];
        if stringblkptr = nil then
          begin
          write('unexpected end of stringtable ');
          abort(inconsistent);
          end;
        end;
      end;
    nextstringfile := n mod (diskbufsize + 1);
  end {seekstringfile} ;
{>>>}
{<<<}
function getstringfile: hostfilebyte;

{ move stringfile buffer pointer to next entry.  'get' is called
  to fill buffer if pointer is at the end.
}

  begin
    if nextstringfile > diskbufsize then
      begin
      nextstringfile := 0;
      if needcaching then
        begin
        get(stringfile);
        curstringblock := curstringblock + 1;
        end
      else
        begin
        curstringblock := curstringblock + 1;
        stringblkptr := stringblkptrtbl[curstringblock];
        if stringblkptr = nil then
          begin
          write('unexpected end of stringtable ');
          abort(inconsistent);
          end;
        end;
      end;

    if needcaching then getstringfile := stringfile^[nextstringfile]
    else getstringfile := stringblkptr^[nextstringfile];

    nextstringfile := nextstringfile + 1;
  end {getstringfile} ;
{>>>}
{<<<}
procedure writeprocname(procn: proctableindex; {number of procedure to copy}
                        len: integer {characters to write} );

{ Copy the procedure name for procedure "procn" from the string file
  to the macro file.
}

  var
    i: integer; {induction var for copy}

  begin
    if needcaching then
      seekstringfile(stringfilecount + proctable[procn].charindex - 1)
    else
      begin
      curstringblock := (stringfilecount + proctable[procn].charindex - 1) div
         (diskbufsize + 1) + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := (stringfilecount + proctable[procn].charindex - 1) mod
                        (diskbufsize + 1);
      end;
    for i := 1 to min(len, proctable[procn].charlen) do
      if language = pascal then writech(uppercase(chr(getstringfile)))
      else writech(chr(getstringfile));
  end {writeprocname} ;
{>>>}
{<<<}
{ Diagnostic table generation.
  The diagnostic tables are generated to allow a walkback by line and
  procedure in case of error.  They are generated in section 14, so the
  diagnostics for each compilation unit are concatenated.  A reference to
  a common section in section 14 is generated to indicate the end of
  the tables.
  The syntax of the tables, as currently generated, is
  diagtables = endpointer       (distance to end of tables)
               startpc          (start of code for this unit)
               codelength       (length of code for this unit)
               [* procedure *]
               startpointer     (distance to start of tables)
  The tables are bitwise packed and highly encoded.  The format for various
  pieces is given in the individual routines generating them.
}
{>>>}
{<<<}
procedure diag_word(value: unsigned {value to put} );

{ Put a single word of diagnostic data to the macro and object files.
  At the moment it is formatted with one value per line.  This is
  subject to change to make the file look prettier.
}


  begin
    newsection(diagsect);
    if switcheverplus[outputmacro] then
      begin
      reposition(opcolumn);
      writestr('DC.W');
      reposition(opndcolumn);
      writech('$');
      writehex(value);
      writeline;
      end;

    if switcheverplus[outputobj] then
      putbuffer(value, false);
    currentpc := currentpc + 2;
  end; {diag_word}
{>>>}
{<<<}
procedure diag_bits(value: unsigned; {value to generate}
                    length: bits {number of bits in value} );

{ Put "length" bits of "value" to the diagnostic bit stream.
}

  var
    i: bits; {induction var}
    bit: array [bits] of 0..1; {each bit}


  begin
    for i := 0 to length - 1 do
      begin
      bit[i] := value mod 2;
      value := value div 2;
      end;

    for i := length - 1 downto 0 do
      begin
      diagbitbuffer := diagbitbuffer * 2 + bit[i];
      if nextdiagbit = 15 then
        begin
        diag_word(diagbitbuffer);
        diagbitbuffer := 0;
        nextdiagbit := 0;
        end
      else
        nextdiagbit := nextdiagbit + 1;
      end;
  end; {diag_bits}
{>>>}
{<<<}
procedure initdiags;

{ Initialize the diagnostic code, called only if diagnostics are
  going to be generated.
}

var kludge: integer; {to get around a purposeful range error}

  begin
    everdiagnosing := true;
    nextdiagbit := 0;
    diagbitbuffer := 0;

    if switcheverplus[outputmacro] then
      begin
      writeln(macfile, 'P_DIAG', 'SECTION': opcolumn + 6 - 6,
              ' ': opndcolumn - opcolumn - 7, sectionno[diagsect]:1);
      newsection(diagsect);
      writeln(macfile, 'STDIAG', 'DC.W': opcolumn + 3 - 6,
              'ENDIAG-STDIAG': opndcolumn - opcolumn - 4 + 13);
      writeln(macfile, 'DC.L': opcolumn + 3,
              'L': opndcolumn - opcolumn - 4 + 1);
      writeln(macfile, 'DC.L': opcolumn + 3, 'LAST-L':
              opndcolumn - opcolumn - 4 + 6);
      if switcheverplus[debugging] or switcheverplus[profiling] then
        begin
        writeln(macfile, 'DC.B': opcolumn+3, '''':opndcolumn - opcolumn - 4,
                outputname: 8, '''');
        write(macfile, 'DC.L':opcolumn + 3, ' ':opndcolumn - opcolumn - 4);
        if switcheverplus[own] and (ownsize > 0) then writeln(macfile, 'G')
        else writeln(macfile, '0');
        end;
      end;

    if switcheverplus[outputobj] then
      begin
      newsection(diagsect);
      absfixup(diaglenfix, word);
      putbuffer(0, false);
      putbuffer(50B * 256 + sectionno[codesect] + 1, true);
      currentpc := 6; {needed for absfixup diagnostic}
      absfixup(codelenfix, long);
      putbuffer(0, false);
      putbuffer(0, false);

      if switcheverplus[debugging] or switcheverplus[profiling] then
        begin
        putbuffer(ord(outputname[1]) * 256 + ord(outputname[2]), false);
        putbuffer(ord(outputname[3]) * 256 + ord(outputname[4]), false);
        putbuffer(ord(outputname[5]) * 256 + ord(outputname[6]), false);
        putbuffer(ord(outputname[7]) * 256 + ord(outputname[8]), false);

        if switcheverplus[own] and (ownsize > 0) then
          putbuffer(50B * 256, true)
        else
          begin
          putbuffer(0, false);
          putbuffer(0, false);
          end;
        end;
      end;

    if switcheverplus[debugging] or switcheverplus[profiling] then
      currentpc := 22
    else currentpc := 10; {length of header data}
  end; {initdiags}
{>>>}
{<<<}
procedure WriteSymbolName(name: packed array[m..n: integer] of char);
{ Write a symbol name string to the macro file. }
var i: 0..maxprocnamelen;
begin
  i := 0;
  while (i < n) and (name[i + 1] <> ' ') do begin
    i := i + 1;
    if language = pascal then writech(uppercase(name[i]))
    else writech(name[i]);
    end;
end;
{>>>}
{<<<}
procedure import_name(n: integer;
                      var name: packed array [lo..hi: integer] of char);

  { Return a Modula2 import name;
    name is prefixed with 'i_' and is blank-terminated (blank may be
    followed by garbage).
  }

  var
    i, t: 0..maxprocnamelen;


  begin
    if language = modula2 then
      begin
      seekstringfile(stringfilecount + proctable[n].charindex - 1);
      t := proctable[n].charlen + 2;
      if t > maxprocnamelen then t := maxprocnamelen;
      name[1] := 'i';
      name[2] := '_';

      for i := 3 to t do name[i] := chr(getstringfile);

      name[t + 1] := ' ';
      end;
  end; {import_name}
{>>>}

{<<<}
function get_from_sfile (loc, len: integer;
                         ident: boolean {true if in identifier section} ): linknametype;
{ Read a string starting from "loc", "len" characters long from the stringtable.
}
  var
    i: integer;
    linkname: linknametype;

  begin {get_from_sfile}
  if ident then loc := stringfilecount + loc - 1; {skip strings}

  if needcaching then
    seekstringfile(loc)
  else
    begin
    curstringblock := loc div (diskbufsize + 1) + 1;
    stringblkptr := stringblkptrtbl[curstringblock];
    nextstringfile := loc mod (diskbufsize + 1);
    end;

  for i := 1 to maxprocnamelen do
    linkname[i] := ' '; { initialize link name }

  for i := 1 to min(linknameused, len) do
    begin { copy procedure name from string file }
    if language = pascal then linkname[i] := uppercase(chr(getstringfile))
    else linkname[i] := chr(getstringfile);
    end;

  get_from_sfile := linkname;
  end; {get_from_sfile}
{>>>}
{<<<}
procedure CopySFile;

{ Copy the string table and constant table from the string file to
  the macro file.  This is written as straight binary data, with no
  attempt to interpret it as strings or real numbers.

  The string file is actually divided into three parts.  The first is
  the string table, which contains string constants parsed by the
  scanner.  The second part is a table of identifiers, and the third
  part is constants built by analys.  Only the first and third
  parts are written here.
}

  var
    i: integer; { outer loop counter }


  procedure write_constants(i: integer);

    const
      charsperline = 12;  { number of chars in translated string }
      wordsperline = 6;  { number of constant structure values (hex) per line }

    var
      buffer: packed array [1..charsperline] of char;
      ch: char;
      old_i: integer;
      k: integer;
      v: uns_word;

    begin {write_constants}
    while i > 0 do
      begin
      reposition(opcolumn);
      writestr('DC.W');
      reposition(opndcolumn);
      old_i := i;

      for k := 1 to min(wordsperline, (i + 1) div 2) do
        begin
        if k > 1 then { separate words with commas }
          writech(',');
        v := getstringfile * 256;
        i := i - 1;

        if i > 0 then
          begin { don't grab a non-existent odd byte }
          v := getstringfile + v;
          i := i - 1;
          end;

        Writech('$');
        WriteHex(v);

        ch := chr(v div 256);

        if (ch < ' ') or (ch > '~') then buffer[k * 2 - 1] := '.'
        else buffer[k * 2 - 1] := ch;

        ch := chr(v mod 256);

        if (ch < ' ') or (ch > '~') then buffer[k * 2] := '.'
        else buffer[k * 2] := ch;
        end; { for k }

      reposition(nodecolumn);

      for k := 1 to min(charsperline, old_i) do
        writech(buffer[k]);

      writeline;
      end;
    end; {write_constants}


  begin {CopySFile}
    if needcaching then
      seekstringfile(0)
    else
      begin
      curstringblock := 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := 0;
    end;

    { first write the string table as fixed length character strings }

    i := stringfilecount;
    if i > 0 then begin
      writech('*');
      writeline;
      writeln(MacFile, '*  String Constants:');
      writech('*');
      writeline;
      writech('L'); { the label associated with string constants }
      writech(':');
      end;

    write_constants(i);

    currentpc := stringfilecount;

    {now move on to the constant table and write it}

    if needcaching then
      seekstringfile(stringtablelimit)
    else
      begin
      curstringblock := stringtablelimit div (diskbufsize + 1) + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := stringtablelimit mod (diskbufsize + 1);
      end;

    i := consttablelimit - stringtablelimit;

    if i > 0 then
      begin
      writech('*');
      writeline;
      writeln(MacFile, '*  Structured Constants:');
      writech('*');
      writeline;

      if currentpc = 0 then
        begin
        writech('L'); { the label associated with structured constants }
        writech(':');
        end;

      currentpc := currentpc + i;
      end;

    if odd(currentpc) then currentpc := currentpc + 1; { should not be necessary! }

    write_constants(i);

    if odd(currentpc) then currentpc := currentpc + 1; { should not be necessary! }

    if currentpc = 0 then
      begin
      writech('L');
      reposition(opcolumn);
      writestr('EQU');
      reposition(opndcolumn);
      writech('*');
      writeline;
      end;
  end {CopySFile} ;
{>>>}
{<<<}
procedure InitMac;

{ Generate preliminary pieces of the macro file. In the process, some
  global variables used for code generation are initialized to point to
  some generated tables.
}

  var
    loc: integer; {location in the string file}
    i: integer; {induction variable}


  begin {initmac}

    if shortsection then sectiontail := '.S' else sectiontail := '  ';

    write(MacFile, '*  Oregon Software Pascal-2 V', version, ' ');

    if (hostopsys = targetopsys) and (hostmachine = targetmachine) then
      begin
      writeln(MacFile, 'Compiler');
      write(MacFile, '*         ');
      end
    else
      begin
      writeln(MacFile, 'Cross Compiler');

      case hostmachine of
        vax:
          write(MacFile, '*    VAX ');
        iapx86:
          write(MacFile, '*    IAPX-86 ');
        ns32k:
          write(MacFile, '*    NS32000 ');
        mc68000:
          write(MacFile, '*    MC68000 ');
        otherwise
          write(MacFile, '*    ??? ');
        end;

      case hostopsys of
        vms:
          write(MacFile, '(VMS) to ');
        unix:
          write(MacFile, '(Unix) to ');
        msdos:
          write(MacFile, '(MSDOS) to ');
        end;
      end;

    if mc68020 then
      begin
      write(MacFile, 'MC68020');
      if mc68881 then write(MacFile, '/MC68881');
      writeln(MacFile, ' (VERSAdos)');
      end
    else
      writeln(MacFile, 'MC68000 (VERSAdos)');

    write(MacFile, '*  Assembly Listing of:  ');
    writesymbolname(outputname);
    writeline;
    writech('*');
    writeline;

    if newobjectfmt then
      begin
      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('NEWOBJ');
      writeline;

      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('CASE');
      writeline;

      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('MODULA2');
      writeline;
      end;

    writesymbolname(outputname);
    reposition(opcolumn);

    if newobjectfmt then
      writestr('NIDNT')
    else writestr('IDNT');

    reposition(opndcolumn);

    if newobjectfmt then writech('''');

    writeint(objversion);
    writech(',');
    writeint(objrevision);

    if newobjectfmt then writech('''');

    if ident_strlength > 0 then
      begin
      writech(' ');
      loc := stringfilecount + ident_string - 1;

      if needcaching then
        seekstringfile(loc)
      else
        begin
        curstringblock := loc div (diskbufsize + 1) + 1;
        stringblkptr := stringblkptrtbl[curstringblock];
        nextstringfile := loc mod (diskbufsize + 1);
        end;

      if newobjectfmt then writech('''');

      for i := 1 to ident_strlength do
        writech(chr(getstringfile));

      if newobjectfmt then writech('''');
      end;

    writeline;

    { Tell the assembler this is 68020 code.
    }
    if mc68020 then
      begin
      reposition(opcolumn);
      writestr('OPT');
      reposition(opndcolumn);
      writestr('P=68020');
      if mc68881 then
        begin
        writestr('/68881');
        writeline;
        reposition(opcolumn);
        writestr('FOPT');
        reposition(opndcolumn);
        writestr('ID=');
        writeint(coprocessor_id);
        end;
      writeline;
      end;

    reposition(opcolumn);
    writestr('SECTION');
    writestr(sectiontail);
    reposition(opndcolumn);
    writeint(sectionno[codesect]);
    writeline;

    CopySFile; { emit string and structured constants }
    highcode := currentpc; { initialize code address }
    lastobjpc := currentpc; { required if dumping object code }
  end {InitMac} ;
{>>>}
{<<<}
procedure InitObj;

  var
    i: integer;
    j: 0..proctablesize;
    data: unsigned;    { assembles string constant bytes into words }
    count: integer;    { number of stringfile bytes }


  begin {initObj}

{ Write the string constants and structured constants to the temporary
  object file.  Every 32 words of such data are preceeded by 2 words of
  zeroes, which signals the final object file that the data does not
  require relocation.
}
    nexttempbuffer := 0;
    nexttemprelocn := 0;
    nextrelfile := - 1;
    nextobjfile := - 1;
    nextobjblk := 0;
    tempfilesize := 0; { number of bytes written to temp file }

    if scanalys then count := stringfilecount + ord(odd(stringfilecount))
    else count := stringfilecount;

    if needcaching then
      seekstringfile(0)
    else
      begin
      curstringblock := 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := 0;
      end;

    for i := 1 to count div 2 do
      begin
      data := getstringfile * 256;
      data := data + getstringfile;
      putbuffer(data, false);
      end;

    currentpc := count;
    if odd(currentpc) then
      currentpc := currentpc + 1; {should not be necessary!}

    {now move on to the constant table and write it}

    if needcaching then
      seekstringfile(stringtablelimit)
    else
      begin
      curstringblock := stringtablelimit div (diskbufsize + 1) + 1;
      stringblkptr := stringblkptrtbl[curstringblock];
      nextstringfile := stringtablelimit mod (diskbufsize + 1);
      end;

    i := consttablelimit - stringtablelimit;

    if i > 0 then currentpc := currentpc + i;

    if odd(currentpc) then currentpc := currentpc + 1;

    while i > 0 do
      begin
      data := getstringfile * 256;
      i := i - 1;

      if i > 0 then
        begin { don't grab a non-existent odd byte }
        data := data + getstringfile;
        i := i - 1;
        end;

      putbuffer(data, false);
      end;

    if odd(currentpc) then currentpc := currentpc + 1;

    lastobjpc := currentpc;

    highcode := currentpc; { highest address so far }
    sectionpc[codesect] := currentpc;
  end {InitObj} ;
{>>>}
{<<<}
procedure FixMac;

{ Clean up the macro file.  There isn't much to do for this file.
}

  var
    i: ESDrange;
    j: integer;
    k: 1..linknamesize;
    suppcall: libroutines;
    support: packed array [libroutines] of boolean;
    s: namestring;
    linkname: linknametype;
    running_offset: integer;
    vtptr: vartablerecptr;

  {<<<}
  procedure do_setuse;

  { Write SET/USE lines to the macro file for Modula2.
  }

    var
      procno : proctableindex;
      linkname: linknametype;
      fixupblock, fixupbyte: integer;

    procedure writetimestamp(start: integer);

    { Convert a four byte binary time stamp to 8 hex digits.
    }

      begin {writetimestamp}
        if language = modula2 then
          begin
          linkname := get_from_sfile(start, 4, true);

          write(Macfile, ord(linkname[1]):-2, ord(linkname[2]):-2,
                ord(linkname[3]):-2, ord(linkname[4]):-2)
          end;
      end; {writetimestamp}


    begin {do_setuse}
      if language = modula2 then
        begin
        if proctable[1].calllinkage = implementationbody then
          writeprocname(1, linknameused)
        else { main body }
          writestr('p_main');

        writech(':');
        reposition(opcolumn);
        writestr('SETKEY');
        reposition(opndcolumn);
        writech('''');
        writetimestamp(proctable[1].charindex + proctable[1].charlen);
        writech('''');
        writeline;

        for procno := 1 to proctabletop do
          begin
          if (proctable[procno].calllinkage = definitionbody) and
             (proctable[procno].level = 1) {directly imported} then
            begin
            writeprocname(procno, linknameused);
            writech(':');
            reposition(opcolumn);
            writestr('USEKEY');
            reposition(opndcolumn);
            writech('''');
            writetimestamp(proctable[procno].charindex +
                           proctable[procno].charlen);
            writech('''');
            writeline;
            end;
          end;
        end;
    end; {do_setuse}
  {>>>}

  begin {fixmac}
    writech('*');
    writeline;

    for suppcall := first_call to last_call do support[suppcall] := false;

    for i := firstESD to nextESD - 1 do
      begin
      with ESDtable[i] do
        case esdkind of
          esdsupport:
            support[suppno] := true;

          esdentry, esdexternal:
            begin
            reposition(opcolumn);
            if esdkind = esdentry then
              writestr('XDEF')
            else writestr('XREF');
            reposition(opndcolumn);
            writeprocname(exproc, linknameused);
            writeline;
            end;

          esdglobal:
            begin
            reposition(opcolumn);
            writestr('XDEF');
            reposition(opndcolumn);
            writestr('GLOBAL$$');
            writeline;
            writestr('GLOBAL$$');
            reposition(opcolumn);
            writestr('EQU');
            reposition(opndcolumn);
            writeint(glbsize);
            writeline;
            end;

          esddiag, esdload, esdcommon: { ignore this one };
          end; {case}
        end;

    for suppcall := first_call to last_call do
      if support[suppcall] then
        begin
        reposition(opcolumn);
        writestr('XREF');
        if not switcheverplus[longlib] then writestr('.S');
        reposition(opndcolumn);
        supname(suppcall, s);
        WriteSymbolName(s);
        writeline;
        end;

    if ownsize > 0 then
      begin
      if ownsect_string > 0 then
        begin
        linkname := get_from_sfile(ownsect_string, ownsect_strlength, true);
        WriteSymbolName(linkname);
        end
      else if proctable[0].charindex = 0 then writesymbolname(outputname)
      else writeprocname(0, linknameused); {use program name}
      reposition(opcolumn);
      writestr('SECTION');
      reposition(opndcolumn);
      writeint(datasection);
      writeline;
      writech('G');
      reposition(opcolumn);
      writestr('EQU');
      reposition(opndcolumn);
      writech('*');
      writeline;
      reposition(opcolumn);
      writestr('DS.B');
      reposition(opndcolumn);
      writech('$');
      write(MacFile, ownsize: -4);
      writeline;
      end;


    { Dump out all the "use" and "shared" variables that have the referenced
      flag set and dump all of the "define" variables.
    }
    if lastvartableentry > 0 then
      begin
      writech('*');
      writeline;

      if definesize > 0 then
        begin
        sectionpc[datasect] := definesize;
        sectionno[datasect] := datasection;
        newsection(datasect);
        end;

      running_offset := 0;

      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          begin
          if referenced and (extvaralloc = usealloc) then
            begin
            reposition(opcolumn);
            writestr('XREF');
            reposition(opndcolumn);
            linkname := get_from_sfile(charindex, charlen, not aliased);
            WriteSymbolName(linkname);
            writeline;
            end
          else if extvaralloc = definealloc then
            begin
            { Adjust for alignment.
            }
            if running_offset < offset then
              begin
              reposition(opcolumn);
              writestr('DS.B');
              reposition(opndcolumn);
              writech('$');
              write(MacFile, offset - running_offset: -4);
              writeline;
              running_offset := offset + size;
              end
            else running_offset := running_offset + size;

            reposition(opcolumn);
            writestr('XDEF');
            reposition(opndcolumn);
            linkname := get_from_sfile(charindex, charlen, not aliased);
            WriteSymbolName(linkname);
            writeline;
            WriteSymbolName(linkname);
            writech(':');
            reposition(opcolumn);
            writestr('DS.B');
            reposition(opndcolumn);
            writech('$');
            write(MacFile, size: -4);
            writeline;
            end
          else if referenced then { shared_var }
            begin
            linkname := get_from_sfile(charindex, charlen, true);
            WriteSymbolName(linkname);
            reposition(opcolumn);
            writestr('SECTION');
            reposition(opndcolumn);
            writeint(datasection);
            writeline;

            WriteSymbolName(linkname);
            writech(':');
            reposition(opcolumn);
            writestr('DS.B');
            reposition(opndcolumn);
            writech('$');
            write(MacFile, size + ord(odd(size)): -4);
            writeline;
            end;
          end; {with}
        end; {for}

      { Align the defined variables.
      }
      if odd(definesize) then
        begin
        reposition(opcolumn);
        writestr('DS.B');
        reposition(opndcolumn);
        writech('$');
        write(MacFile, 1: 1);
        writeline;
        end;
      end; {lastvartableentry > 0}

    if language = modula2 then do_setuse;

    if testing and (fixuphead <> nil) then
      begin
      fixp := fixuphead;
      writech('*');
      writeline;
      while fixp <> nil do
        with fixp^ do
          begin
          if fixupkind <> fixupESDid then
            writeln(MacFile, '* Fixup location ', fixupobjpc: -4,
                    ' with value ', fixupaddr: -4);
          fixp := fixuplink; { get next pointer }
          end; { with }
      end {if testing};

    writech('*');
    writeline;

    if totalputerr > 0 then
      begin
      writeln(MacFile, stars, totalputerr:1, checkmsg);
      write(totalputerr: 1, checkmsg);
      abort(inconsistent);
      end

    else
      begin
      writeln(MacFile, '*  [', sectionpc[codesect]: -4,
              ']  ', sectionpc[codesect]:1, ' code bytes generated');

      if everdiagnosing then
        begin
        writeln(MacFile, '*  [', sectionpc[diagsect]: -4, ']  ',
                sectionpc[diagsect]:1, ' diagnostic bytes generated');
        end;
      writech('*');
      writeline;

      { Oasys assembler likes to know where begin$ is }
      if startaddress <> undefinedaddr then
        begin
        reposition(opcolumn);
        writestr('XDEF');
        reposition(opndcolumn);
        writestr('BEGIN$');
        writeline;
        end;

      reposition(opcolumn);
      writestr('END');
      if startaddress <> undefinedaddr then
        begin
        reposition(opndcolumn);
        writestr('BEGIN$');
        end;
      writeline;
      end;
  end; {FixMac}
{>>>}
{<<<}
procedure FixDefines;

  { Put out an XDEF to the object file for each non-referenced "define"
    variable.
  }

  var
    vtptr: vartablerecptr;
    j: integer;

  begin {fixdefines}
    if lastvartableentry > 0 then
      begin
      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          if not referenced and (extvaralloc = definealloc) then
            begin
            newESD.ESDkind := ESDdefine;
            newESD.vartabindex := j;
            findESDid;
            end; { not referenced and (extvaralloc = definealloc) }
        end; { for }
      end; { lastvartableentry > 0 }
  end; {fixdefines}
{>>>}
{<<<}
procedure FixObj;

  var
    procid: proctableindex;
    loc: integer;
    j: integer;
    vtptr: vartablerecptr;

  {<<<}
  procedure putbyte(data: bytesize);


    begin {putbyte}
      if nextobjfile = 255 then
        begin
        put(objfile); { good riddence }
        nextobjfile := 0; { reset fill index }
        nextobjblk := nextobjblk + 1;
        end
      else nextobjfile := nextobjfile + 1;

      objfile^[nextobjfile] := data;
      objbytecount := objbytecount + 1;
    end; {putbyte}
  {>>>}
  {<<<}
  procedure putlong(data: unsigned);

    begin {putlong}
      putbyte((data and 16#FF000000) div 16#1000000); { high order first }
      putbyte((data and 16#FF0000) div 16#10000);
      putbyte((data and 16#FF00) div 16#100);
      putbyte(data and 16#FF); { down to low order }
    end; {putlong}
  {>>>}
  {<<<}
  procedure putword(data: unsigned);

    begin {putword}
      putbyte(data div 256); { high order first }
      putbyte(data mod 256); { then low order }
    end; {putword}
  {>>>}
  {<<<}
  procedure putname(linkname: packed array [l..h: shortint] of char);

  { Write out the given string to the object file.  If we are writing a
    new format object, a null byte is added to the end.
  }

    var i: shortint;


    begin {putname}
      if newobjectfmt then
        begin
        i := 1;
        while (i <= h) and (linkname[i] <> ' ') do
          begin
          if language = pascal then putbyte(ord(uppercase(linkname[i])))
          else putbyte(ord(linkname[i]));
          i := i + 1;
          end;
        putbyte(0);
        end
      else
        begin
        for i := 1 to h do
          if i <= linknameused then putbyte(ord(uppercase(linkname[i])));
        for i := linknameused + 1 to linknamesize do
          putbyte(ord(' '));
        end;
    end; {putname}
  {>>>}
  {<<<}
  procedure putdateandtime;

  { Write the current date and time to the object file.
  }
    type
      datimtype = (hour, min, sec, month, day, year);

    var
      bcdbuf: array [datimtype] of integer;
      i: datimtype;


    begin {putdateandtime}
      timestamp(bcdbuf[day], bcdbuf[month], bcdbuf[year], bcdbuf[hour],
                bcdbuf[min], bcdbuf[sec]);
      bcdbuf[year] := bcdbuf[year] mod 100;
      for i := hour to year do
        begin
        bcdbuf[i] := (bcdbuf[i] div 10) * 16 + bcdbuf[i] mod 10;
        putbyte(bcdbuf[i]);
        end;
    end {putdateandtime} ;
  {>>>}
  {<<<}
  procedure fixuplength(block, byte, count: integer);

    var
      old_nextobjfile: integer;
      old_nextobjblk: integer;

    begin {fixuplength}
      if block <> nextobjblk then
        begin
        put(objfile);
        seek(objfile, block + 1);
        end;

      old_nextobjfile := nextobjfile;
      old_nextobjblk := nextobjblk;
      nextobjfile := byte;
      putlong(count);

      if block <> nextobjblk then
        begin
        put(objfile);
        nextobjblk := old_nextobjblk;
        seek(objfile, nextobjblk + 1);
        end;

      nextobjfile := old_nextobjfile;
    end; {fixuplength}
  {>>>}
  {<<<}
  procedure writeidentrecord;

  { Write a old type 1 ident record or a new type 5 ident record to the object
    file.
  }
    const
      magiclength = 44; { number of bytes generated below }

    var
      i: 1..magiclength; { induction var for filling record }
      fixupblock, fixupbyte: integer;


    begin {writeidentrecord}
      if newobjectfmt then
        begin
        putbyte(1);
        objbytecount := 0;
        putbyte(ord('5')); { type 5 record }
        fixupblock := nextobjblk;
        fixupbyte := nextobjfile;
        putlong(0);
        putname(outputname);
        putbyte(0); { version --  NYI }
        case language of
          pascal: putbyte(ord('P'));
          modula2: putbyte(ord('M'));
          c: putbyte(ord('?')); { ??? I don't know what this is going to be }
          end;
        putbyte(0); { filename -- NYI }
        end
      else
        begin
        putbyte(magiclength + ident_strlength);
        putbyte(ord('1'));
        putname(outputname);
        putbyte(objversion);
        putbyte(objrevision);
        putbyte(ord('P')); { Language = Pascal }
        for i := 1 to 4 do { Volume Name }
          putbyte(ord(' '));
        putword(0); { User Number }
        for i := 1 to 18 do { Cat, Fname, Ex }
          putbyte(ord(' '));
        end;

      putdateandtime;

      if ident_strlength > 0 then
        begin
        loc := stringfilecount + ident_string - 1;

        if needcaching then
          seekstringfile(loc)
        else
          begin
          curstringblock := loc div (diskbufsize + 1) + 1;
          stringblkptr := stringblkptrtbl[curstringblock];
          nextstringfile := loc mod (diskbufsize + 1);
          end;

        for i := 1 to ident_strlength do
          putbyte(getstringfile);
        end;
      if newobjectfmt then
        begin
        putbyte(0);
        fixuplength(fixupblock, fixupbyte, objbytecount);
        end;
    end; {writeidentrecord}
  {>>>}
  {<<<}
  procedure write_setuse_records;

  { Write a SET/USE record to the object file for Modula2.
  }
    var
      procno : proctableindex;
      linkname: linknametype;
      fixupblock, fixupbyte: integer;

    procedure writetimestamp(start: integer);

    { Convert a four byte binary time stamp to 8 hex digits.
    }

      type
        cvtarray = packed array [0..15] of char;

      const
        cvt = cvtarray('0', '1', '2', '3', '4', '5', '6', '7',
                       '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

      var
        i :integer;

      begin {writetimestamp}
        if language = modula2 then
          begin
          linkname := get_from_sfile(start, 4, true);

          for i := 1 to 4 do
            begin
            putbyte(ord(cvt[ord(linkname[i]) div 16]));
            putbyte(ord(cvt[ord(linkname[i]) mod 16]));
            end;

          putbyte(0);
          end;
      end; {writetimestamp}


    begin {write_setuse_records}
      if language = modula2 then
        begin
        putbyte(1); { length of ESD record always 1 }
        objbytecount := 0;
        putbyte(ord('7')); { type 7 record }
        fixupblock := nextobjblk;
        fixupbyte := nextobjfile;
        putlong(0);
        putbyte(1); { set }

        if proctable[1].calllinkage = implementationbody then
          begin
          linkname := get_from_sfile(proctable[1].charindex,
                                     proctable[1].charlen, true);

          putname(linkname); { send name to object file }
          end
        else { main body }
          putname('p_main');

        writetimestamp(proctable[1].charindex + proctable[1].charlen);

        for procno := 1 to proctabletop do
          begin
          if (proctable[procno].calllinkage = definitionbody) and
             (proctable[procno].level = 1) {directly imported} then
            begin
            putbyte(0); { use }
            linkname := get_from_sfile(proctable[procno].charindex,
                                       proctable[procno].charlen, true);
            putname(linkname); { send name to object file }
            writetimestamp(proctable[procno].charindex +
                           proctable[procno].charlen);
            end;
          end;

        fixuplength(fixupblock, fixupbyte, objbytecount);
        end;
    end; {write_setuse_records}
  {>>>}
  {<<<}
  procedure writeESDrecord;

  { Write an only type 2 ESD record or a new type 6 ESD record to the object
    file.
  }
    var
      s: namestring;
      linkname: linknametype;
      i: 1..linknamesize;
      loc: integer; { location in string file }
      temp: integer; { used to convert support number to decimal }
      ESDlo,
      ESDhi,
      ESD: ESDrange; { induction vars for scanning ESDtable }
      ESDcounter: bytesize; { tracks length of ESDrecord }
      count: 0..15; { records length of one table entry }
      vtptr: vartablerecptr;
      fixupblock, fixupbyte: integer;
      writeheader: boolean;

    begin {writeESDrecord}
      writeheader := true;
      ESDlo := firstESD;
      objbytecount := 0;

      while ESDlo < nextESD do
        begin
        ESDhi := ESDlo;
        ESDcounter := 1; { for the identification byte }

        while (ESDcounter <= 255) and (ESDhi < nextESD) do
          begin

          with ESDtable[ESDhi] do { compute length of next entry }
            case ESDkind of
              ESDuse,
              ESDsupport,
              ESDexternal: count := 11;
              ESDglobal,
              ESDcommon,
              ESDdiag,
              ESDdefine,
              ESDshrvar,
              ESDentry: count := 15;
              ESDload: count :=  5;
              end; { case newESD.kind }
          ESDhi := ESDhi + 1;
          ESDcounter := ESDcounter + count;
          end; { while }

        if ESDcounter > 255 then
          begin
          ESDhi := ESDhi - 1; { because we're 2 beyond }
          ESDcounter := ESDcounter - count;
          end;

        if not newobjectfmt then
          begin
          { now that the length of the record is known in advance ... }
          putbyte(ESDcounter); { length of ESD record }
          putbyte(ord('2')); { type 2 record }
          end
        else if writeheader then { newobjectfmt }
          begin
          writeheader := false;
          putbyte(1); { length of ESD record always 1 }
          objbytecount := 0;
          putbyte(ord('6')); { type 6 record }
          fixupblock := nextobjblk;
          fixupbyte := nextobjfile;
          putlong(0);
          end;

        for ESD := ESDlo to ESDhi - 1 do
          with ESDtable[ESD] do
            case ESDkind of

              ESDsupport: { construct support routine name }
                begin
                putbyte(160B);
                supname(suppno, s);
                putname(s);
                end; { ESD support name }

              ESDexternal, { pull name from StringFile }
              ESDentry,
              ESDcommon: { pull name from StringFile, append offset }
                begin
                procid := exproc;
                if ESDkind = ESDentry then
                  putbyte(100B + sectionno[codesect])
                else if ESDkind = ESDcommon then
                  begin
                  procid := 0;
                  putbyte(20B + datasection);
                  end
                else { ESDexternal }
                  putbyte(160B); { to any section }

                if (ESDkind = ESDcommon) and (ownsect_string > 0) then
                  linkname := get_from_sfile(ownsect_string, ownsect_strlength,
                                             true)
                else if proctable[procid].charindex = 0 then
                  linkname := outputname
                else linkname := get_from_sfile(proctable[procid].charindex,
                                 proctable[procid].charlen, true);

                putname(linkname); { send name to object file }

                if ESDkind = ESDentry then
                  begin
                  putlong(procmap[procid].addr);
                  end
                else if ESDkind = ESDcommon then
                  begin
                  putlong(glbsize);
                  end;
                end; { ESD entry and external }

              ESDglobal: { global name and address }
                begin
                putbyte(120B); { XDEF in absolute section }
                putname('GLOBAL$$       ');
                putlong(glbsize); { offset may be > 64K }
                end;

              ESDbegin: { BEGIN$ and startaddress }
                begin
                  { XDEF in relocatable code section }
                putbyte(100B + sectionno[codesect]);
                putname('BEGIN$         ');
                putlong(startaddress); { but offset will be < 64K }
                end;

              ESDdiag: {diagnostic code common reference}
                begin
                putbyte(20B+sectionno[diagsect]); {common section}
                putname('P_DIAG         '); {named common}
                putlong(0); {just a nominal reference}
                end;

              ESDload: { standard load section and length of load module }
                begin
                if (sect = codesect) and shortsection then
                  putbyte(60B + sectionno[sect])
                else putbyte(40B + sectionno[sect]);
                putlong(sectionpc[sect]); { this is the next free word }
                end;

              ESDuse:
                begin
                vtptr := getvartableptr(vartabindex);

                with vtptr^ do
                  begin
                  linkname := get_from_sfile(charindex, charlen, not aliased);
                  putbyte(160B); { to any section }
                  putname(linkname); { send name to object file }
                  end; { with }
                end;

              ESDdefine:
                begin
                vtptr := getvartableptr(vartabindex);

                with vtptr^ do
                  begin
                  linkname := get_from_sfile(charindex, charlen, not aliased);
                  putbyte(100B + datasection);
                  putname(linkname); { send name to object file }
                  putlong(offset);
                  end; { with }
                end;

              ESDshrvar:
                begin
                vtptr := getvartableptr(vartabindex);

                with vtptr^ do
                  begin
                  linkname := get_from_sfile(charindex, charlen, true);
                  putbyte(20B + datasection);
                  putname(linkname); { send name to object file }
                  putlong(size + ord(odd(size)));
                  end; { with }
                end;
              end; { case ESDtable[i] }

        ESDlo := ESDhi; { prepare for the next record, if any }
        end; { while ESDlo < nextESD }

      if newobjectfmt then fixuplength(fixupblock, fixupbyte, objbytecount);
    end; {writeESDrecord}
  {>>>}
  {<<<}
    procedure writeDataRecords;

  { Note that this contains a gross kluge to put the esd record for
    the common section at the end.  Any esdid of 0 is assumed to be
    a reference to the common esdid, and is so treated.

    This is kluged in because Wayne wants to do it right and sort
    esdids anyway, and I didn't feel like doing it twice.
  }

      var
        data, offset_len: unsigned; { single word buffer between files }
        finalcount: addressrange; { number of words re-read from temp file }
        cursection: 0..15; {current section}
        tempbuffer: unsigned; {first word before being broken up}
        datacount: 0..32; { number of words remaining before header is req'd }
        fixuplocation,
        fixupaddress: addressrange; { pulled from fixup list }
        fixuplength: integer; { word or long fixup for procedures }
        fixupobjectpc: integer; { address of fixup }
        fixupvartabindex: integer;
        { The following is a kluge and should be replaced later }
        relcode: array [1..32] of boolean;
        pieceno: 0..32;
        i: 0..7; { loop counter for ESDID's }
        esdid_count: 0..7; { Number of ESDID's in relocation expression }
        vtptr: vartablerecptr;

      const
        debug = false;

      {<<<}
      function gettemp: unsigned;


        begin {gettemp}
          if nextrelfile = maxrelfile then
            begin
            get(relfile);
            nextrelfile := 0;
            end
          else nextrelfile := nextrelfile + 1;

          finalcount := finalcount + 1;
          gettemp := relfile^[nextrelfile];
        end; {gettemp}
      {>>>}
      {<<<}
      procedure nextfixup;


        begin {nextfixup}
          if fixuphead = nil then { no fixups to be had }
            begin
            fixupaddress := 0;
            fixuplocation := maxint;
            end
          else
            begin { consume fixupnode }
            fixp := fixuphead;
            with fixp^ do
              begin
              fixupaddress := fixupaddr;
              fixuplocation := fixupfileloc;
              fixuphead := fixuplink; { unhook this node }
              fixuplength := fixuplen;
              fixupobjectpc := fixupobjpc;
              if fixupkind = fixupesdid then fixupvartabindex := vartabindex;
              end; { with fixp^ }
            dispose(fixp); { and return it to the heap }
            end; { consume fixupnode }
        end; {nextfixup}
      {>>>}
      {<<<}
      procedure decr_datacount;
        { Decrement the word count and check for consistency.
        }
        begin {decr_datacount}
        datacount := datacount - 1;

        if datacount < 0 then
          begin
          write('WRITEDATARECORDS internal error -- bad datacount');
          abort(inconsistent);
          end;
        end; {decr_datacount}
      {>>>}
      {<<<}
      procedure do_fixup(relocated: boolean {Don't bump pieceno if true});

        { If the current location is the next fixup then handle it.  If a long
          fixup is needed, get the next word and build the longword before adding.
        }
        begin {do_fixup}
        if finalcount = fixuplocation then
          begin { apply the fixup }
          if fixuplength = long then
            begin
            data := data * 16#10000 + gettemp;
            if not relocated then pieceno := pieceno + 1;
            end;

          data := data + fixupaddress;

          if fixuplength = long then
            begin
            putword(data div 16#10000);
            data := data mod 16#10000;
            decr_datacount; { account for extra word }
            end;

          nextfixup;
          end { apply the fixup }
        else if finalcount > fixuplocation then
          begin
          write('DO_FIXUP internal error -- missed fixup at filepos=',
                fixuplocation:1, ', fixup=',fixupaddress:-1,', pc=',
                fixupobjectpc:-1);
          abort(inconsistent);
          nextfixup;  { Just in case of /test }
          end;

        putword(data);
        end; {do_fixup}
      {>>>}

      begin {writeDataRecords}
        nextfixup; { get fixup data, if any }
        datacount := 0;
        finalcount := 0;
        nextrelfile := - 1;

        while finalcount <> tempfilesize do
          begin
          if datacount = 0 then
            begin { new record header }
            tempbuffer := gettemp;
            datacount := tempbuffer mod 256;
            cursection := tempbuffer div 256;
            putbyte(datacount * 2 + 6); { size in bytes}
            putbyte(ord('3')); { type of Data Record }

            {the following is part of the main kluge}
            data := gettemp;
            putword(data); { 1st 16 relocation bits }
            for pieceno := 16 downto 1 do
              begin
              relcode[pieceno] := odd(data);
              data := data div 2;
              end;
            data := gettemp;
            putword(data); { 2nd 16 relocation bits }
            for pieceno := 32 downto 17 do
              begin
              relcode[pieceno] := odd(data);
              data := data div 2;
              end;
            pieceno := 0;
            putbyte(cursection); { in every record }
            end; { new record header }

          decr_datacount;
          data := gettemp; { fetch word from temp file }

          { The main part of the kluge}
          pieceno := pieceno + 1;
          if debug then writeln('pieceno = ', pieceno:1);

          if pieceno > 32 then
            begin
            write('WRITEDATARECORDS internal error -- bad pieceno');
            abort(inconsistent);
            end;

          if relcode[pieceno] then
            begin
            offset_len := (data and 16#700) div 256; { offset field length }

            { There are two hacks here:

              1 - There may be a fixup of the ESD record to fill in the ESDid
                  of a "shared" variable which is really a named common.  This
                  is necessary because all named commons must appear after all
                  xref's and xdef's.
              2 - If there is no fixup, then a zero ESDid in the first ESDid
                  position, which would normally be meaningless, means common
                  section relocation.
            }
            if (finalcount >= fixuplocation) then
              if finalcount > fixuplocation then
                begin
                write('WRITEDATARECORDS internal error -- missed fixup at filepos=',
                  fixuplocation:1, ', fixup=',fixupaddress:-1,', pc=',
                  fixupobjectpc:-1);
                abort(inconsistent);
                nextfixup;  { Just in case of /test }
                end
              else
                begin { insert the ESDid in data word }
                vtptr := getvartableptr(fixupvartabindex);
                putword(data + vtptr^.offset);
                nextfixup;
                end
            else if data and 255 = 0 then putword(data + commonesdid)
            else putword(data);

            esdid_count := data div (32 * 256); { shift the ESDID count down }
            if debug then writeln('esdid_count = ', esdid_count:1);

            { Account for multiple ESDID's.  They will always appear in odd
              numbers.  A zero ESDID in the last position is used to pad to
              word boundary for our self-imposed file of words.
            }
            for i := 2 to esdid_count do
              if not odd(i) then { by two's }
                begin
                decr_datacount;
                data := gettemp;
                putword(data);
                end;

            if offset_len > 0 then
              begin
              decr_datacount;
              data := gettemp;
              end;

            { If there is no fixup then process the high order word here,
              otherwise the application of the fixup below will skip over
              both words.
            }
            if (offset_len = 4) and (finalcount <> fixuplocation) then
              begin
              putword(data);
              decr_datacount;
              data := gettemp;
              end;

            if offset_len > 0 then do_fixup(true);
            end { if relcode[pieceno] }
          else
            do_fixup(false);

          end; { while }

      end; {writeDataRecords}
  {>>>}
  {<<<}
  procedure writeEndRecord;


    begin {writeEndRecord}
      if startaddress = undefinedaddr then
        begin
        putword(2 * 256 + ord('4')); { length = 2 bytes }
        putbyte(17); { indicates no start address }
        end
      else
        begin
        putword(6 * 256 + ord('4')); {length = 6 bytes}
        putbyte(sectionno[codesect]); {starts in the code}
        putlong(startaddress);
        end;
    end; {writeEndRecord}
  {>>>}

  begin {FixObj}
    newESD.ESDkind := ESDload;
    newesd.sect := codesect;
    insertnewESD;

    if ownsize > 0 then
      begin
      newesd.esdkind := esdcommon;
      newesd.glbsize := ownsize;
      findesdid;
      commonesdid := esdid;
      end;

    if definesize > 0 then
      begin
      sectionpc[datasect] := definesize + ord(odd(definesize));
      sectionno[datasect] := datasection;
      newESD.ESDkind := ESDload;
      newesd.sect := datasect;
      insertnewESD;
      end;

    { Allocate an ESDID for each "shared" variable that has been referenced.
    }
    if lastvartableentry > 0 then
      begin
      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          if referenced and (extvaralloc = sharedalloc) then
            begin
            newESD.ESDkind := ESDshrvar;
            newESD.vartabindex := j;
            findESDid;
            offset := ESDid;
            end; { referenced and (extvaralloc = sharedalloc) }
        end; { for }
      end; { lastvartableentry > 0 }

    flushtempbuffer;

    repeat
      putrelfile(0); { flush the file buffer }
    until nextrelfile = 0;

    seek(relfile, 1); { prepare to read it back in }

    writeidentrecord;
    if language = modula2 then write_setuse_records;
    writeESDrecord;
    if newobjectfmt and odd(nextobjfile) then putbyte(0);
    writeDataRecords;
    writeEndRecord;

    repeat
      putbyte(0); { flush objfilebuffer }
    until nextobjfile = 0;

    if totalputerr <> 0 then
      begin
      writeln(stars, totalputerr:1, checkmsg);
      abort(inconsistent);
      end;

  end; {FixObj}
{>>>}
{<<<}
procedure fixdiags;
{ Final code when diagnostics are generated
}

  var
    junk: integer; { throw-away result of dump_externals }
    switches: packed record case boolean of
      true: (sw: packed array [1..16] of boolean);
      false: (u: uns_word);
      end;

  begin {fixdiags}
    switches.u := 0;

    if reversebytes then
      begin
      switches.sw[16] := switchcounters[mainbody] > 0;
      switches.sw[15] := switcheverplus[own];
      switches.sw[14] := switcheverplus[doublereals];
      switches.sw[13] := switcheverplus[caseswitch];
      switches.sw[12] := switcheverplus[shortintegers];
      switches.sw[11] := mc68881;
      end
    else
      begin
      switches.sw[1] := switchcounters[mainbody] > 0;
      switches.sw[2] := switcheverplus[own];
      switches.sw[3] := switcheverplus[doublereals];
      switches.sw[4] := switcheverplus[caseswitch];
      switches.sw[5] := switcheverplus[shortintegers];
      switches.sw[6] := mc68881;
      end;

    if switcheverplus[debugging] or switcheverplus[profiling] then
      currentpc := currentpc + 2;

    if switcheverplus[outputmacro] then
      begin
      newsection(codesect);
      writeln(macfile, 'LAST', 'EQU': opcolumn - 4 + 2,
              '*': opndcolumn - opcolumn - 3 + 1);
      end;

    newsection(diagsect);
    if nextdiagbit > 0 then diag_bits(0, 16 - nextdiagbit);

    if switcheverplus[outputmacro] then
      if switcheverplus[debugging] or switcheverplus[profiling] then
        writeln(macfile, 'DC.W': opcolumn + 3, ' ': opndcolumn - opcolumn - 4,
                switches.u: 1);

    if switcheverplus[outputobj] then
      if switcheverplus[debugging] or switcheverplus[profiling] then
        putbuffer(switches.u, false);

    if switcheverplus[debugging] then junk := dump_externals;

    if switcheverplus[outputmacro] then
      writeln(macfile, 'ENDIAG', 'DC.W': opcolumn - 6 + 3,
              'STDIAG-ENDIAG': opndcolumn - opcolumn - 4 + 13);

    if switcheverplus[outputobj] then
      begin
      diaglenfix^.fixupaddr := currentpc;
      codelenfix^.fixupaddr := sectionpc[codesect];
      putbuffer(-currentpc, false);
      newesd.esdkind := ESDdiag;
      insertnewesd;
      end;

    currentpc := currentpc + 2;
    sectionpc[diagsect] := currentpc;
    newesd.esdkind := esdload;
    newesd.sect := diagsect;
    insertnewesd;
  end; {fixdiags}
{>>>}

{<<<}
const
  objreslen = objtypesx(0, {objnorm}
                        0, {objext}
                        3, {objforw}
                        0, {objsup}
                        3, {objcom}
                        0, {objoff}
                        4, {objpic}
                        0, {objign}
                        2 {objlong}
                        );
{>>>}

{<<<}
procedure put_diags;

{ Generate one procedure's worth of incore diagnostic tables.

  The tables are highly packed, with characters Huffman encoded and
  numbers stored in a compressed format.  They are bit packed with
  no regard for word boundaries.

  The syntax of the tables is:

  tables = [* lines
              [ ("diag_proc" procname) |
                ("diag_err" pcdiff, errorno ) ] *]  .

  procname = character [* character *] ' '  .
}

{ The following declarations define the translation from the upper case
  alphanumerics to a Huffman code.  The code values were determined with
  statistics from a large number of Pascal programs, and take an average
  of 4.5 bits per character.
}

  const
    hm_chars = 39;
    hm_max = 15;

  type
    hm_code = array [' '..'_'] of
        record
          length: 0..hm_max;
          value: unsigned;
        end;

  const

    hm_value = hm_code((3, 5), (0, 0), (0, 0), (0, 0), (14, 4670), (0, 0),
                       (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0),
                       (0, 0), (0, 0), (0, 0), (12, 1166), (10, 290), (10, 295),
                       (12, 1165), (13, 2329), (13, 2334), (14, 4656),
                       (15, 9315), (15, 9314), (14, 4671), (0, 0), (0, 0),
                       (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (4, 7), (6, 10),
                       (5, 8), (5, 4), (3, 7), (6, 4), (6, 11), (6, 19), (4, 12)
                       , (10, 294), (7, 5), (4, 13), (5, 19), (4, 5), (5, 0),
                       (5, 18), (9, 144), (4, 6), (4, 8), (4, 3), (6, 3),
                       (7, 37), (7, 11), (7, 4), (7, 10), (9, 146), (0, 0),
                       (0, 0), (0, 0), (0, 0), (5, 3));

  var
    i: 1..maxprocnamelen; {induction var for writing procedure names}
    codewords: addressrange; {pc in current procedure (in words)}
    thisnode: nodeindex; {induction var for scanning nodes}
    n: nodeptr; {used to access thisnode}
    firststmt: boolean; {first statement in the procedure}
    stringfilebyte: hostfilebyte;
    name_part: shortint;
    ch: 0..255;
    filenameindex: integer;


  procedure diag_number(n: integer);

{ Generate a compressed format number.  The format was determined
  empirically from data gathered over a large selection of Pascal
  programs.

  0..2          2 bits of n
  3..17         2 bits = 3, 4 bits of (n-3)
  19..48        2 bits = 3, 4 bits = 15, 5 bits of (n-18)
  otherwise     2 bits = 3, 4 bits = 15, 5 bits = 31, 16 bits of n

  cute, huh?
}


    begin
      if (n >= 0) and (n < 3) then diag_bits(n, 2)
      else
        begin
        diag_bits(3, 2);
        if (n >= 0) and (n < 18) then diag_bits(n - 3, 4)
        else
          begin
          diag_bits(15, 4);
          if (n >= 0) and (n < 49) then diag_bits(n - 18, 5)
          else
            begin
            diag_bits(31, 5);
            diag_bits(n, 16);
            end;
          end;
        end;
    end; {diag_number}



  procedure diag_line(line: integer; {new line reference}
                      pc: addressrange {pc at that line} );

  { Generate code to indicate that there is a new line number "line"
    beginning at "pc" in the code.  Again, this is very tight coding,
    based on actual statistics.  The default assumption in the diagnostic
    tables is that we are keeping track of lines.  Anything else is treated
    as an exception.  The syntax is:

    lines = [* [* pcinc *]
               "0"
               ( lineinc |
                 ("0"
                  <other stuff>)) *]  .

    pcinc = number  . (add one to line, number to pc)

    lineinc = number  . (add number to line)

    Thus the otherwise illegal value "0" is used to flag a change in the
    data.  Normally we have a series of pcincs with an occasional lineinc
    thrown in.
  }


    begin
      if (pc <> lastdiagpc) and (line <> lastdiagline) then
        begin { we have a change in both, note it }
        if line <> lastdiagline + 1 then
          begin {set in lineinc before the pcinc}
          diag_number(0); {flag change to lineinc}
          diag_number(line - lastdiagline - 1);
          end;
        diag_number(pc - lastdiagpc); {line := line + 1, pc = pc + pcinc}
        lastdiagpc := pc;
        lastdiagline := line;
        end;
    end; {diag_line}


  begin {put_diags}
    if not everdiagnosing then initdiags;

    { first generate the procedure name }
    diag_number(0); {end of lineincs}
    diag_number(0); {end of lines, on to something else}
    case language of
      modula2:
        if proctable[blockref].calllinkage = modulebody then
          diag_number(diag_mod_m2)
          else diag_number(diag_proc_m2);
      pascal:
        if level > 1 then diag_number(diag_proc_p2) {say this is a procedure}
        else diag_number(diag_prog_p2);
      c: diag_number(diag_proc_c);
      end;
    if proctable[blockref].charindex <> 0 then
      begin
      if needcaching then
        seekstringfile(stringfilecount + proctable[blockref].charindex - 1)
      else
        begin
        curstringblock := (stringfilecount + proctable[blockref].charindex -
                          1) div (diskbufsize + 1) + 1;
        stringblkptr := stringblkptrtbl[curstringblock];
        nextstringfile := (stringfilecount + proctable[blockref].charindex -
                          1) mod (diskbufsize + 1);
        end;
      for i := 1 to proctable[blockref].charlen do
        begin
        stringfilebyte := getstringfile;
        case language of
          modula2:
            diag_bits(stringfilebyte, 8);
          pascal:
            diag_bits(hm_value[uppercase(chr(stringfilebyte))].value,
                      hm_value[uppercase(chr(stringfilebyte))].length
                     );
          end;
        end;
      end
    else
      for i := 1 to 10 do
        if outputname[i] <> ' ' then
          case language of
            modula2:
              diag_bits(ord(outputname[i]), 8);
            pascal:
              diag_bits(hm_value[uppercase(outputname[i])].value,
                        hm_value[uppercase(outputname[i])].length);
            end;

    { mark the end with a blank }
    case language of
      modula2:
        diag_bits(ord(' '), 8);
      pascal:
        diag_bits(hm_value[' '].value, hm_value[' '].length);
      end;

    {Now scan the code, generating table entries for errors and lines}
    codewords := highcode div 2;
    thisnode := 1;
    firststmt := true;
    while thisnode < lastnode do
      begin
      if bigcompilerversion then n := ref(bignodetable[thisnode])
      else creadaccess(thisnode, n);
      with n^ do
        if kind = instnode then
          begin
          codewords := codewords + n^.computed_length div 2;
          thisnode := thisnode + oprndcount;
          end
        else if kind = labeldeltanode then codewords := codewords + 1
        else if kind = errornode then
          begin {write an error label}
          diag_number(0); {first end lines}
          diag_number(0);
          diag_number(diag_error); {say it is an error}
          diag_number(codewords - lastdiagpc); {pc increment}
          diag_number(errorno); {actual error number}
          end
        else if kind = stmtref then
          begin
          if filename <> 0 then
            begin
            diag_number(0);
            diag_number(0);
            diag_number(0);
            diag_number(0);

            filenameindex := filename;
            filenameindex := stringfilecount + filenameindex - 1; {skip strings}

            if needcaching then
              seekstringfile(filenameindex)
            else
              begin
              curstringblock := filenameindex div (diskbufsize + 1) + 1;
              stringblkptr := stringblkptrtbl[curstringblock];
              nextstringfile := filenameindex mod (diskbufsize + 1);
              end;

            { Pull the null terminated filename from the stringfile and
              write it as 8 bit ascii to the diag table.
            }
            ch := getstringfile;
            name_part := 0; {dev(0):usrno(1).cat(2).fname(3).ext(4)}
            while (ch <> 0) and (chr(ch) <> ' ') do
              begin
              if hostopsys <> unix then ch := ord(uppercase(chr(ch)));
              if hostopsys = vdos then
                begin
                if (ch = ord(':')) or (ch = ord('.')) then
                  name_part := name_part + 1
                else if name_part = 3 then diag_bits(ch, 8);
                end
              else diag_bits(ch, 8);
              ch := getstringfile;
              end;

            diag_bits(ord(' '), 8);
            diag_number(sourceline - lineoffset);
            lastdiagline := sourceline - lineoffset;
            end
          else if firststmt then firststmt := false
          else diag_line(sourceline - lineoffset, codewords);
          end;
      thisnode := thisnode + 1;
      end;
    diag_line(lastdiagline + 1, codewords);
    newsection(codesect);
  end; {put_diags}
{>>>}
{<<<}
procedure findlabelpc(labelno: integer; {label to match}
                      var forwardlab: integer {forward reference} );
 { Search the label table for a match on the label number parameter.
  Return the label's address in "labelpc".  If not found the label is
  a forward non-local goto, which must be appended to the fixup list.
}

  var
    lscan: labelindex; { induction var for scanning label table }
    found: boolean; { boolean to control loop escape }


  begin
    lscan := 1; { start with first entry in labeltable }
    found := false;
    forwardlab := 0;

    while (lscan <= nextlabel) and not found do
      if labeltable[lscan].labno = labelno then found := true
      else lscan := lscan + 1;

    if found then labelpc := labeltable[lscan].address
    else
      begin { an entry for future fixup }
      labelpc := 0; { don't use undefinedaddr here }
      forwardlab := 1;
      end;
  end; { findlabelpc }
{>>>}
{<<<}
procedure starttempgroup(length: addressrange);

{ Make sure that a group of "length" bytes will all fit in a single
  tempbuffer
}


  begin
    if nexttempbuffer > maxtempbuffer - (length + 1) div 2 then
      flushtempbuffer;
  end; {starttempgroup}
{>>>}
{<<<}
procedure insertobj(data: uns_word);


  begin { insertobj }
    objctr := objctr + 1;
    relocn[objctr] := false; { assume its not relocatable }
    fixups[objctr] := nil;
    object[objctr] := data;
    objtype[objctr] := objnorm;
    currentpc := currentpc + 2;
  end; { insertobj }
{>>>}
{<<<}
procedure puterror(err: puterrortype);


  begin
    if lineerrors < maxerrs then lineerrors := lineerrors + 1;
    pcerrortable[lineerrors] := err;
    totalputerr := totalputerr + 1;
  end; {puterror}
{>>>}
{<<<}
procedure dumperrors;

  var
    i: 0..maxerrs; { to cycle thru errortable }


  begin
    for i := 1 to lineerrors do
      begin
      write(MacFile, stars);

      case pcerrortable[i] of
        endofnodes: writeln(MacFile, 'end of nodes');
        badrelative: writeln(MacFile, 'bad relative');
        bitindexmode: writeln(MacFile, 'bit indexed mode');
        baddisplacement: writeln(MacFile, 'bad displacement');
        nolongaddrs: writeln(MacFile, 'no long addrs');
        badsupportcall: writeln(MacFile, 'bad support call');
        missingoperand: writeln(MacFile, 'missing operand');
        missinginst: writeln(MacFile, 'missing instruction');
        badoffset: writeln(MacFile, 'bad offset');
        badoperand: writeln(MacFile, 'bad operand');
        unknowninst: writeln(MacFile, 'unknown instruction');
        badoprndlength: writeln(MacFile, 'bad operand length');
        missingDreg: writeln(MacFile, 'missing D-register');
        missingAreg: writeln(MacFile, 'missing A-register');
        badopcount: writeln(MacFile, 'bad operand count');
        badsource: writeln(MacFile, 'bad source operand');
        baddestination: writeln(MacFile, 'bad destination operand');
        nomodeoprnd: writeln(MacFile, '"no mode" operand');
        negativesp: writeln(MacFile, 'negative offset to SP');
        nolabelnode: writeln(MacFile, 'missing label node');
        badpcindexed: writeln(MacFile, 'pc indexed mode disallowed');
        badsize: writeln(MacFile, 'bad data size');
        end; {case err}
      end; {for loop}
  end; {dumperrors}
{>>>}

{<<<}
procedure getnextnode;

{ localizes calls to "creadaccess", which accepts a nodeindex
  request and returns a node pointer result.
}


  begin
    if currnode >= lastnode then puterror(endofnodes)
    else
      begin
      currnode := currnode + 1;
      if bigcompilerversion then n := ref(bignodetable[currnode])
      else creadaccess(currnode, n);
      end
  end; {getnextnode}
{>>>}
{<<<}
procedure lookahead(n: integer);

{ Return with "p" pointing "n" nodes away.  Similar to "getnextnode".
}


  begin {lookahead}
    if currnode + n > lastnode then puterror(endofnodes)
    else if bigcompilerversion then p := ref(bignodetable[currnode + n])
    else creadaccess(currnode + n, p)
  end; {lookahead}
{>>>}
{<<<}
procedure getoperand;

 { get another node and give error if not an operand }


  begin
    getnextnode;
    if n^.kind <> oprndnode then
      begin
      puterror(missingoperand);
      currinst := nop; { to facilitate recovery }
      end { if }
  end; { getoperand }
{>>>}
{<<<}
procedure getprevoperand(num: integer);

   { Fetch the node NUM nodes back and give error if not an operand.  This is
     required for some 68020 two word instructions in which the effective
     address descriptors must follow the second instruction word, but
     the information is passed from genblk in the first operand.

     There is no need to move forward again after this call because
     "getoperand" will increment and use "currinst" which this procedure
     leaves unchanged.

     Returns with the global "n" pointing to the node.
   }


  begin { getprevoperand }
    if bigcompilerversion then n := ref(bignodetable[currnode - num])
    else creadaccess(currnode - num, n);

    if n^.kind <> oprndnode then
      begin
      puterror(missingoperand);
      currinst := nop; { to facilitate recovery }
      end { if }
  end; { getprevoperand }
{>>>}
{<<<}
procedure writemaprecord(typ: maprecordtype;
                         flags, lineno, info: integer);
 { write record to map file }


  begin
    stmtfile^.pc := currentpc;
    stmtfile^.typ := typ;
    stmtfile^.exit := (flags and 8) <> 0;
    stmtfile^.profile := (switchcounters[profiling] > 0) and ((typ = plabrec) or
                         ((typ = stmntrec) and (lineno <> 0)));
    stmtfile^.lineno := lineno;
    case typ of
      plabrec: stmtfile^.recordnr := info;
      stmntrec: stmtfile^.proclinenr := info;
      end;
    stmtfile^.filepos1 := lineno;
    put(stmtfile);
    lastmaprecord := lastmaprecord + 1;
  end;
{>>>}
{<<<}
procedure writeobjline;

  var
    i: 1..maxwords;

{ When called, one complete instruction has been converted to binary in the
  word array "object."  A parallel array of booleans, "relocn", contains the
  relocation status of each word (true = relocate).  Writeobjline transfers
  this data (word and boolean) to putbuffer, which fills corresponding
  32 element arrays.  When those buffers are filled, they in turn are trans-
  ferred to the tempobj file buffer for an eventual put to relfile.
}


  begin
    if switcheverplus[outputobj] then
      for i := 1 to objctr do
        begin
        if (objtype[i] = objoff) or (objtype[i] = objign) then
          begin
          if fixups[i] <> nil then
            fixups[i]^.fixupfileloc := tempfilesize + nexttempbuffer + 4;
          putdata(object[i]);
          end
        else
          begin
          if objtype[i] = objlong then starttempgroup(4)
          else if objtype[i] = objcom then starttempgroup(6)
          else if objtype[i] = objforw then starttempgroup(6)
          else if objtype[i] = objpic then starttempgroup(8);

          if fixups[i] <> nil then
            fixups[i]^.fixupfileloc := tempfilesize + nexttempbuffer + 4;

          putbuffer(object[i], relocn[i]);
          end;
        end;

    if testing and not skip_macro_details then
      begin
      if column >= nodecolumn then
        begin
        writeline;
        writech('*');
        end;

      reposition(nodecolumn);

   { "column" has served its purpose and is ignored until the next "writeline" }

      { pc when opcode was scanned }
      write(MacFile, '(', instindex: 3, ')   ', lastobjpc: - 4, '  ');
      WriteHex(object[1]); { write opcode in hexadecimal }

      for i := 2 to objctr do
        begin
        if not (objtype[i] in [objcom, objforw, objpic, objign]) then
          writech(' ');
        case objtype[i] of
          objnorm, objoff, objlong: WriteHex(object[i]);
          objext: write(MacFile, 'xxxx xxxx');
          objforw: ;
          objsup:
            begin
            write(MacFile, 'ssss');
            if switcheverplus[longlib] then write(MacFile, ' ssss');
            end;
          objcom: ;
          end;
        end;
      end; { testing dump }

    lastobjpc := currentpc;

    if switcheverplus[outputmacro] then
      begin
      writeline;
      dumperrors; { if any }
      end;
  end; { writeobjline }
{>>>}

{<<<}
function dump_externals;

    { Put out the debugger entries for externals.
    }

  var
    vtptr: vartablerecptr;
    j: integer;
    temp: unsigned;
    ctr: integer;


  begin {dump_externals}
    skip_macro_details := true;
    objctr := 0;
    ctr := 0;

    if lastvartableentry > 0 then
      for j := 1 to lastvartableentry do
        begin
        vtptr := getvartableptr(j);
        with vtptr^ do
          ctr := ctr + long;
        end;

    temp := ctr;
    insertobj(temp div 16#10000); { high order }
    insertobj(temp mod 16#10000); { low order }

    if switcheverplus[outputmacro] then
      begin
      reposition(opcolumn);
      writestr('DC.L');
      reposition(opndcolumn);
      writeint(ctr);
      end;

    writeobjline;

    if lastvartableentry > 0 then
      begin

      for j := 1 to lastvartableentry do
        begin
        objctr := 0;
        vtptr := getvartableptr(j);
        with vtptr^ do
          case extvaralloc of
            definealloc:
              begin
              newESD.ESDkind := ESDdefine;
              newESD.vartabindex := j;
              findESDid;

              insertobj(54B * 256 + datasection + 1);
              relocn[objctr] := true; { tag the word relocatable }
              objtype[objctr] := objforw;
              currentpc := currentpc - 2; { apply the PC correction }
              temp := vtptr^.offset;
              insertobj(temp div 16#10000); { high order offset in psect }
              objtype[objctr] := objoff;
              insertobj(temp mod 16#10000); { low order offset in psect }
              objtype[objctr] := objoff;
              end;

            sharedalloc:
              if referenced then
                begin
                { Named common esd's must go out after all xref's and xdef's, so
                  we must patch in the ESDID in writedatarecords.
                }
                insertobj(50B * 256);
                relocn[objctr] := true; { tag the word relocatable }
                currentpc := currentpc + 2; { apply the PC correction }

                allocfixup;
                fixups[objctr] := fixuptail;
                with fixuptail^ do
                  begin
                  fixupkind := fixupesdid;
                  fixupobjpc := fixupobjpc - 4;
                  vartabindex := j;
                  end;
                end
              else
                begin
                insertobj(0);
                insertobj(0);
                end;

            usealloc:
              if referenced then
                begin
                newESD.ESDkind := ESDuse;
                newESD.vartabindex := j;
                findESDid;

                insertobj(50B * 256 + ESDid);
                relocn[objctr] := true; { tag the word relocatable }
                currentpc := currentpc + 2; { apply the PC correction }
                end
              else
                begin
                insertobj(0);
                insertobj(0);
                end;
            end; { case extvaralloc }

        if switcheverplus[outputmacro] then
          begin
          reposition(opcolumn);
          writestr('DC.L');
          reposition(opndcolumn);

          with vtptr^ do
            if referenced or (extvaralloc = definealloc) then
              WriteSymbolName(get_from_sfile(charindex, charlen, not aliased))
            else writech('0');
          end;

        writeobjline;
        end; { for }
      end; { lastvartableentry > 0 }

    dump_externals := ctr;  { used by unix }
  end; {dump_externals}
{>>>}
{<<<}
procedure dump_import_table;

{ Dump the import table for Modula2 to the object file and assembly listing.
}
  var
    procno : proctableindex;
    s: longname;
    temp: unsigned;

  begin {dump_import_table}
    if language = modula2 then { deadcode this stuff otherwise }
      begin
      skip_macro_details := true;
      if switcheverplus[outputmacro] then writeline;

      if switcheverplus[outputmacro] then
        begin
        reposition(opcolumn);
        writestr('SECTION');
        writestr(sectiontail);
        reposition(opndcolumn);
        writeint(sectionno[codesect]);
        writeline;
        end
      else
writeln('NYI');
      { If this is not a program module, make module-table a global symbol.
      }
      if not switcheverplus[outputmacro] then
        procmap[import_table_ref].addr := currentpc
      else if proctable[1].calllinkage = implementationbody then
        begin
        reposition(opcolumn);
        writestr('XREF');
        reposition(opndcolumn);
        import_name(1, s);
        WriteSymbolName(s);
        writeobjline;

        { Label the import list.  (Non-main body)
        }
        import_name(1, s);
        WriteSymbolName(s);
        writech(':');
        writeobjline;
        end
      else
        begin
        { Label the import list.  (main body)
        }
        writestr('p_main');
        writech(':');
        writeobjline;
        end;

      { Generate a pointer to the procedure constituting the module body.
        The name is always p1.
      }
      if switcheverplus[outputmacro] then
        begin
        reposition(opcolumn);

        writestr('DC.L');
        reposition(opndcolumn);
        WriteSymbolName('P1');
        if level = 1 then
          begin
          writech('+');
          writeint(main_import_offset);
          end;
        end
      else
        begin
writeln('NYI');
        end;

      writeobjline;

      for procno := 1 to proctabletop do
        begin
        if (proctable[procno].calllinkage = definitionbody) and
           (proctable[procno].level = 1) {directly imported} then
          if switcheverplus[outputmacro] then
            begin
            reposition(opcolumn);
            writestr('XREF');
            reposition(opndcolumn);
            import_name(procno, s);
            WriteSymbolName(s);
            writeobjline;
            reposition(opcolumn);

            writestr('DC.L');
            reposition(opndcolumn);
            import_name(procno, s);
            WriteSymbolName(s);
            writeobjline;
            end {if macro output}
          else
            begin
            newESD.ESDkind := ESDexternal;
            newESD.exproc := procno;
            findESDid;

            insertobj(50B * 256 + ESDid);
            relocn[objctr] := true; { tag the word relocatable }
            writeobjline;
            end;
          end; {for}

        { Now generate a -1 to terminate the table.
        }
        if switcheverplus[outputmacro] then
          begin
          reposition(opcolumn);

          writestr('DC.L');
          reposition(opndcolumn);
          writech('$');
          writehexlong(16#ffffffff);
          end
        else
          begin
          objctr := 0;
          insertobj(16#ffff);
          insertobj(16#ffff);
          end;

        writeobjline;
      end; {if modula2}
  end {dump_import_table} ;
{>>>}
{<<<}
procedure setmodeonly;

{ scan the current operand node pointed to by "n" and set "mode" to
  the appropriate mode and register pair (six bits)
}

 { 68020 scale factor encoding }

  type
    scale_factor_type = array [1..8] of integer;

  const {  scale factor in bytes --> 1  2  .  4  .  .  .  8 }
    scale_factor = scale_factor_type(0, 1, 0, 2, 0, 0, 0, 3);

    word_bd_size = 2; { 68020 base displacement size for a word }
    long_bd_size = 3; { 68020 base displacement size for a long }

  var
    isforward: integer; {true if forward reference}
    vtptr: vartablerecptr;
    temp: unsigned;
    extension_size: integer;
    kluge:
      record
        case integer of
          1: (l: integer {long word} );
          2: (w: packed array [boolean] of - 32767..32767);
      end {kluge} ;


  procedure pic_gen;

      { For $pic only.

        This procedure will generate a pcrelative mode for an external
        symbol.  First the relocated external symbol is added to the
        negative of the offset in the module, then the base of the current
        section is subtracted to kill the relocation of the external symbol.
        The result is the distance to the desired object.

        The ESDID of the current section is the section number + 1.  A
        do-nothing zero ESDID is added in to align the file to a word.

        NOTE:  Variable ESDID must be setup prior to call.
      }

    var
      temp_esdid: integer;


    begin {pic_gen}
      if mc68020 then
        begin
        { Generate a "PC indirect with index (base displacement)"
          mode with the index register suppressed.
        }
        extension_size := long_bd_size;

        insertobj( {no index reg}
        {no index size}
        {no scale}
                  + 400B {bit 8}
        {BS = 0}
                  + 100B {IS = 1}
                  + (extension_size * 20B) {BD size}
                  + 000 {index/indirect selection}
        {no indirection}
                  );
        end;

      temp_esdid := ESDid;

      insertobj(16#6000 { Emit 3 ESDID's }
                + (ord(mc68020) * 16#800) {1 word reloc'ed, 2 for 020}
                + (ord(mc68020) * 16#200 + 16#200) {2 disp bytes, 4 for 020}
                + temp_esdid); { first ESDID }
      relocn[objctr] := true; { tag the word relocatable }
      objtype[objctr] := objpic; { be sure this won't span buffers }
      insertobj((sectionno[codesect] + 1) * 256);
      { ^^^ a zero ESDID is emitted to keep file word aligned }
      objtype[objctr] := objign;
      currentpc := currentpc - 4; { last two words don't go into memory }

      if mc68020 then
        begin

        { Generate a long offset.  The pc is the address of the extension word.
        }
        insertobj(( - currentpc + 2) div 16#10000);
        objtype[objctr] := objoff;
        insertobj(( - currentpc + 4) mod 16#10000);
        objtype[objctr] := objoff;
        mode := 73B; { pcrelative, long disp }
        end
      else
        begin
        insertobj( - currentpc);
        objtype[objctr] := objoff;
        mode := 72B; { pcrelative }
        end;
    end; {pic_gen}


  begin {setmodeonly}
    with n^.oprnd do
      case m of

        nomode: puterror(nomodeoprnd);

        fpreg: puterror(badoperand);

        dreg:
          begin
          mode := reg;
          end; {dreg}

        areg:
          begin
          mode := reg + 10B;
          end; {areg}

        indr:
          begin
          mode := reg + 20B;
          end; {indr}

        autoi:
          begin
          mode := reg + 30B;
          end; {autoi}

        autod:
          begin
          mode := reg + 40B;
          end; {autod}

        relative:
          begin
            { This generates the 68000 mode "address register indirect with
              displacement" which has a 16 bit displacement.  If a long word
              displacement is needed on the mc68020, we generate an "address
              register indirect with index (base displacement)" mode with the
              index register suppressed.
            }
          if (reg = 7) and (offset < 0) then puterror(negativesp);

          if (offset > 32767) or (offset < - 32768) then
            begin
            if mc68020 then
              begin
              mode := reg + 60B;

              extension_size := long_bd_size;

              insertobj( {no index reg}
              {no index size}
              {no scale}
                        + 400B {bit 8}
              {BS = 0}
                        + 100B {IS = 1}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

                { Generate long offset
                }
              kluge.l := offset;
                { the next two lines assume that "reversebytes" implies that
                  words are also reversed. }
              insertobj(kluge.w[reversebytes]);
              insertobj(kluge.w[not reversebytes]);
              end
            else puterror(baddisplacement);
            end
          else
            begin {mc68000 16-bit register indirect mode}
            mode := reg + 50B;
            insertobj(offset);
            end;
          end; {relative}

        indexed:
            { This generates the 68000 mode "address register indirect with
              index (8-bit displacement)".  If a word or long word displacement
              is needed on the mc68020, we generate an "address register
              indirect with index (base displacement)" mode.  Suppression of
              the base and index registers is currently not supported.
            }
          begin
            { NOTE:  The scale is always 1 for the 68000, but may be 1, 2, 4
              or 8 for the 68020.
            }
          mode := reg + 60B;

          if (offset > 127) or (offset < - 128) then
            begin
            if mc68020 then
              begin
              if (offset <= 32767) and (offset >= - 32768) then
                extension_size := word_bd_size
              else extension_size := long_bd_size;

              insertobj(indxr * 10000B {index reg}
                        + ord(indxlong) * 4000B {word/long word index size}
                        + (scale_factor[scale] * 1000B) + 400B {bit 8}
              {BS = 0}
              {IS = 0}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

              if extension_size = long_bd_size then {generate long offset}
                begin
                kluge.l := offset;
                  { the next two lines assume that "reversebytes" implies that
                    words are also reversed. }
                insertobj(kluge.w[reversebytes]);
                insertobj(kluge.w[not reversebytes]);
                end
              else {generate word offset} insertobj(offset and 16#FFFF);
              end
            else {not mc68020} puterror(baddisplacement);
            end
          else {mc68000 or byte displacement}
            begin
            if (offset > 127) or (offset < - 128) then
              puterror(baddisplacement);

            insertobj(indxr * 10000B + ord(indxlong) * 4000B +
                      (scale_factor[scale] * 1000B) + (offset and 16#FF));
              { The scale is always 1 for the 68000, but may be 1, 2, 4 or 8
                for the 68020.
              }
            end;
          end; {indexed}

        bitindexed:
          begin
          puterror(bitindexmode);
          end; {bitindexed}

        absshort:
          begin
          mode := 70B;
          insertobj(offset);
          end; {absshort}

        abslong:
          begin
          mode := 71B;
          kluge.l := offset;
            { the next two lines assume that "reversebytes" implies that
              words are also reversed. }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);
          end; {abslong}

        immediate:
          begin
          mode := 74B;
          if datasize <= word then insertobj(offset)
          else
            begin
            if hostintsize < long then
              begin
              if offset < 0 then insertobj(maxusword) {really -1}
              else insertobj(0);
              insertobj(offset);
              end
            else
              begin
              kluge.l := offset;
                { the next two lines assume that "reversebytes" implies that
                  words are also reversed. }
              insertobj(kluge.w[reversebytes]);
              insertobj(kluge.w[not reversebytes]);
              end;
            end;
          end; {immediate}

        immediatelong:
          begin
          mode := 74B;
          insertobj(offset1); { high order }
          insertobj(offset); { low order }
          end; {immediatelong}

        immediatequad:
          begin
          mode := 74B;

          { The lines below assume that "reversebytes" implies that
            words are also reversed.
          }
          kluge.l := offset1; { high order }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);

          kluge.l := offset; { low order }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);
          end; {immediatelong}

        immediate_extended:
          begin
          mode := 74B;

          { The lines below assume that "reversebytes" implies that
            words are also reversed.
          }
          kluge.l := offset2; { 1st longword }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);

          kluge.l := offset1; { 2nd longword }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);

          kluge.l := offset; { 3rd longword }
          insertobj(kluge.w[reversebytes]);
          insertobj(kluge.w[not reversebytes]);
          end;

        commonlong:
          begin
          mode := 71B; {absolute long}

          if commonlong_reloc < 0 then
            begin
            {first get the common section esdid}
            insertobj(54B * 256); {relocate by common section base (zero)}
            relocn[objctr] := true;
            objtype[objctr] := objcom;
            currentpc := currentpc - 2;

            { Output offset within the common block.
            }
            temp := offset; { convert to unsigned }
            insertobj(temp div 16#10000);
            objtype[objctr] := objoff;
            insertobj(temp mod 16#10000);
            objtype[objctr] := objoff;
            end;

          if commonlong_reloc > 0 then
            begin
            vtptr := getvartableptr(commonlong_reloc);

            case vtptr^.extvaralloc of
              definealloc:
                begin
                newESD.ESDkind := ESDdefine;
                newESD.vartabindex := commonlong_reloc;
                findESDid;
                insertobj(54B * 256 + datasection + 1);
                relocn[objctr] := true; { tag the word relocatable }
                objtype[objctr] := objforw;
                currentpc := currentpc - 2; { apply the PC correction }
                temp := offset + vtptr^.offset;
                insertobj(temp div 16#10000); { high order offset in psect }
                objtype[objctr] := objoff;
                insertobj(temp mod 16#10000); { low order offset in psect }
                objtype[objctr] := objoff;
                end;

              sharedalloc:
                begin
                temp := 0;
                if $pic then
                  begin

                  { Generate "#<global>-P_OWN" for a PIC reference to a
                    shared variable, i.e. named common.

                    Named common esd's must go out after all xref's and
                    xdef's, so we must patch in the ESDID in writedatarecords.
                  }
                  insertobj(154B * 256);
                  relocn[objctr] := true; { tag the word relocatable }
                  objtype[objctr] := objpic;
                  mode := 74B; { immediate }

                  allocfixup;
                  fixups[objctr] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupesdid;
                    vartabindex := commonlong_reloc;
                    end;

                  newESD.ESDkind := ESDsupport;
                  newESD.suppno := libown;
                  findESDid;

                  insertobj(ESDid * 256); { The third ESDID is a "do nothing"
                                            zero to word align the file. }
                  objtype[objctr] := objign;
                  currentpc := currentpc - 4; { apply the PC correction }
                  end
                else
                  begin
                  { Named common esd's must go out after all xref's and xdef's,
                    so we must patch in the ESDID in writedatarecords.
                  }
                  insertobj(54B * 256);
                  relocn[objctr] := true; { tag the word relocatable }
                  objtype[objctr] := objforw;
                  currentpc := currentpc - 2; { apply the PC correction }

                  allocfixup;
                  fixups[objctr] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupesdid;
                    vartabindex := commonlong_reloc;
                    end;
                  temp := offset + vtptr^.offset;
                  end;

                insertobj(temp div 16#10000); { high order offset in psect }
                objtype[objctr] := objoff;
                insertobj(temp mod 16#10000); { low order offset in psect }
                objtype[objctr] := objoff;
                end;

              usealloc:
                begin
                newESD.ESDkind := ESDuse;
                newESD.vartabindex := commonlong_reloc;
                findESDid;

                insertobj(54B * 256 + ESDid);
                relocn[objctr] := true; { tag the word relocatable }
                currentpc := currentpc - 2; { apply the PC correction }
                objtype[objctr] := objforw;
                temp := offset + vtptr^.offset;
                insertobj(temp div 16#10000); { high order offset in psect }
                objtype[objctr] := objoff;
                insertobj(temp mod 16#10000); { low order offset in psect }
                objtype[objctr] := objoff;
                end;
              end;
            end;
          end; {commonlong}

        pcrelative:
          begin
            { Pcrelative is only used to access the current section, so
              we always know exactly what the distance is.

              This generates the 68000 mode "address register indirect with
              displacement" which has a 16 bit displacement.  If a long word
              displacement is needed on the mc68020 (PIC only as it is slow),
              we generate an "PC indirect with index (base displacement)"
              mode with the index register suppressed.  If a long displacement
              is needed in nopic mode on the 68020 we use absolute.  A long
              displacement in pic mode on the 68000 causes an error.
            }

          if $pic and (n^.operandcost >= long) then
            begin
            if mc68020 then
              begin
              mode := 73B;

              extension_size := long_bd_size;

              insertobj( {no index reg}
              {no index size}
              {no scale}
                        + 400B {bit 8}
              {BS = 0}
                        + 100B {IS = 1}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

              {generate a long offset}
              kluge.l := offset - currentpc + 2; { pc = addr of extension word }
                { the next two lines assume that "reversebytes" implies that
                  words are also reversed. }
              insertobj(kluge.w[reversebytes]);
              insertobj(kluge.w[not reversebytes]);
              end
            else {not mc68020} puterror(baddisplacement);
            end
          else if n^.operandcost >= long then
            begin {far, far away}
            mode := 71B; {absolute long}
            insertobj(54B * 256 + sectionno[codesect] + 1);
            objtype[objctr] := objforw;
            relocn[objctr] := true;
            temp := offset; { convert to unsigned }
            insertobj(temp div 16#10000); {high order}
            objtype[objctr] := objoff;
            currentpc := currentpc - 2; {this stuff's longer than code}
            insertobj(temp mod 16#10000);
            objtype[objctr] := objoff;
            end {long displacement}
          else
            begin {in pcrelative range}
            mode := 72B;
            if offset - currentpc < - 32768 then puterror(baddisplacement);
            insertobj(offset - currentpc); {pc = addr of displacement}
            end;
          end; {pcrelative}

        pcindexed:
            { This generates the 68000 mode "PC indirect with index (8-bit
              displacement)".  If a word or long word displacement is needed
              on the mc68020, we generate an "PC indirect with index (base
              displacement)" mode.  Suppression of the PC and index registers
              is currently not supported.
            }
          begin
          mode := 73B;
            { NOTE:  The scale is always 1 for the 68000, but may be 1, 2, 4
              or 8 for the 68020.
            }

          if (offset > 127) or (offset < - 128) then
            begin
            if mc68020 then
              begin
              if (offset <= 32767) and (offset >= - 32768) then
                extension_size := word_bd_size
              else extension_size := long_bd_size;

              insertobj(indxr * 10000B {index reg}
              {word/long word index size (always word)}
                        + (scale_factor[scale] * 1000B) + 400B {bit 8}
              {BS = 0}
              {IS = 0}
                        + (extension_size * 20B) {BD size}
                        + 000 {index/indirect selection}
              {no indirection}
                        );

              if extension_size = long_bd_size then {generate long offset}
                begin
                kluge.l := offset;
                  { the next two lines assume that "reversebytes" implies that
                    words are also reversed. }
                insertobj(kluge.w[reversebytes]);
                insertobj(kluge.w[not reversebytes]);
                end
              else {generate word offset} insertobj(offset and 16#FFFF);
              end
            else {not mc68020} puterror(baddisplacement);
            end
          else {byte displacement}
            begin
            { note: this mode is only issued as "Dreg.W" }

            insertobj(indxr * 10000B + (scale_factor[scale] * 1000B) +
                      (offset and 16#FF));
            end;
          end; {pcindexed}

        supportcall:
          begin {treat it like an external usercall}
          if (offset < ord(first_call)) or (offset > ord(last_call)) then
            puterror(badsupportcall);

{ note: support call operands generate 2 bytes of relocation data.
  The first is always hexadecimal(20), which signifies one ESD index
  byte follows (with no offset data).  The second is the ESD index
  value, which is the index into the ESD table which has the unique
  occurrence of this support number.  Therefore, we search the table
  to see if this support routine is already present; if not, we enter
  it, and use the newly assigned location for the second byte.
}

          newESD.ESDkind := ESDsupport;
          newESD.suppno := loophole(libroutines, offset);
          findESDid;

          if $pic then pic_gen
          else
            begin
            if switcheverplus[longlib] then
              begin {treat it like an external usercall}
              insertobj(50B * 256 + ESDid);
              mode := 71B; {absolute long}
              currentpc := currentpc + 2; {correct for long address}
              end
            else
              begin
              insertobj(40B * 256 + ESDid);
              mode := 70B; {absolute short}
              end;
            relocn[objctr] := true; { tag the word relocatable }
            objtype[objctr] := objsup;
            end; {not $pic}
          end; {supportcall}

        usercall:
          begin
          if $pic then
            begin
            if proctable[offset].externallinkage and
               not proctable[offset].bodydefined then
              begin {external reference}

                { We must search the ESD table to match the procedure
                  number, thereby calculating the ESDID.
                }
              newESD.ESDkind := ESDexternal;
              newESD.exproc := offset;
              findESDid;
              pic_gen;
              end
            else
              begin { not external call }
              if procmap[offset].addr = undefinedaddr then
                begin
                if mc68020 then
                  begin
                    { Generate a "PC indirect with index (base displacement)"
                      mode with the index register suppressed.
                    }
                  extension_size := long_bd_size;

                  insertobj( {no index reg}
                  {no index size}
                  {no scale}
                            + 400B {bit 8}
                  {BS = 0}
                            + 100B {IS = 1}
                            + (extension_size * 20B) {BD size}
                            + 000 {index/indirect selection}
                  {no indirection}
                            );

                    { Generate a long offset.  The fixup will plug in the
                      pic displacement.  The pc is the address of the
                      extension word.
                    }
                  temp := offset1 - currentpc + 2; { convert to unsigned }
                  insertobj(temp div 16#10000);
                  objtype[objctr] := objlong;
                  insertobj(temp mod 16#10000);
                  mode := 73B; { pcrelative, long disp }
                  allocfixup;
                  fixups[objctr - 1] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupproc;
                    fixuplen := long;
                    fixupprocno := offset;
                    fixupobjpc := fixupobjpc - 4;
                    end;
                  end
                else { not mc68020 -- generate simple 16-bit PIC }
                  begin
                  insertobj(offset1 - currentpc);
                  mode := 72B; { pcrelative }
                  allocfixup;
                  fixups[objctr] := fixuptail;
                  with fixuptail^ do
                    begin
                    fixupkind := fixupproc;
                    fixuplen := word;
                    fixupprocno := offset;
                    fixupobjpc := fixupobjpc - 2;
                    end;
                  end; { not mc68020 }
                end { undefined addr }
              else if n^.operandcost >= long then
                begin {far, far away}

                if mc68020 then
                  begin
                    { Generate a "PC indirect with index (base displacement)"
                      mode with the index register suppressed.
                    }
                  extension_size := long_bd_size;

                  insertobj( {no index reg}
                  {no index size}
                  {no scale}
                            + 400B {bit 8}
                  {BS = 0}
                            + 100B {IS = 1}
                            + (extension_size * 20B) {BD size}
                            + 000 {index/indirect selection}
                  {no indirection}
                            );

                    { Generate a long offset.  The pc is the address of the
                      extension word.
                    }
                  insertobj((procmap[offset].addr + offset1 - currentpc + 2) div
                            16#10000);
                  insertobj((procmap[offset].addr + offset1 - currentpc + 4) mod
                            16#10000);
                  mode := 73B; { pcrelative, long disp }
                  end
                else
                  begin
                  write('Reference too long for 16-bit PIC');
                  abort(inconsistent);
                  end;
                end
              else
                begin { simple case -- 16-bit offset to known location }
                mode := 72B; {68000 PC relative}
                insertobj(procmap[offset].addr + offset1 - currentpc);
                end;
              end;
            end {if $pic}
          else
          if proctable[offset].externallinkage and
             not proctable[offset].bodydefined then
            begin {external reference}

{ note: although this operand will be 4 bytes long when eventually loaded,
  it is emitted to the relocatable object file as just 2 bytes.  The first
  is an encoded byte which says one ESD is used to resolve a 4 byte operand;
  the second is the number of that ESD.  We must search the ESD table to
  match the procedure number, thereby calculating the ESD number.  The dump
  file will show only this 2 byte operand, but the PC will increment by 4,
  and the line will be flagged "EXTERNAL".
}

            newESD.ESDkind := ESDexternal;
            newESD.exproc := offset;
            findESDid;

            insertobj(50B * 256 + ESDid);
            relocn[objctr] := true; { tag the word relocatable }
            mode := 71B; { absolute long }
            currentpc := currentpc + 2; { apply the PC correction }
            objtype[objctr] := objext;
            end

          else
            begin { not external call }
            if procmap[offset].addr = undefinedaddr then
              begin {forward procedure call}
              mode := 71B; {absolute long}
              insertobj(54B * 256 + sectionno[codesect] + 1);
              objtype[objctr] := objforw;
              relocn[objctr] := true;
              currentpc := currentpc - 2; {this stuff's longer than code}
              temp := offset1; { convert to unsigned }
              insertobj(temp div 16#10000); {high order}
              objtype[objctr] := objoff;
              insertobj(temp mod 16#10000); { fixup adds proctable addr }
              objtype[objctr] := objoff;
              allocfixup;
              fixups[objctr - 1] := fixuptail;
              with fixuptail^ do
                begin
                fixupkind := fixupproc;
                fixuplen := long;
                fixupprocno := offset;
                fixupobjpc := fixupobjpc - 4;
                end;
              end { undefined addr }

            else if n^.operandcost >= long then
              begin {long, but not forward}
              mode := 71B; {absolute long}
              insertobj(54B * 256 + sectionno[codesect] + 1);
              objtype[objctr] := objforw;
              relocn[objctr] := true;
              insertobj((procmap[offset].addr + offset1) div 16#10000);
              { ^^^ high order }
              objtype[objctr] := objoff;
              currentpc := currentpc - 2; {this stuff's longer than code}
              insertobj((procmap[offset].addr + offset1) mod 16#10000);
              objtype[objctr] := objoff;
              end
            else
              begin {normal call within 32k bytes}
              mode := 72B; {PC relative}
              insertobj(procmap[offset].addr + offset1 - currentpc);
              end;
            end;
          end; {usercall}

        pic_own_immed:
            { In PIC mode this can only occur for the code to load A3
              at the beginning of each procedure.
            }
          begin
          newESD.ESDkind := ESDsupport;
          newESD.suppno := libown;
          findESDid;

          mode := 74B; { immediate }
            { A zero in the first position is a hack for own section
              relocation. }
          insertobj(150B * 256);
          relocn[objctr] := true;
          objtype[objctr] := objlong;
          insertobj(ESDid * 256); { The third ESDID is a "do nothing" zero to
                                   word align the file. }
          objtype[objctr] := objign;
          end; {pic_own_immed}

        pic_splat_pcrel:

          { For 68000 24-bit PIC only.  Generates "<offset>+*(PC)".
          }
          begin
          mode := 72B; { pc-relative }
          insertobj(offset - 2);
          end;

        pic_usercall:

          { For 68000 24-bit PIC only.  Generates "#<name>-<offset1>-*".
          }
          begin
          if proctable[offset].externallinkage and
             not proctable[offset].bodydefined then
            begin {external reference}

              { We must search the ESD table to match the procedure
                number, thereby calculating the ESDID.
              }
            newESD.ESDkind := ESDexternal;
            newESD.exproc := offset;
            findESDid;

            mode := 74B; { immediate }
            insertobj(154B * 256 + ESDid);
            relocn[objctr] := true;
            objtype[objctr] := objpic;
            insertobj((sectionno[codesect] + 1) * 256);
                  { ^^^ The third ESDid is a "do nothing" zero to word align the
                    file. }
            objtype[objctr] := objign;
            currentpc := currentpc - 4;

            insertobj(( - (offset1 - 2) - currentpc) div 16#10000);
            objtype[objctr] := objoff;
            insertobj(( - (offset1 - 2) - currentpc + 2) mod 16#10000);
            objtype[objctr] := objoff;
            end
          else if procmap[offset].addr = undefinedaddr then
            begin { long forward reference }
            mode := 74B; { immediate }
            insertobj(( - (offset1 - 2) - currentpc) div 16#10000);
            objtype[objctr] := objlong;
            insertobj(( - (offset1 - 2) - currentpc + 2) mod 16#10000);
            allocfixup;
            fixups[objctr - 1] := fixuptail;
            with fixuptail^ do
              begin
              fixupkind := fixupproc;
              fixuplen := long;
              fixupprocno := offset;
              fixupobjpc := fixupobjpc - 4;
              end;
            end { undefined addr }
          else
            begin { simple case -- 24-bit offset to known location }
            mode := 74B; { immediate }
            insertobj((procmap[offset].addr - (offset1 - 2) - currentpc) div
                      16#10000);
            insertobj((procmap[offset].addr - (offset1 - 2) - currentpc + 2) mod
                      16#10000);
            end;
          end;

        pic_supportcall:

          { For 68000 24-bit PIC only.  Generates "#<suppt_call>-<offset1>-*".
          }
          begin
          newESD.ESDkind := ESDsupport;
          newESD.suppno := loophole(libroutines, offset);
          findESDid;

          mode := 74B; { immediate }
          insertobj(154B * 256 + ESDid);
          relocn[objctr] := true;
          objtype[objctr] := objpic;
          insertobj((sectionno[codesect] + 1) * 256);
            { ^^^ The third ESDid is a "do nothing" zero to word align the
              file. }
          objtype[objctr] := objign;
          currentpc := currentpc - 4;

          insertobj(( - (offset1 - 2) - currentpc) div 16#10000);
          objtype[objctr] := objoff;
          insertobj(( - (offset1 - 2) - currentpc + 2) mod 16#10000);
          objtype[objctr] := objoff;
          end;

        pic_branch:

          { For 68000 24-bit PIC only.  Generates "#L<labelno>-<offset1>-*"
            only for Pascal goto's.
          }
          begin
          mode := 74B; {immediate}
          op := op + mode;
          findlabelpc(offset, isforward);

          insertobj((labelpc - (offset1 - 2) - currentpc) div 16#10000);
          insertobj((labelpc - (offset1 - 2) - currentpc + 2) mod 16#10000);

          if isforward <> 0 then
            begin
            objtype[objctr - 1] := objlong;
            allocfixup; { generate a new fixupnode }
            fixups[objctr - 1] := fixuptail;
            with fixuptail^ do
              begin
              fixupkind := fixuplabel;
              fixuplen := long;
              fixuplabno := offset;
              fixupobjpc := fixupobjpc - 4;
              end;
            end;
          end;

        pic_pcrelative:

          { For 68000 24-bit PIC only.  Generates "#L+<offset>-<offset1>-*"
            only for constant section references greater than 32k bytes.
          }
          begin
          mode := 74B; {immediate}
          op := op + mode;
          insertobj((offset - (offset1 - 2) - currentpc) div 16#10000);
          insertobj((offset - (offset1 - 2) - currentpc + 2) mod 16#10000);
          end;
        end; { case m }
  end; {setmodeonly}
{>>>}
{<<<}
procedure seteffective;

{ call setmodeonly to get the current mode, then logically insert mode
  into the low order 6 bits of the current instruction word
}


  begin { seteffective }
    setmodeonly;
    op := op or mode;
  end; { seteffective }
{>>>}
{<<<}
procedure insertsize;

{ Insert size field into bits 6..7 of the main instruction word.
}


  begin
    op := (op and 177477B); {make certain the field is clear}
    if datasize = word then op := op + 100B
    else if datasize = long then op := op + 200B;
  end; {insertsize}
{>>>}
{<<<}
procedure insertreghi;

{ extract register specification from current operand node, shift left
  9 bits, and insert into the current instruction binary in bits 9..11.
}


  begin
    op := (n^.oprnd.reg and 7B) * 1000B + op;
  end; { insertreghi }
{>>>}
{<<<}
procedure insertreglo;

{ extract register specification from current operand node,
  and insert into the current instruction binary in bits 0..2.
}


  begin
    op := (n^.oprnd.reg and 7B) + op;
  end; { insertreglo }
{>>>}
{<<<}
procedure writeinst(inst: insttype);

{ Write the 68000 mnemonic for the current instruction.
  Note that the Motorola assembler accepts only upper case!
}

  var
    mnemonic: string [10];
    i: integer; {induction var for printing mnemonic}
    short_ext, long_ext: char; { Extension character to use for non-Bcc
                                instructions }
    branch_byte_ext, branch_word_ext, branch_long_ext: char; { Extension
      character to use for 68000 or 68020 Bcc insts}


  procedure postcorrect;


    begin
      if mc68020 then
        begin
        branch_byte_ext := 'B';
        branch_word_ext := 'W';
        branch_long_ext := 'L';
        end
      else
        begin
        branch_byte_ext := 'S';
        branch_word_ext := 'L';
        branch_long_ext := '?'; {error}
        end;

      short_ext := 'S';
      long_ext := 'L';

      if inst = jsr then
        begin
        lookahead(1);
        if p^.oprnd.m = supportcall then
          begin
          writech('.');
          if switcheverplus[longlib] then writech(long_ext)
          else writech(short_ext);
          end
        else if (p^.oprnd.m = usercall) and (p^.operandcost > word) then
          begin
          writech('.');
          writech(long_ext); {external, forward, or far away procedure}
          end;
        end {inst = jsr}
      else if inst = jmp then
        begin
        lookahead(1);
        if p^.kind = oprndnode then
          begin
          if p^.oprnd.m = supportcall then
            begin
            writech('.');
            if switcheverplus[longlib] then writech(long_ext)
            else writech(short_ext);
            end;
          end
        else {must be a labelnode}
          begin
          writech('.');
          writech(long_ext);
          end;
        end
    { several names were too long to fill "mnemonic",
      so a post-correction will be applied }

      else if inst = movea then writech('A')
      else if inst = movem then writech('M')
      else if inst = moveq then writech('Q');
      if inst in qualifiedinsts then
        begin
        writech('.');

        case datasize of
          byte: writech('B');
          word: writech('W');
          long: writech('L');
          otherwise puterror(badoprndlength)
          end {case oprndlength}
        end {qualifiedinsts}

      else if (inst in [fp_first..fp_last]) and not (inst in fpbranches) then
        begin
        writech('.');
        case n^.fp_format of
          single_real: writech('S');
          double_real: writech('D');
          extended_real: writech('X');
          byte_integer: writech('B');
          word_integer: writech('W');
          long_integer: writech('L');
          end;
        end
      else if (inst in branches) or (inst in fpbranches) then
        begin
        writech('.');
        lookahead(1);
        if p^.kind = relnode then
          writech(branch_byte_ext)
        else if p^.kind = labelnode then
          begin
          if p^.labelcost = 0 then writech(branch_byte_ext)
          else if p^.labelcost = word then writech(branch_word_ext)
          else if p^.labelcost = long then
            writech(branch_long_ext)
            { ^^^ 68020 and pic only }
          end
        else puterror(missingoperand);
        end;

    end {postcorrect} ;


  begin {writeinst}
    case inst of

        { 68881 instructions
        }
      fabs:
        begin
        mnemonic := 'fabs';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000030B;
        end;

      facos:
        begin
        mnemonic := 'facos';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000034B;
        end;

      fadd:
        begin
        mnemonic := 'fadd';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000042B;
        end;

      fasin:
        begin
        mnemonic := 'fasin';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000014B;
        end;

      fatan:
        begin
        mnemonic := 'fatan';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000012B;
        end;

      fatanh:
        begin
        mnemonic := 'fatanh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000015B;
        end;

      fbeq:
        begin
        mnemonic := 'fbeq';
        op := 170201B + coprocessor_id * 1000B;
        end;

      fbne:
        begin
        mnemonic := 'fbne';
        op := 170216B + coprocessor_id * 1000B;
        end;

      fbgt:
        begin
        if aware then
          begin
          op := 170202B + coprocessor_id * 1000B;
          mnemonic := 'fbogt';
          end
        else
          begin
          op := 170222B + coprocessor_id * 1000B;
          mnemonic := 'fbgt';
          end;
        end;

      fbngt:
        begin
        if aware then
          begin
          op := 170215B + coprocessor_id * 1000B;
          mnemonic := 'fbule';
          end
        else
          begin
          op := 170235B + coprocessor_id * 1000B;
          mnemonic := 'fbngt';
          end;
        end;

      fbge:
        begin
        if aware then
          begin
          op := 170203B + coprocessor_id * 1000B;
          mnemonic := 'fboge';
          end
        else
          begin
          op := 170223B + coprocessor_id * 1000B;
          mnemonic := 'fbge';
          end;
        end;

      fbnge:
        begin
        if aware then
          begin
          op := 170214B + coprocessor_id * 1000B;
          mnemonic := 'fbult';
          end
        else
          begin
          op := 170234B + coprocessor_id * 1000B;
          mnemonic := 'fbnge';
          end;
        end;

      fblt:
        begin
        if aware then
          begin
          op := 170204B + coprocessor_id * 1000B;
          mnemonic := 'fbolt';
          end
        else
          begin
          op := 170224B + coprocessor_id * 1000B;
          mnemonic := 'fblt';
          end;
        end;

      fbnlt:
        begin
        if aware then
          begin
          op := 170213B + coprocessor_id * 1000B;
          mnemonic := 'fbuge';
          end
        else
          begin
          op := 170233B + coprocessor_id * 1000B;
          mnemonic := 'fbnlt';
          end;
        end;

      fble:
        begin
        if aware then
          begin
          op := 170205B + coprocessor_id * 1000B;
          mnemonic := 'fbole';
          end
        else
          begin
          op := 170225B + coprocessor_id * 1000B;
          mnemonic := 'fble';
          end;
        end;

      fbnle:
        begin
        if aware then
          begin
          op := 170212B + coprocessor_id * 1000B;
          mnemonic := 'fbugt';
          end
        else
          begin
          op := 170232B + coprocessor_id * 1000B;
          mnemonic := 'fbnle';
          end;
        end;

      fbgl:
        begin
        if aware then
          begin
          op := 170206B + coprocessor_id * 1000B;
          mnemonic := 'fbogl';
          end
        else
          begin
          op := 170226B + coprocessor_id * 1000B;
          mnemonic := 'fbgl';
          end;
        end;

      fbngl:
        begin
        if aware then
          begin
          op := 170211B + coprocessor_id * 1000B;
          mnemonic := 'fbueq';
          end
        else
          begin
          op := 170231B + coprocessor_id * 1000B;
          mnemonic := 'fbngl';
          end;
        end;

      fbgle:
        begin
        if aware then
          begin
          op := 170207B + coprocessor_id * 1000B;
          mnemonic := 'fbor';
          end
        else
          begin
          op := 170227B + coprocessor_id * 1000B;
          mnemonic := 'fbgle';
          end;
        end;

      fbngle:
        begin
        if aware then
          begin
          op := 170210B + coprocessor_id * 1000B;
          mnemonic := 'fbun';
          end
        else
          begin
          op := 170230B + coprocessor_id * 1000B;
          mnemonic := 'fbngle';
          end;
        end;

      fcmp:
        begin
        mnemonic := 'fcmp';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000070B;
        end;

      fcos:
        begin
        mnemonic := 'fcos';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000035B;
        end;

      fcosh:
        begin
        mnemonic := 'fcosh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000031B;
        end;

      fdiv:
        begin
        mnemonic := 'fdiv';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000040B;
        end;

      fetox:
        begin
        mnemonic := 'fetox';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000020B;
        end;

      fetoxm1:
        begin
        mnemonic := 'fetoxm1';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000010B;
        end;

      fgetexp:
        begin
        mnemonic := 'fgetexp';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000036B;
        end;

      fgetman:
        begin
        mnemonic := 'fgetman';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000037B;
        end;

      fint:
        begin
        mnemonic := 'fint';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000001B;
        end;

      fintrz:
        begin
        mnemonic := 'fintrz';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000003B;
        end;

      flog10:
        begin
        mnemonic := 'flog10';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000025B;
        end;

      flog2:
        begin
        mnemonic := 'flog2';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000026B;
        end;

      flogn:
        begin
        mnemonic := 'flogn';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000024B;
        end;

      flognp1:
        begin
        mnemonic := 'flognp1';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000006B;
        end;

      fmod:
        begin
        mnemonic := 'fmod';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000041B;
        end;

      fmove:
        begin
        mnemonic := 'fmove';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 0; { must know which direction }
        end;

      fmovecr:
        begin
        mnemonic := 'fmovecr';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 056000B;
        end;

      fmove_to_fpcr, fmove_from_fpcr:
        begin
        mnemonic := 'fmove';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 100000B;
        end;

      fmovem:
        begin
        mnemonic := 'fmovem';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 140000B;
        end;

      fmul:
        begin
        mnemonic := 'fmul';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000043B;
        end;

      fneg:
        begin
        mnemonic := 'fneg';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000032B;
        end;

      fnop:
        begin
        mnemonic := 'fnop';
        op := 170200B + coprocessor_id * 1000B;
        op2 := 0;
        end;

      frem:
        begin
        mnemonic := 'frem';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000045B;
        end;

      fscale:
        begin
        mnemonic := 'fscale';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000046B;
        end;

      fsgldiv:
        begin
        mnemonic := 'fsgldiv';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000044B;
        end;

      fsglmul:
        begin
        mnemonic := 'fsglmul';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000047B;
        end;

      fsin:
        begin
        mnemonic := 'fsin';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000016B;
        end;

      fsincos:
        begin
        mnemonic := 'fsincos';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000060B;
        end;

      fsinh:
        begin
        mnemonic := 'fsinh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000002B;
        end;

      fsqrt:
        begin
        mnemonic := 'fsqrt';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000004B;
        end;

      fsub:
        begin
        mnemonic := 'fsub';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000050B;
        end;

      ftan:
        begin
        mnemonic := 'ftan';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000017B;
        end;

      ftanh:
        begin
        mnemonic := 'ftanh';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000011B;
        end;

      ftentox:
        begin
        mnemonic := 'ftentox';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000022B;
        end;

      ftrap: { NYI }
        begin
        mnemonic := 'ftrap';
        op := 170170B + coprocessor_id * 1000B;
        op2 := 0;
        end;

      ftst:
        begin
        mnemonic := 'ftest';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000072B;
        end;

      ftwotox:
        begin
        mnemonic := 'ftwotox';
        op := 170000B + coprocessor_id * 1000B;
        op2 := 000021B;
        end;

        { 68000 and 68020 instructions
        }
      add:
        begin
        mnemonic := 'add';
        op := 150000B; { + reg*512 + mode*64 + EA }
        end;

      adda:
        begin
        mnemonic := 'ADDA';
        op := 150000B; { + reg*512 + mode*64 + EA }
        end;

      addi:
        begin
        mnemonic := 'ADDI';
        op := 003000B; { + size*64 + EA }
        end;

      addq:
        begin
        mnemonic := 'ADDQ';
        op := 050000B; { data*512 + size*64 + EA }
        end;

      addx:
        begin
        mnemonic := 'addx';
        op := 150400B; { + reg*512 + size*64 + RM*8 + regy }
        end;

      andi:
        begin
        mnemonic := 'ANDI';
        op := 001000B; { + size*64 + EA }
        end;

      andinst:
        begin
        mnemonic := 'and';
        op := 140000B; { + reg*512 + mode*64 + EA }
        end;

      asl:
        begin
        mnemonic := 'asl';
        op := 160400B;
        end;

      asr:
        begin
        mnemonic := 'asr';
        op := 160000B;
        end;

      beq:
        begin
        mnemonic := 'beq';
        op := 063400B;
        end;

      bge:
        begin
        mnemonic := 'bge';
        op := 066000B;
        end;

      bgt:
        begin
        mnemonic := 'bgt';
        op := 067000B;
        end;

      bhi:
        begin
        mnemonic := 'bhi';
        op := 061000B;
        end;

      ble:
        begin
        mnemonic := 'ble';
        op := 067400B;
        end;

      bls:
        begin
        mnemonic := 'bls';
        op := 061400B;
        end;

      blt:
        begin
        mnemonic := 'blt';
        op := 066400B;
        end;

      bmi:
        begin
        mnemonic := 'bmi';
        op := 065400B;
        end;

      bpl:
        begin
        mnemonic := 'bpl';
        op := 065000B;
        end;

      bne:
        begin
        mnemonic := 'bne';
        op := 063000B;
        end;

      blo:
        begin
        mnemonic := 'bcs';
        op := 062400B; { =bcs }
        end;

      bhs:
        begin
        mnemonic := 'bcc';
        op := 062000B; { =bcc }
        end;

      bvc:
        begin
        mnemonic := 'bvc';
        op := 064000B;
        end;

      bvs:
        begin
        mnemonic := 'bvs';
        op := 064400B;
        end;

      bchg:
        begin
        mnemonic := 'bchg';
        op := 000100B;
        end;

      bclr:
        begin
        mnemonic := 'bclr';
        op := 000200B;
        end;

      bfclr:
        begin
        mnemonic := 'bfclr';
        op := 166300B;
        end;

      bfexts:
        begin
        mnemonic := 'bfexts';
        op := 165700B;
        end;

      bfextu:
        begin
        mnemonic := 'bfextu';
        op := 164700B;
        end;

      bfins:
        begin
        mnemonic := 'bfins';
        op := 167700B;
        end;

      bfset:
        begin
        mnemonic := 'bfset';
        op := 167300B;
        end;

      bftst:
        begin
        mnemonic := 'bftst';
        op := 164300B;
        end;

      bra:
        begin
        mnemonic := 'bra';
        op := 060000B;
        end;

      bset:
        begin
        mnemonic := 'bset';
        op := 000300B;
        end;

      bsr:
        begin
        mnemonic := 'bsr';
        op := 060400B;
        end;

      btst:
        begin
        mnemonic := 'btst';
        op := 000000B;
        end;

      chk:
        begin
        mnemonic := 'chk';

        if datasize = word then op := 040600B { + reg*512 + EA }
        else {long} op := 040400B; { + reg*512 + EA }
        end;

      clr:
        begin
        mnemonic := 'clr';
        op := 041000B; { + size*64 + EA }
        end;

      cmp:
        begin
        mnemonic := 'cmp';
        op := 130000B; { + reg*512 + mode*64 + EA }
        end;

      cmpa:
        begin
        mnemonic := 'CMPA';
        op := 130000B; { + reg*512 + mode*64 + EA }
        end;

      cmpi:
        begin
        mnemonic := 'CMPI';
        op := 006000B; { + size*64 + EA }
        end;

      cmpm:
        begin
        mnemonic := 'cmpm';
        op := 130410B;
        end;

      dbra:
        begin
        mnemonic := 'dbra';
        op := 050710B; { =dbf }
        end;

      dbeq:
        begin
        mnemonic := 'dbeq';
        op := 053710B;
        end;

      dbge:
        begin
        mnemonic := 'dbge';
        op := 056310B;
        end;

      dbgt:
        begin
        mnemonic := 'dbgt';
        op := 057310B;
        end;

      dbhi:
        begin
        mnemonic := 'dbhi';
        op := 051310B;
        end;

      dbhs:
        begin
        mnemonic := 'dbcc';
        op := 052310B; { =dbcc }
        end;

      dble:
        begin
        mnemonic := 'dble';
        op := 057710B;
        end;

      dbls:
        begin
        mnemonic := 'dbls';
        op := 051710B;
        end;

      dblt:
        begin
        mnemonic := 'dblt';
        op := 056710B;
        end;

      dblo:
        begin
        mnemonic := 'dbcs';
        op := 052710B; { =dbcs }
        end;

      dbmi:
        begin
        mnemonic := 'dbmi';
        op := 055710B;
        end;

      dbpl:
        begin
        mnemonic := 'dbpl';
        op := 055310B;
        end;

      dbne:
        begin
        mnemonic := 'dbne';
        op := 053310B;
        end;

      dbvc:
        begin
        mnemonic := 'dbvc';
        op := 054310B;
        end;

      dbvs:
        begin
        mnemonic := 'dbvs';
        op := 054710B;
        end;

      divs:
        begin
        mnemonic := 'divs';
        op := 100700B; { + reg*512 + EA }
        end;

      divu:
        begin
        mnemonic := 'divu';
        op := 100300B; { + reg*512 + EA }
        end;

      divsl:
        begin
        mnemonic := 'tdivsl';
        op := 046100B; { + EA }
        end;

      divul:
        begin
        mnemonic := 'tdivul';
        op := 046100B; { + EA }
        end;

      eor:
        begin
        mnemonic := 'eor';
        op := 130000B; { + reg*512 + mode*64 + EA }
        end;

      eori:
        begin
        mnemonic := 'EORI';
        op := 005000B; { + size*64 + EA }
        end;

      exg:
        begin
        mnemonic := 'exg';
        op := 140400B; { + reg*512 + regy }
        end;

      ext:
        begin
        mnemonic := 'ext';
        op := 044200B; { + longflag*64 + reg }
        end;

      extb:
        begin
        mnemonic := 'extb';
        op := 044200B; { + byte-to-long-bits*64 + reg }
        end;

      jmp:
        begin
        mnemonic := 'jmp';
        op := 047300B; { + EA }
        end;

      jsr:
        begin
        mnemonic := 'jsr';
        op := 047200B; { + EA }
        end;

      lea:
        begin
        mnemonic := 'lea';
        op := 040700B; { + reg*512 + EA }
        end;

      link:
        begin
        mnemonic := 'link';

        if datasize = word then op := 047120B { + reg }
        else {long} op := 044010B; { + reg }
        end;

      lsl:
        begin
        mnemonic := 'lsl';
        op := 160410B;
        end;

      lsr:
        begin
        mnemonic := 'lsr';
        op := 160010B;
        end;

      movea, move:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'A' if movea }
        if datasize = byte then op := 010000B
        else if datasize = word then op := 030000B
        else op := 020000B;
        end;

      move_to_ccr:
        begin
        mnemonic := 'move';
        op := 042300B;
        end;

      movem:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'M' }
        op := 044200B; { + size*64 + EA }
        end;

      moveq:
        begin
        mnemonic := 'MOVE'; { postcorrect adds 'Q' }
        op := 070000B; { + reg*512 + data }
        end;

      muls:
        begin
        if mc68020 and (datasize = long) then
          begin
          mnemonic := 'tmulsl';
          op := 046000B; { + EA }
          end
        else
          begin
          mnemonic := 'muls';
          op := 140700B { + reg*512 + EA }
          end;
        end;

      mulu:
        begin
        if mc68020 and (datasize = long) then
          begin
          mnemonic := 'tmulul';
          op := 046000B; { + EA }
          end
        else
          begin
          mnemonic := 'mulu';
          op := 140300B { + reg*512 + EA }
          end;
        end;

      neg:
        begin
        mnemonic := 'neg';
        op := 042000B; { + size*64 + EA }
        end;

      negx:
        begin
        mnemonic := 'negx';
        op := 040000B; { + size*64 + EA }
        end;

      notinst:
        begin
        mnemonic := 'not';
        op := 043000B; { + size*64 + EA }
        end;

      ori:
        begin
        mnemonic := 'ORI';
        op := 000000B; { + size*64 + EA }
        end;

      orinst:
        begin
        mnemonic := 'or';
        op := 100000B; { + reg*512 + mode*64 + EA }
        end;

      pea:
        begin
        mnemonic := 'pea';
        op := 044100B; { + EA, control modes only }
        end;

      rol:
        begin
        mnemonic := 'rol';
        op := 160430B;
        end;

      ror:
        begin
        mnemonic := 'ror';
        op := 160030B;
        end;

      roxl:
        begin
        mnemonic := 'roxl';
        op := 160420B;
        end;

      roxr:
        begin
        mnemonic := 'roxr';
        op := 160020B;
        end;

      rte:
        begin
        mnemonic := 'rte';
        op := 047163B;
        end;

      rts:
        begin
        mnemonic := 'rts';
        op := 047165B;
        end;

      sub:
        begin
        mnemonic := 'sub';
        op := 110000B; { + reg*512 + mode*64 + EA }
        end;

      suba:
        begin
        mnemonic := 'SUBA';
        op := 110000B; { + reg*512 + mode*64 + EA }
        end;

      subi:
        begin
        mnemonic := 'SUBI';
        op := 002000B; { + size*64 + EA }
        end;

      subq:
        begin
        mnemonic := 'SUBQ';
        op := 050400B; { data*512 + size*64 + EA }
        end;

      subx:
        begin
        mnemonic := 'subx';
        op := 110400B; { + reg*512 + size*64 + RM*8 + regy }
        end;

      swap:
        begin
        mnemonic := 'swap';
        op := 044100B; { + Dreg }
        end;

      trap:
        begin
        mnemonic := 'trap';
        op := 047100B; { + vector }
        end;

      trapcc:
        begin
          { Thus far we only need the TRAPLS form of this instruction (for
            Versados).  The Versados assembler wants the mnemonic to be TLS
            instead of TRAPLS.
          }
        mnemonic := 'tls';
        op := 051774B;
        end;

      trapv:
        begin
        mnemonic := 'trapv';
        op := 047166B;
        end;

      tst:
        begin
        mnemonic := 'tst';
        op := 045000B; { + size*64 + EA }
        end;

      unlk:
        begin
        mnemonic := 'unlk';
        op := 047130B; { + reg }
        end;

      end; {case inst}

    insertobj(op);

    if inst in [fp_first..fp_last] then
      fp_src_spec := loophole(integer, n^.fp_format);

    if switcheverplus[outputmacro] then
      begin
      { print mnemonic to macro file, suppressing trailing blanks }
      reposition(opcolumn);

      for i := 1 to ord(mnemonic[0]) do writech(uppercase(mnemonic[i]));

      postcorrect;

      if n^.oprndcount <> 0 then reposition(opndcolumn);
      end; {macro output}

  end; {writeinst}
{>>>}
{<<<}
procedure writebitfield(reg, offset, width: integer);

{ Output the bit field descriptor for the 68020 bit field instructions.
  If the reg field is not -1 then it indicates that the offset is in that
  D-reg.
}


  begin
    if switcheverplus[outputmacro] then
      begin
      writech('{');
      if reg <> - 1 then
        begin
        writech('D');
        writeint(reg);
        end
      else
        begin
        writeint(offset);
        end;

      writech(':');
      writeint(width);
      writech('}');
      end;
  end;
{>>>}
{<<<}
procedure writelastopnd;

{ Outputs the assembler code for the node currently pointed to by "n".
  If this procedure has been called by "BuildInstruction" then it is
  the last operand for the current instruction.  If, however, it has
  been called by "writeopnd", it is actually the first of two operands,
  and will have a comma appended by "writeopnd" before returning to
  process the second operand.
}

  var
    vtptr: vartablerecptr;
    kluge:
      record
        case integer of
          1: (l: integer {long word} );
          2:
            (w: packed array [boolean] of - 32767..32767);
      end {kluge} ;
    s: longname;

    { Write out the scale factor for the 68020 indexed instructions.
      A scale factor of 1 is the default and so is ignored.
    }


  procedure write_scale;


    begin
      if mc68020 then
        with n^.oprnd do
          if scale <> 1 then
            begin
            writech('*');
            writeint(scale);
            end;
    end;


  begin {writelastopnd}
    if switcheverplus[outputmacro] then
      with n^.oprnd do
        case m of

          nomode: puterror(nomodeoprnd);

          dreg:
            begin
            writech('D');
            writeint(reg);
            end; {dreg}

          twodregs:
              { This is currently only for the divsl and divul instructions.
                Reg is the quotient register and indxr is the remainder
                register.  The format is Dr:Dq (remainder:quotient).

                Note: If the quotient and remainder registers are the same
                then only a 32 bit quotient will be generated.
              }

            begin
            writech('D');
            writeint(indxr);

            if reg <> indxr then
              begin
              writech(':');
              writech('D');
              writeint(reg);
              end;
            end; {twodregs}

          areg:
            begin
            if reg = 7 then writestr('SP')
            else
              begin
              writech('A');
              writeint(reg);
              end;
            end; {areg}

          fpreg:
            begin
            writestr('FP');
            writeint(reg);
            end;

          twofpregs:
              { This is currently only for 68881 fsincos instruction.
                Reg is the cosine register and indxr is the sine.  The
                format is FPc:FPs (cosine:sine).
              }

            begin
            writestr('FP');
            writeint(reg);
            writech(':');
            writestr('FP');
            writeint(indxr);
            end; {twofpregs}

          indr:
            begin
            if reg = 7 then writestr('(SP)')
            else
              begin
              writestr('(A');
              writeint(reg);
              writech(')');
              end;
            end; {indr}

          autoi:
            begin
            if reg = 7 then writestr('(SP)+')
            else
              begin
              writestr('(A');
              writeint(reg);
              writestr(')+');
              end;
            end; {autoi}

          autod:
            begin
            if reg = 7 then writestr('-(SP)')
            else
              begin
              writestr('-(A');
              writeint(reg);
              writech(')');
              end;
            end; {autod}

          relative:
            begin
            if mc68020 and ((offset > 32767) or (offset < -32768)) then
              begin
              writech('(');
              writeint(offset);
              writech(',');
              end
            else
              begin
              writeint(offset);
              writech('(');
              end;

            if reg = 7 then
              begin
              writestr('SP)');
              if offset < 0 then puterror(negativesp)
              end
            else
              begin
              writech('A');
              writeint(reg);
              writech(')');
              end;
            end; {relative}

          indexed:
            begin
            if not mc68020 and ((offset > 127) or (offset < - 128)) then
              puterror(baddisplacement);
            if mc68020 then
              begin
              writech('(');
              writeint(offset);
              writech(',');
              end
            else
              begin
              writeint(offset);
              writech('(');
              end;

            writech('A');
            writeint(reg);
            writestr(',D');
            writeint(indxr);
            if indxlong then writestr('.L')
            else writestr('.W');
            write_scale;
            writech(')');
            end; {indexed}

          bitindexed:
            begin
            puterror(bitindexmode);
            end; {bitindexed}

          absshort:
            begin
            writeint(offset);
            end; {absshort}

          abslong:
            begin
            writech('$');
            kluge.l := offset;
              { the next two lines assume that "reversebytes" implies that
                words are also reversed. }
            WriteHex(kluge.w[reversebytes]);
            WriteHex(kluge.w[not reversebytes]);
            end; {abslong}

          immediate, special_immediate:
            begin
            writech('#');
            if datasize <= word then writeint(offset)
            else
              begin
              writech('$');
              if hostintsize <= word then
                begin
                if offset < 0 then WriteHex( - 1)
                else WriteHex(0);
                WriteHex(offset);
                end
              else
                begin
                kluge.l := offset;
                  { the next two lines assume that "reversebytes" implies that
                    words are also reversed. }
                WriteHex(kluge.w[reversebytes]);
                WriteHex(kluge.w[not reversebytes]);
                end;
              end;
            end; {immediate}

          immediatelong:
            begin
            writech('#');

              { Floating point constants in hex in 68881 instructions must
                be prefixed by a ":" instead of a "$" -- Strange!
              }
            if (currinst in [fp_first..fp_last]) then writech(':')
            else writech('$');

            WriteHex(offset1);
            WriteHex(offset);
            end; { immediatelong }

          immediatequad:
            begin
            writech('#');

              { Floating point constants in hex in 68881 instructions must
                be prefixed by a ":" instead of a "$" -- Strange!
              }
            if (currinst in [fp_first..fp_last]) then writech(':')
            else writech('$');

            WriteHexLong(offset1);
            WriteHexLong(offset);
            end; { immediatequad }

          immediate_extended:
            begin
            writech('#');

              { Floating point constants in hex in 68881 instructions must
                be prefixed by a ":" instead of a "$" -- Strange!
              }
            if (currinst in [fp_first..fp_last]) then writech(':')
            else writech('$');

            WriteHexLong(offset2);
            WriteHexLong(offset1);
            WriteHexLong(offset);
            end; { immediate_extended }

          commonlong:
            begin
            if commonlong_reloc < 0 then writech('G');
            if commonlong_reloc > 0 then
              begin
              vtptr := getvartableptr(commonlong_reloc);
              with vtptr^ do
                begin
{                n^.oprnd.offset := n^.oprnd.offset + offset;}
                  { Add in psect offset for Versados define'd var }
                if $pic and (extvaralloc = sharedalloc) then
                  begin
                  writech('#');
                  WriteSymbolName(get_from_sfile(charindex, charlen, true));
                  writech('-');
                  supname(loophole(libroutines, libown), s);
                  WriteSymbolName(s);
                  end
                else WriteSymbolName(get_from_sfile(charindex, charlen,
                                     not aliased));
                end;
              end;

            if offset <> 0 then
              begin
              if offset >= 0 then writech('+');
              writeint(offset);
              end;
            end; {commonlong}

          pcrelative:
            begin
            writech('L');
            if offset >= 0 then writech('+');
            writeint(offset);
            if (n^.operandcost < long) or $pic then writestr('(PC)');
            end; {pcrelative}

          pcindexed:
            begin
            if not mc68020 and ((offset > 127) or (offset < - 128)) then
              puterror(baddisplacement);
            if mc68020 then
              begin
              writestr('(*');
              {??? how much bias is needed for 68020 ???}
              if offset + word >= 0 then writech('+');
              writeint(offset + word);
              writech(',');
              end
            else
              begin
              writech('*');
              if offset + word >= 0 then writech('+');
              writeint(offset + word);
              writech('(');
              end;
            writestr('PC,D');
            writeint(indxr);
            writestr('.W');
            write_scale;
            writech(')');
            end; {pcindexed}

          supportcall:
            begin
            if (offset < ord(first_call)) or (offset > ord(last_call)) then
              puterror(badsupportcall);

            supname(loophole(libroutines, offset), s);

            if $pic then
              if mc68020 then
                begin { must use 68020 32 bit form }
                writech('(');
                WriteSymbolName(s);
                writestr(',PC)');
                end
              else
                begin { use 68000 16 bit form }
                WriteSymbolName(s);
                writestr('(PC)');
                end
            else WriteSymbolName(s); { non pic -- use absolute form }
            end; {supportcall}

          usercall:
            begin
            if $pic then
              if mc68020 and ((proctable[offset].externallinkage and
                 not proctable[offset].bodydefined) or
                 (procmap[offset].addr = undefinedaddr) or
                 (n^.operandcost >= long)) then
                begin { must use 68020 32 bit form }
                writech('(');
                if proctable[n^.oprnd.offset].externallinkage then
                  writeprocname(offset, linknameused) {maintains col counter}
                else
                  begin
                  writech('P');
                  writeint(offset);
                  end;

                if offset1 <> 0 then
                  begin
                  writeint(offset1);
                  if odd(offset1) or (offset1 > 0) then puterror(badoffset);
                  end;

                writestr(',PC)');
                end
              else
                begin { use 68000 16 bit form }
                if proctable[n^.oprnd.offset].externallinkage then
                  writeprocname(offset, linknameused) {maintains col counter}
                else
                  begin
                  writech('P');
                  writeint(offset);
                  end;

                if offset1 <> 0 then
                  begin
                  writeint(offset1);
                  if odd(offset1) or (offset1 > 0) then puterror(badoffset);
                  end;

                writestr('(PC)');
                end
            else if proctable[n^.oprnd.offset].externallinkage then
              begin
              writeprocname(offset, linknameused); {maintains column counter}

              if proctable[offset].bodydefined and
                 (procmap[offset].addr <> undefinedaddr) and
                 (n^.operandcost < long) then writestr('(PC)');
              end
            else
              begin { not external call }
              writech('P');
              writeint(offset);

              if offset1 <> 0 then
                begin
                writeint(offset1);
                if odd(offset1) or (offset1 > 0) then puterror(badoffset);
                end;

              if n^.operandcost < long then writestr('(PC)');
              end;
            end; {usercall}

          pic_own_immed:
              { In PIC mode this can only occur for the code to load A3
                at the beginning of each procedure.
              }
            begin
            writestr('#G-');
            supname(loophole(libroutines, libown), s);
            WriteSymbolName(s);
            end;

          pic_splat_pcrel:

            { For 68000 24-bit PIC only.  Generates "#<offset>+*(PC)".
            }
            begin
            writeint(offset);
            writestr('+*');
            writestr('(PC)');
            end;

          pic_usercall:

            { For 68000 24-bit PIC only.  Generates "#<name>-<offset>-*".
            }
            begin
            writech('#');

            if proctable[n^.oprnd.offset].externallinkage then
              writeprocname(offset, linknameused) {maintains column counter}
            else
              begin { not external call }
              writech('P');
              writeint(offset);
              end;

            writech('-');
            writeint(offset1);
            writestr('-*');
            end;

          pic_supportcall:

            { For 68000 24-bit PIC only.  Generates "#<suppt_call>-<offset>-*".
            }
            begin
            writech('#');
            supname(loophole(libroutines, offset), s);
            WriteSymbolName(s);
            writech('-');
            writeint(offset1);
            writestr('-*');
            end;

          pic_branch:
            begin
            writestr('#L');
            writeint(offset);
            writech('-');
            writeint(offset1);
            writestr('-*');
            end;

          pic_pcrelative:
            begin
            writestr('#L+');
            writeint(offset);
            writech('-');
            writeint(offset1);
            writestr('-*');
            end;
          end; {case mode}

  end; {writelastopnd}
{>>>}
{<<<}
procedure writeopnd;


  begin
    if switcheverplus[outputmacro] then
      begin
      writelastopnd;
      writech(',');
      end;
  end;
{>>>}
{<<<}
function computedistance: addressrange;

{ The current node contains a (signed) number of instructions to branch
  over.  The current instruction is either a branch or a decrement-and-
  branch.  If the latter, the value returned is relative to the 2nd word
  of the instruction.
}

  var
    tempnode: nodeindex; { so we don't screw up currnode }
    instcount: integer; { number of instructions to skip over }
    bytecount: integer; { accumulates the byte offset }
    i: integer; { induction var for counting instructions }


  begin
    tempnode := currnode;
    instcount := n^.distance;

    repeat { find current instruction node }
      tempnode := tempnode - 1;
      if bigcompilerversion then p := ref(bignodetable[tempnode])
      else creadaccess(tempnode, p);
    until p^.kind = instnode;

    bytecount := - 2; { the opcode is at (PC-2) regardless of length! }

    if instcount < 0 then { backward scan }
      for i := - 2 downto instcount do
        begin
        repeat { find previous instruction node }
          tempnode := tempnode - 1;
          if bigcompilerversion then p := ref(bignodetable[tempnode])
          else creadaccess(tempnode, p);
        until p^.kind = instnode;

        bytecount := bytecount - p^.computed_length {instlength(tempnode)}
        end

    else { instcount > 0 } { forward scan }
      for i := 0 to instcount do
        begin
        bytecount := bytecount + p^.computed_length {instlength(tempnode)} ;

        repeat { find next instruction node }
          tempnode := tempnode + 1;
          if bigcompilerversion then p := ref(bignodetable[tempnode])
          else creadaccess(tempnode, p);
        until p^.kind = instnode;
        end;

    computedistance := bytecount
  end; { computedistance }
{>>>}
{<<<}
procedure writelabels;


  begin { write all labels which refer to this node }
    if column <> 1 then writeline; { a previous label may be "open" if a nop
                                     node has intervened }
    writech('L');
    writeint(labeltable[currlabel].labno);
    writech(':');
    currlabel := currlabel + 1;

    while currnode = labeltable[currlabel].nodelink do
      begin { write additional labels on separate lines }
      writeline;
      writech('L');
      writeint(labeltable[currlabel].labno);
      writech(':');
      currlabel := currlabel + 1;
      end; { write additional labels }

  end; { write all labels }
{>>>}
{<<<}
procedure doblocklabel;

{ Print a label in the assembler file to mark the start of a procedure.
}



  begin
    writech('*');
    writeline;
    writestr('*  [');
    write(MacFile, currentpc: - 4);
    write(MacFile, ']  ');

    if blockref = 0 then write(MacFile, 'Main Body:')
    else
      begin
      writeprocname(blockref, 100);
      end;
    writeline;
    writech('*');
    writeline;
  end; {doblocklabel}
{>>>}
{<<<}
procedure DoBlockEntryCode;

  var
    lscan: labelindex;


  begin
    if (switchcounters[debugging] > 0) or (switchcounters[profiling] > 0) then
      writemaprecord(plabrec, 0, 0, procsym);
    procmap[blockref].addr := currentpc; { update procedure address table }

    if  (level = 1)
    and (  (proctable[blockref].calllinkage = pascal2call)
        or (proctable[blockref].calllinkage = modulebody))
    then
      if switchcounters[mainbody] > 0 then
        begin { process main body of program }
        if switcheverplus[outputmacro] then writestr('BEGIN$:');

        startaddress := currentpc;
        end { switchcounters[mainbody] > 0 }
      else
        begin { level=1 and nomainbody }
        if switcheverplus[outputmacro] then
          begin
          writech('*');
          writeline;
          end;
        end

    else
      begin { block other than main }

      { test for external procedure (body is obviously defined) }
      if proctable[blockref].externallinkage
      or (   (proctable[blockref].calllinkage = implementationbody)
         and (level = 1))
      then
        with newESD do
          begin { prepare a new table entry }
          ESDkind := ESDentry;
          exproc := blockref;
          insertnewESD;
          end; { table entry }

      if switcheverplus[outputmacro] then
        begin
        if proctable[blockref].externallinkage
        or (   (proctable[blockref].calllinkage = implementationbody)
           and (level = 1))
        then
          begin
          writeprocname(blockref, linknameused);
          writech(':');
          writeline;
          end
        else
          begin { not external }
          writech('P');
          writeint(blockref);
          writech(':');
          end;
        end {macro output for block entry} ;
      end; { block other than main }


{ Service the fixup list with new label definitions, and possibly the
  current procedure address, if it was declared forward.
}

    fixp := fixuphead;
    while fixp <> nil do
      begin
      with fixp^ do { examine the node }
        if fixupkind = fixupproc then { compare proc numbers }

          if fixupprocno = blockref then
            fixupaddr := currentpc { this is the forward target }
          else { maybe next time }

        else { fixupkind = fixuplabel }
          for lscan := 1 to nextlabel do
            if labeltable[lscan].labno = fixuplabno then
              fixupaddr := labeltable[lscan].address;

      fixp := fixp^.fixuplink;
      end;
  end; { DoBlockEntryCode }
{>>>}
{<<<}
procedure buildbranches;

{ Code to build a branch instruction.  This is pulled out of buildinstruction
  so the compiler can handle it.
}

  var
    labeldelta: integer; {for signed comparisons}
    isforward: integer; {true if forward reference}


  begin
    if n^.kind = labelnode then
      begin
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;

      findlabelpc(n^.labelno, isforward); { find or compute target's addr }

      if n^.labelcost = long then { 68020 and pic only }
        begin
        op := 16#FF + op;
        insertobj((labelpc - currentpc) div 16#10000);
        insertobj((labelpc - currentpc + 2) mod 16#10000);

        if isforward <> 0 then
          begin
          objtype[objctr - 1] := objlong;
          allocfixup; { generate a new fixupnode }
          fixups[objctr - 1] := fixuptail;
          with fixuptail^ do
            begin
            fixupkind := fixuplabel;
            fixuplen := long;
            fixuplabno := n^.labelno;
            fixupobjpc := fixupobjpc - 4;
            end;
          end;
        end
      else
      if n^.labelcost = word then
        begin { word branch }
        insertobj(labelpc - currentpc); { this bumps pc by 2 }
        if isforward <> 0 then
          begin
          write('Forward BRA illegal');
          abort(inconsistent);
          end;
        end { long branch }

      else
        begin { short branch }
        labeldelta := labelpc - currentpc;

          { This is a signed test on unsigned operands
          }
        if (labeldelta > 127) or (labeldelta < - 128) then
          puterror(badoperand);

        op := (labeldelta and 377B) + op;
        end { short branch }
      end {labelnode}

    else if n^.kind = relnode then
      begin { relnodes are short, unlabeled offsets }
      distancetemp := computedistance;
      op := (distancetemp and 255) + op;

      if switcheverplus[outputmacro] then
        begin
        writech('*');
        if distancetemp < 0 then writeint(distancetemp)
        else
          begin
          writech('+');
          writeint(distancetemp + word);
          end;
        end;
      end
    else puterror(nolabelnode);
  end; {buildbranches}
{>>>}
{<<<}
procedure buildfpbranches;

{ Code to build a 68881 branch instruction.
}

  var
    isforward: integer; {true if forward reference}


  begin
    if n^.kind = labelnode then
      begin
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;

      findlabelpc(n^.labelno, isforward); { find or compute target's addr }
      distancetemp := labelpc - currentpc; { bump pc by 2 }

      if n^.labelcost = long then { 32 bit branch }
        begin
        op := op + 100B; { set size bit }
        insertobj(distancetemp div 16#10000);
        insertobj(distancetemp mod 16#10000);

        if isforward <> 0 then
          begin
          objtype[objctr - 1] := objlong;
          allocfixup; { generate a new fixupnode }
          fixups[objctr - 1] := fixuptail;
          with fixuptail^ do
            begin
            fixupkind := fixuplabel;
            fixuplen := long;
            fixuplabno := n^.labelno;
            fixupobjpc := fixupobjpc - 4;
            end;
          end;
        end
      else { 16 bit branch } insertobj(distancetemp);
      end {labelnode}
    else if n^.kind = relnode then
      begin { relnodes are short, unlabeled branches. }
      distancetemp := computedistance;

      insertobj(distancetemp);

      if switcheverplus[outputmacro] then
        begin
        writech('*');
        if distancetemp < 0 then writeint(distancetemp)
        else
          begin
          writech('+');
          writeint(distancetemp + word);
          end;
        end;
      end
    else puterror(nolabelnode);
  end; {buildfpbranches}
{>>>}
{<<<}
procedure builddbxx;

  var
    isforward: integer; {true if forward reference}


  begin
    if n^.oprnd.m <> dreg then puterror(badsource);
    op := op + n^.oprnd.reg;
    writeopnd;
    getnextnode;

    if n^.kind = labelnode then
      begin { process the label }
      if switcheverplus[outputmacro] then
        begin
        writech('L');
        writeint(n^.labelno);
        end;

      findlabelpc(n^.labelno, isforward); { find or compute target's addr }
      insertobj(labelpc - currentpc); { this bumps pc by 2 }
      if isforward <> 0 then abort(inconsistent);
      end
    else if n^.kind = relnode then
      begin
      distancetemp := computedistance;
      insertobj(labelpc - currentpc);

      if switcheverplus[outputmacro] then
        begin
        writech('*');
        if distancetemp < 0 then writeint(distancetemp)
        else
          begin
          writech('+');
          writeint(distancetemp + word);
          end;
        end;
      end
    else puterror(nolabelnode);
  end; {builddbxx}
{>>>}
{<<<}
procedure buildmovem(gen_fmovem: boolean);

  var
    i: 0..7;


  begin
    if n^.oprnd.m = immediate then
      begin { save registers }
      datasize := word; {mask is only 16 bits long}

      if not gen_fmovem then setmodeonly; { process the register mask }

      if switcheverplus[outputmacro] then
      begin
      mask := 1;
      first := true;

      if gen_fmovem then
        for i := 0 to 7 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writestr('FP');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end
      else
        begin
        for i := 7 downto 0 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('A');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;

        for i := 7 downto 0 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('D');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;
        end;
      writech(',');
      end;
      if gen_fmovem then
        begin { mode 00 is static list, -(An) }
        op2 := op2 + 20000B { indicate direction mem-to-reg }
               + n^.oprnd.offset; { glue in list }
        insertobj(op2);
        end;

      getoperand;
      if not (n^.oprnd.m in [relative, autod]) then puterror(baddestination);
      seteffective; { autodec mode and register }
      writelastopnd;
      end
    else if n^.oprnd.m = autoi then
      begin { restore registers }
      writeopnd;
      seteffective;
      getoperand;

      if gen_fmovem then
        begin
        op2 := op2 + 2 * 4000B { mode 10 is static list, (An)+ }
               + n^.oprnd.offset; { glue in list }
        insertobj(op2);
        end
      else op := op + 2000B; { indicate direction mem-to-reg }

      datasize := word; { mask is only 16 bits long }

      if not gen_fmovem then setmodeonly; { append register mask }

      if n^.oprnd.m <> immediate then puterror(baddestination);
      if switcheverplus[outputmacro] then
      begin
      mask := 1;
      first := true;

      if gen_fmovem then
        for i := 7 downto 0 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writestr('FP');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end
      else
        begin
        for i := 0 to 7 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('D');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;

        for i := 0 to 7 do
          begin
          if n^.oprnd.offset and mask <> 0 then
            begin
            if not first then writech('/');
            writech('A');
            writech(chr(i + ord('0')));
            first := false;
            end;
          mask := mask * 2;
          end;
        end;
      end;
      end
    else puterror(badsource);

    if not gen_fmovem then op := op + 100B; { preinitialized for word; change
                                              to long }

  end; {buildmovem}
{>>>}
{<<<}
procedure BuildInstruction;

  var
    i: 0..7;
    offset1: integer;
    register: regindex;
    memory: boolean; { used for 68881 instructions }
    n1: nodeptr;
    isforward: integer;


  procedure output_fp_creg;

    { Output the 68881 control register name for the FMOVE system control
      register instruction.
    }


    begin
      if switcheverplus[outputmacro] then
        begin
        case n^.oprnd.offset of
          1: writestr('FPIAR');
          2: writestr('FPSR');
          4: writestr('FPCR');
          end;
        end
    end; {output_fp_creg}


  begin {BuildInstruction}
    {branches are pulled out to let this fit through the 11 compiler}

    if currinst in branches then buildbranches
    else if currinst in fpbranches then buildfpbranches
    else if currinst in
            [dbra, dbeq, dbge, dbgt, dbhi, dbhs, dble, dblo, dbls, dblt, dbmi,
            dbpl, dbne, dbvc, dbvs] then
      builddbxx
    else
      case currinst of

          { 68881 instructions
          }

        fabs, facos, fadd, fasin, fatan, fatanh, fcos, fcosh, fetox, fetoxm1,
        fgetexp, fgetman, fint, fintrz, flog10, flog2, flogn, flognp1, fmod,
        fmul, fneg, frem, fscale, fsglmul, fsin, fsinh, fsqrt, ftan, ftanh,
        ftentox, ftrap, ftwotox:
          begin
          if n^.oprnd.m <> fpreg then
            begin
            memory := true;
            register := fp_src_spec;
            writeopnd;
            getoperand;
            end
          else
            begin
            memory := false;
            register := n^.oprnd.reg;
            getoperand;

            { Suppress duplicate registers
            }
            if (register = n^.oprnd.reg) and
               not (currinst in [fadd, fmul, fsglmul, frem, fmod, fscale]) then
              {suppress}
            else
              begin
              getprevoperand(1);
              writeopnd;
              currnode := currnode - 1; { Get back in sync }
              getoperand;
              end;
            end;

          writelastopnd;
          insertobj(op2 + ord(memory) * 40000B + register * 2000B +
                    n^.oprnd.reg * 200B);

          if memory then
            begin
            getprevoperand(1);
            seteffective;
            end;
          end;

        fsincos:
          begin
          if n^.oprnd.m <> fpreg then
            begin
            memory := true;
            register := fp_src_spec;
            end
          else
            begin
            memory := false;
            register := n^.oprnd.reg;
            end;

          writeopnd;
          getoperand;
          writelastopnd;
          insertobj(op2 + ord(memory) * 40000B + register * 2000B +
                    n^.oprnd.indxr * 200B + n^.oprnd.reg);
          if memory then
            begin
            getprevoperand(1);
            seteffective;
            end;
          end;

        ftst:
          begin
          if n^.oprnd.m <> fpreg then
            begin
            memory := true;
            register := fp_src_spec;
            end
          else
            begin
            memory := false;
            register := n^.oprnd.reg;
            end;

          writelastopnd;
          insertobj(op2 + ord(memory) * 40000B + register * 2000B);
          if memory then seteffective;
          end;

        fcmp, fsub, fdiv, fsgldiv:

          { These sometimes have reversed operands in the assembler source.
          }
          begin
          if n^.oprnd.m = fpreg then
            begin
            memory := false;
            register := n^.oprnd.reg;
            end
          else
            begin
            memory := true;
            register := fp_src_spec;
            end;

            begin { put out funny Motorola assembler form }
            writeopnd;
            getoperand;
            writelastopnd;
            insertobj(op2 + ord(memory) * 40000B + register * 2000B +
                      n^.oprnd.reg * 200B);
            if memory then
              begin
              getprevoperand(1);
              seteffective;
              end;
            end;
          end;

        fmove:
          begin
          memory := n^.oprnd.m <> fpreg;
          writeopnd;
          register := n^.oprnd.reg;
          getoperand;
          writelastopnd;

          if n^.oprnd.m = fpreg then { memory-to-register form (includes
                                      register-to-register) }
            if memory then
              begin
              insertobj(ord(memory) * 40000B + fp_src_spec * 2000B +
                        n^.oprnd.reg * 200B);
              getprevoperand(1);
              seteffective;
              end
            else { fpreg-to-fpreg }
              insertobj(register * 2000B + n^.oprnd.reg * 200B)
          else { register-to-memory form }
            begin
            insertobj(60000B + fp_src_spec * 2000B + register * 200B);
            seteffective;
            end;
          end;

        fnop:
          begin
          insertobj(op2);
          end;

        fmove_from_fpcr:

          { Move from system control register.
          }
          begin
          offset1 := n^.oprnd.offset;
          output_fp_creg;
          if switcheverplus[outputmacro] then writech(',');
          getoperand;
          writelastopnd;
          insertobj(op2 + 20000B + offset1 * 2000B);
          seteffective;
          end;

        fmove_to_fpcr:

          { Move to system control register.
          }
          begin
          writeopnd;
          getoperand;
          output_fp_creg;
          insertobj(op2 + n^.oprnd.offset * 2000B);
          getprevoperand(1);
          seteffective;
          end;

        fmovecr:

          { Move from 68881 constant rom.
          }
          begin
          offset1 := n^.oprnd.offset;
          writeopnd;
          getoperand;
          writelastopnd;
          insertobj(op2 + n^.oprnd.reg * 200B + offset1);
          end;

        fmovem: buildmovem(true);

          { 68000 and 68020 instructions
          }

        movea, move:
          begin
          writeopnd;
          seteffective;
          getoperand;
          writelastopnd;
          setmodeonly;
          if currinst = movea then
            if datasize = byte then puterror(badsize)
            else if n^.oprnd.m <> areg then puterror(missingAreg);
          op := ((((mode and 7B) * 100B + mode) * 10B) and 7700B) + op;
          end;

        move_to_ccr:
          begin
          writeopnd;
          seteffective;
          if switcheverplus[outputmacro] then writestr('CCR');
          end;

        moveq:
          begin
          if not (n^.oprnd.m in [immediatelong, immediate]) or
             (n^.oprnd.offset > 127) or (n^.oprnd.offset < - 128) then
            puterror(badoperand);
          datasize := byte; { just in case }
          writeopnd;
          op := (n^.oprnd.offset and 377B) + op;
          getoperand;
          writelastopnd;
          if n^.oprnd.m <> dreg then puterror(badoperand);
          op := n^.oprnd.reg * 1000B + op;
          end;

        add, cmp, sub, andinst, orinst:
          begin
          if datasize = word then op := op + 100B
          else if datasize = long then op := op + 200B;

            begin
            writeopnd;

            lookahead(1); { Check destination for d-reg first! If it is, then
                           emit "<Dn> op <EA> --> <Dn>" form only }
            if (n^.oprnd.m = dreg) and (p^.oprnd.m <> dreg) then
              begin
              op := op + 400B; { indicate direction }
              if currinst = cmp then puterror(badsource); {cmp is one-way}
              insertreghi;
              getoperand;
              seteffective;
              end
            else
              begin { must be "<EA> to Dn" form }
              seteffective;
              getoperand;
              if n^.oprnd.m <> dreg then puterror(missingDreg);
              insertreghi;
              end;
            writelastopnd;
            end;
          end;

        addq, subq:
          begin
          if (n^.oprnd.m <> immediate) or (n^.oprnd.offset < 1) or
             (n^.oprnd.offset > 8) then
            puterror(badoperand);
          if n^.oprnd.offset < 8 then { value 8 == bit pattern 0 }
            op := n^.oprnd.offset * 1000B + op;
          insertsize;
          datasize := byte;
          writeopnd;
          getoperand;
          writelastopnd;
          seteffective;
          end;

        adda, cmpa, suba: { address register destination }
          begin
            begin
            seteffective;
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> areg then puterror(missingAreg);
            insertreghi;
            if datasize = word then op := op + 300B
            else if datasize = long then op := op + 700B
            else puterror(badoperand); { no byte mode }
            end;
          end;

        addi, cmpi, subi, andi, eori, ori: { immediate source }
          begin
          if (n^.oprnd.m <> immediate) and (n^.oprnd.m <> immediatelong) then
            puterror(badoperand);
            begin
            insertsize;
            setmodeonly; { processes the immediate data }
            writeopnd;
            getoperand;
            writelastopnd;
            seteffective;
            end;
          end;

        eor: { exclusive or -- differs from other logicals }
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);

          if datasize = word then op := op + 500B
          else if datasize = long then op := op + 600B
          else op := op + 400B;

          writeopnd;
          insertreghi;
          getoperand;
          writelastopnd;
          if n^.oprnd.m = areg then puterror(badoperand);
          seteffective;
          end;

        asl, asr, lsl, lsr, rol, ror, roxl, roxr: { shift group }
          begin
          if n^.oprnd.m = immediate then
            begin
            if (n^.oprnd.offset < 1) or (n^.oprnd.offset > 8) then
              puterror(badoperand);
            lookahead(1); { check for single bit memory shifts }
            if p^.kind <> oprndnode then puterror(missingoperand);

            if p^.oprnd.m = dreg then
              begin { immediate/register }
              if n^.oprnd.offset < 8 then { shift 8 == bit pattern 0 }
                op := n^.oprnd.offset * 1000B + op;
              insertsize;
              datasize := word;
              writeopnd;
              getoperand;
              op := n^.oprnd.reg + op;
              end { immediate/register }

            else
              begin { immediate/memory -- enforce shift count = 1 }
              if n^.oprnd.offset <> 1 then puterror(badsource)
              else
                op := (op and 30B) * 100B { relocate subtype }
                      + (op and 400B) { save direction bit }
                      + 160300B; { size of 3 decodes to memory shift! }
              if datasize <> word then puterror(badsize);
              getoperand; { do not write out the shift count! }
              seteffective;
              end; { immediate/memory }
            end { immediate (or implied) shift count form }

          else { register/register form -- instruction needs correction }
            begin
            op := op + 40B;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertreghi; { this reg has the shift count }
            insertsize; { all sizes are permissible }
            writeopnd;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            op := n^.oprnd.reg + op;
            end;

          writelastopnd;
          end;

        bchg, bclr, bset, btst: { bit manipulation group }
          begin
          if (n^.oprnd.m <> dreg) and (n^.oprnd.m <> immediate) then
            puterror(badsource);
          if n^.oprnd.m = dreg then
            begin { bit number dynamic mode }
            op := op + 400B;
            insertreghi; { register containing bit number }
            end
          else
            begin { bit number static mode }
            op := op + 4000B;
            setmodeonly; { process the immediate data }
            end;
          writeopnd;
          getoperand;
          writelastopnd;
          seteffective;
          end;

        bfclr, bfset, bftst:
          begin
          writelastopnd; { The effective address }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits.
              }
            insertobj(((n^.oprnd.offset1 and 37B) * 100B) + (datasize and 37B));
            writebitfield( - 1, n^.oprnd.offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(4000B + ((n^.oprnd.reg and 37B) * 100B) + (datasize and
                      37B));
            writebitfield(n^.oprnd.reg, 0, datasize);
            end;

          getprevoperand(1);
          seteffective;
          end;

        bfexts, bfextu:
          begin
          writelastopnd; { The effective address (leave off comma) }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits,
                "reg" is the source register.
              }
            offset1 := n^.oprnd.offset1;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(((n^.oprnd.reg and 7B) * 10000B) + ((offset1 and
                      37B) * 100B) + (datasize and 37B));
            writebitfield( - 1, offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            register := n^.oprnd.reg;
            getoperand;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(((n^.oprnd.reg and 7B) * 10000B) + 4000B + ((register and
                      37B) * 100B) + (datasize and 37B));
            writebitfield(register, 0, datasize);
            end;

          if switcheverplus[outputmacro] then writech(',');
          writelastopnd; { The register }

            { Back up two operands and output the effective address
              field to the object file. This is neccessary because the effective
              address is the source field in the assembler output, but any
              effect address descriptor words must follow the second word
              of the instruction.
            }
          getprevoperand(2);
          seteffective;
          end;

        bfins:
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          writeopnd; { The register }
          register := n^.oprnd.reg;
          getoperand;
          writelastopnd; { The effective address }
          getoperand;

          if n^.oprnd.m = bit_field_const then
            begin
              { "Len" is the length in bits, "offset1" is the offset in bits,
                "reg" is the source register.
              }
            insertobj(((register and 7B) * 10000B) + ((n^.oprnd.offset1 and
                      37B) * 100B) + (datasize and 37B));
            writebitfield( - 1, n^.oprnd.offset1, datasize);
            end
          else
            begin
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertobj(((register and 7B) * 10000B) + 4000B + ((n^.oprnd.reg and
                      37B) * 100B) + (datasize and 37B));
            writebitfield(n^.oprnd.reg, 0, datasize);
            end;
          getprevoperand(1);
          seteffective;
          end;

        chk:
          begin
          seteffective;
          writeopnd;
          getoperand;
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          writelastopnd;
          insertreghi;
          end;

        clr, neg, negx, notinst, tst:
          begin
          insertsize;
          seteffective;
          writelastopnd;
          end;

        cmpm:
          begin
          if n^.oprnd.m <> autoi then puterror(badsource);
            begin
            insertsize;
            op := (n^.oprnd.reg and 7B) + op;
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> autoi then puterror(badoperand);
            insertreghi;
            end;
          end;

        divs, divu:
          begin
          if datasize <> word then puterror(badsize);
          seteffective;
          writeopnd;
          getoperand;
          writelastopnd;
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          insertreghi;
          end;

        divsl, divul:
          begin
          if datasize <> long then puterror(badsize);
          writeopnd;
          getoperand;
          writelastopnd;

            { reg is the quotient register and indxr is the remainder
              register.  Note: If the quotient and remainder registers
              are the same then only a 32 bit quotient will be generated.
            }
          if n^.oprnd.m = twodregs then
            insertobj((((n^.oprnd.reg and
                      7B) * 10000B) + ord(currinst =
                      divsl) * 4000B) + n^.oprnd.indxr)
          else
            insertobj((((n^.oprnd.reg and
                      7B) * 10000B) + ord(currinst =
                      divsl) * 4000B) + n^.oprnd.reg);

            { Back up one operand and output the effective address field
              to the opject file. This is neccessary because the effective
              address is the source field in the assembler output, but any
              effect address descriptor words must follow the second word
              of the instruction.
            }
          getprevoperand(1);
          seteffective;
          end;

        exg:
          begin
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          writeopnd;
          insertreghi; { assume that this is ok }
          if n^.oprnd.m = dreg then
            begin
            getoperand;
            if (n^.oprnd.m <> dreg) and (n^.oprnd.m <> areg) then
              puterror(baddestination);
            if n^.oprnd.m = dreg then op := op + 100B
            else op := op + 210B;
            insertreglo;
            end
          else
            begin
            if n^.oprnd.m <> areg then puterror(badsource);
            getoperand;
            if n^.oprnd.m = areg then
              begin
              op := op + 110B;
              insertreglo;
              end
            else if n^.oprnd.m = dreg then
              begin
              op := ((op and 7000B) div 1000B {remove high reg}
                    + op + 210B) and 170777B; {put it in lowend}
              insertreghi;
              end
            else puterror(baddestination);
            end;
          writelastopnd;
          end;

        ext, extb:
          begin
          if n^.oprnd.m <> dreg then puterror(missingDreg);
          if datasize = byte then puterror(badsize);

            { The mask is setup for a word to long, if the instruction is
              an EXTB (68020 only) or if this is an extend word to long set
              the correct bits.
            }
          if currinst = extb then
            op := op + 500B { change to byte-to-long form }
          else if datasize = long then op := op + 100B; { change to
                                                          word-to-long form }
          insertreglo;
          writelastopnd;
          end;

        jmp, jsr: { special operands }
          begin
          if n^.kind = oprndnode then
            begin
            writelastopnd;
            if (n^.oprnd.m = usercall) and switcheverplus[outputmacro] then
              begin
              reposition(procnamecolumn);
              writeprocname(n^.oprnd.offset, 100); {write procedure name}
              end;
            seteffective;
            end
          else {must be a labelnode}
            begin
            if switcheverplus[outputmacro] then
              begin
              writech('L');
              writeint(n^.labelno);
              end;

            mode := 71B; {absolute long}
            op := op + mode;
            insertobj(54B * 256 + sectionno[codesect] + 1);
            objtype[objctr] := objforw;
            currentpc := currentpc - 2; {this stuff's longer than code}
            findlabelpc(n^.labelno, isforward);
            relocn[objctr] := true;

            insertobj(labelpc div 16#10000); {high order}
            objtype[objctr] := objoff;
            insertobj(labelpc mod 16#10000); {low order}

            if isforward <> 0 then
              begin
              allocfixup; { generate a new fixupnode }
              fixups[objctr - 1] := fixuptail;
              with fixuptail^ do
                begin
                fixupkind := fixuplabel;
                fixuplen := long;
                fixuplabno := n^.labelno;
                fixupobjpc := fixupobjpc - 4;
                end;
              end;
            objtype[objctr] := objoff;
            end;
          end;

        lea:
          begin
          n1 := nil;
          if n^.kind = oprndnode then
            begin
            seteffective;
            if n^.oprnd.m = usercall then {caused by stuffregisters} n1 := n;
            if n^.oprnd.m in
               [areg, dreg, autoi, autod, immediate, immediatelong] then
              puterror(badoperand);
            writeopnd;
            end
          else
            begin {must be relnode, used only for initial call}
            distancetemp := computedistance;
            op := op + 72B;
            insertobj(distancetemp);

            if switcheverplus[outputmacro] then
              begin
              writech('*');
              writech('+');
              writeint(distancetemp + word);
              writestr('(PC)');
              writech(',');
              end;
            end;
          getoperand;
          writelastopnd;
          if (n1 <> nil) and switcheverplus[outputmacro] then
            begin
            reposition(procnamecolumn);
            writeprocname(n1^.oprnd.offset, 100); {write procedure name}
            end;
          if n^.oprnd.m <> areg then puterror(missingAreg);
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          insertreghi;
          end;

        link:
          begin
          if n^.oprnd.m <> areg then puterror(missingAreg);
          insertreglo; { dynamic link register }
          writeopnd;
          getoperand;

          if not mc68020 then datasize := word; {size operand is only 16 bits
                                                  long}

          writelastopnd; { 68020 long is written here }
          if n^.oprnd.m <> immediate then puterror(baddestination)
          else if n^.oprnd.offset > 0 then puterror(badoffset);
          setmodeonly;
          end;

        movem: buildmovem(false);

        muls, mulu:
          begin
          if mc68020 and (datasize = long) then
            begin
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> dreg then puterror(missingDreg);

            insertobj(((n^.oprnd.reg and
                      7B) * 10000B) + ord(currinst = muls) * 4000B);

              { Back up one operand and output the effective address field
                to the opject file. This is neccessary because the effective
                address is the source field in the assembler output, but any
                effect address descriptor words must follow the second word
                of the instruction.
              }
            getprevoperand(1);
            seteffective;
            end
          else if datasize = word then
            begin
            seteffective;
            writeopnd;
            getoperand;
            writelastopnd;
            if n^.oprnd.m <> dreg then puterror(missingDreg);
            insertreghi;
            end
          else puterror(badsize);
          end;

        pea:
          begin {* * * add control mode only checks * * *}
{**note: genblk fix
            if datasize <> long then puterror(badsize);
}
          seteffective;
          writelastopnd;
          end;

        swap:
          begin
          if n^.oprnd.m <> dreg then puterror(badoperand);
          insertreglo;
          writelastopnd;
          end;

        rte, rts, trapcc, trapv:
        { remember, we did no "getoperand" for these guys } ;

        trap:
          begin
          if (n^.oprnd.m <> immediate) or (n^.oprnd.offset < 0) or
             (n^.oprnd.offset > 15) then
            puterror(badoperand);
          op := op + n^.oprnd.offset;
          writelastopnd;
          end;

        unlk:
          begin
          if n^.oprnd.m <> areg then puterror(missingAreg);
          insertreglo;
          writelastopnd;
          end

        otherwise puterror(unknowninst)
        end; {case inst}
  end {BuildInstruction} ;
{>>>}
{<<<}
procedure PutCode;

{ Output a block of either (or both) macro code or object code.  This simply
  scans the nodes and writes the instructions out.  It is basically a large
  case statement which does the formatting necessary for any unusual
  instructions (of which there are an unfortunate number).
}

  var
    i, j: integer;
    s: longname;
    swbegkludgecount: unsignedint; {Do we need to put out obnoxious 'swbeg'
                                    assembly pseudo-op? Non-zero value = 'yes'}

  begin {PutCode}
    newsection(codesect);
    currentpc := highcode;
    lastobjpc := highcode;

    currnode := 0; { initialize node counter for code generation scan }
    relocn[1] := false; { opcodes do not get relocated }
    fixups[1] := nil;
    currlabel := 1;
    if switcheverplus[outputmacro] then doblocklabel;

    while currnode < lastnode do
      begin
      getnextnode;
      lineerrors := 0;
      instindex := currnode;
      instpc := currentpc;

      if currnode = blocklabelnode then DoBlockEntryCode;

      if switcheverplus[outputmacro] then
        if currnode = labeltable[currlabel].nodelink then writelabels;

      objctr := 0;
      with n^ do
        if kind <> instnode then

          if kind = labeldeltanode then
            begin
            findlabelpc(targetlabel, isforward); {can't be forward}
            labelpctemp := labelpc;
            findlabelpc(tablebase, isforward); {can't be forward}
            insertobj(labelpctemp - labelpc);

            if switcheverplus[outputmacro] then
              begin
              reposition(opcolumn);
              writestr('DC.W');
              reposition(opndcolumn);
              writech('L');
              writeint(targetlabel);
              writestr('-L');
              writeint(tablebase);
              end;

            writeobjline;
            currinst := nop; { to flush nodes to next inst }
            end { labeldeltanode}

          else
            begin
            if kind = stmtref then
              begin
              i := stmtno;
              if i <> 0 then i := i - firststmt + 1;
              if switcheverplus[outputmacro] then
                begin
                if column > 1 then writeline;

                writeln(MacFile, '* Line: ', sourceline - lineoffset: 1,
                        ', Stmt: ', i: 1);
                end;

              if ((switchcounters[debugging] > 0) or
                 (switchcounters[profiling] > 0)) then
                begin
                writemaprecord(stmntrec, flags, sourceline, i);
                end;
              end
            else if kind = datanode then { insert constant data for 68881 }
              begin
              putdata(data div 16#10000);
              putdata(data mod 16#10000);

              if switcheverplus[outputmacro] then
                begin
                reposition(opcolumn);
                writestr('DC.W');
                reposition(opndcolumn);
                writech('$');
                WriteHex(data div 16#10000);
                writech(',');
                writech('$');
                WriteHex(data mod 16#10000);
                writeline;
                end
              end

            else if kind <> errornode then
              begin { HINT: prologuelength may be too small }
              puterror(missinginst);
              if switcheverplus[outputmacro] then dumperrors; { if any }
              end;
            currinst := nop { to flush nodes to next inst }
            end

        else
          begin { save instruction }
          currinst := inst;
          opcount := oprndcount;
          datasize := oprndlength;
          computed_len := computed_length;
          end; { save instruction }

      swbegkludgecount := 0;
      if currinst <> nop then
        begin
        writeinst(currinst);
        if opcount = 0 then { check mnemonic }
          if not (currinst in [rte, rts, trapcc, trapv]) then
            puterror(badopcount)
          else { no operands required }
        else
          begin
          getnextnode;
          if (n^.kind = oprndnode) and (n^.oprnd.m = pcindexed)
          then swbegkludgecount := n^.oprnd.offset2;
          end;

        mode := 0;
        BuildInstruction;

        if computed_len <> (currentpc - instpc) then
          begin
          writeln('Instruction length mismatch, PC=', instpc: - 4, ', pass1=',
                  computed_len: 1, ', pass2=', currentpc - instpc: 1);
          abort(inconsistent);
          end;

        {update op value: may have changed due to operand modes}

        object[1] := op;
        writeobjline;

        if (swbegkludgecount <> 0) and
           (unixtarget in [Nti, VMEV2, NCR, Lmi, UniPlusV2, CTIX, NEC]) and
           switcheverplus[outputmacro]
        then {this is ugly, but we never wanted to do it in the first place}
          begin
          reposition(opcolumn);
          writestr('swbeg');
          reposition(opndcolumn);
          writech('&');
          writeint(swbegkludgecount);
          writeline;
          end;

        end; {inst <> nop}
      end; {while currnode}

    sectionpc[codesect] := currentpc;
    if nowdiagnosing then put_diags;
    highcode := sectionpc[codesect];
  end {PutCode} ;
{>>>}
