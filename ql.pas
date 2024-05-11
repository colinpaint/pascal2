{ ql.pas }
PROGRAM ql (input,output);
{<<<}
const
  maxStringLength = 100;

  symbolNameLength = 10;
  maxHash = 4095;

  esc = 27;
  startBase = %x'400';

  { probably chosen to match ethernet download packet size }
  bytesPerFileRec$ = 1536;
  recsPerFileRec$ = {102} (bytesPerFileRec$ - 4) DIV 15;
{>>>}
const
  moreDebugInfo = false;
{<<<}
type
  byte = 0..255;
  word = -32768..32767;

  symbolNameType = packed array [1..symbolNameLength] of char;

  objBlockType = packed array [0..255] of byte;
  {<<<}
  objRecordType = packed record
    length: integer;
    recordType: char;
    block: objBlockType;
    end;
  {>>>}

  binBlockType = packed array [0..511] of byte;

  referencePtr = ^referenceType;
  {<<<}
  referenceType = packed record
    next : referencePtr;
    symbolName : symbolNameType;
    end;
  {>>>}

  resolvePtr = ^resolveType;
  {<<<}
  resolveType = packed record
    next : resolvePtr;
    addr   : integer;
    offset : integer;
    end;
  {>>>}

  symbolPtr = ^symbolType;
  {<<<}
  symbolType = packed record
    nextSymbol : symbolPtr;
    nextCommon : symbolPtr;
    symbolName : symbolNameType;
    modName    : symbolNameType;
    section    : integer;
    addr       : integer;
    comsize    : integer;
    def        : boolean;
    used       : boolean;
    flagged    : boolean;
    hist       : boolean;
    refList    : referencePtr;
    resList    : resolvePtr;
    end;
  {>>>}

  {<<<}
  milestoneType = record
    intTime   : integer;
    millTime  : integer;
    timeOfDay : packed array [1..11] of char;
    end;
  {>>>}

  { history }
  historyType = ($historyObj, $historySymbol, $historyRef);
  {<<<}
  historyRecordType = record
    CASE historyType : historyType of
      $historyObj    : (obj_addr : integer;);
      $historySymbol : (symbolAddr : integer;
                        symbolName : symbolNameType;);
      $historyRef    : (refAddr: integer;
                        refOffset : integer;);
    end;
  {>>>}

  {<<<}
  fileHistoryRecordType = record CASE boolean of
    true : (numRecs : integer;
            recs    : ARRAY [1 .. recsPerFileRec$] of historyRecordType;);
    false: (dummy   : packed array [1 .. bytesPerFileRec$] of char);
    end;
  {>>>}
  historyFileType = FILE of fileHistoryRecordType;
{>>>}
{<<<}
var
  { switches }
  modules, download, check, bell: boolean;
  xref, map, bin, out, symout: boolean;
  debugInfo, chat, logging, friendly, quiet: boolean;
  files, history, escape: boolean;

  { command line }
  cmd: packed array [0..100] of char;
  cmdLen: word;
  cmdString: string;
  cmdFileRootString: string;
  cmdFileExtString: string;
  cmdFileNameString: string;

  curFileNameString: string;
  fileIdString: string;

  { files }
  cmdFile: text;
  objFile : FILE of byte;
  textObjFile: text;
  binaryFile: FILE of binBlockType;
  downloadTargetFile: FILE of binBlockType;
  targetFile: text;
  srFormatFile: text;
  logFile: text;
  moduleFile: text;

  pass: integer;

  { obj file input }
  objBlock: objBlockType;

  objRecordBlockIndex: integer;
  objRecord: objRecordType;

  duffer: boolean;
  inpacket: boolean;
  usingHistory: boolean;

  prevCommon: symbolPtr;
  commonHead: symbolPtr;

  { symbols }
  numSymbols: integer;
  numUndefinedSymbols: integer;
  hashTable: array [0..maxHash] of symbolPtr;

  { sections }
  codestart: integer;
  codelen: integer;
  topESD: integer;

  userbase: array [-1..15] of integer;
  sectbase: array [-1..15] of integer;
  baseaddr: array [-1..15] of integer;
  sbase:array [-1..15] of integer;

  esdSymbolArray: array [0..255] of symbolPtr;
  outAddrArray: array [0..255] of integer;
  esdArray: array [0..255] of integer;

  codeArray: array [1..64] of integer;

  { timing }
  startLinkMilestone, endPass1Milestone, endPass2Milestone, endLinkMilestone: milestoneType;
  endMapGenMilestone, endHisGenMilestone, endSymGenMilestone: milestoneType;
  endSpaceAllocMilestone, endXrefGenMilestone: milestoneType;
  startReadHisMilestone, endReadHisMilestone: milestoneType;

  modName: symbolNameType;

  { .bin file output }
  binBlockIndex: integer;
  binBlock: binBlockType;

  outputMaxSize: integer;
  outputChecksum: integer;

  i, total, basepos: integer;
  datestring: packed array [1..11] of CHAR;
{>>>}

  {<<<  bit utils}
  {<<<}
  function ior (i1, i2: integer): integer;

  begin
    ior := ord (uor (uint (i1), uint (i2)));
  end;
  {>>>}
  {<<<}
  function ixor (i1, i2: integer): integer;

  begin
    ixor := ord (uxor (uint(i1), uint(i2)));
  end;
  {>>>}
  {<<<}
  function iand (i1, i2: integer): integer;

  begin
    iand := ord (uand (uint (i1), uint (i2)));
  end;
  {>>>}

  {<<<}
  function mvl (i: integer): integer;

  begin
    mvl := i*256;
  end;
  {>>>}
  {<<<}
  function mvr (i: integer): integer;

  begin
    if i < 0 then
      mvr := int (uor (%x'800000', uint (mvr (int (uand (%x'7FFFFFFF', uint(i)))))))
    else
      mvr := (i DIV 256) mod (%x'1000000');
  end;
  {>>>}
  {>>>}
  {<<<  char utils}
  {<<<}
  function null (ch: char): boolean;

  begin
    null := (ch = chr(13)) OR (ch = chr(10)) OR (ch = ' ') OR (ch = chr(9)) OR (ch = chr(0));
  end;
  {>>>}
  {<<<}
  function digit (ch: char): boolean;

  begin
    digit := (ch >= '0') and (ch <= '9');
  end;
  {>>>}
  {<<<}
  function upper (ch: char): boolean;

  begin
    upper := (ch >= 'A') and (ch <= 'Z');
  end;
  {>>>}
  {<<<}
  function alpha (ch: char): boolean;

  begin
    alpha := ((ch >= 'a') and (ch <= 'z')) or ((ch >= 'A') and (ch <= 'Z'));
  end;
  {>>>}
  {<<<}
  function alphaDigit (ch: char): boolean;

  begin
    alphaDigit := digit (ch) or alpha (ch) or (ch = '_');
  end;
  {>>>}
  {<<<}
  function fileNameChar (ch: char): boolean;

  begin
    fileNameChar := alphaDigit (ch) or (ch = '.') or (ch = '/');
  end;
  {>>>}

  {<<<}
  function toHex (ch: char): integer;

  begin
    if (ch >= '0') and (ch <= '9') then
      toHex := ord(ch) - ord('0')
    else if (ch >= 'a') and (ch <= 'f') then
      toHex := ord(ch) - ord('a') + 10
    else if (ch >= 'A') and (ch <= 'F') then
      toHex := ord(ch) - ord('A') + 10
    else
      begin
      writeln ('Duff char ', ch,'when hex char expected!');
      toHex := 0;
      end;
  end;
  {>>>}
  {<<<}
  function toLower (ch: char): char;

  begin
    if upper (ch) then
      toLower := chr(ord(ch) + ord('a') - ord('A'))
    else
      toLower := ch;
  end;
  {>>>}

  {<<<}
  procedure copySubString (var d: string; var s: string; start, span: integer);

  var
    i: integer;

  begin
    {writeln ('copySubString ', s, ' ', start:0, ' ', span:0);
    }

    { negative span ? }
    if span < 0 then
      begin
      span := -span;
      start := start - span
      end;

    { constrain start and span to be within s }
    if start < 1 then
      begin
      span := span + start - 1;
      start := 1
      end;
    if start + span - 1 > s.length then
      span := s.length - start + 1;

    for i := 1 to span do
      d[i] := s[start + i - 1];

    d[span+1] := 0;
  end;
  {>>>}
  {<<<}
  procedure concatStrings (var d: string; var s1: string; var s2: string);

  var
    i, len1: integer;

  begin
    len1 := s1.length;

    for i := 1 to len1 do
      d[i] := s1[i];

    for i := 1 to s2.length do
      d[len1 + i] := s2[i];

    d[len1 + s2.length + 1] := 0;
  end;
  {>>>}
  {>>>}
  {<<<  file utils}
  {<<<}
  procedure getFileStrings (var fileName: string; var defaultExt: string;
                            var root: string; var ext: string; var fullFileName: string);
  var
    i: integer;

  begin
    if moreDebugInfo then
      writeln ('getFileStrings fileName:', fileName, ' len:', fileName.length:0,
               ' defaultExt:', defaultExt, ' len:', defaultExt.length:0);


    { search backwards, to find first non-alpha char }
    i := fileName.length;
    while (i > 0) and alphaDigit (fileName[i]) do
      i := i - 1;

    { first non alpha is extension . }
    if (i > 1) and (fileName[i] = '.') then
      begin
      { writeln ('has ext:', i:0, ' of:', fileName.length:0);
      }
      copySubString (root, fileName, 1, i - 1);
      copySubString (ext, fileName, i, fileName.length - i + 1);
      copySubString (fullFileName, fileName, 1, fileName.length);
      end
    else
      begin
      if moreDebugInfo then
        writeln ('no ext:', i:0, ' of:', fileName.length:0);
      copySubString (root, fileName, 1, fileName.length);
      copySubString (ext, defaultExt, 1, defaultExt.length);
      concatStrings (fullFileName, fileName, defaultExt);
      end;
  end;
  {>>>}

  { logging file }
  {<<<}
  procedure openloggingFile;

  var
    fullFileName: string;

  begin
    concatStrings (fullFileName, cmdFileRootString, '.log');
    rewrite (logFile, fullFileName);
    writeln (logFile, 'Linking from ', cmdFileNameString);
  end;
  {>>>}
  {<<<}
  procedure closeloggingFile;

  var
    total, i:integer;
    datestring: packed array [1..11] of CHAR;

  begin
    if friendly then
      begin
      writeln (logFile);

      total := 0;
      if sectbase[8] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of P                 (8)  = ', sectbase[8]:8, ' bytes');
        total := total + sectbase[8];
        end;
        {>>>}
      if sectbase[9] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of HELP              (9)  = ', sectbase[9]:8, ' bytes');
        total := total + sectbase[9];
        end;
        {>>>}
      if sectbase[12] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of error messages   (12)  = ', sectbase[12]:8, ' bytes');
        total := total + sectbase[12];
        end;
        {>>>}
      if sectbase[13] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of code & constants (13)  = ', sectbase[13]:8, ' bytes');
        total := total + sectbase[13];
        end;
        {>>>}
      if sectbase[14] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of diagnostic block (14)  = ', sectbase[14]:8, ' bytes');
        total := total + sectbase[14];
        end;
        {>>>}
      if sectbase[15] <> 0 then
        {<<<}
        begin
        writeln (logFile, 'Size of global variables (15)  = ', sectbase[15]:8, ' bytes');
        total := total + sectbase[15];
        end;
        {>>>}

      writeln (logFile, 'Total size                     = ', total:8, ' bytes');
      end

    else for i:=0 TO 15 DO
      if sectbase[i] <> 0 then
        writeln (logFile, 'section:', i:2,
                          ' start:',  hex (baseaddr[i], 6, 6),
                          ' length:', hex (sectbase[i], 6, 6),
                          ' finish:', hex (baseaddr[i] + sectbase[i], 6, 6));

    date (datestring);
    writeln (logFile);
    writeln (logFile, 'Link started ', startLinkMilestone.timeOfDay, ' ', datestring);
    writeln (logFile, 'Link ended   ', endLinkMilestone.timeOfDay, ' ', datestring);
    writeln (logFile, 'total CPU time:- ',(endLinkMilestone.millTime - startLinkMilestone.millTime)/1000:7:2);

    close (logFile);
  end;
  {>>>}

  { .bin binary output file }
  {<<<}
  procedure openOutput;

  var
    fullFileName: string;

  begin
    binBlockIndex := 0;
    inpacket := false;

    if bin then
      begin
      if download then
        begin
        writeln ('Downloading binary file - target.txt');
        rewrite (downloadTargetFile, 'target.txt');
        end;

      if out then
        begin
        concatStrings (fullFileName, cmdFileRootString, '.bin');

        if chat OR debugInfo OR (NOT quiet) then
          writeln ('Making binary file ', fullFileName);
        if logging then
          writeln (logFile, 'Making binary file ', fullFileName);
        rewrite (binaryFile, fullFileName);
        end;
      end

    else
      begin
      if download then
        begin
        writeln ('Downloading SR file - target.txt');
        rewrite (targetFile, 'target.txt');
        end;

      if out then
        begin
        concatStrings (fullFileName, cmdFileRootString, '.sr');
        writeln ('Making SR file ', fullFileName);
        rewrite (srFormatFile, fullFileName);
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure binByte (b: byte);

  begin
    binBlock[binBlockIndex] := b;
    binBlockIndex := binBlockIndex + 1;

    if binBlockIndex > 511 then
      begin
      if out then
        write (binaryFile, binBlock);
      if download then
        write (downloadTargetFile, binBlock);

      binBlockIndex := 0;
      end;

  end;
  {>>>}
  {<<<}
  procedure sendBin (b: byte);

  begin
    if (b = esc) AND (escape = true) then
      binbyte (b);

    binbyte (b);
  end;
  {>>>}
  {<<<}
  procedure sendsfnewline;

  begin
    if download then
      writeln (targetFile);
    if out then
      writeln (srFormatFile);
  end;
  {>>>}
  {<<<}
  procedure sendsform (var s: string);

  begin
    if out then
      write (srFormatFile,s);
    if download then
      write (targetFile,s);
  end;
  {>>>}
  {<<<}
  procedure wbyte (b: byte);

  var
    s: string;

  begin
    if bin then
      begin
      sendbin (b);
      outputChecksum := ixor (outputChecksum,b);
      end
    else
      begin
      s := hex (b,2,2);
      sendsform (s);
      outputChecksum := outputChecksum + b;
      end;
  end;
  {>>>}
  {<<<}
  procedure endPacket;

  var
    s:string;

  begin
    if bin then
      sendbin (outputChecksum)
    else
      begin
      s := hex (255 - (outputChecksum MOD 256), 2, 2);
      sendsform (s);
      sendsfnewline;
      end;
  end;
  {>>>}
  {<<<}
  procedure sendStop;

  var
    endstr : string;
    I : integer;

  begin
    if bin then
      BEGIN
      outputChecksum := 0;
      binbyte (esc);
      binbyte (0);
      wbyte (2);
      wbyte (0);
      wbyte (4);

      for i := 1 TO 4 DO
        wbyte(0);
      endPacket;

      for i := 0 TO 511 DO
        binbyte (0);
      end

    else
      begin
      endstr := 'S9030000FC';
      sendsform (endstr);
      sendsfnewline;
      end;
  end;
  {>>>}
  {<<<}
  procedure closeOutput;

  begin
    if inpacket then
      endpacket;

    sendStop;

    if bin then
      begin
      if download then
        close (downloadTargetFile);
      if out then
        close (binaryFile);
      end
    else
      begin
      if download then
        close (targetFile);
      if out then
        close (srFormatFile);
      end;
  end;
  {>>>}

  { dump to file }
  {<<<}
  procedure dumpSymbols;

  var
    i: integer;
    symbol: symbolPtr;

    symBlockIndex: integer;
    symBlock: objBlockType;
    symbolTableFile: file of objBlockType;

    fullFileName: string;

    {<<<}
    procedure pbyte (b: byte);

    begin
      symBlock[symBlockIndex] := b;
      symBlockIndex := symBlockIndex + 1;

      if symBlockIndex > 255 then
        begin
        write (symbolTableFile, symBlock);
        symBlockIndex := 0;
        end;
    end;
    {>>>}
    {<<<}
    procedure outdata (s: string);

    var
      i:integer;

    begin
      pbyte (s.length);
      for i := 1 TO s.length DO
        pbyte (ord(s[i]));
    end;
    {>>>}
    {<<<}
    function binInt (val: integer): string;

    var
      intString: string;
      i: integer;

    begin
      for i := 4 downto 1 DO
        begin
        intString[i] := chr (val);
        val := mvr (val);
        end;

      binInt := intString;
    end;
    {>>>}
    {<<<}
    procedure outSymbol (symbol: symbolPtr);

    begin
      WITH symbol^ DO
        begin
        outdata ('1' + modName + 'VRLvvvvuuccccccccffffffffxxtttddd');
        outdata ('2' + chr(%x'50') + symbolName + binInt (addr + baseaddr[section]));
        end;
    end;
    {>>>}

  begin
    symBlockIndex := 0;

    concatStrings (fullFileName, cmdFileRootString, '.sym');
    rewrite (symbolTableFile, fullFileName);

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        symbol := hashTable[i];
        repeat
          outSymbol (symbol);
          symbol := symbol^.nextSymbol;
          until symbol = nil;
        end;

      outdata ('4' + chr(17) + binInt(0)); { module end record }
      if symBlockIndex > 0 then
        begin
        for i := symBlockIndex TO 255 DO
          symBlock[i] := 0;
        write (symbolTableFile, symBlock);
        end;

    close (symbolTableFile);
  end;
  {>>>}
  {<<<}
  procedure dumpSymbolMap;

  var
    i:integer;
    symbol: symbolPtr;
    map_file : text;
    fullFileName: string;

  begin
    concatStrings (fullFileName, cmdFileRootString, '.map');
    rewrite (map_file, fullFileName);

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        if debugInfo then
          writeln (map_file, i, ':');
        symbol := hashTable[i];

        repeat
          write (map_file,symbol^.symbolName,' ',
          hex (symbol^.addr+baseaddr[symbol^.section], 6, 6),' ',symbol^.modName);
          if symbol^.comsize<>-1 then
            writeln (map_file,' C:', hex (symbol^.comsize, 4, 4))
          else
            if NOT symbol^.def then
              writeln (map_file,' Undef!')
            else
              writeln (map_file);
          symbol := symbol^.nextSymbol;
          until symbol = nil;
      end;

    close (map_file);
  end;
  {>>>}
  {<<<}
  procedure dumpXreferences;
  { dump cross references to file from list held per symbol }

  var
    i,refcount: integer;
    symbol: symbolPtr;
    ref_file: text;
    r: referencePtr;
    fullFileName: string;

  begin
    concatStrings (fullFileName, cmdFileRootString, '.xrf');
    rewrite (ref_file, fullFileName);

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        if debugInfo then
          writeln (ref_file, i, ':');
        symbol := hashTable[i];
        REPEAT
          refcount:=0;
          write (ref_file,symbol^.symbolName,' ',
                 hex (symbol^.addr + baseaddr[symbol^.section], 6, 6),' ', symbol^.modName);
          if symbol^.comsize<>-1 then
            writeln (ref_file,' C:',hex(symbol^.comsize, 4, 4))
          else
            if NOT symbol^.def then
              writeln (ref_file,' Undef!')
            else
              writeln (ref_file);

          if symbol^.refList<>nil then
            begin
            r := symbol^.refList;
            REPEAT
              write (ref_file,r^.symbolName, '    ');
              refcount := refcount + 1;
              if refcount MOD 6 = 0 then
                writeln (ref_file);
              r := r^.next;
              until r = nil;
            if refcount MOD 6 <> 0 then
              writeln (ref_file);
          end
        else
          writeln (ref_file, 'Not referenced ANYWHERE!');

        writeln
          (ref_file,'--------------------------------------------------------------------------');
        symbol := symbol^.nextSymbol;
        until symbol = nil;
      end;

    close (ref_file);
  end;
  {>>>}
  {<<<}
  procedure dumpHistory;
  { disp history and readHistory must have match in file format }

  var
    i, rescount: integer;
    symbol: symbolPtr;
    r : resolvePtr;
    historyFile : historyFileType;
    historyRecord : historyRecordType;
    fileHistoryRecord : fileHistoryRecordType;
    fullFileName: string;

    {<<<}
    procedure writeHistoryFile (historyRecord: historyRecordType);

    begin
      fileHistoryRecord.numRecs := fileHistoryRecord.numRecs + 1;
      fileHistoryRecord.recs[fileHistoryRecord.numRecs] := historyRecord;

      if fileHistoryRecord.numRecs = recsPerFileRec$ then
        begin
        Write (historyFile, fileHistoryRecord);
        fileHistoryRecord.numRecs := 0;
        end;
    end;
    {>>>}

  begin
    concatStrings (fullFileName, cmdFileRootString, '.his');
    rewrite (historyFile, fullFileName);
    fileHistoryRecord.numRecs := 0;

    historyRecord.historyType := $historyObj;
    historyRecord.obj_addr := basepos;

    { Write (historyFile, historyRecord); }
    writeHistoryFile (historyRecord);

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        symbol := hashTable[i];

        repeat
          begin
          if symbol^.comsize = -1 then { dont dump commons in history }
            begin
            historyRecord.historyType := $historySymbol;
            historyRecord.symbolAddr := symbol^.addr+baseaddr[symbol^.section];
            historyRecord.symbolName := symbol^.symbolName;
            {historyRecord.mod_name := symbol^.modName;}
            {Write (historyFile, historyRecord);}
            writeHistoryFile (historyRecord);

            if symbol^.resList <> nil then
              begin
              r := symbol^.resList;
              repeat
                begin
                historyRecord.historyType := $historyRef;
                historyRecord.refAddr := r^.addr;
                historyRecord.refOffset := r^.offset;
                {Write (historyFile, historyRecord);}
                writeHistoryFile (historyRecord);
                r := r^.next;
                end until r = nil;
              end;

            end;

          symbol := symbol^.nextSymbol;
          end until symbol = nil;

        end;

    { Send the last one }
    if fileHistoryRecord.numRecs > 0 then
      Write (historyFile, fileHistoryRecord);

    close (historyFile);
  end;
  {>>>}

  { .mod module file }
  {<<<}
  procedure openModules;

  var
    fullFileName: string;

  begin
  if modules then
    begin
    concatStrings (fullFileName, cmdFileRootString, '.mod');
     rewrite (moduleFile, fullFileName);
    end;
  end;
  {>>>}
  {<<<}
  procedure closeModules;

  begin
  if modules then
    close (moduleFile);
  end;
  {>>>}
  {>>>}

  {<<<}
  function getByte: byte;

  begin
    getByte := objRecord.block[objRecordBlockIndex];
    objRecordBlockIndex := objRecordBlockIndex + 1;
  end;
  {>>>}
  {<<<}
  function getInt: integer;

  var
    i, j: integer;

  begin
    i := 0;
    for j := 1 TO 4 DO
      i := mvl (i) + getByte;

    getInt := i;
  end;
  {>>>}
  {<<<}
  function getSymbolName: symbolNameType;

  var
    i: integer;
    symbolName: symbolNameType;

  begin
    for i := 1 TO symbolNameLength DO
      symbolName[i] := chr(getByte);

    getSymbolName := symbolName;
  end;
  {>>>}

  {<<<}
  function symbolHash (var symbolName: symbolNameType): integer;

  var
    i: integer;
    hash: integer;

  begin
    hash := 0;
    i := 1;
    while (i <= symbolNameLength) and (symbolName[i] <> ' ') do
      begin
      hash := hash * 97 + ord (symbolName[i]);
      i := i + 1;
      end;

    symbolHash := uint(hash) mod maxHash;
  end;
  {>>>}

  {<<<}
  function getMilestone: milestoneType;

  var
    ms: milestoneType;
    temp: integer;

    {<<<}
    function getNum (startch: integer): integer;

    var
      temp1, temp2: integer;

    begin
      temp1 := ORD(ms.timeOfDay[startch]) - ORD('0');
      temp2 := ORD(ms.timeOfDay[startch + 1] ) - ORD('0');
      getNum := (temp1 * 10) + temp2;
    end;
    {>>>}

  begin
    time (ms.timeOfDay);

    { ms.timeOfDay is hh:mm:ss.cc }
    {                   12 45 78 AB }
    ms.intTime := 0;

    temp := getNum (1); {hh}
    ms.intTime := temp;

    temp := getNum (4); {mm}
    ms.intTime := ms.intTime * 60 + temp;

    temp := getNum (7); {ss}
    ms.intTime := ms.intTime * 60 + temp;

    temp := getNum (10); {cc}
    ms.intTime := ms.intTime * 100 + temp;

    getMilestone := ms;
  end;
  {>>>}
  {<<<}
  procedure clearMilestone (var ms: milestoneType);

  begin
    ms.millTime := 0;
    ms.intTime := 0;
    ms.timeOfDay := '           ';
  end;
  {>>>}
  {<<<}
  procedure showMilestone (s: string; milestone1, milestone2: milestoneType);

  var
    i, diff, cc, ss, mm, hh: integer;
    timeString: packed array [1..11] of char;

  begin
    diff := milestone1.intTime - milestone2.intTime;

    cc := diff MOD 100;
    diff := diff DIV 100;
    ss := diff MOD 60;

    diff := diff DIV 60;
    mm := diff MOD 60;

    diff := diff DIV 60;
    hh := diff MOD 60;

    write (s);
    write (milestone1.timeOfDay,' ',(milestone1.millTime-milestone2.millTime) / 1000:7:2);
    write (hh:2, ':', mm:2, ':', ss:2, '.', cc:2 );

    if endLinkMilestone.millTime - startLinkMilestone.millTime > 0 then
      write ('  ', ((milestone1.millTime - milestone2.millTime) * 100) /
                    (endLinkMilestone.millTime - startLinkMilestone.millTime):7:2,'%');
    writeln;
  end;
  {>>>}

  {<<<}
  procedure showModName;

  begin
    writeln ('in module:', modName, ' from file:', curFileNameString);
    if logging then
      writeln (logFile, 'in module:', modName, ' from file:', curFileNameString);
  end;
  {>>>}

  {<<<}
  function findInsert (var symbolName: symbolNameType; var symbol: symbolPtr; ins: boolean): boolean;
  var
    found : boolean;
    hash : integer;

    {<<<}
    procedure forceUpper (var s: symbolNameType);

    var
      i:integer;

    begin
      for i := 1 TO 10 DO
        if (s[i] >= 'a') AND (s[i] <= 'z') then
          s[i] := chr (ord (s[i]) + ord('A') - ord('a'));
    end;
    {>>>}

  begin
    forceUpper (symbolName);

    hash := symbolHash (symbolName);
    symbol := hashTable[hash];

    found := false;
    while (NOT found) AND (symbol <> nil) DO
      begin
      if symbol^.symbolName = symbolName then
        found := true
      else
        symbol := symbol^.nextSymbol;
      end;

    findInsert := found;

    if (NOT found) AND ins then
      begin
      numSymbols := numSymbols + 1;

      new (symbol);
      symbol^.nextSymbol := hashTable[hash];
      hashTable[hash] := symbol;

      symbol^.nextCommon := nil;
      symbol^.symbolName := symbolName;
      symbol^.modName := modName;
      symbol^.section := 0;
      symbol^.addr := -1;
      symbol^.comsize := -1;
      symbol^.def := false;
      symbol^.used := true;
      symbol^.flagged := false;
      symbol^.hist := false;
      symbol^.refList := nil;
      symbol^.resList := nil;
      end;
  end;
  {>>>}
  {<<<}
  procedure allocCom;

  var
    symbol : symbolPtr;

  begin
    symbol := commonHead;
    while symbol <> nil DO
      begin
      symbol^.addr := sectbase[symbol^.section];
      sectbase[symbol^.section] := sectbase[symbol^.section] + symbol^.comsize;

      if odd(sectbase[symbol^.section]) then
        sectbase[symbol^.section] := sectbase[symbol^.section]+1;

      symbol := symbol^.nextCommon;
      end;
  end;
  {>>>}
  {<<<}
  procedure addRes (symbol: symbolPtr; addr, offset : integer);
  { add a resolved symbol reference to list held per symbol }

  var
    resolve: resolvePtr;

  begin
    new (resolve);
    resolve^.next := symbol^.resList;
    resolve^.addr := addr;
    resolve^.offset := offset;
    symbol^.resList := resolve;
  end;
  {>>>}
  {<<<}
  procedure overlapCheck;

  var
    i, j: integer;

    {<<<}
    function clash (i, j: integer):boolean;

    begin
      clash := NOT (((sectbase[i] + baseaddr[i]) <= baseaddr[j]) OR
                    ((sectbase[j] + baseaddr[j]) <= baseaddr[i]));
    end;
    {>>>}

  begin
    for i := 0 TO 14 DO
      for j := i+1 TO 15 DO
        if clash (i,j) then
          writeln ('Sections', i:2,' and ', j:2, ' overlap!');
  end;
  {>>>}
  {<<<}
  procedure checkUndefinedSymbols;

  var
    i:integer;
    symbol: symbolPtr;

  begin
    writeln ('Undefined symbols:-');
    if logging then
      writeln (logFile, 'Undefined symbols:-');

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        symbol := hashTable[i];

        repeat
          if NOT symbol^.def then
            begin
            writeln (symbol^.symbolName,' first referenced in module ', symbol^.modName);
            if logging then
              writeln (logFile, symbol^.symbolName, ' first referenced in module ', symbol^.modName);
            symbol^.section := -1;
            symbol^.addr := %X'FAFF';
            end;

          symbol := symbol^.nextSymbol;
          until symbol = nil;
        end;
  end;
  {>>>}
  {<<<}
  procedure doubleDef (symbol: symbolPtr);

  begin
    showModName;

    writeln ('Doubly defined label  ', symbol^.symbolName);
    writeln ('Previously defined in module ', symbol^.modName);

    if logging then
      begin
      writeln (logFile, 'Doubly defined label  ', symbol^.symbolName);
      writeln (logFile, 'Previously defined in module ', symbol^.modName);
      end;

    symbol^.flagged := true;
  end;
  {>>>}
  {<<<}
  procedure readHistory (fileName: string);
  { disp history and readHistory must have match in file format }

  var
    symbol: symbolPtr;
    resolve: resolvePtr;

    fileHistoryRecord: fileHistoryRecordType;
    historyFile: historyFileType;

  begin
    symbol := nil;

    reset (historyFile, fileName);

    while NOT eof (historyFile) DO
      begin
      Read (historyFile, fileHistoryRecord);

      for i := 1 TO fileHistoryRecord.numRecs DO
        WITH fileHistoryRecord.recs[i] DO
          CASE historyType of
            $historyObj :
              basepos := obj_addr;

            $historySymbol :
              begin
              if findInsert (symbolName, symbol, true) then;
              symbol^.hist := TRUE;
              symbol^.modName := 'patched!!!';
              symbol^.section := -1;
              symbol^.def := true;
              symbol^.addr := symbolAddr;
              end;

            $historyRef :
              begin
              new (resolve);
              resolve^.next := symbol^.resList;
              resolve^.addr := refAddr;
              resolve^.offset := refOffset;
              symbol^.resList := resolve;
              end;
            end;

      end;

    close (historyFile);
  end;
  {>>>}

  {<<<}
  procedure switchSettingsProcess (var s: string);

  var
    i, j, l: integer;
    slashfound: boolean;
    lineBuffer: packed array [1..maxStringLength+1] of char; {extra byte to stop bound overflow!}

    {<<<}
    procedure doSwitch (start, switchLen: integer);
    { processes a switch setting directive }

    var
      c : char;
      noflag : boolean;
      tla : packed array [1..3] of char;
      i: integer;
      pos, endPos, switchEndPos: integer;
      section : integer;

      {<<<}
      function getNextCh: char;

      begin
        getNextCh := lineBuffer [pos];
        if pos < switchLen then
          pos := pos + 1;
      end;
      {>>>}
      {<<<}
      procedure setSwitch (var b: boolean);

      BEGIN
        b := NOT noflag;
      end;
      {>>>}

    begin
      for i := 1 TO switchLen DO
        lineBuffer[i] := toLower (lineBuffer[i]);

      pos := start;
      repeat
        noflag := false;
        tla[1] := getNextCh;
        tla[2] := getNextCh;
        tla[3] := '.';
        if tla = 'no.' then
          {<<<  no.}
          begin
          noflag := true;
          tla[1] := getNextCh;
          tla[2] := getNextCh;
          end;
          {>>>}

        tla[3] := getNextCh;
        switchEndPos := pos;

        { skip to next switch }
        repeat
          c := getNextCh;
          until (c='/') OR (null (c)) OR (pos >= switchLen);

        if (tla[1] = 'o') AND (tla[2] >= '0') AND (tla[2] <= '9') then
          {<<<  set section startAddress}
          begin
          if tla[3] = ':' then
            section := ord (tla[2]) - ord('0')
          else
            begin
            section := 10 * (ord(tla[2]) - ord('0')) + (ord(tla[3]) - ord('0'));
            switchEndPos := switchEndPos + 1;
            end;

          if pos <> switchLen then
            endPos := pos - 2
          else
            endPos := pos;

          if (section >= 0) AND (section <= 15) then
            begin
            userbase[section] := 0;
            for i := switchEndPos TO endPos DO
              userbase[section] := 16 * userbase[section] + toHex (lineBuffer[i]);
            if debugInfo then
              writeln (hex (userbase[section], 6, 6), ' ', section);
            end
          else
            writeln (' Illegal section number in switch ', tla);
          end
          {>>>}
        else if (tla = 'xre') OR (tla = 'xrf') then
          setSwitch (xref) { generate xref file }
        else if tla = 'map' then
          setSwitch (map)  { generate map file}
        else if tla = 'sym' then
          setSwitch (symout) { generate symbol file}
        else if tla = 'bin' then
          setSwitch (bin)  { binary output}
        else if tla = 'mod' then
          setSwitch (modules) { module list}
        else if tla = 'deb' then
          setSwitch (debugInfo) { debugInfo mode}
        else if (tla = 'dld') OR (tla = 'dow') then
          setSwitch (download) { download to target}
        else if tla = 'out' then
          setSwitch (out) { generate any output at all!}
        else if tla = 'cha' then
          setSwitch (chat) { generate loads of output }
        else if tla = 'qui' then
          setSwitch (quiet) { generate minimum output}
        else if tla = 'eng' then
          setSwitch (friendly) { say understandable things}
        else if tla = 'log' then
          {<<<  generate .log file}
          begin
          setSwitch (logging);
          openloggingFile;
          end
          {>>>}
        else if tla = 'fil' then
          setSwitch (files) { generate put fileNames in mod file }
        else if tla = 'his' then
          setSwitch (history) { generate history file }
        else if tla = 'bel' then
          {<<<  bells}
          begin
          { generate bells at end of link }
          setSwitch (bell);
          if noflag then
            writeln ('What do you want, a prize or something??');
          end
          {>>>}
        else if tla = 'che' then
          setSwitch (check) { check all possible grubbies }
        else if tla = 'esc' then
          setSwitch (escape) { replace all 1B's in code with 1B1B }
        else
          writeln ('Unknown switch :', tla);
      until (pos >= switchLen) OR (null (c));
    end;
    {>>>}

  begin
    Writeln ('switchSettingsProcess ', s);

    l := s.length;
    i := 1;
    slashfound := false;

    while (i < l) AND NOT slashfound DO
      if s[i] = '/' then
        slashfound := true
      else
        i := i + 1;

    if slashfound then
      begin
      for j := i + 1 TO l DO
        lineBuffer[j-i] := s[j]; { pick up everything after the slash }

      doSwitch (1, l-i);
      if i = 1 then
        s := ''
      else
        copySubString (s, s, 1, i-1);
      end;
  end;
  {>>>}

  {<<<}
  procedure getObjFileName (var fileName: string);

  var
    line: string;
    lineIndex: integer;
    lineLength: integer;
    fileNameIndex: integer;
    objFileFound: boolean;

  begin
    objFileFound := false;

    fileNameIndex := 1;
    repeat
      readln (cmdFile, line);
      lineIndex := 1;
      lineLength := line.length;

      if line[lineIndex] = '!' then
        Writeln ('comment - ', line)
      else if line[lineIndex] = '@' then
        Writeln ('@ in .cmd not implemented')
      else if line[lineIndex] = '/' then
        begin
        if pass = 1 then
          switchSettingsProcess (line);
        end
      else
        begin
        objFileFound := true;
        while ((lineIndex <= lineLength) and fileNameChar (line[lineIndex])) do
          begin
          { copy obj fileName from .cmd file line }
          fileName[fileNameIndex] := line[lineIndex];
          fileNameIndex := fileNameIndex + 1;
          lineIndex := lineIndex + 1;
          end;
        end
    until objFileFound or eof (cmdFile);

    { terminate string with 0 }
    fileName[fileNameIndex] := 0;
  end;
  {>>>}
  {<<<}
  procedure getObjRecord (var objRecord: objRecordType);
  { .ro files are 256 byte fixed size blocks. Within these blocks, records are
    packed end to end i.e. one record can span a block boundary. Each record
    conisits of a single byte <n> followed by <n> data bytes
  }
  var
    b: byte;
    i: integer;
    numBytes: integer;

  begin
    read (objFile, b);
    objRecord.length := b;

    read (objFile, b);
    objRecord.recordType := chr(b);
    if moreDebugInfo then
      Writeln ('getObjRecord len:', objRecord.length:0, ' type:', objRecord.recordType);

    for i := 0 TO objRecord.length-2 do
      begin
      read (objFile, b);
      objRecord.block[i] := b;
      if moreDebugInfo then
        Write (hex (objRecord.block[i]));
      end;

    if moreDebugInfo then
      Writeln;
  end;
  {>>>}
  {<<<}
  procedure getTextRec (var o: objRecordType);
  { .rx files are a text version of the .ro file. Each record is a single line
    of text, written out as hex characters i.e. 2 characters per byte. The record
    length is derived from the bytes on the line. This format is provided to allow
    .rx files to be easily ported from other systems e.g. Unix
  }
  var
    bytes, i: integer;
    textObjRecord: string;

  begin
    readln (textObjFile, textObjRecord);

    bytes := textObjRecord.length DIV 2;
    objRecord.length := bytes;

    for i := 1 TO bytes DO
      objRecord.block[i] := chr ((toHex (textObjRecord[i*2-1]) * 16) + toHex (textObjRecord[i*2]));
  end;
  {>>>}

  {<<<}
  procedure processModuleId;

  var
    section: integer;

  begin
    topESD := 17;
    esdArray[0] := 0;  {unused esd value}

    objRecordBlockIndex := 0;
    modName := getSymbolName;

    if chat OR debugInfo then
      writeln ('Pass ', pass:0, ' of ', modName);

    { we need to init these esd values, in case of zero length sections}
    if pass = 2 then
      begin
      if modules then
        begin
        write (moduleFile, modName, ':');
        if files then
          write (moduleFile, fileIdString, ':' );
        end;

      for section := 0 TO 15 DO
        begin
        esdArray[section+1] := baseaddr[section] + sbase[section];
        esdSymbolArray[topESD] := NIL;
        outAddrArray[section+1] := esdArray[section+1];
        end;
      end;
  end;
  {>>>}
  {<<<}
  procedure pass1;

  var
    fileName, root, ext, fullFileName: string;

    {<<<}
    function processRecord: boolean;
    { pass1 obj record processor, return true on EOM }

      {<<<  processESD}
      procedure processESD;

      var
        esdType: byte;
        section: integer;
        symbolName: symbolNameType;
        b: boolean;
        i: integer;
        symbol: symbolPtr;

        {<<<}
        procedure addRef (symbol: symbolPtr; modName: symbolNameType);

        var
          reference: referencePtr;

        begin
          new (reference);
          reference^.next := symbol^.refList;
          reference^.symbolName := modName;
          symbol^.refList := reference;
        end;
        {>>>}

      begin
        objRecordBlockIndex := 0;
        while objRecordBlockIndex < objRecord.length DO
          begin
          symbol := nil;
          esdType := getByte;
          section := esdType MOD 16;
          esdType := esdType DIV 16;

          CASE esdType of
            {<<<}
            0:   { not sure }
              objRecordBlockIndex := objRecordBlockIndex + 8;
            {>>>}
            {<<<}
            1:   { common area symbol }
              begin
              symbolName := getSymbolName;
              i := getInt;
              b := findInsert (symbolName, symbol, true);

              if debugInfo then
                writeln ('Common data - section ', section:2, ' ', symbolName, ' length = ', hex (i, 6, 6));

              if xref then
                addRef (symbol, modName);

              if NOT symbol^.def then
                begin
                if b then
                  numUndefinedSymbols := numUndefinedSymbols-1;
                symbol^.modName := modName;
                symbol^.section := section;
                symbol^.def := true;
                symbol^.comsize := i;
                if prevCommon <> nil then
                  prevCommon^.nextCommon := symbol
                else
                  commonHead := symbol;
                symbol^.nextCommon := nil;
                prevCommon := symbol;
                end

              else
                if (i <> symbol^.comsize) then
                  begin
                  if (NOT symbol^.flagged) AND (symbol^.comsize=-1) then
                    begin
                    showModName;
                    writeln ('Label ', symbolName, ' is used double defined - ');
                    writeln ('as a common in module ', modName);
                    writeln (' and as an XDEF in module ', symbol^.modName);
                    symbol^.flagged := true;
                    end

                  else if check AND (NOT symbol^.flagged) then
                    begin
                    showModName;
                    writeln ('Common area size clash - common ', symbolName);
                    writeln ('size in this module is ',hex (i, 6, 6), ' bytes');
                    writeln ('size in ', symbol^.modName,' is ', hex (symbol^.comsize, 6, 6), ' bytes');
                    symbol^.flagged := true;
                    end;

                  if (i > symbol^.comsize) AND (symbol^.comsize <> -1) then
                    begin
                    symbol^.modName := modName;
                    symbol^.comsize := i;
                    end;
                  end;
              end;
            {>>>}
            {<<<}
            2,3: { section definition and allocation }
              begin
              i := getInt;
              if debugInfo then
                writeln ('section:', section:2, ' length:', hex (i, 6, 6));

              sectbase[section] := sectbase[section]+i;
              if odd(sectbase[section]) then
                sectbase[section] := sectbase[section] + 1;
              end;
            {>>>}
            {<<<}
            4,5: { symbol defintion }
              begin
              if esdType = 5 then
                section := -1;

              symbolName := getSymbolName;
              b := findInsert (symbolName, symbol, true);

              { this isnt right yet, should fix it }
              if (symbol^.def) AND (NOT symbol^.flagged) then
                begin
                if symbol^.hist then { previously defined by history file }
                  begin
                  if chat then
                    writeln ('redefining ', symbolName);
                  end
                else
                  doubledef(symbol)
                end

              else
                if b then
                  begin
                  if symbol^.hist then { previously defined by history file }
                    begin
                    if chat then
                      writeln ('redefining ',symbolName);
                    end
                  else
                    numUndefinedSymbols := numUndefinedSymbols - 1;
                  end;

              symbol^.modName := modName;
              symbol^.section := section;
              symbol^.def := true;
              symbol^.addr := getInt + sectbase[section];
              end;
            {>>>}
            {<<<}
            6,7: { symbol reference }
              begin
              if esdType = 6 then
                begin
                showModName;
                writeln ('xref ',section);
                end;

              symbolName := getSymbolName;
              b := findInsert (symbolName, symbol, true);

              if xref then
                addRef (symbol, modName);

              if (NOT b) then
                numUndefinedSymbols := numUndefinedSymbols + 1;
              end;
            {>>>}
            {<<<}
            8,9: { clAddress }
              begin
              showModName;
              writeln ('cl address');
              objRecordBlockIndex := objRecordBlockIndex + 5;
              end;
            {>>>}
            {<<<}
            10:  { cl addr common }
              begin
              showModName;
              writeln ('cl addr common');
              objRecordBlockIndex := objRecordBlockIndex + 15;
              end;
            {>>>}
            end;
          end;
      end;
      {>>>}

    begin
      processRecord := false;
      CASE objRecord.recordType of
        '1':
          processModuleId;

        '2':
          processESD;

        '3':
           begin
           if moreDebugInfo then
             writeln ('processText');
           end;

        '4':
           begin
           if moreDebugInfo then
             writeln ('processEOM');
           processRecord := true;
           end;
        end;
    end;
    {>>>}

  begin
    pass := 1;
    basepos := startBase;

    repeat
      getObjFileName (fileName);
      getFileStrings (fileName, '.ro', root, ext, fullFileName);

      if ext = '.his' then
        {<<<  .his}
        begin

        if usingHistory then
          writeln ('Only one history file, ignoring ', fullFileName)
        else
          begin
          startReadHisMilestone := getMilestone;
          readHistory (fullFileName);
          endReadHisMilestone := getMilestone;
          end;

        usingHistory := TRUE;
        end
        {>>>}
      else if ext = '.ro' then
        {<<<  .ro}
        begin
        cmdFileNameString := fullFileName;
        reset (objFile, fullFileName);

        repeat
          getObjRecord (objRecord);
        until processRecord;

        close (objFile);
        end
        {>>>}
      else if ext = '.rx' then
        {<<<  .rx}
        begin
        cmdFileNameString := fullFileName;
        reset (textObjFile, fullFileName);

        repeat
          getTextRec (objRecord);
        until processRecord;

        close (textObjFile);
        end
        {>>>}
    until eof (cmdFile);

    endPass1Milestone := getMilestone;
  end;
  {>>>}
  {<<<}
  procedure pass2;

  var
    fileName, root, ext, fullFileName: string;
    section: integer;
    numEsdRecords : integer;
    numTxtRecords : integer;

    {<<<}
    function processRecord: boolean;
    { pass2 obj record processor, return true on EOM }

      {<<<}
      procedure outputData;

      var
        c,pos : integer;

        {<<<}
        procedure srFormat (pos, len:integer);

        var
          b,cstart,i:integer;

          {<<<}
          procedure startpacket;

          var
            pktstart : string;
            plen : integer;

          begin
            cstart := codestart + pos*2;
            plen := len*2 + 4; {this happens to be right for both}

            if bin then
              BEGIN
              binbyte (esc);
              binbyte (0);
              wbyte (1);
              wbyte (mvr(plen));
              wbyte (plen MOD 256);
              wbyte (mvr(mvr(mvr(cstart))));
              wbyte (mvr(mvr(cstart)));
              wbyte (mvr(cstart));
              wbyte (cstart MOD 256);
              end

            else
              begin
              pktstart:='S2';
              sendsform (pktstart);
              wbyte (plen);
              wbyte (mvr(mvr(cstart)));
              wbyte (mvr(cstart));
              wbyte ((cstart MOD 256));
              end;
          end;
          {>>>}

        begin
          outputChecksum := 0;
          startpacket;

          if bin then
            for i := 1 TO len DO
              begin
              b := int (uand (%x'FFFF', uint (codeArray[i+pos]))) DIV 256;
              if (b = esc) AND (escape = true) then
                begin
                binBlock[binBlockIndex] := b;
                binBlockIndex := binBlockIndex + 1;
                if binBlockIndex > 511 then
                  begin
                  if out then write
                    (binaryFile, binBlock);
                  if download then
                    write (downloadTargetFile, binBlock);
                  binBlockIndex := 0;
                  end;
                end;

              binBlock[binBlockIndex] := b;
              binBlockIndex := binBlockIndex + 1;
              if binBlockIndex > 511 then
                begin
                if out then
                  write (binaryFile, binBlock);
                if download then
                  write (downloadTargetFile, binBlock);
                binBlockIndex := 0;
                end;

              outputChecksum := ord(uxor(uint(b),uint(outputChecksum)));

              b := codeArray[i+pos] MOD 256;
              if (b = esc) AND (escape = true) then
                begin
                binBlock[binBlockIndex] := b;
                binBlockIndex := binBlockIndex + 1;
                if binBlockIndex > 511 then
                  begin
                  if out then
                    write(binaryFile, binBlock);
                  if download then
                    write (downloadTargetFile, binBlock);
                  binBlockIndex := 0;
                  end;
                end;

              binBlock[binBlockIndex] := b;
              binBlockIndex := binBlockIndex + 1;
              if binBlockIndex > 511 then
                begin
                if out then
                  write(binaryFile, binBlock);
                if download then
                  write(downloadTargetFile, binBlock);
                binBlockIndex := 0;
                end;
              outputChecksum := ord (uxor (uint(b), uint (outputChecksum)));
              end
          else
            for i := 1 TO len DO
              begin
              wbyte (mvr (codeArray[i+pos]));
              wbyte (codeArray[i+pos] MOD 256);
              end;

          endPacket;
        end;
        {>>>}

      begin
        c := codelen;

        pos := 0;
        while c > outputMaxSize DO
          begin
          srFormat (pos, outputMaxSize);
          pos := pos + outputMaxSize;
          c := c - outputMaxSize;
          end;

        if c > 0 then
          srFormat (pos, c);
      end;
      {>>>}
      {<<<}
      procedure processESD;

        {<<<}
        procedure doESD;

        var
          i: integer;
          section: byte;
          esdType: byte;

          s: symbolNameType;
          b: boolean;
          patch: integer;
          symbol: symbolPtr;
          r: resolvePtr;

        begin
          esdType := getByte;
          section := esdType MOD 16;
          esdType := esdType DIV 16;

          CASE esdType of
            {<<<}
            0:
              begin
              objRecordBlockIndex := objRecordBlockIndex + 4;

              i := getInt;
              esdArray[topESD] := i;
              esdSymbolArray [topESD] := NIL;

              outAddrArray[topESD] := esdArray[topESD];
              topESD := topESD + 1;
              end;
            {>>>}
            {<<<}
            1:
              begin { common area symbol }
              s := getSymbolName;
              objRecordBlockIndex := objRecordBlockIndex + 4; {skip int}

              b := findInsert (s, symbol, false);
              if NOT b then
                begin
                showModName;
                writeln ('internal consistency check failure - lost symbol');
                end;

              esdArray[topESD] := symbol^.addr + baseaddr[symbol^.section];
              esdSymbolArray [topESD] := symbol;

              outAddrArray[topESD] := esdArray[topESD];
              topESD := topESD + 1;
              end;
            {>>>}
            {<<<}
            2,3 :
              begin { sectionion symbol }
              i := getInt;
              esdArray[section+1] := baseaddr[section] + sbase[section];
              esdSymbolArray [topESD] := NIL;

              outAddrArray[section+1] := esdArray[section+1];
              if modules then
                write (moduleFile, ' ', section:2, ':', hex (esdArray[section+1], 6, 6), '+', hex (i, 6, 6));

              sbase[section] := sbase[section] + i;

              if odd (sbase[section]) then
                sbase[section] := sbase[section] + 1;
              end;
            {>>>}
            {<<<}
            4,5 :
              if usingHistory then
                begin { symbol defintion, use to make patches on second pass }
                s := getSymbolName;
                b := findInsert (s, symbol, false); { find it }

                if symbol^.resList <> nil then
                  begin
                  r := symbol^.resList;
                  repeat
                    begin
                    patch := symbol^.addr + baseaddr[symbol^.section] + r^.offset;
                    if debugInfo then
                      writeln ('patching ',hex (r^.addr, 6, 6), ' with ',
                                           hex (patch-r^.offset, 6, 6), ' + ', hex (r^.offset, 6, 6));

                    codestart := r^.addr;
                    codeArray [1] := mvr (mvr (patch));
                    codeArray [2] := patch;
                    codelen := 2;
                    outputData;

                    r := r^.next;
                    end until r = nil;
                  end;

                objRecordBlockIndex := objRecordBlockIndex + 4; { skip past offset into module }
                end
              else
                objRecordBlockIndex := objRecordBlockIndex + 14; { skip past offset into module }
            {>>>}
            {<<<}
            6,7 :
              begin { symbol reference }
              s := getSymbolName;

              b := findInsert (s, symbol, false);
              if NOT b then
                begin
                showModName;
                writeln ('internal check failure - lost symbol');
                end;

              esdArray[topESD] := symbol^.addr + baseaddr[symbol^.section];
              esdSymbolArray [topESD] := symbol;

              outAddrArray[topesd] := esdArray[topESD];
              topESD := topESD + 1;
              end;
            {>>>}
            8,9 : objRecordBlockIndex := objRecordBlockIndex + 5;
            10  : objRecordBlockIndex := objRecordBlockIndex + 15;
            end;
        end;
        {>>>}

      begin
        objRecordBlockIndex := 0;
        while objRecordBlockIndex < objRecord.length DO
          doESD;
      end;
      {>>>}
      {<<<}
      procedure processText;

      var
        bitmap, curresd: integer;

        {<<<}
        procedure procbyte;

        var
          longwd : boolean;
          offset, add, i, numesds, offsize : integer;
          thisesd, w: integer;
          flag : byte;

          {<<<}
          procedure adddata (w:integer);

          begin
            duffer := w = %x'4EBA';
            codelen := codelen + 1;
            codeArray[codelen] := w;
          end;
          {>>>}

        begin
          if bitmap >= 0 then
            begin
            adddata (mvl (ord (objRecord.block[objRecordBlockIndex])) + ord(objRecord.block[objRecordBlockIndex+1]));
            objRecordBlockIndex := objRecordBlockIndex + 2;
            end

          else
            begin
            if duffer then
              begin
              showModName;
              writeln ('Warning - possible assembler foul-up');
              end;

            flag := getByte;
            numesds := flag DIV 32;
            offsize := flag MOD 8;
            { writeln('num esds, ',numesds,'  offset size ',offsize);}
            longwd := ((flag DIV 8) MOD 2) = 1;

            add := 0;
            for i := 1 TO numesds DO
              begin
              thisesd := getByte;
              if thisesd > topESD then
                begin
                showModName;
                writeln (' assembler foul-up.. trying to use an undefined ESD : ' , thisesd);
                end;

              if odd(i) then
                add := add + esdArray[thisesd]
              else
                add := add - esdArray[thisesd];
              end;

            offset := 0;
            for i := 1 TO offsize DO offset := mvl(offset) + getByte;
            CASE offsize of
              0,4:;
              1: if offset > 127   then
                  offset := int (uor (uint (offset),%X'FFFFFF00'));
              2: if offset > 32767 then
                  offset := int (uor (uint (offset),%X'FFFF0000'));
              end;

            {writeln('ofFSET ',hex(add,6,6),'+',hex(offset,6,6),'=',hex(add+offset,6,6)); }
            add := add + offset;
            if numesds = 0 then
              begin
              if odd (offset) then
                begin
                showModName;
                writeln ('odd fix-up offset - assembler error .', offset, curresd);
                writeln ('>>', hex (codestart, 6, 6));
                offset := offset + 1;
                end;

              if codelen > 0 then
                outputData;

              outAddrArray[curresd] := outAddrArray[curresd] + codelen*2 + offset;
              codelen := 0;
              codestart := outAddrArray[curresd];
              end

            else { numesd <> 0 }
              begin
              if NOT longwd then
                begin
                if (add > 32767) OR (add < -32768) then
                  begin
                  showModName;
                  writeln ('Long address generated into word location :', hex (add, 8, 8));
                  end;
                end;

              if esdSymbolArray [thisesd] <> NIL then { only need named symbols }
                if modName <> esdSymbolArray [thisesd]^.modName then { outside module }
                  begin
                  if history then
                    { address to be resolved LONGWORD only at present}
                    addRes (esdSymbolArray [thisesd], codestart + codelen*2, offset);

                  if debugInfo then
                    writeln ('sym ', longwd,
                             ' ', thisesd:2,
                             ' ', esdSymbolArray [thisesd]^.symbolName,
                             ' ', hex (add, 8, 8), ' = ', hex (esdArray[thisesd]),
                             ' + ', hex (offset, 4, 4), ';', hex (offsize, 1, 1),
                             ' at ', hex (codestart + codelen * 2, 8, 8));
                  end;

              { generate resolved address }
              if longwd then
                adddata (mvr (mvr (add)));
              adddata (add);
              end;
            end;

          bitmap := bitmap * 2;
        end;
        {>>>}

      begin
        objRecordBlockIndex := 0;
        bitmap := getInt;

        codelen := 0;
        curresd := getByte;
        codestart := outAddrArray[curresd];

        while objRecordBlockIndex < objRecord.length DO
          procbyte;
        outputData;

        { dont forget convert to bytes}
        outAddrArray[curresd] := outAddrArray[curresd] + (codelen * 2);
      end;
      {>>>}
      {<<<}
      procedure processEOM;

      begin
        if modules then
          writeln (moduleFile);
      end;
      {>>>}

    begin
      processRecord := false;
      CASE objRecord.recordType of
        '1':
          processModuleId;

        '2':
          begin
          numEsdRecords := numEsdRecords + 1;
          processESD;
          end;

        '3':
          begin
          numTxtRecords := numTxtRecords + 1;
          processText;
          end;

        '4':
          begin
          processEOM;
          processRecord := true;
          end;
        end;
    end;
    {>>>}

  begin
    Writeln ('pass2');
    openOutput;
    openModules;

    { init sections }
    for section := 0 TO 15 DO
      sbase[section] := 0;

    { open .cmd file again for second pass }
    reset (cmdFile);

    repeat
      getObjFileName (fileName);
      getFileStrings (fileName, '.ro', root, ext, fullFileName);

      if ext = '.his' then
        begin
        end
      else if ext = '.ro' then
        {<<<  .ro file}
        begin
        numEsdRecords := 0;
        numTxtRecords := 0;

        cmdFileNameString := fullFileName;
        reset (objFile, fullFileName);

        repeat
          getObjRecord (objRecord);
        until processRecord;

        Writeln (filename, ' num esdRecords:', numEsdRecords:2, ' numTxtRecords:', numTxtRecords:2);
        close (objFile);
        end
        {>>>}
      else if ext = '.rx' then
        {<<<  .rx file}
        begin
        cmdFileNameString := fullFileName;
        reset (textObjFile, fullFileName);

        repeat
          getTextRec (objRecord);
        until processRecord;

        close (textObjFile);
        end
        {>>>}
    until eof (cmdFile);

    closeModules;
    closeoutput;

    endPass2Milestone := getMilestone;
  end;
  {>>>}

  {<<<}
  procedure init;

  begin
    modules := false;
    download := false;
    check := false;
    bell := false;
    xref := false;

    map := true;
    bin := false;
    out := true;
    symout := false;

    debugInfo := false;
    chat := false;
    logging := false;
    friendly := false;
    quiet := false;

    files := false;
    history := false;
    escape := true;

    usingHistory := false;
    fileIdString := 'no file open.' ;

    { set up pointers for common area list }
    commonHead := nil;
    prevCommon := nil;

    total := 0;
    numUndefinedSymbols := 0;

    pass := 0;

    for i := -1 TO 15 do
      begin
      userbase[i] := -1;  { set up user bases as not needed }
      sectbase[i] := 0;
      sbase[i] := 0;
      baseaddr[i] := 0;
      end;

    for i := 0 TO 255 do
      begin
      esdSymbolArray[i] = nil;
      esdArray[i] := 0;
      outAddrArray[i] = 0;
      end;

    for i := 0 TO maxHash do
      hashTable[i] := nil;

    clearMilestone (startLinkMilestone);
    clearMilestone (endPass1Milestone);
    clearMilestone (endPass2Milestone);
    clearMilestone (endLinkMilestone);
    clearMilestone (endMapGenMilestone);
    clearMilestone (endHisGenMilestone);
    clearMilestone (endSymGenMilestone);
    clearMilestone (endSpaceAllocMilestone);
    clearMilestone (endXrefGenMilestone);
    clearMilestone (startReadHisMilestone);
    clearMilestone (endReadHisMilestone);

    outputMaxSize :=  0;
    outputChecksum := 0;

    total := 0;
    basepos := 0;
  end;
  {>>>}
  {<<<}
  procedure reportHash;

  var
    depth, i, hash_used: integer;
    depth_used: array[ 0 .. 10 ] of integer;
    symbol: symbolPtr;

  begin
    hash_used := 0;

    for depth := 0 TO 10 DO
      depth_used[ depth ] := 0;

    for i := 0 TO maxHash DO
      if hashTable[i] <> nil then
        begin
        hash_used := hash_used + 1;
        depth := 0;
        symbol := hashTable[i];
        repeat
          depth := depth + 1;
          symbol := symbol^.nextSymbol;
        until symbol = nil;

        if depth > 10 then
          begin
          writeln ('Depth ', depth :0, ' too deep, rounding to 10' );
          depth := 10;
          end;
        depth_used[ depth ] := depth_used[ depth ] + 1;
        end;

    if chat OR debugInfo then
      begin
      writeln (hash_used :0, ' out of ', maxHash:0, ' hash table entries used');
      for depth := 0 TO 10 DO
        if depth_used[depth] <> 0 then
          write (depth_used[depth]:0, ' @ ', depth:0, ',' );
      writeln;
      end;
  end;
  {>>>}

{ main }
{<<<}
begin
  init;

  { get cmd line }
  cmdLen := -1;
  P_getcmdline (cmd, cmdLen);

  cmdString = '';
  for i := 1 to cmdLen do
    cmdString := cmdString + cmd[i];
  writeln ('cmdString:', cmdString, ' cmdLen:', cmdLen:0);

  { cmd line switches }
  switchSettingsProcess (cmdString);

  { get cmdFileNameString, cmdFileRootString, cmdFileextString from cmdString and ext }
  getFileStrings (cmdString, '.cmd', cmdFileRootString, cmdFileExtString, cmdFileNameString);

  startLinkMilestone := getMilestone;
  startReadHisMilestone := getMilestone;
  endReadHisMilestone := startReadHisMilestone;

  if chat OR debugInfo then
    writeln ('File given is ', cmdFileNameString);
  writeln ('Linking from ', cmdFileNameString);

  { open .cmd file for first pass }
  reset (cmdFile, cmdFileNameString);
  pass1;

  { report on pass1 }
  allocCom;
  reportHash;
  if chat OR debugInfo OR (NOT quiet) then
    {<<<  report symbolTable}
    begin
    writeln (numSymbols:5,' in symbol table');

    if logging then
      writeln (logFile, numSymbols:5,' in symbol table');
    end;
    {>>>}
  baseaddr[-1] := 0;      {set up base of absolute section}
  for i := 0 TO 15 DO
    {<<<  report section}
    begin
    if userbase[i] <> -1 then { put this section somewhere special}
      basepos := userbase[i];

    if sectbase[i] <> 0 then
      begin
      if not friendly then
        write ('section:', i:2,
               ' start:', hex (basepos, 6, 6),
               ' length:', hex (sectbase[i], 6, 6));

      baseaddr[i] := basepos;
      basepos := basepos + sectbase[i];

      if not friendly then
        writeln (' finish  ', hex (basepos, 6, 6));

      end;
    end;
    {>>>}
  if friendly then
    {<<<  report section usage nicely}
    begin
    writeln;
    if sectbase[8] <> 0 then
      begin
      writeln ('Size of P                 (8)  = ', sectbase[8]:8, ' bytes');
      total := total + sectbase[8];
      end;

    if sectbase[9] <> 0 then
      begin
      writeln ('Size of HELP              (9)  = ', sectbase[9]:8, ' bytes');
      total := total + sectbase[9];
      end;

    if sectbase[12] <> 0 then
      begin
      writeln ('Size of error messages   (12)  = ', sectbase[12]:8, ' bytes');
      total := total + sectbase[12];
      end;

    if sectbase[13] <> 0 then
      begin
      writeln ('Size of code & constants (13)  = ', sectbase[13]:8, ' bytes');
      total := total + sectbase[13];
      end;

    if sectbase[14] <> 0 then
      begin
      writeln ('Size of diagnostic block (14)  = ', sectbase[14]:8, ' bytes');
      total := total + sectbase[14];
      end;

    if sectbase[15] <> 0 then
      begin
      writeln ('Size of global variables (15)  = ', sectbase[15]:8, ' bytes');
      total := total + sectbase[15];
      end;

    writeln ('Total size                     = ', total:8, ' bytes');
    end;
    {>>>}
  overlapCheck;
  endSpaceAllocMilestone := getMilestone;
  if numUndefinedSymbols <> 0 then
    {<<<  report undefined symbols}
    begin
    writeln ('undefined symbols:', numUndefinedSymbols:0);

    if logging then
      writeln (logFile, 'undefined symbols:', numUndefinedSymbols:0);

    checkUndefinedSymbols;
    end;
    {>>>}

  if modules OR out OR download then
    {<<<  run pass2}
    begin

    if bin then
      outputMaxSize := 512 { big number }
    else
      outputMaxSize := 16; { s-format max line size }

    pass2;
    end;
    {>>>}

  { close .cmd file after second pass }
  close (cmdFile);

  {<<<  report histoy}
  if history then
    dumpHistory;

  endHisGenMilestone := getMilestone;
  {>>>}
  {<<<  report symbolTable}
  if symout then
    dumpSymbols;

  endSymGenMilestone := getMilestone;
  {>>>}
  {<<<  report map}
  if map then
    dumpSymbolMap;

  endMapGenMilestone := getMilestone;
  {>>>}
  {<<<  report xref}
  if xref then
    dumpXreferences;

  endXrefGenMilestone := getMilestone;
  {>>>}

  if bell then for i := 1 TO 10 DO
    write (chr(7));
  writeln;
  endLinkMilestone := getMilestone;

  if chat OR debugInfo OR (NOT quiet) then
    {<<<  report timings}
    begin
    writeln ('Link started           ', startLinkMilestone.timeOfDay);

    showMilestone ('Pass 1                 ', endPass1Milestone, startLinkMilestone);

    if startReadHisMilestone.millTime <> endReadHisMilestone.millTime then
      showMilestone ('Reading history file   ', endReadHisMilestone, startReadHisMilestone);

    showMilestone ('Space allocation       ', endSpaceAllocMilestone, endPass1Milestone);
    showMilestone ('Pass 2                 ', endPass2Milestone, endSpaceAllocMilestone);

    if history then
      showMilestone ('.HIS generation        ', endHisGenMilestone, endPass2Milestone);
    if symout then
      showMilestone ('.SYM generation        ', endSymGenMilestone, endHisGenMilestone);
    if map then
      showMilestone ('.MAP generation        ', endMapGenMilestone, endSymGenMilestone);
    if xref then
      showMilestone ('.XRF generation        ', endXrefGenMilestone, endMapGenMilestone);
    showMilestone ('Link ended             ', endLinkMilestone, startLinkMilestone);

    writeln;
    writeln ('total CPU time:- ', (endLinkMilestone.millTime - startLinkMilestone.millTime) / 1000:7:2);
    end;
    {>>>}
  if friendly then
    {<<<  report timigs nicely}
    begin
    date (datestring);
    writeln;
    writeln ('Link started ', startLinkMilestone.timeOfDay, ' ', datestring);
    writeln ('Link ended   ', endLinkMilestone.timeOfDay, ' ', datestring);
    end;
    {>>>}

  if logging then
    closeloggingFile;
end.
{>>>}
