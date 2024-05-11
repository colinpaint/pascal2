// ql.cpp - liker for 68k motorola, .ro, relocatable object, format for versados
//{{{  includes
#include <cstdio>
#include <cstdint>

#include <string>
#include <array>
#include <vector>
#include <map>

#include <fstream>
#include <iomanip>

// use C++20 format library, still not in gcc so use fmt
#include "fmt/core.h"
#include "fmt/color.h"

using namespace std;
using namespace fmt;
//}}}
//{{{  constexpr, const enum
constexpr bool kOptionDebug = false;
constexpr bool kCmdLineDebug = false;
constexpr bool kObjectFileDebug = false;
constexpr bool kPassDebug = false;

constexpr bool kEsdDebug = true;
constexpr bool kXrefDebug = false;
constexpr bool kXdefDebug = false;

constexpr bool kOutDebug = false;

// enum options, easier to use as globals
enum eOption { eChat, eDebug, eXrf, eEscape, eBin, eSr, eLastOption };
constexpr size_t kNumOptions = eLastOption; // for enum arrays to play nice

// sections
constexpr size_t kNumSections = 16;

// symbol - usually 8 chars for oregon pascal compiler, but masm may use 10, padded with trailing spaces
constexpr size_t kObjectSymbolNameLength = 10;

// default section start address, no idea why its this number, VersaDos history ?
constexpr uint32_t kDefaultStartAddress = 0x400;

constexpr uint8_t kEscapeCh = 0x1B;

constexpr int kDumpWidth = 100;
//}}}

//{{{
class cOptions {
public:
  cOptions() {}
  virtual ~cOptions() = default;

  //{{{
  bool getEnabled (eOption option) {
    return mEnabled[option];
    }
  //}}}
  //{{{
  uint32_t getSectionBaseAddress (uint8_t section) {
    return mSectionBaseAddress[section];
    }
  //}}}

  //{{{
  void parseLine (const string& line) {

    if (kOptionDebug)
      print ("parseOptions {}\n", line);

    // parse into individual options, stripping out /
    size_t start = 1;
    size_t found = line.find ('/', start);
    while (found != string::npos)  {
      parseToken (line.substr (start, found-start));
      start = found + 1;
      found = line.find ('/', start);
      }

    parseToken (line.substr (start, found));
    }
  //}}}
  //{{{
  void dump() {

    print (fg (color::yellow), "options");
    for (uint8_t optionIndex = eChat; optionIndex < eLastOption; optionIndex++)
      if (mEnabled[optionIndex])
        print (fg (color::lime_green), " {}", kOptionNames [optionIndex]);
    print ("\n");

    for (uint8_t section = 0;  section <= 15; section++)
      if (mSectionBaseAddress[section])
        print (fg (color::yellow),
               "  section {:2d} baseAddress:{:6x}\n", section, mSectionBaseAddress[section]);
    }
  //}}}

private:
  //{{{
  char getCh() {
    if (mTokenIndex < mToken.size())
      return mToken[mTokenIndex++];
    else // space if no more chars in token, easy to parse
      return ' ';
    }
  //}}}
  //{{{
  uint32_t getHex() {

    uint32_t value = 0;
    while (true) {
      char ch = getCh();
      if ((ch >= '0') && (ch <= '9'))
        value = (value << 4) + (ch - '0');
      else if ((ch >= 'A') && (ch <= 'F'))
        value = (value << 4) + ((ch - 'A') + 10);
      else if ((ch >= 'a') && (ch <= 'f'))
        value = (value << 4) + ((ch - 'a') + 10);
      else
        return value;
      }
    }
  //}}}
  //{{{
  uint8_t getSection() {

    uint8_t value = 0;
    while (true) {
      char ch = getCh();
      if ((ch >= '0') && (ch <= '9'))
        value = (value * 10) + (ch - '0');
      else
        return value;
      }
    }
  //}}}

  //{{{
  void parseToken (const string& token) {

    mTokenIndex = 0;
    mToken = token;

    bool found = false;
    for (int optionIndex = eChat; optionIndex < eLastOption; optionIndex++)
      if ((token == kOptionNames[optionIndex]) ||(token == kOptionAltNames[optionIndex])) {
        mEnabled[optionIndex] = true;
        found = true;
        break;
        }

    if (!found) {
      // maybe section abase address
      char ch = getCh();
      if (ch == 'o') {
        size_t section = getSection();
        uint32_t address = getHex();
        if ((section >= 0) && (section <= 15))
          mSectionBaseAddress[section] = address;

        if (kOptionDebug)
          print ("parseOption section {} sectionNum:{:2d} address:{:6x}\n", token, (int)section, address);
        }
      else
        print (fg (color::orange_red), "parseOption - unrecognised option {}\n", token);
      }
    }
  //}}}

  // const
  const array <string, kNumOptions> kOptionNames    = { "chat", "debug", "xref", "esc", "bin", "sr"};
  const array <string, kNumOptions> kOptionAltNames = { "cha",  "deb",   "xrf",  ""   , ""   ,  ""  };

  // var
  array <bool, kNumOptions> mEnabled = { false };
  array <uint32_t, kNumSections> mSectionBaseAddress = { 0 };

  size_t mTokenIndex = 0;
  string mToken;
  };
//}}}
//{{{
class cSymbol {
public:
  cSymbol (const string& name) : mName(name) {}
  virtual ~cSymbol() = default;

  //{{{
  void addReference (const string& moduleName) {
    mReferences.push_back (moduleName);
    }
  //}}}

  bool mDefined = false;

  string mName;
  string mModuleName;

  int mSection = 0;
  uint32_t mAddress = 0;

  bool mCommonSizeDefined = false;
  uint32_t mCommonSize = 0;

  bool mUsed = false;
  bool mHist = false;
  bool mErrorFlagged = false;

  vector <string> mReferences;
  };
//}}}
//{{{
class cLinker {
public:
  cLinker() = default;
  virtual ~cLinker() = default;

  //{{{
  bool getEnabled (eOption option) {
    return mOptions.getEnabled (option);
    }
  //}}}
  //{{{
  string getCurrentModuleName() {
    return mCurrentModuleName;
    }
  //}}}
  //{{{
  void setCurrentModuleName (const string& moduleName) {
    mCurrentModuleName = moduleName;
    }
  //}}}

  //{{{
  cSymbol* findSymbol (const string& symbolName) {
  // find symbol, return nullptr if not found

    auto it = mSymbolMap.find (symbolName);
    return (it == mSymbolMap.end()) ? nullptr : (*it).second;
    }
  //}}}
  //{{{
  cSymbol* findCreateSymbol (const string& symbolName, bool& found) {

    cSymbol* symbol = findSymbol (symbolName);

    found = symbol != nullptr;
    if (!found) {
      symbol = new cSymbol (symbolName);
      mSymbolMap.emplace (symbolName, symbol);
      }

    return symbol;
    }
  //}}}

  //{{{
  void addCommonSymbol (cSymbol* symbol) {
    mCommonSymbols.push_back (symbol);
    }
  //}}}
  //{{{
  void allocCommonSections() {
  // allocate common sections

    for (auto& symbol : mCommonSymbols) {
      mBaseAddress = mOptions.getSectionBaseAddress (symbol->mSection);

      symbol->mAddress = mSections[symbol->mSection].mSectBase;
      mSections[symbol->mSection].mSectBase = mSections[symbol->mSection].mSectBase + uint32_t(symbol->mCommonSize);

      if (mSections[symbol->mSection].mSectBase & 1)
        mSections[symbol->mSection].mSectBase = mSections[symbol->mSection].mSectBase + 1;
      }
    }
  //}}}

  //{{{
  void parseOptions (const string& line) {
    mOptions.parseLine (line);
    }
  //}}}

  //{{{
  void incObjectFiles() {
    mNumObjectFiles++;
    }
  //}}}
  //{{{
  void incCommonDefs() {
    mNumCommonDefs++;
    }
  //}}}
  //{{{
  void incXdefs() {
    mNumXdefs++;
    }
  //}}}
  //{{{
  void incXrefs() {
    mNumXrefs++;
    }
  //}}}

  //{{{
  void dumpOptions() {
    mOptions.dump();
    }
  //}}}
  //{{{
  void dumpSections() {

    for (uint8_t section = 0; section < 16; section++) {
      if (mOptions.getSectionBaseAddress (section))
        mBaseAddress = mOptions.getSectionBaseAddress (section);

      if (mSections[section].mSectBase) {
        print (fg (color::light_yellow),
               "section:{:2d} start:{:6x} size:{:6x}\n", (int)section, mBaseAddress, mSections[section].mSectBase);
        mSections[section].mBaseAddress = mBaseAddress;
        mBaseAddress = mBaseAddress + mSections[section].mSectBase;
        }
      }

    print (fg (color::light_yellow),
           "          finish:{:6x} size:{:6x}\n", mBaseAddress, mBaseAddress - mOptions.getSectionBaseAddress(0));
    }
  //}}}
  //{{{
  void dumpSummary() {

    uint32_t numDefinedSymbols = 0;
    uint32_t numUndefinedSymbols = 0;

    for (auto const& [key, symbol] : mSymbolMap)
      symbol->mDefined ? numDefinedSymbols++: numUndefinedSymbols++;

    if (numUndefinedSymbols)
      print (fg (color::orange_red), "{} undefined symbols, ", numUndefinedSymbols);

    print (fg (color::light_salmon),
           "{} symbols, {} objectFiles, {} commonDefs, {} xDefs, {} xRefs\n",
            numDefinedSymbols, mNumObjectFiles, mNumCommonDefs, mNumXdefs, mNumXrefs);
    }
  //}}}
  //{{{
  void dumpReferences (const string& fileName) {

    ofstream stream (fileName + ".xrf", ofstream::out);

    // list undefined symbols
    stream << "---------------- undefined symbols ------------------------------" << endl;
    for (auto const& [key, symbol] : mSymbolMap)
      if (!symbol->mDefined) {
        //{{{  list symbol
        stream << left << setw (kObjectSymbolNameLength) << symbol->mName.c_str() << "undefined";
        if (symbol->mReferences.empty())
          stream << endl;

        else {
          stream << " usedBy";
          int width = kObjectSymbolNameLength + 9 + 7;
          for (auto& symbolName : symbol->mReferences) {
            if (!width) {
              width = kObjectSymbolNameLength + 9 + 7;
              for (int i = 0; i < width; i++)
                stream << ' ';
              }
            stream << " " << symbolName;
            width += (int)symbolName.length() + 1;
            if (width > kDumpWidth) {
              stream << endl;
              width = 0;
              }
            }
          if (width)
            stream << endl;
          }
        }
        //}}}

    // list defined used symbols
    stream << endl
           << "----------------- defined symbols -------------------------------" << endl;
    for (auto const& [key, symbol] : mSymbolMap)
      if (symbol->mDefined && !symbol->mReferences.empty()) {
        //{{{  list symbol
        stream << left << setw (kObjectSymbolNameLength) << symbol->mName.c_str()
               << "definedBy "
               << left << setw(9) << symbol->mModuleName
               << " usedBy";
        int width = kObjectSymbolNameLength + 10  + 9 + 7;

        for (auto& symbolName : symbol->mReferences) {
          if (!width) {
            width = kObjectSymbolNameLength + 10 + 9 + 7;
            for (int i = 0; i < width; i++)
              stream << ' ';
            }

          stream << " " << symbolName;
          width += (int)symbolName.length() + 1;
          if (width > kDumpWidth) {
            stream << endl;
            width = 0;
            }
          }

        if (width)
          stream << endl;
        }
        //}}}

    // list defined but unused symbols
    stream << endl
           << "------------- defined but unused symbols ------------------------" << endl;
    for (auto const& [key, symbol] : mSymbolMap)
      if (symbol->mDefined && symbol->mReferences.empty())
        stream << left << setw (kObjectSymbolNameLength) << symbol->mName.c_str()
               << " unused definedBy " << symbol->mModuleName << endl;
    stream << "-----------------------------------------------------------------" << endl;

    stream.close();
    }
  //}}}

  uint32_t mBaseAddress = kDefaultStartAddress;

  //{{{
  struct sSection {
    uint32_t mSbase;
    uint32_t mSectBase;
    uint32_t mBaseAddress;
    };
  //}}}
  array <sSection, 16> mSections = {};
  sSection mAbsoluteSection = {};

private:
  cOptions mOptions;

  map <string, cSymbol*> mSymbolMap;
  vector <cSymbol*> mCommonSymbols;

  string mCurrentModuleName;

  uint32_t mNumObjectFiles = 0;
  uint32_t mNumCommonDefs = 0;
  uint32_t mNumXdefs = 0;
  uint32_t mNumXrefs = 0;
  };
//}}}
//{{{
class cOutput {
public:
  //{{{
  cOutput (const string& fileName, bool bin, bool escape)
     : mBin(bin), mEscape(escape), mOutputMaxSize (bin ? 512 : 16) {
    mStream.open (fileName + ".sr", bin ? ofstream::out | ofstream::binary : ofstream::out);
    }
  //}}}
  //{{{
  virtual ~cOutput() {

    if (mBin) {
      writeByte (kEscapeCh);
      writeByte (0);

      mOutputChecksum = 0;
      writeCheckSummedByte (2);
      writeCheckSummedByte (0);
      writeCheckSummedByte (4);

      for (int i = 0; i < 4; i++)
        writeCheckSummedByte (0);

      // !!! this looks odd, flushes the stream for download ?
      for (int i = 0; i < 512; i++)
        writeByte (0);
      }

    else
      mStream << "S9030000FC" << endl;

    mStream.close();
    }
  //}}}

  //{{{
  void init() {
    mCodeLength = 0;
    }
  //}}}

  //{{{
  void addWord (uint16_t word) {
    mCode[mCodeLength++] = word;
    }
  //}}}
  //{{{
  uint32_t outputCode (uint32_t start) {
  // output codeArray

    uint32_t pos = 0;

    uint32_t length = mCodeLength;
    while (length > mOutputMaxSize) {
      outputPacket (start, pos, mOutputMaxSize);
      pos = pos + mOutputMaxSize;
      length = length - mOutputMaxSize;
      }

    if (length > 0)
      outputPacket (start, pos, length);

    // retunr codeLength as number of bytes
    return mCodeLength * 2;
    }
  //}}}

private:
  //{{{
  static string toHex (uint8_t byte) {

    string result;

    uint8_t digit = byte >> 4;
    result += digit > 9 ? 'a' + digit - 10 : '0' + digit;

    digit = byte & 0x0F;
    result += digit > 9 ? 'a' + digit - 10 : '0' + digit;

    return  result;
    }
  //}}}

  //{{{
  void writeByte (uint8_t b) {

    if ((b == kEscapeCh) && mEscape)
      mStream << b;

    mStream << b;
    }
  //}}}
  //{{{
  void writeCheckSummedByte (uint8_t b) {

    if (mBin) {
      writeByte (b);
      mOutputChecksum ^= b;
      }

    else {
      mStream << toHex (b);
      mOutputChecksum += b;
      }
    }
  //}}}
  //{{{
  void outputPacket (uint32_t start, uint32_t pos, uint32_t length) {
  // output packet of codeArray

    if (kOutDebug)
      print ("outputPacket start:{:6x} pos:{:2x} length:{}\n", start, pos, length);
    uint32_t packetAddress = start + (pos * 2);
    uint32_t packetLength = (length * 2) + 4; // this happens to be right for both

    if (mBin) {
      // .bin packetStart
      writeByte (kEscapeCh);
      writeByte (0);

      writeCheckSummedByte (1);
      writeCheckSummedByte (packetLength >> 8);
      writeCheckSummedByte (packetLength & 0xFF);

      writeCheckSummedByte (packetAddress >> 24);
      writeCheckSummedByte (packetAddress >> 16);
      writeCheckSummedByte (packetAddress >> 8);
      writeCheckSummedByte (packetAddress & 0xFF);

      mOutputChecksum = 0;

      // packet
      for (uint8_t i = 0; i < length; i++) {
        // output length code words
        writeCheckSummedByte (mCode[i+pos] >> 8);
        writeCheckSummedByte (mCode[i+pos] & 0xFF);
        }

      writeByte (mOutputChecksum);
      }

    else {
      // s format .sr packetStart
      mStream << "S2";

      mOutputChecksum = 0;
      writeCheckSummedByte (packetLength);

      writeCheckSummedByte (packetAddress >> 16);
      writeCheckSummedByte (packetAddress >> 8);
      writeCheckSummedByte (packetAddress & 0xFF);

      // packet
      for (uint8_t i = 0; i < length; i++) {
        // output length code words
        writeCheckSummedByte (mCode[i+pos] >> 8);
        writeCheckSummedByte (mCode[i+pos] & 0xFF);
        }

      // packet end
      mStream << toHex (0xFF - (mOutputChecksum & 0xFF));
      mStream << endl;
      }
    }
  //}}}

  const bool mBin;
  const bool mEscape;
  const uint32_t mOutputMaxSize = 0;

  ofstream mStream;

  uint32_t mCodeLength = 0; // code length in words
  int mOutputChecksum = 0;
  array <uint16_t, 64> mCode;
  };
//}}}
//{{{
class cObjectFile {
public:
  enum eType { eUnknown, eRo, eRx, eXro, eHis };

  //{{{
  cObjectFile (const string& fileRootName, const string& extension)
      : mFileName (fileRootName + "." + extension) {

    if (extension == "ro")
      mType = eRo;
    else if (extension == "rx")
      mType = eRx;
    else if (extension == "xro")
      mType = eXro;
    else if (extension == "his")
      mType = eHis;
    else
      mType = eUnknown;
    }
  //}}}

  virtual ~cObjectFile() = default;

  //{{{
  void pass1 (cLinker& linker) {

    if (linker.getEnabled (eChat))
      print ("objectFile {}\n", mFileName);
    else if (kPassDebug)
      print ("pass1 file {}\n", mFileName);

    if ((mType == eRx) || (mType == eHis) || (mType == eUnknown))
      return;

    ifstream stream (mFileName.c_str(), ifstream::in | ifstream::binary);
    if (!stream.is_open()) {
      //{{{  error, return
      mErrorFlagged = true;
      print (fg (color::orange_red), "error - objectFile {} not found\n", mFileName);
      return;
      }
      //}}}

    cRecord record;
    while (record.load (stream)) {
      if (kObjectFileDebug)
        record.dump();

      switch (record.getHeader()) {
        case '1':
          incIdRecords();
          record.parseId (this, linker, true);
          break;

        case '2':
          incEsdRecords();
          record.parseEsd (this, linker, true);
          break;

        case '3':
          incTxtRecords();
          break;

        case '4':
          incEndRecords();
          break;

        default:
          break;
        }
      }

    stream.close();
    }
  //}}}
  //{{{
  void pass2 (cLinker& linker, cOutput& output) {

    if ((mType == eRx) || (mType == eHis) || (mType == eUnknown) || mErrorFlagged)
      return;

    if (kPassDebug)
      print ("pass 2 file {}\n", mFileName);

    ifstream stream (mFileName.c_str(), ifstream::in | ifstream::binary);
    if (!stream.is_open()) {
      //{{{  error, return
      mErrorFlagged = true;
      print (fg (color::orange_red), "error - objectFile {} not found on pass2 but ok on pass1\n", mFileName);
      return;
      }
      //}}}

    cRecord record;
    while (record.load (stream)) {
      switch (record.getHeader()) {
        case '1':
          record.parseId (this, linker, false);
          break;

        case '2':
          record.parseEsd (this, linker, false);
          break;

        case '3':
          record.parseText (this, linker, output);
          break;

        case '4':
          break;

        default:
          break;
        }
      }

    stream.close();
    }
  //}}}

  //{{{
  void dumpSummary() {

    print ("objectFile {} id:{} esd:{} text:{} end:{} topEsd:{}\n",
           mFileName.c_str(), mNumIdRecords, mNumEsdRecords, mNumTxtRecords, mNumEndRecords, mTopEsd);
    };
  //}}}

  // esd
  //{{{
  struct sEsd {
    cSymbol* mSymbol;
    uint32_t mAddress;
    uint32_t mOutAddress;
    };
  //}}}
  array <sEsd, 256> mEsds = {};
  uint8_t mTopEsd = 0;

private:
  //{{{
  class cRecord {
  public:
    cRecord() = default;
    virtual ~cRecord() = default;

    // load
    //{{{
    bool load (ifstream& stream) {
    // load .ro format record
    // - return true if EOM, should use EOF as well

      mHeader = 0;
      mLength = 0;
      mBlockIndex = 0;

      // get record length
      if (stream.eof())
        return false;
      mLength = stream.get();

      // skip any trailing zeros in file block
      while (!mLength) {
        if (stream.eof())
          return false;
        uint8_t byte = stream.get();
        }

      // get  header type, ascii 1,2,3,4
      if (stream.eof())
        return false;
      mHeader = stream.get();

      if ((mHeader > '0') && (mHeader <= '4')) {
        // get length bytes of block
        for (size_t i = 0; i < mLength-1; i++)
          if (stream.eof())
            return false;
          else
            mBlock[i] = stream.get();
        }
      else
        print (fg (color::orange_red), "error - unrecognised objectRecord header {:x}\n", mHeader);

      return true;
      }
    //}}}

    // gets
    //{{{
    uint8_t getHeader() {
      return mHeader;
      };
    //}}}
    //{{{
    int getBytesLeft() {
      return mLength - mBlockIndex - 1;
      }
    //}}}
    //{{{
    uint8_t getUint8() {
    // get 1 bytes to form uint8_t result

      if (mBlockIndex >= mLength) {
        print (fg (color::orange_red), "error - cObject::getUint8 past end of record data\n");
        return 0;
        }

      return mBlock[mBlockIndex++];
      }
    //}}}
    //{{{
    uint32_t getUint32() {
    // get 4 bytes to form uint32_t result

      uint32_t value = 0;
      for (int i = 1; i <= 4; i++)
        value = (value << 8) + getUint8();

      return value;
      }
    //}}}
    //{{{
    string getSymbolName() {
    // 10 bytes but only 8 have ascii chars, trailing spaces pad
    // force upperCase

      string name;
      name.reserve (kObjectSymbolNameLength);

      // read maxSymbolNameLength, but only append to name up to first space in string
      bool spaceFound = false;
      for (int i = 0; i < kObjectSymbolNameLength; i++) {
        char byte = getUint8();
        if (byte == ' ')
          spaceFound = true;
        if (!spaceFound)
          name = name + char(toupper (byte));
        }

      return name;
      }
    //}}}

    // parse
    //{{{
    void parseId (cObjectFile* objectFile, cLinker& linker, bool pass1) {

      linker.setCurrentModuleName (getSymbolName());

      if (pass1 && kEsdDebug)
        print ("Id  module:{}\n", linker.getCurrentModuleName());

      if (kPassDebug)
        print ("Id record - module:{}\n", linker.getCurrentModuleName());

      // init esd values in case of zero length sections
      objectFile->mTopEsd = 17;

      if (!pass1) {
        // pass2 - set esd 0 info, not sure why???
        objectFile->mEsds[0].mAddress = 0;

        // set common esd 1-16 from sections 0-15 info
        for (int section = 0; section < 16; section++) {
          objectFile->mEsds[objectFile->mTopEsd].mSymbol = nullptr;
          objectFile->mEsds[section+1].mAddress = linker.mSections[section].mBaseAddress +
                                                  linker.mSections[section].mSbase;
          objectFile->mEsds[section+1].mOutAddress = objectFile->mEsds[section+1].mAddress;
          }
        }
      }
    //}}}
    //{{{
    void parseEsd (cObjectFile* objectFile, cLinker& linker, bool pass1) {
    // parse ExternalSymbolDefinition record

      while (getBytesLeft() > 0) {
        uint8_t byte = getUint8();
        uint8_t section = byte & 0x0F;
        uint8_t esdType = byte >> 4;

        switch (esdType) {
          //{{{
          case  0: { // absolute section
            uint32_t size = getUint32();
            uint32_t start = getUint32();

            if (pass1)
              print ("Absolute section in {} size:{:x} start:{:x}\n", linker.getCurrentModuleName(), size, start);
            else {
              objectFile->mEsds[objectFile->mTopEsd].mSymbol = nullptr;
              objectFile->mEsds[objectFile->mTopEsd].mAddress = start;
              objectFile->mEsds[objectFile->mTopEsd].mOutAddress = objectFile->mEsds[objectFile->mTopEsd].mAddress;
              objectFile->mTopEsd++;
              }

            break;
            }
          //}}}
          //{{{
          case  1: { // common section xx
            string symbolName = getSymbolName();
            uint32_t size = getUint32();

            if (pass1) {
              linker.incCommonDefs();
              if (kPassDebug || kEsdDebug)
                print ("commonSection:{} {} size:{:x}\n", (int)section, symbolName, size);

              bool found;
              cSymbol* symbol = linker.findCreateSymbol (symbolName, found);
              symbol->addReference (linker.getCurrentModuleName());

              if (symbol->mDefined) {
                //{{{  check for common symbol redefinition
                if (size != symbol->mCommonSize) {
                  if (!symbol->mErrorFlagged && !symbol->mCommonSizeDefined) {
                    print ("Common symbol {} is defined twice\n", symbolName);
                    print ("- common in {}\n", linker.getCurrentModuleName());
                    print ("- xDef in {}\n", symbol->mModuleName);
                    symbol->mErrorFlagged = true;
                    }
                  else if (!symbol->mErrorFlagged) {
                    // check
                    print ("Common area size clash - common {}\n ", symbolName);
                    print ("- in {} size:{}\n", linker.getCurrentModuleName(), int(size));
                    print ("- in {} size:{}\n", symbol->mModuleName, int(symbol->mCommonSize));
                    symbol->mErrorFlagged = true;
                    }

                  if (symbol->mCommonSizeDefined && (size > symbol->mCommonSize)) {
                    symbol->mModuleName = linker.getCurrentModuleName();
                    symbol->mCommonSize = size;
                    symbol->mCommonSizeDefined = true;
                    }
                  }
                }
                //}}}
              else {
                // define common section
                symbol->mDefined = true;
                symbol->mModuleName = linker.getCurrentModuleName();
                symbol->mSection = section;
                symbol->mCommonSize = size;
                linker.addCommonSymbol (symbol);
                }
              }
            else {
              // pass2
              cSymbol* symbol = linker.findSymbol (symbolName);
              if (!symbol)
                print (fg (color::orange_red), "error - common symbol not found on pass2 {}\n", symbolName);

              objectFile->mEsds[objectFile->mTopEsd].mAddress = symbol->mAddress + linker.mSections[symbol->mSection].mBaseAddress;
              objectFile->mEsds[objectFile->mTopEsd].mSymbol = symbol;
              objectFile->mEsds[objectFile->mTopEsd].mOutAddress = objectFile->mEsds[objectFile->mTopEsd].mAddress;
              objectFile->mTopEsd++;
              }

            break;
            }
          //}}}

          case 2:         // standard relocatable section xx
          //{{{
          case  3: { // short address relocatable section xx
            uint32_t size = getUint32();

            if (pass1) {
              if (kEsdDebug)
                print ("Relocatable section:{:2d} size:{:x}\n", (int)section, size);

              linker.mSections[section].mSectBase += size;
              if (linker.mSections[section].mSectBase & 1)
                linker.mSections[section].mSectBase++;
              }
            else {
              // pass2 - set esd info
              objectFile->mEsds [objectFile->mTopEsd].mSymbol = nullptr;
              objectFile->mEsds[section+1].mAddress = linker.mSections[section].mBaseAddress +
                                                      linker.mSections[section].mSbase;
              objectFile->mEsds[section+1].mOutAddress = objectFile->mEsds[section+1].mAddress;

              // not sure why this is done again ???
              linker.mSections[section].mSbase += size;
              if (linker.mSections[section].mSbase & 1)
                linker.mSections[section].mSbase++;
              }

            break;
            }
          //}}}

          case 4:         // xDef symbol in relocatble section xx
          //{{{
          case  5: { // xDef symbol in absolute section
            string symbolName = getSymbolName();
            uint32_t address = getUint32();

            if (pass1) {
              if (kXdefDebug)
                print ("symbol xDef - section:{:2d} module:{} symbol:{} address:{:x}\n",
                       section, linker.getCurrentModuleName(), symbolName, address);

              linker.incXdefs();

              bool found;
              cSymbol* symbol = linker.findCreateSymbol (symbolName, found);
              if (kPassDebug)
                print ("symbol xDef {} - section:{:2d} module:{} {} symbol:{} address:{:x}\n",
                       found ? "redefined": "",
                       (int)section, linker.getCurrentModuleName(), found ? symbol->mModuleName : "",
                       symbolName, address);

              symbol->mDefined = true;
              symbol->mModuleName = linker.getCurrentModuleName();

              if (esdType == 5) {
                // symbol in absolute section
                symbol->mSection = -1;
                symbol->mAddress = address + linker.mAbsoluteSection.mSectBase;
                }
              else {
                symbol->mSection = section;
                symbol->mAddress = address + linker.mSections[section].mSectBase;
                }
              }

            break;
            }
          //}}}

          case 6:         // xRef symbol to section xx - unexpected
            print ("xRef to specified section:{}\n", int(section));
            [[fallthrough]];
          //{{{
          case  7: { // xRef symbol to any section
            string symbolName = getSymbolName();


            if (pass1) {
              if (kPassDebug || kXrefDebug)
                print ("symbol xRef - section:{:2d} {:8s}\n", (int)section, symbolName);

              linker.incXrefs();

              // find, or create symbol undefined
              bool found;
              cSymbol* symbol = linker.findCreateSymbol (symbolName, found);

              // add reference to symbol's references
              symbol->addReference (linker.getCurrentModuleName());
              }

            else {
              // pass2
              cSymbol* symbol = linker.findSymbol (symbolName);
              if (symbol) {
                // set esd info
                objectFile->mEsds[objectFile->mTopEsd].mSymbol = symbol;
                objectFile->mEsds[objectFile->mTopEsd].mAddress = symbol->mAddress + linker.mSections[symbol->mSection].mBaseAddress;
                objectFile->mEsds[objectFile->mTopEsd].mOutAddress = objectFile->mEsds[objectFile->mTopEsd].mAddress;
                objectFile->mTopEsd++;
                }
              else
                print (fg (color::orange_red), "error - xRef symbol not found in pass2 {}\n", symbolName);
              }

            break;
            }
          //}}}

          //{{{
          case  8: { // commandLineAddress in section - unexpected
            if (kPassDebug)
              print ("command Line address section\n");

            for (int i = 0; i < 15; i++)
              getUint8();

            break;
            }
          //}}}
          //{{{
          case  9: { // commandLineAddress in absolute section - unexpected
            if (kPassDebug)
              print ("command Line address absolute section\n");

            for (int i = 0; i < 5; i++)
              getUint8();

            break;
            }
          //}}}
          //{{{
          case 10: { // commandLineAddress in common section in section xx - unexpected
            if (kPassDebug)
              print ("command Line address common section\n");

            for (int i = 0; i < 15; i++)
              getUint8();

            break;
            }
          //}}}
          //{{{
          default:
            if (pass1)
              print ("unknown EsdType {}\n", esdType);
          //}}}
          }
        }
      }
    //}}}
    //{{{
    void parseText (cObjectFile* objectFile, cLinker& linker, cOutput& output) {
    // parse text record, to out stream, pass 2 only

      if (kOutDebug)
        dump();

      uint32_t bitmap = getUint32();
      uint8_t currentEsd = getUint8();
      uint32_t codeStart = objectFile->mEsds[currentEsd].mOutAddress;

      if (kOutDebug)
        print ("output bitmap:{:x} currentEsd:{:d}\n", bitmap, currentEsd);

      output.init();

      uint8_t thisEsd = 0;
      while (getBytesLeft() > 0) {
        if (bitmap & 0x80000000) {
          //{{{  relocation data
          uint8_t byte = getUint8();
          uint8_t numEsds = byte >> 5;
          uint8_t offsetFieldLength = byte & 0x07;
          bool longAddress = (byte >> 3) & 1;

          if (kOutDebug)
            print ("byte:{:2x} numEsds:{} offsetFieldLength:{} longAddress:{}\n",
                   byte, numEsds, offsetFieldLength, longAddress);

          uint32_t add = 0;
          for (int i = 1; i <= numEsds; i++) {
            thisEsd = getUint8();
            if (kOutDebug)
              print ("thisEsd:{} topEsd:{}\n", thisEsd, objectFile->mTopEsd);
            if (thisEsd > objectFile->mTopEsd)
              //{{{  error, using esd greater than topEsd
              print (fg (color::orange_red), "error - {} using esd:%{}greater than topEsd:{}\n",
                     linker.getCurrentModuleName(), thisEsd, objectFile->mTopEsd);
              //}}}

            if (i & 0x1)
              add = add + objectFile->mEsds[thisEsd].mAddress;
            else
              add = add - objectFile->mEsds[thisEsd].mAddress;
            }

          // get offset, either byte or word, sign extend
          uint32_t offset = 0;
          for (int i = 1; i <= offsetFieldLength; i++)
            offset = (offset << 8) + getUint8();

          switch (offsetFieldLength) {
            case 0: break;
            case 1: if (offset & 0x80)
                      offset = 0xFFFFFF00 | offset;
                    break;
            case 2: if (offset & 0x8000)
                      offset = 0xFFFF0000 | offset;
                     break;
            case 4: break;
            default: print (fg (color::orange_red), "error - unexpected offsetFieldLength:{}\n", offsetFieldLength);
            }

          if (kOutDebug)
            print ("offset {:x} + {:x} = {:x}\n", add, offset, add + offset);

          add = add + offset;
          if (numEsds == 0) {
            //{{{  not sure what this does
            if (offset & 0x01) {
              print (fg (color::orange_red), "error - {} odd fix-up offset:{:8x} esd:{}, codeStart:{:8x}\n",
                     linker.getCurrentModuleName().c_str(), offset, currentEsd, codeStart);
              offset = offset + 1;
              }

            uint32_t codeBytes = output.outputCode (codeStart);
            objectFile->mEsds[currentEsd].mOutAddress = objectFile->mEsds[currentEsd].mOutAddress + codeBytes + offset;

            output.init();
            codeStart = objectFile->mEsds[currentEsd].mOutAddress;
            }
            //}}}
          else {
            if (!longAddress && (add & 0xFFFF0000))
              print (fg (color::orange_red), "error - trying to put long address into word location:%8x\n", add);

            if (objectFile->mEsds [thisEsd].mSymbol != nullptr) {
              // only need named symbols
              if (linker.getCurrentModuleName() != objectFile->mEsds[thisEsd].mSymbol->mModuleName) {
                //{{{  outside module
                //if history then
                //  { address to be resolved longAddress only at present}
                //  addRes (esdSymbolArray [thisesd], codeStart + mCodeLen*2, offset);

                //if debugInfo then
                //  writeln ('sym ', longAddress,
                //           ' ', thisesd:2,
                //           ' ', esdSymbolArray [thisesd]^.symbolName,
                //           ' ', hex (add, 8, 8), ' = ', hex (esdArray[thisesd]),
                //           ' + ', hex (offset, 4, 4), ';', hex (offsetFieldLength, 1, 1),
                //           ' at ', hex (codeStart + mCodeLen * 2, 8, 8));
                }
                //}}}

              // generate resolved address
              if (longAddress)
                output.addWord (add >> 16);
              output.addWord (add);
              }
            }
          }
          //}}}
        else {
          //{{{  absolute code
          uint16_t codeWord = getUint8() << 8;
          codeWord |= getUint8();
          output.addWord (codeWord);
          }
          //}}}
        bitmap = bitmap << 1;
        }

      objectFile->mEsds[currentEsd].mOutAddress += output.outputCode (codeStart);
      }
    //}}}

    // dump
    //{{{
    void dump() {

      print ("record length:0x{:x} header:{}\n", mLength, mHeader);

      // dump block
      int i = 0;
      while (i < mLength-1) {
        if ((i % 32) == 0) // indent
          print ("  {:2x}  ", i);
        if ((i % 32) == 16)
          print (" ");
        print ("{:2x} ", mBlock[i]);
        if ((i % 32) == 31)
          print ("\n");
        i++;
        }

      if (i % 32)
        print ("\n");
      }
    //}}}

  private:
    uint32_t mFileIndex = 0;

    int mLength = 0;
    uint8_t mHeader = 0;
    int mBlockIndex = 0;
    array <uint8_t,256> mBlock = { 0 };
    };
  //}}}

  //{{{
  void incIdRecords() {
    mNumIdRecords++;
    }
  //}}}
  //{{{
  void incEsdRecords() {
    mNumEsdRecords++;
    }
  //}}}
  //{{{
  void incTxtRecords() {
    mNumTxtRecords++;
    }
  //}}}
  //{{{
  void incEndRecords() {
    mNumEndRecords++;
    }
  //}}}

  const string mFileName;
  eType mType;

  uint32_t mNumIdRecords = 0;
  uint32_t mNumEsdRecords = 0;
  uint32_t mNumTxtRecords = 0;
  uint32_t mNumEndRecords = 0;

  bool mErrorFlagged = false;
  };
//}}}

//{{{
void parseCmdStream (ifstream& stream, vector <cObjectFile>& objectFiles, cLinker& linker) {

  string line;
  while (getline (stream, line))
    if (line[0] == '/')
      linker.parseOptions (line);
    else if (line[0] == '@') {
      //{{{  include
      string includeFileName = line.substr (1, line.length()-1) + ".cmd";
      print ("including file {}\n", includeFileName);

      ifstream includeStream (includeFileName, ifstream::in);
      if (!includeStream.is_open())
        print (fg (color::orange_red), "error - include file {} not found\n", includeFileName);
      else {
        parseCmdStream (includeStream, objectFiles, linker);
        includeStream.close();
        }
      }
      //}}}
    else if (line[0] == '!') {
      //{{{  comment
      if (kCmdLineDebug)
        print ("comment {}\n", line);
      }
      //}}}
    else if (line[0] == '#') {
      //{{{  comment
      if (kCmdLineDebug)
        print ("comment {}\n", line);
      }
      //}}}
    else {
      //{{{  objectFileName
      if (kObjectFileDebug)
        print ("objectFileName {}\n", line);

      // find any trailing comma
      size_t terminatorPos = line.find (',');
      if (terminatorPos == string::npos) // no comma, find any trailing cr in linux file i/o
        terminatorPos = line.find ('\r');

      // look for extension dot, assumes only one dot, !!! could search backwards !!!
      size_t dotPos = line.find ('.');
      if (dotPos == string::npos) // no dot, use default .ro extension
        objectFiles.push_back (cObjectFile (line.substr (0, terminatorPos), "ro"));
      else {
        string fileRootName = line.substr (0, dotPos);
        string extension = line.substr (dotPos+1, terminatorPos - dotPos-1);
        if (kObjectFileDebug)
          print ("extension {} {} {}\n", line, fileRootName, extension);
        objectFiles.push_back (cObjectFile (fileRootName, extension));
        }

      linker.incObjectFiles();
      }
      //}}}
  }
//}}}
//{{{
int main (int numArgs, char* args[]) {

  print (fg (color::white), "ql - 68K linker\n");

  cLinker linker;

  // get commandLine args
  string cmdFileName;
  for (int i = 1; i < numArgs; i++)
    if (args[i][0] == '/')
      linker.parseOptions (args[i]);
    else
      cmdFileName = args[i];

  if (cmdFileName.empty()) {
    //{{{  no .cmd filename - error, exit
    print (fg (color::orange_red), "no .cmd file specified\n");
    return 1;
    }
    //}}}

  print (fg (color::orange), "using cmdFileName {}\n", cmdFileName);

  vector <cObjectFile> objectFiles;

  // read option,objectFiles from .cmd file
  ifstream cmdStream (cmdFileName + ".cmd", ifstream::in);
  if (!cmdStream.is_open()) {
    //{{{  error, return
    print (fg (color::orange_red), "error - cmd file {} not found\n", cmdFileName);
    return 1;
    }
    //}}}
  parseCmdStream (cmdStream, objectFiles, linker);
  cmdStream.close();

  // show options
  linker.dumpOptions();

  // pass 1 - read symbols, accumulate section sizes
  for (auto& objectFile : objectFiles)
    objectFile.pass1 (linker);

  // allocate sections
  linker.allocCommonSections();
  linker.dumpSections();
  linker.dumpSummary();

  // pass 2 - resolve addresses, output .sr or .bin
  if (linker.getEnabled (eBin) || linker.getEnabled (eSr)) {
    cOutput output (cmdFileName, linker.getEnabled(eBin), linker.getEnabled (eEscape));
    for (auto& objectFile : objectFiles)
      objectFile.pass2 (linker, output);
    }
  print (fg (color::orange), "pass2 done\n");

  if (linker.getEnabled (eXrf))
    linker.dumpReferences (cmdFileName);

  return 0;
  }
//}}}
