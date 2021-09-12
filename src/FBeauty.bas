' This is file FBeauty.bas, version 1.04
' Licence: GPLv3
' (C) 2010-2021 Thomas[ dOt }Freiherr{ at }gmx{ DoT ]net
'
' This program is free software; you can redistribute it
' and/or modify it under the terms of the GNU General Public
' License as published by the Free Software Foundation; either
' version 3 of the License, or (at your option) ANY later version.
'
' This program is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
' Lesser General Public License for more details, refer to:
' http://www.gnu.org/licenses/gpl-3.0.html

'#DEFINE DIALECTS ' support for  keywords related to FB dialects
'#DEFINE THISFIX  ' support for THIS./@THIS->

' evaluate command line   / Kommandozeile auswerten
DIM SHARED AS INTEGER Modus
Modus = IIF(INSTR(COMMAND, "-l"), 1, _ '    lower case keywords
        IIF(INSTR(COMMAND, "-c"), 2, _ '   capitalized keywords
        IIF(INSTR(COMMAND, "-i"), 3, 0))) ' individual keywords
' no options = upper case / keine Option = Grossbuchstaben

DIM SHARED  AS UBYTE Char, Check(255)
DIM SHARED AS STRING Wort

FUNCTION Cases(BYREF W AS STRING, BYVAL I AS ZSTRING PTR) AS STRING
  SELECT CASE AS CONST Modus
  CASE 0 : RETURN W
  CASE 1 : RETURN LCASE(W)
  CASE 2
    IF W[0] < ASC("A") THEN RETURN LEFT(W, 2) & LCASE(MID(W, 3))
    RETURN LEFT(W, 1) & LCASE(MID(W, 2))
  END SELECT : RETURN (*I)
END FUNCTION

FUNCTION Change(BYREF T AS STRING) AS STRING
  VAR w = UCASE(T)
  SELECT CASE AS CONST w[0]
    CASE ASC("_")
      SELECT CASE w
        CASE               "__DATE__" : RETURN Cases(w, @"__Date__")
        CASE           "__DATE_ISO__" : RETURN Cases(w, @"__Date_Iso__")
        CASE           "__FB_64BIT__" : RETURN Cases(w, @"__Fb_64Bit__")
        CASE       "__FB_ARG_COUNT__" : RETURN Cases(w, @"__Fb_Arg_Count__")
        CASE     "__FB_ARG_EXTRACT__" : RETURN Cases(w, @"__Fb_Arg_Extract__")
        CASE      "__FB_ARG_LEFTOF__" : RETURN Cases(w, @"__Fb_Arg_Leftof__")
        CASE     "__FB_ARG_RIGHTOF__" : RETURN Cases(w, @"__Fb_Arg_Rightof__")
        CASE            "__FB_ARGC__" : RETURN Cases(w, @"__Fb_ArgC__")
        CASE            "__FB_ARGV__" : RETURN Cases(w, @"__Fb_ArgV__")
        CASE             "__FB_ARM__" : RETURN Cases(w, @"__Fb_Arm__")
        CASE             "__FB_ASM__" : RETURN Cases(w, @"__Fb_Asm__")
        CASE         "__FB_BACKEND__" : RETURN Cases(w, @"__Fb_Backend__")
        CASE       "__FB_BIGENDIAN__" : RETURN Cases(w, @"__Fb_BigEndian__")
        CASE      "__FB_BUILD_DATE__" : RETURN Cases(w, @"__Fb_Build_Date__")
        CASE  "__FB_BUILD_DATE_ISO__" : RETURN Cases(w, @"__Fb_Build_Date_Iso__")
        CASE      "__FB_BUILD_SHA1__" : RETURN Cases(w, @"__Fb_Build_Sha1__")
        CASE          "__FB_CYGWIN__" : RETURN Cases(w, @"__Fb_CygWin__")
        CASE          "__FB_DARWIN__" : RETURN Cases(w, @"__Fb_DarWin__")
        CASE           "__FB_DEBUG__" : RETURN Cases(w, @"__Fb_Debug__")
        CASE             "__FB_DOS__" : RETURN Cases(w, @"__Fb_Dos__")
        CASE             "__FB_ERR__" : RETURN Cases(w, @"__Fb_Err__")
        CASE            "__FB_EVAL__" : RETURN Cases(w, @"__Fb_Eval__")
        CASE          "__FB_FPMODE__" : RETURN Cases(w, @"__Fb_FpMode__")
        CASE             "__FB_FPU__" : RETURN Cases(w, @"__Fb_Fpu__")
        CASE         "__FB_FREEBSD__" : RETURN Cases(w, @"__Fb_FreeBsd__")
        CASE             "__FB_GCC__" : RETURN Cases(w, @"__Fb_Gcc__")
        CASE             "__FB_GUI__" : RETURN Cases(w, @"__Fb_Gui__")
        CASE            "__FB_JOIN__" : RETURN Cases(w, @"__Fb_Join__")
        CASE            "__FB_LANG__" : RETURN Cases(w, @"__Fb_Lang__")
        CASE           "__FB_LINUX__" : RETURN Cases(w, @"__Fb_Linux__")
        CASE            "__FB_MAIN__" : RETURN Cases(w, @"__Fb_Main__")
        CASE     "__FB_MIN_VERSION__" : RETURN Cases(w, @"__Fb_Min_Version__")
        CASE              "__FB_MT__" : RETURN Cases(w, @"__Fb_Mt__")
        CASE          "__FB_NETBSD__" : RETURN Cases(w, @"__Fb_NetBsd__")
        CASE         "__FB_OPENBSD__" : RETURN Cases(w, @"__Fb_OpenBsd__")
        CASE    "__FB_OPTION_BYVAL__" : RETURN Cases(w, @"__Fb_Option_ByVal__")
        CASE  "__FB_OPTION_DYNAMIC__" : RETURN Cases(w, @"__Fb_Option_Dynamic__")
        CASE   "__FB_OPTION_ESCAPE__" : RETURN Cases(w, @"__Fb_Option_Escape__")
        CASE "__FB_OPTION_EXPLICIT__" : RETURN Cases(w, @"__Fb_Option_Explicit__")
        CASE    "__FB_OPTION_GOSUB__" : RETURN Cases(w, @"__Fb_Option_Gosub__")
        CASE  "__FB_OPTION_PRIVATE__" : RETURN Cases(w, @"__Fb_Option_Private__")
        CASE        "__FB_OPTIMIZE__" : RETURN Cases(w, @"__Fb_Optimize__")
        CASE         "__FB_OUT_DLL__" : RETURN Cases(w, @"__Fb_Out_Dll__")
        CASE         "__FB_OUT_EXE__" : RETURN Cases(w, @"__Fb_Out_Exe__")
        CASE         "__FB_OUT_LIB__" : RETURN Cases(w, @"__Fb_Out_Lib__")
        CASE         "__FB_OUT_OBJ__" : RETURN Cases(w, @"__Fb_Out_Obj__")
        CASE            "__FB_PCOS__" : RETURN Cases(w, @"__Fb_Pcos__")
        CASE             "__FB_PPC__" : RETURN Cases(w, @"__Fb_Ppc__")
        CASE           "__FB_QUOTE__" : RETURN Cases(w, @"__Fb_Quote__")
        CASE       "__FB_SIGNATURE__" : RETURN Cases(w, @"__Fb_Signature__")
        CASE             "__FB_SSE__" : RETURN Cases(w, @"__Fb_Sse__")
        CASE            "__FB_UNIX__" : RETURN Cases(w, @"__Fb_Unix__")
        CASE   "__FB_UNIQUEID_PUSH__" : RETURN Cases(w, @"__Fb_Uniqueid_Push__")
        CASE    "__FB_UNIQUEID_POP__" : RETURN Cases(w, @"__Fb_Uniqueid_Pop__")
        CASE        "__FB_UNIQUEID__" : RETURN Cases(w, @"__Fb_Uniqueid__")
        CASE         "__FB_UNQUOTE__" : RETURN Cases(w, @"__Fb_Unquote__")
        CASE       "__FB_VECTORIZE__" : RETURN Cases(w, @"__Fb_Vectorize__")
        CASE       "__FB_VER_MAJOR__" : RETURN Cases(w, @"__Fb_Ver_Major__")
        CASE       "__FB_VER_MINOR__" : RETURN Cases(w, @"__Fb_Ver_Minor__")
        CASE       "__FB_VER_PATCH__" : RETURN Cases(w, @"__Fb_Ver_Patch__")
        CASE         "__FB_VERSION__" : RETURN Cases(w, @"__Fb_Version__")
        CASE           "__FB_WIN32__" : RETURN Cases(w, @"__Fb_Win32__")
        CASE            "__FB_XBOX__" : RETURN Cases(w, @"__Fb_XBox__")
        CASE               "__FILE__" : RETURN Cases(w, @"__File__")
        CASE            "__FILE_NQ__" : RETURN Cases(w, @"__File_NQ__")
        CASE           "__FUNCTION__" : RETURN Cases(w, @"__Function__")
        CASE        "__FUNCTION_NQ__" : RETURN Cases(w, @"__Function_NQ__")
        CASE               "__LINE__" : RETURN Cases(w, @"__Line__")
        CASE               "__PATH__" : RETURN Cases(w, @"__Path__")
        CASE               "__TIME__" : RETURN Cases(w, @"__Time__")
        CASE             "__FB_X86__" : RETURN Cases(w, @"__Fb_X86__")
      END SELECT
    CASE ASC("#")
      SELECT CASE w
        CASE   "#ASSERT" : RETURN Cases(w, @"#Assert")
        CASE  "#CMDLINE" : RETURN Cases(w, @"#Cmdline")
        CASE      "#DEF" : RETURN Cases(w, @"#Def")
        CASE   "#DEFINE" : RETURN Cases(w, @"#Define")
        CASE     "#ELSE" : RETURN Cases(w, @"#Else")
        CASE   "#ELSEIF" : RETURN Cases(w, @"#ElseIf")
        CASE    "#ENDIF" : RETURN Cases(w, @"#EndIf")
        CASE "#ENDMACRO" : RETURN Cases(w, @"#EndMacro")
        CASE    "#ERROR" : RETURN Cases(w, @"#Error")
        CASE       "#IF" : RETURN Cases(w, @"#If")
        CASE    "#IFDEF" : RETURN Cases(w, @"#IfDef")
        CASE   "#IFNDEF" : RETURN Cases(w, @"#IfNDef")
        CASE  "#INCLUDE" : RETURN Cases(w, @"#Include")
        CASE   "#INCLIB" : RETURN Cases(w, @"#IncLib")
        CASE     "#LANG" : RETURN Cases(w, @"#Lang")
        CASE  "#LIBPATH" : RETURN Cases(w, @"#LibPath")
        CASE     "#LINE" : RETURN Cases(w, @"#Line")
        CASE    "#MACRO" : RETURN Cases(w, @"#Macro")
        CASE   "#PRAGMA" : RETURN Cases(w, @"#Pragma")
        CASE    "#PRINT" : RETURN Cases(w, @"#Print")
        CASE    "#UNDEF" : RETURN Cases(w, @"#UnDef")
      END SELECT
#IFDEF DIALECTS
    CASE ASC("$")
      SELECT CASE w
        CASE "$INCLUDE" : RETURN Cases(w, @"$Include")
        CASE "$DYNAMIC" : RETURN Cases(w, @"$Dynamic")
        CASE    "$LANG" : RETURN Cases(w, @"$Lang")
        CASE  "$STATIC" : RETURN Cases(w, @"$Static")
      END SELECT
#ENDIF
    CASE ASC("A")
      SELECT CASE w
        CASE        "ABS" : RETURN Cases(w, @"Abs")
        CASE   "ABSTRACT" : RETURN Cases(w, @"Abstract")
        CASE     "ACCESS" : RETURN Cases(w, @"Access")
        CASE       "ACOS" : RETURN Cases(w, @"ACos")
        CASE        "ADD" : RETURN Cases(w, @"Add")
        CASE      "ALIAS" : RETURN Cases(w, @"Alias")
        CASE   "ALLOCATE" : RETURN Cases(w, @"Allocate")
        CASE      "ALPHA" : RETURN Cases(w, @"Alpha")
        CASE        "AND" : RETURN Cases(w, @"And")
        CASE    "ANDALSO" : RETURN Cases(w, @"AndAlso")
        CASE        "ANY" : RETURN Cases(w, @"Any")
        CASE     "APPEND" : RETURN Cases(w, @"Append")
        CASE         "AS" : RETURN Cases(w, @"As")
        CASE        "ASC" : RETURN Cases(w, @"Asc")
        CASE       "ASIN" : RETURN Cases(w, @"ASin")
        CASE        "ASM" : RETURN Cases(w, @"Asm")
        CASE     "ASSERT" : RETURN Cases(w, @"Assert")
        CASE "ASSERTWARN" : RETURN Cases(w, @"AssertWarn")
        CASE      "ATAN2" : RETURN Cases(w, @"ATan2")
        CASE        "ATN" : RETURN Cases(w, @"Atn")
      END SELECT
    CASE ASC("B")
      SELECT CASE w
        CASE     "BEEP" : RETURN Cases(w, @"Beep")
        CASE      "BIN" : RETURN Cases(w, @"Bin")
        CASE   "BINARY" : RETURN Cases(w, @"Binary")
        CASE      "BIT" : RETURN Cases(w, @"Bit")
        CASE "BITRESET" : RETURN Cases(w, @"BitReSet")
        CASE   "BITSET" : RETURN Cases(w, @"BitSet")
        CASE    "BLOAD" : RETURN Cases(w, @"BLoad")
        CASE  "BOOLEAN" : RETURN Cases(w, @"Boolean")
        CASE    "BSAVE" : RETURN Cases(w, @"BSave")
        CASE     "BYTE" : RETURN Cases(w, @"Byte")
        CASE    "BYREF" : RETURN Cases(w, @"ByRef")
        CASE    "BYVAL" : RETURN Cases(w, @"ByVal")
        CASE     "BASE" : RETURN Cases(w, @"Base")
      END SELECT
    CASE ASC("C")
      SELECT CASE w
        CASE          "CALL" : RETURN Cases(w, @"Call")
        CASE     "CALLOCATE" : RETURN Cases(w, @"CAllocate")
        CASE         "CALLS" : RETURN Cases(w, @"Calls")
        CASE          "CASE" : RETURN Cases(w, @"Case")
        CASE          "CAST" : RETURN Cases(w, @"Cast")
        CASE         "CBOOL" : RETURN Cases(w, @"Cbool")
        CASE         "CBYTE" : RETURN Cases(w, @"CByte")
        CASE          "CDBL" : RETURN Cases(w, @"CDbl")
        CASE         "CDECL" : RETURN Cases(w, @"CDecl")
        CASE         "CHAIN" : RETURN Cases(w, @"Chain")
        CASE         "CHDIR" : RETURN Cases(w, @"ChDir")
        CASE           "CHR" : RETURN Cases(w, @"Chr")
        CASE          "CINT" : RETURN Cases(w, @"CInt")
        CASE        "CIRCLE" : RETURN Cases(w, @"Circle")
        CASE         "CLASS" : RETURN Cases(w, @"Class")
        CASE         "CLEAR" : RETURN Cases(w, @"Clear")
        CASE          "CLNG" : RETURN Cases(w, @"CLng")
        CASE       "CLNGINT" : RETURN Cases(w, @"CLngInt")
        CASE         "CLOSE" : RETURN Cases(w, @"Close")
        CASE           "CLS" : RETURN Cases(w, @"Cls")
        CASE         "COLOR" : RETURN Cases(w, @"Color")
        CASE           "COM" : RETURN Cases(w, @"Com")
        CASE       "COMMAND" : RETURN Cases(w, @"Command")
        CASE        "COMMON" : RETURN Cases(w, @"Common")
        CASE "CONDBROADCAST" : RETURN Cases(w, @"CondBroadCast")
        CASE    "CONDCREATE" : RETURN Cases(w, @"CondCreate")
        CASE   "CONDDESTROY" : RETURN Cases(w, @"CondDestroy")
        CASE    "CONDSIGNAL" : RETURN Cases(w, @"CondSignal")
        CASE      "CONDWAIT" : RETURN Cases(w, @"CondWait")
        CASE          "CONS" : RETURN Cases(w, @"Cons")
        CASE         "CONST" : RETURN Cases(w, @"Const")
        CASE   "CONSTRUCTOR" : RETURN Cases(w, @"Constructor")
        CASE      "CONTINUE" : RETURN Cases(w, @"Continue")
        CASE           "COS" : RETURN Cases(w, @"Cos")
        CASE          "CPTR" : RETURN Cases(w, @"CPtr")
        CASE        "CSHORT" : RETURN Cases(w, @"CShort")
        CASE         "CSIGN" : RETURN Cases(w, @"CSign")
        CASE          "CSNG" : RETURN Cases(w, @"CSng")
        CASE        "CSRLIN" : RETURN Cases(w, @"CsrLin")
        CASE        "CUBYTE" : RETURN Cases(w, @"CUByte")
        CASE         "CUINT" : RETURN Cases(w, @"CUInt")
        CASE         "CULNG" : RETURN Cases(w, @"CULng")
        CASE      "CULNGINT" : RETURN Cases(w, @"CULngInt")
        CASE         "CUNSG" : RETURN Cases(w, @"CUnSg")
        CASE        "CURDIR" : RETURN Cases(w, @"CurDir")
        CASE       "CUSHORT" : RETURN Cases(w, @"CUShort")
        CASE        "CUSTOM" : RETURN Cases(w, @"Custom")
        CASE       "CVA_ARG" : RETURN Cases(w, @"Cva_Arg")
        CASE      "CVA_COPY" : RETURN Cases(w, @"Cva_Copy")
        CASE       "CVA_END" : RETURN Cases(w, @"Cva_End")
        CASE      "CVA_LIST" : RETURN Cases(w, @"Cva_List")
        CASE     "CVA_START" : RETURN Cases(w, @"Cva_Start")
        CASE           "CVD" : RETURN Cases(w, @"CvD")
        CASE           "CVI" : RETURN Cases(w, @"CvI")
        CASE           "CVL" : RETURN Cases(w, @"CvL")
        CASE     "CVLONGINT" : RETURN Cases(w, @"CvLongInt")
        CASE           "CVS" : RETURN Cases(w, @"CvS")
        CASE       "CVSHORT" : RETURN Cases(w, @"CvShort")
      END SELECT
    CASE ASC("D")
      SELECT CASE w
        CASE        "DATA" : RETURN Cases(w, @"Data")
        CASE        "DATE" : RETURN Cases(w, @"Date")
        CASE     "DATEADD" : RETURN Cases(w, @"DateAdd")
        CASE    "DATEDIFF" : RETURN Cases(w, @"DateDiff")
        CASE    "DATEPART" : RETURN Cases(w, @"DatePart")
        CASE  "DATESERIAL" : RETURN Cases(w, @"DateSerial")
        CASE   "DATEVALUE" : RETURN Cases(w, @"DateValue")
        CASE         "DAY" : RETURN Cases(w, @"Day")
        CASE  "DEALLOCATE" : RETURN Cases(w, @"DeAllocate")
        CASE     "DECLARE" : RETURN Cases(w, @"Declare")
        CASE     "DEFBYTE" : RETURN Cases(w, @"DefByte")
        CASE      "DEFDBL" : RETURN Cases(w, @"DefDbl")
        CASE      "DEFINE" : RETURN Cases(w, @"Define")
        CASE     "DEFINED" : RETURN Cases(w, @"Defined")
        CASE      "DEFINT" : RETURN Cases(w, @"DefInt")
        CASE      "DEFLNG" : RETURN Cases(w, @"DefLng")
        CASE  "DEFLONGINT" : RETURN Cases(w, @"DefLongInt")
        CASE    "DEFSHORT" : RETURN Cases(w, @"DefShort")
        CASE      "DEFSNG" : RETURN Cases(w, @"DefSng")
        CASE      "DEFSTR" : RETURN Cases(w, @"DefStr")
        CASE    "DEFUBYTE" : RETURN Cases(w, @"DefUByte")
        CASE     "DEFUINT" : RETURN Cases(w, @"DefUInt")
        CASE "DEFULONGINT" : RETURN Cases(w, @"DefULongInt")
        CASE   "DEFUSHORT" : RETURN Cases(w, @"DefUShort")
        CASE      "DELETE" : RETURN Cases(w, @"Delete")
        CASE  "DESTRUCTOR" : RETURN Cases(w, @"Destructor")
        CASE         "DIM" : RETURN Cases(w, @"Dim")
        CASE         "DIR" : RETURN Cases(w, @"Dir")
        CASE          "DO" : RETURN Cases(w, @"Do")
        CASE      "DOUBLE" : RETURN Cases(w, @"Double")
        CASE        "DRAW" : RETURN Cases(w, @"Draw")
        CASE   "DYLIBFREE" : RETURN Cases(w, @"DyLibFree")
        CASE   "DYLIBLOAD" : RETURN Cases(w, @"DyLibLoad")
        CASE "DYLIBSYMBOL" : RETURN Cases(w, @"DyLibSymbol")
#IFDEF DIALECTS
        CASE "DYNAMIC" : RETURN Cases(w, @"Dynamic")
#ENDIF
      END SELECT
    CASE ASC("E")
      SELECT CASE w
        CASE     "ELSE" : RETURN Cases(w, @"Else")
        CASE   "ELSEIF" : RETURN Cases(w, @"ElseIf")
        CASE "ENCODING" : RETURN Cases(w, @"Encoding")
        CASE      "END" : RETURN Cases(w, @"End")
        CASE    "ENDIF" : RETURN Cases(w, @"EndIf")
        CASE "ENDMACRO" : RETURN Cases(w, @"EndMacro")
        CASE     "ENUM" : RETURN Cases(w, @"Enum")
        CASE  "ENVIRON" : RETURN Cases(w, @"Environ")
        CASE      "EOF" : RETURN Cases(w, @"Eof")
        CASE      "EQV" : RETURN Cases(w, @"Eqv")
        CASE    "ERASE" : RETURN Cases(w, @"Erase")
        CASE     "ERFN" : RETURN Cases(w, @"ErFn")
        CASE      "ERL" : RETURN Cases(w, @"ErL")
        CASE     "ERMN" : RETURN Cases(w, @"ErMn")
        CASE      "ERR" : RETURN Cases(w, @"Err")
        CASE    "ERROR" : RETURN Cases(w, @"Error")
        CASE    "EVENT" : RETURN Cases(w, @"Event")
        CASE   "ESCAPE" : RETURN Cases(w, @"Escape")
        CASE     "EXEC" : RETURN Cases(w, @"Exec")
        CASE  "EXEPATH" : RETURN Cases(w, @"ExePath")
        CASE "EXPLICIT" : RETURN Cases(w, @"Explicit")
        CASE     "EXIT" : RETURN Cases(w, @"Exit")
        CASE      "EXP" : RETURN Cases(w, @"Exp")
        CASE   "EXPORT" : RETURN Cases(w, @"Export")
        CASE  "EXTENDS" : RETURN Cases(w, @"Extends")
        CASE   "EXTERN" : RETURN Cases(w, @"Extern")
#IFDEF DIALECTS
        CASE   "ESCAPE" : RETURN Cases(w, @"Escape")
        CASE "EXPLICIT" : RETURN Cases(w, @"Explicit")
#ENDIF
      END SELECT
    CASE ASC("F")
      SELECT CASE w
        CASE        "FALSE" : RETURN Cases(w, @"False")
        CASE   "FB_MEMCOPY" : RETURN Cases(w, @"Fb_MemCopy")
        CASE "FB_MEMCOPYCLEAR" : RETURN Cases(w, @"Fb_MemCopyClear")
        CASE   "FB_MEMMOVE" : RETURN Cases(w, @"Fb_MemMove")
        CASE      "FBARRAY" : RETURN Cases(w, @"FbArray")
        CASE        "FIELD" : RETURN Cases(w, @"Field")
        CASE     "FILEATTR" : RETURN Cases(w, @"FileAttr")
        CASE     "FILECOPY" : RETURN Cases(w, @"FileCopy")
        CASE "FILEDATETIME" : RETURN Cases(w, @"FileDateTime")
        CASE   "FILEEXISTS" : RETURN Cases(w, @"FileExists")
        CASE    "FILEFLUSH" : RETURN Cases(w, @"FileFlush")
        CASE      "FILELEN" : RETURN Cases(w, @"FileLen")
        CASE   "FILESETEOF" : RETURN Cases(w, @"FileSetEof")
        CASE          "FIX" : RETURN Cases(w, @"Fix")
        CASE         "FLIP" : RETURN Cases(w, @"Flip")
        CASE          "FOR" : RETURN Cases(w, @"For")
        CASE       "FORMAT" : RETURN Cases(w, @"Format")
        CASE         "FRAC" : RETURN Cases(w, @"Frac")
        CASE          "FRE" : RETURN Cases(w, @"Fre")
        CASE     "FREEFILE" : RETURN Cases(w, @"FreeFile")
        CASE     "FUNCTION" : RETURN Cases(w, @"Function")
      END SELECT
    CASE ASC("G")
      SELECT CASE w
        CASE         "GET" : RETURN Cases(w, @"Get")
        CASE "GETJOYSTICK" : RETURN Cases(w, @"GetJoyStick")
        CASE      "GETKEY" : RETURN Cases(w, @"GetKey")
        CASE    "GETMOUSE" : RETURN Cases(w, @"GetMouse")
        CASE       "GOSUB" : RETURN Cases(w, @"GoSub")
        CASE        "GOTO" : RETURN Cases(w, @"GoTo")
      END SELECT
    CASE ASC("H")
      SELECT CASE w
        CASE    "HEX" : RETURN Cases(w, @"Hex")
        CASE "HIBYTE" : RETURN Cases(w, @"HiByte")
        CASE "HIWORD" : RETURN Cases(w, @"HiWord")
        CASE   "HOUR" : RETURN Cases(w, @"Hour")
      END SELECT
    CASE ASC("I")
      SELECT CASE w
        CASE              "IF" : RETURN Cases(w, @"If")
        CASE             "IIF" : RETURN Cases(w, @"IIf")
        CASE "IMAGECONVERTROW" : RETURN Cases(w, @"ImageConvertRow")
        CASE     "IMAGECREATE" : RETURN Cases(w, @"ImageCreate")
        CASE    "IMAGEDESTROY" : RETURN Cases(w, @"ImageDestroy")
        CASE       "IMAGEINFO" : RETURN Cases(w, @"ImageInfo")
        CASE             "IMP" : RETURN Cases(w, @"Imp")
        CASE      "IMPLEMENTS" : RETURN Cases(w, @"Implements")
        CASE          "IMPORT" : RETURN Cases(w, @"Import")
        CASE          "INCLIB" : RETURN Cases(w, @"IncLib")
        CASE         "INCLUDE" : RETURN Cases(w, @"Include")
        CASE           "INKEY" : RETURN Cases(w, @"InKey")
        CASE             "INP" : RETURN Cases(w, @"Inp")
        CASE           "INPUT" : RETURN Cases(w, @"Input")
        CASE           "INSTR" : RETURN Cases(w, @"InStr")
        CASE        "INSTRREV" : RETURN Cases(w, @"InStrRev")
        CASE             "INT" : RETURN Cases(w, @"Int")
        CASE         "INTEGER" : RETURN Cases(w, @"Integer")
        CASE              "IS" : RETURN Cases(w, @"Is")
        CASE          "ISDATE" : RETURN Cases(w, @"IsDate")
        CASE    "ISREDIRECTED" : RETURN Cases(w, @"IsRedirected")
      END SELECT
    CASE ASC("K")
      SELECT CASE w
        CASE "KILL" : RETURN Cases(w, @"Kill")
      END SELECT
    CASE ASC("L")
      SELECT CASE w
        CASE  "LBOUND" : RETURN Cases(w, @"LBound")
        CASE   "LCASE" : RETURN Cases(w, @"LCase")
        CASE    "LEFT" : RETURN Cases(w, @"Left")
        CASE     "LEN" : RETURN Cases(w, @"Len")
        CASE     "LET" : RETURN Cases(w, @"Let")
        CASE     "LIB" : RETURN Cases(w, @"Lib")
        CASE "LIBPATH" : RETURN Cases(w, @"LibPath")
        CASE    "LINE" : RETURN Cases(w, @"Line")
        CASE  "LOBYTE" : RETURN Cases(w, @"LoByte")
        CASE     "LOC" : RETURN Cases(w, @"Loc")
        CASE   "LOCAL" : RETURN Cases(w, @"Local")
        CASE  "LOCATE" : RETURN Cases(w, @"Locate")
        CASE    "LOCK" : RETURN Cases(w, @"Lock")
        CASE     "LOF" : RETURN Cases(w, @"Lof")
        CASE     "LOG" : RETURN Cases(w, @"Log")
        CASE    "LONG" : RETURN Cases(w, @"Long")
        CASE "LONGINT" : RETURN Cases(w, @"LongInt")
        CASE    "LOOP" : RETURN Cases(w, @"Loop")
        CASE  "LOWORD" : RETURN Cases(w, @"LoWord")
        CASE    "LPOS" : RETURN Cases(w, @"LPos")
        CASE  "LPRINT" : RETURN Cases(w, @"LPrint")
        CASE     "LPT" : RETURN Cases(w, @"Lpt")
        CASE    "LSET" : RETURN Cases(w, @"LSet")
        CASE   "LTRIM" : RETURN Cases(w, @"LTrim")
      END SELECT
    CASE ASC("M")
      SELECT CASE w
        CASE          "MID" : RETURN Cases(w, @"Mid")
        CASE       "MINUTE" : RETURN Cases(w, @"Minute")
        CASE          "MKD" : RETURN Cases(w, @"MkD")
        CASE        "MKDIR" : RETURN Cases(w, @"MkDir")
        CASE          "MKI" : RETURN Cases(w, @"MkI")
        CASE          "MKL" : RETURN Cases(w, @"MkL")
        CASE    "MKLONGINT" : RETURN Cases(w, @"MkLongInt")
        CASE          "MKS" : RETURN Cases(w, @"MkS")
        CASE      "MKSHORT" : RETURN Cases(w, @"MkShort")
        CASE          "MOD" : RETURN Cases(w, @"Mod")
        CASE        "MONTH" : RETURN Cases(w, @"Month")
        CASE    "MONTHNAME" : RETURN Cases(w, @"MonthName")
        CASE     "MULTIKEY" : RETURN Cases(w, @"MultiKey")
        CASE  "MUTEXCREATE" : RETURN Cases(w, @"MutexCreate")
        CASE "MUTEXDESTROY" : RETURN Cases(w, @"MutexDestroy")
        CASE    "MUTEXLOCK" : RETURN Cases(w, @"MutexLock")
        CASE  "MUTEXUNLOCK" : RETURN Cases(w, @"MutexUnLock")
      END SELECT
    CASE ASC("N")
      SELECT CASE w
        CASE     "NAKED" : RETURN Cases(w, @"Naked")
        CASE      "NAME" : RETURN Cases(w, @"Name")
        CASE "NAMESPACE" : RETURN Cases(w, @"NameSpace")
        CASE       "NEW" : RETURN Cases(w, @"New")
        CASE      "NEXT" : RETURN Cases(w, @"Next")
        CASE       "NOT" : RETURN Cases(w, @"Not")
        CASE       "NOW" : RETURN Cases(w, @"Now")
#IFDEF DIALECTS
        CASE   "NOGOSUB" : RETURN Cases(w, @"NoGoSub")
        CASE "NOKEYWORD" : RETURN Cases(w, @"NoKeyWord")
#ENDIF
      END SELECT
    CASE ASC("O")
      SELECT CASE w
        CASE   "OBJECT" : RETURN Cases(w, @"Object")
        CASE      "OCT" : RETURN Cases(w, @"Oct")
        CASE "OFFSETOF" : RETURN Cases(w, @"OffsetOf")
        CASE       "ON" : RETURN Cases(w, @"On")
        CASE     "ONCE" : RETURN Cases(w, @"Once")
        CASE     "OPEN" : RETURN Cases(w, @"Open")
        CASE "OPERATOR" : RETURN Cases(w, @"Operator")
        CASE   "OPTION" : RETURN Cases(w, @"Option")
        CASE       "OR" : RETURN Cases(w, @"Or")
        CASE   "ORELSE" : RETURN Cases(w, @"OrElse")
        CASE      "OUT" : RETURN Cases(w, @"Out")
        CASE   "OUTPUT" : RETURN Cases(w, @"Output")
        CASE "OVERLOAD" : RETURN Cases(w, @"Overload")
        CASE "OVERRIDE" : RETURN Cases(w, @"Override")
#IFDEF DIALECTS
        CASE   "OPTION" : RETURN Cases(w, @"Option")
#ENDIF
      END SELECT
    CASE ASC("P")
      SELECT CASE w
        CASE     "PAINT" : RETURN Cases(w, @"Paint")
        CASE   "PALETTE" : RETURN Cases(w, @"Palette")
        CASE    "PASCAL" : RETURN Cases(w, @"Pascal")
        CASE     "PCOPY" : RETURN Cases(w, @"PCopy")
        CASE      "PEEK" : RETURN Cases(w, @"Peek")
        CASE      "PIPE" : RETURN Cases(w, @"Pipe")
        CASE      "PMAP" : RETURN Cases(w, @"PMap")
        CASE     "POINT" : RETURN Cases(w, @"Point")
        CASE"POINTCOORD" : RETURN Cases(w, @"PointCoord")
        CASE   "POINTER" : RETURN Cases(w, @"Pointer")
        CASE      "POKE" : RETURN Cases(w, @"Poke")
        CASE       "POS" : RETURN Cases(w, @"Pos")
        CASE       "POP" : RETURN Cases(w, @"Pop")
        CASE  "PRESERVE" : RETURN Cases(w, @"Preserve")
        CASE    "PRESET" : RETURN Cases(w, @"PreSet")
        CASE     "PRINT" : RETURN Cases(w, @"Print")
        CASE   "PRIVATE" : RETURN Cases(w, @"Private")
        CASE   "PROCPTR" : RETURN Cases(w, @"ProcPtr")
        CASE  "PROPERTY" : RETURN Cases(w, @"Property")
        CASE "PROTECTED" : RETURN Cases(w, @"Protected")
        CASE      "PSET" : RETURN Cases(w, @"PSet")
        CASE       "PTR" : RETURN Cases(w, @"Ptr")
        CASE    "PUBLIC" : RETURN Cases(w, @"Public")
        CASE       "PUT" : RETURN Cases(w, @"Put")
        CASE      "PUSH" : RETURN Cases(w, @"Push")
      END SELECT
    CASE ASC("R")
      SELECT CASE w
        CASE     "RANDOM" : RETURN Cases(w, @"Random")
        CASE  "RANDOMIZE" : RETURN Cases(w, @"Randomize")
        CASE       "READ" : RETURN Cases(w, @"Read")
        CASE "REALLOCATE" : RETURN Cases(w, @"ReAllocate")
        CASE      "REDIM" : RETURN Cases(w, @"ReDim")
        CASE        "REM" : RETURN Cases(w, @"Rem")
        CASE      "RESET" : RETURN Cases(w, @"ReSet")
        CASE    "RESTORE" : RETURN Cases(w, @"ReStore")
        CASE     "RESUME" : RETURN Cases(w, @"ReSume")
        CASE     "RETURN" : RETURN Cases(w, @"Return")
        CASE        "RGB" : RETURN Cases(w, @"RGB")
        CASE       "RGBA" : RETURN Cases(w, @"RGBA")
        CASE      "RIGHT" : RETURN Cases(w, @"Right")
        CASE      "RMDIR" : RETURN Cases(w, @"RmDir")
        CASE        "RND" : RETURN Cases(w, @"Rnd")
        CASE       "RSET" : RETURN Cases(w, @"RSet")
        CASE      "RTRIM" : RETURN Cases(w, @"RTrim")
        CASE        "RUN" : RETURN Cases(w, @"Run")
      END SELECT
    CASE ASC("S")
      SELECT CASE w
        CASE          "SADD" : RETURN Cases(w, @"SAdd")
        CASE         "SCOPE" : RETURN Cases(w, @"Scope")
        CASE        "SCREEN" : RETURN Cases(w, @"Screen")
        CASE "SCREENCONTROL" : RETURN Cases(w, @"ScreenControl")
        CASE    "SCREENCOPY" : RETURN Cases(w, @"ScreenCopy")
        CASE   "SCREENEVENT" : RETURN Cases(w, @"ScreenEvent")
        CASE  "SCREENGLPROC" : RETURN Cases(w, @"ScreenGlProc")
        CASE    "SCREENINFO" : RETURN Cases(w, @"ScreenInfo")
        CASE    "SCREENLIST" : RETURN Cases(w, @"ScreenList")
        CASE    "SCREENLOCK" : RETURN Cases(w, @"ScreenLock")
        CASE     "SCREENPTR" : RETURN Cases(w, @"ScreenPtr")
        CASE     "SCREENRES" : RETURN Cases(w, @"ScreenRes")
        CASE     "SCREENSET" : RETURN Cases(w, @"ScreenSet")
        CASE    "SCREENSYNC" : RETURN Cases(w, @"ScreenSync")
        CASE  "SCREENUNLOCK" : RETURN Cases(w, @"ScreenUnLock")
        CASE          "SCRN" : RETURN Cases(w, @"Scrn")
        CASE        "SECOND" : RETURN Cases(w, @"Second")
        CASE          "SEEK" : RETURN Cases(w, @"Seek")
        CASE        "SELECT" : RETURN Cases(w, @"Select")
        CASE       "SETDATE" : RETURN Cases(w, @"SetDate")
        CASE    "SETENVIRON" : RETURN Cases(w, @"SetEnviron")
        CASE      "SETMOUSE" : RETURN Cases(w, @"SetMouse")
        CASE       "SETTIME" : RETURN Cases(w, @"SetTime")
        CASE           "SGN" : RETURN Cases(w, @"Sgn")
        CASE        "SHARED" : RETURN Cases(w, @"Shared")
        CASE         "SHELL" : RETURN Cases(w, @"Shell")
        CASE           "SHL" : RETURN Cases(w, @"ShL")
        CASE         "SHORT" : RETURN Cases(w, @"Short")
        CASE           "SHR" : RETURN Cases(w, @"ShR")
        CASE           "SIN" : RETURN Cases(w, @"Sin")
        CASE        "SINGLE" : RETURN Cases(w, @"Single")
        CASE        "SIZEOF" : RETURN Cases(w, @"SizeOf")
        CASE         "SLEEP" : RETURN Cases(w, @"Sleep")
        CASE         "SPACE" : RETURN Cases(w, @"Space")
        CASE           "SPC" : RETURN Cases(w, @"Spc")
        CASE           "SQR" : RETURN Cases(w, @"Sqr")
        CASE        "STATIC" : RETURN Cases(w, @"Static")
        CASE       "STDCALL" : RETURN Cases(w, @"StdCall")
        CASE          "STEP" : RETURN Cases(w, @"Step")
        CASE         "STICK" : RETURN Cases(w, @"Stick")
        CASE          "STOP" : RETURN Cases(w, @"Stop")
        CASE           "STR" : RETURN Cases(w, @"Str")
        CASE         "STRIG" : RETURN Cases(w, @"Strig")
        CASE        "STRING" : RETURN Cases(w, @"String")
        CASE        "STRPTR" : RETURN Cases(w, @"StrPtr")
        CASE           "SUB" : RETURN Cases(w, @"Sub")
        CASE          "SWAP" : RETURN Cases(w, @"Swap")
        CASE        "SYSTEM" : RETURN Cases(w, @"System")
      END SELECT
    CASE ASC("T")
      SELECT CASE w
        CASE          "TAB" : RETURN Cases(w, @"Tab")
        CASE          "TAN" : RETURN Cases(w, @"Tan")
        CASE         "THEN" : RETURN Cases(w, @"Then")
        CASE         "THIS" : RETURN Cases(w, @"This")
        CASE     "THISCALL" : RETURN Cases(w, @"ThisCall")
        CASE   "THREADCALL" : RETURN Cases(w, @"Threadcall")
        CASE "THREADCREATE" : RETURN Cases(w, @"ThreadCreate")
        CASE "THREADDETACH" : RETURN Cases(w, @"ThreadDetach")
        CASE   "THREADSELF" : RETURN Cases(w, @"ThreadSelf")
        CASE   "THREADWAIT" : RETURN Cases(w, @"ThreadWait")
        CASE         "TIME" : RETURN Cases(w, @"Time")
        CASE        "TIMER" : RETURN Cases(w, @"Timer")
        CASE   "TIMESERIAL" : RETURN Cases(w, @"TimeSerial")
        CASE    "TIMEVALUE" : RETURN Cases(w, @"TimeValue")
        CASE           "TO" : RETURN Cases(w, @"To")
        CASE        "TRANS" : RETURN Cases(w, @"Trans")
        CASE         "TRIM" : RETURN Cases(w, @"Trim")
        CASE         "TRUE" : RETURN Cases(w, @"True")
        CASE         "TYPE" : RETURN Cases(w, @"Type")
        CASE       "TYPEOF" : RETURN Cases(w, @"TypeOf")
      END SELECT
#IFDEF THISFIX
      IF LEFT(w, 4) = "THIS" THEN
        SELECT CASE AS CONST ASC(w, 5)
        CASE ASC("."), ASC("-") : MID(T, 1, 4) = Cases(LEFT(w, 4), @"This")
        END SELECT
      END IF
#ENDIF
    CASE ASC("U")
      SELECT CASE w
        CASE   "UBOUND" : RETURN Cases(w, @"UBound")
        CASE    "UBYTE" : RETURN Cases(w, @"UByte")
        CASE    "UCASE" : RETURN Cases(w, @"UCase")
        CASE "UINTEGER" : RETURN Cases(w, @"UInteger")
        CASE    "ULONG" : RETURN Cases(w, @"ULong")
        CASE "ULONGINT" : RETURN Cases(w, @"ULongInt")
        CASE    "UNDEF" : RETURN Cases(w, @"UnDef")
        CASE    "UNION" : RETURN Cases(w, @"Union")
        CASE   "UNLOCK" : RETURN Cases(w, @"UnLock")
        CASE "UNSIGNED" : RETURN Cases(w, @"UnSigned")
        CASE    "UNTIL" : RETURN Cases(w, @"Until")
        CASE   "USHORT" : RETURN Cases(w, @"UShort")
        CASE    "USING" : RETURN Cases(w, @"Using")
      END SELECT
    CASE ASC("V")
      SELECT CASE w
        CASE   "VA_ARG" : RETURN Cases(w, @"VA_Arg")
        CASE "VA_FIRST" : RETURN Cases(w, @"VA_First")
        CASE  "VA_NEXT" : RETURN Cases(w, @"VA_Next")
        CASE      "VAL" : RETURN Cases(w, @"Val")
        CASE    "VAL64" : RETURN Cases(w, @"Val64")
        CASE   "VALINT" : RETURN Cases(w, @"ValInt")
        CASE   "VALLNG" : RETURN Cases(w, @"ValLng")
        CASE  "VALUINT" : RETURN Cases(w, @"ValUInt")
        CASE  "VALULNG" : RETURN Cases(w, @"ValULng")
        CASE      "VAR" : RETURN Cases(w, @"Var")
        CASE   "VARPTR" : RETURN Cases(w, @"VarPtr")
        CASE     "VIEW" : RETURN Cases(w, @"View")
        CASE  "VIRTUAL" : RETURN Cases(w, @"Virtual")
      END SELECT
    CASE ASC("W")
      SELECT CASE w
        CASE        "WAIT" : RETURN Cases(w, @"Wait")
        CASE        "WBIN" : RETURN Cases(w, @"WBin")
        CASE        "WCHR" : RETURN Cases(w, @"WChr")
        CASE     "WEEKDAY" : RETURN Cases(w, @"WeekDay")
        CASE "WEEKDAYNAME" : RETURN Cases(w, @"WeekDayName")
        CASE        "WEND" : RETURN Cases(w, @"Wend")
        CASE        "WHEX" : RETURN Cases(w, @"WHex")
        CASE       "WHILE" : RETURN Cases(w, @"While")
        CASE       "WIDTH" : RETURN Cases(w, @"Width")
        CASE      "WINDOW" : RETURN Cases(w, @"Window")
        CASE "WINDOWTITLE" : RETURN Cases(w, @"WindowTitle")
        CASE      "WINPUT" : RETURN Cases(w, @"WInput")
        CASE        "WITH" : RETURN Cases(w, @"With")
        CASE        "WOCT" : RETURN Cases(w, @"WOct")
        CASE       "WRITE" : RETURN Cases(w, @"Write")
        CASE      "WSPACE" : RETURN Cases(w, @"WSpace")
        CASE        "WSTR" : RETURN Cases(w, @"WStr")
        CASE     "WSTRING" : RETURN Cases(w, @"WString")
      END SELECT
    CASE ASC("X")
      SELECT CASE w
        CASE     "XOR" : RETURN Cases(w, @"Xor")
      END SELECT
    CASE ASC("Y")
      SELECT CASE w
        CASE    "YEAR" : RETURN Cases(w, @"Year")
      END SELECT
    CASE ASC("Z")
      SELECT CASE w
        CASE "ZSTRING" : RETURN Cases(w, @"ZString")
      END SELECT
  END SELECT  : RETURN T
END FUNCTION

SUB readTo(BYREF T AS STRING) '        skip a comment or standard STRING
  VAR i = 0, lt = LEN(T)
  WHILE NOT EOF(1)
    GET #1, , Char
    PRINT #2, CHR(Char);
    IF Char <> T[i] THEN i = 0 ELSE i += 1 : IF i = lt THEN EXIT SUB
  WEND
END SUB

FUNCTION readEsc() AS STRING '      check for an escaped STRING, skip it
  IF EOF(1) THEN RETURN ""
  GET #1, , Char
  IF Char <> ASC("""") THEN
    IF Check(Char) = 1 THEN RETURN CHR(Char)
    PRINT #2, CHR(Char); : RETURN ""
  END IF : PRINT #2, CHR(Char);
  VAR i = 0
  WHILE NOT EOF(1)
    GET #1, , Char
    PRINT #2, CHR(Char);
    SELECT CASE AS CONST Char
    CASE ASC("\") : i += 1
    CASE ASC("""") : IF BIT(i, 0) THEN i = 0 ELSE EXIT WHILE
    CASE ELSE : i = 0
    END SELECT
  WEND : RETURN ""
END FUNCTION

' main

' define character types (1 = keyword characters)
Check(ASC("_")) = 1
Check(ASC("-")) = 1
Check(ASC(">")) = 1
Check(ASC(".")) = 1
Check(ASC("$")) = 1
Check(ASC("#")) = 1
Check(ASC("2")) = 1
Check(ASC("3")) = 1
Check(ASC("4")) = 1
Check(ASC("6")) = 1
FOR i AS INTEGER = ASC("A") TO ASC("Z")
  Check(i) = 1
  Check(i + 32) = 1
NEXT
Check(ASC( "!")) = 2 ' escaped STRING
Check(ASC("""")) = 3 ' standard STRING
Check(ASC( "'")) = 4 ' comment
Check(ASC( "/")) = 5 ' may be start of multi line comment
Check(0) = 255

OPEN CONS FOR INPUT AS #1
OPEN CONS FOR OUTPUT AS #2

WHILE NOT EOF(1)
  GET #1, , Char
  SELECT CASE AS CONST Check(Char)
  CASE 1 : Wort &= CHR(Char) '                                a word character
  CASE 255 : EXIT WHILE '                                         end of input
  CASE ELSE '                     evaluate non-word letter. But first, process
    IF LEN(Wort) THEN PRINT #2, Change(Wort); : Wort = "" '       pending word
    PRINT #2, CHR(Char); '                                output the character
    SELECT CASE AS CONST Check(Char) '                                check it
    CASE 2 : Wort &= readEsc() '                          check escaped STRING
    CASE 3 : readTo("""") '                               skip standard STRING
    CASE 4 : readTo(!"\n") '                          skip single line comment
    CASE 5 '                                         may be multi line comment
      IF NOT EOF(1) THEN GET #1, , Char ELSE EXIT WHILE '   get next character
      SELECT CASE AS CONST Check(Char) '                              check it
      CASE 1 : Wort &= CHR(Char) '                            a keyword letter
      CASE 2 : PRINT #2, CHR(Char); : Wort &= readEsc() ' check escaped STRING
      CASE 3 : PRINT #2, CHR(Char); : readTo("""") '      skip standard STRING
      CASE 4 : PRINT #2, CHR(Char); : readTo("'/") '  skip single line comment
      CASE ELSE : PRINT #2, CHR(Char); '          other letters, simply output
      END SELECT
    END SELECT
  END SELECT
WEND : IF LEN(Wort) THEN PRINT #2, Change(Wort); '        process pending word

CLOSE #2
CLOSE #1
