' DEMO S - Two byte map storage

CONST SCREENSIZEX = 512
CONST SCREENSIZEY = 369

' Center position defined at the start of global variables

CONST flashTextClr = 14
CONST highlClr = 14
CONST highlClr2 = 13
CONST hexClr = 244
CONST windowClr = 1
CONST gameBgClr = 0

CONST BYTEZERO = 0

CONST palRED = 0
CONST palGREEN = 1
CONST palBLUE = 2

CONST PALSTD = 16
CONST PALNONE = 17 ' For drawing a low-color tile without a palette

CONST NORMAL = 0 ' For assistance with drawing pattern tables
CONST TIMESTWO = 1
CONST TIMESTHREE = 2

CONST NOWRAP = 0
CONST WRAPOK = 1

CONST NOFLIP = 0
CONST FLIPH = 1
CONST FLIPV = 2
CONST FLIPHV = 3

CONST TABLE0 = 0
CONST TABLE1 = 1
CONST TABLE2 = 2
CONST TABLE3 = 3

CONST SOUTH = 0: CONST WEST = 4: CONST EAST = 6: CONST NORTH = 8
' Normally would use 2 for SOUTH, but I want to catch any zero variables

CONST mBlockBlank = 0 ' Map blocks, the codes loaded directly from the file
'CONST mBlockHeart = 258
'CONST mBlockChest = 260
CONST mBlockTree = 16
CONST mBlockStatue = 17
CONST mBlockWall = 18

CONST objHero = 0

CONST gBlockGround = 0 ' Golem blocks, the 2 x 2 tile graphic images, doesn't always mBlock values
CONST gBlockTree = 16
CONST gBlockStatue = 17
CONST gBlockRock = 18
CONST gBlockWall = 19
CONST gBlockChest = 7

CONST objInvalid = 0
CONST objBush = 1
CONST objGrave = 2

CONST viewStartPixelsX = 8 ' Where the playfield starts on the screen
CONST viewStartPixelsY = 8

'''' Visual setup ''''''''''''''''''''''''''''''''
_FULLSCREEN
SCREEN _NEWIMAGE(SCREENSIZEX, SCREENSIZEY, 13), 1, 0 ' The 13 mimics the SCREEN 13 mode which allows 256 colors
_FONT 8 ' QB64 specific command to ensure the font is 8 x 8 in this mode
''''''''''''''''''''''''''''''''

'''' Global arrays

TYPE dataTypeHero ' Hero is separate from the other objects
  hTrueX AS INTEGER
  hTrueY AS INTEGER
  ScreenX AS INTEGER
  ScreenY AS INTEGER
  Dir AS _UNSIGNED _BYTE
END TYPE: DIM SHARED Hero AS dataTypeHero

TYPE dataTypeObject ' Will be used for non-hero objects like bushes
  TrueX AS _UNSIGNED INTEGER
  TrueY AS _UNSIGNED INTEGER
  Type AS _UNSIGNED _BYTE ' works as a variable name in this case, despite also being a command
  Dir AS _UNSIGNED _BYTE
  Data0 AS _UNSIGNED _BYTE
  Data1 AS _UNSIGNED _BYTE
  Data2 AS _UNSIGNED _BYTE
  Data3 AS _UNSIGNED _BYTE
  Layer AS _UNSIGNED INTEGER
END TYPE: DIM SHARED Obj(255) AS dataTypeObject

COMMON SHARED objCount AS _UNSIGNED INTEGER

TYPE dataTypeObjectInfo
  Solid AS INTEGER
  Mobile AS INTEGER
  Projectile AS INTEGER
END TYPE: DIM SHARED ObjInfo(255) AS dataTypeObjectInfo

TYPE dataTypeMapHeader
  MpSizeX AS _UNSIGNED INTEGER
  MpSizeY AS _UNSIGNED _BYTE
  MapWrap AS _UNSIGNED _BYTE
  EdgeBarrier AS _UNSIGNED _BYTE
  LinkedMap AS _UNSIGNED _BYTE ' If walking off the edge, what map to go to
  ' mapBytesPer and numMaps apply to all maps
END TYPE: DIM SHARED mapHeader(10) AS dataTypeMapHeader

TYPE dataTypeAttrTable
  Flip AS _UNSIGNED _BYTE
  Pal AS _UNSIGNED _BYTE
END TYPE: DIM SHARED attrTable(511, 511) AS dataTypeAttrTable

TYPE dataTypeGTable ' 16 x 16 pixel blocks in the background
  Index AS _UNSIGNED INTEGER
  pTable AS _UNSIGNED _BYTE
  Pal AS _UNSIGNED _BYTE
  Flip AS _UNSIGNED _BYTE
END TYPE: DIM SHARED gTable(1023) AS dataTypeGTable

DIM SHARED mapData(16, 511, 511, 3) AS _UNSIGNED INTEGER ' Map number, X, Y, which byte
DIM SHARED mapOffsetX(511) ' For hex tiling of RPG overworld, one dimension for rows
DIM SHARED gamePal(20, 3) AS _UNSIGNED _BYTE ' Game specific, 16 palettes, 3 bytes each
DIM SHARED bmpPal256(255, 3) AS _UNSIGNED _BYTE ' R G B for all 256 colors that can be displayed
DIM SHARED bmpData(255, 511) AS _UNSIGNED _BYTE ' Graphic tile storage
DIM SHARED fontData(255, 255) AS _UNSIGNED _BYTE
DIM SHARED bgTable(511, 511) ' Stores tile numbers
DIM SHARED fTable(2047) AS _UNSIGNED _BYTE ' Fairy table

DIM SHARED proj_PosX(10)
DIM SHARED proj_PosY(10)
DIM SHARED proj_Dir(10) AS _UNSIGNED _BYTE
DIM SHARED proj_Type(10) AS _UNSIGNED _BYTE
DIM SHARED projCount AS _UNSIGNED INTEGER

DIM SHARED SHADOWS(50, 50) ' Value of 1 means in shadow, make sure this is as large as any screen shown
DIM SHARED SHADOWMASK(12, 12) ' A little larger than needed for safety

'''' Global variables

' These view variables define the pixels of the playfield, they are global variables
' instead of constants because I may want to load them from a file later

' These view variables define the pixels of the playfield, they are global variables
' instead of constants because I may load viewCenter from file
COMMON SHARED viewCenterPosX: viewCenterPosX = 9 ' Center position of view window
COMMON SHARED viewCenterPosY: viewCenterPosY = 9

COMMON SHARED viewSizeBlocksX, viewSizeBlocksY ' blockSize was set above
COMMON SHARED viewSizeTilesX, viewSizeTilesY

' Assume 16 pixel blocks

viewSizeBlocksX = (viewCenterPosX * 2) + 1
viewSizeBlocksY = (viewCenterPosY * 2) + 1

viewSizeTilesX = (viewSizeBlocksX * 2)
viewSizeTilesY = (viewSizeBlocksX * 2)

COMMON SHARED wrapHorizontal, wrapVertical ' Defaults to zero

COMMON SHARED modeEdit, modePT, modeInfo, showShadows
COMMON SHARED mapZeroOrigSizeY, mapBytesPer, numMaps, mapNum, mapPosX, mapPosY, mapEdgeTaperRows
' mapZeroOrigSizeY will be smaller than the world map
' mapEdgeTaperRows always needs to be even
COMMON SHARED trimHorizontal, StoreMapZeroEfficient

COMMON SHARED barrierNum, lastKeyPress
COMMON SHARED ptm1TileSelect, ptmPtnTableSelect, ptm3PtnTableSelect, ptmFairyTileSelect, ptmFairyStripBase ' For pattern table clicking
COMMON SHARED ptmPalSelect, ptm3PalSelect, ptmPalClrSelect, ptmGolemSelect, ptmGolemTableSelect
COMMON SHARED mouse1Clicked, mouse2Clicked, mouse1Released, mouse2Released, mPosX, mPosY ' New for mouse input

COMMON SHARED soundHandle

COMMON SHARED cornerMechanicTriggered, cornerMechanicClock

COMMON SHARED hWalkFrame, hWalkCounter

COMMON SHARED compassDir ' For flipping vertically the display order of 16x16 blocks on the screen. Will always be north or south.

COMMON SHARED walkActive, walkDistance, walkDir, walkDirWhenDone

modeEdit = 0 ' Now we turn it on to start

trimHorizontal = 1
StoreMapZeroEfficient = 0 ' Store the triangles of RPG world map data in the hidden top triangles

mapHeader(0).MpSizeX = 64 ' Will be overwritten by file load
mapHeader(0).MpSizeY = 32 ' Will be overwritten by file load, and also when initMapOffsets is called

mapBytesPer = 1
compassDir = NORTH

Hero.hTrueX = 16 * 20 ' These are split into screen positions and map positions later
Hero.hTrueY = 16 * 9

barrierNum = 16 ' If tile numbers are higher than this, the player can't move through them

'''''' I like to set the initial values for the global (COMMON SHARED) variables here,
'''''' then call initialization subroutines in the main function

gamePal(PALSTD, 0) = 0 ' Black
gamePal(PALSTD, 1) = 200 ' Teal
gamePal(PALSTD, 2) = 122 ' Peach
gamePal(PALSTD, 3) = 32 ' Red

ptmPalSelect = PALSTD
ptm3PalSelect = 7

mapEdgeTaperRows = 8

CALL a_main

END ' When the a_Main subroutine exits, it returns here, which ends the program. SYSTEM would also work.

''''''''''''''''''''''''''''''''
SUB a_main ' Main subroutine of our program

  createFont
  fileBitmapLoad
  setPalettes256
  fileRomLoad
  initMapOffsets
  initShadowMask
  initObjects

  '''' Start midi file ''''

  wFile$ = "zov.mid"

  IF wFile$ = "" THEN
    PRINT "Midi file name empty."
    END
  END IF

  IF _FILEEXISTS(wFile$) = 0 THEN
    PRINT "Midi file not found: '" + wFile$ + "'"
    END
  END IF

  soundHandle = _SNDOPEN(wFile$)
  '''''''''''''''''''''''''
  '_SNDPLAY soundHandle
  '  midiPlay

  DO '''' BEGIN MAIN LOOP, OUTER ''''

    clearShadows

    IF showShadows = 1 THEN
      addShadows
      clearShadows
    END IF

    DO ' Begin inner loop

      'if modeEdit > 0 or modePT > 0 then _LIMIT 60 ' Slows the game down to 60 FPS so it isn't too fast

      mouseReadInput

      redrawAll ' Moved inside of the inner loops since there is both slow and fast input to process now
      _LIMIT 27

      _DISPLAY
      '      _DISPLAY

      ' Poll the controls several times per loop, that seems to work best in terms of game speed

      IF modeEdit = 0 THEN
        processInput ("")
        processInput ("")
        processInput ("")

        IN$ = "" ' Assume no slow input

        ' I tried putting this section after loop ends, but it slows the processing down too much
        'EXIT DO
      ELSE

        IN$ = UCASE$(INKEY$)
        IF IN$ <> "" THEN EXIT DO

      END IF

    LOOP

    CALL processInput(IN$)

  LOOP ' Closing outer loop. Exit process is handled by processInput

END SUB ' a_Main                                                    2

''''''''''''''''''''''''''''''''
SUB addShadows

  FOR ix = 0 TO viewSizeBlocksX - 1 ' Shadows is an array the size of the viewing array, not the map
    FOR iy = 0 TO viewSizeBlocksY - 1
      SHADOWS(ix, iy) = 1
    NEXT
  NEXT

END SUB

'''''''''''''''''''''''''''''''
SUB adjustToScreenPos (wObj, toChangeX, toChangeY, passBackX AS INTEGER, passBackY AS INTEGER)
  ' Calculates the screen pixel offset (passBackX, passBackY) relative to the
  ' top-left of the playfield (viewStartPixelsX, viewStartPixelsY) for an object
  ' at true world coordinates (toChangeX, toChangeY), considering map scrolling (mapPosX, mapPosY).
  ' Assumes map wrapping is always enabled.

  DIM halfMapSizeX AS _UNSIGNED INTEGER ' Using _UNSIGNED INTEGER for half sizes is okay, but calculations involve signed differences
  DIM halfMapsizeY AS _UNSIGNED INTEGER ' Consider using signed INTEGER or LONG if map sizes could lead to large negative differences

  halfMapSizeX = (mapHeader(mapNum).MpSizeX * 16) \ 2 ' Calculate half width directly
  halfMapsizeY = (mapHeader(mapNum).MpSizeY * 16) \ 2 ' Calculate half height directly

  '  IF wObj = 0 THEN
  '    ZLOCATE 0, 0: PRINT mapPosX, toChangeX
  '  END IF

  '''' X Coordinate Calculation ''''
  passBackX = toChangeX - mapPosX ' Calculate the direct difference

  ' If more than half the map away in the negative direction,
  ' wrap around from the other side (object is effectively to the right)
  IF passBackX < -halfMapSizeX THEN
    passBackX = passBackX + (mapHeader(mapNum).MpSizeX * 16) ' Add full map pixel width
  ELSE
    IF passBackX > halfMapSizeX THEN ' See if the object is effectively to the left
      passBackX = passBackX - (mapHeader(mapNum).MpSizeX * 16) ' Subtract full map pixel width
    END IF

    ' Otherwise, passBackX was originally set correctly, do not change

  END IF

  '''' Y Coordinate Calculation ''''
  passBackY = toChangeY - mapPosY ' Calculate the direct difference

  ' If more than half the map away in the negative direction,
  ' wrap around from the other side (object is effectively below)
  IF passBackY < -halfMapsizeY THEN
    passBackY = passBackY + (mapHeader(mapNum).MpSizeY * 16) ' Add full map pixel height
  ELSE
    IF passBackY > halfMapsizeY THEN ' See if the object is effectively to the left
      passBackY = passBackY - (mapHeader(mapNum).MpSizeY * 16) ' Subtract full map pixel height
    END IF

    ' Otherwise, passBackY was originally set correctly, do not change

  END IF

END SUB ' adjustToScreenPos

''''''''''''''''''''''''''''''''
FUNCTION bitRead (wByte AS _UNSIGNED _BYTE, wPos AS INTEGER)
  ' Read one byte of a byte

  SELECT CASE wPos
    CASE 0
      bitRead = wByte AND 1
    CASE 1
      bitRead = (wByte AND 2) \ 2
    CASE 2
      bitRead = (wByte AND 4) \ 4
    CASE 3
      bitRead = (wByte AND 8) \ 8
    CASE 4
      bitRead = (wByte AND 16) \ 16
    CASE 5
      bitRead = (wByte AND 32) \ 32
    CASE 6
      bitRead = (wByte AND 64) \ 64
    CASE 7
      bitRead = (wByte AND 128) \ 128
    CASE ELSE
      ESCAPETEXT "Called bitRead with an invalid position"
  END SELECT

END FUNCTION ' bitRead

''''''''''''''''''''''''''''''''
FUNCTION bitWrite (wByte AS _UNSIGNED _BYTE, wPos AS INTEGER, wVal AS _UNSIGNED _BYTE)
  ' Write one bit to a byte at specified position (0-7) with value (0 or 1)
  ' Returns the modified byte

  DIM mask AS _UNSIGNED _BYTE
  DIM result AS _UNSIGNED _BYTE

  SELECT CASE wPos
    CASE 0
      mask = 1
    CASE 1
      mask = 2
    CASE 2
      mask = 4
    CASE 3
      mask = 8
    CASE 4
      mask = 16
    CASE 5
      mask = 32
    CASE 6
      mask = 64
    CASE 7
      mask = 128
    CASE ELSE
      ESCAPETEXT "Called bitWrite with an invalid position"
  END SELECT

  IF wVal = 0 THEN
    ' Clear the bit at position
    result = wByte AND NOT mask
  ELSEIF wVal = 1 THEN
    ' Set the bit at position
    result = wByte OR mask
  ELSE
    ESCAPETEXT "Called bitWrite with an invalid value (must be 0 or 1)"
  END IF

  bitWrite = result
END FUNCTION ' bitWrite

''''''''''''''''''''''''''''''''
SUB centerFairyStrip

  IF (ptmFairyTileSelect AND 65532) <= 20 THEN
    ptmFairyStripBase = 0
  ELSE ' > 44
    IF (ptmFairyTileSelect AND 65532) >= 1004 THEN
      ptmFairyStripBase = 980
    ELSE
      ptmFairyStripBase = (ptmFairyTileSelect AND 65532) - 20
    END IF
  END IF
END SUB ' centerFairyStrip


''''''''''''''''''''''''''''''''
SUB centerFairyStripSpecial

  ' This sub brings the tile back inside the strip if outside the strip, at the edges

  offset = ptmFairyStripBase AND 3 ' Save the position within the block

  IF (ptmFairyTileSelect AND 65532) < ptmFairyStripBase THEN ptmFairyTileSelect = ptmFairyStripBase + offset
  IF (ptmFairyTileSelect AND 65532) > ptmFairyStripBase + 40 THEN ptmFairyTileSelect = ptmFairyStripBase + 40 + offset

END SUB ' centerFairyStripSpecial

''''''''''''''''''''''''''''''''
SUB clearAttrTables

  FOR iy = 0 TO 511
    FOR ix = 0 TO 511
      attrTable(ix, iy).Pal = 0
      attrTable(ix, iy).Flip = 0
    NEXT
  NEXT

END SUB ' clearAttrTables

''''''''''''''''''''''''''''''''
SUB clearBGtable

  FOR iy = 0 TO 511
    FOR ix = 0 TO 511
      bgTable(ix, iy) = 0
    NEXT
  NEXT

END SUB ' clearBGtable

''''''''''''''''''''''''''''''''
SUB clearMap (wMap)

  ' This function just clears the map

  FOR iy = 0 TO mapHeader(wMap).MpSizeX STEP 1
    FOR ix = 0 TO mapHeader(wMap).MpSizeY STEP 1
      mapData(wMap, ix, iy, 0) = 0
    NEXT ix
  NEXT iy

END SUB ' clearMap

''''''''''''''''''''''''''''''''
SUB clearObjectArrays

  FOR ii = 0 TO 255
    Obj(ii).TrueX = 0
    Obj(ii).TrueY = 0
    Obj(ii).Type = 0
    Obj(ii).Dir = 0
    Obj(ii).Data0 = 0
    Obj(ii).Data1 = 0
    Obj(ii).Data2 = 0
    Obj(ii).Data3 = 0
  NEXT ii

END SUB ' clearObjArrays

''''''''''''''''''''''''''''''''

SUB clearShadows

  CALL splitPos(Hero.hTrueX, Hero.hTrueY) ' Pass the true X and Y positions to be split into map and screen positions

  ' Now using hero screen position X and Y

  '' clear shadows around player as defined by shadowMask

  FOR iy = minZero(Hero.ScreenY - 4) TO maxLimit(Hero.ScreenY + 4, viewSizeBlocksY - 1) ' Maxlimit is so they don't go past map array
    FOR ix = minZero(Hero.ScreenX - 4) TO maxLimit(Hero.ScreenX + 4, viewSizeBlocksX - 1) ' Maxlimit is so they don't go past map array

      xShadow = ix - (Hero.ScreenX - 4)
      yShadow = iy - (Hero.ScreenY - 4)

      IF SHADOWMASK(xShadow, yShadow) = 1 THEN
        SHADOWS(ix, iy) = 0 ' Set to no shadow
      END IF
    NEXT
  NEXT

  '' Next step, move forward and clear anything in visible range

END SUB ' clearShadows

''''''''''''''''''''''''''''''''
SUB compassCheck (refX, refY)
  ' Determines if the player is in a normally mapped or mirrored area on map 0
  ' Sets compassDir to NORTH if DX=RX and DY=RY (non-mirrored)
  ' Sets compassDir to SOUTH if either differs (mirrored or redirected)
  ' Parameters: refX (DX), refY (DY) - direct block positions
  ' Uses global compassDir and mapNum

  IF mapNum <> 0 THEN ' Only applies to map 0 with redirects
    compassDir = NORTH ' Default for non-world maps
    EXIT SUB
  END IF

  DX = refX
  DY = refY

  ' Calculate redirected coordinates
  RX = mapReturnRX(refX, refY, BYTEZERO) ' Redirected X
  RY = mapReturnRY(refX, refY, BYTEZERO) ' Redirected Y

  IF DX <> RX OR DY <> RY THEN
    compassDir = SOUTH ' Mirrored or redirected area
  ELSE
    compassDir = NORTH ' Non-mirrored area
  END IF

END SUB ' compassCheck

''''''''''''''''''''''''''''''''
SUB copyMaptoBG

  ' This loop section places the background tiles into the array, it is drawn in the next loop section

  ' Calculate the pixel offsets for smooth scrolling
  lowBitsX = mapPosX AND 15 ' Low 4 bits for sub-tile X offset
  lowBitsY = mapPosY AND 15 ' Low 4 bits for sub-tile Y offset

  FOR iy = 0 TO viewSizeBlocksY - 1 + gtz(lowBitsY, 2)
    FOR ix = 0 TO viewSizeBlocksX - 1 + gtz(lowBitsX, 2)

      baseX = ix * 2
      baseY = iy * 2

      ' We apply shadows by skipping ahead if shadow is active, avoiding a long IF nest
      IF showShadows = 1 AND SHADOWS(ix, iy) = 1 THEN GOTO shadowSkip ' To this label, below

      tempX = shRight(mapPosX, 4) + ix ' Makes it easier
      tempY = shRight(mapPosY, 4) + iy

      ' Wrap tempX and tempY to map bounds,  ensuring they stay within 0 to mpSizeX-1 and 0 to mpSizeY-1
      ' This repeats the map block across the screen when the map is smaller than the viewable area
      ' We calculate the remainder of tempX and tempY by removing the largest multiple of mpSizeX or mpSizeY, then adjust for bounds

      ' For X-axis wrapping:

      IF mapHeader(mapNum).MpSizeX = 0 OR mapHeader(mapNum).MpSizeY = 0 THEN ESCAPETEXT "Division by zero in copyMapToBG" ' Safety

      tempDivX = tempX / mapHeader(mapNum).MpSizeX ' Divide tempX by mpSizeX to get how many full map widths it spans (may be fractional)
      tempX = tempX - (mapHeader(mapNum).MpSizeX * INT(tempDivX)) ' Subtract the largest whole number of map widths (INT truncates), leaving the remainder
      IF tempX < 0 THEN tempX = tempX + mapHeader(mapNum).MpSizeX ' If the remainder is negative (e.g., from a negative tempX), add mpSizeX to wrap it to a positive value
      IF tempX >= mapHeader(mapNum).MpSizeX THEN tempX = tempX - mapHeader(mapNum).MpSizeX ' If the remainder equals or exceeds mpSizeX, subtract mpSizeX to keep it in bounds

      ' For Y-axis wrapping (same logic applied vertically):
      tempDivY = tempY / mapHeader(mapNum).MpSizeY ' Divide tempY by mpSizeY to determine full map height spans
      tempY = tempY - (mapHeader(mapNum).MpSizeY * INT(tempDivY)) ' Remove the largest whole number of map heights, keeping the remainder
      IF tempY < 0 THEN tempY = tempY + mapHeader(mapNum).MpSizeY ' Adjust negative remainders by adding mpSizeY to wrap to a positive value
      IF tempY >= mapHeader(mapNum).MpSizeY THEN tempY = tempY - mapHeader(mapNum).MpSizeY ' Ensure the remainder stays below mpSizeY by subtracting if too large

      ' Set the attribute table to zero every time, then change it if a tile that needs it

      setGolemToBG baseX, baseY, map(tempX, tempY, 0)

      ' Now overwrite special

      ' Blank
      IF map(tempX, tempY, 0) = 0 THEN
        setGolemToBG baseX, baseY, gBlockGround
      END IF

      ' Tree
      IF map(tempX, tempY, 0) = mBlockTree THEN ' Draw characters like trees and walls first
        setGolemToBG baseX, baseY, gBlockTree
      END IF

      ' Statue
      IF map(tempX, tempY, 0) = gBlockStatue THEN ' Draw characters like trees and walls first
        'setGolemToBG baseX, baseY, gBlockStatue
      END IF

      ' Wall
      IF map(tempX, tempY, 0) = 18 THEN
        'setGolemToBG baseX, baseY, gBlockWall
      END IF

      shadowSkip:
    NEXT ix
  NEXT iy

END SUB ' copyMapToBG

''''''''''''''''''''''''''''''''
FUNCTION cTrNum$ (wNum) ' Center trim number

  cTrNum$ = LTRIM$(RTRIM$(STR$(wNum)))

END FUNCTION ' Center trim number
''''''''''''''''''''''''''''''''

FUNCTION cTrim$ (wStr$) ' Center trim

  cTrim$ = LTRIM$(RTRIM$(wStr$))

END FUNCTION ' Center trim
''''''''''''''''''''''''''''''''

SUB createFont ' Create a font for use with more flexibility

  CLS
  COLOR 15

  FOR iy = 0 TO 15
    FOR ix = 0 TO 15
      wASC = (iy * 16) + ix ' Calculate what ASCII tile is being printed
      ZLOCATE ix, iy
      IF wASC <> 12 THEN PRINT CHR$(wASC) ' Character 12 is the form feed character, scrolls the previous ones off screen
    NEXT
  NEXT

  FOR tileY = 0 TO 144
    FOR tileX = 0 TO 127

      fontData(tileX, tileY) = POINT(tileX, tileY)

    NEXT ' tileX
  NEXT ' tileY

  CLS

END SUB
''''''''''''''''''''''''''''''''

SUB displayFlashText

  IF flashTextTimer > 0 THEN

    COLOR flashTextClr

    flashTextTimer = flashTextTimer - 1

    ZLOCATE 10, 25
    PRINT "        "
    ZLOCATE 10, 26
    PRINT " ";
    PRINT flashText$
    PRINT " "
    ZLOCATE 10, 27
    PRINT "        "
  END IF

END SUB ' displayFlashText
''''''''''''''''''''''''''''''''

SUB displaymodePT1 ' Display pattern tables

  f = drawGamePalettesPMI(3, 0, modePT) ' Mouse input included
  IF f <> -1 THEN ptmPalSelect = f

  ' Draw bmp palette grid at lower left
  pStartX = 3
  pStartY = 176

  FOR iy = 0 TO 15
    FOR ix = 0 TO 15
      drawBorderBox pStartX + (ix * 9), pStartY + (iy * 9), 9, 9, 0, (iy * 16) + ix
    NEXT 'ix
  NEXT iy

  ' Draw large 4 color palette to the right of the 16 x 16 palette grid

  ' Print the palette number above it
  IF ptmPalSelect <> PALSTD THEN printStr pStartX + 157, pStartY - 11, cTrNum$(gamePal(ptmPalSelect, ptmPalClrSelect)), 15, 0

  boxSize = 33

  drawBorderBox pStartX + 148, pStartY, boxSize, boxSize, 8, gamePal(ptmPalSelect, 0)
  drawBorderBox pStartX + 148, pStartY + boxSize + 4, boxSize, boxSize, 8, gamePal(ptmPalSelect, 1)
  drawBorderBox pStartX + 148, pStartY + (boxSize * 2) + 8, boxSize, boxSize, 8, gamePal(ptmPalSelect, 2)
  drawBorderBox pStartX + 148, pStartY + (boxSize * 3) + 12, boxSize, boxSize, 8, gamePal(ptmPalSelect, 3)

  ' Highlight large color box if not standard palette
  IF ptmPalSelect <> PALSTD THEN drawClearBox pStartX + 148, pStartY + (boxSize * ptmPalClrSelect) + (4 * ptmPalClrSelect), boxSize, boxSize, highlClr

  '''' Mouse input for large palette grid and large squares

  ' Check for mouse input and change selected tile if necessary
  IF mouse1Clicked = 1 THEN

    ' Changing game palette selection

    ' For picking from bmp 256 color palette
    IF ptmPalSelect <> PALSTD THEN
      FOR iy = 0 TO 15
        FOR ix = 0 TO 15
          IF mouseWithinBoxBounds(pStartX + (ix * 9), pStartY + (iy * 9), 9, 9) THEN
            drawClearBox pStartX + (ix * 9), pStartY + (iy * 9), 9, 9, highlClr
            wClr = ((mPosY - pStartY) \ 9) * 16 + ((mPosX - pStartX) \ 9)
            gamePal(ptmPalSelect, ptmPalClrSelect) = wClr
          END IF
        NEXT 'ix
      NEXT iy
    END IF

    ' For picking the individual color of the current palette, drawn large on right

    IF ptmPalSelect <> PALSTD THEN
      FOR ii = 0 TO 3
        IF mouseWithinBoxBounds(pStartX + 148, pStartY + (boxSize * ii) + (4 * ii), boxSize, boxSize) THEN ptmPalClrSelect = ii
      NEXT
    END IF

  END IF ' end mouse input for selecting out of 256 color and then large four box palettes

  tStartX = 216
  tStartY = 16

  '''' Draw tile tables and process mouse input for them

  'drawClearBox tStartX - 1, tStartY - 1, 145, 145, 3
  f0 = drawTileTablePMI(tStartX, tStartY, 0, ptmPalSelect, NORMAL, 3)
  f1 = drawTileTablePMI(tStartX, tStartY + 152, 256, ptmPalSelect, NORMAL, 3)
  f2 = drawTileTablePMI(tStartX + 152, tStartY, 512, ptmPalSelect, NORMAL, 3)
  f3 = drawTileTablePMI(tStartX + 152, tStartY + 152, 768, ptmPalSelect, NORMAL, 3)

  IF f0 <> -1 THEN ptm1TileSelect = f0
  IF f1 <> -1 THEN ptm1TileSelect = 256 + f1
  IF f2 <> -1 THEN ptm1TileSelect = 512 + f2
  IF f3 <> -1 THEN ptm1TileSelect = 768 + f3

  ''''
  ' Highlight box around the tile on the grid itself
  IF ptm1TileSelect <= 255 THEN
    drawTileTableHighlBox tStartX, tStartY, ptm1TileSelect, highlClr
  END IF

  IF ptm1TileSelect >= 256 AND ptm1TileSelect <= 511 THEN

    drawTileTableHighlBox tStartX, tStartY + 152, ptm1TileSelect, highlClr
  END IF

  IF ptm1TileSelect >= 512 AND ptm1TileSelect <= 767 THEN
    drawTileTableHighlBox tStartX + 152, tStartY, ptm1TileSelect, highlClr
  END IF

  IF ptm1TileSelect >= 768 AND ptm1TileSelect <= 1023 THEN
    drawTileTableHighlBox tStartX + 152, tStartY + 152, ptm1TileSelect, highlClr
  END IF

  ''''
  ' Done drawing tile map, continue on to info

  ' Draw highlight boxes

  '''' Right hand section ''''

  textX = (tStartX \ 8) + 38 ' Two layers of pattern tables will draw, show text just to the right of this

  textInfoX = 9

  ' Box around selected tile, which is drawn thrice as large
  drawClearBox (textInfoX * 8) + 9, 15, 26, 26, highlClr

  ' Draw selected tile on right, triple size, using the standard palette
  drawTile (textInfoX * 8) + 10, 16, ptm1TileSelect, PALSTD, NOFLIP, TIMESTHREE

  ' Print number of selected tile

  printStr textInfoX * 8, 48, "TILE:" + cTrNum$(ptm1TileSelect), 15, 0
  printStr textInfoX * 8, 56, "TABLE", 15, 0
  IF ptm1TileSelect < 255 THEN
    printStr (textInfoX * 8) + 48, 56, "0, TILE " + cTrNum(ptm1TileSelect AND 255), 15, 0
  ELSE IF ptm1TileSelect > 256 AND ptm1TileSelect < 511 THEN
      printStr (textInfoX * 8) + 48, 56, "1, TILE " + cTrNum(ptm1TileSelect AND 255), 15, 0

    ELSE IF ptm1TileSelect > 512 AND ptm1TileSelect < 767 THEN
        printStr (textInfoX * 8) + 48, 56, "2, TILE " + cTrNum(ptm1TileSelect AND 255), 15, 0
      ELSE IF ptm1TileSelect > 768 THEN
          printStr (textInfoX * 8) + 48, 56, "3, TILE " + cTrNum(ptm1TileSelect AND 255), 15, 0
        END IF
      END IF
    END IF
  END IF

  ' Print mouse position x and y

  ZLOCATE textInfoX + 8, 2
  PRINT "MSX:"; mPosX
  ZLOCATE textInfoX + 8, 3
  PRINT "MSY:"; mPosY

END SUB ' displaymodePT1
''''''''''''''''''''''''''''''''

SUB displaymodePT2 ' Fairy Table

  centerFairyStripSpecial

  f = drawGamePalettesPMI(3, 0, modePT) ' Mouse input included
  IF f <> -1 THEN ptmPalSelect = f

  ' Avoid using ptm1TileSelect in this function

  tStartX = 240
  tStartY = 1

  f = drawTileTablePMI(tStartX, tStartY, ptmPtnTableSelect * 256, ptmPalSelect, NORMAL, 3)
  IF f <> -1 THEN fTable(ptmFairyTileSelect) = f

  wTileX = fTable(ptmFairyTileSelect) AND 15
  wTileY = shRight(fTable(ptmFairyTileSelect), 4)

  ' Draw the box around the selected tile in the table grid
  drawTileTableHighlBox tStartX, tStartY, fTable(ptmFairyTileSelect), highlClr2

  ' Mouse input, button 2, selecting 2 x 2 block from pattern table
  IF mouse2Clicked = 1 THEN
    ' Selecting 8x8 pixel tile from pattern table
    IF mPosX >= tStartX AND mPosX < (tStartX + 144) - 1 AND mPosY >= tStartY AND mPosY < (tStartY + 144) THEN
      wQuadBlock = ((mPosY - tStartY) \ 18) * 8 + ((mPosX - tStartX) \ 18)

      qbY = shRight(wQuadBlock, 3)
      qbX = wQuadBlock AND 7
      drawClearBox tStartX + (qbX * 18) - 1, tStartY + (qbY * 18) - 1, 19, 19, highlClr

      ' Print the quadblock clicked no
      COLOR highlClr
      ZLOCATE 20, 15
      COLOR 15
      PRINT wQuadBlock * 2

      fTable(ptmFairyTileSelect AND 65532) = (qbY * 32) + (qbX * 2)
      fTable((ptmFairyTileSelect AND 65532) + 1) = (qbY * 32) + (qbX * 2) + 1
      fTable((ptmFairyTileSelect AND 65532) + 2) = (qbY * 32) + (qbX * 2) + 16
      fTable((ptmFairyTileSelect AND 65532) + 3) = (qbY * 32) + (qbX * 2) + 17

    END IF
  END IF

  ''''
  ' Draw bar at the bottom of the tile table

  barX = tStartX - 1
  barY = 147

  drawBorderBox barX, barY, 145, 17, 15, windowClr

  wStr$ = "PTN TBL " + LTRIM$(STR$(ptmPtnTableSelect))
  printStr barX + 37, barY + 5, wStr$, 15, windowClr

  ' Draw left arrow box for table
  drawBorderBox barX, barY, 26, 17, 15, windowClr
  printStr barX + 9, barY + 5, CHR$(17), 15, windowClr

  ' Mouse input, clicking left arrow for pattern table decrease

  IF mouse1Released = 1 AND mouseWithinBoxBounds(barX, barY + 1, 26, 16) THEN
    IF ptmPtnTableSelect > 0 THEN ptmPtnTableSelect = ptmPtnTableSelect - 1
  END IF

  ' Draw right arrow box for table
  drawBorderBox barX + 119, barY, 26, 17, 15, windowClr
  printStr barX + 129, barY + 5, CHR$(16), 15, windowClr

  ' Mouse input, clicking right arrow for pattern table increase
  IF mouse1Released = 1 AND mouseWithinBoxBounds(barX + 119, barY + 1, 26, 16) THEN
    IF ptmPtnTableSelect < 3 THEN ptmPtnTableSelect = ptmPtnTableSelect + 1
  END IF

  ''''

  textInfoX = 9

  centerX = textInfoX * 8
  tempY = 8

  printStr centerX, tempY + 8, "Base:", 15, 0
  printStr centerX + (8 * 5), tempY + 8, hexLen$(ptmFairyTileSelect AND 65532, 3), hexClr, 0

  printStr centerX + (8 * 2), tempY + 16, numRightDec$(ptmFairyTileSelect AND 65532), 7, 0


  printStr centerX, tempY + 32, "Div4:", 2, 0
  printStr centerX + (8 * 2), tempY + 40, numRightDec$((ptmFairyTileSelect AND 65532) \ 4), 7, 0

  ' Print index (base) of the tile
  printStr centerX + (8 * 9), tempY + 8, "Exact:", 15, 0
  printStr centerX + (8 * 15), tempY + 8, hexLen$(ptmFairyTileSelect, 3), hexClr, 0

  printStr centerX, tempY + 80, "Tile:", 15, 0
  printStr centerX + (8 * 5), tempY + 80, hexLen$(fTable(ptmFairyTileSelect), 2), hexClr, 0
  printStr centerX, tempY + 88, "    " + numRightDec$(fTable(ptmFairyTileSelect)), 7, 0

  tempY = tempY + 22

  ' The box around the selected fairy table tile, in center, triple size

  drawBorderBox centerX + 48, tempY, 51, 51, 3, 3

  tempBase = ptmFairyTileSelect AND 65532

  ' Draw the sub-tiles of the fairy table select square
  drawTile centerX + 49, tempY + 1, pTableStart + fTable(tempBase), ptmPalSelect, NOFLIP, TIMESTHREE
  drawTile centerX + 74, tempY + 1, pTableStart + fTable(tempBase + 1), ptmPalSelect, NOFLIP, TIMESTHREE
  drawTile centerX + 49, tempY + 26, pTableStart + fTable(tempBase + 2), ptmPalSelect, NOFLIP, TIMESTHREE
  drawTile centerX + 74, tempY + 26, pTableStart + fTable(tempBase + 3), ptmPalSelect, NOFLIP, TIMESTHREE

  ' The box around the selected tile within the fairy table selection

  SELECT CASE ptmFairyTileSelect AND 3
    CASE 0:
      drawClearBox centerX + 48, tempY, 26, 26, highlClr2
    CASE 1:
      drawClearBox centerX + 73, tempY, 26, 26, highlClr2
    CASE 2:
      drawClearBox centerX + 48, tempY + 25, 26, 26, highlClr2
    CASE 3:
      drawClearBox centerX + 73, tempY + 25, 26, 26, highlClr2
  END SELECT

  ''''
  ' Mouse input for the four tiles of the fairy table tile, drawn three times as large

  tStartX = centerX + 48
  tStartY = tempY

  IF mouse1Clicked AND mouseWithinBoxBounds(tStartX, tStartY, 25, 25) THEN ptmFairyTileSelect = (ptmFairyTileSelect AND 65532)
  IF mouse1Clicked AND mouseWithinBoxBounds(tStartX + 26, tStartY, 25, 25) THEN ptmFairyTileSelect = (ptmFairyTileSelect AND 65532) + 1
  IF mouse1Clicked AND mouseWithinBoxBounds(tStartX, tStartY + 26, 25, 25) THEN ptmFairyTileSelect = (ptmFairyTileSelect AND 65532) + 2
  IF mouse1Clicked AND mouseWithinBoxBounds(tStartX + 26, tStartY + 26, 25, 25) THEN ptmFairyTileSelect = (ptmFairyTileSelect AND 65532) + 3

  ''''
  tempX = textInfoX * 8
  tempY = 147

  drawBorderBox tempX, tempY, 56, 17, 15, windowClr
  printStr tempX + 8, tempY + 5, "CLEAR", 15, windowClr

  ' Mouse input for CLEAR button, clear out all 1024 tiles
  IF mouse1Released = 1 AND mouseWithinBoxBounds(tempX + 1, tempY + 1, 55, 16) THEN

    BEEP ' Notification that this was applied

    startAddr = (ptmFairyTileSelect AND 65280)
    endAddr = (ptmFairyTileSelect AND 65280) + 255 ' Strip down to the correct table, then go to the end of it

    FOR ii = ii TO 1023
      fTable(ii) = 0
    NEXT ' ii
  END IF ' End mouse input section for CLEAR box

  tempX = tempX + 64

  ' Draw the button to auto-apply fairy table tiles for the remainder of the 256 tile section, autofill

  drawBorderBox tempX, tempY, 79, 17, 15, windowClr
  printStr tempX + 8, tempY + 5, "AUTOFILL", 15, windowClr

  '''' Mouse input for clicking CLEAR And AUTOFILL buttons
  IF mouse1Released = 1 AND mouseWithinBoxBounds(tempX + 1, tempY + 1, 78, 16) THEN

    ' Clicking on the button to clear out the fairy table

    ' Clicking on the button to auto-apply 1024 fairy table tiles, autofill

    BEEP ' Notification that this was applied

    ' When using, choose the fairy table tile to go from. It will fill up the base of that fairyTable block to the end of the 256-tile section.

    startAddr = ptmFairyTileSelect AND 65532
    endAddr = (ptmFairyTileSelect AND 65280) + 255 ' Strip down to the correct table, then go to the end of it

    FOR ii = startAddr TO endAddr
      fTable(ii) = ii AND 255
    NEXT ' ii
  END IF ' Mouse input for AUTOFILL button

  f = drawFairyStripPMI(176, ptmPtnTableSelect, modePT, 1)

  tempY = 240
  LINE (0, 239)-(511, 239), 8 ' Gray separator line

  ' Bottom of screen, draw fairy table in full ''''

  FOR iy = 0 TO 15
    FOR ix = 0 TO 63
      wTile = (ptmPtnTableSelect * 256) + fTable((iy * 64) + ix)
      drawTile (ix * 8), tempY + (iy * 8), wTile, PALSTD, NOFLIP, NORMAL
    NEXT
  NEXT

  ' Draw a long box around the selected tile starting at the base of the fairy table tile

  ' 65532
  wTile = ptmFairyTileSelect AND 65532

  wTileX = wTile AND 63
  wTileY = shRight(wTile, 6)

  drawClearBox (wTileX * 8) - 1, tempY + (wTileY * 8) - 1, 34, 10, highlClr

  wTile = ptmFairyTileSelect

  wTileX = wTile AND 63
  wTileY = shRight(wTile, 6)

  ' Draw square box around selected fairy tile in bottom section
  drawClearBox (wTileX * 8) - 1, tempY + (wTileY * 8) - 1, 10, 10, highlClr2

  ' Info section, center: ''''

  ' print mouse position x and y

  IF mPosX < 145 AND mPosY < 145 THEN ' if mouse is over tile table (assume table starts at 1,1)
    ZLOCATE 19, 17

    PRINT "@:";
    wNum = ((mPosY - 1) \ 9) * 16 + ((mPosX - 1) \ 9)
    PRINTCLR cTrNum$(wNum), 15
  END IF

  ZLOCATE textInfoX + 8, 11
  PRINT "MSX:"; mPosX
  ZLOCATE textInfoX + 8, 12
  PRINT "MSY:"; mPosY

  ' Mouse input for bottom section, select fairy table tile
  IF mouse1Clicked = 1 THEN

    tempY = 240

    FOR iy = 0 TO 15
      FOR ix = 0 TO 63
        IF mouseWithinBoxBounds(ix * 8, tempY + (iy * 8), 8, 8) THEN
          ptmFairyTileSelect = (iy * 64) + ix
          centerFairyStrip
          EXIT SUB
        END IF
      NEXT ' ix
    NEXT ' iy
  END IF ' End if mouse1Clicked

END SUB ' displaymodePT2

''''''''''''''''''''''''''''''''
SUB displaymodePT3 ' Display Golem Blocks

  ' This function uses ptm3PalSelect instead of ptmPalSelect
  ' This function usees ptm3PtnTableSelect instead of ptmPtnTableSelect

  centerFairyStripSpecial

  ' Make sure the selection is in the golem table shown
  IF ptmGolemTableSelect = 0 THEN

    ptmGolemSelect = ptmGolemSelect AND 127 ' Make sure selection is in the current table
  ELSE
    ptmGolemSelect = ptmGolemSelect OR 128
  END IF

  f = drawGamePalettesPMI(3, 0, modePT) ' Mouse input included
  IF f <> -1 THEN ptm3PalSelect = f

  ' Print mouse position x and y

  COLOR 7

  ZLOCATE 9, 19
  PRINT "MSX:"; mPosX
  ZLOCATE 9, 20
  PRINT "MSY:"; mPosY

  COLOR 15

  tStartX = 216
  tStartY = 9

  ' Draw a grid of fairy table blocks in the upper left corner

  f = drawGolemGridPMI(tStartX, tStartY, 0)

  '''' Center: ''''

  centerX = 100
  tempY = 8

  printStr centerX, tempY + 8, "Gol:", 15, 0
  printStr centerX + (8 * 5), tempY + 8, hexLen$(ptmGolemSelect, 2), hexClr, 0
  wStr$ = "(" + cTrNum$(ptmGolemSelect) + ")"
  printStr centerX + (8 * 5), tempY + 16, wStr$, 15, 0

  tempVal = gTable(ptmGolemSelect).Index
  printStr centerX, tempY + 24, "FTI:", 15, 0 ' Fairy table index, from the golem table directly
  printStr centerX + (8 * 5), tempY + 24, hexLen$(tempVal, 3), hexClr, 0
  wStr$ = "(" + cTrNum$(tempVal) + ")"
  printStr centerX + (8 * 5), tempY + 32, wStr$, 15, 0

  tempTextX = 12
  tempTextY = 7

  ZLOCATE tempTextX, tempTextY: PRINT "P.TBL:"
  ZLOCATE tempTextX + 2, tempTextY + 1

  COLOR hexClr
  PRINT hexLen(gTable(ptmGolemSelect).pTable, 2)

  COLOR 15
  ZLOCATE tempTextX + 8, tempTextY: PRINT "PAL:"
  COLOR hexClr
  ZLOCATE tempTextX + 8, tempTextY + 1: PRINT gTable(ptmGolemSelect).Pal
  ' If 16 or over, will set to 15

  COLOR 15
  ZLOCATE tempTextX, tempTextY + 3: PRINT "TILES:"
  COLOR hexClr
  ZLOCATE tempTextX, tempTextY + 4
  PRINT hexLen(retGolemTileNoTable(ptmGolemSelect, 0), 2); " "; hexLen((retGolemTileNoTable(ptmGolemSelect, 1)), 2)
  ZLOCATE tempTextX, tempTextY + 5
  PRINT hexLen(retGolemTileNoTable(ptmGolemSelect, 2), 2); " "; hexLen((retGolemTileNoTable(ptmGolemSelect, 3)), 2)

  COLOR 15
  ZLOCATE tempTextX + 8, tempTextY + 3: PRINT "FLIP:"

  ZLOCATE tempTextX + 8, tempTextY + 4

  SELECT CASE gTable(ptmGolemSelect).Flip
    CASE 0:
      PRINT "NONE"
    CASE 1:
      PRINT "HORIZ"
    CASE 2:
      PRINT "VERT"
    CASE 3:
      PRINT "H+V"
    CASE ELSE
      gTable(ptmGolemSelect).Flip = 0 ' Fix any error
      PRINT "NONE"
  END SELECT

  ' Draw two strips of fairy table, each from their own pattern table

  f = drawFairyStripPMI(176, ptm3PtnTableSelect, modePT, 1) ' First with top labels
  ' If clicking on the fairy strip, assign golem table a tile value

  IF f <> -1 THEN
    gTable(ptmGolemSelect).Index = f
    gTable(ptmGolemSelect).pTable = ptm3PtnTableSelect AND 1 ' Would be AND 3 if four were displayed, accounts for whether strips are rotated around
    gTable(ptmGolemSelect).Pal = maxLimitSpecial(ptm3PalSelect, 15, 7) ' If PALSTD > 15, then go back to 7, which is similar to the standard palette

  END IF

  ' Clicking will rotate around table strips
  f = drawFairyStripPMI(220, (ptm3PtnTableSelect + 1) AND 1, modePT, 0) ' Pattern table 2
  ' If clicking on the fairy strip, assign golem table a tile value

  IF f <> -1 THEN ' -1 is no click
    gTable(ptmGolemSelect).Index = f
    gTable(ptmGolemSelect).pTable = (ptm3PtnTableSelect + 1) AND 1 ' Would be AND 3 if four were displayed, accounts for whether strips are rotated around
    gTable(ptmGolemSelect).Pal = maxLimitSpecial(ptm3PalSelect, 15, 7) ' If PALSTD > 15, then go back to 7, which is similar to the standard palette
  END IF

  '''' Right hand side: ''''

  tempY = 64
  textInfoX = 6

  ''''
  ' Draw bar at the bottom of the tile table

  barX = tStartX - 1
  barY = tStartY + 145

  drawBorderBox barX, barY, 119, 17, 15, windowClr

  wStr$ = "GLM TBL " + LTRIM$(STR$(ptmGolemTableSelect))
  printStr barX + 37, barY + 5, wStr$, 15, windowClr

  ' Draw left arrow box for table
  drawBorderBox barX, barY, 26, 17, 15, windowClr
  printStr barX + 9, barY + 5, CHR$(17), 15, windowClr

  ''''
  ' Mouse input, clicking left arrow for golem table decrease
  IF mouse1Released = 1 AND mouseWithinBoxBounds(barX, barY + 1, 26, 16) THEN
    IF ptmGolemTableSelect > 0 THEN ptmGolemTableSelect = ptmGolemTableSelect - 1
  END IF

  ' Draw right arrow box for table
  drawBorderBox barX + 117, barY, 26, 17, 15, windowClr
  printStr barX + 127, barY + 5, CHR$(16), 15, windowClr

  ' Mouse input, clicking right arrow for golem table increase
  IF mouse1Released = 1 AND mouseWithinBoxBounds(barX + 118, barY + 1, 25, 16) THEN
    IF ptmGolemTableSelect < 1 THEN ptmGolemTableSelect = ptmGolemTableSelect + 1
  END IF

  ''''
  ' Draw pattern table change bar at the bottom right of the golem table

  barX = tStartX + 144

  drawBorderBox barX, barY, 130, 17, 15, windowClr

  wStr$ = "PTN TBL " + LTRIM$(STR$(ptm3PtnTableSelect))
  printStr barX + 37, barY + 5, wStr$, 15, windowClr

  ' Draw left arrow box for table
  drawBorderBox barX, barY, 26, 17, 15, windowClr
  printStr barX + 9, barY + 5, CHR$(17), 15, windowClr

  ' Mouse input, clicking left arrow for pattern table decrease
  IF mouse1Released = 1 AND mouseWithinBoxBounds(barX + 1, barY + 1, 26, 16) THEN
    IF ptm3PtnTableSelect > 0 THEN ptm3PtnTableSelect = ptm3PtnTableSelect - 1
  END IF

  ' Draw right arrow box for table
  drawBorderBox barX + 118, barY, 26, 17, 15, windowClr
  printStr barX + 128, barY + 5, CHR$(16), 15, windowClr

  ' Mouse input, clicking right arrow for pattern table increase
  IF mouse1Released = 1 AND mouseWithinBoxBounds(barX + 120, barY + 1, 25, 16) THEN
    IF ptm3PtnTableSelect < 1 THEN ptm3PtnTableSelect = ptm3PtnTableSelect + 1
  END IF

END SUB ' displaymodePT3

''''''''''''''''''''''''''''''''
SUB drawBorderBox (wPosX, wPosY, wSizeX, wSizeY, fgClr, bgClr)

  IF wSizeX <= 1 THEN EXIT SUB ' It will be subtracting 2 below, safety
  IF wSizeY <= 1 THEN EXIT SUB

  ' This subtracts one more pixel than you'd expect, because the endpoint to
  ' base + size is one pixel larger than you'd expect

  ' Draw the border to the box
  LINE (wPosX, wPosY)-(wPosX + wSizeX - 1, wPosY + wSizeY - 1), fgClr, B

  ' Inside the box
  LINE (wPosX + 1, wPosY + 1)-(wPosX + wSizeX - 2, wPosY + wSizeY - 2), bgClr, BF

END SUB

''''''''''''''''''''''''''''''''
SUB drawClearBox (wPosX, wPosY, wSizeX, wSizeY, fgClr)

  IF wSizeX <= 1 THEN EXIT SUB ' It will be subtracting 2 below, safety
  IF wSizeY <= 1 THEN EXIT SUB

  ' This subtracts one more pixel than you'd expect, because the endpoint to
  ' base + size is one pixel larger than you'd expect

  ' Draw the border to the box
  LINE (wPosX, wPosY)-(wPosX + wSizeX - 1, wPosY + wSizeY - 1), fgClr, B

END SUB

''''''''''''''''''''''''''''''''
FUNCTION drawFairyStripPMI (wStartY, wTable, wMode, topLabel)

  drawFairyStripPMI = -1 ' Will change if something is clicked, only modePT 3 uses

  ' Mode 2 includes numbering in decimal, gray, and highlight boxes

  wStartX = 24
  'wStartY = 176

  ' Draw a horizontal strip (2x2) of nearby fairy table locations, drawn double size
  startX = 24
  startY = 176
  xSpace = 43

  ' Left arrow box for bottom strip of fairy table
  drawBorderBox 0, wStartY + 12, 17, 35, 14, windowClr
  printStr 4, wStartY + 26, CHR$(17), 15, windowClr

  ' Right arrow box for bottom strip of fairy table
  drawBorderBox 495, wStartY + 12, 17, 35, 14, windowClr
  printStr 500, wStartY + 26, CHR$(16), 15, windowClr

  ' Print table at left in mode 3
  IF topLabel = 1 AND wMode = 3 THEN printStr 0, wStartY, cTrNum$(wTable), hexClr, 0

  FOR ix = 0 TO 10

    ' Print the number and highlight box at top if labels chosen

    IF topLabel = 1 THEN printStr wStartX + (ix * xSpace) + 7, wStartY, hexLen$(ptmFairyStripBase + ix * 4, 3), hexClr, 0
    IF wMode < 3 THEN
      ' Highlight box around selected tile in strip
      IF ptmFairyStripBase + (ix * 4) = (ptmFairyTileSelect AND 65532) THEN
        ' Box around number above the fairy strip box
        drawClearBox wStartX + (ix * xSpace), wStartY - 3, 35, 13, highlClr
      END IF
    END IF

    drawBorderBox wStartX + (ix * xSpace), wStartY + 12, 35, 35, 3, 3

    wPTblStart = wTable * 256

    IF wMode <> 3 THEN
      wPal = ptmPalSelect
    ELSE
      wPal = ptm3PalSelect
    END IF

    ' Draw the sub-tiles pTableStart adds to the 8-bit tile number the pattern table base * 256
    drawTile wStartX + (ix * xSpace) + 1, wStartY + 13, wPTblStart + fTable(ptmFairyStripBase + (ix * 4)), wPal, NOFLIP, TIMESTWO
    drawTile wStartX + (ix * xSpace) + 18, wStartY + 13, wPTblStart + fTable(ptmFairyStripBase + (ix * 4) + 1), wPal, NOFLIP, TIMESTWO
    drawTile wStartX + (ix * xSpace) + 1, wStartY + 30, wPTblStart + fTable(ptmFairyStripBase + (ix * 4) + 2), wPal, NOFLIP, TIMESTWO
    drawTile wStartX + (ix * xSpace) + 18, wStartY + 30, wPTblStart + fTable(ptmFairyStripBase + (ix * 4) + 3), wPal, NOFLIP, TIMESTWO

    ' Highlight around 8x8 boxes inside the fairy table blocks
    IF wMode < 3 AND ptmFairyStripBase + (ix * 4) = (ptmFairyTileSelect AND 65532) THEN

      SELECT CASE ptmFairyTileSelect AND 3
        CASE 0:
          drawClearBox wStartX + (ix * xSpace), wStartY + 12, 18, 18, highlClr2
        CASE 1:
          drawClearBox wStartX + (ix * xSpace) + 17, wStartY + 12, 18, 18, highlClr2
        CASE 2:
          drawClearBox wStartX + (ix * xSpace), wStartY + 29, 18, 18, highlClr2
        CASE 3:
          drawClearBox wStartX + (ix * xSpace) + 17, wStartY + 29, 18, 18, highlClr2
      END SELECT
    END IF

    ' Number below, value / 4

    adjBase = (ptmFairyStripBase + ix * 4) \ 4

    addX = 0
    IF adjBase < 10 THEN addX = 8
    IF adjBase >= 10 AND adjBase < 100 THEN addX = 4

    ' Number in decimal
    IF wMode < 3 THEN printStr wStartX + addX + (ix * xSpace) - 1, wStartY + 52, STR$((ptmFairyStripBase + (ix * 4))), 7, 0

  NEXT ' ix

  ' Mouse input section for fairy strip

  IF mouse1Clicked OR mouse2Clicked THEN

    ' Clicking on the fairy table strip, left arrow

    IF mouseWithinPixelBounds(0, wStartY + 12, 15, wStartY + 46) AND ptmFairyStripBase > 3 THEN
      ptmFairyStripBase = ptmFairyStripBase - 4
      centerFairyStripSpecial
    END IF
    ' Right arrow:
    IF mouseWithinPixelBounds(496, wStartY + 12, 511, wStartY + 46) AND ptmFairyStripBase < 977 THEN
      ptmFairyStripBase = ptmFairyStripBase + 4
      centerFairyStripSpecial
    END IF

    ' Changing selected tile in strip

    ' For changing fairy table tile, within strip

    FOR ix = 0 TO 10

      ' Top goes back to -15, to catch the number above
      ' Center bar pixels count as first for this one, to ensure a click in the larger selects the whole index
      IF wMode <= 2 THEN ' Account for label click space
        IF mouseWithinBoxBounds(wStartX + (ix * xSpace), wStartY - 3, 17, 32) THEN ptmFairyTileSelect = ptmFairyStripBase + (ix * 4)
        IF mouseWithinBoxBounds(wStartX + (ix * xSpace) + 18, wStartY - 3, 17, 32) THEN ptmFairyTileSelect = ptmFairyStripBase + (ix * 4) + 1
        IF mouseWithinBoxBounds(wStartX + (ix * xSpace), wStartY + 30, 17, 17) THEN ptmFairyTileSelect = ptmFairyStripBase + (ix * 4) + 2
        IF mouseWithinBoxBounds(wStartX + (ix * xSpace) + 18, wStartY + 30, 17, 17) THEN ptmFairyTileSelect = ptmFairyStripBase + (ix * 4) + 3
      END IF

      IF wMode = 3 THEN
        IF mouseWithinBoxBounds(wStartX + (ix * xSpace), wStartY + 12, 35, 35) THEN
          ptmFairyTileSelect = ptmFairyStripBase + (ix * 4)
          drawClearBox wStartX + (ix * xSpace), wStartY + 12, 35, 35, highlClr

          drawFairyStripPMI = ptmFairyTileSelect ' Tile ranges from 0-1023
        END IF
      END IF
    NEXT

  END IF ' mouse1Clicked

END FUNCTION ' drawFairyStrip Plus Mouse Input

''''''''''''''''''''''''''''''''
FUNCTION drawGamePalettesPMI (wStartX, wStartY, wMode)

  ' Draw game palettes, plus mouse input. Returns the palette clicked on, sets individual colors locally

  toReturn = -1 ' If nothing clicked

  '  IF wMode <> 3 THEN

  IF ptmPalSelect < 16 THEN
    printStr wStartX + 2, 0, "PAL:" + cTrNum$(ptmPalSelect), 15, 0
  ELSE
    printStr wStartX + 2, 0, "PAL:STD", 15, 0
  END IF
  'ELSE

  '    IF ptm3PalSelect <= 7 THEN
  '      printStr wStartX + 2, 0, "PAL:" + cTrNum$(ptm3PalSelect), 15, 0
  '    ELSE
  '      ESCAPE 97 ' Safety, should never get this high
  '  END IF

  ' END IF

  palBoxClr = 8

  ' IF wMode = 3 THEN
  '   toY = 7
  ' ELSE
  toY = 15
  ' END IF

  FOR ii = 0 TO toY
    drawBorderBox wStartX, wStartY + 12 + (ii * 10), 8, 8, palBoxClr, gamePal(ii, 0)
    drawBorderBox wStartX + 10, wStartY + 12 + (ii * 10), 8, 8, palBoxClr, gamePal(ii, 1)
    drawBorderBox wStartX + 20, wStartY + 12 + (ii * 10), 8, 8, palBoxClr, gamePal(ii, 2)
    drawBorderBox wStartX + 30, wStartY + 12 + (ii * 10), 8, 8, palBoxClr, gamePal(ii, 3)

    printStr wStartX + 42, wStartY + 12 + (ii * 10), cTrNum$(ii), 15, 0
  NEXT

  IF wMode <> 3 THEN
    wPal = ptmPalSelect
  ELSE
    wPal = ptm3PalSelect
  END IF

  IF wPal < 16 THEN
    ' Draw box around selected palette
    drawClearBox wStartX - 3, wStartY + 11 + (wPal * 10), 63, 10, highlClr
  ELSE ' Standard palette
    drawClearBox wStartX - 3, -1, 63, 10, highlClr
  END IF

  '''' MOUSE INPUT SECTION

  ' Changing palette selection

  IF mouse1Clicked = 1 THEN ' Standard palette above others:
    IF wMode <> 3 AND mouseWithinBoxBounds(wStartX - 3, wStartY, 63, 10) THEN toReturn = PALSTD
    FOR iy = 0 TO toY
      IF mouseWithinBoxBounds(wStartX - 3, wStartY + 12 + (iy * 10), 63, 10) THEN
        toReturn = iy

        IF mPosX < wStartX + 10 THEN
          ptmPalClrSelect = 0
        END IF

        IF mPosX >= wStartX + 10 AND mPosX < wStartX + 19 THEN
          ptmPalClrSelect = 1
        END IF

        IF mPosX >= wStartX + 19 AND mPosX < wStartX + 30 THEN
          ptmPalClrSelect = 2
        END IF

        IF mPosX >= wStartX + 30 THEN
          ptmPalClrSelect = 3
        END IF

      END IF
    NEXT 'iy
  END IF ' mouse1Clicked

  drawGamePalettesPMI = toReturn

END FUNCTION ' drawGamePalettes Plus Mouse Input

''''''''''''''''''''''''''''''''
FUNCTION drawGolemGridPMI (wStartX, wStartY, wSize)

  ' Golem format is 4 bits for palette (P), 2 bits for flip (F), 1 bit reserved (R), 1 bit pattern table (T), 8 bits fairy table index (I) shifted left 2
  ' First two bytes stored as: PPPPTTII.IIIIIIII

  ' Uses ptm3PalSelect

  tempBase = ptmFairyTileSelect AND 65532

  ' Draw a border around the grid

  IF wSize = 0 THEN ' Half sized, only 128 blocks
    endY = 7
    endYP = 144
  ELSE
    endY = 15
    endYP = 288
  END IF

  drawClearBox wStartX - 1, wStartY - 1, 289, endYP + 1, 3

  FOR iy = 0 TO 7
    FOR ix = 0 TO 15

      wGolem = (ptmGolemTableSelect * 128) + (iy * 16) + ix
      wPTblStart = gTable(wGolem).pTable * 256 ' Cannot be higher than 1
      wPal = gTable(wGolem).Pal AND 15 ' Safety, cannot be higher than 15

      SELECT CASE gTable(wGolem).Flip
        CASE 0:
          ' Draw the sub-tiles of the fairy table select square
          drawTile wStartX + (ix * 18), wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index), wPal, NOFLIP, NORMAL
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index + 1), wPal, NOFLIP, NORMAL
          drawTile wStartX + (ix * 18), wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index + 2), wPal, NOFLIP, NORMAL
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index + 3), wPal, NOFLIP, NORMAL
        CASE 1: ' Horizontal flip
          ' Draw the sub-tiles of the fairy table select square
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index), wPal, 1, NORMAL
          drawTile wStartX + (ix * 18), wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index + 1), wPal, 1, NORMAL
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index + 2), wPal, 1, NORMAL
          drawTile wStartX + (ix * 18), wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index + 3), wPal, 1, NORMAL
        CASE 2: ' Vertical flip
          ' Draw the sub-tiles of the fairy table select square
          drawTile wStartX + (ix * 18), wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index), wPal, 2, NORMAL
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index + 1), wPal, 2, NORMAL
          drawTile wStartX + (ix * 18), wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index + 2), wPal, 2, NORMAL
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index + 3), wPal, 2, NORMAL
        CASE 3: ' Horizontal and vertical flips
          ' Draw the sub-tiles of the fairy table select square
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index), wPal, 3, NORMAL
          drawTile wStartX + (ix * 18), wStartY + (iy * 18) + 9, wPTblStart + fTable(gTable(wGolem).Index + 1), wPal, 3, NORMAL
          drawTile wStartX + (ix * 18) + 9, wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index + 2), wPal, 3, NORMAL
          drawTile wStartX + (ix * 18), wStartY + (iy * 18), wPTblStart + fTable(gTable(wGolem).Index + 3), wPal, 3, NORMAL
        CASE ELSE
          ESCAPE 89 ' Safety
      END SELECT
    NEXT
  NEXT

  drawGolemGridPMI = -1 ' Didn't click within, keeps this unless changed below

  '''' Mouse input for drawGolemGridPMI, selecting 2x2 block from golem table
  IF mouse1Clicked = 1 OR mouse2Clicked = 1 THEN

    ' Selecting 2x2 block of tiles from pattern table
    IF mPosX >= wStartX AND mPosX < (wStartX + 288) AND mPosY >= wStartY AND mPosY < (wStartY + endYP) THEN
      ptmGolemSelect = (ptmGolemTableSelect * 128) + ((mPosY - wStartY) \ 18) * 16 + ((mPosX - wStartX) \ 18)
    END IF
  END IF

  ' Draw a box around the selected golem in the golem table (upper left)
  qbY = shRight((ptmGolemSelect AND 127), 4)
  qbX = ptmGolemSelect AND 15
  drawClearBox wStartX + (qbX * 18) - 1, wStartY + (qbY * 18) - 1, 18, 18, highlClr

  drawGolemGridPMI = ptmGolemSelect

END FUNCTION ' drawGolemGrid Plus Mouse Input

''''''''''''''''''''''''''''''''
SUB drawObjFrameToPlayfieldAtPos (wPosX, wPosY, wTable, wIndex, wPal, wFlip, flagWrap)

  ' Not used, golem tiles go to background, leftover in case

  FOR ii = 0 TO 100
    'IF fTable(ii) <> 0 THEN ESCAPE ii' fTable(ii)
  NEXT

  ' Saves time if fully off screen
  'adjustToScreenPos obj(wObj).truex, obj(wObj).truey, wPosX, wPosY ' Returns by reference to wPos

  SELECT CASE wFlip
    CASE 0:
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
    CASE 1:
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
    CASE 2:
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
    CASE 3:
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap

  END SELECT
END SUB ' drawObjFrameToPlayfieldAtPos

''''''''''''''''''''''''''''''''
SUB drawObjFrameByObj (wObj, wTable, wIndex, wPal, wFlip, flagWrap)

  ' Same as the original, but uses the object positions in the array, just send the object number

  DIM wPosX AS INTEGER ' Signed integer, can be negative
  DIM wPosY AS INTEGER ' Signed integer, can be negative

  wPosX = 0
  wPosY = 0

  IF wObj > 0 THEN
    ZLOCATE 16, 41
    PRINT " DRAWING > 0"
  END IF

  adjustToScreenPos wObj, Obj(wObj).TrueX, Obj(wObj).TrueY, wPosX, wPosY ' Returns wPos variables by reference to wPos

  SELECT CASE wFlip
    CASE 0:
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
    CASE 1:
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
    CASE 2:
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
    CASE 3:
      drawTileToPlayfield wPosX + 8, wPosY + 8, (wTable * 256) + fTable(wIndex), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY + 8, (wTable * 256) + fTable(wIndex + 1), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX + 8, wPosY, (wTable * 256) + fTable(wIndex + 2), wPal, wFlip, flagWrap
      drawTileToPlayfield wPosX, wPosY, (wTable * 256) + fTable(wIndex + 3), wPal, wFlip, flagWrap
  END SELECT

END SUB ' drawObjFrameByObj

''''''''''''''''''''''''''''''''
SUB drawTile (wPosX, wPosY, wTile, wPal, wFlip, wSize) ' Draw an 8 x 8 pixel tile
  ' Can draw anywhere on the screen

  ' Split wTile into X and Y positions for our 8 x 8 tiles
  offsetY = wTile \ 16 ' Shift right four bits
  offsetX = wTile AND 15 ' Drop high four bits

  ' 0 = No flip, 1 = Horizontal flip

  FOR iy = 0 TO 7
    FOR ix = 0 TO 7
      drawPosX = (wPosX + ix) ' Since they will be used several times
      drawPosY = (wPosY + iy)

      ' Get the color to use
      IF (wFlip AND 2) = 0 THEN ' Check vertical flip
        IF (wFlip AND 1) = 0 THEN ' Check horizontal flip
          wClr = palAdd(bmpData((offsetX * 8) + ix, (offsetY * 8) + iy), wPal) ' No flip
        ELSE ' wFlip = 1
          wClr = palAdd(bmpData((offsetX * 8) + (7 - ix), (offsetY * 8) + iy), wPal) ' Horizontal flip
        END IF
      ELSE ' Horizontal flip on, wFlip = 1 or 3
        IF (wFlip AND 1) = 0 THEN ' wFlip = 2
          wClr = palAdd(bmpData((offsetX * 8) + ix, (offsetY * 8) + (7 - iy)), wPal) ' Vertical flip
        ELSE ' wFlip = 3, horizontal and vertical flip
          wClr = palAdd(bmpData((offsetX * 8) + (7 - ix), (offsetY * 8) + (7 - iy)), wPal) ' Both horizontal and vertical flips
        END IF
      END IF

      IF wSize = NORMAL THEN
        PSET (drawPosX, drawPosY), wClr
      ELSEIF wSize = TIMESTWO THEN
        CALL PSETmult(wPosX + (ix * 2), wPosY + (iy * 2), wClr, TIMESTWO)
      ELSEIF wSize = TIMESTHREE THEN
        CALL PSETmult(wPosX + (ix * 3), wPosY + (iy * 3), wClr, TIMESTHREE)
      END IF
    NEXT
  NEXT

END SUB ' drawTile

''''''''''''''''''''''''''''''''
SUB drawTileToPlayfield (wPosX, wPosY, wTile, wPal, wFlip, flagWrap) ' Draw an 8 x 8 pixel tile
  ' This function only will draw inside the gameplay area
  ' wTile is a 16-bit value, includes all four tables
  ' Anything more than 512 X or Y will draw to the left or top

  ' If called in the negative, it will not display beyond screen pixel zero (-4 will only show half)

  ' Split wTile into X and Y positions for our 8 x 8 tiles
  offsetY = wTile \ 16 ' Shift right four bits
  offsetX = wTile AND 15 ' Drop high four bits

  FOR iy = 0 TO 7
    FOR ix = 0 TO 7
      drawPosX = (wPosX + ix) ' Since they will be used several times
      drawPosY = (wPosY + iy)

      ' Wrapping, if pixel is too far over, move it back to the other side

      IF wrapHorizontal = 1 AND drawPosX > (viewSizeTilesX * 8) - 1 THEN drawPosX = drawPosX - (viewSizeTilesX * 8)
      IF wrapVertical = 1 AND drawPosY > (viewSizeTilesX * 8) - 1 THEN drawPosY = drawPosY - (viewSizeTilesY * 8)

      ' Get the color to use
      IF (wFlip AND 2) = 0 THEN ' Check vertical flip
        IF (wFlip AND 1) = 0 THEN ' Check horizontal flip
          wClr = palAdd(bmpData((offsetX * 8) + ix, (offsetY * 8) + iy), wPal) ' No flip
        ELSE ' wFlip = 1
          wClr = palAdd(bmpData((offsetX * 8) + (7 - ix), (offsetY * 8) + iy), wPal) ' Horizontal flip
        END IF
      ELSE ' Horizontal flip on, wFlip = 1 or 3
        IF (wFlip AND 1) = 0 THEN ' wFlip = 2
          wClr = palAdd(bmpData((offsetX * 8) + ix, (offsetY * 8) + (7 - iy)), wPal) ' Vertical flip
        ELSE ' wFlip = 3, horizontal and vertical flip
          wClr = palAdd(bmpData((offsetX * 8) + (7 - ix), (offsetY * 8) + (7 - iy)), wPal) ' Both vertical and horizontal flips
        END IF
      END IF

      IF wClr <> 0 THEN ' Skip transparent
        IF drawPosX >= 0 AND drawPosY >= 0 AND drawPosX < viewSizeTilesX * 8 AND drawPosY < viewSizeTilesY * 8 THEN
          PSET (viewStartPixelsX + drawPosX, viewStartPixelsY + drawPosY), palAdd(wClr, wPal)
        END IF
      END IF
    NEXT
  NEXT

END SUB ' drawTileToPlayfield

''''''''''''''''''''''''''''''''
FUNCTION drawTileTablePMI (wPosX, wPosY, tileStart, wPal, wSize, borderClr)

  ' Draws 256 tiles at the given positions, with a 1 pixel gap

  IF borderClr <> 0 THEN drawClearBox wPosX - 1, wPosY - 1, 145, 145, 3

  FOR iy = 0 TO 15
    FOR ix = 0 TO 15
      wTile = tileStart + (iy * 16) + ix
      IF wSize = NORMAL THEN
        drawTile wPosX + (ix * 9), wPosY + (iy * 9), wTile, wPal, NOFLIP, NORMAL
      ELSEIF wSize = TIMESTWO THEN
        drawTile wPosX + (ix * 17), wPosY + (iy * 17), wTile, wPal, NOFLIP, TIMESTWO
      ELSEIF wSize = TIMESTHREE THEN
        drawTile wPosX + (ix * 24), wPosY + (iy * 25), wTile, wPal, NOFLIP, TIMESTHREE
      END IF
    NEXT
  NEXT

  drawTileTablePMI = -1 ' Indicate no tile clicked, keeps this unless changed below

  IF mouse1Clicked THEN
    ' Does not include the base tile number
    IF mPosX >= wPosX AND mPosX < (wPosX + 144) AND mPosY >= wPosY AND mPosY < (wPosY + 144) THEN
      temp = ((mPosY - wPosY) \ 9) * 16 + ((mPosX - wPosX) \ 9)
      IF temp < 0 OR temp > 255 THEN ESCAPE 88 ' Safety
      drawTileTablePMI = temp
      EXIT FUNCTION
    END IF
  END IF

END FUNCTION ' drawTileTablePMI

''''''''''''''''''''''''''''''''
SUB drawTileTableHighlBox (tStartX, tStartY, wTile, wClr)

  wTileX = (wTile AND 255) AND 15
  wTileY = shRight((wTile AND 255), 4)

  drawClearBox tStartX + (wTileX * 9) - 1, tStartY + (wTileY * 9) - 1, 10, 10, wClr

END SUB ' drawTileTableHighlBox

''''''''''''''''''''''''''''''''
SUB endProgram

  ESCAPETEXT "PROGRAM ENDED"

END SUB ' endProgram

''''''''''''''''''''''''''''''''
FUNCTION extractDigit (wNum, wDigit)

  numStr$ = LTRIM$(STR$(wNum))
  wPos = LEN(numStr$) - (wDigit)

  extractDigit = VAL(MID$(numStr$, wPos, 1))
END FUNCTION

''''''''''''''''''''''''''''''''
SUB fileBitmapLoad

  ' Call this function after using createBmp to make a file and the game will load
  ' this graphic data

  fln$ = "BmpDemo.bmp"
  rwvar$ = " " ' Necessary
  OPEN fln$ FOR BINARY AS #1

  IF LOF(1) = 0 THEN
    CLOSE #1
    KILL fln$
    outText$ = "Bitmap file '" + fln$ + "' does not exist"
    ESCAPETEXT outText$
  END IF

  ' In the .bmp file format, the first part of header is 14 bytes,
  ' and the InfoHeader is 40 bytes. We're not even going to check the data,
  ' we'll just assume the properties of the file like the size are what we planned.
  ' Our bitmap image is 128 pixels wide by 1024 pixels tall.

  FOR ii = 1 TO 54 ' Pull out and ignore the initial header data (14 + 40 bytes)
    GET #1, , rwvar$
  NEXT
  palCounter = 0
  FOR ii = 0 TO 255 ' Next the palette in BGR0
    GET #1, , rwvar$
    wVal = ASC(rwvar$)
    bmpPal256(palCounter, palBLUE) = wVal

    GET #1, , rwvar$
    wVal = ASC(rwvar$)
    bmpPal256(palCounter, palGREEN) = wVal

    GET #1, , rwvar$
    wVal = ASC(rwvar$)
    bmpPal256(palCounter, palRED) = wVal

    GET #1, , rwvar$ ' Don't keep this reserved byte

    palCounter = palCounter + 1

  NEXT ii

  FOR iy = 511 TO 0 STEP -1
    FOR ix = 0 TO 127

      GET #1, , rwvar$
      wVal = ASC(rwvar$)

      bmpData(ix, iy) = wVal

    NEXT
  NEXT

  CLOSE #1

END SUB ' fileBitmapLoad

''''''''''''''''''''''''''''''''
SUB fileRomLoad

  ' File map:
  ' 64 bytes reserved
  ' 1024 bytes fairy table
  ' 256 bytes golem table
  ' Remainder map data, including headers

  fln$ = "_demo.rom"
  rwVar$ = " " ' Necessary
  OPEN fln$ FOR BINARY AS #1
  IF LOF(1) = 0 THEN
    CLOSE #1
    KILL fln$
    CLS
    LOCATE 1, 1
    outStr$ = "File '" + fln$ + "' does not exist"
    ESCAPETEXT (outStr$)
  END IF

  FOR ii = 0 TO 63 ' Pull and discard 64 reserved bytes
    GET #1, , rwVar$
  NEXT

  ' Load the 64 bytes of initial palette data
  FOR ii = 0 TO 15
    GET #1, , rwVar$
    gamePal(ii, 0) = ASC(rwVar$)
    GET #1, , rwVar$
    gamePal(ii, 1) = ASC(rwVar$)
    GET #1, , rwVar$
    gamePal(ii, 2) = ASC(rwVar$)
    GET #1, , rwVar$
    gamePal(ii, 3) = ASC(rwVar$)
  NEXT ' ii

  ' Load fairy table

  FOR ii = 0 TO 1023
    GET #1, , rwVar$
    fTable(ii) = ASC(rwVar$)
  NEXT ' ii

  ' Load golem table

  ' Golem format is:

  ' 4 bits for palette (P)
  ' 2 bits for flip status (F)
  ' 1 bit reserved, I might want to use two bits for pattern table (T)
  ' 1 bit for pattern table (T)
  ' 8 bits for fairy table index / tile (I), shifted left two slots when loaded, final two bits assumed zero

  ' First two bytes stored as: PPPPFFRT.IIIIIIII

  FOR ii = 0 TO 255 ' Loop through the golem storage

    GET #1, , rwVar$ ' First byte of golem
    byteH = ASC(rwVar$)

    GET #1, , rwVar$ ' Second byte of golem
    byteL = ASC(rwVar$)

    ' Get palette from the top four bits of the high byte
    gTable(ii).Pal = shRight(byteH, 4) ' Shift right four spots

    ' Get two flip bits
    gTable(ii).Flip = shRight(byteH, 2) AND 3 ' Shift right two spots, keep low 2 bits

    ' Bit 1 is reserved

    ' Get the pattern table number T, bit 7
    gTable(ii).pTable = byteH AND 1 ' Shift right 7 spots, no need to strip off any bits

    'byteL = 255
    ' Get the low 8 bits of the index from the low byte, shifting left two
    gTable(ii).Index = byteL * 4

  NEXT ii

  ''''
  ' Load map data

  ' Four bytes in the file read "MAPS", pull and discard those

  GET #1, , rwVar$
  discard = ASC(rwVar$)

  GET #1, , rwVar$
  discard = ASC(rwVar$)

  GET #1, , rwVar$
  discard = ASC(rwVar$)

  GET #1, , rwVar$
  discard = ASC(rwVar$)

  ' Four bytes for all map data at start of the map storage section

  GET #1, , rwVar$ ' Number of maps
  numMaps = ASC(rwVar$)
  IF numMaps = 0 THEN numMaps = 1

  GET #1, , rwVar$ ' Number of bytes per block
  mapBytesPer = ASC(rwVar$)

  GET #1, , rwVar$
  discard = ASC(rwVar$)

  GET #1, , rwVar$
  discard = ASC(rwVar$)

  '''''' Now the eight bytes of map data at the start of any individual map

  FOR im = 0 TO minZero(numMaps - 1)

    GET #1, , rwVar$ ' Flags + bits 11-9 of mpSizeX
    byteH = ASC(rwVar$)

    bits11_9 = byteH AND 15 ' Keep low four bits to append onto mpSizeX

    mapHeader(im).MapWrap = bitRead(byteH, 5)
    mapHeader(im).EdgeBarrier = bitRead(byteH, 4)

    GET #1, , rwVar$ ' Second byte of mpSizeX
    byteL = ASC(rwVar$)
    mapHeader(im).MpSizeX = (bits11_9 * 256) + byteL

    GET #1, , rwVar$ ' mpSizeY, 1 byte

    IF im = 0 THEN ' For map zero, set the double size (minus whatever) value
      mapZeroOrigSizeY = ASC(rwVar$)
      recalcMapSizeYandOffsets
    ELSE
      mapHeader(im).MpSizeY = ASC(rwVar$)
    END IF

    GET #1, , rwVar$ ' LinkedMap, what map to travel to if walking off the edge
    mapHeader(im).LinkedMap = ASC(rwVar$) ' Other details, like the location the player starts, is handled in code

    GET #1, , rwVar$ ' Four more reserved bytes
    discard$ = rwVar$

    GET #1, , rwVar$
    discard$ = rwVar$

    GET #1, , rwVar$
    discard$ = rwVar$

    GET #1, , rwVar$
    discard$ = rwVar$

    ' Now load the block data

    FOR wByte = 0 TO minZero(mapBytesPer - 1)

      IF im = 0 THEN ' Don't load twice the data for mpSizeY when map 0
        targetY = mapZeroOrigSizeY - 1
      ELSE
        targetY = mapHeader(im).MpSizeY - 1
      END IF

      FOR iy = 0 TO targetY
        FOR ix = 0 TO mapHeader(im).MpSizeX - 1
          GET #1, , rwVar$

          wVal = ASC(rwVar$)
          mapData(im, ix, iy, wByte) = wVal

        NEXT ix
      NEXT iy
    NEXT wByte
  NEXT im ' map number

  mapNum = 0 ' Might be loading a game with maps smaller than current, so reset to 0

  CLOSE #1

END SUB ' fileRomLoad

''''''''''''''''''''''''''''''''
SUB fileRomSave

  fln$ = "_demo.rom"

  ' Delete the old file if necessary
  OPEN fln$ FOR BINARY AS #1
  IF LOF(1) > 0 THEN
    CLOSE #1
    KILL fln$
    OPEN fln$ FOR BINARY AS #1
  END IF

  FOR ii = 0 TO 63
    rwVar$ = CHR$(0) ' 64 byes, reserved
    PUT #1, , rwVar$
  NEXT

  ' Store the 64 bytes of initial game palette data
  FOR ii = 0 TO 15

    rwVar$ = CHR$(gamePal(ii, 0))
    PUT #1, , rwVar$

    rwVar$ = CHR$(gamePal(ii, 1))
    PUT #1, , rwVar$

    rwVar$ = CHR$(gamePal(ii, 2))
    PUT #1, , rwVar$

    rwVar$ = CHR$(gamePal(ii, 3))
    PUT #1, , rwVar$

  NEXT ' ii

  ' Store fairy table

  FOR ii = 0 TO 1023
    rwVar$ = CHR$(fTable(ii))
    PUT #1, , rwVar$
  NEXT ii

  ' Store golem table

  ' Golem format is:

  ' 4 bits for palette (P)
  ' 2 bits for flip status (F)
  ' 1 bit reserved, I might want to use two bits for pattern table (T)
  ' 1 bit for pattern table (T)
  ' 8 bits for fairy table index / tile (I), shifted right two slots for file storage (10 bits total with last 2 bits assumed zero)

  ' First two bytes stored as: PPPPFFRT.IIIIIIII

  FOR ii = 0 TO 255

    ' Get four palette bits and put into bits 7-4
    byteHBits7654 = (gTable(ii).Pal AND 15) * 16 ' Shift left four spots

    ' Store flip status
    byteHBits32 = gTable(ii).Flip * 4 ' Shift left two spots

    ' Reserved bit is bit 1

    ' Handle the pattern table number. Golem table can only use pattern tables 0 and 1
    byteHBit0 = (gTable(ii).pTable AND 1) ' Strip to low bit

    ' Combine them
    byteH = byteHBits7654 + byteHBits32 + byteHBit0

    ' Trim the index to 8 bits for low byte
    byteL = shRight(gTable(ii).Index, 2) AND 255 ' The 255 is just for safety, 10-bit value shouldn't go over 1023 so will be < 256 after shift

    rwVar$ = CHR$(byteH)
    PUT #1, , rwVar$

    rwVar$ = CHR$(byteL)
    PUT #1, , rwVar$
  NEXT ii

  ' end golem table

  ' This puts us at position 1,664 in the file, or $680

  ''''
  ' Store map data

  ' Put a note for me in ASCII so I can see where the maps in the file start

  rwVar$ = "M"
  PUT #1, , rwVar$

  rwVar$ = "A"
  PUT #1, , rwVar$

  rwVar$ = "P"
  PUT #1, , rwVar$

  rwVar$ = "S"
  PUT #1, , rwVar$

  ' Put the active map header information for the loaded map into the header first so it gets saved

  ' Now store the four header bytes that go at the beginning of the map section of the file

  rwVar$ = CHR$(numMaps)
  PUT #1, , rwVar$

  rwVar$ = CHR$(mapBytesPer)
  PUT #1, , rwVar$

  rwVar$ = CHR$(0) ' Reserved
  PUT #1, , rwVar$

  rwVar$ = CHR$(0) ' Reserved
  PUT #1, , rwVar$

  '''' Now the four bytes of information for the specific map

  FOR im = 0 TO minZero(numMaps - 1)

    byteH = shRight(mapHeader(im).MpSizeX, 8) AND 15 ' bits 11-9 of mpSizeX

    byteH = bitWrite(byteH, 5, mapHeader(0).MapWrap)
    byteH = bitWrite(byteH, 4, mapHeader(0).EdgeBarrier)

    byteL = mapHeader(im).MpSizeX AND 255 ' Get the low byte of mpSizeX

    rwVar$ = CHR$(byteH) ' Two bytes for X size
    PUT #1, , rwVar$

    rwVar$ = CHR$(byteL)
    PUT #1, , rwVar$

    IF im = 0 THEN ' Compensate for how one map zero the size Y doubles when loading
      rwVar$ = CHR$(mapZeroOrigSizeY) ' One byte for Y size
      PUT #1, , rwVar$
    ELSE
      rwVar$ = CHR$(mapHeader(im).MpSizeY) ' One byte for Y size
      PUT #1, , rwVar$
    END IF
    rwVar$ = CHR$(mapHeader(im).LinkedMap) ' What map is linked for walking off the edge
    PUT #1, , rwVar$

    ' Four more bytes for individual map, reserved

    rwVar$ = CHR$(0)
    PUT #1, , rwVar$

    rwVar$ = CHR$(0)
    PUT #1, , rwVar$

    rwVar$ = CHR$(0)
    PUT #1, , rwVar$

    rwVar$ = CHR$(0)
    PUT #1, , rwVar$

    FOR wByte = 0 TO minZero(mapBytesPer - 1) ' Map block data, store up to four bytes, I put this outside because during implementation I was encountering problems with the ordering of the bytes.
      IF im = 0 THEN ' Don't store the double sized map 0
        targetY = mapZeroOrigSizeY - 1
      ELSE
        targetY = mapHeader(im).MpSizeY - 1
      END IF
      FOR iy = 0 TO targetY
        FOR ix = 0 TO mapHeader(im).MpSizeX - 1

          IF mapData(mapNum, ix, iy, wByte) < 255 THEN ' Make sure we didn't add a block of too high a value
            rwVar$ = CHR$(mapData(im, ix, iy, wByte))
          ELSE
            rwVar$ = CHR$(0) ' Be sure we didn't include anything more than tile 255 on the map to save
          END IF
          PUT #1, , rwVar$
        NEXT
      NEXT
    NEXT ' wByte
  NEXT im ' map number

  CLOSE #1

END SUB ' fileRomSave

''''''''''''''''''''''''''''''''
FUNCTION gtz (wVal, toReturn) ' Checks to see if greater than zero, returns toReturn if so

  IF wVal > 0 THEN gtz = toReturn

END FUNCTION ' gtz

''''''''''''''''''''''''''''''''
FUNCTION hexLen$ (wVal, wLen) '

  outStr$ = HEX$(wVal)

  FOR ii = 0 TO 2 ' Extend up to a length of four
    IF LEN(outStr$) < wLen THEN
      outStr$ = "0" + outStr$
    END IF
  NEXT

  hexLen$ = outStr$

END FUNCTION ' hexLen$

''''''''''''''''''''''''''''''''
FUNCTION hexToDec (wHex$)

  wHex$ = LTRIM$(RTRIM$(wHex$))
  IF LEN(wHex$) > 2 THEN ' Trim to 2 spaces if too long
    wHex$ = LEFT$(wHex$, 2)
  END IF

  SELECT CASE MID$(wHex$, 1, 1)
    CASE "0": wVal = wVal + 0
    CASE "1": wVal = wVal + &H10
    CASE "2": wVal = wVal + &H20
    CASE "3": wVal = wVal + &H30
    CASE "4": wVal = wVal + &H40
    CASE "5": wVal = wVal + &H50
    CASE "6": wVal = wVal + &H60
    CASE "7": wVal = wVal + &H70
    CASE "8": wVal = wVal + &H80
    CASE "9": wVal = wVal + &H90
    CASE "A": wVal = wVal + &HA0
    CASE "B": wVal = wVal + &HB0
    CASE "C": wVal = wVal + &HC0
    CASE "D": wVal = wVal + &HD0
    CASE "E": wVal = wVal + &HE0
    CASE "F": wVal = wVal + &HF0
  END SELECT

  SELECT CASE MID$(wHex$, 2, 1)
    CASE "0": wVal = wVal + 0
    CASE "1": wVal = wVal + 1
    CASE "2": wVal = wVal + 2
    CASE "3": wVal = wVal + 3
    CASE "4": wVal = wVal + 4
    CASE "5": wVal = wVal + 5
    CASE "6": wVal = wVal + 6
    CASE "7": wVal = wVal + 7
    CASE "8": wVal = wVal + 8
    CASE "9": wVal = wVal + 9
    CASE "A": wVal = wVal + &HA
    CASE "B": wVal = wVal + &HB
    CASE "C": wVal = wVal + &HC
    CASE "D": wVal = wVal + &HD
    CASE "E": wVal = wVal + &HE
    CASE "F": wVal = wVal + &HF
  END SELECT

  hexToDec = wVal

END FUNCTION ' hexToDec

''''''''''''''''''''''''''''''''
SUB hWalkCounterInc

  hWalkCounter = hWalkCounter + 1
  IF hWalkCounter > 15 THEN hWalkCounter = 0

  setHWalkFrame

END SUB ' hWalkCounterInc

''''''''''''''''''''''''''''''''
SUB initMapOffsets ' Call any time the map size Y variables are changed

  ' For all of these, initMapOffsetSingle will set the far end the same, mirrored

  mapEdgeTaperRows = 0 ' Each initMapOffsetSingle call increments the total

  initMapOffsetSingle 0, 30 ' 30 on each side, 4 left in center
  initMapOffsetSingle 1, 26 ' 26 on each side, 12 left in center
  initMapOffsetSingle 2, 22 ' 22 on each side, 20 left in center
  initMapOffsetSingle 3, 18 ' 18 on each side, 28 left in center
  initMapOffsetSingle 4, 14 ' 14 on each side, 36 left in center
  initMapOffsetSingle 5, 10 ' 10 on each side, 44 left in center
  initMapOffsetSingle 6, 6 ' 6 on each side, 52 left in center
  initMapOffsetSingle 7, 2 ' 2 on each side, 56 left in center

END SUB ' initMapOffsets

''''''''''''''''''''''''''''''''
SUB initMapOffsetSingle (wRow, wDistance)

  mapOffsetX(wRow) = wDistance ' 30 on each side, 4 left in center
  ' Mirror to the other side
  mapOffsetX((mapZeroOrigSizeY - 1) - wRow) = wDistance

  mapEdgeTaperRows = mapEdgeTaperRows + 1

END SUB ' initMapOffsetSingle

''''''''''''''''''''''''''''''''
SUB initObjects ' Declare objects, make certain ones solid or mobile

  ' Object details that will be used by various processes
  ObjInfo(objBush).Solid = 1
  ObjInfo(objGrave).Solid = 1

  objCount = 0 ' Will change as I add, insert, and delete objects

  objectAdd 1 * 16, 4 * 16, objGrave, SOUTH
  FOR ii = 2 TO 61
    objectAdd ii * 16, ii * 16, objBush, SOUTH
  NEXT
  objectAdd ii * 16, ii * 16, objGrave, SOUTH ' ii will be one higher
  objectAdd (ii + 1) * 16, (ii + 1) * 16, objGrave, SOUTH ' last two will be doors

END SUB ' initObjects

''''''''''''''''''''''''''''''''
SUB initShadowMask ' At present, just applies shadowMask

  ' Guaranteed visible area
  '   ***
  '  *****
  ' *******
  ' ***H***
  ' *******
  '  *****
  '   ***
  ' 0123456

  CALL initShadowMaskRow(0, "  11111")
  CALL initShadowMaskRow(1, " 1111111")
  CALL initShadowMaskRow(2, "111111111")
  CALL initShadowMaskRow(3, "111111111")
  CALL initShadowMaskRow(4, "1111H1111")
  CALL initShadowMaskRow(5, "111111111")
  CALL initShadowMaskRow(6, "111111111")
  CALL initShadowMaskRow(7, " 1111111")
  CALL initShadowMaskRow(8, "  11111")

END SUB

'''''''''''''''''''''''''''''''
SUB initShadowMaskRow (starty, td$)

  FOR ii = 1 TO LEN(td$)
    wChar$ = MID$(td$, ii, 1)
    IF wChar$ = "H" THEN
      wVal = 1
    ELSE
      wVal = VAL(wChar$)
    END IF
    SHADOWMASK(ii - 1, starty) = VAL(wChar$)
  NEXT
END SUB ' initShadowMaskRow

'''''''''''''''''''''''''''''''
FUNCTION inputStr$ (wx, wy) ' Input and locate for a string in one command

  LOCATE wy + 1, wx + 1
  INPUT wStr$

  inputStr$ = wStr$

END FUNCTION ' inputStr$

'''''''''''''''''''''''''''''''
FUNCTION keyCheck (wStr$) ' KeyCheck fast (Follow up with waitKeyRelease to use as slow option)
  wStr$ = UCASE$(wStr$) ' The character that's being checked for. Should be sent uppercase, but just in case
  ' since _KEYDOWN does not return a value, this function was made to poll for key presses and return the value pressed
  ' This function will also help keep track of the special codes that keyDown uses. Only the
  ' codes that have specifically been entered, plus a-z, A-Z (calculated by formula) will work.
  ' Instead of returning 1 that the key has been pressed, the function returns the value of the
  ' key pressed. if keyCheck(whatever$) works just fine, because it checks to see if the return is greater than zero

  ' For the keys that don't exit the program right away, this function stores to rVal instead
  ' of returning right away, to provide the option to put it into a global "last key pressed" variable at the end of the function

  rVal = 0

  IF wStr$ = "*" THEN ' * KEY
    IF _KEYDOWN(42) THEN
      keyCheck = 42
      EXIT FUNCTION
    END IF
  END IF

  IF wStr$ = "ESC" THEN ' Escape key
    IF _KEYDOWN(27) THEN
      keyCheck = 27
      EXIT FUNCTION
    END IF
  END IF

  ' Detect a-z, A-Z, matches ASCII
  IF LEN(wStr$) = 1 THEN ' Handles characters A-Z, uppercase A is 65
    wVal = ASC(wStr$)
    IF ((wVal >= 65) AND (wVal <= 90)) OR ((wVal >= 97) AND (wVal <= 122)) THEN
      IF _KEYDOWN(wVal) OR _KEYDOWN(wVal + 32) THEN rVal = wVal ' The +32 is for lowercase
    END IF
  END IF

  IF LEN(wStr$) = 2 AND MID$(wStr$, 1, 1) = "F" THEN ' Handles characters A-Z, uppercase A is 65

    SELECT CASE MID$(wStr$, 2, 1)
      CASE "1": wVal = 15104
      CASE "2": wVal = 15360
      CASE "3": wVal = 15616
      CASE "4": wVal = 15872
      CASE "5": wVal = 16128
      CASE "6": wVal = 16384
      CASE "7": wVal = 16640
      CASE "8": wVal = 16896
      CASE "9": wVal = 17152
      CASE "A": wVal = 17408
      CASE "B": wVal = 34048
      CASE "C": wVal = 34304
      CASE ELSE: EXIT FUNCTION
    END SELECT

    IF _KEYDOWN(wVal) THEN
      rVal = wVal
    ELSE
      EXIT FUNCTION
    END IF

  END IF

  IF wStr$ = "0" THEN ' KEY 0, KEY INS
    IF _KEYDOWN(48) OR _KEYDOWN(20992) THEN rVal = 50
  END IF

  ' Arrow keys and number pad for controls (not used yet)
  IF wStr$ = "2" THEN '  OR IN$ = "2" OR IN$ = CHR$(0) + "" THEN
    IF _KEYDOWN(50) OR _KEYDOWN(20480) THEN rVal = 50 ' KEY 2, KEY RIGHT
  END IF

  IF wStr$ = "3" THEN
    IF _KEYDOWN(51) OR _KEYDOWN(20736) THEN rVal = 51 ' KEY 3
  END IF

  IF wStr$ = "4" THEN ' OR IN$ = "4" OR IN$ = CHR$(0) + "K" THEN
    IF _KEYDOWN(52) OR _KEYDOWN(19200) THEN rVal = 52 ' KEY 4, KEY LEFT
  END IF

  IF wStr$ = "6" THEN ' OR IN$ = "6" OR IN$ = CHR$(0) + "M" THEN
    IF _KEYDOWN(54) OR _KEYDOWN(19712) THEN rVal = 54 ' KEY 6, KEY RIGHT
  END IF

  IF wStr$ = "8" THEN ' 'OR IN$ = "8" OR IN$ = CHR$(0) + "H" THEN
    IF _KEYDOWN(56) OR _KEYDOWN(18432) THEN rVal = 56 ' KEY 8, KEY UP
  END IF

  ' \ key (right above enter on the keyboard)
  IF wStr$ = "\" THEN
    IF _KEYDOWN(124) OR _KEYDOWN(92) THEN rVal = 124
  END IF

  IF wStr$ = "<" THEN
    IF _KEYDOWN(60) OR _KEYDOWN(44) THEN rVal = 60
  END IF

  IF wStr$ = ">" THEN
    IF _KEYDOWN(62) OR _KEYDOWN(46) THEN rVal = 62
  END IF

  IF wStr$ = "ENT" THEN ' ENTER key
    IF _KEYDOWN(13) THEN rVal = 13
  END IF

  IF wStr$ = "PGUP" THEN
    IF _KEYDOWN(18688) THEN rVal = 18688
  END IF

  IF wStr$ = "PGDN" THEN
    IF _KEYDOWN(20736) THEN rVal = 20736
  END IF

  IF wStr$ = "HOME" THEN
    IF _KEYDOWN(18176) THEN rVal = 18176
  END IF

  IF wStr$ = "END" THEN
    IF _KEYDOWN(20224) THEN rVal = 20224
  END IF

  IF wStr$ = "[" THEN ' Key [ or {
    IF _KEYDOWN(123) OR _KEYDOWN(91) THEN rVal = 123
  END IF

  IF wStr$ = "]" THEN ' Key ] or {
    IF _KEYDOWN(125) OR _KEYDOWN(93) THEN rVal = 125
  END IF


  IF rVal <> 0 THEN lastKeyPress = rVal
  keyCheck = rVal ' Always returns uppercase equivalent

END FUNCTION ' keyCheck

'''''''''''''''''''''''''''''''
FUNCTION map (refX, refY, wByte) ' returns the map value, adjusted for compass direction
  ' These two inner functions retrieve the map value at (wPosX, wPosY, wByte) from mapData.
  ' For wPosY >= mapZeroOrigSizeY, it mirrors the upper half vertically and staggers X at half the map width
  ' If TRIMHORIZONTAL is set, it redirects to skip over the unused tiles, like a hex grid
  ' Uses the current map assigned, mapNum

  IF refX < 0 OR refY < 0 THEN ESCAPETEXT "Error: DX or DY below zero in function map"
  IF wByte <> 0 THEN STOP

  wPosX = mapReturnRX(refX, refY, BYTEZERO)
  wPosY = mapReturnRY(refX, refY, BYTEZERO)

  IF wPosX > 511 THEN STOP
  IF wPosY > 511 THEN STOP

  IF mapNum <> 0 THEN STOP
  IF wPosX < 0 OR wPosX < 0 THEN ESCAPETEXT "Error: RX or RY below zero in function map"

  map = mapData(mapNum, wPosX, wPosY, wByte)

END FUNCTION ' map

''''''''''''''''''''''''''''''''
FUNCTION mapMirrorY (refY)
  ' Returns the vertically mirrored Y position for the compass flip on map 0
  ' Input: refY (DY position in blocks, effective range 0 - (mapHeader(0).mpSize * 2) + (mapEdgeTaperRows * 2))
  ' Output: The mirrored DY position (e.g., 0->7, 8->47, 32->23 if mapZeroOrigSize is 32)
  ' Keeps player notes intact as requested

  wPosY = refY ' Copy to avoid modifying reference

  ' Ensure wPosY is within effective map size
  wPosY = wrapRange(wPosY, 0, mapHeader(0).MpSizeY)

  IF wPosY >= 0 AND wPosY < mapEdgeTaperRows THEN
    ' Top taper mirrors within itself: 0->7, 1->6, ..., 7->0
    mapMirrorY = (mapEdgeTaperRows - 1) - wPosY
    EXIT FUNCTION
  END IF

  IF wPosY >= mapEdgeTaperRows THEN
    ' Mirror across the center of the effective map size
    mapMirrorY = (mapHeader(0).MpSizeY + mapEdgeTaperRows - 1) - wPosY
    EXIT FUNCTION
  END IF

  ' Fallback (shouldn't reach here due to wrapRange)
  mapMirrorY = wPosY
END FUNCTION ' mapMirrorY

''''''''''''''''''''''''''''''''
FUNCTION mapReturnRX (refX, refY, wByte) ' returns the map value, adjusted for compass direction
  ' This function retrieves the redirected X location to access mapData
  ' Uses mapWrapSpecialY for Y to determine staggering and trimming

  ' QB64 passes by reference, copy into new variables so the change aren't passed back
  wPosX = refX
  wPosY = refY

  IF mapNum <> 0 THEN ' Non-world maps use direct access
    mapReturnRX = refX ' Make no change if not map zero
    EXIT FUNCTION
  END IF

  ' Safety to make sure the map size bounds weren't set too low
  IF mapHeader(mapNum).MpSizeX <= 0 OR mapHeader(mapNum).MpSizeY <= 0 OR mapHeader(mapNum).MpSizeX > 512 OR mapHeader(mapNum).MpSizeY > 512 THEN ESCAPETEXT "Mp size variable out of bounds in map function"

  ' Making sure position is on the map, shouldn't get to this point in code if outside of
  IF wPosX < 0 OR wPosY < 0 OR wPosX >= mapHeader(mapNum).MpSizeX * 16 OR wPosY >= mapHeader(mapNum).MpSizeY * 16 THEN ESCAPETEXT "Called mapReturnRX with coordinates outside of bounds"

  ' Apply special Y wrapping first to determine correct row for staggering
  wPosY = mapWrapSpecialY(wPosY)

  IF trimHorizontal > 0 AND mapOffsetX(wPosY) > 0 THEN ' Process sandwiching tapered ends if in an area flagged to do it

    ' Do the "trim" redirect, first calculate whether
    boundaryLow = mapOffsetX(wPosY) ' Left trim boundary
    boundaryHigh = mapHeader(mapNum).MpSizeX - mapOffsetX(wPosY) ' Right trim boundary

    IF wPosX < boundaryLow OR wPosX >= boundaryHigh THEN
      ' Confirmed position is in a taper / trimmed area, redirect X with X-stagger

      wPosX = wPosX + (mapHeader(mapNum).MpSizeX \ 2)
      wPosX = wrapRange(wPosX, 0, mapHeader(mapNum).MpSizeX)

      IF StoreMapZeroEfficient = 1 THEN ' Optional map storage method
        ' We might be undoing what we just did, depending on the area
        ' Eventually, I'll try to find a way to make this more efficient, probably by putting the calculation above in an if statement
        IF refY >= mapZeroOrigSizeY - mapEdgeTaperRows AND refY < mapZeroOrigSizeY THEN

          wPosX = wPosX + (mapHeader(mapNum).MpSizeX \ 2)
          wPosX = wrapRange(wPosX, 0, mapHeader(mapNum).MpSizeX)
        END IF


      END IF ' Efficient storage

    ELSE ' In the center V-shaped space, where I would normally store real map data

      'IF wPosX = 35 AND refY = 28 THEN STOP

      IF refY >= mapZeroOrigSizeY - mapEdgeTaperRows AND refY < mapZeroOrigSizeY THEN
        ' wPosX = wPosX + (mapHeader(mapNum).MpSizeX \ 2)
        ' wPosX = wrapRange(wPosX, 0, mapHeader(mapNum).MpSizeX)
      END IF

      IF StoreMapZeroEfficient = 1 THEN ' Optional map storage method
        ' If in the bottom rows, redirect to the opposite (on the top) corner of the map to load the "inner" data wedges
        ' Makes more efficient use of space to store the bottom rows in the triangles that the top does not use
        IF refY >= mapZeroOrigSizeY - mapEdgeTaperRows AND refY < mapZeroOrigSizeY THEN

          wPosX = wPosX + (mapHeader(mapNum).MpSizeX \ 2)
          wPosX = wrapRange(wPosX, 0, mapHeader(mapNum).MpSizeX)
        END IF
      END IF ' Efficient storage
    END IF

    ' Blocks in the taper rows won't need to be adjusted again, so we can just kick out with the RX value now
    mapReturnRX = wPosX
    EXIT FUNCTION
  END IF ' Sandwich

  IF refY < mapZeroOrigSizeY THEN ' Use refY (DY value) instead of wPosY because wPosY may have already redirected, we want to judge where we really are on the extended map using DY
    ' Upper half, direct access, no automatic X-stagger

  ELSE ' DY location is high enough to use automatic X stagger

    wPosX = wPosX + (mapHeader(mapNum).MpSizeX \ 2)
    wPosX = wrapRange(wPosX, 0, mapHeader(mapNum).MpSizeX)
    mapReturnRX = wPosX
    EXIT FUNCTION
  END IF

  mapReturnRX = wPosX

END FUNCTION ' mapReturnRX

''''''''''''''''''''''''''''''''
FUNCTION mapReturnRY (refX, refY, wByte) ' returns the map value, adjusted for compass direction
  ' This function retrieves the redirected Y location to access mapData
  ' For wPosY >= mapZeroOrigSizeY, it uses mapWrapSpecialY to skip sandwiched rows

  ' for mapsizeorigy = 36, 32 SHOULD BE 4
  ' Here is where I'm trying to work it out

  ' QB64 passes by reference, copy into new variables so the change aren't passed back
  wPosX = refX
  wPosY = refY

  IF mapNum <> 0 THEN ' Non-world maps use direct access
    mapReturnRY = refY ' Make no change if not map zero
    EXIT FUNCTION
  END IF

  ' Safety to make sure the map size bounds weren't set too low
  IF mapHeader(mapNum).MpSizeX <= 0 OR mapHeader(mapNum).MpSizeY <= 0 OR mapHeader(mapNum).MpSizeX > 256 OR mapHeader(mapNum).MpSizeY > 256 THEN ESCAPETEXT "Mp size variable out of bounds in map function"

  ' Making sure position is on the map, shouldn't get to this point in code if outside of the map bounds
  IF wPosX < 0 OR wPosY < 0 OR wPosX >= mapHeader(mapNum).MpSizeX * 16 OR wPosY >= mapHeader(mapNum).MpSizeY * 16 THEN ESCAPETEXT "Called mapReturnRX with coordinates outside of bounds"

  ' Apply special Y wrapping
  wPosY = mapWrapSpecialY(wPosY)

  IF trimHorizontal > 0 AND mapOffsetX(wPosY) > 0 THEN ' Process sandwiching tapered ends if in an area flagged to do it

    ' If too few rows are being redirected on the Y-axis, check to see if every row in the taper area is set, otherwise it won't reach this point for those rows

    ' Check if the position is in a trimmed area and redirect Y, if there are 8 tapered rows on one side, RY needs to redirect up to 8
    boundaryLow = mapOffsetX(wPosY) ' Left "trim" boundary
    boundaryHigh = mapHeader(mapNum).MpSizeX - mapOffsetX(wPosY) ' Right trim boundary

    IF wPosX < boundaryLow OR wPosX >= boundaryHigh THEN
      ' Confirmed position is in a trimmed / taper area, redirect Y using the pre-set Y mirror mapping values

      IF wPosY < mapEdgeTaperRows THEN ' Top taper: mirror within the last mapEdgeTaperRows rows
        wPosY = mirrorRange(wPosY, mapEdgeTaperRows) ' Top taper: e.g., 0->7, 1->6 if mapEdgeTaperRows=8
      END IF

      IF wPosY >= (mapZeroOrigSizeY - mapEdgeTaperRows) THEN ' Bottom taper: mirror within the last mapEdgeTaperRows rows
        wPosY = (mapZeroOrigSizeY - 1) - wPosY
        wPosY = mirrorRange(wPosY, mapEdgeTaperRows)
        wPosY = (mapZeroOrigSizeY - 1) - wPosY
      END IF

      IF StoreMapZeroEfficient = 1 THEN ' Optional map storage method
        ' We already did the mirroring near the edge
        ' If near the bottom edge, in the taper area

        IF refY >= mapZeroOrigSizeY - mapEdgeTaperRows AND refY < mapZeroOrigSizeY THEN
          wPosY = wPosY - mapZeroOrigSizeY + mapEdgeTaperRows ' Gives the mirroring effect near the edge
        END IF

      END IF ' Efficient storage

    ELSE ' In the center V-shaped space, where I would normally store real map data
      IF StoreMapZeroEfficient = 1 THEN ' Optional map storage method
        ' If in the bottom rows, redirect to the opposite (on the top) corner of the map to load the "inner" data wedges
        ' Makes more efficient use of space to store the bottom rows in the triangles that the top does not use
        ' No local flip needed in the taper area
        IF refY >= mapZeroOrigSizeY - mapEdgeTaperRows AND refY < mapZeroOrigSizeY THEN
          wPosY = wPosY - (mapZeroOrigSizeY - mapEdgeTaperRows)
        END IF
      END IF ' Efficient storage
    END IF

    ' Blocks in the taper rows won't need to be adjusted again, so we can just kick out with the RY value now
    mapReturnRY = wPosY
    EXIT FUNCTION
  END IF ' Sandwich

  ' Automatic X stagger in the vertically mirrored, non-tapered space doesn't affect Y, no need to consider

  mapReturnRY = wPosY

END FUNCTION ' mapReturnRY

''''''''''''''''''''''''''''''''
FUNCTION mapWrapSpecialY (refY)
  ' Special Y wrapping function for map 0 to handle a 48-row effective size
  ' Skips the sandwiched hex-tiled rows (0-7 mirrored) when wrapping north or south

  wPosY = refY ' Copy to avoid modifying the reference

  ' Ensure wPosY stays within 0 to effective size Y (currently 48 rows)
  wPosY = wrapRange(wPosY, 0, mapHeader(0).MpSizeY)

  IF wPosY < 0 THEN ESCAPE 34 ' Safety

  ' Direct access for the rows in the non-Y-mirrored section
  IF wPosY < mapZeroOrigSizeY THEN
    mapWrapSpecialY = wPosY
    EXIT FUNCTION
  END IF

  ' As an example, when the original map size Y is 32, within rows 32 to 47, map to the descending range 23 down to 8
  IF wPosY >= mapZeroOrigSizeY THEN
    wPosY = (mapZeroOrigSizeY - mapEdgeTaperRows - 1) - (wPosY - mapZeroOrigSizeY) ' For mapZeroOrigSizeY = 31 and mapHeader(0).mpSizeY, maps 32->23, 33->22, ..., 47->8
    mapWrapSpecialY = wPosY
    EXIT FUNCTION
  END IF

  mapWrapSpecialY = wPosY

END FUNCTION ' mapWrapSpecialY

''''''''''''''''''''''''''''''''
SUB mapWriteTrue (wPosX, wPosY, wByte, wVal) ' writes a value to the specified byte in the map, adjusting for mirroring and staggering
  ' This subroutine writes wVal to the true position in mapData
  ' Parameters: refX (X block position), refY (Y block position), wByte (byte index 0-3), wVal (value to write).
  ' Only call with redirected coordinates when map editing
  ' Uses the current map assigned, mapNum

  mapData(mapNum, wPosX, wPosY, wByte) = wVal

END SUB ' mapWriteTrue

'''''''''''''''''''''''''''''''
FUNCTION maxLimit (wVal, limit)

  IF wVal <= limit THEN
    maxLimit = wVal
  ELSE
    maxLimit = limit
  END IF

END FUNCTION ' maxLimit

'''''''''''''''''''''''''''''''
FUNCTION maxLimitSpecial (wVal, limit, redir)
  ' If over a certain amount, set to redir

  IF wVal <= limit THEN
    maxLimitSpecial = wVal
  ELSE
    maxLimitSpecial = redir
  END IF

END FUNCTION ' maxLimitSpecial

'''''''''''''''''''''''''''''''
SUB midiPlay

  '$UNSTABLE:MIDI

  '$MidiSoundFont:"soundfont2.sf2"

  ' setup screen.
  _TITLE "Midiplay"
  _SCREENMOVE _MIDDLE

  StartLoop:

  'PRINT "  Enter <space>=pause/+plus 10/-minus 10/?=length."
  'PRINT "  Press <escape> to exit, <enter> to restart."

  IF soundHandle THEN

    _SNDPLAY soundHandle
    DO
      _LIMIT 50

      '      x$ = INKEY$
      '      IF LEN(x$) THEN
      '        IF x$ = CHR$(27) THEN
      '          STOP
      '          _SNDSTOP soundHandle
      '          _SNDCLOSE soundHandle
      '          EXIT DO
      '        END IF

      'IF x$ = CHR$(13) THEN
      '  _SNDSTOP soundHandle
      '  _SNDCLOSE soundHandle
      '  GOTO StartLoop
      'END IF

      '     IF x$ = "?" THEN
      '       IF soundHandle <> 0 THEN
      '         S& = _SNDLEN(soundHandle)
      '         S$ = STR$(S&)
      '         IF INSTR(S$, ".") THEN
      '            S$ = LEFT$(S$, INSTR(S$, ".") + 2)
      '          END IF
      '          PRINT "Handle"; soundHandle; "is"; S$; " seconds.";
      '          PRINT " Position is"; _SNDGETPOS(soundHandle)
      '        END IF
      '      END IF

      '    IF x$ = "+" THEN ' Skip ahead
      '      z! = _SNDGETPOS(soundHandle)
      '      z! = z! + 10!
      '      _SNDSETPOS soundHandle, z!
      '    END IF

      '   IF x$ = "-" THEN ' Skip back
      '     z! = _SNDGETPOS(soundHandle)
      '     z! = z! - 10!
      '     IF z! < 0! THEN z! = 0!
      '     _SNDSETPOS soundHandle, z!
      '   END IF

      'IF x$ = " " THEN
      '  STOP
      'IF _SNDPAUSED(soundHandle) THEN
      'PRINT "Resume play."
      '_SNDPLAY soundHandle
      'ELSE
      ' STOP
      ' PRINT "Pause play."
      ' _SNDPAUSE (soundHandle)
      ' END IF
      ' END IF

    LOOP
  END IF

END SUB ' midiPlay

'''''''''''''''''''''''''''''''
FUNCTION minLimit (wVal, limit)

  IF wVal > limit THEN
    minLimit = wVal
  ELSE
    minLimit = limit
  END IF

END FUNCTION ' minLimit

'''''''''''''''''''''''''''''''
FUNCTION minZero (wVal)

  IF wVal <= 0 THEN
    minZero = 0
  ELSE
    minZero = wVal
  END IF

END FUNCTION ' minZero

'''''''''''''''''''''''''''''''
FUNCTION mirrorRange (wNum, rangeLimit)
  ' Mirrors a number within a range from 0 to rangeLimit - 1 (e.g., 0->7, 1->6 if maxVal=8)

  IF wNum >= rangeLimit THEN ESCAPE 14 ' Safety

  mirrorRange = (rangeLimit - 1) - wNum
END FUNCTION ' mirrorRange

'''''''''''''''''''''''''''''''
SUB mouseReadInput

  mouse1Released = 0
  mouse2Released = 0

  DO WHILE _MOUSEINPUT

    mPosX = _MOUSEX: mPosY = _MOUSEY

    pollMouse1Button = _MOUSEBUTTON(1)
    pollMouse2Button = _MOUSEBUTTON(2)

    IF mouse1Clicked <> 0 AND pollMouse1Button = 0 THEN mouse1Released = 1
    IF mouse2Clicked <> 0 AND pollMouse2Button = 0 THEN mouse2Released = 1

    mouse1Clicked = 0
    mouse2Clicked = 0

  LOOP

  ' Mouse button 1 pressed
  IF pollMouse1Button <> 0 THEN mouse1Clicked = 1 ' Mouse button 1 pressed

  ' Mouse button 2 pressed
  IF pollMouse2Button <> 0 THEN mouse2Clicked = 1

END SUB ' mouseReadInput

'''''''''''''''''''''''''''''''
FUNCTION mouseWithinPixelBounds (wStartX, wStartY, wEndX, wEndY)

  IF mPosX >= wStartX AND mPosX <= wEndX AND mPosY >= wStartY AND mPosY <= wEndY THEN
    mouseWithinPixelBounds = 1 ' Return yes

  END IF

END FUNCTION
'''''''''''''''''''''''''''''''

FUNCTION mouseWithinBoxBounds (wStartX, wStartY, wSizeX, wSizeY)

  IF mPosX >= wStartX AND mPosX < wStartX + wSizeX AND mPosY >= wStartY AND mPosY < wStartY + wSizeY THEN
    mouseWithinBoxBounds = 1 ' Return yes

  END IF

END FUNCTION ' mouseWithinBoxBounds
'''''''''''''''''''''''''''''''

FUNCTION moveCheckPlusRelay (dir1, dir2, dir3, dir4, useCornerMechanic)
  ' Compass reset at end end

  IF mapHeader(wMap).EdgeBarrier > 0 THEN
    checkBorder = 1
  ELSE
    checkBorder = 0
  END IF

  SELECT CASE dir1
    CASE NORTH: f = moveCheckPlusNorth(useCornerMechanic, checkBorder)
    CASE WEST: f = moveCheckPlusWest(useCornerMechanic, checkBorder)
    CASE EAST: f = moveCheckPlusEast(useCornerMechanic, checkBorder)
    CASE SOUTH: f = moveCheckPlusSouth(useCornerMechanic, checkBorder)
  END SELECT

  IF f > 0 THEN
    compassCheck shRight(Hero.hTrueX, 4), shRight(Hero.hTrueY, 4)
    moveCheckPlusRelay = f
    EXIT FUNCTION
  END IF

  SELECT CASE dir2
    CASE NORTH: f = moveCheckPlusNorth(useCornerMechanic, checkBorder)
    CASE WEST: f = moveCheckPlusWest(useCornerMechanic, checkBorder)
    CASE EAST: f = moveCheckPlusEast(useCornerMechanic, checkBorder)
    CASE SOUTH: f = moveCheckPlusSouth(useCornerMechanic, checkBorder)
  END SELECT

  IF f > 0 THEN
    compassCheck shRight(Hero.hTrueX, 4), shRight(Hero.hTrueY, 4)
    moveCheckPlusRelay = f
    EXIT FUNCTION
  END IF

  SELECT CASE dir3
    CASE NORTH: f = moveCheckPlusNorth(useCornerMechanic, checkBorder)
    CASE WEST: f = moveCheckPlusWest(useCornerMechanic, checkBorder)
    CASE EAST: f = moveCheckPlusEast(useCornerMechanic, checkBorder)
    CASE SOUTH: f = moveCheckPlusSouth(useCornerMechanic, checkBorder)
  END SELECT

  IF f > 0 THEN
    compassCheck shRight(Hero.hTrueX, 4), shRight(Hero.hTrueY, 4)
    moveCheckPlusRelay = f
    EXIT FUNCTION
  END IF

  SELECT CASE dir4
    CASE NORTH: f = moveCheckPlusNorth(useCornerMechanic, checkBorder)
    CASE WEST: f = moveCheckPlusWest(useCornerMechanic, checkBorder)
    CASE EAST: f = moveCheckPlusEast(useCornerMechanic, checkBorder)
    CASE SOUTH: f = moveCheckPlusSouth(useCornerMechanic, checkBorder)
  END SELECT

  IF f > 0 THEN
    compassCheck shRight(Hero.hTrueX, 4), shRight(Hero.hTrueY, 4)
    moveCheckPlusRelay = f
  END IF

END FUNCTION ' moveCheckPlusRelay

'''''''''''''''''''''''''''''''

FUNCTION moveCheckPlusEast (useCornerMechanic, checkBorder)

  ' Move right, move east
  IF keyCheck("6") THEN ' KEY 6, KEY RIGHT, move right

    origDir = Hero.Dir

    Hero.Dir = EAST
    ff = moveCheck(Hero.hTrueX, Hero.hTrueY, EAST, checkBorder)

    IF modeEdit = 1 THEN
      IF ff = 0 THEN
        Hero.hTrueX = wrapAdd(Hero.hTrueX, mapHeader(mapNum).MpSizeX * 16, 16) ' Move a full block
        moveCheckPlusEast = 1 ' Flag that player moved
        EXIT FUNCTION
      END IF
    END IF

    IF ff = 0 THEN ' No barrier

      vAlign = (Hero.hTrueY AND 7)

      IF vAlign = 0 THEN ' Aligned full or half
        planWalk EAST, EAST
        moveCheckPlusEast = 1 ' Flag that player moved
        EXIT FUNCTION
      ELSE ' 1/4 or 3/4 aligned

        SELECT CASE origDir ' Consider the direction currently facing
          CASE NORTH
            planWalk NORTH, EAST
            moveCheckPlusEast = 1 ' Flag that player moved
            EXIT FUNCTION
          CASE SOUTH:
            planWalk SOUTH, EAST
            moveCheckPlusEast = 1 ' Flag that player moved
            EXIT FUNCTION
          CASE ELSE: ' If facing east or west
            IF vAlign <= 4 THEN
              planWalk NORTH, EAST ' Choose closest block
              moveCheckPlusEast = 1 ' Flag that player moved
              EXIT FUNCTION
            ELSE ' Will be 12 or greater
              planWalk SOUTH, EAST ' Chose closest blck
              moveCheckPlusEast = 1 ' Flag that player moved
              EXIT FUNCTION
            END IF
        END SELECT ' origDir

        'END IF
      END IF '' check of 8-pixel alignment
    ELSE ' ff = 1
      ' Movecheck failed, but double check just above or below to see if clear
      IF useCornerMechanic > 0 THEN ' Use corner wrap mechanic

        vAlign = Hero.hTrueY AND 15

        IF vAlign > 0 AND vAlign <= 4 THEN ' 1/4 aligned, go back
          IF moveCheck(Hero.hTrueX, Hero.hTrueY AND 4080, EAST, checkBorder) = 0 THEN
            planWalk NORTH, EAST
            moveCheckPlusEast = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

        IF vAlign >= 12 AND vAlign <= 15 THEN ' 3/4 aligned, go forward
          IF moveCheck(Hero.hTrueX, (Hero.hTrueY AND 4080) + 16, EAST, checkBorder) = 0 THEN
            planWalk SOUTH, EAST
            moveCheckPlusEast = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

      END IF ' cornerWrapMechanic

      ' No special check if in the middle or 16-pixel aligned
    END IF ' ff
  END IF ' keyCheck

END FUNCTION ' moveCheckPlusEast

'''''''''''''''''''''''''''''''
FUNCTION moveCheckPlusNorth (useCornerMechanic, checkBorder)

  ' Move up, move north
  IF keyCheck("8") THEN ' KEY 8, KEY UP, move up

    origDir = Hero.Dir

    Hero.Dir = NORTH

    ff = moveCheck(Hero.hTrueX, Hero.hTrueY, NORTH, checkBorder)

    IF modeEdit = 1 THEN
      IF ff = 0 THEN
        Hero.hTrueY = wrapSubtract(Hero.hTrueY, mapHeader(mapNum).MpSizeY * 16, 16) ' Move a full block
        moveCheckPlusNorth = 1 ' Flag that player moved
        EXIT FUNCTION
      END IF
    END IF

    IF ff = 0 THEN ' No barrier

      hAlign = (Hero.hTrueX AND 7)

      IF hAlign = 0 THEN ' Aligned full or half
        planWalk NORTH, NORTH
        moveCheckPlusNorth = 1 ' Flag that player moved
        EXIT FUNCTION
      ELSE ' 1/4 or 3/4 aligned

        SELECT CASE origDir ' Consider the direction currently facing
          CASE WEST:
            planWalk WEST, NORTH ' Face north when done
            moveCheckPlusNorth = 1 ' Flag that player moved
            EXIT FUNCTION
          CASE EAST:
            planWalk EAST, NORTH ' Face north when done
            moveCheckPlusNorth = 1 ' Flag that player moved
            EXIT FUNCTION
          CASE ELSE ' If facing north or south move toward the best aligned block
            IF hAlign <= 4 THEN
              planWalk WEST, NORTH ' Choose closest block
              moveCheckPlusNorth = 1 ' Flag that player moved
              EXIT FUNCTION
            ELSE ' Will be 12 or grater
              planWalk EAST, NORTH ' Choose closest block
              moveCheckPlusNorth = 1 ' Flag that player moved
              EXIT FUNCTION
            END IF
        END SELECT ' origDir

      END IF ' check of 8-pixel alignment

    ELSE ' Movecheck failed, but double check just to the left or right to see if clear
      IF useCornerMechanic > 0 THEN ' Use corner wrap mechanic

        hAlign = (Hero.hTrueX AND 15)

        IF hAlign > 0 AND hAlign <= 4 THEN ' 1/4 aligned, go back
          IF moveCheck(Hero.hTrueX AND 4080, Hero.hTrueY, NORTH, checkBorder) = 0 THEN
            planWalk WEST, NORTH
            moveCheckPlusNorth = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

        IF hAlign >= 12 AND hAlign <= 15 THEN ' 3/4 aligned, go forward
          IF moveCheck((Hero.hTrueX AND 4080) + 16, Hero.hTrueY, NORTH, checkBorder) = 0 THEN
            planWalk EAST, NORTH
            moveCheckPlusNorth = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

      END IF ' useCornerMechanic

      ' No special check if in the middle or 16-pixel aligned
    END IF ' ff
  END IF ' keyCheck

END FUNCTION ' moveCheckPlusNorth

'''''''''''''''''''''''''''''''
FUNCTION moveCheckPlusSouth (useCornerMechanic, checkBorder)

  ' Move down, move south
  IF keyCheck("2") THEN ' KEY 2, KEY DOWN, move down

    origDir = Hero.Dir

    Hero.Dir = SOUTH
    ff = moveCheck(Hero.hTrueX, Hero.hTrueY, SOUTH, checkBorder)

    IF modeEdit = 1 THEN
      IF ff = 0 THEN
        Hero.hTrueY = wrapAdd(Hero.hTrueY, mapHeader(mapNum).MpSizeY * 16, 16) ' Move a full block
        moveCheckPlusSouth = 1 ' Flag that player moved
        EXIT FUNCTION
      END IF
    END IF

    IF ff = 0 THEN

      hAlign = (Hero.hTrueX AND 7)

      IF hAlign = 0 THEN
        planWalk SOUTH, SOUTH
        moveCheckPlusSouth = 1 ' Flag that player moved
        EXIT FUNCTION
      ELSE ' 1/4 or 3/4 aligned

        hAlign = (Hero.hTrueX AND 15)
        IF hAlign <= 4 OR hAlign >= 12 THEN

          SELECT CASE origDir ' Consider the direction currently facing
            CASE WEST:
              planWalk WEST, SOUTH ' Face north when done
              moveCheckPlusSouth = 1 ' Flag that player moved
              EXIT FUNCTION

            CASE EAST:
              planWalk EAST, SOUTH ' Face north when done
              moveCheckPlusSouth = 1 ' Flag that player moved
              EXIT FUNCTION

            CASE ELSE ' If facing north or south move toward the best aligned block
              IF hAlign <= 4 THEN
                planWalk WEST, SOUTH
                moveCheckPlusSouth = 1 ' Flag that player moved
                EXIT FUNCTION
              ELSE ' Will be 12 or grater
                planWalk EAST, SOUTH
                moveCheckPlusSouth = 1 ' Flag that player moved
                EXIT FUNCTION
              END IF
          END SELECT ' origDir

        END IF ' check of 8-pixel alignment
      END IF ' Checking 8-pixel alignment for exact

    ELSE ' Movecheck failed, but double check just to the left or right to see if clear
      IF useCornerMechanic > 0 THEN ' Use corner wrap mechanic

        hAlign = (Hero.hTrueX AND 15)

        IF hAlign > 0 AND hAlign <= 4 THEN ' 1/4 aligned, go back
          IF moveCheck(Hero.hTrueX AND 4080, Hero.hTrueY, SOUTH, checkBorder) = 0 THEN
            planWalk WEST, SOUTH
            moveCheckPlusSouth = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

        IF hAlign >= 12 AND hAlign <= 15 THEN ' 3/4 aligned, go forward
          IF moveCheck((Hero.hTrueX AND 4080) + 16, Hero.hTrueY, SOUTH, checkBorder) = 0 THEN
            planWalk EAST, SOUTH
            moveCheckPlusSouth = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

      END IF ' cornerWrapMechanic

      ' No special check if in the middle or 16-pixel aligned
    END IF ' ff
  END IF ' keyCheck

END FUNCTION ' MoveCheckPlusSouth

'''''''''''''''''''''''''''''''
FUNCTION moveCheckPlusWest (useCornerMechanic, checkBorder)

  ' Move left, move west
  IF keyCheck("4") THEN ' KEY 4, KEY LEFT, move left

    origDir = Hero.Dir

    Hero.Dir = WEST
    ff = moveCheck(Hero.hTrueX, Hero.hTrueY, WEST, checkBorder)

    IF modeEdit = 1 THEN
      IF ff = 0 THEN

        Hero.hTrueX = wrapSubtract(Hero.hTrueX, mapHeader(mapNum).MpSizeX * 16, 16) ' Move a full block
        moveCheckPlusWest = 1 ' Flag that player moved
        EXIT FUNCTION
      END IF
    END IF

    IF ff = 0 THEN ' No barrier

      vAlign = (Hero.hTrueY AND 7)

      IF vAlign = 0 THEN
        planWalk WEST, WEST ' Aligned full or half
        moveCheckPlusWest = 1 ' Flag that player moved
        EXIT FUNCTION
      ELSE ' 1/4 or 3/4 aligned

        SELECT CASE origDir ' Consider the direction currently facing
          CASE NORTH
            planWalk NORTH, WEST
            moveCheckPlusWest = 1 ' Flag that player moved
            EXIT FUNCTION
          CASE SOUTH:
            planWalk SOUTH, WEST
            moveCheckPlusWest = 1 ' Flag that player moved
            EXIT FUNCTION
          CASE ELSE: ' If facing east or west
            IF vAlign <= 4 THEN
              planWalk NORTH, WEST ' Choose closest block
              moveCheckPlusWest = 1 ' Flag that player moved
              EXIT FUNCTION
            ELSE ' Will be 12 or greater
              planWalk SOUTH, WEST ' Choose closest block
              moveCheckPlusWest = 1 ' Flag that player moved
              EXIT FUNCTION
            END IF
        END SELECT ' origDir

      END IF ' check of 8-pixel alignment
    ELSE
      ' Movecheck failed, but double check just above or below to see if clear
      IF useCornerMechanic > 0 THEN ' Use corner wrap mechanic

        vAlign = Hero.hTrueY AND 15

        IF vAlign > 0 AND vAlign <= 4 THEN ' 1/4 aligned, go back
          IF moveCheck(Hero.hTrueX, Hero.hTrueY AND 4080, WEST, checkBorder) = 0 THEN
            planWalk NORTH, WEST
            moveCheckPlusWest = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

        IF vAlign >= 12 AND vAlign <= 15 THEN ' 3/4 aligned, go forward
          IF moveCheck(Hero.hTrueX, (Hero.hTrueY AND 4080) + 16, WEST, checkBorder) = 0 THEN
            planWalk SOUTH, WEST
            moveCheckPlusWest = 2 ' Flag that player moved with corner wrap mechanic
            EXIT FUNCTION
          END IF
        END IF

      END IF ' cornerWrapMechanic

      ' No special check if in the middle or 16-pixel aligned
    END IF ' ff
  END IF ' end move west

END FUNCTION ' moveCheckPlusWest

'''''''''''''''''''''''''''''''
FUNCTION moveCheckPoint (passBackX, passBackY, checkBorder)

  ' passBackX and passBackY are passed by reference to implement a wrap option, which was easiest to put inside this function given the movecheck relay

  ' This one only sends the corner position, moved ahead by one pixel

  ' Return 0 if okay to move

  IF checkBorder = 1 THEN ' Check if the coordinates are within the map size bounds
    IF passBackX < 0 OR passBackY < 0 OR passBackX >= (mapHeader(mapNum).MpSizeX * 16) OR passBackY >= (mapHeader(mapNum).MpSizeY * 16) THEN
      moveCheckPoint = 1
      EXIT FUNCTION
    END IF
  END IF

  IF checkBorder = 0 THEN ' Here is where it wraps and the only place it passes back
    passBackX = wrapAt(passBackX, mapHeader(mapNum).MpSizeX * 16)
    passBackY = wrapAt(passBackY, mapHeader(mapNum).MpSizeY * 16)
  END IF

  IF modeEdit = 1 THEN ' Verified within map bounds, now if in edit mode, okay to move
    EXIT FUNCTION ' returns 0
  END IF

  ' Calculate the block coordinates within the MAP array
  blockX = shRight(passBackX, 4)
  blockY = shRight(passBackY, 4)

  ' Ensure block coordinates are within bounds (fixes a bug)
  blockX = blockX MOD mapHeader(mapNum).MpSizeX
  blockY = blockY MOD mapHeader(mapNum).MpSizeY

  ' If the block at the calculated coordinates is a wall, return 1
  IF map(blockX, blockY, 0) >= barrierNum THEN
    moveCheckPoint = 1
    EXIT FUNCTION
  END IF

  '' Check against objects, object barriers, barrier check, object obstacle

  FOR ii = 1 TO objCount - 1 ' Starts at 1 because hero is 0

    IF ObjInfo(Obj(ii).Type).Solid = 1 THEN

      ' objInvalid, objBush, objGrave

      ' Compare the player's position to the object position
      IF passBackX >= Obj(ii).TrueX AND passBackX <= Obj(ii).TrueX + 15 AND passBackY >= Obj(ii).TrueY AND passBackY <= Obj(ii).TrueY + 15 THEN
        moveCheckPoint = 1 ' If so, do not move
        EXIT FUNCTION
      END IF
    END IF
  NEXT ii

  ' If no wall collision is detected, return 0
  moveCheckPoint = 0
END FUNCTION ' moveCheckPoint

'''''''''''''''''''''''''''''''
FUNCTION moveCheck (passBackX, passBackY, wDir, checkBorder)

  ' This one only sends the upper left corner position. A second check is done at the far corner by this function

  ' Having this in a function means I don't have to do a separate if check in the processing of automatic walks

  ' Return 0 if okay to move

  IF wDir = NORTH THEN ' Hero.True values are sent as reference, and can change within for wrapping
    IF moveCheckPoint(passBackX, passBackY - 1, checkBorder) = 1 OR moveCheckPoint(passBackX + 15, passBackY - 1, checkBorder) THEN
      moveCheck = 1
      EXIT FUNCTION
    END IF
  END IF

  IF wDir = EAST THEN
    IF moveCheckPoint(passBackX + 16, passBackY, checkBorder) = 1 OR moveCheckPoint(passBackX + 16, passBackY + 15, checkBorder) THEN
      moveCheck = 1
      EXIT FUNCTION
    END IF
  END IF

  IF wDir = WEST THEN
    IF moveCheckPoint(passBackX - 1, passBackY, checkBorder) = 1 OR moveCheckPoint(passBackX - 1, passBackY + 15, checkBorder) THEN
      moveCheck = 1
      EXIT FUNCTION
    END IF
  END IF

  IF wDir = SOUTH THEN
    IF moveCheckPoint(passBackX, passBackY + 16, checkBorder) = 1 OR moveCheckPoint(passBackX + 15, passBackY + 16, checkBorder) THEN
      moveCheck = 1
      EXIT FUNCTION
    END IF
  END IF

  ' If no wall collision is detected, return 0
  moveCheck = 0
END FUNCTION ' moveCheck

'''''''''''''''''''''''''''''''
FUNCTION numRightDec$ (wVal) ' returns a number as a string with spaces in front to align to the right
  ' Assumes 3 digits

  wStr$ = cTrNum$(wVal) ' Strip off extra spaces to start, string convert, left trim, right trim

  IF LEN(wStr$) < 3 THEN wStr$ = " " + wStr$
  IF LEN(wStr$) < 3 THEN wStr$ = " " + wStr$

  numRightDec$ = wStr$

END FUNCTION ' numRightDec$

'''''''''''''''''''''''''''''''
SUB objectInsert (insertPos, wPosX, wPosY, wType, wDir, wData0, wData1, wData2, wData3)
  ' Inserts an object at specified position and shifts everything above up one
  ' insertPos is the array index where the new object should be inserted

  ' First, check if insertPos is valid
  IF insertPos < 0 OR insertPos > objCount THEN
    EXIT SUB ' Invalid position, exit without doing anything
  END IF

  ' Shift all objects from insertPos and up one position higher
  FOR i = objCount - 1 TO insertPos STEP -1
    Obj(i + 1).TrueX = Obj(i).TrueX
    Obj(i + 1).TrueY = Obj(i).TrueY
    Obj(i + 1).Type = Obj(i).Type
    Obj(i + 1).Dir = Obj(i).Dir
    Obj(i + 1).Data0 = Obj(i).Data0
    Obj(i + 1).Data1 = Obj(i).Data1
    Obj(i + 1).Data2 = Obj(i).Data2
    Obj(i + 1).Data3 = Obj(i).Data3
  NEXT i

  ' Insert the new object at insertPos
  Obj(insertPos).TrueX = wPosX
  Obj(insertPos).TrueY = wPosY
  Obj(insertPos).Type = wType
  Obj(insertPos).Dir = wDir
  Obj(insertPos).Data0 = wData0
  Obj(insertPos).Data1 = wData1
  Obj(insertPos).Data2 = wData2
  Obj(insertPos).Data3 = wData3

  ' Increment the object counter
  objCount = objCount + 1

END SUB ' objectInsert

'''''''''''''''''''''''''''''''
SUB objectAdd (wPosX, wPosY, wType, wDir) ' Short version
  ' Adds an object to the top of the Obj data struture, then increments objCount

  Obj(objCount).TrueX = wPosX
  Obj(objCount).TrueY = wPosY
  Obj(objCount).Type = wType
  Obj(objCount).Dir = wDir

  objCount = objCount + 1 ' objCount is naturally one higher than the top array number

END SUB ' objectAdd

'''''''''''''''''''''''''''''''
SUB objectAddFull (wPosX, wPosY, wType, wDir, wData0, wData1, wData2, wData3)
  ' Adds an object to the Obj data struture, then increments objCount

  Obj(objCount).TrueX = wPosX
  Obj(objCount).TrueY = wPosY
  Obj(objCount).Type = wType
  Obj(objCount).Dir = wDir
  Obj(objCount).Data0 = wData0
  Obj(objCount).Data1 = wData1
  Obj(objCount).Data2 = wData2
  Obj(objCount).Data3 = wData3

  objCount = objCount + 1 ' objCount is naturally one higher than the top array number

END SUB ' objectAddFull

''''''''''''''''''''''''''''''''
FUNCTION palAdd (wClr, wPal) ' If value is 0-3, draw from palettes

  IF wPal > 17 THEN '  PALNONE is 17
    ESCAPETEXT "palAdd called with palette #" + cTrNum$(wClr)
  END IF

  IF wClr > 3 OR wPal = PALNONE THEN
    palAdd = wClr
    EXIT FUNCTION
  END IF

  palAdd = gamePal(wPal, wClr)

END FUNCTION ' palAdd

'''''''''''''''''''''''''''''''
SUB planWalk (wDir, wDirWhenDone)

  walkDirWhenDone = wDirWhenDone

  IF wDir = NORTH THEN

    Hero.hTrueY = Hero.hTrueY - 1

    SELECT CASE (Hero.hTrueY) AND 3
      CASE 0: walkLen = 0 ' Already a multiple of 4
      CASE 1: walkLen = 1
      CASE 2: walkLen = 2
      CASE 3: walkLen = 3
    END SELECT

    walkDir = NORTH
    Hero.Dir = NORTH
    walkActive = 1
    walkDistance = walkLen

    EXIT SUB
  END IF

  IF wDir = WEST THEN
    Hero.hTrueX = Hero.hTrueX - 1

    SELECT CASE (Hero.hTrueX) AND 3 ' + ix to avoid going into negatives
      CASE 0: walkLen = 0 ' Already a multiple of 4
      CASE 1: walkLen = 1
      CASE 2: walkLen = 2
      CASE 3: walkLen = 3
    END SELECT

    walkDir = WEST
    Hero.Dir = WEST
    walkActive = 1
    walkDistance = walkLen

    EXIT SUB
  END IF

  IF wDir = EAST THEN
    Hero.hTrueX = Hero.hTrueX + 1
    SELECT CASE Hero.hTrueX AND 3
      CASE 0: walkLen = 0
      CASE 1: walkLen = 3
      CASE 2: walkLen = 2
      CASE 3: walkLen = 1
    END SELECT

    walkDir = EAST
    Hero.Dir = EAST
    walkActive = 1
    walkDistance = walkLen

    EXIT SUB
  END IF

  IF wDir = SOUTH THEN

    Hero.hTrueY = Hero.hTrueY + 1
    SELECT CASE Hero.hTrueY AND 3
      CASE 0: walkLen = 0
      CASE 1: walkLen = 3
      CASE 2: walkLen = 2
      CASE 3: walkLen = 1
    END SELECT

    walkDir = SOUTH
    Hero.Dir = SOUTH
    walkActive = 1
    walkDistance = walkLen

    EXIT SUB
  END IF

END SUB ' planWalk

'''''''''''''''''''''''''''''''
SUB PRINTCLR (wStr$, wClr) ' Prints a certain color and then sets the text back to color white

  COLOR wClr
  PRINT wStr$
  COLOR 15 ' White

END SUB

'''''''''''''''''''''''''''''''
SUB PRINTCHR (wPosPixelsX, wPosPixelsY, wStr$, fgClr, bgClr) ' Prints a single character

  ' Can draw a custom font character anywhere on the screen

  ' Assume one character

  IF LEN(wStr$) <> 1 THEN
    CLS
    COLOR 15
    PRINT "ERROR: Only send one character to PRINTC"
    PRINT "Character string sent was '"; wStr$; "'"
    _DISPLAY
    END
  END IF

  wTile = ASC(wStr$)

  ' Split wTile into X and Y positions for our 8 x 8 tiles
  offsetY = wTile \ 16 ' Shift right four bits
  offsetX = wTile AND 15 ' Drop high four bits

  FOR iy = 0 TO 7
    FOR ix = 0 TO 7
      drawPosX = (wPosPixelsX + ix) ' Since they will be used several times
      drawPosY = (wPosPixelsY + iy)

      wClr = fontData((offsetX * 8) + ix, (offsetY * 8) + iy)
      IF wClr = 0 THEN wClr = bgClr
      IF wClr = 15 THEN wClr = fgClr

      PSET (drawPosX, drawPosY), wClr
    NEXT ' ix
  NEXT ' iy

END SUB ' PRINTCHR

'''''''''''''''''''''''''''''''
SUB printStr (wPosPixelsX, wPosPixelsY, wStr$, fgClr, bgClr)

  FOR ii = 1 TO LEN(wStr$)
    toSend$ = MID$(wStr$, ii, 1)
    PRINTCHR wPosPixelsX - 8 + (ii * 8), wPosPixelsY, toSend$, fgClr, bgClr
  NEXT
END SUB ' printStr

'''''''''''''''''''''''''''''''
SUB processInput (IN$) ' Process keyboard input

  ' Exit keys:
  IF keyCheck("ESC") OR keyCheck("*") THEN ' Slow key ESC, key *
    ESCAPETEXT "PROGRAM ENDED"
  END IF

  IF keyCheck("P") THEN ' Toggle pattern table mode
    IF modePT >= 3 THEN ' Can go up to mode 3
      modePT = 0
    ELSE
      modePT = modePT + 1
    END IF
    waitKeyRelease "P"
    EXIT SUB
  END IF

  IF keyCheck("L") THEN
    COLOR 14
    ZLOCATE 8, 8: PRINT "          "
    ZLOCATE 8, 9: PRINT "LOADING FILE"
    ZLOCATE 8, 10: PRINT "          "
    COLOR 15

    fileRomLoad
    waitKeyRelease "L"
    EXIT SUB
  END IF

  IF keyCheck("S") THEN
    COLOR 14
    ZLOCATE 8, 8: PRINT "          "
    ZLOCATE 8, 9: PRINT "SAVING FILE"
    ZLOCATE 8, 10: PRINT "          "
    COLOR 15
    fileRomSave
    waitKeyRelease "S"
    EXIT SUB
  END IF

  IF keyCheck("C") THEN
    IF wMap = 0 THEN
      waitKeyRelease "C"

      ' In RPG, player will always have come to a 16 pixel alignment after a step
      DX = shRight(Hero.hTrueX, 4)
      DY = shRight(Hero.hTrueY, 4)

      ' Calculate the flipped coordinates
      'newDX = 'mapMirrorX(DX, DY)
      newDX = wrapAdd(DX, mapHeader(0).MpSizeX, 32) ' X coordinates Will always be 32 apart (I'll make 32 a variable later)
      'newDY = DX
      newDY = mapMirrorY(DY)

      ' Update player's true position (pixel coordinates)
      Hero.hTrueX = newDX * 16 ' Shift left 4 bits by multiplying by 16
      Hero.hTrueY = newDY * 16

      ' Now determine

      compassCheck shRight(Hero.hTrueX, 4), shRight(Hero.hTrueY, 4)

    END IF ' wMap
  END IF ' Key C

  IF keyCheck("O") THEN
    waitKeyRelease "O"
    Obj(0).TrueX = Obj(0).TrueX + 16
  END IF

  IF keyCheck("T") THEN
    waitKeyRelease "O"
    Obj(0).TrueX = Obj(0).TrueX + 1
    IF Obj(2).TrueX = Obj(0).TrueX > (viewSizeBlocksX * 16) THEN Obj(0).TrueX = 0
  END IF

  IF modePT > 0 THEN
    processInputmodePT (IN$)
    EXIT SUB
  END IF

  ''''
  ' Movement section

  '' If walking, continue walk, process walk
  IF walkActive = 1 THEN
    IF walkDistance > 0 THEN
      hWalkCounterInc

      IF mapNum = 0 THEN
        ff = moveCheck(Hero.hTrueX, Hero.hTrueY, walkDir, 0) ' Allow walk wrap
      ELSE
        ff = moveCheck(Hero.hTrueX, Hero.hTrueY, walkDir, 1)
      END IF

      IF ff = 0 THEN
        IF walkDir = NORTH THEN
          Hero.hTrueY = wrapSubtract(Hero.hTrueY, mapHeader(mapNum).MpSizeY * 16, 1)
          walkDistance = walkDistance - 1
        END IF ' walkDir

        IF walkDir = EAST THEN
          Hero.hTrueX = wrapAdd(Hero.hTrueX, mapHeader(mapNum).MpSizeX * 16, 1)
          walkDistance = walkDistance - 1
        END IF ' walkDir

        IF walkDir = WEST THEN
          Hero.hTrueX = wrapSubtract(Hero.hTrueX, mapHeader(mapNum).MpSizeX * 16, 1)
          walkDistance = walkDistance - 1
        END IF ' walkDir

        IF walkDir = SOUTH THEN
          Hero.hTrueY = wrapAdd(Hero.hTrueY, mapHeader(mapNum).MpSizeY * 16, 1)
          walkDistance = walkDistance - 1
        END IF ' walkDir
      ELSE
        walkDistance = 0 ' In case I hit an object on a sub-four pixel mark
        Hero.Dir = walkDirWhenDone
      END IF

      IF walkDistance <= 0 THEN
        walkDistance = 0
        walkActive = 0
        Hero.Dir = walkDirWhenDone
        EXIT SUB
      END IF
    ELSE ' If walkDistance = 0 then
      walkActive = 0 ' Clear walkdistance
    END IF ' walkDistance
  END IF

  IF modeEdit = 1 THEN ' Fix a rare bug where if the player pushes a direction about the same time as edit, it doesn't align
    Hero.hTrueX = Hero.hTrueX AND 4080 ' 1111.11110000
    Hero.hTrueY = Hero.hTrueY AND 4080
  END IF

  ' So there aren't extra walking frames from a diagonal move
  IF walkActive = 0 THEN
    IF (keyCheck("2") OR keyCheck("4") OR keyCheck("6") OR keyCheck("8")) THEN hWalkCounterInc

    IF cornerMechanicTriggered = 0 THEN

      SELECT CASE Hero.Dir
        CASE NORTH:
          f = moveCheckPlusRelay(WEST, EAST, NORTH, SOUTH, 1)
        CASE WEST:
          f = moveCheckPlusRelay(NORTH, SOUTH, WEST, EAST, 1)
        CASE EAST:
          f = moveCheckPlusRelay(NORTH, SOUTH, EAST, WEST, 1)
        CASE SOUTH:
          f = moveCheckPlusRelay(WEST, EAST, SOUTH, NORTH, 1)
      END SELECT

      IF f = 2 THEN
        cornerMechanicTriggered = 1
        cornerMechanicClock = TIMER
      END IF
    ELSE ' Just used corner mechanic
      SELECT CASE Hero.Dir
        CASE NORTH:
          f = moveCheckPlusRelay(NORTH, WEST, EAST, SOUTH, 0)
        CASE WEST:
          f = moveCheckPlusRelay(WEST, NORTH, SOUTH, EAST, 0)
        CASE EAST:
          f = moveCheckPlusRelay(EAST, NORTH, SOUTH, WEST, 0)
        CASE SOUTH:
          f = moveCheckPlusRelay(SOUTH, WEST, EAST, NORTH, 0)
      END SELECT

      IF TIMER >= (cornerMechanicClock + .75) THEN ' Delay of 3/4 seconds before the corner mechanic can be used again
        cornerMechanicTriggered = 0
        cornerMechanicClock = TIMER
      END IF
    END IF
  END IF ' walkActive = 1

  IF keyCheck("[") THEN
    Hero.hTrueX = Hero.hTrueX - 1 ' For testing, decrease player's X position by 1
    IF Hero.hTrueX < 0 THEN Hero.hTrueX = (mapHeader(wMap).MpSizeX * 16) - 1
    Hero.Dir = WEST
    waitKeyRelease "["
  END IF

  IF keyCheck("]") THEN
    Hero.hTrueX = Hero.hTrueX + 1 ' For testing, increase player's X position by 1
    IF Hero.hTrueX >= (mapHeader(wMap).MpSizeX * 16) THEN Hero.hTrueX = 0 ' For size 64, if greater than 1024
    Hero.Dir = EAST
    waitKeyRelease "]"
  END IF

  IF keyCheck("0") THEN ' Action button

    IF projCount = 0 THEN
      projCount = 1

      proj_PosX(0) = Hero.hTrueX + 16
      proj_PosY(0) = Hero.hTrueY
      proj_Type = 1
    END IF
  END IF

  ' Less important elements down here to speed up action control

  IF keyCheck("W") THEN
    Hero.hTrueX = Hero.hTrueX AND 4080 ' 1111.11110000
    Hero.hTrueY = Hero.hTrueY AND 4080
    hWalkCounter = 0
    hWalkFrame = 0

    waitKeyRelease "W"

  END IF

  IF keyCheck("E") AND modePT = 0 THEN ' Toggle edit mode

    IF modeEdit = 0 THEN
      modeEdit = 1

      Hero.hTrueX = Hero.hTrueX AND 4080 ' 1111.11110000
      Hero.hTrueY = Hero.hTrueY AND 4080

    ELSE
      modeEdit = 0
    END IF
    waitKeyRelease "E"
    EXIT SUB
  END IF

  IF keyCheck("D") THEN
    IF showShadows = 0 THEN
      showShadows = 1
    ELSE
      showShadows = 0
    END IF
    waitKeyRelease "D" ' So it doesn't process too fast, loop back again, and switch right back on
    EXIT SUB
  END IF

  IF keyCheck("H") THEN
    IF wrapHorizontal = 0 THEN
      wrapHorizontal = 1
    ELSE
      wrapHorizontal = 0
    END IF
    waitKeyRelease "H"
    EXIT SUB
  END IF

  IF keyCheck("V") THEN
    IF wrapVertical = 0 THEN
      wrapVertical = 1
    ELSE
      wrapVertical = 0
    END IF
    waitKeyRelease "V"
    EXIT SUB
  END IF

  ' Map editing

  IF keyCheck("ENT") AND modeEdit = 1 THEN ' ENTER during edit mode, change map objects
    waitKeyRelease "ENT"

    discard$ = INKEY$ ' Somehow fixes a bug that skips over input statements, may be _DISPLAY related

    ZLOCATE 10, 9: PRINT "              " ' Space around input
    ZLOCATE 10, 10: PRINT "  TILENUM$   "
    ZLOCATE 10, 11: PRINT "               "
    LINE (81, 72)-(191, 72), 1
    LINE (81, 92)-(191, 92), 1

    newTile$ = inputStr$(20, 10)

    newTile$ = UCASE$(LTRIM$(RTRIM$(newTile$)))

    DX = shRight(Hero.hTrueX, 4)
    DY = shRight(Hero.hTrueY, 4)
    RX = mapReturnRX(DX, DY, 0) ' Redirect to the correct position in memory storage
    RY = mapReturnRY(DX, DY, 0)

    SELECT CASE (newTile$)
      CASE "T": mapWriteTrue RX, RY, BYTEZERO, gBlockTree
      CASE "R": mapWriteTrue RX, RY, BYTEZERO, gBlockRock
      CASE "W": mapWriteTrue RX, RY, BYTEZERO, gBlockWall
      CASE "$": mapWriteTrue RX, RY, BYTEZERO, gBlockChest
      CASE "S": mapWriteTrue RX, RY, BYTEZERO, gBlockStatue

      CASE ELSE

        newTileVal = VAL(newTile$)

        IF newTileVal > 255 OR newTile$ = "" OR LEN(newTile$) > 2 THEN
          waitKeyRelease ("ENT")
          EXIT SUB ' Skip over
        END IF

        mapWriteTrue RX, RY, BYTEZERO, newTileVal
    END SELECT

    EXIT SUB
  END IF ' Enter key when in modeEdit

END SUB ' processInput

'''''''''''''''''''''''''''''''
SUB processInputmodePT (IN$) ' Process keyboard input when in either PT mode

  IF IN$ <> "" THEN ESCAPE 222 ' Ensure I never call this without something in IN$

  IF modePT = 2 THEN ' For the fairy table scroll bar keyboard control

    IF keyCheck("4") THEN
      ' Decrement the fairy table block selected
      IF ptmFairyTileSelect > 3 THEN ptmFairyTileSelect = ptmFairyTileSelect - 4
      IF (ptmFairyTileSelect AND 65532) < ptmFairyStripBase THEN ptmFairyStripBase = (ptmFairyTileSelect AND 65532)
    END IF

    IF keyCheck("6") THEN
      ' Increment the fairy table block selected
      IF ptmFairyTileSelect < 1020 THEN ptmFairyTileSelect = ptmFairyTileSelect + 4
      IF (ptmFairyTileSelect AND 65532) > ptmFairyStripBase + 40 THEN ptmFairyStripBase = (ptmFairyTileSelect AND 65532) - 40
    END IF

    IF keyCheck("N") THEN
      IF ptmFairyTileSelect < 1023 THEN ptmFairyTileSelect = ptmFairyTileSelect + 1
      waitKeyRelease "N"
    END IF

    IF keyCheck("M") THEN
      IF ptmFairyTileSelect < 1020 THEN ptmFairyTileSelect = (ptmFairyTileSelect + 4) AND 65532
      waitKeyRelease "M"
    END IF

  END IF ' modePT 2

  IF modePT = 3 THEN

    IF keyCheck("F") THEN
      gTable(ptmGolemSelect).Flip = gTable(ptmGolemSelect).Flip + 1
      IF gTable(ptmGolemSelect).Flip > 3 THEN gTable(ptmGolemSelect).Flip = 0
      waitKeyRelease "F"
    END IF

    IF keyCheck("N") THEN
      IF ptmGolemSelect < 255 THEN ptmGolemSelect = ptmGolemSelect + 1
      IF ptmGolemSelect = 128 THEN ptmGolemTableSelect = 1
      waitKeyRelease "N"
    END IF

  END IF

  ' Page up to increase tile table in modePT 2
  IF keyCheck("HOME") THEN
    IF ptmPtnTableSelect > 0 THEN ptmPtnTableSelect = ptmPtnTableSelect - 1
    waitKeyRelease ("HOME")
  END IF

  ' Page up to increase tile table in modePT 2
  IF keyCheck("END") THEN
    IF ptmPtnTableSelect < 3 THEN ptmPtnTableSelect = ptmPtnTableSelect + 1
    waitKeyRelease ("END")
  END IF

  ' Page down to increase selected palette table in modePT 1 or 2
  IF keyCheck("PGUP") THEN
    IF ptmPalSelect > 0 THEN
      ptmPalSelect = ptmPalSelect - 1
    ELSE
      ptmPalSelect = 16 ' STDPAL
    END IF
    waitKeyRelease ("PGUP")
    EXIT SUB
  END IF

  IF keyCheck("PGDN") THEN

    ' Page up to increase selected palette in modePT 1 or 2
    IF ptmPalSelect < 16 THEN
      ptmPalSelect = ptmPalSelect + 1
    ELSE
      ptmPalSelect = 0
    END IF

    waitKeyRelease ("PGDN")
    EXIT SUB
  END IF

END SUB ' processInputmodePT

'''''''''''''''''''''''''''''''
SUB processMouseInputRedrawAll ' process mouse input for non-edit mode, called at the end of redrawall to ensure I can draw boxes

  '''' Process mouse input when not in edit mode

  pStartX = 8 * 54
  pStartY = 0
  textX = 54

  '  drawClearBox pStartX, pStartY, 64, 7, 3

  '''' Mouse input for drawGolemGridPMI, selecting 2x2 block from golem table
  IF mouse1Clicked = 0 AND mouse2Clicked = 0 THEN EXIT SUB

  FOR ii = 0 TO 15 ' Try to clear the keyboard buffer so a directional press doesn't add numbers to input
    discard$ = INKEY$
  NEXT

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 1), 64, 7) THEN ' set numMaps
    ZLOCATE textX, 1: PRINT "         "
    ZLOCATE textX, 1:
    INPUT wVar
    IF wVar >= 1 AND wVar <= 4 THEN numMaps = wVar

    FOR im = 0 TO minZero(numMaps - 1)
      ' If any of the maps are new, the size will be zero, so fix that now
      IF mapHeader(im).MpSizeX <= 0 THEN mapHeader(im).MpSizeX = 20
      IF mapHeader(im).MpSizeY <= 0 THEN mapHeader(im).MpSizeY = 20
      IF mapBytesPer = 0 THEN mapBytesPer = 1
    NEXT

    IF mapHeader(mapNum).LinkedMap > numMaps - 1 THEN mapHeader(mapNum).LinkedMap = 0 ' Extra safety
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 2), 64, 7) THEN ' set mapBytesPer
    ZLOCATE textX, 2: PRINT "         "
    ZLOCATE textX, 2:
    INPUT wVar
    IF wVar > 0 AND wVar <= 4 THEN mapBytesPer = wVar
    IF mapBytesPer = 0 THEN mapBytesPer = 1 ' Extra safety
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 3), 64, 7) THEN ' set mapNum
    ZLOCATE textX, 3: PRINT "         "
    ZLOCATE textX, 3:
    INPUT wVar
    IF wVar >= 0 AND wVar <= 4 AND wVar < numMaps THEN mapNum = wVar

    ' If the map is new, the size will be zero, so fix that now
    IF mapHeader(mapNum).MpSizeX <= 0 THEN mapHeader(mapNum).MpSizeX = 20
    IF mapHeader(mapNum).MpSizeY <= 0 THEN mapHeader(mapNum).MpSizeY = 20

    DX = shRight(Hero.hTrueX, 4) ' If the hero is past the new map edge, move back
    DY = shRight(Hero.hTrueY, 4)
    IF DX > mapHeader(mapNum).MpSizeX THEN Hero.hTrueX = (mapHeader(mapNum).MpSizeX - 1) * 16
    IF DY > mapHeader(mapNum).MpSizeY THEN Hero.hTrueY = (mapHeader(mapNum).MpSizeY - 1) * 16
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 4), 64, 7) THEN ' set edgeBarrier
    ZLOCATE textX, 4: PRINT "         "
    ZLOCATE textX, 4:
    INPUT wStr$
    wStr$ = UCASE$(cTrim$(wStr$))

    IF wStr$ <> "" THEN ' Leave as it was if user didn't enter anything
      IF wStr$ = "Y" OR wStr$ = "1" THEN mapHeader(mapNum).EdgeBarrier = 1
      IF wStr$ = "N" OR wStr$ = "0" THEN mapHeader(mapNum).EdgeBarrier = 0
    END IF
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 5), 64, 7) THEN ' set MapWrap
    ZLOCATE textX, 5: PRINT "         "
    ZLOCATE textX, 5
    INPUT wStr$
    wStr$ = UCASE$(cTrim$(wStr$))

    IF wStr$ <> "" THEN ' Leave as it was if user didn't enter anything
      IF wStr$ = "Y" OR wStr$ = "1" THEN mapHeader(mapNum).MapWrap = 1
      IF wStr$ = "N" OR wStr$ = "0" THEN mapHeader(mapNum).MapWrap = 0
    END IF
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 6), 64, 7) THEN ' set mpSizeX
    ZLOCATE textX, 6: PRINT "         "
    ZLOCATE textX, 6:
    INPUT wVar
    IF wVar >= 8 AND wVar <= 512 THEN mapHeader(mapNum).MpSizeX = wVar

    DX = shRight(Hero.hTrueX, 4) ' If the hero is past the new map edge, move back
    IF DX > mapHeader(mapNum).MpSizeX THEN Hero.hTrueX = (mapHeader(mapNum).MpSizeX - 1) * 16

  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 7), 64, 7) THEN ' set mpSizeY
    ZLOCATE textX, 7: PRINT "         "
    ZLOCATE textX, 7:
    INPUT wVar
    IF wVar >= 8 AND wVar <= 255 THEN
      IF mapNum = 0 THEN ' Account for the double sizing of map zero
        mapZeroOrigSizeY = wVar
        recalcMapSizeYandOffsets
      ELSE
        mapHeader(mapNum).MpSizeY = wVar
      END IF

      DY = shRight(Hero.hTrueY, 4) ' If the hero is past the new map edge, move back
      IF DY > mapHeader(mapNum).MpSizeY THEN Hero.hTrueY = (mapHeader(mapNum).MpSizeY - 1) * 16
    END IF
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 8), 64, 7) THEN ' set LinkedMap
    ZLOCATE textX, 8: PRINT "         "
    ZLOCATE textX, 8:
    INPUT wVar
    IF wVar >= 0 AND wVar <= numMaps - 1 THEN mapHeader(mapNum).LinkedMap = wVar
    IF mapHeader(mapNum).LinkedMap > numMaps - 1 THEN mapHeader(mapNum).LinkedMap = 0 ' Extra safety
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 14), 64, 7) THEN ' set trimHorizontal
    ZLOCATE textX, 14: PRINT "         "
    ZLOCATE textX, 14:
    INPUT wStr$
    wStr$ = UCASE$(cTrim$(wStr$))

    IF wStr$ <> "" THEN ' Leave as it was if user didn't enter anything
      IF wStr$ = "Y" OR wStr$ = "1" THEN trimHorizontal = 1
      IF wStr$ = "N" OR wStr$ = "0" THEN
        trimHorizontal = 0
        StoreMapZeroEfficient = 0 ' Doesn't make sense to use if trimHorizontal is off, those blocks would show in two places
      END IF
    END IF
  END IF

  IF mouseWithinBoxBounds(pStartX, pStartY + (8 * 15), 64, 7) AND trimHorizontal = 1 THEN ' set storeMapZeroEfficient
    ' Doesn't make sense to use if trimHorizontal is off, those blocks would show in two places
    ZLOCATE textX, 15: PRINT "         "
    ZLOCATE textX, 15
    INPUT wStr$
    wStr$ = UCASE$(cTrim$(wStr$))

    IF wStr$ <> "" THEN ' Leave as it was if user didn't enter anything
      IF wStr$ = "Y" OR wStr$ = "1" THEN StoreMapZeroEfficient = 1
      IF wStr$ = "N" OR wStr$ = "0" THEN StoreMapZeroEfficient = 0
    END IF
  END IF

END SUB

'''''''''''''''''''''''''''''''
SUB PSETmult (wPosX, wPosY, wClr, wSize) ' Draws a 2 x 2 pixel or a 3 x 3 pixel square

  IF wSize = TIMESTWO THEN

    PSET (wPosX, wPosY), wClr
    PSET (wPosX + 1, wPosY), wClr
    PSET (wPosX, wPosY + 1), wClr
    PSET (wPosX + 1, wPosY + 1), wClr
    EXIT SUB
  END IF

  IF wSize = TIMESTHREE THEN
    PSET (wPosX, wPosY), wClr
    PSET (wPosX + 1, wPosY), wClr
    PSET (wPosX + 2, wPosY), wClr
    PSET (wPosX, wPosY + 1), wClr
    PSET (wPosX + 1, wPosY + 1), wClr
    PSET (wPosX + 2, wPosY + 1), wClr
    PSET (wPosX, wPosY + 2), wClr
    PSET (wPosX + 1, wPosY + 2), wClr
    PSET (wPosX + 2, wPosY + 2), wClr
  END IF

END SUB ' PSETmult

'''''''''''''''''''''''''''''''
FUNCTION pTableStart ' Returns the table base index of the pTable selected in modePTEdit

  pTableStart = ptmPtnTableSelect * 256

END FUNCTION ' pTableStart

'''''''''''''''''''''''''''''''
SUB recalcMapSizeYandOffsets

  mapHeader(0).MpSizeY = (mapZeroOrigSizeY * 2) - (mapEdgeTaperRows * 2) ' World map, fix topology
  initMapOffsets ' Recalculates the offsets on the far end Y using mirroring

END SUB ' recalcMapSizeYandOffsets

'''''''''''''''''''''''''''''''
SUB redrawAll

  COLOR 15
  CLS

  'PRINT "MODE "; cTrNum$(modePT); ": ";
  SELECT CASE modePT
    CASE 0: ' Normal gameplay, draw border box around playfield area
      drawBorderBox viewStartPixelsX - 1, viewStartPixelsY - 1, (viewSizeTilesX * 8) + 2, (viewSizeTilesY * 8) + 2, 14, 0

    CASE 1:
      ZLOCATE 9, 0
      PRINT "MODE 1: PALETTES"
      displaymodePT1
      EXIT SUB
    CASE 2:
      ZLOCATE 9, 0
      PRINT "MODE 2: FTBL VIEW"
      displaymodePT2
      EXIT SUB
    CASE 3:
      ZLOCATE 9, 0
      PRINT "MODE 3: GOLEM TBL"
      displaymodePT3
      EXIT SUB

    CASE ELSE: PRINT "ERROR"
  END SELECT

  CALL splitPos(Hero.hTrueX, Hero.hTrueY) ' Pass the true X and Y positions to be split into map and screen positions

  clearAttrTables
  clearBGtable

  ' DRAW BACKGROUND, DRAW MAP

  copyMaptoBG ' load data into the bgTable array and attribute tables, next we draw from there

  ' Calculate the pixel offsets for smooth scrolling
  lowBitsX = mapPosX AND 15 ' Low 4 bits for sub-tile X offset
  lowBitsY = mapPosY AND 15 ' Low 4 bits for sub-tile Y offset

  ' Loop through visible tiles
  FOR iy = 0 TO (viewSizeTilesY - 1) + gtz(lowBitsY, 2)
    FOR ix = 0 TO (viewSizeTilesX - 1) + gtz(lowBitsX, 2)
      ' Calculate the on-screen position for each 8x8 tile
      drawPosX = (ix * 8) - lowBitsX
      drawPosY = (iy * 8) - lowBitsY

      ' Draw the sub-tile
      drawTileToPlayfield drawPosX, drawPosY, bgTable(ix, iy), attrTable(ix, iy).Pal, attrTable(ix, iy).Flip, NOWRAP
    NEXT ix
  NEXT iy

  ' In case we want to change where the text shows on the X axis easily

  textX = viewSizeTilesX + (viewStartPixelsX \ 8) + 1

  COLOR 15 ' All text in our sidebar will be white

  wPal = 1

  IF modeEdit = 1 THEN hWalkFrame = 0

  SELECT CASE Hero.Dir
    CASE SOUTH: ' facing south
      wFlip = 0
      IF hWalkFrame = 0 THEN
        fIndexHero = 512
      ELSE
        fIndexHero = 528
      END IF
    CASE WEST
      wFlip = 1
      IF hWalkFrame = 0 THEN
        fIndexHero = 516
      ELSE
        fIndexHero = 532
      END IF
    CASE EAST
      wFlip = 0
      IF hWalkFrame = 0 THEN
        fIndexHero = 516
      ELSE
        fIndexHero = 532
      END IF
    CASE NORTH
      fIndexHero = 520
      IF hWalkFrame = 0 THEN
        wFlip = 0 ' Same block but flipped horizontally
      ELSE
        wFlip = 1
      END IF
    CASE ELSE
      ESCAPE 95 ' Safety
  END SELECT

  wTable = 3

  ' Draw hero on screen
  drawObjFrameToPlayfieldAtPos Hero.ScreenX, Hero.ScreenY, wTable, fIndexHero, wPal, wFlip, NOWRAP

  ''''
  '' Draw objects ' object display '' Done manually for each object ' display objects

  FOR ii = 0 TO objCount - 1 ' Loop through objects
    ' Image display details are entered manually here

    ' Send the fairy index, not the golem index
    IF Obj(ii).Type = objBush THEN ' Stationary ' First variable is the object number
      drawObjFrameByObj ii, TABLE1, 97, wPal, NOFLIP, NOWRAP
    END IF

    IF Obj(ii).Type = objGrave THEN ' Stationary
      drawObjFrameByObj ii, TABLE1, 120, wPal, NOFLIP, NOWRAP
    END IF

  NEXT ii

  IF projCount > 0 THEN
    drawObjFrameToPlayfieldAtPos proj_PosX(0), proj_PosY(0), 3, 576, wPal, NOFLIP, NOWRAP
  END IF

  ' Display game info ''''''''''''
  COLOR 15 ' white
  ZLOCATE textX, 3: PRINT "TX:"; LTRIM$(STR$(Hero.hTrueX))
  ZLOCATE textX + 7, 3: PRINT "HA:"; cTrNum$(Hero.hTrueX AND 7)
  ZLOCATE textX, 4: PRINT "TY:"; LTRIM$(STR$(Hero.hTrueY))

  ZLOCATE textX + 7, 4
  PRINT "DIR:"; cTrNum$(Hero.Dir)

  COLOR 11 ' light blue
  ZLOCATE textX, 5: PRINT "MX:"; LTRIM$(STR$(mapPosX))
  COLOR 15 ' white
  ZLOCATE textX + 7, 5
  PRINT "SX:"; LTRIM$(STR$(Hero.ScreenX))
  COLOR 11 ' light blue
  ZLOCATE textX, 6: PRINT "MY:"; LTRIM$(STR$(mapPosY));
  COLOR 15 ' white
  ZLOCATE textX + 7, 6
  PRINT "SY:"; LTRIM$(STR$(Hero.ScreenY))

  ZLOCATE textX, 7
  PRINT "DX:"; cTrNum$(shRight(Hero.hTrueX, 4))

  ZLOCATE textX + 7, 7
  PRINT "DY:"; cTrNum$(shRight(Hero.hTrueY, 4))

  '  Inverted X and Y, for mirrored section

  ' In redrawAll, update the DX and DY display

  ZLOCATE textX, 8
  IF wMap = 0 THEN ' If the overworld map
    ' In the mirrored lower half, show the staggered X position
    RX = shRight(Hero.hTrueX, 4)
    RY = shRight(Hero.hTrueY, 4)
    RX = mapReturnRX(RX, RY, 0) ' Special function to return the correct position

    PRINT "RX:"; cTrNum$(RX)
  END IF

  ZLOCATE textX + 7, 8
  IF wMap = 0 THEN ' If the overworld map
    ' In the mirrored lower half, show the mirrored Y position
    RX = shRight(Hero.hTrueX, 4)
    RY = shRight(Hero.hTrueY, 4)
    RY = mapReturnRY(RX, RY, 0) ' Special function to return the correct position

    PRINT "RY:"; cTrNum$(RY)

  END IF

  ZLOCATE textX, 10 ' Lets us see what is stored in mapData where the player is
  PRINT "@:";
  PRINT map(shRight(Hero.hTrueX, 4), shRight(Hero.hTrueY, 4), 0)
  COLOR 15

  ZLOCATE textX + 7, 10
  PRINT "MSX:"; cTrNum$(mPosX)

  ZLOCATE textX + 15, 10
  PRINT "MSY:"; cTrNum$(mPosX)

  ''''
  ' Top right

  ZLOCATE textX, 1
  PRINT "Edit Mode:";
  IF modeEdit = 0 THEN
    PRINT "OFF"
  ELSE
    PRINT "ON"
  END IF

  ZLOCATE textX + 14, 1
  PRINT "#MAPS:"; numMaps

  ZLOCATE textX + 14, 2
  PRINT "MBPER:"; mapBytesPer

  ZLOCATE textX + 14, 3
  PRINT "MP#:"; mapNum

  ZLOCATE textX + 14, 4 ' EdgeBarrier
  PRINT "EB:";

  IF mapHeader(mapNum).EdgeBarrier = 0 THEN
    PRINT "N"
  ELSE
    PRINT "Y"
  END IF

  ZLOCATE textX + 14, 5
  PRINT "MWR: ";
  IF mapHeader(mapNum).MapWrap = 0 THEN
    PRINT "N"
  ELSE
    PRINT "Y"
  END IF

  ZLOCATE textX + 14, 6
  PRINT "MSZX: "; cTrNum(mapHeader(mapNum).MpSizeX)
  ZLOCATE textX + 14, 7
  PRINT "MSZY: ";
  IF mapNum = 0 THEN ' Map zero will be double size
    PRINT cTrNum(mapZeroOrigSizeY);
    IF mapZeroOrigSizeY < mapHeader(wMap).MpSizeY THEN PRINT "*"
  ELSE
    PRINT cTrNum(mapHeader(mapNum).MpSizeY)
  END IF

  ZLOCATE textX + 14, 8
  PRINT "LKM: "; cTrNum(mapHeader(mapNum).LinkedMap)

  ' Back to main strip, left

  ZLOCATE textX, 12
  PRINT "PRESS E TO"
  ZLOCATE textX, 13
  PRINT "MAKE WALLS"
  ZLOCATE textX, 14
  PRINT "SOLID AND"
  ZLOCATE textX, 15
  PRINT "D TO TOGGLE"
  ZLOCATE textX, 16
  PRINT "SHADOWS"

  ZLOCATE textX, 18
  PRINT "Shadows:";
  IF showShadows = 0 THEN
    PRINT "OFF"
  ELSE
    PRINT "ON"
  END IF

  ZLOCATE textX + 14, 12
  PRINT "COMPASS:";
  IF compassDir = SOUTH THEN
    PRINT "S"
  ELSE
    PRINT "N"
  END IF

  ZLOCATE textX + 14, 14
  PRINT "H TRIM:";
  IF trimHorizontal = 0 THEN
    PRINT "N"
  ELSE
    PRINT "Y"
  END IF

  ZLOCATE textX + 14, 15
  IF trimHorizontal = 0 THEN COLOR 7 ' Gray
  PRINT "SMZE:";
  IF StoreMapZeroEfficient = 0 THEN
    PRINT "N"
  ELSE
    PRINT "Y"
  END IF

  ZLOCATE textX + 14, 16
  IF trimHorizontal = 0 THEN COLOR 7 ' Gray
  PRINT "METR:"; mapEdgeTaperRows

  COLOR 15 ' In case it was just changed

  ZLOCATE textX + 14, 18 ' Show row offset for DY

  RX = shRight(Hero.hTrueX, 4)
  RY = shRight(Hero.hTrueY, 4)
  PRINT "ODX:"; mapOffsetX(RY)

  ZLOCATE textX + 14, 19 ' Show row offset for RY

  RX = shRight(Hero.hTrueX, 4)
  RY = shRight(Hero.hTrueY, 4)
  RY = mapReturnRY(RX, RY, 0) ' Special function to return the correct position
  PRINT "ORX:"; mapOffsetX(RY)

  ' Back to left column
  ZLOCATE textX, 20
  PRINT "ObjCount:" + cTrNum$(objCount)

  ZLOCATE textX, 21
  PRINT "ProjCount:" + cTrNum$(projCount)

  ZLOCATE textX, 23
  PRINT "TIMER:"; cornerMechanicClock

  ZLOCATE textX, 25: PRINT "HWRAP:";
  IF wrapHorizontal = 1 THEN
    PRINT "ON"
  ELSE
    PRINT "OFF"
  END IF

  ZLOCATE textX, 26: PRINT "VWRAP:";
  IF wrapVertical = 1 THEN
    PRINT "ON"
  ELSE
    PRINT "OFF"
  END IF

  ZLOCATE textX, 28: PRINT "OBJCOUNT:"; objCount
  ZLOCATE textX, 29: PRINT "OBJ TYPE 0:"; Obj(0).Type; Obj(0).TrueX; Obj(0).TrueY
  ZLOCATE textX, 30: PRINT "OBJ TYPE 1:"; Obj(1).Type; Obj(1).TrueX; Obj(1).TrueY
  ZLOCATE textX, 31: PRINT "OBJ TYPE 2:"; Obj(2).Type; Obj(2).TrueX; Obj(2).TrueY
  ZLOCATE textX, 32: PRINT "OBJ TYPE 3:"; Obj(3).Type; Obj(3).TrueX; Obj(3).TrueY

  ' I can't put this in processInput because I want to draw boxes and it automatically clears them, so process it here

  IF modePT > 0 THEN EXIT SUB ' Here on out non-modeEdit

  processMouseInputRedrawAll ' handle any input clicks when not in modePT


END SUB ' redrawAll

'''''''''''''''''''''''''''''''
FUNCTION retGolemTileNoTable (wGolem, wTile)
  ' 0 1
  ' 2 3
  SELECT CASE wTile
    CASE 0
      retGolemTileNoTable = fTable(gTable(wGolem).Index)
    CASE 1
      retGolemTileNoTable = fTable(gTable(wGolem).Index + 1)
    CASE 2
      retGolemTileNoTable = fTable(gTable(wGolem).Index + 2)
    CASE 3
      retGolemTileNoTable = fTable(gTable(wGolem).Index + 3)
    CASE ELSE
      ESCAPE 92 ' Safety
  END SELECT

END FUNCTION ' retGolemTileNoTable

'''''''''''''''''''''''''''''''
FUNCTION retGolemTilePlusTable (wGolem, wTile)
  ' 0 1
  ' 2 3
  SELECT CASE wTile
    CASE 0
      retGolemTilePlusTable = (gTable(wGolem).pTable * 256) + fTable(gTable(wGolem).Index)
    CASE 1
      retGolemTilePlusTable = (gTable(wGolem).pTable * 256) + fTable(gTable(wGolem).Index + 1)
    CASE 2
      retGolemTilePlusTable = (gTable(wGolem).pTable * 256) + fTable(gTable(wGolem).Index + 2)
    CASE 3
      retGolemTilePlusTable = (gTable(wGolem).pTable * 256) + fTable(gTable(wGolem).Index + 3)
    CASE ELSE
      ESCAPE 91 ' Safety
  END SELECT

END FUNCTION ' retGolemTilePlusTable

'''''''''''''''''''''''''''''''
SUB setBGTile (wPosX, wPosY, wTile, wPal, wFlip)

  bgTable(wPosX, wPosY) = wTile
  attrTable(wPosX, wPosY).Pal = wPal
  attrTable(wPosX, wPosY).Flip = wFlip

END SUB ' setBGTile

'''''''''''''''''''''''''''''''
SUB setGolemToBG (wPosX, wPosY, wGolem)

  ' Copy from the golem table (using the fairy table index) into the background table

  wPal = gTable(wGolem).Pal

  SELECT CASE gTable(wGolem).Flip
    CASE 0:
      ' Assign to the bg tile array the sub-tiles of the fairy table select square

      setBGTile wPosX, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index), wPal, 0
      setBGTile wPosX + 1, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 1), wPal, 0
      setBGTile wPosX, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 2), wPal, 0
      setBGTile wPosX + 1, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 3), wPal, 0

    CASE 1: ' Horizontal flip

      setBGTile wPosX, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 1), wPal, 1
      setBGTile wPosX + 1, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index), wPal, 1
      setBGTile wPosX, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 3), wPal, 1
      setBGTile wPosX + 1, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 2), wPal, 1

    CASE 2: ' Vertical flip
      setBGTile wPosX, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 2), wPal, 2
      setBGTile wPosX + 1, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 3), wPal, 2
      setBGTile wPosX, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index), wPal, 2
      setBGTile wPosX + 1, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 1), wPal, 2

    CASE 3: ' Horizontal and vertical flips
      ' Draw the sub-tiles of the fairy table select square
      setBGTile wPosX, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 3), wPal, 3
      setBGTile wPosX + 1, wPosY, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 2), wPal, 3
      setBGTile wPosX, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index + 1), wPal, 3
      setBGTile wPosX + 1, wPosY + 1, gTable(wGolem).pTable * 256 + fTable(gTable(wGolem).Index), wPal, 3

    CASE ELSE
      ESCAPE 90 ' Safety
  END SELECT

END SUB ' setGolemToBG

'''''''''''''''''''''''''''''''
SUB setHWalkFrame

  IF hWalkCounter > 4 AND hWalkCounter < 12 THEN
    hWalkFrame = 0
  ELSE
    hWalkFrame = 1 ' Active pose
  END IF

END SUB ' setWalkFrame

'''''''''''''''''''''''''''''''
SUB setPalettes256

  FOR ii = 1 TO 255
    _PALETTECOLOR ii, _RGB32(bmpPal256(ii, palRED), bmpPal256(ii, palGREEN), bmpPal256(ii, palBLUE))
  NEXT

END SUB ' setPalettes256

'''''''''''''''''''''''''''''''
SUB setTilePixelRow (wTile, starty, td$)

  ' Split wTile into X and Y positions for our 8 x 8 tiles
  offsetY = wTile \ 16 ' Shift right four bits
  offsetX = wTile AND 15 ' Drop high four bits

  FOR lp = 1 TO LEN(td$)
    wChar$ = MID$(td$, lp, 1)

    SELECT CASE wChar$ ' switch ' Check to see if a letter, if not it is assumed a number (bad letter will output zero)
      CASE "A": wClr = 10
      CASE "B": wClr = 11
      CASE "C": wClr = 12
      CASE "D": wClr = 13
      CASE "E": wClr = 14
      CASE "F": wClr = 15
      CASE ELSE: wClr = VAL(wChar$)

    END SELECT

    ' We now draw directly to bmpData by storing in the array

    bmpData((offsetX * 8) + lp - 1, (offsetY * 8) + starty) = wClr

  NEXT

END SUB ' setTilePixelRow

'''''''''''''''''''''''''''''''
FUNCTION shRight (wVal, wDistance) ' Binary shift right
  ' Often this function will be called with ,3 to adjust player coordinates
  ' to the 8x8 tile positions, since they hold true pixel positions

  SELECT CASE wDistance
    CASE 1
      shRight = wVal \ 2
      EXIT FUNCTION
    CASE 2:
      shRight = wVal \ 4
      EXIT FUNCTION
    CASE 3:
      shRight = wVal \ 8
      EXIT FUNCTION
    CASE 4:
      shRight = wVal \ 16
      EXIT FUNCTION
    CASE 5:
      shRight = wVal \ 32
      EXIT FUNCTION
    CASE 6:
      shRight = wVal \ 64
      EXIT FUNCTION
    CASE 7:
      shRight = wVal \ 128
      EXIT FUNCTION
    CASE 8:
      shRight = wVal \ 256
      EXIT FUNCTION

    CASE ELSE:
      shRight = wVal ' No change
      EXIT FUNCTION
  END SELECT

  shRight = wVal

END FUNCTION ' shRight

'''''''''''''''''''''''''''''''
SUB splitPos (wTrueX, wTrueY)
  ' This sub takes true map positions and splits them into map and screen positions
  ' We don't have to pass the variables, since true positions are global variables, but it is here for flexibility

  IF mapHeader(mapNum).MapWrap > 0 THEN ' Wrapping behavior

    ' The player never leaves the board, and we cannot have a negative position

    Hero.ScreenX = viewCenterPosX * 16 ' Center the player's apparent position always
    Hero.ScreenY = viewCenterPosY * 16

    ' X check, check to see if the player is so far to the left that the map scroll position wraps to near the end
    IF wTrueX < viewCenterPosX * 16 THEN ' If on the left side before the center point X

      mapPosX = (mapHeader(mapNum).MpSizeX * 16) - ((viewCenterPosX * 16) - wTrueX) ' Wrap around calculation
    ELSE
      mapPosX = wTrueX - (viewCenterPosX * 16) ' Player is in the center or right side.
    END IF

    ' Y check, see if the player is so far up that the map scroll position wraps to near the end
    IF wTrueY < viewCenterPosY * 16 THEN ' If on the left side before the center point X
      mapPosY = (mapHeader(mapNum).MpSizeY * 16) - ((viewCenterPosY * 16) - wTrueY) ' Wrap around calculation
    ELSE
      mapPosY = wTrueY - (viewCenterPosY * 16) ' Player is in the center or bottom side.
    END IF

  ELSE ' MapWrap = 0

    IF wTrueX < viewCenterPosX * 16 THEN ' If on the left side before the center point X
      mapPosX = 0 ' Then we know we don't need an offset
      Hero.ScreenX = wTrueX
    ELSE ' If it is at center or near the right edge of the map

      IF wTrueX >= (mapHeader(mapNum).MpSizeX - (viewSizeBlocksX - viewCenterPosX)) * 16 THEN ' Near the right edge of the map
        mapPosX = (mapHeader(mapNum).MpSizeX * 16) - (viewSizeBlocksX * 16) ' Move the map offset as far as possible right
        Hero.ScreenX = wTrueX - mapPosX
      ELSE ' If in the center area
        mapPosX = wTrueX - (viewCenterPosX * 16)
        Hero.ScreenX = viewCenterPosX * 16
      END IF
    END IF ' x check

    '' Y section for non-wrap:
    IF wTrueY < viewCenterPosY * 16 THEN
      mapPosY = 0 ' Then we know we don't need an offset
      Hero.ScreenY = wTrueY
    ELSE ' If it is at center or near the top edge of the map
      IF wTrueY >= (mapHeader(mapNum).MpSizeY - (viewSizeBlocksY - viewCenterPosY)) * 16 THEN ' Near the bottom edge of the map
        mapPosY = (mapHeader(mapNum).MpSizeY * 16) - (viewSizeBlocksY * 16) ' Move the map offset as far as possible down
        Hero.ScreenY = wTrueY - mapPosY
      ELSE ' If in the center area
        mapPosY = wTrueY - (viewCenterPosY * 16)
        Hero.ScreenY = viewCenterPosY * 16
      END IF
    END IF ' y check

  END IF ' mapNum

END SUB ' splitPos

'''''''''''''''''''''''''''''''
SUB waitKeyRelease (wKey$)

  _DISPLAY ' _DISPLAY can mess with visual output not shown by it, so this goes here to
  '''''''''' get caught after L and S are used so the printed text stays on the screen
  DO
  LOOP UNTIL keyCheck(wKey$) = 0

END SUB ' waitKeyRelease

'''''''''''''''''''''''''''''''
FUNCTION wrapAdd (wStartVal AS INTEGER, wrapPoint AS INTEGER, wAmount AS INTEGER)
  ' Adds wAmount, and wraps back around at wrapPoint if it exceeds the limit.
  newVal = wStartVal + wAmount ' Pass to a new variable to avoid changing the original, which is passed by reference

  IF newVal >= wrapPoint THEN
    newVal = newVal - wrapPoint

  END IF
  wrapAdd = newVal

END FUNCTION ' wrapAdd

'''''''''''''''''''''''''''''''
FUNCTION wrapAt (wStartVal AS INTEGER, wrapPoint AS INTEGER) ' Wrap to wrapPoint
  ' If below zero, or above wrapPoint, wrap to within that range

  newVal = wStartVal ' Pass to a new variable to avoid changing the original, which is passed by reference

  DO WHILE newVal < 0
    newVal = newVal + wrapPoint
  LOOP

  DO WHILE newVal >= wrapPoint
    newVal = newVal - wrapPoint
  LOOP

  wrapAt = newVal

END FUNCTION ' wrapAt

'''''''''''''''''''''''''''''''
FUNCTION wrapRange (wStartVal AS INTEGER, wrapPointLow AS INTEGER, wrapPointHigh AS INTEGER)
  ' Wraps wStartVal to the range [wrapPointLow, wrapPointHigh - 1]
  ' Returns the wrapped value via the function name

  DIM newVal AS INTEGER
  DIM rangeSize AS INTEGER

  newVal = wStartVal ' Copy to avoid modifying the original (passed by reference)
  rangeSize = wrapPointHigh - wrapPointLow ' Size of the wrapping range

  IF rangeSize <= 0 THEN ' Prevent invalid range
    wrapRange = wrapPointLow ' Default to lower bound if range is invalid
    EXIT FUNCTION
  END IF

  ' Adjust newVal to fall within [wrapPointLow, wrapPointHigh - 1]
  DO WHILE newVal < wrapPointLow
    newVal = newVal + rangeSize
  LOOP

  DO WHILE newVal >= wrapPointHigh
    newVal = newVal - rangeSize
  LOOP

  wrapRange = newVal ' Return via function name

END FUNCTION ' wrapRange

'''''''''''''''''''''''''''''''
FUNCTION wrapSubtract (wStartVal AS INTEGER, wrapPoint AS INTEGER, wAmount AS INTEGER)
  ' Subtracts wAmount, and back around at a custom point if it goes below zero
  newVal = wStartVal - wAmount ' Pass to a new variable to avoid changing the original, which is passed by reference

  DO WHILE newVal < 0
    newVal = newVal + wrapPoint
  LOOP

  wrapSubtract = newVal

END FUNCTION ' wrapSubtract

'''''''''''''''''''''''''''''''
SUB ZLOCATE (wx, wy)
  LOCATE wy + 1, wx + 1
END SUB ' ZLOCATE

'''''''''''''''''''''''''''''''
SUB ESCAPE (code)
  COLOR 15
  ZLOCATE 10, 5: PRINT "                              "
  ZLOCATE 10, 6: PRINT "ENDED WITH CODE:"; code; "    "
  ZLOCATE 10, 7: PRINT "                             "

  END ' SYSTEM also works
END SUB ' ESCAPE

'''''''''''''''''''''''''''''''
SUB ESCAPE2 (code1, code2)
  COLOR 15
  ZLOCATE 10, 5: PRINT "                              "
  ZLOCATE 10, 6: PRINT "ENDED WITH CODES:"; code1; ","; code2; "    "
  ZLOCATE 10, 7: PRINT "                             "

  END ' SYSTEM also works
END SUB ' ESCAPE2

'''''''''''''''''''''''''''''''
SUB ESCAPETEXT (wStr$)

  COLOR 15

  xPos = 1
  yPos = 5

  IF wStr$ = "" THEN wStr$ = "No text sent"

  ZLOCATE xPos, yPos: PRINT "                 "
  ZLOCATE xPos, yPos + 2: PRINT "                 " ' Print before text in case text wraps
  ZLOCATE xPos, yPos + 1: PRINT "  "; wStr$; " "

  END ' SYSTEM
END SUB
