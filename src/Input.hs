module Input (getInput, updateKeyboard) where

  import qualified Graphics.Gloss.Interface.Pure.Game as G
  
  import Types
  
  keyboard = Key{}

  getInput :: SF (Event Input) GameInput
  getInput = arr $ \event ->
    case event of
      Event (G.EventKey (G.SpecialKey G.KeyF1) G.Up _ _) -> event `tag` keyboard{keyF1 =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyF2) G.Up _ _) -> event `tag` keyboard{keyF2 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF3) G.Up _ _) -> event `tag` keyboard{keyF2 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF4) G.Up _ _) -> event `tag` keyboard{keyF4 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF5) G.Up _ _) -> event `tag` keyboard{keyF5 =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyF6) G.Up _ _) -> event `tag` keyboard{keyF6 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF7) G.Up _ _) -> event `tag` keyboard{keyF7 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF8) G.Up _ _) -> event `tag` keyboard{keyF8 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF9) G.Up _ _) -> event `tag` keyboard{keyF9 =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyF10) G.Up _ _) -> event `tag` keyboard{keyF10 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF11) G.Up _ _) -> event `tag` keyboard{keyF11 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyF12) G.Up _ _) -> event `tag` keyboard{keyF12 =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyUp) G.Up _ _) -> event `tag` keyboard{keyUP =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyDown) G.Up _ _) -> event `tag` keyboard{keyDOWN =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Up _ _) -> event `tag` keyboard{keyLEFT =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyRight) G.Up _ _) -> event `tag` keyboard{keyRIGHT =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyTab) G.Up _ _) -> event `tag` keyboard{keyTAB =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyEsc) G.Up _ _) -> event `tag` keyboard{keyESC =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyHome) G.Up _ _) -> event `tag` keyboard{keyHOME =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyPageUp) G.Up _ _) -> event `tag` keyboard{keyRIGHT =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyPageDown) G.Up _ _) -> event `tag` keyboard{keyPGUP =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyInsert) G.Up _ _) -> event `tag` keyboard{keyPGDOWN =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyDelete) G.Up _ _) -> event `tag` keyboard{keyDELETE =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyEnter) G.Up _ _) -> event `tag` keyboard{keyENTER =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyBackspace) G.Up _ _) -> event `tag` keyboard{keyBACKSPACE =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyCtrlL) G.Up _ _) -> event `tag` keyboard{keyLCTRL =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyCtrlR) G.Up _ _) -> event `tag` keyboard{keyRCTRL =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyAltL) G.Up _ _) -> event `tag` keyboard{keyLALT =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyAltR) G.Up _ _) -> event `tag` keyboard{keyRALT =  KeyUp} 
      Event (G.EventKey (G.SpecialKey G.KeyShiftL) G.Up _ _) -> event `tag` keyboard{keyLSHIFT =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeyShiftR) G.Up _ _) -> event `tag` keyboard{keyRSHIFT =  KeyUp}
      Event (G.EventKey (G.SpecialKey G.KeySpace) G.Up _ _) -> event `tag` keyboard{keySPACE =  KeyUp}
      Event (G.EventKey (G.Char 'A') G.Up _ _) -> event `tag` keyboard{keyA =  KeyUp}
      Event (G.EventKey (G.Char 'B') G.Up _ _) -> event `tag` keyboard{keyB =  KeyUp}
      Event (G.EventKey (G.Char 'C') G.Up _ _) -> event `tag` keyboard{keyC =  KeyUp}
      Event (G.EventKey (G.Char 'D') G.Up _ _) -> event `tag` keyboard{keyD =  KeyUp}
      Event (G.EventKey (G.Char 'E') G.Up _ _) -> event `tag` keyboard{keyE =  KeyUp}
      Event (G.EventKey (G.Char 'F') G.Up _ _) -> event `tag` keyboard{keyF =  KeyUp}
      Event (G.EventKey (G.Char 'G') G.Up _ _) -> event `tag` keyboard{keyG =  KeyUp}
      Event (G.EventKey (G.Char 'H') G.Up _ _) -> event `tag` keyboard{keyH =  KeyUp}
      Event (G.EventKey (G.Char 'I') G.Up _ _) -> event `tag` keyboard{keyI =  KeyUp}
      Event (G.EventKey (G.Char 'J') G.Up _ _) -> event `tag` keyboard{keyJ =  KeyUp}
      Event (G.EventKey (G.Char 'K') G.Up _ _) -> event `tag` keyboard{keyK =  KeyUp}
      Event (G.EventKey (G.Char 'L') G.Up _ _) -> event `tag` keyboard{keyL =  KeyUp}
      Event (G.EventKey (G.Char 'M') G.Up _ _) -> event `tag` keyboard{keyM =  KeyUp}
      Event (G.EventKey (G.Char 'N') G.Up _ _) -> event `tag` keyboard{keyN =  KeyUp}
      Event (G.EventKey (G.Char 'O') G.Up _ _) -> event `tag` keyboard{keyO =  KeyUp}
      Event (G.EventKey (G.Char 'P') G.Up _ _) -> event `tag` keyboard{keyP =  KeyUp}
      Event (G.EventKey (G.Char 'Q') G.Up _ _) -> event `tag` keyboard{keyQ =  KeyUp}
      Event (G.EventKey (G.Char 'R') G.Up _ _) -> event `tag` keyboard{keyR =  KeyUp}
      Event (G.EventKey (G.Char 'S') G.Up _ _) -> event `tag` keyboard{keyS =  KeyUp}
      Event (G.EventKey (G.Char 'T') G.Up _ _) -> event `tag` keyboard{keyT =  KeyUp}
      Event (G.EventKey (G.Char 'U') G.Up _ _) -> event `tag` keyboard{keyU =  KeyUp}
      Event (G.EventKey (G.Char 'V') G.Up _ _) -> event `tag` keyboard{keyV =  KeyUp}
      Event (G.EventKey (G.Char 'W') G.Up _ _) -> event `tag` keyboard{keyW =  KeyUp}
      Event (G.EventKey (G.Char 'X') G.Up _ _) -> event `tag` keyboard{keyX =  KeyUp}
      Event (G.EventKey (G.Char 'Y') G.Up _ _) -> event `tag` keyboard{keyY =  KeyUp}
      Event (G.EventKey (G.Char 'Z') G.Up _ _) -> event `tag` keyboard{keyZ =  KeyUp}

      Event (G.EventKey (G.SpecialKey G.KeyF1) G.Down _ _) -> event `tag` keyboard{keyF1 = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyF2) G.Down _ _) -> event `tag` keyboard{keyF2 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF3) G.Down _ _) -> event `tag` keyboard{keyF2 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF4) G.Down _ _) -> event `tag` keyboard{keyF4 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF5) G.Down _ _) -> event `tag` keyboard{keyF5 = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyF6) G.Down _ _) -> event `tag` keyboard{keyF6 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF7) G.Down _ _) -> event `tag` keyboard{keyF7 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF8) G.Down _ _) -> event `tag` keyboard{keyF8 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF9) G.Down _ _) -> event `tag` keyboard{keyF9 = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyF10) G.Down _ _) -> event `tag` keyboard{keyF10 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF11) G.Down _ _) -> event `tag` keyboard{keyF11 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyF12) G.Down _ _) -> event `tag` keyboard{keyF12 = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyUp) G.Down _ _) -> event `tag` keyboard{keyUP = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyDown) G.Down _ _) -> event `tag` keyboard{keyDOWN = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyLeft) G.Down _ _) -> event `tag` keyboard{keyLEFT = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyRight) G.Down _ _) -> event `tag` keyboard{keyRIGHT = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyTab) G.Down _ _) -> event `tag` keyboard{keyTAB = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyEsc) G.Down _ _) -> event `tag` keyboard{keyESC = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyHome) G.Down _ _) -> event `tag` keyboard{keyHOME = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyPageUp) G.Down _ _) -> event `tag` keyboard{keyRIGHT = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyPageDown) G.Down _ _) -> event `tag` keyboard{keyPGUP = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyInsert) G.Down _ _) -> event `tag` keyboard{keyPGDOWN = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyDelete) G.Down _ _) -> event `tag` keyboard{keyDELETE = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyEnter) G.Down _ _) -> event `tag` keyboard{keyENTER = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyBackspace) G.Down _ _) -> event `tag` keyboard{keyBACKSPACE = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyCtrlL) G.Down _ _) -> event `tag` keyboard{keyLCTRL = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyCtrlR) G.Down _ _) -> event `tag` keyboard{keyRCTRL = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyAltL) G.Down _ _) -> event `tag` keyboard{keyLALT = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyAltR) G.Down _ _) -> event `tag` keyboard{keyRALT = KeyDown} 
      Event (G.EventKey (G.SpecialKey G.KeyShiftL) G.Down _ _) -> event `tag` keyboard{keyLSHIFT = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeyShiftR) G.Down _ _) -> event `tag` keyboard{keyRSHIFT = KeyDown}
      Event (G.EventKey (G.SpecialKey G.KeySpace) G.Down _ _) -> event `tag` keyboard{keySPACE = KeyDown}

      Event (G.EventKey (G.Char 'A') G.Down _ _) -> event `tag` keyboard{keyA = KeyDown}
      Event (G.EventKey (G.Char 'B') G.Down _ _) -> event `tag` keyboard{keyB = KeyDown}
      Event (G.EventKey (G.Char 'C') G.Down _ _) -> event `tag` keyboard{keyC = KeyDown}
      Event (G.EventKey (G.Char 'D') G.Down _ _) -> event `tag` keyboard{keyD = KeyDown}
      Event (G.EventKey (G.Char 'E') G.Down _ _) -> event `tag` keyboard{keyE = KeyDown}
      Event (G.EventKey (G.Char 'F') G.Down _ _) -> event `tag` keyboard{keyF = KeyDown}
      Event (G.EventKey (G.Char 'G') G.Down _ _) -> event `tag` keyboard{keyG = KeyDown}
      Event (G.EventKey (G.Char 'H') G.Down _ _) -> event `tag` keyboard{keyH = KeyDown}
      Event (G.EventKey (G.Char 'I') G.Down _ _) -> event `tag` keyboard{keyI = KeyDown}
      Event (G.EventKey (G.Char 'J') G.Down _ _) -> event `tag` keyboard{keyJ = KeyDown}
      Event (G.EventKey (G.Char 'K') G.Down _ _) -> event `tag` keyboard{keyK = KeyDown}
      Event (G.EventKey (G.Char 'L') G.Down _ _) -> event `tag` keyboard{keyL = KeyDown}
      Event (G.EventKey (G.Char 'M') G.Down _ _) -> event `tag` keyboard{keyM = KeyDown}
      Event (G.EventKey (G.Char 'N') G.Down _ _) -> event `tag` keyboard{keyN = KeyDown}
      Event (G.EventKey (G.Char 'O') G.Down _ _) -> event `tag` keyboard{keyO = KeyDown}
      Event (G.EventKey (G.Char 'P') G.Down _ _) -> event `tag` keyboard{keyP = KeyDown}
      Event (G.EventKey (G.Char 'Q') G.Down _ _) -> event `tag` keyboard{keyQ = KeyDown}
      Event (G.EventKey (G.Char 'R') G.Down _ _) -> event `tag` keyboard{keyR = KeyDown}
      Event (G.EventKey (G.Char 'S') G.Down _ _) -> event `tag` keyboard{keyS = KeyDown}
      Event (G.EventKey (G.Char 'T') G.Down _ _) -> event `tag` keyboard{keyT = KeyDown}
      Event (G.EventKey (G.Char 'U') G.Down _ _) -> event `tag` keyboard{keyU = KeyDown}
      Event (G.EventKey (G.Char 'V') G.Down _ _) -> event `tag` keyboard{keyV = KeyDown}
      Event (G.EventKey (G.Char 'W') G.Down _ _) -> event `tag` keyboard{keyW = KeyDown}
      Event (G.EventKey (G.Char 'X') G.Down _ _) -> event `tag` keyboard{keyX = KeyDown}
      Event (G.EventKey (G.Char 'Y') G.Down _ _) -> event `tag` keyboard{keyY = KeyDown}
      Event (G.EventKey (G.Char 'Z') G.Down _ _) -> event `tag` keyboard{keyZ = KeyDown}

  updateState:: KeyState -> KeyState
  updateState KeyDown = IsPressed
  updateState KeyUp   = IsReleased
  updateState s = s

  updateKeyboard :: SF GameInput GameInput
  updateKeyboard = arr $ \key -> 
    key `tag` keyboard
               {
                 keyF1 = updateState (keyF1 keyboard)
               , keyF2 = updateState (keyF2 keyboard)
               , keyF3 = updateState (keyF3 keyboard)
               , keyF4 = updateState (keyF4 keyboard)
               , keyF5 = updateState (keyF5 keyboard)
               , keyF6 = updateState (keyF6 keyboard)
               , keyF7 = updateState (keyF7 keyboard)
               , keyF8 = updateState (keyF8 keyboard)
               , keyF9 = updateState (keyF9 keyboard)
               , keyF10 = updateState (keyF10 keyboard)
               , keyF11 = updateState (keyF11 keyboard)
               , keyF12 = updateState (keyF12 keyboard)
               , keyPAD0 = updateState (keyPAD0 keyboard)
               , keyPAD1 = updateState (keyPAD1 keyboard)
               , keyPAD2 = updateState (keyPAD2 keyboard)
               , keyPAD3 = updateState (keyPAD3 keyboard)
               , keyPAD4 = updateState (keyPAD4 keyboard)
               , keyPAD5 = updateState (keyPAD5 keyboard)
               , keyPAD6 = updateState (keyPAD6 keyboard)
               , keyPAD7 = updateState (keyPAD7 keyboard)
               , keyPAD8 = updateState (keyPAD8 keyboard)
               , keyPAD9 = updateState (keyPAD9 keyboard)
               , keyMINUS = updateState (keyMINUS keyboard)
               , keyPLUS = updateState (keyPLUS keyboard)
               , keyTAB  = updateState (keyTAB keyboard)
               , keyESC = updateState (keyESC keyboard)
               , keyHOME = updateState (keyHOME keyboard)
               , keyPGUP = updateState (keyPGUP keyboard)
               , keyPGDOWN = updateState (keyPGDOWN keyboard)
               , keyINSERT = updateState (keyINSERT keyboard)
               , keyDELETE = updateState (keyDELETE keyboard)
               , keyENTER = updateState (keyENTER keyboard)
               , keyBACKSPACE = updateState (keyBACKSPACE keyboard)
               , keyLCTRL = updateState (keyLCTRL keyboard)
               , keyRCTRL = updateState (keyRCTRL keyboard)
               , keyLALT = updateState (keyLALT keyboard)
               , keyRALT = updateState (keyRALT keyboard)
               , keyLSHIFT = updateState (keyLSHIFT keyboard)
               , keyRSHIFT = updateState (keyRSHIFT keyboard)
               , keySPACE = updateState (keySPACE keyboard)
               , keyUP = updateState (keyUP keyboard)
               , keyDOWN = updateState (keyDOWN keyboard)
               , keyLEFT = updateState (keyLEFT keyboard)
               , keyRIGHT = updateState (keyRIGHT keyboard)
               , keyA = updateState (keyA keyboard)
               , keyB = updateState (keyB keyboard)
               , keyC = updateState (keyC keyboard)
               , keyD = updateState (keyD keyboard)
               , keyE = updateState (keyE keyboard)
               , keyF = updateState (keyF keyboard)
               , keyG = updateState (keyG keyboard)
               , keyH = updateState (keyH keyboard)
               , keyI = updateState (keyI keyboard)
               , keyJ = updateState (keyJ keyboard)
               , keyK = updateState (keyK keyboard)
               , keyL = updateState (keyL keyboard)
               , keyM = updateState (keyM keyboard)
               , keyN = updateState (keyN keyboard)
               , keyO = updateState (keyO keyboard)
               , keyP = updateState (keyP keyboard)
               , keyQ = updateState (keyQ keyboard)
               , keyR = updateState (keyR keyboard)
               , keyS = updateState (keyS keyboard)
               , keyT = updateState (keyT keyboard)
               , keyU = updateState (keyU keyboard)
               , keyV = updateState (keyV keyboard)
               , keyW = updateState (keyW keyboard)
               , keyX = updateState (keyX keyboard)
               , keyY = updateState (keyY keyboard)
               , keyZ = updateState (keyZ keyboard)
               , keyLMOUSE = updateState (keyLMOUSE keyboard)
               , keyRMOUSE = updateState (keyRMOUSE keyboard)
               , keyMIDMOUSE = updateState (keyMIDMOUSE keyboard)
               , keyWHEELUP = updateState (keyWHEELUP keyboard)
               , keyWHEELDOWN = updateState (keyWHEELDOWN keyboard)
               }