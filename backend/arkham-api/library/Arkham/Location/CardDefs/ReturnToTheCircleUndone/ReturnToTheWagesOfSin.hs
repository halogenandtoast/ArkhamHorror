module Arkham.Location.CardDefs.ReturnToTheCircleUndone.ReturnToTheWagesOfSin where

import Arkham.Location.CardDefs.Import

returnToHangmansBrook :: CardDef
returnToHangmansBrook =
  otherSideIs "54037b"
    $ location "54037" "Hangman's Brook" mempty Squiggle [Circle, Plus] ReturnToTheWagesOfSin

returnToHangmansBrookSpectral :: CardDef
returnToHangmansBrookSpectral =
  otherSideIs "54037"
    $ location
      "54037b"
      "Hangman's Brook"
      [Spectral]
      Squiggle
      [Circle, Plus]
      ReturnToTheWagesOfSin
