module Arkham.Location.CardDefs.ReturnToThePathToCarcosa.ReturnToDimCarcosa where

import Arkham.Location.CardDefs.Import

recessesOfYourOwnMind :: CardDef
recessesOfYourOwnMind = storyOnBack $ location "52061" "Recesses of Your Own Mind" [] Heart [Star] ReturnToDimCarcosa

returnToPalaceOfTheKing :: CardDef
returnToPalaceOfTheKing =
  storyOnBack
    $ location
      "52060"
      ("Palace of the King" <:> "Hastur's Domain")
      [Otherworld]
      Star
      [Triangle, Diamond, Heart, Droplet, Hourglass]
      ReturnToDimCarcosa

stageOfTheWardTheatre :: CardDef
stageOfTheWardTheatre = storyOnBack $ location "52063" "Stage of the Ward Theatre" [] Hourglass [Star] ReturnToDimCarcosa

theThroneRoom :: CardDef
theThroneRoom = storyOnBack $ location "52062" "The Throne Room" [] Droplet [Star] ReturnToDimCarcosa
