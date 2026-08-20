module Arkham.Location.CardDefs.TheScarletKeys.WithoutATrace where

import Arkham.Keyword qualified as Keyword
import Arkham.Location.CardDefs.Import

courtOfTheOutsiders :: CardDef
courtOfTheOutsiders =
  location_
    "09688"
    "Court of the Outsiders"
    [Otherworld]
    WithoutATrace

outsidersLairWithoutATrace :: CardDef
outsidersLairWithoutATrace =
  victory 1
    $ ( locationWithUnrevealed_
          "09689"
          "City of Remnants"
          [Otherworld]
          "Outsiders' Lair"
          [Otherworld]
          WithoutATrace
      )
      { cdKeywords = singleton (Keyword.Concealed CityOfRemnants (Static 1))
      }
