module Arkham.Treachery.CardDefs.TheDrownedCity.ElderMist where

import Arkham.Treachery.CardDefs.Import

corrosiveFog :: CardDef
corrosiveFog =
  (treachery "11732" "Corrosive Fog" ElderMist 2) {cdCardTraits = setFromList [Hazard]}

elderMist :: CardDef
elderMist =
  (treachery "11731" "Elder Mist" ElderMist 2) {cdCardTraits = setFromList [Hazard]}
