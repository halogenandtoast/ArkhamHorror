module Arkham.Scenarios.CityOfTheElderThings.Helpers where

import Arkham.Campaigns.EdgeOfTheEarth.Helpers
import Arkham.I18n
import Arkham.Location.Grid
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "cityOfTheElderThings" a

{- FOURMOLU_DISABLE -}
setup1Positions :: [Pos]
setup1Positions =
  [ Pos (-1) (-2), Pos 0 (-2)  -- top row
  , Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1), Pos 2 (-1)  -- second row
  , Pos (-2) 0, Pos (-1) 0, Pos 1 0, Pos 2 0 -- middle
  , Pos (-2) 1, Pos (-1) 1, Pos 0 1, Pos 1 1 -- fourth row
  , Pos 0 2, Pos 1 2  -- bottom row
  ]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
setup2Positions :: [Pos]
setup2Positions =
  [ Pos (-1) (-1), Pos 0 (-1), Pos 1 (-1)
  , Pos (-2) (-2), Pos (-1) (-2), Pos 0 (-2), Pos 1 (-2), Pos 2 (-2)
  , Pos (-3) (-3), Pos (-2) (-3), Pos 2 (-3), Pos 3 (-3)
  , Pos (-4) (-4), Pos (-3) (-4), Pos 3 (-4), Pos 4 (-4)
  ]
{- FOURMOLU_ENABLE -}

{- FOURMOLU_DISABLE -}
setup3Positions :: [Pos]
setup3Positions =
  [ Pos (-7) 4, Pos (-6) 4
  , Pos (-7) 3, Pos (-6) 3, Pos (-5) 3, Pos (-4) 3
  , Pos (-5) 2, Pos (-4) 2, Pos (-3) 2, Pos (-2) 2
  , Pos (-4) 1, Pos (-3) 1, Pos (-2) 1, Pos (-1) 1
  , Pos (-2) 0, Pos (-1) 0
  ]
{- FOURMOLU_ENABLE -}
