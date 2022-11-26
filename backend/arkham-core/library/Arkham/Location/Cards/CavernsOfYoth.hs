module Arkham.Location.Cards.CavernsOfYoth
  ( cavernsOfYoth
  , CavernsOfYoth(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.TheDepthsOfYoth.Helpers
import Arkham.Timing qualified as Timing

newtype CavernsOfYoth = CavernsOfYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cavernsOfYoth :: LocationCard CavernsOfYoth
cavernsOfYoth =
  symbolLabel $ location CavernsOfYoth Cards.cavernsOfYoth 1 (PerPlayer 1)

instance HasAbilities CavernsOfYoth where
  getAbilities (CavernsOfYoth a) =
    withBaseAbilities a
      $ [ mkAbility a 1
          $ ForcedAbility
          $ PutLocationIntoPlay Timing.After Anyone
          $ LocationWithId
          $ toId a
        ]

instance RunMessage CavernsOfYoth where
  runMessage msg l@(CavernsOfYoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      n <- getCurrentDepth
      push $ PlaceClues (toTarget attrs) n
      pure l
    _ -> CavernsOfYoth <$> runMessage msg attrs
