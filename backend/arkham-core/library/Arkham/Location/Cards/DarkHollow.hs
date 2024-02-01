module Arkham.Location.Cards.DarkHollow (
  darkHollow,
  DarkHollow (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype DarkHollow = DarkHollow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

darkHollow :: LocationCard DarkHollow
darkHollow = location DarkHollow Cards.darkHollow 3 (PerPlayer 1)

instance HasAbilities DarkHollow where
  getAbilities (DarkHollow attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ PutLocationIntoPlay Timing.After Anyone
          $ LocationWithId
          $ toId attrs
      ]

instance RunMessage DarkHollow where
  runMessage msg l@(DarkHollow attrs) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ _ -> do
      anyHasMap <- getAnyHasSupply Map
      unless anyHasMap $ do
        n <- perPlayer 1
        push $ PlaceClues (toSource attrs) (toTarget attrs) n
      pure l
    _ -> DarkHollow <$> runMessage msg attrs
