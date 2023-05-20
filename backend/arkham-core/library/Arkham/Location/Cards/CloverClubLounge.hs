module Arkham.Location.Cards.CloverClubLounge (
  cloverClubLounge,
  CloverClubLounge (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (cloverClubLounge)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype CloverClubLounge = CloverClubLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubLounge :: LocationCard CloverClubLounge
cloverClubLounge =
  location CloverClubLounge Cards.cloverClubLounge 2 (Static 0)

instance HasAbilities CloverClubLounge where
  getAbilities (CloverClubLounge attrs) =
    withBaseAbilities attrs $
      [ limitedAbility (PlayerLimit PerGame 1) $
        restrictedAbility attrs 1 (OnAct 1) $
          ActionAbility Nothing $
            Costs
              [ ActionCost 1
              , HandDiscardCost 1 $ CardWithType AssetType <> CardWithTrait Ally
              ]
      | locationRevealed attrs
      ]

instance RunMessage CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          l <$ push (GainClues iid (toAbilitySource attrs 1) 2)
    _ -> CloverClubLounge <$> runMessage msg attrs
