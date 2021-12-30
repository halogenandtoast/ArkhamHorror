module Arkham.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (cloverClubLounge)
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Attrs
import Arkham.Message
import Arkham.Trait

newtype CloverClubLounge = CloverClubLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubLounge :: LocationCard CloverClubLounge
cloverClubLounge = location
  CloverClubLounge
  Cards.cloverClubLounge
  2
  (Static 0)
  Circle
  [Moon, Square, Triangle]

instance HasAbilities CloverClubLounge where
  getAbilities (CloverClubLounge attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
            attrs
            1
            (OnAct 1)
            (ActionAbility Nothing
            $ Costs
                [ ActionCost 1
                , HandDiscardCost 1 (Just AssetType) (singleton Ally) mempty
                ]
            )
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (GainClues iid 2)
    _ -> CloverClubLounge <$> runMessage msg attrs
