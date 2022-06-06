module Arkham.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import qualified Arkham.Location.Cards as Cards (cloverClubLounge)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Trait

newtype CloverClubLounge = CloverClubLounge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
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
    withBaseAbilities attrs
      $ [ restrictedAbility
              attrs
              1
              (OnAct 1)
              (ActionAbility Nothing $ Costs
                [ ActionCost 1
                , HandDiscardCost 1
                $ CardWithType AssetType
                <> CardWithTrait Ally
                ]
              )
            & (abilityLimitL .~ PlayerLimit PerGame 1)
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage CloverClubLounge where
  runMessage msg l@(CloverClubLounge attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (GainClues iid 2)
    _ -> CloverClubLounge <$> runMessage msg attrs
