module Arkham.Types.Location.Cards.CloverClubLounge
  ( cloverClubLounge
  , CloverClubLounge(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (cloverClubLounge)
import Arkham.Types.Ability
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.Trait

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

instance HasAbilities env CloverClubLounge where
  getAbilities iid window (CloverClubLounge attrs) =
    withBaseAbilities iid window attrs $ pure
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
