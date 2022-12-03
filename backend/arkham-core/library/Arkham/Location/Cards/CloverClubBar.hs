module Arkham.Location.Cards.CloverClubBar
  ( cloverClubBar
  , CloverClubBar(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( cloverClubBar )
import Arkham.Location.Runner
import Arkham.Message
import Arkham.ScenarioLogKey

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBar :: LocationCard CloverClubBar
cloverClubBar = location CloverClubBar Cards.cloverClubBar 3 (Static 0)

instance HasAbilities CloverClubBar where
  getAbilities (CloverClubBar attrs) = withBaseAbilities
    attrs
    [ limitedAbility (PlayerLimit PerGame 1)
      $ restrictedAbility attrs 1 (OnAct 1 <> Here)
      $ ActionAbility Nothing
      $ Costs [ActionCost 1, ResourceCost 2]
    | locationRevealed attrs
    ]

instance RunMessage CloverClubBar where
  runMessage msg l@(CloverClubBar attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      drawing <- drawCards iid attrs 2
      pushAll [GainClues iid 2, drawing, Remember $ HadADrink iid]
      pure l
    _ -> CloverClubBar <$> runMessage msg attrs
