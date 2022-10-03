module Arkham.Location.Cards.GareDOrsay
  ( gareDOrsay
  , GareDOrsay(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher hiding ( MoveAction )
import Arkham.Message
import Arkham.Target
import Arkham.Trait

newtype GareDOrsay = GareDOrsay LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gareDOrsay :: LocationCard GareDOrsay
gareDOrsay = location GareDOrsay Cards.gareDOrsay 4 (PerPlayer 1)

instance HasAbilities GareDOrsay where
  getAbilities (GareDOrsay attrs) = withBaseAbilities
    attrs
    [restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)]

instance RunMessage GareDOrsay where
  runMessage msg l@(GareDOrsay attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      rails <- filter (/= toId attrs) <$> selectList (LocationWithTrait Rail)
      push $ chooseOne
        iid
        [ TargetLabel (LocationTarget lid) [MoveAction iid lid Free False]
        | lid <- rails
        ]
      pure l
    _ -> GareDOrsay <$> runMessage msg attrs
