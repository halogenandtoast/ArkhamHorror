module Arkham.Location.Cards.DunwichVillage_243
  ( dunwichVillage_243
  , DunwichVillage_243(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Exception
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( dunwichVillage_243 )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers

newtype DunwichVillage_243 = DunwichVillage_243 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_243 :: LocationCard DunwichVillage_243
dunwichVillage_243 =
  location DunwichVillage_243 Cards.dunwichVillage_243 2 (Static 3)

instance HasAbilities DunwichVillage_243 where
  getAbilities (DunwichVillage_243 x) = withResignAction
    x
    [ restrictedAbility
        x
        1
        (Here <> EnemyCriteria
          (EnemyExists $ EnemyWithTitle "Brood of Yog-Sothoth")
        )
      $ ActionAbility Nothing
      $ ActionCost 1
    | locationRevealed x
    ]

instance RunMessage DunwichVillage_243 where
  runMessage msg l@(DunwichVillage_243 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      broodOfYogSothoth <- getBroodOfYogSothoth
      when
        (null broodOfYogSothoth)
        (throwIO $ InvalidState "should not have been able to use this ability")
      l <$ pushAll
        [ chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid)
                [MoveToward (EnemyTarget eid) (LocationWithId $ toId attrs)]
            ]
        | eid <- broodOfYogSothoth
        ]
    _ -> DunwichVillage_243 <$> runMessage msg attrs
