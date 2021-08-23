module Arkham.Types.Location.Cards.DunwichVillage_243
  ( dunwichVillage_243
  , DunwichVillage_243(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (dunwichVillage_243)
import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.EnemyId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

newtype DunwichVillage_243 = DunwichVillage_243 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_243 :: LocationCard DunwichVillage_243
dunwichVillage_243 = location
  DunwichVillage_243
  Cards.dunwichVillage_243
  2
  (Static 3)
  Circle
  [Triangle, Square, Diamond]

instance HasAbilities env DunwichVillage_243 where
  getAbilities iid window (DunwichVillage_243 x) | locationRevealed x =
    withResignAction iid window x $ do
      pure
        [ restrictedAbility
            x
            1
            (Here <> EnemyCriteria
              (EnemyExists $ EnemyWithTitle "Brood of Yog-Sothoth")
            )
          $ ActionAbility Nothing
          $ ActionCost 1
        ]
  getAbilities iid window (DunwichVillage_243 attrs) =
    getAbilities iid window attrs

instance LocationRunner env => RunMessage env DunwichVillage_243 where
  runMessage msg l@(DunwichVillage_243 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      broodOfYogSothoth <- getSetList @EnemyId (CardCode "02255")
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
