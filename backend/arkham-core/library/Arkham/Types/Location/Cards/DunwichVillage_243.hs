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
import Arkham.Types.EnemyId
import Arkham.Types.Exception
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

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

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility Nothing (ActionCost 1))

instance ActionRunner env => HasAbilities env DunwichVillage_243 where
  getAbilities iid window@(Window Timing.When NonFast) (DunwichVillage_243 attrs)
    = do
      baseActions <- withResignAction iid window attrs
      broodOfYogSothoth <- getSet @EnemyId (CardCode "02255")
      pure
        $ baseActions
        <> [ locationAbility (ability attrs) | notNull broodOfYogSothoth ]
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
