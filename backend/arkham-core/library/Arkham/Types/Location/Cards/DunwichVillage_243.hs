module Arkham.Types.Location.Cards.DunwichVillage_243
  ( dunwichVillage_243
  , DunwichVillage_243(..)
  )
where

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
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype DunwichVillage_243 = DunwichVillage_243 LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_243 :: LocationId -> DunwichVillage_243
dunwichVillage_243 = DunwichVillage_243 . baseAttrs
  Cards.dunwichVillage_243
  3
  (Static 1)
  Circle
  [Triangle, Square, Diamond]

instance HasModifiersFor env DunwichVillage_243 where
  getModifiersFor = noModifiersFor

ability :: LocationAttrs -> Ability
ability attrs =
  mkAbility (toSource attrs) 1 (ActionAbility Nothing (ActionCost 1))

instance ActionRunner env => HasActions env DunwichVillage_243 where
  getActions iid NonFast (DunwichVillage_243 attrs) =
    withBaseActions iid NonFast attrs $ do
      broodOfYogSothoth <- getSet @EnemyId (CardCode "02255")
      pure
        $ [ resignAction iid attrs | iid `on` attrs ]
        <> [ ActivateCardAbilityAction iid (ability attrs)
           | notNull broodOfYogSothoth
           ]
  getActions iid window (DunwichVillage_243 attrs) =
    getActions iid window attrs

instance LocationRunner env => RunMessage env DunwichVillage_243 where
  runMessage msg l@(DunwichVillage_243 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      broodOfYogSothoth <- getSetList @EnemyId (CardCode "02255")
      when
        (null broodOfYogSothoth)
        (throwIO $ InvalidState "should not have been able to use this ability")
      l <$ unshiftMessages
        [ chooseOne
            iid
            [ TargetLabel
                (EnemyTarget eid)
                [MoveToward (EnemyTarget eid) (LocationWithId $ toId attrs)]
            ]
        | eid <- broodOfYogSothoth
        ]
    _ -> DunwichVillage_243 <$> runMessage msg attrs
