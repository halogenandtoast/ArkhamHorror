module Arkham.Types.Enemy.Cards.AlmaHill
  ( AlmaHill(..)
  , almaHill
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Runner
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Window

newtype AlmaHill = AlmaHill EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

almaHill :: EnemyCard AlmaHill
almaHill = enemy AlmaHill Cards.almaHill
  $ (sanityDamageL .~ 2)
  . (fightL .~ 3)
  . (healthL .~ Static 3)
  . (evadeL .~ 3)

instance HasModifiersFor env AlmaHill where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env AlmaHill where
  getActions iid NonFast (AlmaHill attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (EnemySource enemyId)
              1
              (ActionAbility (Just Parley) (ActionCost 1))
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env AlmaHill where
  runMessage msg e@(AlmaHill attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Southside")
    UseCardAbility iid (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ unshiftMessages
        (replicate 3 (InvestigatorDrawEncounterCard iid)
        <> [AddToVictory (toTarget attrs)]
        )
    _ -> AlmaHill <$> runMessage msg attrs
