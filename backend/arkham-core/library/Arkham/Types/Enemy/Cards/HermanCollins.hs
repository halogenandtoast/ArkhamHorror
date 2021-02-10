module Arkham.Types.Enemy.Cards.HermanCollins
  ( HermanCollins(..)
  , hermanCollins
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Window
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner

newtype HermanCollins = HermanCollins EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hermanCollins :: EnemyId -> HermanCollins
hermanCollins uuid =
  HermanCollins
    $ baseAttrs uuid "01138"
    $ (healthDamageL .~ 1)
    . (sanityDamageL .~ 1)
    . (fightL .~ 3)
    . (healthL .~ Static 4)
    . (evadeL .~ 4)
    . (uniqueL .~ True)

instance HasModifiersFor env HermanCollins where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env HermanCollins where
  getActions iid NonFast (HermanCollins attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility
                (Just Parley)
                (Costs [ActionCost 1, HandDiscardCost 4 Nothing mempty mempty])
              )
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance EnemyRunner env => RunMessage env HermanCollins where
  runMessage msg e@(HermanCollins attrs@EnemyAttrs {..}) = case msg of
    InvestigatorDrawEnemy iid _ eid | eid == enemyId ->
      e <$ spawnAt (Just iid) eid (LocationWithTitle "Graveyard")
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      e <$ unshiftMessage (AddToVictory $ toTarget attrs)
    _ -> HermanCollins <$> runMessage msg attrs
