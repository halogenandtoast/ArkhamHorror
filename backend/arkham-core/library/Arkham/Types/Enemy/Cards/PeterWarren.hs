module Arkham.Types.Enemy.Cards.PeterWarren
  ( PeterWarren(..)
  , peterWarren
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action hiding (Ability)
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.EnemyId
import Arkham.Types.GameValue
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.Message
import Arkham.Types.Source
import Arkham.Types.Window

newtype PeterWarren = PeterWarren EnemyAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peterWarren :: EnemyId -> PeterWarren
peterWarren uuid =
  PeterWarren
    $ baseAttrs uuid "01139"
    $ (healthDamageL .~ 1)
    . (fightL .~ 2)
    . (healthL .~ Static 3)
    . (evadeL .~ 3)
    . (uniqueL .~ True)
    . (spawnAtL ?~ LocationWithTitle "Miskatonic University")

instance HasModifiersFor env PeterWarren where
  getModifiersFor = noModifiersFor

instance ActionRunner env => HasActions env PeterWarren where
  getActions iid NonFast (PeterWarren attrs@EnemyAttrs {..}) =
    withBaseActions iid NonFast attrs $ do
      locationId <- getId @LocationId iid
      pure
        [ ActivateCardAbilityAction
            iid
            (mkAbility
              (toSource attrs)
              1
              (ActionAbility (Just Parley) (Costs [ActionCost 1, ClueCost 2]))
            )
        | locationId == enemyLocation
        ]
  getActions _ _ _ = pure []

instance (EnemyRunner env) => RunMessage env PeterWarren where
  runMessage msg e@(PeterWarren attrs@EnemyAttrs {..}) = case msg of
    UseCardAbility _ (EnemySource eid) _ 1 _ | eid == enemyId ->
      e <$ unshiftMessage (AddToVictory $ toTarget attrs)
    _ -> PeterWarren <$> runMessage msg attrs
