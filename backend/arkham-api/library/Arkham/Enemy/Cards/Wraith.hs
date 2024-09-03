module Arkham.Enemy.Cards.Wraith (wraith, Wraith (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes.HasQueue (HasQueue)
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Creation
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Enemy.Types (Field (..))
import Arkham.Matcher
import Arkham.Placement
import Arkham.Projection

newtype Wraith = Wraith EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wraith :: EnemyCard Wraith
wraith = enemy Wraith Cards.wraith (2, Static 2, 2) (0, 2)

instance HasAbilities Wraith where
  getAbilities (Wraith a) = case a.placement of
    AttachedToLocation lid -> [haunted "Spawn Wraith at this location" (proxied lid a) 2]
    _ ->
      extend1 a
        $ mkAbility a 1
        $ forced
        $ EnemyDefeated #when Anyone (not_ $ BySource $ oneOf [#spell, #relic]) (be a <> at_ Anywhere)

recreateWraith :: HasQueue Message m => EnemyAttrs -> Placement -> m ()
recreateWraith attrs placement = do
  push $ RemovedFromPlay (toSource attrs)
  push
    $ CreateEnemy
    $ MkEnemyCreation
      { enemyCreationCard = toCard attrs
      , enemyCreationEnemyId = attrs.id
      , enemyCreationMethod = SpawnWithPlacement placement
      , enemyCreationBefore = []
      , enemyCreationAfter = []
      , enemyCreationExhausted = False
      , enemyCreationTarget = Nothing
      , enemyCreationInvestigator = Nothing
      }

instance RunMessage Wraith where
  runMessage msg e@(Wraith attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      cancelEnemyDefeat attrs
      recreateWraith attrs . AttachedToLocation =<< fieldJust EnemyLocation attrs.id
      pure e
    UseThisAbility _iid (isProxySource attrs -> True) 2 -> do
      case attrs.placement of
        AttachedToLocation lid -> recreateWraith attrs $ AtLocation lid
        _ -> error "can not trigger"
      pure e
    _ -> Wraith <$> liftRunMessage msg attrs
