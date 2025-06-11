module Arkham.Act.Cards.ImpossiblePursuit (impossiblePursuit) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype ImpossiblePursuit = ImpossiblePursuit ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impossiblePursuit :: ActCard ImpossiblePursuit
impossiblePursuit = act (3, G) ImpossiblePursuit Cards.impossiblePursuit Nothing

instance HasAbilities ImpossiblePursuit where
  getAbilities = actAbilities1' G \a ->
    mkAbility a 1
      $ forced
      $ EnemyLeavesPlay #when
      $ mapOneOf enemyIs [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]

instance RunMessage ImpossiblePursuit where
  runMessage msg a@(ImpossiblePursuit attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      veda <- getSetAsideCard Assets.vedaWhitsleySkilledBotanist
      iids <-
        select
          $ NearestToLocation
          $ LocationWithEnemy
          $ mapOneOf enemyIs [Enemies.harbingerOfValusia, Enemies.harbingerOfValusiaTheSleeperReturns]
      leadChooseOrRunOneM $ targets iids (`takeControlOfSetAsideAsset` veda)
      deckCount <- getActDecksInPlayCount
      push
        $ if deckCount <= 1
          then R1
          else RemoveCompletedActFromGame (actDeckId attrs) (toId attrs)
      pure a
    _ -> ImpossiblePursuit <$> liftRunMessage msg attrs
