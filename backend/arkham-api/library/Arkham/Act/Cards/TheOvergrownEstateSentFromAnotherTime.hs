module Arkham.Act.Cards.TheOvergrownEstateSentFromAnotherTime (theOvergrownEstateSentFromAnotherTime) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation
import Arkham.Helpers.Log
import Arkham.Matcher

newtype TheOvergrownEstateSentFromAnotherTime = TheOvergrownEstateSentFromAnotherTime ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

theOvergrownEstateSentFromAnotherTime :: ActCard TheOvergrownEstateSentFromAnotherTime
theOvergrownEstateSentFromAnotherTime =
  act (2, G) TheOvergrownEstateSentFromAnotherTime Cards.theOvergrownEstateSentFromAnotherTime
    $ Just
    $ GroupClueCost (PerPlayer 2) "The Hastings Estate"

instance RunMessage TheOvergrownEstateSentFromAnotherTime where
  runMessage msg a@(TheOvergrownEstateSentFromAnotherTime attrs) = runQueueT $ case msg of
    AdvanceAct (isSide H attrs -> True) _ _ -> do
      harbinger <- genCard Enemies.harbingerOfValusiaTheSleeperReturns
      theHastingsEstate <- selectJust $ location_ "The Hastings Estate"
      createEnemyWithM_ harbinger theHastingsEstate \create -> do
        yigsFury <- getRecordCount YigsFury
        let handleCreate = if yigsFury <= 5 then createExhausted else id
        setAfter (handleCreate create) do
          startingDamage <- getRecordCount TheHarbingerIsStillAlive
          when (startingDamage > 0) $ placeTokens attrs create.enemy #damage startingDamage
      advanceToAct attrs Cards.impossiblePursuit G
      pure a
    _ -> TheOvergrownEstateSentFromAnotherTime <$> liftRunMessage msg attrs
