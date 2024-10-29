module Arkham.Act.Cards.ThePathIsBarred (ThePathIsBarred (..), thePathIsBarred) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Card
import Arkham.Helpers.Log (getRecordCount)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher

newtype ThePathIsBarred = ThePathIsBarred ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePathIsBarred :: ActCard ThePathIsBarred
thePathIsBarred =
  act
    (2, A)
    ThePathIsBarred
    Cards.thePathIsBarred
    (Just $ GroupClueCost (PerPlayer 2) (locationIs Locations.tombOfShadows))

instance HasAbilities ThePathIsBarred where
  getAbilities (ThePathIsBarred a) =
    extend1 a
      $ mkAbility a 1
      $ Objective
      $ forced
      $ EnemyDefeated #after Anyone ByAny
      $ enemyIs Enemies.theManInThePallidMask

instance RunMessage ThePathIsBarred where
  runMessage msg a@(ThePathIsBarred attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct aid _ advanceMode | aid == actId attrs && onSide B attrs -> do
      let
        (convictionOrDoubt, nextAct) = case advanceMode of
          AdvancedWithOther -> (Conviction, Cards.theWayOut)
          AdvancedWithClues -> (Doubt, Cards.leadingTheWay)
      convictionOrDoubtCount <- getRecordCount convictionOrDoubt
      recordCount convictionOrDoubt (convictionOrDoubtCount + 2)

      enemy <- getCampaignStoryCard Enemies.theManInThePallidMask
      push $ RemoveFromBearersDeckOrDiscard enemy

      mTheManInThePallidMaskId <- selectOne $ enemyIs Enemies.theManInThePallidMask
      for_ mTheManInThePallidMaskId $ push . RemoveEnemy

      advanceToAct attrs nextAct A
      pure a
    _ -> ThePathIsBarred <$> liftRunMessage msg attrs
