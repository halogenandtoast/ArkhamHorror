module Arkham.Treachery.Cards.EmpyreanBrilliance (empyreanBrilliance) where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card.Cost
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype EmpyreanBrilliance = EmpyreanBrilliance TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empyreanBrilliance :: TreacheryCard EmpyreanBrilliance
empyreanBrilliance = treachery EmpyreanBrilliance Cards.empyreanBrilliance

instance RunMessage EmpyreanBrilliance where
  runMessage msg t@(EmpyreanBrilliance attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getCampaignTime >>= \case
        Day -> do
          n <- fieldMap InvestigatorHand length iid
          sid <- getRandom
          revelationSkillTest sid iid attrs #willpower (Fixed $ 2 + if n >= 5 then 2 else 0)
        Night -> randomDiscardEdit iid attrs \d -> d {discardTarget = Just (toTarget attrs)}
      pure t
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      let
        totalPrintedCost =
          case mapMaybe (.cost) cards of
            [] -> 0
            costs -> sum (map toPrintedCost costs)
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed totalPrintedCost)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      getCampaignTime >>= \case
        Day -> chooseAndDiscardCards iid attrs 2
        Night -> assignHorror iid attrs 2
      pure t
    _ -> EmpyreanBrilliance <$> liftRunMessage msg attrs
