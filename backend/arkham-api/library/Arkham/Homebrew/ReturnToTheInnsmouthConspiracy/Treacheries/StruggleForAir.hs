module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Treacheries.StruggleForAir (struggleForAir) where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers (getFloodLevelFor)
import Arkham.Classes.HasGame (HasGame)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Helpers (campaignI18n)
import Arkham.I18n
import Arkham.Investigator.Types (Field (InvestigatorSlots))
import Arkham.Location.FloodLevel
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Slot (Slot (..))
import Arkham.SlotType
import Arkham.Treachery.Import.Lifted

newtype StruggleForAir = StruggleForAir TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

struggleForAir :: TreacheryCard StruggleForAir
struggleForAir = treachery StruggleForAir Cards.struggleForAir

{- | The designer offers an optional "(max. 3)" soft erratum on the damage and horror.
Printed text is implemented here; see return-to-innsmouth-faq-errata.md.
-}
instance RunMessage StruggleForAir where
  runMessage msg t@(StruggleForAir attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      getFloodLevelFor iid >>= \case
        Unflooded -> gainSurge attrs
        _ -> do
          -- "you may discard any number of assets you control from play"
          assets <- select $ assetControlledBy iid <> DiscardableAsset
          campaignI18n $ scope "struggleForAir" $ chooseSomeM iid "doNotDiscard" do
            targets assets $ toDiscardBy iid attrs
          -- The slots are counted after those discards resolve, so the test is
          -- started from a second step rather than inline.
          doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      filled <- getFilledHandAndBodySlots iid
      sid <- getRandom
      skillTestModifier sid attrs sid (Difficulty filled)
      revelationSkillTest sid iid attrs #agility (Fixed 0)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n | n > 0 -> do
      assignDamageAndHorror iid attrs n n
      pure t
    _ -> StruggleForAir <$> liftRunMessage msg attrs

-- | "For each of your hand or body slots that is filled, this test's difficulty is increased by 1."
getFilledHandAndBodySlots :: HasGame m => InvestigatorId -> m Int
getFilledHandAndBodySlots iid = do
  slots <- field InvestigatorSlots iid
  pure $ length [s | ty <- [HandSlot, BodySlot], s <- findWithDefault [] ty slots, slotFilled s]
 where
  slotFilled s = notNull $ case s of
    Slot {assets = as} -> as
    RestrictedSlot {assets = as} -> as
    AdjustableSlot {assets = as} -> as
