module Arkham.Treachery.Cards.TracesToNowhere.LockedOut (lockedOut) where

import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.TracesToNowhere.Helpers
import Arkham.Treachery.CardDefs.TracesToNowhere qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LockedOut = LockedOut TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockedOut :: TreacheryCard LockedOut
lockedOut = treachery LockedOut Cards.lockedOut

instance RunMessage LockedOut where
  runMessage msg t@(LockedOut attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      allies <- select $ #ally <> AssetAt (locationWithInvestigator iid)
      chooseOrRunOneM iid $ scenarioI18n do
        labeled "lockedOut.loseAction" $ loseActions iid attrs 1
        labeledValidate (notNull allies) "lockedOut.allyDamage" do
          chooseTargetM iid allies \ally -> dealAssetDamage ally attrs 1
      pure t
    _ -> LockedOut <$> liftRunMessage msg attrs
