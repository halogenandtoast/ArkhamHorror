module Arkham.Treachery.Cards.Excruciate (excruciate) where

import Arkham.Campaigns.TheScarletKeys.Key.Cards qualified as Keys
import Arkham.Campaigns.TheScarletKeys.Key.Matcher
import Arkham.Campaigns.TheScarletKeys.Key.Types
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Token
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Excruciate = Excruciate TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

excruciate :: TreacheryCard Excruciate
excruciate = treachery Excruciate Cards.excruciate

instance RunMessage Excruciate where
  runMessage msg t@(Excruciate attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
      placeTokens attrs theShadeReaper #charge 1
      doStep 1 msg
      pure t
    DoStep 1 (Revelation iid (isSource attrs -> True)) -> do
      theShadeReaper <- selectJust $ scarletKeyIs Keys.theShadeReaper
      charges <- fieldMap ScarletKeyTokens (countTokens #charge) theShadeReaper
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ if charges >= 8 then 6 else 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      hasDiscard <- selectAny $ inHandOf NotForPlay iid <> basic DiscardableCard
      chooseOneM iid $ withI18n do
        countVar n $ labeledValidate' hasDiscard "discardCardsFromHand" $ chooseAndDiscardCards iid attrs n
        countVar 2 $ labeled' "takeDamage" $ assignDamage iid attrs 2
      pure t
    _ -> Excruciate <$> liftRunMessage msg attrs
