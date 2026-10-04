module Arkham.Homebrew.TheSymphonyOfErichZann.Treacheries.Overwhelm (overwhelm) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Homebrew.TheSymphonyOfErichZann.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheSymphonyOfErichZann.Helpers (musicTreacheriesInPlay, scenarioI18n)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Overwhelm = Overwhelm TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

overwhelm :: TreacheryCard Overwhelm
overwhelm = treachery Overwhelm Cards.overwhelm

instance RunMessage Overwhelm where
  runMessage msg t@(Overwhelm attrs) = runQueueT $ case msg of
    {- "Test willpower (2). This test gets +1 difficulty for each Music treachery
    in play." -}
    Revelation iid (isSource attrs -> True) -> do
      n <- length <$> musicTreacheriesInPlay
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed $ 2 + n)
      pure t
    {- "If you fail, you must either discard 1 card from your hand for each point
    you fail by, or take 2 horror." -}
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid $ scenarioI18n $ scope "overwhelm" do
        countVar n $ labeled "discardCards" $ chooseAndDiscardCards iid attrs n
        labeled "takeHorror" $ assignHorror iid attrs 2
      pure t
    _ -> Overwhelm <$> liftRunMessage msg attrs
