module Arkham.Homebrew.DarkMatter.Treacheries.Decoherence (decoherence) where

import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype Decoherence = Decoherence TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decoherence :: TreacheryCard Decoherence
decoherence = treachery Decoherence Cards.decoherence

{- | "Revelation - Test [agility] (6). Decrease the difficulty of this test by 1
for each of your 'Memories'. If you fail, you must either take 2 horror or lose
2 resources."
-}
instance RunMessage Decoherence where
  runMessage msg t@(Decoherence attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      memories <- getMemories iid
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed $ max 0 (6 - memories))
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseOneM iid $ withI18n do
        countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        countVar 2 $ labeled' "loseResources" $ loseResources iid attrs 2
      pure t
    _ -> Decoherence <$> liftRunMessage msg attrs
