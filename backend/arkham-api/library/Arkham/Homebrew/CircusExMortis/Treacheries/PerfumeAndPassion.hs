module Arkham.Homebrew.CircusExMortis.Treacheries.PerfumeAndPassion (perfumeAndPassion) where

import Arkham.Helpers.Message.Discard.Lifted (chooseAndDiscardCards)
import Arkham.Homebrew.CircusExMortis.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (..), hasVice)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Import.Lifted

newtype PerfumeAndPassion = PerfumeAndPassion TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

perfumeAndPassion :: TreacheryCard PerfumeAndPassion
perfumeAndPassion = treachery PerfumeAndPassion Cards.perfumeAndPassion

instance RunMessage PerfumeAndPassion where
  runMessage msg t@(PerfumeAndPassion attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      vice <- hasVice iid Intimacy
      chooseNM iid (if vice then 2 else 1) $ withI18n do
        countVar 1 $ labeled "loseActions" $ loseActions iid attrs 1
        countVar 2 $ labeled "discardCardsFromHand" $ chooseAndDiscardCards iid attrs 2
        chooseTest #willpower 3 $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      chooseAndDiscardCards iid attrs 2
      pure t
    _ -> PerfumeAndPassion <$> liftRunMessage msg attrs
