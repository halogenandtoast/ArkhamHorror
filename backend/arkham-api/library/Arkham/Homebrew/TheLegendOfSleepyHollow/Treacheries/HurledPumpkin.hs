module Arkham.Homebrew.TheLegendOfSleepyHollow.Treacheries.HurledPumpkin (hurledPumpkin) where

import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Helpers.SkillTest (getSkillTest, getSkillTestSource)
import Arkham.Homebrew.TheLegendOfSleepyHollow.CardDefs.Treacheries qualified as Cards
import Arkham.Homebrew.TheLegendOfSleepyHollow.Helpers
import Arkham.Investigator.Types (Field (InvestigatorHand, InvestigatorLocation))
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection
import Arkham.Treachery.Import.Lifted

newtype HurledPumpkin = HurledPumpkin TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hurledPumpkin :: TreacheryCard HurledPumpkin
hurledPumpkin = treachery HurledPumpkin Cards.hurledPumpkin

-- | "This test gets +1 difficulty and peril if your location has doom on it."
instance HasModifiersFor HurledPumpkin where
  getModifiersFor (HurledPumpkin attrs) = do
    doomed <- getLocationDoomed
    modifySelf attrs [AddKeyword Keyword.Peril | doomed]
    whenJustM getSkillTest \st -> maybeModified_ attrs (SkillTestTarget st.id) do
      source <- MaybeT getSkillTestSource
      guard $ isSource attrs source
      pure [Difficulty 1 | doomed]
   where
    getLocationDoomed =
      field InvestigatorLocation attrs.drawnBy >>= maybe (pure False) locationIsDoomed

instance RunMessage HurledPumpkin where
  runMessage msg t@(HurledPumpkin attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #agility (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignDamage iid attrs 1
      hand <- filter (`cardMatch` NonWeakness) <$> field InvestigatorHand iid
      unless (null hand) do
        shuffleCardsIntoDeck iid hand
        drawCards iid attrs (length hand)
      pure t
    _ -> HurledPumpkin <$> liftRunMessage msg attrs
