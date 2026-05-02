module Arkham.Act.Cards.DawnOfTheFirstDay (dawnOfTheFirstDay) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, semaphore)
import Arkham.I18n
import Arkham.Matcher

newtype DawnOfTheFirstDay = DawnOfTheFirstDay ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dawnOfTheFirstDay :: ActCard DawnOfTheFirstDay
dawnOfTheFirstDay = act (1, A) DawnOfTheFirstDay Cards.dawnOfTheFirstDay Nothing

instance HasModifiersFor DawnOfTheFirstDay where
  getModifiersFor (DawnOfTheFirstDay attrs) =
    modifySelect attrs Anyone [CannotBeDamaged, CannotBeDefeated]

instance RunMessage DawnOfTheFirstDay where
  runMessage msg a@(DawnOfTheFirstDay attrs) = runQueueT $ case msg of
    KonamiCode pid -> campaignI18n do
      f <- getLogger
      selectEach (InvestigatorIsPlayer pid) \iid ->
        semaphore iid do
          gameModifier attrs iid Semaphore
          liftIO $ f (ClientCardOnly pid "The Brilliance" (toJSON $ flipCard $ toCard attrs))
          gainXp iid attrs (ikey "xp.theBrilliance") 1
      pure a
    _ -> DawnOfTheFirstDay <$> liftRunMessage msg attrs
