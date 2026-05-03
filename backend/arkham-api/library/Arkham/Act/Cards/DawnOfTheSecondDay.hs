module Arkham.Act.Cards.DawnOfTheSecondDay (dawnOfTheSecondDay) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Helpers.Modifiers (ModifierType (..), semaphore)
import Arkham.Matcher

newtype DawnOfTheSecondDay = DawnOfTheSecondDay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dawnOfTheSecondDay :: ActCard DawnOfTheSecondDay
dawnOfTheSecondDay = act (1, A) DawnOfTheSecondDay Cards.dawnOfTheSecondDay Nothing

instance RunMessage DawnOfTheSecondDay where
  runMessage msg a@(DawnOfTheSecondDay attrs) = runQueueT $ case msg of
    KonamiCode pid -> do
      f <- getLogger
      selectEach (InvestigatorIsPlayer pid) \iid ->
        semaphore iid do
          gameModifier attrs iid Semaphore
          liftIO $ f (ClientCardOnly pid "A Mistake" (toJSON $ flipCard $ toCard attrs))
          codex iid attrs 17
      pure a
    _ -> DawnOfTheSecondDay <$> liftRunMessage msg attrs
