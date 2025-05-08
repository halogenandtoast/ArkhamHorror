module Arkham.Treachery.Cards.TheCreaturesTracks (theCreaturesTracks) where

import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype TheCreaturesTracks = TheCreaturesTracks TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCreaturesTracks :: TreacheryCard TheCreaturesTracks
theCreaturesTracks = treachery TheCreaturesTracks Cards.theCreaturesTracks

instance RunMessage TheCreaturesTracks where
  runMessage msg t@(TheCreaturesTracks attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      canSpawn <- notNull <$> getSetAsideBroodOfYogSothoth
      chooseOrRunOneM iid do
        withI18n $ countVar 2 $ labeled' "takeHorror" $ assignHorror iid attrs 2
        when canSpawn $ scenarioI18n do
          labeled' "spawnSetAsideBrood" $ push $ ChooseRandomLocation (toTarget attrs) mempty
      pure t
    ChosenRandomLocation target lid | isTarget attrs target -> do
      setAsideBroodOfYogSothoth <- getSetAsideBroodOfYogSothoth
      for_ (nonEmpty setAsideBroodOfYogSothoth) \xs -> do
        x <- sample xs
        createEnemyAt_ x lid
      pure t
    _ -> TheCreaturesTracks <$> liftRunMessage msg attrs
