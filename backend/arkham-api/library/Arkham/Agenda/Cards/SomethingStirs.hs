module Arkham.Agenda.Cards.SomethingStirs (somethingStirs) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Scenario
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SomethingStirs = SomethingStirs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingStirs :: AgendaCard SomethingStirs
somethingStirs =
  agendaWith (1, A) SomethingStirs Cards.somethingStirs (StaticWithPerPlayer 6 1)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomLocations = Nowhere})

-- ability does not do anything, just triggers the button
instance HasAbilities SomethingStirs where
  getAbilities (SomethingStirs a) = [mkAbility a 1 $ forced $ AgendaAdvances #when (be a)]

instance RunMessage SomethingStirs where
  runMessage msg a@(SomethingStirs attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      maxDoom <- fieldMax LocationDoom Anywhere
      locations <- select $ LocationWithDoom (static maxDoom)
      isReturnTo <- getIsReturnTo
      leadChooseOneM
        $ targets locations
        $ createEnemyAt_
        $ if isReturnTo then Enemies.harbingerOfValusiaTheSleeperReturns else Enemies.harbingerOfValusia

      advanceAgendaDeck attrs
      pure a
    _ -> SomethingStirs <$> liftRunMessage msg attrs
