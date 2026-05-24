module Arkham.Agenda.Cards.IntoTheCaves (intoTheCaves) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Message.Discard.Lifted (randomDiscard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Coastal, Dark))

newtype IntoTheCaves = IntoTheCaves AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

intoTheCaves :: AgendaCard IntoTheCaves
intoTheCaves = agenda (1, A) IntoTheCaves Cards.intoTheCaves (Static 4)

instance RunMessage IntoTheCaves where
  runMessage msg a@(IntoTheCaves attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      locations <- select $ NearestLocationToMost (LocationWithTrait Coastal)
      leadChooseOrRunOneM do
        targets locations $ createSetAsideEnemy_ Enemies.crustaceanHybridInTheLight

      selectEach (InvestigatorAt $ LocationWithTrait Dark) \iid ->
        randomDiscard iid attrs

      day <- getCampaignDay
      let doomCount = case day of
            Day1 -> 1
            Day2 -> 2
            Day3 -> 3
      placeDoomOnAgenda doomCount

      advanceAgendaDeck attrs
      pure a
    _ -> IntoTheCaves <$> liftRunMessage msg attrs
