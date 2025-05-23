module Arkham.Enemy.Cards.DiscipleOfTheDevourer (discipleOfTheDevourer) where

import Arkham.Ability
import Arkham.Agenda.Sequence (AgendaStep (..), agendaStep)
import Arkham.Agenda.Types (Field (..))
import Arkham.Campaigns.NightOfTheZealot.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype DiscipleOfTheDevourer = DiscipleOfTheDevourer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discipleOfTheDevourer :: EnemyCard DiscipleOfTheDevourer
discipleOfTheDevourer =
  enemy DiscipleOfTheDevourer Cards.discipleOfTheDevourer (3, Static 1, 1) (1, 0)
    & setSpawnAt (FarthestLocationFromYou EmptyLocation)

instance HasAbilities DiscipleOfTheDevourer where
  getAbilities (DiscipleOfTheDevourer x) =
    extend1 x $ mkAbility x 1 $ forced $ EnemySpawns #after Anywhere (be x)

instance RunMessage DiscipleOfTheDevourer where
  runMessage msg e@(DiscipleOfTheDevourer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      agendaId <- selectJust AnyAgenda
      mLocationId <- field EnemyLocation (toId attrs)
      hasClues <- fieldP InvestigatorClues (> 0) iid
      step <- fieldMap AgendaSequence agendaStep agendaId
      if step == AgendaStep 1
        then chooseOrRunOneM iid $ campaignI18n do
          labeled' "discipleOfTheDevourer.doom" $ placeDoom (attrs.ability 1) attrs 1
          when hasClues do
            for_ mLocationId \lid ->
              labeled' "discipleOfTheDevourer.clue" $ moveTokens (attrs.ability 1) iid lid #clue 1
        else do
          placeDoom (attrs.ability 1) attrs 1
          when hasClues do
            for_ mLocationId \lid ->
              moveTokens (attrs.ability 1) iid lid #clue 1
      pure e
    _ -> DiscipleOfTheDevourer <$> liftRunMessage msg attrs
