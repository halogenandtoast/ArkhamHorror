module Arkham.Agenda.Cards.SomethingElseStirs (somethingElseStirs) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (InvestigatorDefeated)
import Arkham.Card (genCard)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreationMethod (SpawnEngagedWith))
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Scenarios.RelicsOfThePast.Helpers
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype SomethingElseStirs = SomethingElseStirs AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

somethingElseStirs :: AgendaCard SomethingElseStirs
somethingElseStirs =
  agendaWith (1, A) SomethingElseStirs Cards.somethingElseStirs (StaticWithPerPlayer 4 1)
    $ removeDoomMatchersL
    %~ (\m -> m {removeDoomLocations = Nowhere})

-- first ability does not do anything, it surfaces the doom retention on advance
instance HasAbilities SomethingElseStirs where
  getAbilities (SomethingElseStirs a) =
    [ mkAbility a 1 $ forced $ AgendaAdvances #when (be a)
    , mkAbility a 2 $ forced $ InvestigatorDefeated #when ByAny Anyone
    ]

instance RunMessage SomethingElseStirs where
  runMessage msg a@(SomethingElseStirs attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> pure a
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.InvestigatorDefeated _ iid -> shuffleAncientAssetsIntoExplorationDeck iid
        _ -> pure ()
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      card <- genCard Enemies.dwellerInThePit
      selectOne (InvestigatorWithTitle "Monterey Jack") >>= \case
        Just monterey -> createEnemy_ card (SpawnEngagedWith monterey)
        Nothing -> do
          lead <- getLead
          withLocationOf lead \lid -> createEnemyAt_ card lid
      advanceAgendaDeck attrs
      pure a
    _ -> SomethingElseStirs <$> liftRunMessage msg attrs
