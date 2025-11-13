module Arkham.Agenda.Cards.AllBetsDown (allBetsDown) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Trait (Trait (Casino))

newtype AllBetsDown = AllBetsDown AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allBetsDown :: AgendaCard AllBetsDown
allBetsDown = agenda (4, A) AllBetsDown Cards.allBetsDown (Static 12)

instance HasAbilities AllBetsDown where
  getAbilities (AllBetsDown a) =
    [ restricted a 1 (exists $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
        $ forced
        $ PhaseBegins #when #enemy
    , mkAbility a 2 $ forced $ EnemyDefeated #after You ByAny (EnemyWithTrait Casino)
    ]

instance RunMessage AllBetsDown where
  runMessage msg a@(AllBetsDown attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1)
        =<< select (InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      raiseAlarmLevel (attrs.ability 2) [iid]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach UneliminatedInvestigator (`sufferPhysicalTrauma` 1) 
      push R3
      pure a
    _ -> AllBetsDown <$> liftRunMessage msg attrs
