module Arkham.Agenda.Cards.OpeningHand (openingHand) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Scenarios.FortuneAndFolly.Helpers
import Arkham.Trait (Trait (Casino))

newtype OpeningHand = OpeningHand AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

openingHand :: AgendaCard OpeningHand
openingHand = agenda (2, A) OpeningHand Cards.openingHand (Static 5)

instance HasAbilities OpeningHand where
  getAbilities (OpeningHand a) =
    [ restricted a 1 (exists $ InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
        $ forced
        $ PhaseBegins #when #enemy
    , mkAbility a 2 $ forced $ EnemyDefeated #after You ByAny (EnemyWithTrait Casino)
    ]

instance RunMessage OpeningHand where
  runMessage msg a@(OpeningHand attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      raiseAlarmLevel (attrs.ability 1)
        =<< select (InvestigatorAt $ LocationWithEnemy $ EnemyWithTrait Casino <> ReadyEnemy)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      raiseAlarmLevel (attrs.ability 2) [iid]
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyAtLocationMatching_ Enemies.dimensionalShamblerHunterFromBeyond "Roulette Wheel"
      shuffleEncounterDiscardBackIn
      doStep 1 msg
      advanceAgendaDeck attrs
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      shuffleSetAsideEncounterSetIntoEncounterDeck Set.PlanInShambles
      pure a
    _ -> OpeningHand <$> liftRunMessage msg attrs
