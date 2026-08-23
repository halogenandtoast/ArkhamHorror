module Arkham.Agenda.Cards.ChildrenOfBlood.BloodMoney.UnderABloodMoon (underABloodMoon) where

import Arkham.Ability
import Arkham.Agenda.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Creation (createExhausted)
import Arkham.Enemy.Types (Field (EnemyLocation))
import Arkham.Helpers.Query (getLead)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Projection
import Arkham.Trait (Trait (Civilian, Cultist, Monster))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype UnderABloodMoon = UnderABloodMoon AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underABloodMoon :: AgendaCard UnderABloodMoon
underABloodMoon = agenda (3, A) UnderABloodMoon Cards.underABloodMoon (Static 8)

instance HasAbilities UnderABloodMoon where
  getAbilities (UnderABloodMoon a) =
    [ mkAbility a 1
        $ forced
        $ EnemyDefeated
          #after
          Anyone
          (BySource $ SourceIsEnemy (EnemyWithKeyword Keyword.Predator))
          (mapOneOf EnemyWithTrait [Civilian, Cultist])
    ]

getDefeatedEnemy :: [Window] -> EnemyId
getDefeatedEnemy [] = error "wrong window"
getDefeatedEnemy ((windowType -> Window.EnemyDefeated _ _ eid) : _) = eid
getDefeatedEnemy (_ : xs) = getDefeatedEnemy xs

instance RunMessage UnderABloodMoon where
  runMessage msg a@(UnderABloodMoon attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getDefeatedEnemy -> eid) _ -> do
      lead <- getLead
      mlid <- field EnemyLocation eid
      removeFromGame eid
      for_ mlid \lid ->
        findEncounterCard
          lead
          (ProxyTarget (toTarget attrs) (toTarget lid))
          (#enemy <> CardWithTrait Monster)
      pure a
    FoundEncounterCard _ (ProxyTarget (isTarget attrs -> True) (LocationTarget lid)) (toCard -> card) -> do
      createEnemyWith_ card lid createExhausted
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach UneliminatedInvestigator (investigatorDefeated attrs)
      advanceAgendaDeck attrs
      pure a
    _ -> UnderABloodMoon <$> liftRunMessage msg attrs
