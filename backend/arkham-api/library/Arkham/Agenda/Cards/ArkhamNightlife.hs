module Arkham.Agenda.Cards.ArkhamNightlife (arkhamNightlife) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Creation (EnemyCreationMethod (SpawnEngagedWith))
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Arkham, Criminal))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype ArkhamNightlife = ArkhamNightlife AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamNightlife :: AgendaCard ArkhamNightlife
arkhamNightlife = agenda (1, A) ArkhamNightlife Cards.arkhamNightlife (Static 7)

instance HasAbilities ArkhamNightlife where
  getAbilities (ArkhamNightlife a) =
    [ mkAbility a 1
        $ triggered_ (DiscoveringLastClue #after Anyone (LocationWithTrait Arkham))
    | onSide A a
    ]

getLastClueLocation :: [Window] -> Maybe LocationId
getLastClueLocation =
  asum . map \case
    (windowType -> Window.DiscoveringLastClue _ lid) -> Just lid
    _ -> Nothing

instance RunMessage ArkhamNightlife where
  runMessage msg a@(ArkhamNightlife attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getLastClueLocation -> Just lid) _ -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 3 lid
      pure a
    DiscardedTopOfEncounterDeck _ cards (isSource attrs -> True) (LocationTarget lid) -> do
      let criminals = filterCards (CardWithTrait Criminal) cards
      for_ criminals \card ->
        createEnemyAtLocationMatching_ card (NearestLocationToLocation lid EmptyLocation)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      gangEnforcer <- getSetAsideCard Enemies.gangEnforcer
      investigators <- select MostClues
      leadChooseOrRunOneM $ targets investigators \iid ->
        createEnemy_ gangEnforcer (SpawnEngagedWith iid)
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> ArkhamNightlife <$> liftRunMessage msg attrs
