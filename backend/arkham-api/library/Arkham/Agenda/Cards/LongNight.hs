module Arkham.Agenda.Cards.LongNight (longNight) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Trait (Trait (Arkham, Criminal))
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype LongNight = LongNight AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

longNight :: AgendaCard LongNight
longNight = agenda (2, A) LongNight Cards.longNight (Static 7)

instance HasAbilities LongNight where
  getAbilities (LongNight a) =
    [ mkAbility a 1
        $ triggered_ (DiscoveringLastClue #after Anyone (LocationWithTrait Arkham))
    | onSide A a
    ]

getLastClueLocation :: [Window] -> Maybe LocationId
getLastClueLocation =
  asum . map \case
    (windowType -> Window.DiscoveringLastClue _ lid) -> Just lid
    _ -> Nothing

instance RunMessage LongNight where
  runMessage msg a@(LongNight attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getLastClueLocation -> Just lid) _ -> do
      discardTopOfEncounterDeckAndHandle iid (attrs.ability 1) 3 lid
      pure a
    DiscardedTopOfEncounterDeck _ cards (isSource attrs -> True) (LocationTarget lid) -> do
      let criminals = filterCards (CardWithTrait Criminal) cards
      for_ criminals \card ->
        createEnemyAtLocationMatching_ card (NearestLocationToLocation lid EmptyLocation)
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach UneliminatedInvestigator \iid -> do
        investigatorDefeated attrs iid
        sufferPhysicalTrauma iid 1
      push $ ScenarioResolution NoResolution
      pure a
    _ -> LongNight <$> liftRunMessage msg attrs
