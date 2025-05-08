module Arkham.Agenda.Cards.TheTruthIsHidden (theTruthIsHidden) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Phase
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype TheTruthIsHidden = TheTruthIsHidden AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTruthIsHidden :: AgendaCard TheTruthIsHidden
theTruthIsHidden = agenda (1, A) TheTruthIsHidden Cards.theTruthIsHidden (PerPlayer 2)

instance HasModifiersFor TheTruthIsHidden where
  getModifiersFor (TheTruthIsHidden attrs) =
    modified_ attrs (PhaseTarget #mythos) [SkipMythosPhaseStep PlaceDoomOnAgendaStep]

instance HasAbilities TheTruthIsHidden where
  getAbilities (TheTruthIsHidden attrs) =
    [mkAbility attrs 1 $ forced $ PlacedCounterOnEnemy #after AnyEnemy AnySource #clue AnyValue]

instance RunMessage TheTruthIsHidden where
  runMessage msg a@(TheTruthIsHidden attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 [windowType -> Window.PlacedClues _ target n] _ -> do
      flipCluesToDoom target n
      pure a
    _ -> TheTruthIsHidden <$> liftRunMessage msg attrs
