module Arkham.Agenda.Cards.SecretsBetterLeftHidden (secretsBetterLeftHidden) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Phase
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype SecretsBetterLeftHidden = SecretsBetterLeftHidden AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretsBetterLeftHidden :: AgendaCard SecretsBetterLeftHidden
secretsBetterLeftHidden = agenda (3, A) SecretsBetterLeftHidden Cards.secretsBetterLeftHidden (PerPlayer 3)

instance HasModifiersFor SecretsBetterLeftHidden where
  getModifiersFor (SecretsBetterLeftHidden attrs) =
    modified_ attrs (PhaseTarget #mythos) [SkipMythosPhaseStep PlaceDoomOnAgendaStep]

instance HasAbilities SecretsBetterLeftHidden where
  getAbilities (SecretsBetterLeftHidden attrs) =
    [mkAbility attrs 1 $ forced $ PlacedCounterOnEnemy #after AnyEnemy AnySource #clue AnyValue]

instance RunMessage SecretsBetterLeftHidden where
  runMessage msg a@(SecretsBetterLeftHidden attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R4
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 [windowType -> Window.PlacedClues _ target n] _ -> do
      flipCluesToDoom target n
      pure a
    _ -> SecretsBetterLeftHidden <$> liftRunMessage msg attrs
