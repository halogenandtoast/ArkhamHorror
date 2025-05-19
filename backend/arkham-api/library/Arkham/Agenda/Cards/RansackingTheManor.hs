module Arkham.Agenda.Cards.RansackingTheManor (ransackingTheManor) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Phase
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RansackingTheManor = RansackingTheManor AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ransackingTheManor :: AgendaCard RansackingTheManor
ransackingTheManor = agenda (2, A) RansackingTheManor Cards.ransackingTheManor (PerPlayer 2)

instance HasModifiersFor RansackingTheManor where
  getModifiersFor (RansackingTheManor attrs) =
    modified_ attrs (PhaseTarget #mythos) [SkipMythosPhaseStep PlaceDoomOnAgendaStep]

instance HasAbilities RansackingTheManor where
  getAbilities (RansackingTheManor attrs) =
    [mkAbility attrs 1 $ forced $ PlacedCounterOnEnemy #after AnyEnemy AnySource #clue AnyValue]

instance RunMessage RansackingTheManor where
  runMessage msg a@(RansackingTheManor attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyAtLocationMatching_ Enemies.possessedOathspeaker "Entry Hall"
      whenM (not <$> slain Enemies.sebastienMoreau) do
        createEnemyAtLocationMatching_ Enemies.sebastienMoreau "Entry Hall"
      advanceAgendaDeck attrs
      pure a
    UseCardAbility _ (isSource attrs -> True) 1 [windowType -> Window.PlacedClues _ target n] _ -> do
      flipCluesToDoom target n
      pure a
    _ -> RansackingTheManor <$> liftRunMessage msg attrs
