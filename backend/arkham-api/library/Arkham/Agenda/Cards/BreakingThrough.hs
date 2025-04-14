module Arkham.Agenda.Cards.BreakingThrough (breakingThrough) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EncounterCardSource)
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Matcher

newtype BreakingThrough = BreakingThrough AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

breakingThrough :: AgendaCard BreakingThrough
breakingThrough = agenda (3, A) BreakingThrough Cards.breakingThrough (Static 6)

instance HasAbilities BreakingThrough where
  getAbilities (BreakingThrough x) =
    [mkAbility x 1 $ forced $ MovedBy #after You EncounterCardSource]

instance RunMessage BreakingThrough where
  runMessage msg a@(BreakingThrough attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      yogSothothSpawnLocation <-
        fromMaybeM
          (getJustLocationByName "Another Dimension")
          (getLocationByName "The Edge of the Universe")
      createEnemyAt_ Enemies.yogSothoth yogSothothSpawnLocation
      advanceAgendaDeck attrs
      pure a
    _ -> BreakingThrough <$> liftRunMessage msg attrs
