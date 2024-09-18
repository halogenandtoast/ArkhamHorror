module Arkham.Agenda.Cards.GrowingSuspicion (GrowingSuspicion (..), growingSuspicion) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype GrowingSuspicion = GrowingSuspicion AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

growingSuspicion :: AgendaCard GrowingSuspicion
growingSuspicion = agenda (2, A) GrowingSuspicion Cards.growingSuspicion (Static 7)

instance HasAbilities GrowingSuspicion where
  getAbilities (GrowingSuspicion a) =
    [forcedAbility a 1 $ EnemyDefeated #when You (BySource $ SourceOwnedBy You) notKidnapper]

instance RunMessage GrowingSuspicion where
  runMessage msg a@(GrowingSuspicion attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      moveAllTokens (attrs.ability 1) enemy iid #clue
      outForBlood enemy
      pure a
    _ -> GrowingSuspicion <$> liftRunMessage msg attrs
