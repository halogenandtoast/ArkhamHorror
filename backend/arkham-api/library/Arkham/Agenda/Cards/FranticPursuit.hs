module Arkham.Agenda.Cards.FranticPursuit (FranticPursuit (..), franticPursuit) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Scenarios.TheVanishingOfElinaHarper.Helpers

newtype FranticPursuit = FranticPursuit AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

franticPursuit :: AgendaCard FranticPursuit
franticPursuit = agenda (3, A) FranticPursuit Cards.franticPursuit (Static 7)

instance HasAbilities FranticPursuit where
  getAbilities (FranticPursuit a) =
    [forcedAbility a 1 $ EnemyDefeated #when You (BySource $ SourceOwnedBy You) notKidnapper]

instance RunMessage FranticPursuit where
  runMessage msg a@(FranticPursuit attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R1
      pure a
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> enemy) _ -> do
      moveAllTokens (attrs.ability 1) enemy iid #clue
      outForBlood enemy
      pure a
    _ -> FranticPursuit <$> liftRunMessage msg attrs
