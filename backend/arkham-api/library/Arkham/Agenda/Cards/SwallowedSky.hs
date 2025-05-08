module Arkham.Agenda.Cards.SwallowedSky (swallowedSky) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Trait

newtype SwallowedSky = SwallowedSky AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swallowedSky :: AgendaCard SwallowedSky
swallowedSky = agenda (3, C) SwallowedSky Cards.swallowedSky (Static 8)

instance HasModifiersFor SwallowedSky where
  getModifiersFor (SwallowedSky a) = modifySelect a (EnemyWithTrait Monster) [EnemyFight 1]

instance RunMessage SwallowedSky where
  runMessage msg a@(SwallowedSky attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide D attrs -> True) -> do
      push R3
      pure a
    _ -> SwallowedSky <$> liftRunMessage msg attrs
