module Arkham.Agenda.Cards.TheCityFloods (theCityFloods) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Card
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Treachery.Cards qualified as Treacheries

newtype TheCityFloods = TheCityFloods AgendaAttrs
  deriving anyclass (IsAgenda, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCityFloods :: AgendaCard TheCityFloods
theCityFloods = agenda (3, A) TheCityFloods Cards.theCityFloods (Static 8)

instance HasModifiersFor TheCityFloods where
  getModifiersFor (TheCityFloods a) = do
    ancientEvils <- findAllCards (`isCard` Treacheries.ancientEvils)
    modifyEach a ancientEvils [AddKeyword Keyword.Surge]

instance RunMessage TheCityFloods where
  runMessage msg a@(TheCityFloods attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      push R3
      pure a
    _ -> TheCityFloods <$> liftRunMessage msg attrs
