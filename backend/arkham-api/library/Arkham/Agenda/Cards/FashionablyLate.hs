module Arkham.Agenda.Cards.FashionablyLate (fashionablyLate) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Trait

newtype FashionablyLate = FashionablyLate AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fashionablyLate :: AgendaCard FashionablyLate
fashionablyLate = agenda (1, A) FashionablyLate Cards.fashionablyLate (Static 3)

instance RunMessage FashionablyLate where
  runMessage msg a@(FashionablyLate attrs) = runQueueT $ case msg of
    AdvanceAgenda (isSide B attrs -> True) -> do
      createEnemyAtLocationMatching_ Cards.dianneDevine
        $ LocationWithAsset
        $ AssetWithFewestClues
        $ AssetWithTrait Bystander

      shuffleEncounterDiscardBackIn
      advanceAgendaDeck attrs
      pure a
    _ -> FashionablyLate <$> liftRunMessage msg attrs
