module Arkham.Homebrew.AgesUnwound.Treacheries.Syzygy (syzygy) where

import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Cards
import Arkham.Matcher
import Arkham.Trait (Trait (Future, Past, Present))
import Arkham.Treachery.Import.Lifted

newtype Syzygy = Syzygy TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

syzygy :: TreacheryCard Syzygy
syzygy = treachery Syzygy Cards.syzygy

instance RunMessage Syzygy where
  runMessage msg t@(Syzygy attrs) = runQueueT $ case msg of
    Revelation _ (isSource attrs -> True) -> do
      selectEach (InvestigatorAt $ LocationWithTrait Past) \iid ->
        loseActions iid attrs 1
      selectEach (InvestigatorAt $ LocationWithTrait Present) \iid -> do
        assignDamageAndHorror iid attrs 1 1
      selectEach (InvestigatorAt $ LocationWithTrait Future) \iid -> do
        gainActions iid attrs 1
        drawEncounterCard iid attrs
      pure t
    _ -> Syzygy <$> liftRunMessage msg attrs
