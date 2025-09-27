module Arkham.Treachery.Cards.OminousPortents (ominousPortents) where

import Arkham.Card
import Arkham.Helpers (unDeck)
import Arkham.Keyword (Keyword (Peril))
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.TheWagesOfSin.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype OminousPortents = OminousPortents TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ominousPortents :: TreacheryCard OminousPortents
ominousPortents = treachery OminousPortents Cards.ominousPortents

instance RunMessage OminousPortents where
  runMessage msg t@(OminousPortents attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mTopSpectralCard <- headMay . unDeck <$> getSpectralDeck
      sid <- getRandom
      chooseOrRunOneM iid $ scenarioI18n do
        labeled' "ominousPortents.spectral" do
          for_ mTopSpectralCard \card -> do
            cardResolutionModifiers card attrs card [AddKeyword Peril, EffectsCannotBeCanceled]
            drawCard iid (card {ecAddedPeril = True})
        labeled' "ominousPortents.test" do
          revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      assignHorror iid attrs 2
      pure t
    _ -> OminousPortents <$> liftRunMessage msg attrs
