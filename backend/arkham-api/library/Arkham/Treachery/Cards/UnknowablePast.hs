module Arkham.Treachery.Cards.UnknowablePast (unknowablePast) where

import Arkham.ChaosToken.Types
import Arkham.Deck
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.ShatteredAeons.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype UnknowablePast = UnknowablePast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unknowablePast :: TreacheryCard UnknowablePast
unknowablePast = treachery UnknowablePast Cards.unknowablePast

instance RunMessage UnknowablePast where
  runMessage msg t@(UnknowablePast attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      chooseOneM iid $ scenarioI18n do
        labeled' "unknowablePast.reveal" $ requestChaosTokens iid attrs 4
        labeledValidate'
          (attrs.drawnFrom /= Just (ScenarioDeckByKey ExplorationDeck))
          "unknowablePast.discard"
          do
            chooseAndDiscardCard iid attrs
            shuffleIntoDeck ExplorationDeck attrs
      pure t
    RequestedChaosTokens (isSource attrs -> True) (Just iid) (map (.face) -> tokens) -> do
      let n = count (`elem` [Skull, Cultist, Tablet, ElderThing, AutoFail]) tokens
      when (n > 0) $ assignDamageAndHorror iid attrs n n
      pure t
    _ -> UnknowablePast <$> liftRunMessage msg attrs
