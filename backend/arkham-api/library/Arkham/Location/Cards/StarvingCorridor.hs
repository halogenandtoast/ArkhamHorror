module Arkham.Location.Cards.StarvingCorridor (starvingCorridor) where

import Arkham.Card (onlyEncounterCards)
import Arkham.Helpers (Deck (..))
import Arkham.Helpers.Scenario (getEncounterDeck)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype StarvingCorridor = StarvingCorridor LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

starvingCorridor :: LocationCard StarvingCorridor
starvingCorridor = location StarvingCorridor Cards.starvingCorridor 3 (Static 2)

instance RunMessage StarvingCorridor where
  runMessage msg (StarvingCorridor attrs) = runQueueT $ case msg of
    Revelation _iid (isSource attrs -> True) -> do
      shuffleEncounterDiscardBackIn
      -- Resolve the bottom-10 shuffle after the discard has been merged back in.
      doStep 1 msg
      StarvingCorridor <$> liftRunMessage msg attrs
    DoStep 1 (Revelation _iid (isSource attrs -> True)) -> do
      acidicCoelom <- getSetAsideCard Cards.acidicCoelom
      churningChasm <- getSetAsideCard Cards.churningChasm
      let setAside = onlyEncounterCards [acidicCoelom, churningChasm]
      -- Pull both cards out of the set-aside zone before placing them in the deck.
      obtainCard acidicCoelom
      obtainCard churningChasm
      deck <- unDeck <$> getEncounterDeck
      let (top, bottom) = splitAt (length deck - 10) deck
      bottom' <- shuffleM (setAside <> bottom)
      setEncounterDeck $ Deck (top <> bottom')
      pure $ StarvingCorridor attrs
    _ -> StarvingCorridor <$> liftRunMessage msg attrs
