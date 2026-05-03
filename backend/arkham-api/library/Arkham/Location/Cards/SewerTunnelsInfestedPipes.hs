module Arkham.Location.Cards.SewerTunnelsInfestedPipes (sewerTunnelsInfestedPipes) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (sewerTunnelsInfestedPipes)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype SewerTunnelsInfestedPipes = SewerTunnelsInfestedPipes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sewerTunnelsInfestedPipes :: LocationCard SewerTunnelsInfestedPipes
sewerTunnelsInfestedPipes = location SewerTunnelsInfestedPipes Cards.sewerTunnelsInfestedPipes 1 (PerPlayer 1)

instance HasAbilities SewerTunnelsInfestedPipes where
  getAbilities (SewerTunnelsInfestedPipes a) =
    extendRevealed1 a $ restricted a 1 Here $ forced $ Enters #after You (be a)

instance RunMessage SewerTunnelsInfestedPipes where
  runMessage msg l@(SewerTunnelsInfestedPipes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck #enemy
      pure l
    RequestedEncounterCard (isAbilitySource attrs 1 -> True) (Just iid) (Just card) -> do
      drawCard iid card
      pure l
    _ -> SewerTunnelsInfestedPipes <$> liftRunMessage msg attrs
