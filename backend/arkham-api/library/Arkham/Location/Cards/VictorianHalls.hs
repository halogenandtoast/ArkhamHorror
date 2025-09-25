module Arkham.Location.Cards.VictorianHalls (victorianHalls) where

import Arkham.Ability
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait (Trait (SilverTwilight))

newtype VictorianHalls = VictorianHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

victorianHalls :: LocationCard VictorianHalls
victorianHalls = location VictorianHalls Cards.victorianHalls 4 (Static 0)

instance HasAbilities VictorianHalls where
  getAbilities (VictorianHalls a) =
    extendRevealed1 a $ groupLimit PerGame $ restricted a 1 Here actionAbility

instance RunMessage VictorianHalls where
  runMessage msg l@(VictorianHalls attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardUntilFirst iid (attrs.ability 1) Deck.EncounterDeck
        $ basic (CardWithTrait SilverTwilight <> #enemy)
      pure l
    RequestedEncounterCard (isAbilitySource attrs 1 -> True) (Just iid) (Just ec) -> do
      spawnEnemyAt_ ec attrs
      gainClues iid (attrs.ability 1) 2
      pure l
    _ -> VictorianHalls <$> liftRunMessage msg attrs
