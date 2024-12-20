module Arkham.Location.Cards.DylathLeen (dylathLeen) where

import Arkham.Ability
import Arkham.Capability
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story
import Arkham.Strategy

newtype DylathLeen = DylathLeen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dylathLeen :: LocationCard DylathLeen
dylathLeen = location DylathLeen Cards.dylathLeen 3 (PerPlayer 1)

instance HasAbilities DylathLeen where
  getAbilities (DylathLeen attrs) =
    veiled
      attrs
      [restricted attrs 1 (Here <> can.search.deck You) $ actionAbilityWithCost $ ResourceCost 1]

instance RunMessage DylathLeen where
  runMessage msg l@(DylathLeen attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      search iid (attrs.ability 1) iid [fromTopOfDeck 6] #item (DrawFound iid 1)
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.endlessSecrets
      pure . DylathLeen $ attrs & canBeFlippedL .~ False
    _ -> DylathLeen <$> liftRunMessage msg attrs
