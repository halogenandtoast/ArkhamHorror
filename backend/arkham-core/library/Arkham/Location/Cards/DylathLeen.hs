module Arkham.Location.Cards.DylathLeen (dylathLeen, DylathLeen (..)) where

import Arkham.Capability
import Arkham.GameValue
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype DylathLeen = DylathLeen LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

dylathLeen :: LocationCard DylathLeen
dylathLeen = locationWith DylathLeen Cards.dylathLeen 3 (PerPlayer 1) (canBeFlippedL .~ True)

instance HasAbilities DylathLeen where
  getAbilities (DylathLeen attrs) =
    veiled
      attrs
      [restrictedAbility attrs 1 (Here <> can.search.deck You) $ actionAbilityWithCost $ ResourceCost 1]

instance RunMessage DylathLeen where
  runMessage msg l@(DylathLeen attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ search iid (attrs.ability 1) iid [fromTopOfDeck 6] #item (DrawFound iid 1)
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.endlessSecrets
      pure . DylathLeen $ attrs & canBeFlippedL .~ False
    _ -> DylathLeen <$> runMessage msg attrs
