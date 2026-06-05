module Arkham.Location.Cards.MirrorNest_169 (mirrorNest_169) where

import Arkham.Ability
import Arkham.Helpers.Act (getCurrentAct)
import Arkham.Helpers.Message.Discard.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck (ScenarioDeckKey (..))
import Arkham.Strategy

newtype MirrorNest_169 = MirrorNest_169 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mirrorNest_169 :: LocationCard MirrorNest_169
mirrorNest_169 = location MirrorNest_169 Cards.mirrorNest_169 3 (PerPlayer 2)

-- The "Search The Abyss" group ability is limited to once per act. We mark the
-- current act with a scenario modifier when it's used; the marker disappears
-- naturally when the act advances (the new act is a different entity), so the
-- ability becomes available again.
abyssSearched :: ModifierType
abyssSearched = ScenarioModifier "abyssSearched"

instance HasAbilities MirrorNest_169 where
  getAbilities (MirrorNest_169 a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #when You
      , restricted a 2 (Here <> not_ (ActExists (ActWithModifier abyssSearched)))
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 7) Anywhere)
      ]

instance RunMessage MirrorNest_169 where
  runMessage msg l@(MirrorNest_169 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      randomDiscardN iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      currentAct <- getCurrentAct
      gameModifier (attrs.ability 2) currentAct abyssSearched
      search iid (attrs.ability 2) (ScenarioDeckTarget AbyssDeck) [fromDeck] #any (AddFoundToHand iid 1)
      pure l
    _ -> MirrorNest_169 <$> liftRunMessage msg attrs
