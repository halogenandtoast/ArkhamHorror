module Arkham.Location.Cards.CandlelitTunnels (candlelitTunnels) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype CandlelitTunnels = CandlelitTunnels LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

candlelitTunnels :: LocationCard CandlelitTunnels
candlelitTunnels =
  locationWith CandlelitTunnels Cards.candlelitTunnels 3 (PerPlayer 2)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities CandlelitTunnels where
  getAbilities (CandlelitTunnels a) =
    extendRevealed
      a
      [ skillTestAbility $ groupLimit PerGame $ restricted a 1 Here actionAbility
      , restricted a 2 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [LeftOf, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be a)
      ]

instance RunMessage CandlelitTunnels where
  runMessage msg l@(CandlelitTunnels attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseSelectM iid UnrevealedLocation $ lookAtRevealed iid (attrs.ability 1)
      pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      n <- countM (directionEmpty attrs) [LeftOf, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [LeftOf, RightOf]
      pure l
    _ -> CandlelitTunnels <$> liftRunMessage msg attrs
