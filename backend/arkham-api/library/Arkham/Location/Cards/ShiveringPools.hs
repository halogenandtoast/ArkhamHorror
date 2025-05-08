module Arkham.Location.Cards.ShiveringPools (shiveringPools) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.I18n
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ShiveringPools = ShiveringPools LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

shiveringPools :: LocationCard ShiveringPools
shiveringPools =
  locationWith ShiveringPools Cards.shiveringPools 5 (PerPlayer 1)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities ShiveringPools where
  getAbilities (ShiveringPools a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #after You
      , restricted a 2 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [Below, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be a)
      ]

instance RunMessage ShiveringPools where
  runMessage msg l@(ShiveringPools attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      hasResources <- fieldP InvestigatorResources (> 0) iid
      chooseOrRunOneM iid $ withI18n do
        countVar 1 $ labeled "takeDirectDamage" $ directDamage iid (attrs.ability 1) 1
        when hasResources $ countVar 5 $ labeled' "loseResources" $ loseResources iid (attrs.ability 1) 5
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [card] -> do
          belowEmpty <- directionEmpty attrs Below
          rightEmpty <- directionEmpty attrs RightOf
          chooseOrRunOneM iid $ scenarioI18n do
            when belowEmpty $ labeled' "below" $ placeAtDirection_ Below attrs card
            when rightEmpty $ labeled' "right" $ placeAtDirection_ RightOf attrs card
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> ShiveringPools <$> liftRunMessage msg attrs
