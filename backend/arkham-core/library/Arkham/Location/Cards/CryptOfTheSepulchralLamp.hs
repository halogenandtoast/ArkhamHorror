module Arkham.Location.Cards.CryptOfTheSepulchralLamp (
  cryptOfTheSepulchralLamp,
  CryptOfTheSepulchralLamp (..),
) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype CryptOfTheSepulchralLamp = CryptOfTheSepulchralLamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptOfTheSepulchralLamp :: LocationCard CryptOfTheSepulchralLamp
cryptOfTheSepulchralLamp =
  locationWith CryptOfTheSepulchralLamp Cards.cryptOfTheSepulchralLamp 2 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation])
    . (investigateSkillL .~ #willpower)

instance HasAbilities CryptOfTheSepulchralLamp where
  getAbilities (CryptOfTheSepulchralLamp attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Above, RightOf]])
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage CryptOfTheSepulchralLamp where
  runMessage msg l@(CryptOfTheSepulchralLamp attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Above, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, RightOf]
      pure l
    _ -> CryptOfTheSepulchralLamp <$> runMessage msg attrs
