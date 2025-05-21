module Arkham.Location.Cards.CryptOfTheSepulchralLamp (cryptOfTheSepulchralLamp) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype CryptOfTheSepulchralLamp = CryptOfTheSepulchralLamp LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cryptOfTheSepulchralLamp :: LocationCard CryptOfTheSepulchralLamp
cryptOfTheSepulchralLamp =
  locationWith CryptOfTheSepulchralLamp Cards.cryptOfTheSepulchralLamp 2 (PerPlayer 2)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)
    . (investigateSkillL .~ #willpower)

instance HasAbilities CryptOfTheSepulchralLamp where
  getAbilities (CryptOfTheSepulchralLamp a) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [Above, RightOf]])
      $ forced
      $ RevealLocation #when Anyone (be a)

instance RunMessage CryptOfTheSepulchralLamp where
  runMessage msg l@(CryptOfTheSepulchralLamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Above, RightOf]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, RightOf]
      pure l
    _ -> CryptOfTheSepulchralLamp <$> liftRunMessage msg attrs
