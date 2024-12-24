module Arkham.Location.Cards.Airfield (airfield) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Card.CardDef
import Arkham.Cost
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.FatalMirage.Helpers
import Arkham.Story.Cards qualified as Stories

newtype Airfield = Airfield LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

airfield :: LocationCard Airfield
airfield = location Airfield Cards.airfield 4 (PerPlayer 2)

mirageCards :: [CardDef]
mirageCards = [Cards.memoryOfAMissingFather]

instance HasModifiersFor Airfield where
  getModifiersFor (Airfield a) = do
    modifySelfWhenM
      a
      ( selectAny
          $ mapOneOf
            assetIs
            [Assets.takadaHirokoAeroplaneMechanic, Assets.takadaHirokoAeroplaneMechanicResolute]
          <> at_ (be a)
      )
      [ShroudModifier (-2)]
    modifySelf a [AdditionalCostToInvestigate (ResourceCost 1)]
    clearedOfMirages a mirageCards

instance HasAbilities Airfield where
  getAbilities (Airfield a) =
    extendRevealed a [mirage a 2 mirageCards]

instance RunMessage Airfield where
  runMessage msg (Airfield attrs) = runQueueT $ case msg of
    _ -> Airfield <$> mirageRunner Stories.airfield mirageCards 2 msg attrs
