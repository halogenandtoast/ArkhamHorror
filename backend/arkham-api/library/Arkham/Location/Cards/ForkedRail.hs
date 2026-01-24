module Arkham.Location.Cards.ForkedRail (forkedRail) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token

newtype ForkedRail = ForkedRail LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forkedRail :: LocationCard ForkedRail
forkedRail = location ForkedRail Cards.forkedRail 3 (PerPlayer 2)

instance HasAbilities ForkedRail where
  getAbilities (ForkedRail a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (exists (assetAt a <> assetIs Assets.mineCartReliableButBroken) <> exists (ScenarioWithToken Switch))
      $ forced
      $ RoundEnds #when

instance RunMessage ForkedRail where
  runMessage msg l@(ForkedRail attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      removeTokens (attrs.ability 1) ScenarioTarget Switch 1
      pure l
    _ -> ForkedRail <$> liftRunMessage msg attrs
