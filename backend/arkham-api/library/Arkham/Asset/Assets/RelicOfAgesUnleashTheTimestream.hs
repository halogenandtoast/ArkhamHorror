module Arkham.Asset.Assets.RelicOfAgesUnleashTheTimestream (relicOfAgesUnleashTheTimestream) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RelicOfAgesUnleashTheTimestream = RelicOfAgesUnleashTheTimestream AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

relicOfAgesUnleashTheTimestream :: AssetCard RelicOfAgesUnleashTheTimestream
relicOfAgesUnleashTheTimestream = asset RelicOfAgesUnleashTheTimestream Cards.relicOfAgesUnleashTheTimestream

instance HasAbilities RelicOfAgesUnleashTheTimestream where
  getAbilities (RelicOfAgesUnleashTheTimestream a) =
    [restricted a 1 ControlsThis $ forced $ Explored #after You Anywhere $ FailedExplore #treachery]

getFailureCard :: [Window] -> Card
getFailureCard [] = error "invalid window"
getFailureCard ((windowType -> Window.Explored _ _ (Window.Failure c)) : _) = c
getFailureCard (_ : xs) = getFailureCard xs

instance RunMessage RelicOfAgesUnleashTheTimestream where
  runMessage msg a@(RelicOfAgesUnleashTheTimestream attrs) = runQueueT $ case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getFailureCard -> card) _ -> do
      shuffleCardsIntoDeck ExplorationDeck [card]
      pure a
    _ -> RelicOfAgesUnleashTheTimestream <$> liftRunMessage msg attrs
