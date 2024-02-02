module Arkham.Asset.Cards.RelicOfAgesUnleashTheTimestream (
  relicOfAgesUnleashTheTimestream,
  RelicOfAgesUnleashTheTimestream (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype RelicOfAgesUnleashTheTimestream = RelicOfAgesUnleashTheTimestream AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

relicOfAgesUnleashTheTimestream :: AssetCard RelicOfAgesUnleashTheTimestream
relicOfAgesUnleashTheTimestream =
  asset RelicOfAgesUnleashTheTimestream Cards.relicOfAgesUnleashTheTimestream

instance HasAbilities RelicOfAgesUnleashTheTimestream where
  getAbilities (RelicOfAgesUnleashTheTimestream a) =
    [ restrictedAbility a 1 ControlsThis
        $ ForcedAbility
        $ Explored Timing.After You
        $ FailedExplore
        $ CardWithType TreacheryType
    ]

getFailureCard :: [Window] -> Card
getFailureCard [] = error "invalid window"
getFailureCard ((windowType -> Window.Explored _ (Window.Failure c)) : _) = c
getFailureCard (_ : xs) = getFailureCard xs

instance RunMessage RelicOfAgesUnleashTheTimestream where
  runMessage msg a@(RelicOfAgesUnleashTheTimestream attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (getFailureCard -> card) _ ->
      do
        push $ ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) [card]
        pure a
    _ -> RelicOfAgesUnleashTheTimestream <$> runMessage msg attrs
