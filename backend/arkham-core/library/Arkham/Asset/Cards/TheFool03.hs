module Arkham.Asset.Cards.TheFool03 (
  theFool03,
  TheFool03 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Window (Window, defaultWindows, windowType)
import Arkham.Window qualified as Window

newtype TheFool03 = TheFool03 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

theFool03 :: AssetCard TheFool03
theFool03 = asset TheFool03 Cards.theFool03

instance HasModifiersFor TheFool03 where
  getModifiersFor (InvestigatorTarget iid) (TheFool03 a) | controlledBy a iid && not (assetExhausted a) = do
    pure $ toModifiers a $ [CanReduceCostOf AnyCard 1]
  getModifiersFor _ _ = pure []

instance HasAbilities TheFool03 where
  getAbilities (TheFool03 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (Matcher.PlayCard #when You (BasicCardMatch AnyCard))
        $ exhaust a
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

getCard :: [Window] -> Card
getCard = \case
  [] -> error "impossible"
  ((windowType -> Window.PlayCard _ card) : _) -> card
  (_ : rest) -> getCard rest

instance RunMessage TheFool03 where
  runMessage msg a@(TheFool03 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      push $ costModifier (toAbilitySource attrs 1) iid (ReduceCostOf (CardWithId $ toCardId card) 1)
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      push $ PutCardIntoPlay iid (toCard attrs) Nothing (defaultWindows iid)
      pure a
    _ -> TheFool03 <$> runMessage msg attrs
