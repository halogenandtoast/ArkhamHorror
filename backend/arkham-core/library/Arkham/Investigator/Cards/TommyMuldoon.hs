module Arkham.Investigator.Cards.TommyMuldoon (
  tommyMuldoon,
  TommyMuldoon (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types qualified as Field
import Arkham.Deck qualified as Deck
import Arkham.Id
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Projection
import Arkham.Window (Window, windowType)
import Arkham.Window qualified as Window

newtype TommyMuldoon = TommyMuldoon InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tommyMuldoon :: InvestigatorCard TommyMuldoon
tommyMuldoon =
  investigator TommyMuldoon Cards.tommyMuldoon
    $ Stats {health = 8, sanity = 6, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities TommyMuldoon where
  getAbilities (TommyMuldoon attrs) =
    [ restrictedAbility attrs 1 (Self <> CanGainResources)
        $ freeReaction
        $ Matcher.AssetDefeated #when ByAny
        $ AssetControlledBy You
        <> AssetOneOf [AssetWithHorror, AssetWithDamage]
    ]

instance HasChaosTokenValue TommyMuldoon where
  getChaosTokenValue iid ElderSign (TommyMuldoon attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getAsset :: [Window] -> AssetId
getAsset = \case
  ((windowType -> Window.AssetDefeated aid _) : _) -> aid
  (_ : rest) -> getAsset rest
  _ -> error "impossible"

instance RunMessage TommyMuldoon where
  runMessage msg i@(TommyMuldoon attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getAsset -> asset) _ -> do
      damage <- field Field.AssetDamage asset
      horror <- field Field.AssetHorror asset
      pushAll
        [ TakeResources iid (damage + horror) (toAbilitySource attrs 1) False
        , ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget asset)
        ]
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      -- AssetControlledBy You
      -- <> AssetOneOf [AssetWithHorror, AssetWithDamage]
      pure i
    _ -> TommyMuldoon <$> runMessage msg attrs
