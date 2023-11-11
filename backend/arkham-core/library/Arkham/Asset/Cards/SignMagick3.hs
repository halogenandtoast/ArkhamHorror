module Arkham.Asset.Cards.SignMagick3 (
  signMagick3,
  SignMagick3 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Id
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window (..), defaultWindows)
import Arkham.Window qualified as Window

newtype SignMagick3 = SignMagick3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signMagick3 :: AssetCard SignMagick3
signMagick3 =
  asset SignMagick3 Cards.signMagick3

instance HasAbilities SignMagick3 where
  getAbilities (SignMagick3 a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> ExcludeWindowAssetExists
              ( AssetControlledBy You
                  <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Ritual]
                  <> AssetWithPerformableAbility AbilityIsActionAbility [ActionCostSetToModifier 0]
              )
        )
        $ ReactionAbility
          ( ActivateAbility Timing.After You
              $ AssetAbility
              $ AssetOneOf [AssetWithTrait Spell, AssetWithTrait Ritual]
          )
        $ ExhaustCost (toTarget a)
    ]

toOriginalAsset :: [Window] -> AssetId
toOriginalAsset [] = error "invalid window"
toOriginalAsset ((windowType -> Window.ActivateAbility _ _ ability) : xs) = case abilitySource ability of
  AssetSource aid -> aid
  _ -> toOriginalAsset xs
toOriginalAsset (_ : xs) = toOriginalAsset xs

instance RunMessage SignMagick3 where
  runMessage msg a@(SignMagick3 attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push
        $ AddSlot iid ArcaneSlot
        $ RestrictedSlot (toSource attrs) (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual]) []
      SignMagick3 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (toOriginalAsset -> aid) _ -> do
      let nullifyActionCost ab = applyAbilityModifiers ab [ActionCostSetToModifier 0]
      abilities <-
        selectListMap nullifyActionCost
          $ AbilityIsActionAbility
          <> AssetAbility
            ( NotAsset (AssetWithId aid)
                <> assetControlledBy iid
                <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Ritual]
            )
      abilities' <-
        filterM (\ab -> anyM (\w -> getCanPerformAbility iid w ab) (defaultWindows iid)) abilities
      player <- getPlayer iid
      push $ chooseOne player [AbilityLabel iid ab [] [] | ab <- abilities']
      pure a
    _ -> SignMagick3 <$> runMessage msg attrs
