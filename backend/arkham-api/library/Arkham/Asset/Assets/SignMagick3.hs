module Arkham.Asset.Assets.SignMagick3 (signMagick3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Slot
import Arkham.Trait
import Arkham.Window (Window (..), defaultWindows)
import Arkham.Window qualified as Window

newtype SignMagick3 = SignMagick3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signMagick3 :: AssetCard SignMagick3
signMagick3 = asset SignMagick3 Cards.signMagick3

instance HasAbilities SignMagick3 where
  getAbilities (SignMagick3 a) =
    [ controlled
        a
        1
        ( ExcludeWindowAssetExists
            $ AssetControlledBy You
            <> hasAnyTrait [Spell, Ritual]
            <> AssetWithPerformableAbility AbilityIsActionAbility [ActionCostSetToModifier 0]
        )
        $ triggered
          (ActivateAbility #after You $ AbilityIsActionAbility <> AssetAbility (hasAnyTrait [Spell, Ritual]))
          (exhaust a)
    ]

toOriginalAsset :: [Window] -> AssetId
toOriginalAsset [] = error "invalid window"
toOriginalAsset ((windowType -> Window.ActivateAbility _ _ ability) : xs) =
  fromMaybe (toOriginalAsset xs) (abilitySource ability).asset
toOriginalAsset (_ : xs) = toOriginalAsset xs

instance RunMessage SignMagick3 where
  runMessage msg a@(SignMagick3 attrs) = case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push
        $ AddSlot iid ArcaneSlot
        $ RestrictedSlot (toSource attrs) (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual]) []
      SignMagick3 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 (toOriginalAsset -> aid) _ -> do
      let nullifyActionCost ab = applyAbilityModifiers ab [ActionCostSetToModifier 0]
      abilities <-
        selectMap (doesNotProvokeAttacksOfOpportunity . nullifyActionCost)
          $ AbilityIsActionAbility
          <> AssetAbility
            ( NotAsset (AssetWithId aid)
                <> assetControlledBy iid
                <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Ritual]
            )
      -- True Magick (5) surfaces its borrowed in-hand spells as abilities of its own
      -- (getTrueMagickInHandAbilities), so the matcher offers them alongside True
      -- Magick's wrapper. Only the wrapper reveals the card from hand, so take it and
      -- drop the proxies -- otherwise the list names three cards that aren't in play.
      let notBorrowed ab = case ab.source of
            ProxySource (CardIdSource _) _ -> False
            _ -> True
      -- Sign Magick grants an [action] activation, so the windows must NOT include
      -- FastPlayerWindow. True Magick's wrapper re-filters its in-hand spells against
      -- whatever windows we publish, and a FastPlayerWindow lets a borrowed [fast]
      -- ability through -- Scrying (3) has no [action] at all. Empty windows are not an
      -- option either: `getCanPerformAbility` guards `notNull matching` before any
      -- criteria, so the wrapper would find nothing and `chooseOne` would throw (#5801).
      let actionWindows = [w | w <- defaultWindows iid, windowType w /= Window.FastPlayerWindow]
      abilities' <- filterM (getCanPerformAbility iid actionWindows) (filter notBorrowed abilities)
      player <- getPlayer iid
      push $ chooseOne player [AbilityLabel iid ab actionWindows [] [] | ab <- abilities']
      pure a
    _ -> SignMagick3 <$> runMessage msg attrs
