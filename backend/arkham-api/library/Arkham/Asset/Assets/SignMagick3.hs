module Arkham.Asset.Assets.SignMagick3 (signMagick3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Constants (pattern NonActivateAbility)
import Arkham.GameEnv (getCard)
import Arkham.Helpers.Ability
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Window (getWindowActivatedAsset, getWindowRevealedCardId)
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

{- | The asset to rule out as "not a DIFFERENT asset", if any.

Nothing for a True Magick (5) borrowed activation: what was activated is the revealed
[Spell] True Magick became a copy of, so True Magick is still a different [Spell] asset
we may point back at. It cannot re-offer the card it just revealed, though -- that would
be the same asset -- and it recognises it from the window we forward below.
-}
toOriginalAsset :: [Window] -> Maybe AssetId
toOriginalAsset = join . getWindowActivatedAsset

{- | True Magick (5) resolves an in-hand [Spell] "by revealing them from your hand", so a
borrowed ability has to show the card before it resolves. Its own chooser pushes the
@RevealCard@; when we offer the borrowed ability directly, we owe it.
-}
revealFirst :: Ability -> [Message]
revealFirst ab = case ab.source of
  ProxySource (CardIdSource cid) _ -> [RevealCard cid]
  _ -> []

instance RunMessage SignMagick3 where
  runMessage msg a@(SignMagick3 attrs) = case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push
        $ AddSlot iid ArcaneSlot
        $ RestrictedSlot (toSource attrs) (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual]) []
      SignMagick3 <$> runMessage msg attrs
    UseCardAbility iid (isSource attrs -> True) 1 ws _ -> do
      let nullifyActionCost ab = applyAbilityModifiers ab [ActionCostSetToModifier 0]
      let notTheActivatedAsset = maybe id (\aid -> (NotAsset (AssetWithId aid) <>)) (toOriginalAsset ws)
      abilities <-
        selectMap (doesNotProvokeAttacksOfOpportunity . nullifyActionCost)
          $ AbilityIsActionAbility
          <> AssetAbility
            ( notTheActivatedAsset
                $ assetControlledBy iid
                <> AssetOneOf [AssetWithTrait Spell, AssetWithTrait Ritual]
            )

      -- True Magick (5): what you activate is the revealed [Spell] it became a copy of,
      -- not True Magick (FAQ v2.5 Q69 -- cost, name, text box and traits). So the entries
      -- here are the in-hand spells themselves, which getTrueMagickInHandAbilities
      -- re-sources onto True Magick, each revealed as it is chosen. Two things drop out:
      --
      --   * the NonActivateAbility wrapper, which is a chooser rather than an [action]
      --     ability on an asset, and is redundant once the spells are listed directly;
      --   * every ability belonging to a card revealed in THIS window -- both its proxy
      --     and, while the copy is still live, the copy's own ability off `getAbilities`.
      --     That card is the asset just activated, so it is not "a different asset".
      revealedCodes <- traverse (fmap toCardCode . getCard) (mapMaybe getWindowRevealedCardId ws)
      let isWrapper ab = ab.index == NonActivateAbility
      let isActivatedCopy ab = ab.cardCode `elem` revealedCodes
      let candidates = filter (\ab -> not (isWrapper ab) && not (isActivatedCopy ab)) abilities

      -- Sign Magick grants an [action] activation, so the windows must NOT include
      -- FastPlayerWindow: a borrowed ability is re-checked against them and a fast window
      -- lets a [fast]-only spell through (Scrying (3) has no [action] at all). Empty
      -- windows are not an option either -- `getCanPerformAbility` guards `notNull
      -- matching` ahead of any criteria, so nothing would be performable (#5801).
      let actionWindows = [w | w <- defaultWindows iid, windowType w /= Window.FastPlayerWindow]
      candidates' <- filterM (getCanPerformAbility iid actionWindows) candidates
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ AbilityLabel iid ab actionWindows (revealFirst ab) []
          | ab <- candidates'
          ]
      pure a
    _ -> SignMagick3 <$> runMessage msg attrs
