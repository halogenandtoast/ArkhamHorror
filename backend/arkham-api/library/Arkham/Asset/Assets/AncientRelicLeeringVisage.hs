module Arkham.Asset.Assets.AncientRelicLeeringVisage (ancientRelic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype AncientRelicLeeringVisage = AncientRelicLeeringVisage AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientRelic :: AssetCard AncientRelicLeeringVisage
ancientRelic = assetWith AncientRelicLeeringVisage Cards.ancientRelic (healthL ?~ 5)

instance HasModifiersFor AncientRelicLeeringVisage where
  getModifiersFor (AncientRelicLeeringVisage a) =
    -- "If Ancient Relic would leave play, set it aside, out of play."
    modifySelf a [RemoveFromGameInsteadOfDiscard]

instance HasAbilities AncientRelicLeeringVisage where
  getAbilities (AncientRelicLeeringVisage a) =
    [ controlled_ a 1 $ forced $ DealtDamage #after AnySource You
    , controlled_ a 2 $ forced $ GameEnds #when
    ]

instance RunMessage AncientRelicLeeringVisage where
  runMessage msg a@(AncientRelicLeeringVisage attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      -- "Deal 1 damage to Ancient Relic (place 1 damage on this asset)."
      dealAssetDamage attrs.id (attrs.ability 1) 1
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      flipOver iid attrs
      pure a
    Flip _iid _ (isTarget attrs -> True) -> do
      -- "Flip this card and resolve its text." The back side (story code 11581b)
      -- is currently registered only as a placeholder Asset CardDef, not as a
      -- Story card, so we cannot read it via readStory yet. As a Glyph asset in
      -- The Drowned City, the known resolvable effect is translating its alien glyph.
      -- TODO: once 11581b is implemented as the proper story/back side, resolve its
      -- text here (likely via readStory) instead of (or in addition to) the glyph
      -- translation below, and verify the actual translated word for the rune_s glyph
      -- (placeholder "Spirit" below).
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("Ancient Relic" :: Text, "Spirit" :: Text)
      pure a
    _ -> AncientRelicLeeringVisage <$> liftRunMessage msg attrs
