module Arkham.Asset.Assets.SkyRelicErodedByWinds (skyRelic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Message.Lifted.Log (record)

newtype SkyRelicErodedByWinds = SkyRelicErodedByWinds AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

skyRelic :: AssetCard SkyRelicErodedByWinds
skyRelic = asset SkyRelicErodedByWinds Cards.skyRelic

instance HasModifiersFor SkyRelicErodedByWinds where
  getModifiersFor (SkyRelicErodedByWinds a) = do
    -- If this card would leave play, remove it from the game instead.
    modifySelf a [RemoveFromGameInsteadOfDiscard]
    -- During its own test, double the skill icons of each card committed to the test.
    whenJustM getSkillTest \st ->
      when (isAbilitySource a 1 st.source)
        $ modifyEach a (concat $ toList st.committedCards) [DoubleSkillIcons]

instance HasAbilities SkyRelicErodedByWinds where
  getAbilities (SkyRelicErodedByWinds a) =
    [skillTestAbility $ controlled_ a 1 actionAbility]

instance RunMessage SkyRelicErodedByWinds where
  runMessage msg a@(SkyRelicErodedByWinds attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 8)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      -- "On success, flip this card and resolve its text." This card can be flipped
      -- (no CannotBeFlipped modifier), so just trigger the flip here.
      flipOver iid attrs
      pure a
    Flip _iid _ (isTarget attrs -> True) -> do
      -- "Flip this card and resolve its text." The back side (story code 11663b)
      -- is currently registered only as a placeholder Asset CardDef, not as a
      -- Story card, so we cannot read it via readStory yet. As a Glyph asset in
      -- The Drowned City, the known resolvable effect is translating its alien glyph.
      -- TODO: once 11663b is implemented as the proper story/back side, resolve its
      -- text here (likely via readStory) instead of (or in addition to) the glyph
      -- translation below, and verify the actual translated word for glyph "rune_f".
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("rune_f" :: Text, "Stars" :: Text)
      pure . SkyRelicErodedByWinds $ attrs & flippedL .~ True
    _ -> SkyRelicErodedByWinds <$> liftRunMessage msg attrs
