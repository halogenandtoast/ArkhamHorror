module Arkham.Asset.Assets.ObsidianRelicPuzzlingEffigy (obsidianRelic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Message.Lifted.Log (record)

newtype ObsidianRelicPuzzlingEffigy = ObsidianRelicPuzzlingEffigy AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

obsidianRelic :: AssetCard ObsidianRelicPuzzlingEffigy
obsidianRelic = asset ObsidianRelicPuzzlingEffigy Cards.obsidianRelic

instance HasModifiersFor ObsidianRelicPuzzlingEffigy where
  getModifiersFor (ObsidianRelicPuzzlingEffigy a) = do
    -- If this card would leave play, remove it from the game instead.
    modifySelf a [RemoveFromGameInsteadOfDiscard]
    -- During its own test, double the skill icons of each card committed to the test.
    whenJustM getSkillTest \st ->
      when (isAbilitySource a 1 st.source)
        $ modifyEach a (concat $ toList st.committedCards) [DoubleSkillIcons]

instance HasAbilities ObsidianRelicPuzzlingEffigy where
  getAbilities (ObsidianRelicPuzzlingEffigy a) =
    [skillTestAbility $ controlled_ a 1 actionAbility]

instance RunMessage ObsidianRelicPuzzlingEffigy where
  runMessage msg a@(ObsidianRelicPuzzlingEffigy attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 8)
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      -- "Flip this card and resolve its text." The back side (story code 11550b)
      -- is currently registered only as a placeholder Asset CardDef, not as a
      -- Story card, so we cannot read it via readStory yet. As a Glyph asset in
      -- The Drowned City, the known resolvable effect is translating its alien glyph.
      -- TODO: once 11550b is implemented as the proper story/back side, resolve its
      -- text here (likely via readStory) instead of (or in addition to) the glyph
      -- translation below, and verify the actual translated word for glyph "y".
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("y" :: Text, "Effigy" :: Text)
      pure a
    _ -> ObsidianRelicPuzzlingEffigy <$> liftRunMessage msg attrs
