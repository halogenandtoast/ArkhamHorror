module Arkham.Asset.Assets.SkyRelicErodedByWinds (skyRelic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifyEach, modifySelf)
import Arkham.Helpers.SkillTest (getSkillTest)
import Arkham.Helpers.Story
import Arkham.Story.Cards qualified as Stories

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
      -- "If you succeed, flip this card and resolve its text." The back (11663b)
      -- is a story card, which owns the glyph and the victory display.
      readStory iid attrs Stories.skyRelicStory
      pure a
    _ -> SkyRelicErodedByWinds <$> liftRunMessage msg attrs
