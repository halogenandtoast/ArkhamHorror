module Arkham.Location.Cards.UnderseaVault (underseaVault) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Log (record)

newtype UnderseaVault = UnderseaVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaVault :: LocationCard UnderseaVault
underseaVault =
  locationWith UnderseaVault Cards.underseaVault 5 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities UnderseaVault where
  getAbilities (UnderseaVault a) =
    extendRevealed1 a $ skillTestAbility $ restricted a 1 Here actionAbility

instance RunMessage UnderseaVault where
  runMessage msg l@(UnderseaVault attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- X = this location's level (its grid row + 1; row 0 is level 1).
      let lvl = maybe 0 ((+ 1) . (.row)) (locationPosition attrs)
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed lvl)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (locationCanBeFlipped attrs) $ flipOver iid attrs
      pure l
    Flip _iid _ (isTarget attrs -> True) -> do
      -- "Flip this card and resolve its text." The back side (story code 11532b)
      -- is currently registered only as a placeholder Location CardDef, not as a
      -- Story card, so we cannot read it via readStory yet. As a Glyph location in
      -- The Drowned City, the known resolvable effect is translating its alien glyph.
      -- TODO: once 11532b is implemented as the proper story/back side, resolve its
      -- text here (likely via readStory) instead of (or in addition to) the glyph
      -- translation below, and verify the actual glyph name/translated word.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("Undersea Vault" :: Text, "Vault" :: Text)
      pure . UnderseaVault $ attrs & canBeFlippedL .~ False
    _ -> UnderseaVault <$> liftRunMessage msg attrs
