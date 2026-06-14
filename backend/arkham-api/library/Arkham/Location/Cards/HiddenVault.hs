module Arkham.Location.Cards.HiddenVault (hiddenVault) where

import Arkham.Ability
import Arkham.Card
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype HiddenVault = HiddenVault LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenVault :: LocationCard HiddenVault
hiddenVault =
  locationWith HiddenVault Cards.hiddenVault 3 (Static 1) (canBeFlippedL .~ True)

instance HasAbilities HiddenVault where
  getAbilities (HiddenVault a) =
    extendRevealed a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 Here
          $ freeReaction
          $ SkillTestResult #after You (WhileEvadingAnEnemy $ enemyAt a) (SuccessResult $ atLeast 2)
      ]

instance RunMessage HiddenVault where
  runMessage msg l@(HiddenVault attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      -- Search the encounter deck AND discard pile for an enemy and spawn it here.
      findEncounterCard iid attrs (card_ #enemy)
      pure l
    FoundEncounterCard _iid (isTarget attrs -> True) (toCard -> card) -> do
      createEnemyAt_ card attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      when (locationCanBeFlipped attrs) $ flipOver iid attrs
      pure l
    Flip _iid _ (isTarget attrs -> True) -> do
      -- "Flip this card and resolve its text." The resolvable effect for a Glyph
      -- location in The Drowned City is translating its alien glyph.
      -- TODO: the recorded word for rune_u is unverified in our data; "Eye" is a
      -- placeholder. Verify the actual translated word on the back side.
      record TheInvestigatorsDiscoveredAnAlienLanguage
      campaignSpecific "translateGlyph" ("Hidden Vault" :: Text, "Eye" :: Text)
      pure . HiddenVault $ attrs & canBeFlippedL .~ False
    _ -> HiddenVault <$> liftRunMessage msg attrs
