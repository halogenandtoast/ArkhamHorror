module Arkham.Location.Cards.HiddenVault (hiddenVault) where

import Arkham.Ability
import Arkham.Card
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Stories

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
    Flip iid _ (isTarget attrs -> True) -> do
      -- "Flip this card and resolve its text." The back (11579b) is a story card,
      -- so reading it both shows the flipped side and translates the glyph.
      readStory iid (toId attrs) Stories.hiddenVault
      pure . HiddenVault $ attrs & canBeFlippedL .~ False
    _ -> HiddenVault <$> liftRunMessage msg attrs
