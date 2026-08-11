module Arkham.Homebrew.DarkMatter.Locations.ImpassableRavine (impassableRavine) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ImpassableRavine = ImpassableRavine LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impassableRavine :: LocationCard ImpassableRavine
impassableRavine =
  locationWith ImpassableRavine Cards.impassableRavine 4 (PerPlayer 1) (canBeFlippedL .~ True)

-- | "You cannot enter Impassable Ravine from connecting locations."
instance HasModifiersFor ImpassableRavine where
  getModifiersFor (ImpassableRavine a) = modifySelect a Anyone [CannotEnter a.id]

{- | "[free] If there are no [[Carcosa]] locations in play and each undefeated
investigator is at this location: Read the set aside 'Lost Expedition' story
card. (Max once per game.)"
-}
instance HasAbilities ImpassableRavine where
  getAbilities (ImpassableRavine a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> not_ (exists $ LocationWithTrait Carcosa)
            <> not_ (exists $ UneliminatedInvestigator <> not_ (investigatorAt a.id))
        )
      $ freeReaction AnyWindow

instance RunMessage ImpassableRavine where
  runMessage msg l@(ImpassableRavine attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs.id Stories.lostExpedition
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> ImpassableRavine <$> liftRunMessage msg attrs
