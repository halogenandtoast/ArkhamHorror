module Arkham.Homebrew.DarkMatter.Locations.AbandonedLander (abandonedLander) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (getMemories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype AbandonedLander = AbandonedLander LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "Cannot be flipped." — the card def gives it no other side, so nothing can
flip it and 'caveOrCarcosaLocation' never offers it.
-}
abandonedLander :: LocationCard AbandonedLander
abandonedLander = location AbandonedLander Cards.abandonedLander 4 (PerPlayer 1)

{- | "While investigating this location, reduce the shroud by 1 for each tally
mark next to your 'Memories'."
-}
instance HasAbilities AbandonedLander where
  getAbilities (AbandonedLander a) =
    extendRevealed1 a
      $ mkAbility a 1
      $ silent
      $ InitiatedSkillTest #when You #any #any (WhileInvestigating $ be a)

instance RunMessage AbandonedLander where
  runMessage msg l@(AbandonedLander attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      memories <- getMemories iid
      when (memories > 0) do
        withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) attrs (ShroudModifier $ negate memories)
      pure l
    _ -> AbandonedLander <$> liftRunMessage msg attrs
