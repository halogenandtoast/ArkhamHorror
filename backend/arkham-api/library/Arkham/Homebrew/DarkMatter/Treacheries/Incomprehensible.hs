module Arkham.Homebrew.DarkMatter.Treacheries.Incomprehensible (incomprehensible) where

import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Homebrew.DarkMatter.CardDefs.Treacheries qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Incomprehensible = Incomprehensible TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

incomprehensible :: TreacheryCard Incomprehensible
incomprehensible = treachery Incomprehensible Cards.incomprehensible

{- | "Revelation - Test [intellect] (2). For each point you succeed by, take 1
horror (to a maximum of 3 horror.) Skill icons committed to this test subtract
from your skill value instead of adding to it."
-}
instance RunMessage Incomprehensible where
  runMessage msg t@(Incomprehensible attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      skillTestModifier sid attrs sid SkillIconsSubtract
      revelationSkillTest sid iid attrs #intellect (Fixed 2)
      pure t
    PassedThisSkillTestBy iid (isSource attrs -> True) n -> do
      assignHorror iid attrs (min 3 n)
      pure t
    _ -> Incomprehensible <$> liftRunMessage msg attrs
