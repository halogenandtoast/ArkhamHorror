module Arkham.Homebrew.DarkMatter.Locations.ClassroomK2 (classroomK2) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (addMemories)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype ClassroomK2 = ClassroomK2 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

classroomK2 :: LocationCard ClassroomK2
classroomK2 = location ClassroomK2 Cards.classroomK2 3 (PerPlayer 2)

{- | "[action]: If Classroom K2 is adjacent to 4 other locations: Each
investigator at Classroom K2 adds 1 tally mark next to their 'Memories'.
(Group limit once per game.)"
-}
instance HasAbilities ClassroomK2 where
  getAbilities (ClassroomK2 a) =
    extendRevealed1 a
      $ groupLimit PerGame
      $ restricted a 1 (Here <> LocationCount 4 (connectedFrom (be a))) actionAbility

instance RunMessage ClassroomK2 where
  runMessage msg l@(ClassroomK2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      here <- select $ investigatorAt attrs.id
      for_ here (`addMemories` 1)
      pure l
    _ -> ClassroomK2 <$> liftRunMessage msg attrs
