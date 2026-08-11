module Arkham.Homebrew.DarkMatter.Locations.LabyrinthsOfTasylock (labyrinthsOfTasylock) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype LabyrinthsOfTasylock = LabyrinthsOfTasylock LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The [[Carcosa]] face of Stalagmite Forest.
labyrinthsOfTasylock :: LocationCard LabyrinthsOfTasylock
labyrinthsOfTasylock =
  locationWith LabyrinthsOfTasylock Cards.labyrinthsOfTasylock 2 (PerPlayer 1) (canBeFlippedL .~ True)

{- | "Forced - At the end of your turn, if you are at this location: Take 1
horror." / "{fast} If this location is the only [[Carcosa]] location in play:
Read the set aside "For You Alone" story card. (Max once per game.)"
-}
instance HasAbilities LabyrinthsOfTasylock where
  getAbilities (LabyrinthsOfTasylock a) =
    extendRevealed
      a
      [ restricted a 1 Here $ forced $ TurnEnds #when You
      , playerLimit PerGame
          $ restricted a 2 (Here <> not_ (exists $ LocationWithTrait Carcosa <> not_ (be a)))
          $ freeReaction AnyWindow
      ]

instance RunMessage LabyrinthsOfTasylock where
  runMessage msg l@(LabyrinthsOfTasylock attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignHorror iid (attrs.ability 1) 1
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      readStory iid attrs.id Stories.forYouAlone
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> LabyrinthsOfTasylock <$> liftRunMessage msg attrs
