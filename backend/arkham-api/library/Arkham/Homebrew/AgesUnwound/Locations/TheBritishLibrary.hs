module Arkham.Homebrew.AgesUnwound.Locations.TheBritishLibrary (theBritishLibrary) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Missions.Helpers (taskCompleted)
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (locationClues, revealedL)
import Arkham.Matcher

newtype TheBritishLibrary = TheBritishLibrary LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | /A Treasure Unearthed/ puts this into play "revealed location side faceup".
Shroud 10 and a fixed 7 clues, which the shroud reduction eats into as they come
off.
-}
theBritishLibrary :: LocationCard TheBritishLibrary
theBritishLibrary =
  locationWith TheBritishLibrary Cards.theBritishLibrary 10 (Static 7)
    $ (canBeFlippedL .~ True)
    . (revealedL .~ True)

{- | "The British Library gets -1 shroud for each clue on it."

The second modifier is /Book Heist/'s closing clause -- "if you succeeded, for
the remainder of the game, it cannot be flipped over again". Succeeding is
exactly what completes /A Treasure Unearthed/, so the completed Task is the
lock, and it can be read here where 'HasAbilities' (which is pure) could not
reach it.
-}
instance HasModifiersFor TheBritishLibrary where
  getModifiersFor (TheBritishLibrary a) = do
    modifySelf a [ShroudModifier (negate $ locationClues a)]
    stolen <- taskCompleted Treacheries.aTreasureUnearthed
    modifySelect
      a
      (if stolen then Anyone else NoOne)
      [CannotTriggerAbilityMatching (AbilityIs (toSource a) 1)]

{- | "[action] If investigators at The British Library possess at least 3 clues,
as a group: Flip this card over and resolve its text."
-}
instance HasAbilities TheBritishLibrary where
  getAbilities (TheBritishLibrary a) =
    extendRevealed1 a
      $ restricted a 1 (Here <> InvestigatorsAtHaveClues (be a) (AtLeast $ Static 3)) actionAbility

instance RunMessage TheBritishLibrary where
  runMessage msg l@(TheBritishLibrary attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Stories.bookHeist
      pure l
    _ -> TheBritishLibrary <$> liftRunMessage msg attrs
