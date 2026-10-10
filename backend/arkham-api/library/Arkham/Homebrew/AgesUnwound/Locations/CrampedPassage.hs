module Arkham.Homebrew.AgesUnwound.Locations.CrampedPassage (crampedPassage) where

import Arkham.Ability
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelfWhen)
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Token qualified as Token

newtype CrampedPassage = CrampedPassage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | @clues_fixed@ 0: the clues on it are the ones its own Forced places.
crampedPassage :: LocationCard CrampedPassage
crampedPassage = symbolLabel $ location CrampedPassage Cards.crampedPassage 1 (Static 0)

{- | "No more than one investigator may be in Cramped Passage at a time."

'Blocked' while occupied, which is how the Chamber of Regret prints the same
sentence.
-}
instance HasModifiersFor CrampedPassage where
  getModifiersFor (CrampedPassage a) = do
    occupied <- selectAny $ investigatorAt a.id
    modifySelfWhen a occupied [Blocked]

{- | "Forced - After you enter Cramped Passage: Place 1 clue on Cramped Passage
(from the token pool)."
-}
instance HasAbilities CrampedPassage where
  getAbilities (CrampedPassage a) =
    extendRevealed1 a $ forcedAbility a 1 $ Enters #after You (be a)

instance RunMessage CrampedPassage where
  runMessage msg l@(CrampedPassage attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeTokens (attrs.ability 1) attrs Token.Clue 1
      pure l
    _ -> CrampedPassage <$> liftRunMessage msg attrs
