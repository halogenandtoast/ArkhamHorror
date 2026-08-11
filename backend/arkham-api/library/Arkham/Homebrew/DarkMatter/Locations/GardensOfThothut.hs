module Arkham.Homebrew.DarkMatter.Locations.GardensOfThothut (gardensOfThothut) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Story (readStory)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.DarkMatter.Helpers (flipToOtherSide)
import Arkham.Homebrew.DarkMatter.Traits (pattern Carcosa)
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype GardensOfThothut = GardensOfThothut LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | The [[Carcosa]] face of Ice Cavity.
gardensOfThothut :: LocationCard GardensOfThothut
gardensOfThothut =
  locationWith GardensOfThothut Cards.gardensOfThothut 3 (PerPlayer 1) (canBeFlippedL .~ True)

-- | "Gardens of Thothut gets +1 shroud for each connecting [[Carcosa]] location."
instance HasModifiersFor GardensOfThothut where
  getModifiersFor (GardensOfThothut a) = do
    n <- selectCount $ connectedTo (be a) <> LocationWithTrait Carcosa
    modifySelf a [ShroudModifier n]

{- | "{fast} If this location is the only [[Carcosa]] location in play: Read the
set aside "Delights" story card. (Max once per game.)"
-}
instance HasAbilities GardensOfThothut where
  getAbilities (GardensOfThothut a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted a 1 (Here <> not_ (exists $ LocationWithTrait Carcosa <> not_ (be a)))
      $ freeReaction AnyWindow

instance RunMessage GardensOfThothut where
  runMessage msg l@(GardensOfThothut attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readStory iid attrs.id Stories.delights
      pure l
    Flip _ _ (isTarget attrs -> True) -> do
      flipToOtherSide attrs
      pure l
    _ -> GardensOfThothut <$> liftRunMessage msg attrs
