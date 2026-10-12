module Arkham.Homebrew.AgainstTheWendigo.Locations.Swamp (swamp) where

import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.AgainstTheWendigo.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.AgainstTheWendigo.Helpers (
  connectsEastOrWest,
  fateReaction,
  readSecondPartOfFate,
 )
import Arkham.Location.Import.Lifted
import Arkham.Matcher

newtype Swamp = Swamp LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

swamp :: LocationCard Swamp
swamp = locationWith Swamp Cards.swamp 3 (PerPlayer 2) connectsEastOrWest

instance HasModifiersFor Swamp where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (Swamp a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance HasAbilities Swamp where
  getAbilities (Swamp a) = extendRevealed1 a $ fateReaction a [Stories.normansFateV1, Stories.normansFateV2]

instance RunMessage Swamp where
  runMessage msg l@(Swamp attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readSecondPartOfFate iid [Stories.normansFateV1, Stories.normansFateV2]
      pure l
    _ -> Swamp <$> liftRunMessage msg attrs
