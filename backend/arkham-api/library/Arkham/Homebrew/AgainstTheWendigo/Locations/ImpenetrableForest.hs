module Arkham.Homebrew.AgainstTheWendigo.Locations.ImpenetrableForest (impenetrableForest) where

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

newtype ImpenetrableForest = ImpenetrableForest LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

impenetrableForest :: LocationCard ImpenetrableForest
impenetrableForest = locationWith ImpenetrableForest Cards.impenetrableForest 4 (PerPlayer 2) connectsEastOrWest

instance HasModifiersFor ImpenetrableForest where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (ImpenetrableForest a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance HasAbilities ImpenetrableForest where
  getAbilities (ImpenetrableForest a) = extendRevealed1 a $ fateReaction a [Stories.sylviasFateV1, Stories.sylviasFateV2]

instance RunMessage ImpenetrableForest where
  runMessage msg l@(ImpenetrableForest attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readSecondPartOfFate iid [Stories.sylviasFateV1, Stories.sylviasFateV2]
      pure l
    _ -> ImpenetrableForest <$> liftRunMessage msg attrs
