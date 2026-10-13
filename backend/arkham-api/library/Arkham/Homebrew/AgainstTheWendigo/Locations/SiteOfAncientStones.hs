module Arkham.Homebrew.AgainstTheWendigo.Locations.SiteOfAncientStones (siteOfAncientStones) where

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

newtype SiteOfAncientStones = SiteOfAncientStones LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

siteOfAncientStones :: LocationCard SiteOfAncientStones
siteOfAncientStones = locationWith SiteOfAncientStones Cards.siteOfAncientStones 4 (PerPlayer 2) connectsEastOrWest

instance HasModifiersFor SiteOfAncientStones where
  -- The unrevealed side prints "You cannot move into this location."
  getModifiersFor (SiteOfAncientStones a) =
    modifySelect a Anyone [CannotEnter a.id | not a.revealed]

instance HasAbilities SiteOfAncientStones where
  getAbilities (SiteOfAncientStones a) = extendRevealed1 a $ fateReaction a [Stories.bernardsFateV1, Stories.bernardsFateV2]

instance RunMessage SiteOfAncientStones where
  runMessage msg l@(SiteOfAncientStones attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      readSecondPartOfFate iid [Stories.bernardsFateV1, Stories.bernardsFateV2]
      pure l
    _ -> SiteOfAncientStones <$> liftRunMessage msg attrs
