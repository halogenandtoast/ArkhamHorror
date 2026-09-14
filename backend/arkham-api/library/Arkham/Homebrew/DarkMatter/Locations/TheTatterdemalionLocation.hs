module Arkham.Homebrew.DarkMatter.Locations.TheTatterdemalionLocation (
  theTatterdemalionLocation,
) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (starshipAttachment, starshipDockTargets)
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationLabel), placementL)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

{- | Starfall's [[Starship]] location for the investigators' own ship:

"The Tatterdemalion is connected to attached location and vice versa. Moving to
or from The Tatterdemalion does not cost an action ([free]).
[action]: Attach The Tatterdemalion to any location. Investigators at any
location may activate this ability. (Limit once per investigator per round.)"

Attachment is 'locationPlacement': the starship's own placement is set to
'AttachedToLocation', and both the mutual connection and the free move are
derived from it by 'starshipAttachment'.

The name carries the @Location@ suffix because the campaign and its first
scenario are also called "The Tatterdemalion".
-}
newtype TheTatterdemalionLocation = TheTatterdemalionLocation LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatterdemalionLocation :: LocationCard TheTatterdemalionLocation
theTatterdemalionLocation =
  location TheTatterdemalionLocation Cards.theTatterdemalion 2 (Static 0)

instance HasModifiersFor TheTatterdemalionLocation where
  getModifiersFor (TheTatterdemalionLocation a) = starshipAttachment a

instance HasAbilities TheTatterdemalionLocation where
  getAbilities (TheTatterdemalionLocation a) =
    extendRevealed1 a
      $ limitedAbility (PlayerLimit PerRound 1)
      $ restricted a 1 NoRestriction actionAbility

instance RunMessage TheTatterdemalionLocation where
  runMessage msg l@(TheTatterdemalionLocation attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select starshipDockTargets
      chooseHandleTargetM iid (attrs.ability 1) locations
      pure l
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      -- Starfall's grid reserves an "l<host>"/"r<host>" berth column on either
      -- side of every location; this ship always docks in the left one.
      host <- field LocationLabel lid
      push $ SetLocationLabel attrs.id ("l" <> host)
      pure . TheTatterdemalionLocation $ attrs & placementL ?~ AttachedToLocation lid
    _ -> TheTatterdemalionLocation <$> liftRunMessage msg attrs
