module Arkham.Homebrew.DarkMatter.Locations.TheCassilda (theCassilda) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (starshipAttachment, starshipDockTargets)
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationLabel), placementL)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Projection

{- | Starfall's [[Starship]] location for the cultists' ship:

"The Cassilda is connected to attached location and vice versa. Moving to or from
The Cassilda does not cost an action ([free]).
[free] Spend 2 resources: Attach The Cassilda to any location. Investigators at
any location may activate this ability. (Limit once per investigator per round.)"

Attachment is 'locationPlacement': the starship's own placement is set to
'AttachedToLocation', and both the mutual connection and the free move are
derived from it by 'starshipAttachment'.
-}
newtype TheCassilda = TheCassilda LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCassilda :: LocationCard TheCassilda
theCassilda = location TheCassilda Cards.theCassilda 2 (Static 0)

instance HasModifiersFor TheCassilda where
  getModifiersFor (TheCassilda a) = starshipAttachment a

instance HasAbilities TheCassilda where
  getAbilities (TheCassilda a) =
    extendRevealed1 a
      $ limitedAbility (PlayerLimit PerRound 1)
      $ restricted a 1 NoRestriction
      $ FastAbility (ResourceCost 2)

instance RunMessage TheCassilda where
  runMessage msg l@(TheCassilda attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select starshipDockTargets
      chooseHandleTargetM iid (attrs.ability 1) locations
      pure l
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      -- Starfall's grid reserves an "l<host>"/"r<host>" berth column on either
      -- side of every location; this ship always docks in the right one.
      host <- field LocationLabel lid
      push $ SetLocationLabel attrs.id ("r" <> host)
      pure . TheCassilda $ attrs & placementL ?~ AttachedToLocation lid
    _ -> TheCassilda <$> liftRunMessage msg attrs
