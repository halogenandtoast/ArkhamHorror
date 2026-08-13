module Arkham.Homebrew.DarkMatter.Locations.TheCassilda (theCassilda) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (placementL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

{- | Starfall's [[Starship]] location for the cultists' ship:

"The Cassilda is connected to attached location and vice versa. Moving to or from
The Cassilda does not cost an action ([free]).
[free] Spend 2 resources: Attach The Cassilda to any location. Investigators at
any location may activate this ability. (Limit once per investigator per round.)"

Attachment is 'locationPlacement': the starship's own placement is set to
'AttachedToLocation', and the mutual connection is derived from it.
-}
newtype TheCassilda = TheCassilda LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theCassilda :: LocationCard TheCassilda
theCassilda = location TheCassilda Cards.theCassilda 2 (Static 0)

-- | The location this starship is currently attached to, if any.
attachedTo :: LocationAttrs -> Maybe LocationId
attachedTo a = case locationPlacement a of
  Just (AttachedToLocation lid) -> Just lid
  _ -> Nothing

instance HasModifiersFor TheCassilda where
  getModifiersFor (TheCassilda a) = for_ (attachedTo a) \lid -> do
    modifySelf a [ConnectedToWhen (be a) (LocationWithId lid), AdditionalCostToLeave Free]
    modifySelect a (LocationWithId lid) [ConnectedToWhen (LocationWithId lid) (be a)]

instance HasAbilities TheCassilda where
  getAbilities (TheCassilda a) =
    extendRevealed1 a
      $ limitedAbility (PlayerLimit PerRound 1)
      $ restricted a 1 NoRestriction
      $ FastAbility (ResourceCost 2)

instance RunMessage TheCassilda where
  runMessage msg l@(TheCassilda attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ not_ (LocationWithId attrs.id)
      chooseHandleTargetM iid (attrs.ability 1) locations
      pure l
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pure . TheCassilda $ attrs & placementL ?~ AttachedToLocation lid
    _ -> TheCassilda <$> liftRunMessage msg attrs
