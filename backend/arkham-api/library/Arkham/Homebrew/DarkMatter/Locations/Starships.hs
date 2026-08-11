module Arkham.Homebrew.DarkMatter.Locations.Starships (theCassilda, theTatterdemalionLocation) where

import Arkham.Ability
import Arkham.Card (toCardCode)
import Arkham.Card.CardDef (CardDef, toCardDef)
import Arkham.GameValue
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect, modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Locations qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (placementL)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

{- | Starfall's two [[Starship]] locations share a shape:

"X is connected to attached location and vice versa. Moving to or from X does not
cost an action ([free])."

and differ only in how you re-attach them:

* The Tatterdemalion — "[action]: Attach The Tatterdemalion to any location."
* The Cassilda — "[free] Spend 2 resources: Attach The Cassilda to any location."

Both add "Investigators at any location may activate this ability. (Limit once
per investigator per round.)"

Attachment is 'locationPlacement': the starship's own placement is set to
'AttachedToLocation', and the mutual connection is derived from it.
-}
newtype Starship = Starship LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTatterdemalionLocation :: LocationCard Starship
theTatterdemalionLocation = location Starship Cards.theTatterdemalion 2 (Static 0)

theCassilda :: LocationCard Starship
theCassilda = location Starship Cards.theCassilda 2 (Static 0)

isCassilda :: LocationAttrs -> Bool
isCassilda a = toCardCode (toCardDef a) == toCardCode (Cards.theCassilda :: CardDef)

-- | The location this starship is currently attached to, if any.
attachedTo :: LocationAttrs -> Maybe LocationId
attachedTo a = case locationPlacement a of
  Just (AttachedToLocation lid) -> Just lid
  _ -> Nothing

{- | "…is connected to attached location and vice versa. Moving to or from X does
not cost an action."
-}
instance HasModifiersFor Starship where
  getModifiersFor (Starship a) = for_ (attachedTo a) \lid -> do
    modifySelf a [ConnectedToWhen (be a) (LocationWithId lid), AdditionalCostToLeave Free]
    modifySelect a (LocationWithId lid) [ConnectedToWhen (LocationWithId lid) (be a)]

instance HasAbilities Starship where
  getAbilities (Starship a) =
    extendRevealed1 a
      $ limitedAbility (PlayerLimit PerRound 1)
      $ restricted a 1 NoRestriction
      $ if isCassilda a then FastAbility (ResourceCost 2) else actionAbility

instance RunMessage Starship where
  runMessage msg l@(Starship attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locations <- select $ not_ (LocationWithId attrs.id)
      chooseHandleTargetM iid (attrs.ability 1) locations
      pure l
    HandleTargetChoice _ (isAbilitySource attrs 1 -> True) (LocationTarget lid) -> do
      pure . Starship $ attrs & placementL ?~ AttachedToLocation lid
    _ -> Starship <$> liftRunMessage msg attrs
