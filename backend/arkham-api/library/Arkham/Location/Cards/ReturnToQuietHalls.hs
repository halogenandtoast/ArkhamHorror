module Arkham.Location.Cards.ReturnToQuietHalls (returnToQuietHalls) where

import Arkham.Ability
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Trait

newtype ReturnToQuietHalls = ReturnToQuietHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToQuietHalls :: LocationCard ReturnToQuietHalls
returnToQuietHalls =
  locationWith
    ReturnToQuietHalls
    Cards.returnToQuietHalls
    3
    (Static 0)
    ( (connectedMatchersL <>~ [LocationWithTrait Basement])
        . (revealedConnectedMatchersL <>~ [LocationWithTrait Basement])
    )

instance HasModifiersFor ReturnToQuietHalls where
  getModifiersFor (ReturnToQuietHalls attrs) = do
    modifySelect
      attrs
      (locationIs Cards.entryHall)
      [ConnectedToWhen (locationIs Cards.entryHall) (LocationWithId attrs.id)]

instance HasAbilities ReturnToQuietHalls where
  getAbilities (ReturnToQuietHalls a) =
    extendRevealed1 a
      $ restricted
        a
        1
        (Here <> notExists UnrevealedLocation <> notExists LocationWithAnyClues)
        actionAbility

instance RunMessage ReturnToQuietHalls where
  runMessage msg l@(ReturnToQuietHalls attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeClues (attrs.ability 1) attrs =<< perPlayer 1
      pure l
    _ -> ReturnToQuietHalls <$> liftRunMessage msg attrs
