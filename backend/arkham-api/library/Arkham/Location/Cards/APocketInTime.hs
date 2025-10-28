module Arkham.Location.Cards.APocketInTime (aPocketInTime) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait (Trait (Shattered))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype APocketInTime = APocketInTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aPocketInTime :: LocationCard APocketInTime
aPocketInTime = location APocketInTime Cards.aPocketInTime 5 (PerPlayer 1)

instance HasAbilities APocketInTime where
  getAbilities (APocketInTime a) =
    extendRevealed
      a
      [ mkAbility a 1
          $ forced
          $ oneOf
            [ Moves #after You (NotSource $ SourceIs (a.ability 2)) (be a) match
            , Moves #after You (NotSource $ SourceIs (a.ability 2)) match (be a)
            ]
      , restricted a 2 Here exploreAction_
      ]
   where
    match = LocationWithTrait Shattered <> LocationWithAnyClues

getShatteredLocation :: LocationAttrs -> [Window] -> LocationId
getShatteredLocation _ [] = error "wrong window"
getShatteredLocation attrs ((windowType -> Window.Moves _ _ (Just lid) _) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs ((windowType -> Window.Moves _ _ _ lid) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs (_ : xs) = getShatteredLocation attrs xs

instance RunMessage APocketInTime where
  runMessage msg l@(APocketInTime attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getShatteredLocation attrs -> lid) _ -> do
      clues <- field LocationClues lid
      assignDamage iid (toAbilitySource attrs 1) clues
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      runExplore iid (attrs.ability 2)
      pure l
    _ -> APocketInTime <$> liftRunMessage msg attrs
