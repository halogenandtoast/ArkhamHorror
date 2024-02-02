module Arkham.Location.Cards.APocketInTime (aPocketInTime, APocketInTime (..)) where

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.GameValue
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Shattered))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype APocketInTime = APocketInTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

aPocketInTime :: LocationCard APocketInTime
aPocketInTime = location APocketInTime Cards.aPocketInTime 5 (PerPlayer 1)

instance HasAbilities APocketInTime where
  getAbilities (APocketInTime attrs) =
    withRevealedAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ OrWindowMatcher
            [ Moves
                #after
                You
                (NotSource $ SourceIs (AbilitySource (toSource attrs) 2))
                (LocationWithId $ toId attrs)
                (LocationWithTrait Shattered <> LocationWithAnyClues)
            , Moves
                #after
                You
                (NotSource $ SourceIs (AbilitySource (toSource attrs) 2))
                (LocationWithTrait Shattered <> LocationWithAnyClues)
                (LocationWithId $ toId attrs)
            ]
      , restrictedAbility attrs 2 Here exploreAction_
      ]

getShatteredLocation :: LocationAttrs -> [Window] -> LocationId
getShatteredLocation _ [] = error "wrong window"
getShatteredLocation attrs ((windowType -> Window.Moves _ _ (Just lid) _) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs ((windowType -> Window.Moves _ _ _ lid) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs (_ : xs) = getShatteredLocation attrs xs

instance RunMessage APocketInTime where
  runMessage msg l@(APocketInTime attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getShatteredLocation attrs -> lid) _ -> do
      clues <- field LocationClues lid
      push $ assignDamage iid (toAbilitySource attrs 1) clues
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 2
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure l
    _ -> APocketInTime <$> runMessage msg attrs
