module Arkham.Location.Cards.APocketInTime
  ( aPocketInTime
  , APocketInTime(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Trait ( Trait (Shattered) )
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype APocketInTime = APocketInTime LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aPocketInTime :: LocationCard APocketInTime
aPocketInTime = location APocketInTime Cards.aPocketInTime 5 (PerPlayer 1)

instance HasAbilities APocketInTime where
  getAbilities (APocketInTime attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ OrWindowMatcher
      [ Moves
        Timing.After
        You
        (NotSource $ SourceIs (AbilitySource (toSource attrs) 2))
        (LocationWithId $ toId attrs)
        (LocationWithTrait Shattered <> LocationWithAnyClues)
      , Moves
        Timing.After
        You
        (NotSource $ SourceIs (AbilitySource (toSource attrs) 2))
        (LocationWithTrait Shattered <> LocationWithAnyClues)
        (LocationWithId $ toId attrs)
      ]
    , restrictedAbility attrs 2 (Here <> ScenarioDeckWithCard ExplorationDeck)
    $ ActionAbility (Just Action.Explore)
    $ ActionCost 1
    ]

getShatteredLocation :: LocationAttrs -> [Window] -> LocationId
getShatteredLocation _ [] = error "wrong window"
getShatteredLocation attrs (Window _ (Window.Moves _ _ (Just lid) _) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs (Window _ (Window.Moves _ _ _ lid) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs (_ : xs) = getShatteredLocation attrs xs

instance RunMessage APocketInTime where
  runMessage msg l@(APocketInTime attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getShatteredLocation attrs -> lid) _
      -> do
        clues <- field LocationClues lid
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny clues 0
        pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push $ Explore
        iid
        (toAbilitySource attrs 2)
        (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure l
    _ -> APocketInTime <$> runMessage msg attrs
