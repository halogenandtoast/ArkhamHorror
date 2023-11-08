module Arkham.Location.Cards.NexusOfNKai (
  nexusOfNKai,
  NexusOfNKai (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Id
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait (Otherworld))
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype NexusOfNKai = NexusOfNKai LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nexusOfNKai :: LocationCard NexusOfNKai
nexusOfNKai = location NexusOfNKai Cards.nexusOfNKai 4 (PerPlayer 1)

instance HasAbilities NexusOfNKai where
  getAbilities (NexusOfNKai attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ OrWindowMatcher
            [ Moves
                Timing.After
                You
                (NotSource $ SourceIs (AbilitySource (toSource attrs) 2))
                (LocationWithId $ toId attrs)
                (LocationWithTrait Otherworld <> LocationWithAnyClues)
            , Moves
                Timing.After
                You
                (NotSource $ SourceIs (AbilitySource (toSource attrs) 2))
                (LocationWithTrait Otherworld <> LocationWithAnyClues)
                (LocationWithId $ toId attrs)
            ]
      , restrictedAbility attrs 2 (Here <> ScenarioDeckWithCard ExplorationDeck)
          $ ActionAbility [Action.Explore]
          $ ActionCost 1
      ]

getShatteredLocation :: LocationAttrs -> [Window] -> LocationId
getShatteredLocation _ [] = error "wrong window"
getShatteredLocation attrs ((windowType -> Window.Moves _ _ (Just lid) _) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs ((windowType -> Window.Moves _ _ _ lid) : _)
  | lid /= toId attrs = lid
getShatteredLocation attrs (_ : xs) = getShatteredLocation attrs xs

instance RunMessage NexusOfNKai where
  runMessage msg l@(NexusOfNKai attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getShatteredLocation attrs -> lid) _ ->
      do
        clues <- field LocationClues lid
        push $ InvestigatorAssignDamage iid (toSource attrs) DamageAny 0 clues
        pure l
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      push
        $ Explore
          iid
          (toAbilitySource attrs 2)
          (CardWithOneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure l
    _ -> NexusOfNKai <$> runMessage msg attrs
