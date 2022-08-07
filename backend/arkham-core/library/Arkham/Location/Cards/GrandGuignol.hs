module Arkham.Location.Cards.GrandGuignol
  ( grandGuignol
  , GrandGuignol(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Timing qualified as Timing

newtype GrandGuignol = GrandGuignol LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grandGuignol :: LocationCard GrandGuignol
grandGuignol = location GrandGuignol Cards.grandGuignol 3 (PerPlayer 1)

instance HasAbilities GrandGuignol where
  getAbilities (GrandGuignol attrs) = withBaseAbilities
    attrs
    [ restrictedAbility attrs 1 Here
      $ ForcedAbility
      $ RevealLocation Timing.After You
      $ LocationWithId
      $ toId attrs
    | locationRevealed attrs
    ]

instance RunMessage GrandGuignol where
  runMessage msg a@(GrandGuignol attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      nonWeaknessCards <- selectListMap
        toCardId
        (BasicCardMatch NonWeakness <> InHandOf (InvestigatorWithId iid))
      push
        $ chooseOrRunOne iid
        $ Label
            "Take 2 Horror"
            [InvestigatorAssignDamage iid source DamageAny 0 2]
        : [ Label
              "Shuffle all non-weakness cards from your hand into your deck, then draw an equal number of cards"
              (map (DiscardCard iid) nonWeaknessCards
              <> [DrawCards iid (length nonWeaknessCards) False]
              )
          | not (null nonWeaknessCards)
          ]
      pure a
    _ -> GrandGuignol <$> runMessage msg attrs
