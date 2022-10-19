module Arkham.Location.Cards.DescentToYoth
  ( descentToYoth
  , DescentToYoth(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

newtype DescentToYoth = DescentToYoth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentToYoth :: LocationCard DescentToYoth
descentToYoth = location DescentToYoth Cards.descentToYoth 3 (Static 0)

instance HasAbilities DescentToYoth where
  getAbilities (DescentToYoth attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
    $ ForcedAbility
    $ PutLocationIntoPlay Timing.After Anyone
    $ LocationWithId
    $ toId attrs
    , restrictedAbility
        attrs
        2
        (LocationExists $ LocationWithId (toId attrs) <> LocationWithAnyDoom)
      $ ReactionAbility
          (SkillTestResult
              Timing.After
              You
              (WhileInvestigating $ LocationWithId $ toId attrs)
          $ SuccessResult AnyValue
          )
          Free
    ]

instance RunMessage DescentToYoth where
  runMessage msg l@(DescentToYoth attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      iids <- getInvestigatorIds
      pushAll
        [ chooseOne
            iid
            [ Label
              "Place 1 doom on Descent to Yoth"
              [PlaceDoom (toTarget attrs) 1]
            , Label
              "Draw the top 2 cards of the encounter deck"
              [ InvestigatorDrawEncounterCard iid
              , InvestigatorDrawEncounterCard iid
              ]
            ]
        | iid <- iids
        ]
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 _ _ -> do
      error "need to figure out"
      pure l
    _ -> DescentToYoth <$> runMessage msg attrs
