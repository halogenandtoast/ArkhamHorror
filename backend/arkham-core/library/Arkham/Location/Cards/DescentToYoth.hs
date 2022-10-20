module Arkham.Location.Cards.DescentToYoth
  ( descentToYoth
  , DescentToYoth(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype Metadata = Metadata { flipDoom :: Bool }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype DescentToYoth = DescentToYoth (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descentToYoth :: LocationCard DescentToYoth
descentToYoth = location
  (DescentToYoth . (`with` Metadata False))
  Cards.descentToYoth
  3
  (Static 0)

instance HasAbilities DescentToYoth where
  getAbilities (DescentToYoth (attrs `With` _)) = withBaseAbilities
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
              Timing.When
              You
              (WhileInvestigating $ LocationWithId $ toId attrs)
          $ SuccessResult AnyValue
          )
          Free
    ]

instance RunMessage DescentToYoth where
  runMessage msg l@(DescentToYoth (attrs `With` metadata)) = case msg of
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
    UseCardAbility _iid (isSource attrs -> True) 2 _ _ ->
      pure $ DescentToYoth $ attrs `with` Metadata True
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _
      | flipDoom metadata -> do
        let lid = toId attrs
        whenWindowMsg <- checkWindows
          [Window Timing.When (Window.SuccessfulInvestigation iid lid)]
        afterWindowMsg <- checkWindows
          [Window Timing.After (Window.SuccessfulInvestigation iid lid)]
        pushAll [whenWindowMsg, FlipDoom (toTarget attrs) 1, afterWindowMsg]
        pure $ DescentToYoth $ attrs `with` Metadata False
    _ -> DescentToYoth . (`with` metadata) <$> runMessage msg attrs
