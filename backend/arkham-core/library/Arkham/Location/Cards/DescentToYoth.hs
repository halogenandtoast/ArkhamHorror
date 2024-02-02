module Arkham.Location.Cards.DescentToYoth (
  descentToYoth,
  DescentToYoth (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Helpers.Query
import Arkham.Helpers.Window
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype Metadata = Metadata {flipDoom :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, NoThunks, NFData)

newtype DescentToYoth = DescentToYoth (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

descentToYoth :: LocationCard DescentToYoth
descentToYoth =
  location
    (DescentToYoth . (`with` Metadata False))
    Cards.descentToYoth
    3
    (Static 0)

instance HasAbilities DescentToYoth where
  getAbilities (DescentToYoth (attrs `With` _)) =
    withBaseAbilities
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
            ( SkillTestResult
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
      investigators <- getInvestigatorPlayers
      pushAll
        [ chooseOne
          player
          [ Label
              "Place 1 doom on Descent to Yoth"
              [PlaceDoom (toAbilitySource attrs 1) (toTarget attrs) 1]
          , Label
              "Draw the top 2 cards of the encounter deck"
              [ InvestigatorDrawEncounterCard iid
              , InvestigatorDrawEncounterCard iid
              ]
          ]
        | (iid, player) <- investigators
        ]
      pure l
    UseCardAbility _iid (isSource attrs -> True) 2 _ _ ->
      pure $ DescentToYoth $ attrs `with` Metadata True
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _
      | flipDoom metadata -> do
          let lid = toId attrs
          whenWindowMsg <-
            checkWindows
              [mkWindow Timing.When (Window.SuccessfulInvestigation iid lid)]
          afterWindowMsg <-
            checkWindows
              [mkWindow Timing.After (Window.SuccessfulInvestigation iid lid)]
          pushAll [whenWindowMsg, FlipDoom (toTarget attrs) 1, afterWindowMsg]
          pure $ DescentToYoth $ attrs `with` Metadata False
    _ -> DescentToYoth . (`with` metadata) <$> runMessage msg attrs
