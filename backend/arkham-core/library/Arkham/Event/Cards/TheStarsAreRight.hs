module Arkham.Event.Cards.TheStarsAreRight (
  theStarsAreRight,
  TheStarsAreRight (..),
)
where

import Arkham.Prelude

import Arkham.Action.Additional
import Arkham.Capability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype TheStarsAreRight = TheStarsAreRight EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStarsAreRight :: EventCard TheStarsAreRight
theStarsAreRight =
  event TheStarsAreRight Cards.theStarsAreRight

instance RunMessage TheStarsAreRight where
  runMessage msg e@(TheStarsAreRight attrs) = case msg of
    Revelation iid source | isSource attrs source -> do
      player <- getPlayer iid
      investigators <- select $ affectsOthers UneliminatedInvestigator
      iid' <- getActiveInvestigatorId
      investigatorsWithChoice <- for investigators $ \investigator -> do
        canDraw <- can.draw.cards investigator
        canGainResources <- can.gain.resources investigator
        let drawing = drawCards investigator (toSource attrs) 1
        pure
          ( investigator
          , [drawing | canDraw]
              <> [takeResources investigator (toSource attrs) 1 | canGainResources]
              <> [SetActiveInvestigator iid | iid /= iid']
              <> [ turnModifier attrs iid
                    $ GiveAdditionalAction
                    $ AdditionalAction "The Stars Are Right" (toSource attrs) #any
                 , PlayerWindow iid [] False
                 ]
              <> [SetActiveInvestigator iid' | iid /= iid']
          )

      pushAll
        [ RemoveEvent (toId attrs)
        , chooseOrRunOne
            player
            [targetLabel investigator choices | (investigator, choices) <- investigatorsWithChoice]
        ]
      pure e
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> error "Unplayable"
    _ -> TheStarsAreRight <$> runMessage msg attrs
