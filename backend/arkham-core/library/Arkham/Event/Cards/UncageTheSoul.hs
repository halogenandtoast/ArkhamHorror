module Arkham.Event.Cards.UncageTheSoul (
  uncageTheSoul,
  UncageTheSoul (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Game.Helpers
import Arkham.Matcher hiding (PlayCard)
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

newtype UncageTheSoul = UncageTheSoul EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncageTheSoul :: EventCard UncageTheSoul
uncageTheSoul = event UncageTheSoul Cards.uncageTheSoul

instance RunMessage UncageTheSoul where
  runMessage msg e@(UncageTheSoul attrs) = case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      let
        windows'' =
          nub $
            windows'
              <> map
                (mkWindow Timing.When)
                [Window.DuringTurn iid, Window.NonFast, Window.FastPlayerWindow]
      availableResources <- getSpendableResources iid
      results <-
        selectList
          ( InHandOf You
              <> BasicCardMatch
                (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual])
          )
      cards <-
        filterM
          ( getIsPlayableWithResources
              iid
              (InvestigatorSource iid)
              (availableResources + 3)
              UnpaidCost
              windows''
          )
          results
      pushAll
        [ chooseOne
            iid
            [ TargetLabel
              (CardIdTarget $ toCardId c)
              [ CreateEffect
                  (toCardCode attrs)
                  Nothing
                  (toSource attrs)
                  (CardIdTarget $ toCardId c)
              , PayCardCost iid c windows''
              ]
            | c <- cards
            ]
        ]
      pure e
    _ -> UncageTheSoul <$> runMessage msg attrs
