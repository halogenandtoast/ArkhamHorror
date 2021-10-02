module Arkham.Types.Event.Cards.UncageTheSoul
  ( uncageTheSoul
  , UncageTheSoul(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Game.Helpers
import Arkham.Types.Matcher hiding (PlayCard)
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Trait
import Arkham.Types.Window (Window(..))
import Arkham.Types.Window qualified as Window

newtype UncageTheSoul = UncageTheSoul EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uncageTheSoul :: EventCard UncageTheSoul
uncageTheSoul = event UncageTheSoul Cards.uncageTheSoul

instance CanCheckPlayable env => RunMessage env UncageTheSoul where
  runMessage msg e@(UncageTheSoul attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      let
        windows' = map
          (Window Timing.When)
          [Window.DuringTurn iid, Window.NonFast, Window.FastPlayerWindow]
      availableResources <- unResourceCount <$> getCount iid
      results <- selectList
        (InHandOf You <> BasicCardMatch
          (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual])
        )
      cards <- filterM
        (getIsPlayableWithResources
          iid
          (InvestigatorSource iid)
          (availableResources + 3)
          windows'
        )
        results
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (CardIdTarget $ toCardId c)
              [ CreateEffect
                (toCardCode attrs)
                Nothing
                (toSource attrs)
                (CardIdTarget $ toCardId c)
              , PayCardCost iid (toCardId c)
              , PlayCard iid (toCardId c) Nothing False
              ]
          | c <- cards
          ]
        , Discard (toTarget attrs)
        ]
    _ -> UncageTheSoul <$> runMessage msg attrs
