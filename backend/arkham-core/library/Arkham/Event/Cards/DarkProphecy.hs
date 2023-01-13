module Arkham.Event.Cards.DarkProphecy
  ( darkProphecy
  , DarkProphecy(..)
  ) where

import Arkham.Prelude

import Arkham.ChaosBagStepState
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Window ( Window (..) )
import Arkham.Window qualified as Window

newtype DarkProphecy = DarkProphecy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkProphecy :: EventCard DarkProphecy
darkProphecy = event DarkProphecy Cards.darkProphecy

instance RunMessage DarkProphecy where
  runMessage msg e@(DarkProphecy attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [Window Timing.When (Window.WouldRevealChaosToken drawSource _)] _
      | eid == toId attrs
      -> do
        ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect $ toSource attrs)]
        pushAll
          [ ReplaceCurrentDraw drawSource iid $ ChooseMatch
            1
            (replicate 5 $ Undecided Draw)
            []
            (TokenMatchesAny
            $ map TokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail]
            )
          , ignoreWindow
          , Discard (toTarget attrs)
          ]
        pure e
    _ -> DarkProphecy <$> runMessage msg attrs
