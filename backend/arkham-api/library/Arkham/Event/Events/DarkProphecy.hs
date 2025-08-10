module Arkham.Event.Events.DarkProphecy (darkProphecy, DarkProphecy (..)) where

import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Window
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Taboo
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window

newtype DarkProphecy = DarkProphecy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkProphecy :: EventCard DarkProphecy
darkProphecy = event DarkProphecy Cards.darkProphecy

instance RunMessage DarkProphecy where
  runMessage msg e@(DarkProphecy attrs) = case msg of
    InvestigatorPlayEvent iid eid _ [Window Timing.When (Window.WouldRevealChaosToken drawSource _) _] _
      | eid == toId attrs -> do
          ignoreWindow <-
            checkWindows
              [mkWindow Timing.After (Window.CancelledOrIgnoredCardOrGameEffect (toSource attrs) Nothing)]
          pushAll
            [ ReplaceCurrentDraw drawSource iid
                $ ChooseMatch
                  (toSource attrs)
                  1
                  ResolveChoice
                  (replicate 5 $ Undecided Draw)
                  []
                  ( flip ChaosTokenMatchesOrElse AnyChaosToken
                      $ if tabooed TabooList20 attrs
                        then not_ (ChaosTokenFaceIs ElderSign) <> IsSymbol
                        else ChaosTokenMatchesAny (map ChaosTokenFaceIs [Skull, Cultist, Tablet, ElderThing, AutoFail])
                  )
                  Nothing
            , ignoreWindow
            ]
          pure e
    _ -> DarkProphecy <$> runMessage msg attrs
