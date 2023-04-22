module Arkham.Investigator.Cards.JacquelineFine
  ( jacquelineFine
  , JacquelineFine(..)
  )
where

import Arkham.Prelude

import Arkham.Helpers.Window
import Arkham.ChaosBagStepState
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Ability
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window qualified as Window
import Arkham.Window (Window(..))

newtype JacquelineFine = JacquelineFine InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jacquelineFine :: InvestigatorCard JacquelineFine
jacquelineFine = investigator
  JacquelineFine
  Cards.jacquelineFine
  Stats { health = 6 , sanity = 9 , willpower = 5 , intellect = 3 , combat = 2 , agility = 2 }

instance HasAbilities JacquelineFine where
  getAbilities (JacquelineFine a) =
    [ limitedAbility (PlayerLimit PerRound 1)
      $ restrictedAbility a 1 Self
      $ ReactionAbility (WouldRevealChaosToken Timing.When $ InvestigatorAt YourLocation)
      $ Free
    ]

instance HasTokenValue JacquelineFine where
  getTokenValue iid ElderSign (JacquelineFine attrs) | iid == toId attrs = do
    pure $ TokenValue ElderSign NoModifier
  getTokenValue _ token _ = pure $ TokenValue token mempty

-- TODO: ChooseTokenMatch should have matchers that check the token results
-- and then prompt the user to choose an option rather than having the bag
-- handle the logic, this should work without changing behavior too much
instance RunMessage JacquelineFine where
  runMessage msg i@(JacquelineFine attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 [Window Timing.When (Window.WouldRevealChaosToken drawSource _)] _ -> do
      ignoreWindow <- checkWindows [Window Timing.After (Window.CancelledOrIgnoredCardOrGameEffect (toSource attrs))]
      pushAll
        [ ReplaceCurrentDraw drawSource iid
          $ ChooseMatchChoice
            [Undecided Draw, Undecided Draw, Undecided Draw]
            []
            [ (TokenFaceIs AutoFail , ("Cancel 1 {autofail} token", ChooseMatch (toSource attrs) 1 CancelChoice [] [] (TokenFaceIs AutoFail)))
            , (TokenFaceIsNot AutoFail, ("Cancel 2 non-{autofail} tokens", ChooseMatch (toSource attrs) 2 CancelChoice [] [] (TokenFaceIsNot AutoFail)))
            ]
          -- $ Choose 1 [Undecided Draw, Undecided Draw, Undecided Draw] []
        , ignoreWindow
        ]
      pure i
    TokenCanceled iid _ (tokenFace -> ElderSign) | iid == toId attrs -> do
      drawing <- drawCards (toId attrs) (toSource attrs) 1
      push drawing
      JacquelineFine <$> runMessage msg attrs
    TokenIgnored iid _ (tokenFace -> ElderSign) | iid == toId attrs -> do
      drawing <- drawCards (toId attrs) (toSource attrs) 1
      push drawing
      JacquelineFine <$> runMessage msg attrs
    _ -> JacquelineFine <$> runMessage msg attrs
