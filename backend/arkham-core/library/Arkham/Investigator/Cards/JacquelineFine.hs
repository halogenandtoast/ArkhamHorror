module Arkham.Investigator.Cards.JacquelineFine (
  jacquelineFine,
  JacquelineFine (..),
)
where

import Arkham.Prelude

import Arkham.ChaosBagStepState
import Arkham.Helpers.Window
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Window (Window (..), mkWindow)
import Arkham.Window qualified as Window

newtype JacquelineFine = JacquelineFine InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jacquelineFine :: InvestigatorCard JacquelineFine
jacquelineFine =
  investigator JacquelineFine Cards.jacquelineFine
    $ Stats {health = 6, sanity = 9, willpower = 5, intellect = 3, combat = 2, agility = 2}

instance HasAbilities JacquelineFine where
  getAbilities (JacquelineFine a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 Self
        $ freeReaction (WouldRevealChaosToken #when $ affectsOthers $ InvestigatorAt YourLocation)
    ]

instance HasChaosTokenValue JacquelineFine where
  getChaosTokenValue iid ElderSign (JacquelineFine attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign NoModifier
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

getDrawSource :: [Window] -> Source
getDrawSource [] = error "No draw source"
getDrawSource ((windowType -> Window.WouldRevealChaosToken drawSource _) : _) = drawSource
getDrawSource (_ : rest) = getDrawSource rest

-- TODO: ChooseTokenMatch should have matchers that check the token results
-- and then prompt the user to choose an option rather than having the bag
-- handle the logic, this should work without changing behavior too much
instance RunMessage JacquelineFine where
  runMessage msg i@(JacquelineFine attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      ignoreWindow <-
        checkWindows [mkWindow #after (Window.CancelledOrIgnoredCardOrGameEffect (toSource attrs))]
      pushAll
        [ ReplaceCurrentDraw drawSource iid
            $ ChooseMatchChoice
              [Undecided Draw, Undecided Draw, Undecided Draw]
              []
              [
                ( ChaosTokenFaceIs AutoFail
                ,
                  ( "Cancel 1 {autofail} token"
                  , ChooseMatch (toAbilitySource attrs 1) 1 CancelChoice [] [] (ChaosTokenFaceIs AutoFail)
                  )
                )
              ,
                ( ChaosTokenFaceIsNot AutoFail
                ,
                  ( "Cancel 2 non-{autofail} tokens"
                  , ChooseMatch (toAbilitySource attrs 1) 2 CancelChoice [] [] (ChaosTokenFaceIsNot AutoFail)
                  )
                )
              ]
        , -- \$ Choose 1 [Undecided Draw, Undecided Draw, Undecided Draw] []
          ignoreWindow
        ]
      pure i
    ChaosTokenCanceled iid _ (chaosTokenFace -> ElderSign) | attrs `is` iid -> do
      pushM $ drawCards (toId attrs) (toAbilitySource attrs 1) 1
      JacquelineFine <$> runMessage msg attrs
    ChaosTokenIgnored iid _ (chaosTokenFace -> ElderSign) | attrs `is` iid -> do
      pushM $ drawCards (toId attrs) (toAbilitySource attrs 1) 1
      JacquelineFine <$> runMessage msg attrs
    _ -> JacquelineFine <$> runMessage msg attrs
