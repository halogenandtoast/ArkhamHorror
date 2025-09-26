module Arkham.Investigator.Cards.JacquelineFine (jacquelineFine) where

import Arkham.Ability
import Arkham.ChaosBagStepState
import Arkham.Helpers.ChaosBag
import Arkham.Helpers.Window
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype JacquelineFine = JacquelineFine InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

jacquelineFine :: InvestigatorCard JacquelineFine
jacquelineFine =
  investigator JacquelineFine Cards.jacquelineFine
    $ Stats {health = 6, sanity = 9, willpower = 5, intellect = 3, combat = 2, agility = 2}

instance HasAbilities JacquelineFine where
  getAbilities (JacquelineFine a) =
    [ playerLimit PerRound
        $ restricted a 1 Self
        $ freeReaction (WouldRevealChaosTokens #when $ affectsOthers $ at_ YourLocation)
    ]

instance HasChaosTokenValue JacquelineFine where
  getChaosTokenValue iid ElderSign (JacquelineFine attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

-- TODO: ChooseTokenMatch should have matchers that check the token results
-- and then prompt the user to choose an option rather than having the bag
-- handle the logic, this should work without changing behavior too much
instance RunMessage JacquelineFine where
  runMessage msg i@(JacquelineFine attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getDrawSource -> drawSource) _ -> do
      mchoice <- getChaosBagChoice
      let steps = maybe [] getSteps mchoice
      let
        nested =
          mchoice >>= \case
            Resolved {} -> Nothing
            Decided s -> guard (s /= Draw) $> s
            Undecided s -> guard (s /= Draw) $> s
            Deciding s -> guard (s /= Draw) $> s

      push
        $ ReplaceCurrentDraw drawSource iid
        $ ChooseMatchChoice
          (steps <> [Undecided Draw, Undecided Draw])
          []
          [
            ( ChaosTokenFaceIs #autofail
            ,
              ( "Cancel 1 {autoFail} token"
              , ChooseMatch (attrs.ability 1) 1 CancelChoice [] [] #autofail nested
              )
            )
          ,
            ( ChaosTokenFaceIsNot #autofail
            ,
              ( "Cancel 2 non-{autoFail} tokens"
              , ChooseMatch (attrs.ability 1) 2 CancelChoice [] [] (ChaosTokenFaceIsNot #autofail) nested
              )
            )
          ]
      cancelledOrIgnoredCardOrGameEffect attrs
      pure i
    ChaosTokenCanceled (is attrs -> True) _ ((.face) -> ElderSign) -> do
      drawCardsIfCan attrs (attrs.ability 1) 1
      JacquelineFine <$> liftRunMessage msg attrs
    ChaosTokenIgnored (is attrs -> True) _ ((.face) -> ElderSign) -> do
      drawCardsIfCan attrs (attrs.ability 1) 1
      JacquelineFine <$> liftRunMessage msg attrs
    _ -> JacquelineFine <$> liftRunMessage msg attrs
