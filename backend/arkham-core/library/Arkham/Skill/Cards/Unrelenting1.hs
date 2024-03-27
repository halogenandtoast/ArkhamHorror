module Arkham.Skill.Cards.Unrelenting1 (
  unrelenting1,
  Unrelenting1 (..),
)
where

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Helpers.ChaosBag
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Unrelenting1 = Unrelenting1 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unrelenting1 :: SkillCard Unrelenting1
unrelenting1 =
  skill Unrelenting1 Cards.unrelenting1

instance RunMessage Unrelenting1 where
  runMessage msg s@(Unrelenting1 attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      player <- getPlayer iid
      chaosTokensInBag <- getOnlyChaosTokensInBag
      pushAll
        [ FocusChaosTokens chaosTokensInBag
        , chooseUpToN
            player
            3
            "Done sealing tokens"
            [ TargetLabel
              (ChaosTokenFaceTarget $ token.face)
              [ SealChaosToken token
              , SealedChaosToken token (toCard attrs)
              ]
            | token <- chaosTokensInBag
            ]
        , DoStep 1 msg
        ]
      pure s
    DoStep 1 (InvestigatorCommittedSkill iid sid) | sid == toId attrs -> do
      when
        ( all
            ((`elem` [PlusOne, Zero, BlessToken, ElderSign]) . chaosTokenFace)
            (skillSealedChaosTokens attrs)
        )
        do
          mDrawing <- drawCardsIfCan iid attrs 2
          for_ mDrawing push
      pure s
    SkillTestEnds {} -> do
      for_ (skillSealedChaosTokens attrs) (push . UnsealChaosToken)
      pure s
    _ -> Unrelenting1 <$> runMessage msg attrs
