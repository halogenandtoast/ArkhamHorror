module Arkham.Effect.Effects.UndimensionedAndUnseenTabletToken
  ( undimensionedAndUnseenTabletToken
  , UndimensionedAndUnseenTabletToken(..)
  ) where

import Arkham.Prelude


import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Helpers
import Arkham.Effect.Runner
import Arkham.Enemy.Types ( Field (..) )
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Types ( Field (..) )
import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Target
import Arkham.Token

newtype UndimensionedAndUnseenTabletToken = UndimensionedAndUnseenTabletToken EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseenTabletToken
  :: EffectArgs -> UndimensionedAndUnseenTabletToken
undimensionedAndUnseenTabletToken =
  UndimensionedAndUnseenTabletToken . uncurry4 (baseAttrs "02236")

instance HasModifiersFor UndimensionedAndUnseenTabletToken where
  getModifiersFor (TokenTarget (Token _ Tablet)) (UndimensionedAndUnseenTabletToken attrs)
    = do
      difficulty <- scenarioField ScenarioDifficulty
      pure
        [ toModifier
            attrs
            (if difficulty `elem` [Easy, Standard]
              then ChangeTokenModifier (NegativeModifier 4)
              else ChangeTokenModifier AutoFailModifier
            )
        ]
  getModifiersFor _ _ = pure []

instance RunMessage UndimensionedAndUnseenTabletToken where
  runMessage msg e@(UndimensionedAndUnseenTabletToken attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs -> do
      broodOfYogSothoth <- getBroodOfYogSothoth
      broodOfYogSothothWithClues <- filterM
        (fieldMap EnemyClues (> 0))
        broodOfYogSothoth
      difficulty <- scenarioField ScenarioDifficulty
      let
        result = if difficulty `elem` [Easy, Standard]
          then "token is -4"
          else "automatically fail"
      if null broodOfYogSothothWithClues
        then pure e
        else e <$ push
          (chooseOne iid
          $ Label
              ("Do not remove clues from Brood of Yog-Sothoth and " <> result)
              []
          : [ targetLabel
                enemyId
                [ RemoveAllClues (EnemyTarget enemyId)
                , DisableEffect $ effectId attrs
                ]
            | enemyId <- broodOfYogSothothWithClues
            ]
          )
    SkillTestEnds _ _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> UndimensionedAndUnseenTabletToken <$> runMessage msg attrs
