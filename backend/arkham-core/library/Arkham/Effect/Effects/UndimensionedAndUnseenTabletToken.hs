module Arkham.Effect.Effects.UndimensionedAndUnseenTabletToken
  ( undimensionedAndUnseenTabletToken
  , UndimensionedAndUnseenTabletToken(..)
  ) where

import Arkham.Prelude


import Arkham.Scenarios.UndimensionedAndUnseen.Helpers
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Effect.Attrs
import Arkham.Effect.Helpers
import Arkham.EnemyId
import Arkham.Matcher
import Arkham.Message
import Arkham.Modifier
import Arkham.Query
import Arkham.Target
import Arkham.Token

newtype UndimensionedAndUnseenTabletToken = UndimensionedAndUnseenTabletToken EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseenTabletToken
  :: EffectArgs -> UndimensionedAndUnseenTabletToken
undimensionedAndUnseenTabletToken =
  UndimensionedAndUnseenTabletToken . uncurry4 (baseAttrs "02236")

instance HasId Difficulty env () => HasModifiersFor env UndimensionedAndUnseenTabletToken where
  getModifiersFor _ (TokenTarget (Token _ Tablet)) (UndimensionedAndUnseenTabletToken attrs)
    = do
      difficulty <- getId @Difficulty ()
      pure
        [ toModifier
            attrs
            (if difficulty `elem` [Easy, Standard]
              then ChangeTokenModifier (NegativeModifier 4)
              else ChangeTokenModifier AutoFailModifier
            )
        ]
  getModifiersFor _ _ _ = pure []

instance
  ( HasId Difficulty env ()
  , HasQueue env
  , Query EnemyMatcher env
  , HasCount ClueCount env EnemyId
  )
  => RunMessage env UndimensionedAndUnseenTabletToken where
  runMessage msg e@(UndimensionedAndUnseenTabletToken attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs -> do
      broodOfYogSothoth <- getBroodOfYogSothoth
      broodOfYogSothothWithClues <- filterM
        (fmap ((> 0) . unClueCount) . getCount)
        broodOfYogSothoth
      difficulty <- getId @Difficulty ()
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
          : [ TargetLabel
                (EnemyTarget enemyId)
                [ RemoveAllClues (EnemyTarget enemyId)
                , DisableEffect $ effectId attrs
                ]
            | enemyId <- broodOfYogSothothWithClues
            ]
          )
    SkillTestEnds _ -> e <$ push (DisableEffect $ effectId attrs)
    _ -> UndimensionedAndUnseenTabletToken <$> runMessage msg attrs
