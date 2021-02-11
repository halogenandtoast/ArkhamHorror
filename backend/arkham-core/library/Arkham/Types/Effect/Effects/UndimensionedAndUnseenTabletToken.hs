module Arkham.Types.Effect.Effects.UndimensionedAndUnseenTabletToken
  ( undimensionedAndUnseenTabletToken
  , UndimensionedAndUnseenTabletToken(..)
  )
where

import Arkham.Prelude

import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Difficulty
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers
import Arkham.Types.EnemyId
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Token

newtype UndimensionedAndUnseenTabletToken = UndimensionedAndUnseenTabletToken EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undimensionedAndUnseenTabletToken
  :: EffectArgs -> UndimensionedAndUnseenTabletToken
undimensionedAndUnseenTabletToken =
  UndimensionedAndUnseenTabletToken . uncurry4 (baseAttrs "02236")

instance HasId Difficulty env () => HasModifiersFor env UndimensionedAndUnseenTabletToken where
  getModifiersFor _ (DrawnTokenTarget (DrawnToken _ Tablet)) (UndimensionedAndUnseenTabletToken attrs)
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

instance (HasId Difficulty env (), HasQueue env, HasCount ClueCount env EnemyId, HasSet StoryEnemyId env CardCode) => RunMessage env UndimensionedAndUnseenTabletToken where
  runMessage msg e@(UndimensionedAndUnseenTabletToken attrs) = case msg of
    CreatedEffect eid _ _ (InvestigatorTarget iid) | eid == effectId attrs -> do
      broodOfYogSothoth <- map unStoryEnemyId
        <$> getSetList @StoryEnemyId (CardCode "02255")
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
        else e <$ unshiftMessage
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
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> UndimensionedAndUnseenTabletToken <$> runMessage msg attrs
