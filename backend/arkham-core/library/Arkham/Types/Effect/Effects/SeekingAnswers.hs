module Arkham.Types.Effect.Effects.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Types.Action as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers
import Arkham.Types.LocationId
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Target

newtype SeekingAnswers = SeekingAnswers EffectAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

seekingAnswers :: EffectArgs -> SeekingAnswers
seekingAnswers = SeekingAnswers . uncurry4 (baseAttrs "02023")

instance HasModifiersFor env SeekingAnswers where
  getModifiersFor _ (LocationTarget lid) (SeekingAnswers attrs@EffectAttrs {..})
    = case effectTarget of
      InvestigationTarget _ lid' | lid == lid' ->
        pure [toModifier attrs AlternateSuccessfullInvestigation]
      _ -> pure []
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasSet LocationId env LocationMatcher) => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigationTarget iid lid) | eid == effectId ->
      e <$ push (Investigate iid lid (toSource attrs) SkillIntellect False)
    SuccessfulInvestigation iid _ source | isSource attrs source -> do
      lids <- getSetList (ConnectedLocation <> LocationWithClues)
      e <$ pushAll
        [ chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid')
              [InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate)]
          | lid' <- lids
          ]
        , DisableEffect effectId
        ]
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> SeekingAnswers <$> runMessage msg attrs
