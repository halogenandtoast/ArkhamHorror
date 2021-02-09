module Arkham.Types.Effect.Effects.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
  )
where


import qualified Arkham.Types.Action as Action
import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

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

instance (HasQueue env, HasSet ConnectedLocationId env LocationId) => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs@EffectAttrs {..}) = case msg of
    CreatedEffect eid _ _ (InvestigationTarget iid lid) | eid == effectId ->
      e <$ unshiftMessage
        (Investigate iid lid (toSource attrs) SkillIntellect False)
    SuccessfulInvestigation iid lid source | isSource attrs source -> do
      lids <- map unConnectedLocationId <$> getSetList @ConnectedLocationId lid
      e <$ unshiftMessages
        [ chooseOne
          iid
          [ TargetLabel
              (LocationTarget lid')
              [InvestigatorDiscoverClues iid lid' 1 (Just Action.Investigate)]
          | lid' <- lids
          ]
        , DisableEffect effectId
        ]
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> SeekingAnswers <$> runMessage msg attrs
