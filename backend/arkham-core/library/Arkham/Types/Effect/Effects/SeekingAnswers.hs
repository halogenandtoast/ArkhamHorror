{-# LANGUAGE UndecidableInstances #-}

module Arkham.Types.Effect.Effects.SeekingAnswers
  ( seekingAnswers
  , SeekingAnswers(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype SeekingAnswers = SeekingAnswers Attrs
  deriving newtype (Show, ToJSON, FromJSON)

seekingAnswers :: EffectArgs -> SeekingAnswers
seekingAnswers = SeekingAnswers . uncurry4 (baseAttrs "02023")

instance HasModifiersFor env SeekingAnswers where
  getModifiersFor _ target (SeekingAnswers attrs)
    | target == effectTarget attrs = pure [modifier attrs CannotDiscoverClues]
  getModifiersFor _ _ _ = pure []

instance (HasQueue env, HasSet ConnectedLocationId env LocationId) => RunMessage env SeekingAnswers where
  runMessage msg e@(SeekingAnswers attrs) = case msg of
    SuccessfulInvestigation iid lid _
      | LocationTarget lid == effectTarget attrs -> do
        lids <- map unConnectedLocationId
          <$> getSetList @ConnectedLocationId lid
        e <$ unshiftMessages
          [ chooseOne
            iid
            [ TargetLabel
                (LocationTarget lid')
                [InvestigatorDiscoverClues iid lid' 1]
            | lid' <- lids
            ]
          , DisableEffect $ effectId attrs
          ]
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> SeekingAnswers <$> runMessage msg attrs
