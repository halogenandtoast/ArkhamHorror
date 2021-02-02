module Arkham.Types.Effect.Effects.BaseballBat
  ( baseballBat
  , BaseballBat(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs

newtype BaseballBat = BaseballBat Attrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

baseballBat :: EffectArgs -> BaseballBat
baseballBat = BaseballBat . uncurry4 (baseAttrs "01074")

instance HasModifiersFor env BaseballBat where
  getModifiersFor = noModifiersFor

instance HasQueue env => RunMessage env BaseballBat where
  runMessage msg e@(BaseballBat attrs@Attrs {..}) = case msg of
    RevealToken _ iid token | InvestigatorTarget iid == effectTarget ->
      case effectSource of
        AssetSource assetId -> e <$ when
          (token `elem` [Skull, AutoFail])
          (unshiftMessages
            [Discard (AssetTarget assetId), DisableEffect effectId]
          )
        _ -> error "wrong source"
    SkillTestEnds _ -> e <$ unshiftMessage (DisableEffect effectId)
    _ -> BaseballBat <$> runMessage msg attrs
