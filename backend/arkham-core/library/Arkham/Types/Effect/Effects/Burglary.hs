module Arkham.Types.Effect.Effects.Burglary
  ( burglary
  , Burglary(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Attrs
import Arkham.Types.Effect.Helpers

newtype Burglary = Burglary Attrs
  deriving newtype (Show, ToJSON, FromJSON)

burglary :: EffectArgs -> Burglary
burglary = Burglary . uncurry4 (baseAttrs "01045")

instance HasModifiersFor env Burglary where
  getModifiersFor _ target (Burglary attrs) | target == effectTarget attrs =
    pure [modifier attrs CannotDiscoverClues]
  getModifiersFor _ _ _ = pure []

instance HasQueue env => RunMessage env Burglary where
  runMessage msg e@(Burglary attrs) = case msg of
    SuccessfulInvestigation iid lid _
      | LocationTarget lid == effectTarget attrs -> e <$ unshiftMessages
        [TakeResources iid 3 False, DisableEffect $ effectId attrs]
    SkillTestEnds -> e <$ unshiftMessage (DisableEffect $ effectId attrs)
    _ -> Burglary <$> runMessage msg attrs
