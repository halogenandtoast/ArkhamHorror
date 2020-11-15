module Arkham.Types.Effect
  ( lookupEffect
  , Effect(..)
  )
where

import Arkham.Import

import Arkham.Types.Effect.Effects

data Effect = LetMeHandleThis' LetMeHandleThis | JeremiahPierce' JeremiahPierce
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

deriving anyclass instance HasModifiersFor env Effect
deriving anyclass instance HasQueue env => RunMessage env Effect

lookupEffect :: CardCode -> (Source -> Target -> EffectId -> Effect)
lookupEffect cardCode =
  fromJustNote ("Unknown effect: " <> show cardCode)
    $ lookup cardCode allEffects

allEffects :: HashMap CardCode (Source -> Target -> EffectId -> Effect)
allEffects = mapFromList
  [ ("03022", ((LetMeHandleThis' .) .) . letMeHandleThis)
  , ("50044", ((JeremiahPierce' .) .) . jeremiahPierce)
  ]
