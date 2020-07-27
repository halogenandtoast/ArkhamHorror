{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.ZoeySamaras where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype ZoeySamarasI = ZoeySamarasI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

zoeySamaras :: ZoeySamarasI
zoeySamaras = ZoeySamarasI $ baseAttrs
  "02001"
  "Zoey Samaras"
  Stats
    { health = 9
    , sanity = 6
    , willpower = 4
    , intellect = 2
    , combat = 4
    , agility = 2
    }
  [Believer, Hunter]

instance (InvestigatorRunner env) => RunMessage env ZoeySamarasI where
  runMessage msg i@(ZoeySamarasI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> ZoeySamarasI <$> runMessage msg attrs
