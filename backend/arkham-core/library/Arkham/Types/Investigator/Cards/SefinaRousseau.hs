{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.SefinaRousseau where

import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype SefinaRousseau = SefinaRousseau Attrs
  deriving newtype (Show, ToJSON, FromJSON)

sefinaRousseau :: SefinaRousseau
sefinaRousseau = SefinaRousseau $ baseAttrs
  "03003"
  "Sefina Rousseau"
  Rogue
  Stats
    { health = 5
    , sanity = 9
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 4
    }
  [Artist]

instance (InvestigatorRunner Attrs env) => RunMessage env SefinaRousseau where
  runMessage msg i@(SefinaRousseau attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> SefinaRousseau <$> runMessage msg attrs
