{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AkachiOnyele where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype AkachiOnyeleI = AkachiOnyeleI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

akachiOnyele :: AkachiOnyeleI
akachiOnyele = AkachiOnyeleI $ baseAttrs
  "03004"
  "Akachi Onyele"
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 3
    , agility = 3
    }
  [Sorcerer]

instance (InvestigatorRunner env) => RunMessage env AkachiOnyeleI where
  runMessage msg i@(AkachiOnyeleI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> AkachiOnyeleI <$> runMessage msg attrs
