{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DianaStanley where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype DianaStanleyI = DianaStanleyI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dianaStanley :: DianaStanleyI
dianaStanley = DianaStanleyI $ baseAttrs
  "05004"
  "Diana Stanley"
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Cultist, SilverTwilight]

instance (InvestigatorRunner env) => RunMessage env DianaStanleyI where
  runMessage msg i@(DianaStanleyI attrs@Attrs {..}) = case msg of
    ResolveToken Token.ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> DianaStanleyI <$> runMessage msg attrs
