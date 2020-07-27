{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.LolaHayes where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype LolaHayesI = LolaHayesI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

lolaHayes :: LolaHayesI
lolaHayes = LolaHayesI $ baseAttrs
  "03006"
  "Lola Hayes"
  Stats
    { health = 6
    , sanity = 6
    , willpower = 3
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Performer]

instance (InvestigatorRunner env) => RunMessage env LolaHayesI where
  runMessage msg i@(LolaHayesI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> LolaHayesI <$> runMessage msg attrs
