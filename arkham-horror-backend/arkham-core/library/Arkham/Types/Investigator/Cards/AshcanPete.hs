{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AshcanPete where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype AshcanPeteI = AshcanPeteI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ashcanPete :: AshcanPeteI
ashcanPete = AshcanPeteI $ baseAttrs
  "02005"
  "\"Ashcan\" Pete"
  Stats
    { health = 6
    , sanity = 5
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 3
    }
  [Drifter]

instance (InvestigatorRunner env) => RunMessage env AshcanPeteI where
  runMessage msg i@(AshcanPeteI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> AshcanPeteI <$> runMessage msg attrs
