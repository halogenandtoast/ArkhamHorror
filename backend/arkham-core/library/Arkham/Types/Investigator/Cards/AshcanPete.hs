{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AshcanPete where

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

newtype AshcanPete = AshcanPete Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ashcanPete :: AshcanPete
ashcanPete = AshcanPete $ baseAttrs
  "02005"
  "\"Ashcan\" Pete"
  Survivor
  Stats
    { health = 6
    , sanity = 5
    , willpower = 4
    , intellect = 2
    , combat = 2
    , agility = 3
    }
  [Drifter]

instance (InvestigatorRunner Attrs env) => RunMessage env AshcanPete where
  runMessage msg i@(AshcanPete attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> AshcanPete <$> runMessage msg attrs
