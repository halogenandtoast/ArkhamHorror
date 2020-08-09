{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.MarieLambeau where

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

newtype MarieLambeau = MarieLambeau Attrs
  deriving newtype (Show, ToJSON, FromJSON)

marieLambeau :: MarieLambeau
marieLambeau = MarieLambeau $ baseAttrs
  "05006"
  "Marie Lambeau"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 4
    , intellect = 4
    , combat = 1
    , agility = 3
    }
  [Performer, Sorcerer]

instance (InvestigatorRunner env) => RunMessage env MarieLambeau where
  runMessage msg i@(MarieLambeau attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> MarieLambeau <$> runMessage msg attrs
