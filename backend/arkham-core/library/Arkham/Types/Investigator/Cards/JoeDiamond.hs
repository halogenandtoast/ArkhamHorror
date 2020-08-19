{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JoeDiamond where

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

newtype JoeDiamond = JoeDiamond Attrs
  deriving newtype (Show, ToJSON, FromJSON)

joeDiamond :: JoeDiamond
joeDiamond = JoeDiamond $ baseAttrs
  "05002"
  "Joe Diamond"
  Seeker
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }
  [Detective]

instance HasActions env investigator JoeDiamond where
  getActions i window (JoeDiamond attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env JoeDiamond where
  runMessage msg i@(JoeDiamond attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> JoeDiamond <$> runMessage msg attrs
