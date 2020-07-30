{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JoeDiamond where

import Arkham.Types.Classes
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import Arkham.Types.Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype JoeDiamondI = JoeDiamondI Attrs
  deriving newtype (Show, ToJSON, FromJSON)

joeDiamond :: JoeDiamondI
joeDiamond = JoeDiamondI $ baseAttrs
  "05002"
  "Joe Diamond"
  Stats
    { health = 8
    , sanity = 6
    , willpower = 2
    , intellect = 4
    , combat = 4
    , agility = 2
    }
  [Detective]

instance (InvestigatorRunner env) => RunMessage env JoeDiamondI where
  runMessage msg i@(JoeDiamondI attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> JoeDiamondI <$> runMessage msg attrs
