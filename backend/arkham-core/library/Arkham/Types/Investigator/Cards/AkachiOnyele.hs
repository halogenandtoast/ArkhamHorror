{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.AkachiOnyele where

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

newtype AkachiOnyele = AkachiOnyele Attrs
  deriving newtype (Show, ToJSON, FromJSON)

akachiOnyele :: AkachiOnyele
akachiOnyele = AkachiOnyele $ baseAttrs
  "03004"
  "Akachi Onyele"
  Mystic
  Stats
    { health = 6
    , sanity = 8
    , willpower = 5
    , intellect = 2
    , combat = 3
    , agility = 3
    }
  [Sorcerer]

instance (InvestigatorRunner Attrs env) => RunMessage env AkachiOnyele where
  runMessage msg i@(AkachiOnyele attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid _skillValue | iid == investigatorId -> pure i
    _ -> AkachiOnyele <$> runMessage msg attrs
