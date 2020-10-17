{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JimCulver where

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

newtype JimCulver = JimCulver Attrs
  deriving newtype (Show, ToJSON, FromJSON)

jimCulver :: JimCulver
jimCulver = JimCulver $ baseAttrs
  "02004"
  "Jim Culver"
  Mystic
  Stats
    { health = 7
    , sanity = 8
    , willpower = 4
    , intellect = 3
    , combat = 3
    , agility = 2
    }
  [Performer]

instance ActionRunner env => HasActions env JimCulver where
  getActions i window (JimCulver attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env JimCulver where
  runMessage msg i@(JimCulver attrs@Attrs {..}) = case msg of
    ResolveToken ElderSign iid | iid == investigatorId ->
      i <$ runTest investigatorId (TokenValue ElderSign 1)
    When (RevealToken source iid ElderSign) | iid == investigatorId -> do
      Just RevealToken{} <- popMessage
      i <$ unshiftMessage
        (Ask
          iid
          (ChooseOne
            [ Label "Resolve as Elder Sign" [RevealToken source iid ElderSign]
            , Label "Resolve as Skull" [RevealToken source iid Skull]
            ]
          )
        )
    When (RunSkillTest iid [TokenValue Skull _]) | iid == investigatorId -> do
      Just (RunSkillTest _ _) <- popMessage
      i <$ unshiftMessage (RunSkillTest iid [TokenValue Skull 0])
    _ -> JimCulver <$> runMessage msg attrs
