{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.JimCulver where

import Arkham.Import

import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Stats
import Arkham.Types.Trait

newtype JimCulver = JimCulver Attrs
  deriving newtype (Show, ToJSON, FromJSON)

instance HasModifiersFor env JimCulver where
  getModifiersFor (SkillTestSource iid _ _) (DrawnTokenTarget token) (JimCulver attrs)
    | iid == investigatorId attrs && drawnTokenFace token == Skull
    = pure [ChangeTokenModifier $ PositiveModifier 0]
  getModifiersFor source target (JimCulver attrs) =
    getModifiersFor source target attrs

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

instance InvestigatorRunner env => HasTokenValue env JimCulver where
  getTokenValue (JimCulver attrs) iid token | iid == investigatorId attrs =
    case drawnTokenFace token of
      ElderSign -> pure $ TokenValue token (PositiveModifier 1)
      _other -> getTokenValue attrs iid token
  getTokenValue (JimCulver attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env JimCulver where
  runMessage msg i@(JimCulver attrs@Attrs {..}) = case msg of
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
    _ -> JimCulver <$> runMessage msg attrs
