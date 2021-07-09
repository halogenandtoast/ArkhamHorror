module Arkham.Types.Investigator.Cards.JimCulver where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Source
import Arkham.Types.Stats
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait

newtype JimCulver = JimCulver InvestigatorAttrs
  deriving newtype (Show, ToJSON, FromJSON, Entity)

instance HasModifiersFor env JimCulver where
  getModifiersFor (SkillTestSource iid _ _ _ _) (DrawnTokenTarget token) (JimCulver attrs)
    | iid == investigatorId attrs && drawnTokenFace token == Skull
    = pure $ toModifiers attrs [ChangeTokenModifier $ PositiveModifier 0]
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

instance HasActions env JimCulver where
  getActions i window (JimCulver attrs) = getActions i window attrs

instance HasTokenValue env JimCulver where
  getTokenValue (JimCulver attrs) iid ElderSign | iid == investigatorId attrs =
    pure $ TokenValue ElderSign (PositiveModifier 1)
  getTokenValue (JimCulver attrs) iid token = getTokenValue attrs iid token

instance InvestigatorRunner env => RunMessage env JimCulver where
  runMessage msg i@(JimCulver attrs@InvestigatorAttrs {..}) = case msg of
    When (RevealToken source iid ElderSign) | iid == investigatorId -> do
      mmsg <- popMessage
      case mmsg of
        Just RevealToken{} -> i <$ push
          (chooseOne
            iid
            [ Label "Resolve as Elder Sign" [RevealToken source iid ElderSign]
            , Label "Resolve as Skull" [RevealToken source iid Skull]
            ]
          )
        _ -> error "wrong message type"
    _ -> JimCulver <$> runMessage msg attrs
