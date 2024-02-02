module Arkham.Event.Cards.Eucatastrophe3 (
  eucatastrophe3,
  Eucatastrophe3 (..),
)
where

import Arkham.Prelude

import Arkham.ChaosToken
import Arkham.Classes
import Arkham.EffectMetadata
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Eucatastrophe3 = Eucatastrophe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

eucatastrophe3 :: EventCard Eucatastrophe3
eucatastrophe3 =
  event Eucatastrophe3 Cards.eucatastrophe3

toWindowChaosToken :: [Window] -> ChaosToken
toWindowChaosToken [] = error "Missing window"
toWindowChaosToken ((windowType -> Window.RevealChaosToken _ token) : _) = token
toWindowChaosToken (_ : xs) = toWindowChaosToken xs

instance RunMessage Eucatastrophe3 where
  runMessage msg e@(Eucatastrophe3 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ (toWindowChaosToken -> token) _ | eid == toId attrs -> do
      push
        $ CreateChaosTokenEffect
          (EffectModifiers $ toModifiers attrs [ChaosTokenFaceModifier [ElderSign]])
          (toSource attrs)
          token
      pure e
    _ -> Eucatastrophe3 <$> runMessage msg attrs
