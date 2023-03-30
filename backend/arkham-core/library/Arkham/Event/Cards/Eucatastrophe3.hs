module Arkham.Event.Cards.Eucatastrophe3
  ( eucatastrophe3
  , Eucatastrophe3(..)
  )
where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Token
import Arkham.Helpers.Modifiers
import Arkham.EffectMetadata
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype Eucatastrophe3 = Eucatastrophe3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eucatastrophe3 :: EventCard Eucatastrophe3
eucatastrophe3 =
  event Eucatastrophe3 Cards.eucatastrophe3

toWindowToken :: [Window] -> Token
toWindowToken [] = error "Missing window"
toWindowToken (Window _ (Window.RevealToken _ token) : _) = token
toWindowToken (_ : xs) = toWindowToken xs

instance RunMessage Eucatastrophe3 where
  runMessage msg e@(Eucatastrophe3 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ (toWindowToken -> token) _ | eid == toId attrs -> do
      push $ CreateTokenEffect (EffectModifiers $ toModifiers attrs [TokenFaceModifier [ElderSign]]) (toSource attrs) token
      pure e
    _ -> Eucatastrophe3 <$> runMessage msg attrs
