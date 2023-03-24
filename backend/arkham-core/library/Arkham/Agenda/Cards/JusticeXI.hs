module Arkham.Agenda.Cards.JusticeXI
  ( JusticeXI(..)
  , justiceXI
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Trait (Trait(SilverTwilight, Monster))
import Arkham.Window (Window(..))
import Arkham.Window qualified as Window

newtype JusticeXI = JusticeXI AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

justiceXI :: AgendaCard JusticeXI
justiceXI = agenda (1, A) JusticeXI Cards.justiceXI (Static 8)

instance HasModifiersFor JusticeXI where
  getModifiersFor (EnemyTarget eid) (JusticeXI attrs) = do
    isSilverTwilight <- eid <=~> EnemyWithTrait SilverTwilight
    pure $ toModifiers attrs [CannotBeDamaged | isSilverTwilight]
  getModifiersFor _ _ = pure []

instance HasAbilities JusticeXI where
  getAbilities (JusticeXI a) = [mkAbility a 1 $ ForcedAbility $ DrawCard Timing.When Anyone (BasicCardMatch $ CardWithTrait Monster) EncounterDeck]

toDrawnCard :: [Window] -> Card
toDrawnCard [] = error "Missing DrawCard window"
toDrawnCard (Window _ (Window.DrawCard _ card _) : _) = card
toDrawnCard (_ : xs) = toDrawnCard xs

instance RunMessage JusticeXI where
  runMessage msg a@(JusticeXI attrs) =
    case msg of
      AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
        push $ AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        pure a
      UseCardAbility _ (isSource attrs -> True) 1 (toDrawnCard -> card) _ -> do
        removeMessageType DrawEnemyMessage
        pushAll [SetCardAside card, PlaceDoomOnAgenda, AdvanceAgendaIfThresholdSatisfied]
        pure a
      _ -> JusticeXI <$> runMessage msg attrs
