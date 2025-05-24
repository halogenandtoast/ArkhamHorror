module Arkham.Event.Events.Improvisation (improvisation, improvisationEffect) where

import Arkham.Card
import Arkham.ClassSymbol
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection

newtype Improvisation = Improvisation EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EventCard Improvisation
improvisation = event Improvisation Cards.improvisation

instance RunMessage Improvisation where
  runMessage msg e@(Improvisation attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      let roles = filter (/= Mythos) [minBound .. maxBound]
      chooseOneM iid $ for_ roles \role -> labeled (tshow role) $ push $ SetRole iid role
      createCardEffect Cards.improvisation Nothing attrs iid
      drawCards iid attrs 1
      pure e
    _ -> Improvisation <$> liftRunMessage msg attrs

newtype ImprovisationEffect = ImprovisationEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisationEffect :: EffectArgs -> ImprovisationEffect
improvisationEffect = cardEffect ImprovisationEffect Cards.improvisation

instance HasModifiersFor ImprovisationEffect where
  getModifiersFor (ImprovisationEffect attrs) = case attrs.target of
    InvestigatorTarget iid -> do
      role <- field InvestigatorClass iid
      modified_ attrs iid [ReduceCostOf (CardWithClass role) 3]
    _ -> pure mempty

instance RunMessage ImprovisationEffect where
  runMessage msg e@(ImprovisationEffect attrs) = runQueueT $ case msg of
    CardEnteredPlay iid card | isTarget iid attrs.target -> do
      role <- field InvestigatorClass iid
      when ((== Just role) . headMay . toList $ cdClassSymbols (toCardDef card)) (disable attrs)
      pure e
    EndTurn iid | attrs.target == InvestigatorTarget iid -> do
      disableReturn e
    _ -> ImprovisationEffect <$> liftRunMessage msg attrs
