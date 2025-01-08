module Arkham.Investigator.Cards.PatriceHathaway (patriceHathaway, PatriceHathaway (..)) where

import Arkham.Capability
import Arkham.Card
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Investigator.Projection ()
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PatriceHathaway = PatriceHathaway InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

patriceHathaway :: InvestigatorCard PatriceHathaway
patriceHathaway =
  investigator PatriceHathaway Cards.patriceHathaway
    $ Stats {health = 7, sanity = 7, willpower = 4, intellect = 2, combat = 2, agility = 2}

instance HasModifiersFor PatriceHathaway where
  getModifiersFor (PatriceHathaway attrs) =
    modifySelf attrs [HandSize (-3), AlternateUpkeepDraw (toTarget attrs)]

instance HasChaosTokenValue PatriceHathaway where
  getChaosTokenValue iid ElderSign (PatriceHathaway attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage PatriceHathaway where
  runMessage msg i@(PatriceHathaway attrs) = runQueueT $ case msg of
    SendMessage (isTarget attrs -> True) AllDrawCardAndResource | attrs.inGame -> do
      discards <- filterCards DiscardableCard <$> attrs.hand
      chooseOneAtATimeM attrs.id $ targets discards $ discardCard attrs attrs
      doStep 1 msg
      pure i
    DoStep 1 (SendMessage (isTarget attrs -> True) AllDrawCardAndResource) | attrs.inGame -> do
      drawCardsIfCan attrs ScenarioSource . (`subtract` 5) . length =<< attrs.hand
      pure i
    ElderSignEffect (is attrs -> True) -> do
      afterSkillTest $ doStep 1 msg
      pure i
    DoStep 1 msg'@(ElderSignEffect (is attrs -> True)) -> do
      canModifyDeck <- can.manipulate.deck attrs.id
      canHaveCardsLeaveDiscard <- can.have.cards.leaveDiscard attrs.id
      when (canModifyDeck && canHaveCardsLeaveDiscard && length attrs.discard > 1) do
        chooseOrRunOneM attrs.id do
          labeled "Shuffle all but 1 card from your discard pile into your deck" $ doStep 2 msg'
          labeled "Skip" nothing
      pure i
    DoStep 2 (ElderSignEffect (is attrs -> True)) -> do
      let discards = map toCard attrs.discard
      focusCards discards \unfocus -> do
        chooseOneM attrs.id do
          questionLabeled "Choose 1 card to leave in discard"
          for_ (eachWithRest discards) \(card, rest) -> do
            targeting card do
              push unfocus
              shuffleCardsIntoDeck attrs.id rest
      pure i
    _ -> PatriceHathaway <$> liftRunMessage msg attrs
