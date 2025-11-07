module Arkham.Agenda.Cards.ABetrayalOfEyes (aBetrayalOfEyes) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Card
import Arkham.Helpers.GameValue
import Arkham.Helpers.Scenario
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log
import Data.Map.Strict qualified as Map

newtype ABetrayalOfEyes = ABetrayalOfEyes AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aBetrayalOfEyes :: AgendaCard ABetrayalOfEyes
aBetrayalOfEyes = agenda (3, A) ABetrayalOfEyes Cards.aBetrayalOfEyes (Static 5)

instance HasAbilities ABetrayalOfEyes where
  getAbilities (ABetrayalOfEyes a) =
    [restricted a 1 (ExtendedCardCount (AtLeast $ PerPlayer 3) HollowedCard) $ forced AnyWindow]

instance RunMessage ABetrayalOfEyes where
  runMessage msg a@(ABetrayalOfEyes attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- perPlayer 1
      cards <- select HollowedCard
      focusCards cards $ chooseNM iid n $ targets cards removeHollow
      placeDoomOnAgendaAndCheckAdvance 1
      pure a
    AdvanceAgenda (isSide B attrs -> True) -> do
      selectEach HollowedCard removeHollow
      doStep 1 msg
      push R2
      pure a
    DoStep 1 (AdvanceAgenda (isSide B attrs -> True)) -> do
      meta <- getScenarioMetaKeyDefault @(Map InvestigatorId [Card]) "removedHollows" mempty
      for_ (Map.assocs meta) \(iid, cards) -> do
        let highestXp = maxes $ cards & map \c -> (c, c.experienceCost)
        focusCards cards do
          chooseTargetM iid highestXp \c -> do
            exile c.id
            when c.unique do
              recordSetInsert ErasedFromExistence [c.cardCode]
              removeCampaignCardFromDeck iid c
      pure a
    _ -> ABetrayalOfEyes <$> liftRunMessage msg attrs
