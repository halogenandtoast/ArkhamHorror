module Arkham.Treachery.Cards.Sublimation (sublimation) where

import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.FateOfTheVale.Helpers (scenarioI18n)
import Arkham.Trait (Trait (Emissary))
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype Sublimation = Sublimation TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sublimation :: TreacheryCard Sublimation
sublimation = treachery Sublimation Cards.sublimation

placeHandCardsOnAbyss :: ReverseQueue m => InvestigatorId -> Int -> m ()
placeHandCardsOnAbyss iid n = do
  cards <- field InvestigatorHand iid
  chooseNM iid (min n $ length cards) do
    targets cards \card -> do
      push $ RemoveCardFromHand iid (toCardId card)
      push $ PutCardOnTopOfDeck iid (Deck.ScenarioDeckByKey AbyssDeck) card

nearestEmissaryAttacks :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
nearestEmissaryAttacks source iid = do
  emissaries <- select $ NearestEnemyTo iid (EnemyWithTrait Emissary)
  chooseOrRunOneM iid $ targets emissaries \eid -> initiateEnemyAttack eid source iid

instance RunMessage Sublimation where
  runMessage msg t@(Sublimation attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      revelationSkillTest sid iid attrs #willpower (Fixed 4)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      chooseOneM iid $ scenarioI18n do
        labeled' "sublimation.placeHandOnAbyss" $ placeHandCardsOnAbyss iid n
        labeled' "sublimation.nearestEmissaryAttacks" $ nearestEmissaryAttacks attrs iid
      pure t
    _ -> Sublimation <$> liftRunMessage msg attrs
