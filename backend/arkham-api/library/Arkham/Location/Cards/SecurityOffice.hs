module Arkham.Location.Cards.SecurityOffice (securityOffice) where

import Arkham.Ability
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey
import Arkham.Scenarios.FortuneAndFolly.Helpers

newtype SecurityOffice = SecurityOffice LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

securityOffice :: LocationCard SecurityOffice
securityOffice = symbolLabel $ location SecurityOffice Cards.securityOffice 4 (PerPlayer 2)

instance HasAbilities SecurityOffice where
  getAbilities (SecurityOffice a) =
    extendRevealed1 a $ restricted a 1 Here actionAbility

instance RunMessage SecurityOffice where
  runMessage msg l@(SecurityOffice attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      scenarioI18n $ chooseAmount' iid "cardsToDiscard" "$cards" 1 3 attrs
      pure l
    ResolveAmounts iid (getChoiceAmount "$cards" -> cardAmount) (isTarget attrs -> True) -> do
      checkGameIcons attrs iid NoMulligan cardAmount
      pure l
    DiscardedCards iid _ (isTarget attrs -> True) cards -> do
      enemyCards <-
        fmap (map (.suit)) . mapMaybeM toPlayingCard =<< selectField EnemyCard (InPlayEnemy AnyEnemy)
      cards' <- cards & mapMaybeM toPlayingCard
      when (any ((`elem` enemyCards) . (.suit)) cards') do
        remember ObservedTheStaff
      when (sum (map (.value) cards') >= 19) do
        drawEncounterCard iid (attrs.ability 1)
      pure l
    _ -> SecurityOffice <$> liftRunMessage msg attrs
