module Arkham.Enemy.Cards.ParadigmEfficer (paradigmEfficer) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), getModifiers)
import Arkham.Matcher
import Arkham.Strategy

newtype ParadigmEfficer = ParadigmEfficer EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradigmEfficer :: EnemyCard ParadigmEfficer
paradigmEfficer = enemy ParadigmEfficer Cards.paradigmEfficer (3, Static 3, 3) (1, 1)

instance HasAbilities ParadigmEfficer where
  getAbilities (ParadigmEfficer a) = extend1 a $ restricted a 1 OnSameLocation $ forced $ TurnEnds #after You

instance RunMessage ParadigmEfficer where
  runMessage msg e@(ParadigmEfficer attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
      do_ msg
      pure e
    Do (UseThisAbility iid (isSource attrs -> True) 1) -> do
      mods <- getModifiers iid
      let hollowCount = length [() | Hollow _ <- mods]
      when (hollowCount >= 3) $ initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards do
        continue_ iid
        for_ cards \card -> do
          when (cardMatch card NonWeakness) $ hollow iid card
      pure e
    _ -> ParadigmEfficer <$> liftRunMessage msg attrs
