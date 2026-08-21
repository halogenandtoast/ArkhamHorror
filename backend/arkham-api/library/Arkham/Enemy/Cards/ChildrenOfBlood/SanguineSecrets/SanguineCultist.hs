module Arkham.Enemy.Cards.ChildrenOfBlood.SanguineSecrets.SanguineCultist (sanguineCultist) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.SanguineSecrets qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype SanguineCultist = SanguineCultist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sanguineCultist :: EnemyCard SanguineCultist
sanguineCultist =
  enemyWith SanguineCultist Cards.sanguineCultist (spawnAtL ?~ SpawnAt EmptyLocation)

instance HasAbilities SanguineCultist where
  getAbilities (SanguineCultist a) =
    extend1 a $ restricted a 1 CanPlaceDoomOnThis $ forced $ EnemyEntersPlay #after (be a)

instance RunMessage SanguineCultist where
  runMessage msg e@(SanguineCultist attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      mBlood <- selectOne (OnlyInBag #blood)
      investigators <- select UneliminatedInvestigator
      case mBlood of
        Just blood | notNull investigators -> leadChooseOneM $ withI18n do
          targets investigators \iid -> sealChaosToken iid iid blood
          labeled' "doNothing" $ placeDoom (attrs.ability 1) attrs 1
        _ -> placeDoom (attrs.ability 1) attrs 1
      pure e
    _ -> SanguineCultist <$> liftRunMessage msg attrs
