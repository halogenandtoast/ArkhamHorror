module Arkham.Act.Cards.FalseLight (falseLight) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Campaigns.TheScarletKeys.Key
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Projection

newtype FalseLight = FalseLight ActAttrs
  deriving anyclass IsAct
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

falseLight :: ActCard FalseLight
falseLight = act (3, A) FalseLight Cards.falseLight Nothing

isADesiderio :: EnemyMatcher
isADesiderio = mapOneOf enemyIs [Enemies.desiderioDelgadoAlvarez106, Enemies.desiderioDelgadoAlvarez107]

instance HasAbilities FalseLight where
  getAbilities = actAbilities \a ->
    [ restricted
        (proxied (EnemyMatcherSource isADesiderio) a)
        1
        (hasRecordCriteria TheCellKnowsOfDesisPast)
        parleyAction_
    , mkAbility a 2
        $ Objective
        $ forced
        $ IfEnemyDefeated #after Anyone ByAny isADesiderio
    ]

instance HasModifiersFor FalseLight where
  getModifiersFor (FalseLight a) = do
    modifySelect
      a
      isADesiderio
      [RemoveConcealed, RemoveKeyword Keyword.Hunter, AddKeyword Keyword.Aloof]

instance RunMessage FalseLight where
  runMessage msg a@(FalseLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (ProxySource (EnemySource eid) (isSource attrs -> True)) 1 -> do
      lookAtRevealed iid (attrs.ability 1) eid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      inPlay <- selectOne isADesiderio
      case inPlay of
        Just enemy -> do
          card <- field EnemyCard enemy
          campaignSpecific "desidarioVersion" card.cardCode
        Nothing ->
          campaignSpecific "desidarioVersion"
            =<< sample2 Enemies.desiderioDelgadoAlvarez106.cardCode Enemies.desiderioDelgadoAlvarez107.cardCode
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> FalseLight <$> liftRunMessage msg attrs
