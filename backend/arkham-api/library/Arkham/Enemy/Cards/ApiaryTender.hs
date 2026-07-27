module Arkham.Enemy.Cards.ApiaryTender (apiaryTender) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword (Keyword (Aloof))
import Arkham.Matcher
import Arkham.Modifier

newtype ApiaryTender = ApiaryTender EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

apiaryTender :: EnemyCard ApiaryTender
apiaryTender =
  enemy ApiaryTender Cards.apiaryTender
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities ApiaryTender where
  getAbilities (ApiaryTender a) =
    extend1 a
      $ mkAbility a 1
      $ forced
      $ oneOf
        [ PlacedCounterOnLocation #after (locationWithEnemy a) AnySource DoomCounter (atLeast 1)
        , PlacedCounterOnEnemy #after (EnemyAt (locationWithEnemy a)) AnySource DoomCounter (atLeast 1)
        , PlacedCounterOnAsset #after (AssetAt (locationWithEnemy a)) AnySource DoomCounter (atLeast 1)
        , PlacedCounterOnInvestigator
            #after
            (InvestigatorAt $ locationWithEnemy a)
            AnySource
            DoomCounter
            (atLeast 1)
        ]

instance RunMessage ApiaryTender where
  runMessage msg e@(ApiaryTender attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      roundModifier (attrs.ability 1) attrs (RemoveKeyword Aloof)
      doStep 1 msg
      pure e
    DoStep 1 (UseThisAbility _ (isSource attrs -> True) 1) -> do
      selectEach (investigatorEngagedWith attrs) (initiateEnemyAttack attrs (attrs.ability 1))
      pure e
    _ -> ApiaryTender <$> liftRunMessage msg attrs
