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
  enemy ApiaryTender Cards.apiaryTender (3, Static 3, 3) (1, 1)
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
        ]

instance RunMessage ApiaryTender where
  runMessage msg e@(ApiaryTender attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      readyThis attrs
      roundModifier (attrs.ability 1) attrs (RemoveKeyword Aloof)
      selectOne (investigatorEngagedWith attrs) >>= traverse_ \iid ->
        initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> ApiaryTender <$> liftRunMessage msg attrs
