module Arkham.Asset.Cards.FluteOfTheOuterGods4 (fluteOfTheOuterGods4, FluteOfTheOuterGods4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.DamageEffect
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Matcher
import Arkham.Projection

newtype FluteOfTheOuterGods4 = FluteOfTheOuterGods4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fluteOfTheOuterGods4 :: AssetCard FluteOfTheOuterGods4
fluteOfTheOuterGods4 = asset FluteOfTheOuterGods4 Cards.fluteOfTheOuterGods4

instance HasAbilities FluteOfTheOuterGods4 where
  getAbilities (FluteOfTheOuterGods4 x) =
    [ doesNotProvokeAttacksOfOpportunity
        $ controlledAbility
          x
          1
          ( oneOf
              [ exists (EnemyAt YourLocation <> NonEliteEnemy <> EnemyCanEnter ConnectedLocation)
              , DifferentEnemiesExist
                  (EnemyAt YourLocation <> NonEliteEnemy <> EnemyWithDamage (atLeast 1))
                  (EnemyAt YourLocation <> EnemyCanBeDamagedBySource (x.ability 1))
              ]
          )
        $ actionAbilityWithCost
        $ exhaust x
        <> ReleaseChaosTokensCost 1 #curse
    ]

instance RunMessage FluteOfTheOuterGods4 where
  runMessage msg a@(FluteOfTheOuterGods4 attrs) = runQueueT $ case msg of
    PaidForCardCost _ card payment | toCardId card == toCardId attrs -> do
      let x = totalResourcePayment payment
      curseTokens <- take x <$> select (ChaosTokenFaceIs #curse)
      for_ curseTokens $ \token -> do
        pushAll [SealChaosToken token, SealedChaosToken token (toCard attrs)]
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      moveableEnemies <-
        select $ enemyAtLocationWith iid <> NonEliteEnemy <> EnemyCanEnter ConnectedLocation
      damageableEnemies <-
        select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource (attrs.ability 1)
      nonEliteEnemies <- case damageableEnemies of
        [] -> pure []
        [x] ->
          select
            $ EnemyAt YourLocation
            <> NonEliteEnemy
            <> not_ (EnemyWithId x)
            <> EnemyWithDamage (atLeast 1)
        _ -> select $ EnemyAt YourLocation <> NonEliteEnemy <> EnemyWithNonZeroField Enemy.EnemyHealthDamage

      let choices = nub $ moveableEnemies <> nonEliteEnemies

      chooseOne
        iid
        [ targetLabel enemy [HandleTargetChoice iid (attrs.ability 1) (toTarget enemy)]
        | enemy <- choices
        ]

      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget eid) -> do
      locations <- select $ LocationCanBeEnteredBy eid <> ConnectedFrom (locationWithInvestigator iid)
      damage <- field Enemy.EnemyHealthDamage eid
      damageableEnemies <-
        select
          $ EnemyAt YourLocation
          <> EnemyCanBeDamagedBySource (attrs.ability 1)
          <> not_ (EnemyWithId eid)

      chooseOne iid
        $ [targetLabel location [EnemyMove eid location] | location <- locations]
        <> [ targetLabel enemy [EnemyDamage enemy $ nonAttack eid damage] | damage > 0, enemy <- damageableEnemies
           ]
      pure a
    _ -> FluteOfTheOuterGods4 <$> lift (runMessage msg attrs)
