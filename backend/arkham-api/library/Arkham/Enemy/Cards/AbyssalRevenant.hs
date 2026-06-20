module Arkham.Enemy.Cards.AbyssalRevenant (abyssalRevenant) where

import Arkham.Ability
import Arkham.Campaigns.GuardiansOfTheAbyss.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (AssetDefeated, EnemyAttacks, InvestigatorDefeated)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher
import Arkham.Trait (Trait (Desert, Otherworld))
import Arkham.Window (windowType)
import Arkham.Window qualified as Window

newtype AbyssalRevenant = AbyssalRevenant EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalRevenant :: EnemyCard AbyssalRevenant
abyssalRevenant =
  enemyWith AbyssalRevenant Cards.abyssalRevenant
    $ spawnAtL
    ?~ SpawnAt (FarthestLocationFromYou $ oneOf [LocationWithTrait Desert, LocationWithTrait Otherworld])

instance HasAbilities AbyssalRevenant where
  getAbilities (AbyssalRevenant a) =
    extend
      a
      [ restricted a 1 (DuringPhase #enemy)
          $ forced
          $ EnemyAttacks #when Anyone AnyEnemyAttack (be a)
      , mkAbility a 2
          $ forced
          $ oneOf
            [ InvestigatorDefeated
                #after
                (DefeatedByMatches [ByDamage, BySource (SourceIsEnemyAttack $ be a)])
                Anyone
            , AssetDefeated
                #after
                (DefeatedByMatches [ByDamage, BySource (SourceIsEnemyAttack $ be a)])
                (#ally <> AssetControlledBy Anyone)
            ]
      ]

instance RunMessage AbyssalRevenant where
  runMessage msg e@(AbyssalRevenant attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      n <- getStrengthOfTheAbyss
      enemyAttackModifier (attrs.ability 1) attrs (DamageDealt (n - 1))
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 ws _ -> do
      for_ ws \w -> case windowType w of
        Window.InvestigatorDefeated _ iid -> investigatorTakenByTheAbyss iid
        Window.AssetDefeated aid _ -> assetTakenByTheAbyss aid
        _ -> pure ()
      pure e
    _ -> AbyssalRevenant <$> liftRunMessage msg attrs
