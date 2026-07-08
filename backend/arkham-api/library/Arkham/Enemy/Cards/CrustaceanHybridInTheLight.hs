module Arkham.Enemy.Cards.CrustaceanHybridInTheLight (crustaceanHybridInTheLight) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Enemy (reduceDamageTakenTo)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Window qualified as Window

newtype CrustaceanHybridInTheLight = CrustaceanHybridInTheLight EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crustaceanHybridInTheLight :: EnemyCard CrustaceanHybridInTheLight
crustaceanHybridInTheLight = enemy CrustaceanHybridInTheLight Cards.crustaceanHybridInTheLight

instance HasModifiersFor CrustaceanHybridInTheLight where
  getModifiersFor (CrustaceanHybridInTheLight a) = do
    day <- getCampaignDay
    let dayNum = case day of
          Day1 -> 1
          Day2 -> 2
          Day3 -> 3
    modifySelf a [HealthModifier dayNum]

instance HasAbilities CrustaceanHybridInTheLight where
  getAbilities (CrustaceanHybridInTheLight a) =
    extend
      a
      [ restricted a 1 (isDark a <> youExist LeadInvestigator)
          $ SilentForcedAbility
          $ oneOf [EnemyEnters #after Anywhere (be a), EnemySpawns #after Anywhere (be a)]
      , mkAbility a 2
          $ forced
          $ EnemyTakeDamage #when AttackDamageEffect (be a) (atLeast 2) AnySource
      ]

instance RunMessage CrustaceanHybridInTheLight where
  runMessage msg e@(CrustaceanHybridInTheLight attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      reduceDamageTakenTo attrs 1
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let darkCard = lookupCard Cards.crustaceanHybridInTheDark attrs.cardId
      push $ ReplaceEnemy attrs.id darkCard Swap
      checkAfter $ Window.EnemyFlipped attrs.id
      pure e
    _ -> CrustaceanHybridInTheLight <$> liftRunMessage msg attrs
