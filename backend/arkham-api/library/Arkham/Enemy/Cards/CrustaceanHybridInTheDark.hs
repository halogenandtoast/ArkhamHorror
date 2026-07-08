module Arkham.Enemy.Cards.CrustaceanHybridInTheDark (crustaceanHybridInTheDark) where

import Arkham.Ability
import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))

newtype CrustaceanHybridInTheDark = CrustaceanHybridInTheDark EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crustaceanHybridInTheDark :: EnemyCard CrustaceanHybridInTheDark
crustaceanHybridInTheDark = enemy CrustaceanHybridInTheDark Cards.crustaceanHybridInTheDark

instance HasModifiersFor CrustaceanHybridInTheDark where
  getModifiersFor (CrustaceanHybridInTheDark a) = do
    day <- getCampaignDay
    let dayNum = case day of
          Day1 -> 1
          Day2 -> 2
          Day3 -> 3
    modifySelf a [HealthModifier dayNum]

instance HasAbilities CrustaceanHybridInTheDark where
  getAbilities (CrustaceanHybridInTheDark a) =
    extend
      a
      [ restricted a 1 (isLight a <> youExist LeadInvestigator)
          $ SilentForcedAbility
          $ oneOf [EnemyEnters #after Anywhere (be a), EnemySpawns #after Anywhere (be a)]
      , restricted a 2 (exists $ investigator_ $ at_ $ locationWithEnemy a)
          $ forced
          $ EnemyFlipped #after (be a)
      ]

instance RunMessage CrustaceanHybridInTheDark where
  runMessage msg e@(CrustaceanHybridInTheDark attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      flipOverBy iid (attrs.ability 1) attrs
      pure e
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      selectEach (InvestigatorAt $ locationWithEnemy attrs) \iid ->
        assignDamage iid (attrs.ability 2) 1
      pure e
    Flip _ _ (isTarget attrs -> True) -> do
      let lightCard = lookupCard Cards.crustaceanHybridInTheLight attrs.cardId
      push $ ReplaceEnemy attrs.id lightCard Swap
      pure e
    _ -> CrustaceanHybridInTheDark <$> liftRunMessage msg attrs
