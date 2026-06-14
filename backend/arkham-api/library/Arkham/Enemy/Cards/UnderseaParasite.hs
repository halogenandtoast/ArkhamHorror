module Arkham.Enemy.Cards.UnderseaParasite (underseaParasite) where

import Arkham.Ability
import Arkham.Campaigns.TheDrownedCity.Import
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks, pattern EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Log (record)

newtype UnderseaParasite = UnderseaParasite EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

underseaParasite :: EnemyCard UnderseaParasite
underseaParasite = enemy UnderseaParasite Cards.underseaParasite (5, Static 1, 5) (1, 0)

instance HasAbilities UnderseaParasite where
  getAbilities (UnderseaParasite a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)
      , mkAbility a 2 $ forced $ EnemyLeavesPlay #when (be a)
      ]

instance RunMessage UnderseaParasite where
  runMessage msg e@(UnderseaParasite attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) n | n `elem` [1, 2] -> do
      -- TODO: resolve flipped story side "11549b" (translated word unknown)
      campaignSpecific "translateGlyph" ("x" :: Text, "Parasite" :: Text)
      record TheInvestigatorsDiscoveredAnAlienLanguage
      pure e
    _ -> UnderseaParasite <$> liftRunMessage msg attrs
