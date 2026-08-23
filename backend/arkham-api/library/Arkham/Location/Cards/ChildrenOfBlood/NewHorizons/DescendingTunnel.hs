module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.DescendingTunnel (descendingTunnel) where

import Arkham.Ability
import Arkham.Fight
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Modifier

newtype DescendingTunnel = DescendingTunnel LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

descendingTunnel :: LocationCard DescendingTunnel
descendingTunnel = symbolLabel $ location DescendingTunnel Cards.descendingTunnel 3 (PerPlayer 1)

instance HasAbilities DescendingTunnel where
  getAbilities (DescendingTunnel a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , restricted a 2 (Here <> exists (enemyAt a <> EnemyWithTitle "Zburamoarte"))
          $ actionAbilityWithCost (GroupClueCost (PerPlayer 1) (be a))
      ]

instance RunMessage DescendingTunnel where
  runMessage msg l@(DescendingTunnel attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      zburamoarte <- getSetAsideCardsMatching (CardWithTitle "Zburamoarte")
      for_ zburamoarte \card -> createEnemyAt_ card attrs.id
      addChaosToken #blood
      addChaosToken #blood
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      selectOne (enemyAt attrs <> EnemyWithTitle "Zburamoarte") >>= traverse_ \zburamoarte -> do
        skillTestModifier sid (attrs.ability 2) sid SkillTestAutomaticallySucceeds
        skillTestModifier sid (attrs.ability 2) iid (DamageDealt 1)
        push $ FightEnemy zburamoarte $ mkChooseFightPure sid iid (attrs.ability 2)
      pure l
    _ -> DescendingTunnel <$> liftRunMessage msg attrs
