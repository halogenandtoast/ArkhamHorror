module Arkham.Enemy.Cards.KnightOfTheInnerCircle (knightOfTheInnerCircle) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype KnightOfTheInnerCircle = KnightOfTheInnerCircle EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightOfTheInnerCircle :: EnemyCard KnightOfTheInnerCircle
knightOfTheInnerCircle =
  enemyWith KnightOfTheInnerCircle Cards.knightOfTheInnerCircle (4, Static 4, 2) (2, 0)
    $ (spawnAtL ?~ SpawnAt ConnectedLocation)
    . (preyL .~ Prey MostKeys)

instance HasAbilities KnightOfTheInnerCircle where
  getAbilities (KnightOfTheInnerCircle attrs) =
    extend
      attrs
      [ skillTestAbility
          $ mkAbility attrs 1
          $ forced
          $ Enters #after You (locationWithEnemy attrs.id)
      , skillTestAbility
          $ mkAbility attrs 2
          $ forced
          $ EnemyEnters #after YourLocation (be attrs)
      ]

instance RunMessage KnightOfTheInnerCircle where
  runMessage msg e@(KnightOfTheInnerCircle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      iids <- select $ InvestigatorAt $ locationWithEnemy attrs.id
      leadChooseOneAtATimeM do
        targets iids \iid -> do
          sid <- getRandom
          beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 4)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      enemyEngageInvestigator attrs iid
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      enemyEngageInvestigator attrs iid
      pure e
    _ -> KnightOfTheInnerCircle <$> liftRunMessage msg attrs
