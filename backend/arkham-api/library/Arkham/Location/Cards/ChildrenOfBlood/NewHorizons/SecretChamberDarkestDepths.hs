module Arkham.Location.Cards.ChildrenOfBlood.NewHorizons.SecretChamberDarkestDepths (
  secretChamberDarkestDepths,
) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.ChildrenOfBlood.NewHorizons qualified as Enemies
import Arkham.Helpers.Location (getConnectedMoveLocations)
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Location.CardDefs.ChildrenOfBlood.NewHorizons qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype SecretChamberDarkestDepths = SecretChamberDarkestDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

secretChamberDarkestDepths :: LocationCard SecretChamberDarkestDepths
secretChamberDarkestDepths =
  symbolLabel
    $ location SecretChamberDarkestDepths Cards.secretChamberDarkestDepths 3 (PerPlayer 1)

instance HasAbilities SecretChamberDarkestDepths where
  getAbilities (SecretChamberDarkestDepths a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , doesNotProvokeAttacksOfOpportunity $ skillTestAbility $ restricted a 2 Here actionAbility
      ]

instance RunMessage SecretChamberDarkestDepths where
  runMessage msg l@(SecretChamberDarkestDepths attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      workers <- getSetAsideCardsMatching (cardIs Enemies.blightedWorker)
      for_ workers \card -> createEnemyAt_ card attrs.id
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      n <- selectCount $ enemyAt attrs <> ReadyEnemy
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed n)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      selectEach (enemyEngagedWith iid) $ disengageEnemy iid
      ls <- getConnectedMoveLocations iid (attrs.ability 2)
      chooseTargetM iid ls $ moveTo (attrs.ability 2) iid
      pure l
    _ -> SecretChamberDarkestDepths <$> liftRunMessage msg attrs
