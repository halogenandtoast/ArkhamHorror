module Arkham.Enemy.Cards.DagonDeepInSlumberIntoTheMaelstrom (
  dagonDeepInSlumberIntoTheMaelstrom,
  DagonDeepInSlumberIntoTheMaelstrom (..),
)
where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message (ReplaceStrategy (..))
import Arkham.Message.Lifted.Choose

newtype DagonDeepInSlumberIntoTheMaelstrom = DagonDeepInSlumberIntoTheMaelstrom EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dagonDeepInSlumberIntoTheMaelstrom :: EnemyCard DagonDeepInSlumberIntoTheMaelstrom
dagonDeepInSlumberIntoTheMaelstrom = enemyWith
  DagonDeepInSlumberIntoTheMaelstrom
  Cards.dagonDeepInSlumberIntoTheMaelstrom
  (0, Static 1, 0)
  (0, 0)
  $ \a -> a {enemyFight = Nothing, enemyHealth = Nothing, enemyEvade = Nothing}

instance HasModifiersFor DagonDeepInSlumberIntoTheMaelstrom where
  getModifiersFor (DagonDeepInSlumberIntoTheMaelstrom a) = modifySelf a [Omnipotent]

instance HasAbilities DagonDeepInSlumberIntoTheMaelstrom where
  getAbilities (DagonDeepInSlumberIntoTheMaelstrom a) =
    [ restricted
        a
        1
        ( exists (InvestigatorAt $ locationIs Locations.lairOfDagonIntoTheMaelstrom)
            <> thisExists a (IncludeOmnipotent ReadyEnemy)
        )
        $ forced
        $ RoundEnds #when
    , skillTestAbility $ restricted a 1 OnSameLocation actionAbility
    ]

instance RunMessage DagonDeepInSlumberIntoTheMaelstrom where
  runMessage msg e@(DagonDeepInSlumberIntoTheMaelstrom attrs) = runQueueT $ case msg of
    EnemyCheckEngagement eid | eid == attrs.id -> pure e
    Flip _ _ (isTarget attrs -> True) -> do
      awakened <- genCard Cards.dagonAwakenedAndEnragedIntoTheMaelstrom
      push $ ReplaceEnemy attrs.id awakened Swap
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      placeDoom (attrs.ability 1) attrs 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #willpower (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      chooseOrRunOneM iid do
        when attrs.ready do
          labeled "Exhaust Dagon" $ exhaustThis attrs
        when (attrs.doom > 0) do
          labeled "Remove 1 doom from Dagon" $ removeDoom (attrs.ability 2) attrs 1
      pure e
    FailedThisSkillTestBy _iid (isAbilitySource attrs 2 -> True) n | n >= 3 -> do
      placeDoom (attrs.ability 2) attrs 1
      pure e
    _ -> DagonDeepInSlumberIntoTheMaelstrom <$> liftRunMessage msg attrs
