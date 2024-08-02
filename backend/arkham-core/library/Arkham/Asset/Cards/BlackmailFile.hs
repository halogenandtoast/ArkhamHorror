module Arkham.Asset.Cards.BlackmailFile (blackmailFile, BlackmailFile (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Modifier

newtype BlackmailFile = BlackmailFile AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackmailFile :: AssetCard BlackmailFile
blackmailFile = asset BlackmailFile Cards.blackmailFile

instance HasAbilities BlackmailFile where
  getAbilities (BlackmailFile x) =
    [ skillTestAbility
        $ restrictedAbility x 1 ControlsThis
        $ parleyAction (ChooseEnemyCost $ EnemyAt YourLocation <> NonEliteEnemy <> EnemyWithHealth)
    ]

instance RunMessage BlackmailFile where
  runMessage msg a@(BlackmailFile attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (chosenEnemyPayment -> Just eid) -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) eid #willpower
        $ EnemyMaybeGameValueFieldCalculation eid EnemyHealthActual
      pure a
    PassedThisSkillTest _iid (isAbilitySource attrs 1 -> True) -> do
      getSkillTestTarget >>= \case
        Just (EnemyTarget eid) -> do
          roundModifier (attrs.ability 1) eid (AddKeyword Keyword.Aloof)
          push $ DisengageEnemyFromAll eid
        _ -> error "invalid target"
      pure a
    _ -> BlackmailFile <$> liftRunMessage msg attrs
