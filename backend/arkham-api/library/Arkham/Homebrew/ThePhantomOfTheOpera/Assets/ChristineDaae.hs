module Arkham.Homebrew.ThePhantomOfTheOpera.Assets.ChristineDaae (christineDaae) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Cost (getSpendableClueCount)
import Arkham.Helpers.GameValue (perPlayer)
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.ThePhantomOfTheOpera.CardDefs.Enemies qualified as Enemies
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype ChristineDaae = ChristineDaae AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

christineDaae :: AssetCard ChristineDaae
christineDaae = asset ChristineDaae Cards.christineDaae

instance HasAbilities ChristineDaae where
  getAbilities (ChristineDaae a) =
    [ skillTestAbility
        $ restricted
          (proxied (EnemyMatcherSource $ enemyIs Enemies.thePhantom) a)
          1
          OnSameLocation
          parleyAction_
    ]

instance RunMessage ChristineDaae where
  runMessage msg a@(ChristineDaae attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(isProxySource attrs -> True) 1 -> do
      sid <- getRandom
      chooseBeginSkillTest sid iid (AbilitySource p 1) iid [#willpower, #intellect] (Fixed 4)
      pure a
    PassedThisSkillTest iid source@(isProxyAbilitySource attrs 1 -> True) -> do
      selectOne (enemyIs Enemies.thePhantom) >>= traverse_ \eid -> do
        placeTokensOn source #clue 1 eid
        investigators <- select $ affectsOthers $ colocatedWith iid
        for_ investigators \iid' -> do
          n <- getSpendableClueCount [iid']
          when (n >= 2) $ chooseOneM iid' $ withI18n $ countVar 2 do
            labeled "spendClues" do
              spendClues iid' 2
              placeTokensOn source #clue 1 eid
            labeled "doNotSpendClues" nothing
      doStep 1 msg
      pure a
    FailedThisSkillTest iid source@(isProxyAbilitySource attrs 1 -> True) -> do
      selectOne (enemyIs Enemies.thePhantom)
        >>= traverse_ \eid -> initiateEnemyAttack eid source iid
      doStep 1 msg
      pure a
    DoStep 1 (PassedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True)) -> do
      checkPhantom iid attrs
      pure a
    DoStep 1 (FailedThisSkillTest iid (isProxyAbilitySource attrs 1 -> True)) -> do
      checkPhantom iid attrs
      pure a
    _ -> ChristineDaae <$> liftRunMessage msg attrs

checkPhantom :: ReverseQueue m => InvestigatorId -> AssetAttrs -> m ()
checkPhantom iid attrs = do
  selectOne (enemyIs Enemies.thePhantom) >>= traverse_ \eid -> do
    required <- perPlayer 2
    clues <- field EnemyClues eid
    when (clues >= required) do
      flipOverBy iid (attrs.ability 1) eid
      exhaustThis eid
      disengageFromAll eid
      addToVictory iid attrs
