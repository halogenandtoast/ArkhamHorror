module Arkham.Homebrew.CircusExMortis.Enemies.StruttingPeacock (struttingPeacock) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.Homebrew.CircusExMortis.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.CircusExMortis.Helpers (Vice (Opulence), hasVice, investigatorWithVice)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype StruttingPeacock = StruttingPeacock EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

struttingPeacock :: EnemyCard StruttingPeacock
struttingPeacock = enemy StruttingPeacock Cards.struttingPeacock & setPrey (investigatorWithVice Opulence)

instance HasAbilities StruttingPeacock where
  getAbilities (StruttingPeacock a) =
    extend
      a
      [ restricted a 1 (exists $ InvestigatorAt (locationWithEnemy a)) $ forced $ RoundEnds #when
      , skillTestAbility $ restricted a 2 OnSameLocation parleyAction_
      ]

instance RunMessage StruttingPeacock where
  runMessage msg e@(StruttingPeacock attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt (locationWithEnemy attrs)) \iid -> do
        additional <- hasVice iid Opulence
        loseResources iid (attrs.ability 1) (if additional then 2 else 1)
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      whenM (hasVice iid Opulence) do
        skillTestModifier sid (attrs.ability 2) sid (Difficulty 2)
        selectEach (notInvestigator iid) \other ->
          skillTestModifier sid (attrs.ability 2) other (CannotCommitCards AnyCard)
      chooseSkillM iid [#willpower, #intellect] \sType ->
        parley sid iid (attrs.ability 2) attrs sType (Fixed 4)
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> StruttingPeacock <$> liftRunMessage msg attrs
