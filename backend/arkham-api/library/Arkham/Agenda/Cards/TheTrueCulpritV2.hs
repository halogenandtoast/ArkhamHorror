module Arkham.Agenda.Cards.TheTrueCulpritV2 (theTrueCulpritV2) where

import Arkham.Ability
import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Import.Lifted hiding (EnemyDefeated)
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Enemy.Cards qualified as Cards
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype TheTrueCulpritV2 = TheTrueCulpritV2 AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTrueCulpritV2 :: AgendaCard TheTrueCulpritV2
theTrueCulpritV2 = agenda (3, A) TheTrueCulpritV2 Cards.theTrueCulpritV2 (Static 8)

instance HasAbilities TheTrueCulpritV2 where
  getAbilities (TheTrueCulpritV2 attrs) =
    guard (onSide A attrs)
      *> [ skillTestAbility
             $ controlled
               (proxied (assetIs Cards.sinisterSolution) attrs)
               1
               (exists $ You <> InvestigatorAt "Room 212")
               actionAbility
         , mkAbility attrs 2
             $ Objective
             $ forced
             $ EnemyDefeated #after Anyone ByAny
             $ enemyIs Cards.otherworldlyMeddler
         ]

instance RunMessage TheTrueCulpritV2 where
  runMessage msg a@(TheTrueCulpritV2 attrs) = runQueueT $ case msg of
    UseThisAbility iid p@(ProxySource _ (isSource attrs -> True)) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (toAbilitySource p 1) iid #intellect (Fixed 2)
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advanceAgenda attrs
      pure a
    AdvanceAgendaBy (isSide B attrs -> True) means -> do
      push $ if means == AgendaAdvancedWithDoom then R2 else R1
      pure a
    PassedThisSkillTest iid (AbilitySource (isProxySource attrs -> True) 1) -> do
      sinisterSolution <- selectJust $ assetIs Cards.sinisterSolution
      otherworldlyMeddler <- selectJust $ enemyIs Cards.otherworldlyMeddler
      clues <- field AssetClues sinisterSolution
      isReady <- otherworldlyMeddler <=~> ReadyEnemy

      when (clues >= 2) do
        chooseOneM iid do
          when isReady do
            labeled "remove 2 clues from Sinister Solution to either exhaust Otherworldly Meddler" do
              removeClues sinisterSolution sinisterSolution 2
              exhaustThis otherworldlyMeddler

          labeled "remove 2 clues to deal it 2 damage" do
            removeClues sinisterSolution sinisterSolution 2
            nonAttackEnemyDamage (Just iid) sinisterSolution 2 otherworldlyMeddler
          withI18n skip_
      pure a
    _ -> TheTrueCulpritV2 <$> liftRunMessage msg attrs
