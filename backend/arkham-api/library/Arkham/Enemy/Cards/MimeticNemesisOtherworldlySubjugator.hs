module Arkham.Enemy.Cards.MimeticNemesisOtherworldlySubjugator (mimeticNemesisOtherworldlySubjugator) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Window (damagedEnemyAmount)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Scenarios.WithoutATrace.Helpers

newtype MimeticNemesisOtherworldlySubjugator = MimeticNemesisOtherworldlySubjugator EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mimeticNemesisOtherworldlySubjugator :: EnemyCard MimeticNemesisOtherworldlySubjugator
mimeticNemesisOtherworldlySubjugator = enemyWith
  MimeticNemesisOtherworldlySubjugator
  Cards.mimeticNemesisOtherworldlySubjugator
  (3, Static 1, 4)
  (1, 1)
  \a -> a {enemyHealth = Nothing}

instance HasAbilities MimeticNemesisOtherworldlySubjugator where
  getAbilities (MimeticNemesisOtherworldlySubjugator a) =
    extend
      a
      [ mkAbility a 1 $ forced $ EnemyWouldTakeDamage #when (be a)
      , restricted a 2 (exists $ InvestigatorAt $ locationWithEnemy a) $ forced $ RoundEnds #when
      ]

instance RunMessage MimeticNemesisOtherworldlySubjugator where
  runMessage msg e@(MimeticNemesisOtherworldlySubjugator attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (damagedEnemyAmount -> n) _ -> do
      hollows <- select $ HollowedCard <> basic (not_ $ cardIs Assets.theRedGlovedManHeWasAlwaysThere)
      chooseNM iid n $ targets hollows $ discardCard iid (attrs.ability 1)
      pure e
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      selectEach (InvestigatorAt $ locationWithEnemy attrs) \iid -> do
        cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
        chooseOneM iid $ scenarioI18n do
          labeledValidate' (notNull cards) "mimeticNemesis.hollow" do
            chooseTargetM iid cards $ hollow iid
          labeled' "mimeticNemesis.attack" do
            initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    _ -> MimeticNemesisOtherworldlySubjugator <$> liftRunMessage msg attrs
