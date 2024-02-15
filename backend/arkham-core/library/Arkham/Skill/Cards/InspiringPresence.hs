module Arkham.Skill.Cards.InspiringPresence (
  inspiringPresence,
  InspiringPresence (..),
) where

import Arkham.Prelude

import Arkham.Asset.Types
import Arkham.Classes
import Arkham.Damage
import Arkham.Matcher hiding (AssetExhausted)
import Arkham.Message hiding (AssetDamage)
import Arkham.Projection
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype InspiringPresence = InspiringPresence SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspiringPresence :: SkillCard InspiringPresence
inspiringPresence = skill InspiringPresence Cards.inspiringPresence

instance RunMessage InspiringPresence where
  runMessage msg s@(InspiringPresence attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      assets <-
        select
          $ AssetAt
            (LocationWithInvestigator $ InvestigatorWithId $ skillOwner attrs)
          <> AllyAsset
      player <- getPlayer (skillOwner attrs)
      choices <- flip mapMaybeM assets $ \a -> do
        let
          target = AssetTarget a
          healDamage = HealDamage target (toSource attrs) 1
          healHorror = HealHorror target (toSource attrs) 1
        canHealDamage <- a <=~> HealableAsset (toSource attrs) DamageType (AssetWithId a)
        canHealHorror <- a <=~> HealableAsset (toSource attrs) HorrorType (AssetWithId a)
        exhausted <- field AssetExhausted a
        let
          andChoices =
            if canHealDamage || canHealHorror
              then
                [ chooseOrRunOne player
                    $ [Label "Heal 1 damage" [healDamage] | canHealDamage]
                    <> [Label "Heal 1 horror" [healHorror] | canHealHorror]
                ]
              else []
          msgs = [Ready target | exhausted] <> andChoices

        pure $ if null msgs then Nothing else Just $ TargetLabel target msgs

      unless (null choices) $ push $ chooseOne player choices
      pure s
    _ -> InspiringPresence <$> runMessage msg attrs
