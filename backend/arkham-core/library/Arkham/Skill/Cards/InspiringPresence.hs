module Arkham.Skill.Cards.InspiringPresence
  ( inspiringPresence
  , InspiringPresence(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Skill.Cards as Cards
import Arkham.Asset.Types
import Arkham.Classes
import Arkham.Matcher hiding (AssetExhausted)
import Arkham.Message hiding (AssetDamage)
import Arkham.Projection
import Arkham.Target
import Arkham.Skill.Runner

newtype InspiringPresence = InspiringPresence SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

inspiringPresence :: SkillCard InspiringPresence
inspiringPresence =
  skill InspiringPresence Cards.inspiringPresence

instance RunMessage InspiringPresence where
  runMessage msg s@(InspiringPresence attrs) = case msg of
    PassedSkillTest _ _ _ (isTarget attrs -> True) _ _ -> do
      assets <- selectList
        $ AssetAt
          (LocationWithInvestigator $ InvestigatorWithId $ skillOwner attrs)
        <> AllyAsset
      choices <- flip mapMaybeM assets $ \a -> do
        let target = AssetTarget a
            healDamage = HealDamage target (toSource attrs) 1
            healHorror = HealHorror target (toSource attrs) 1
        hasDamage <- fieldP AssetDamage (> 0) a
        hasHorror <- fieldP AssetHorror (> 0) a
        exhausted <- field AssetExhausted a
        let
          andChoices = if hasDamage && hasHorror
            then
              [ chooseOne (skillOwner attrs)
                [ Label "Heal 1 damage" [healDamage]
                , Label "Heal 1 horror" [healHorror]
                ]
              ]
            else [healDamage | hasDamage] <> [healHorror | hasHorror]
          msgs = [Ready target | exhausted] <> andChoices

        pure $ if null msgs then Nothing else Just $ TargetLabel target msgs

      unless (null choices) $
        push $ chooseOne (skillOwner attrs) choices
      pure s
    _ -> InspiringPresence <$> runMessage msg attrs
