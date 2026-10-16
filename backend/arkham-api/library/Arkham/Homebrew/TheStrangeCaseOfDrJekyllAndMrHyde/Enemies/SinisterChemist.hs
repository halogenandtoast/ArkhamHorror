{- | Sinister Chemist, the far side of the Adapted Formula story card.

"During attacks against Sinister Chemist, you may use your {intellect} instead
of the skill indicated." The engine offers an alternative skill from
'UseSkillInPlaceOf' on the attacking investigator, read once when the fight test
begins, so the modifier has to be hung on the attempt itself: the
'AttemptToFight' window is raised ahead of the test and is the only place that
knows both who is attacking and that it is this enemy they are attacking. The
ability is 'SilentForcedAbility' because the card prints a constant effect, not
an ability -- the choice it grants is the engine's own skill prompt.
-}
module Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Enemies.SinisterChemist (
  sinisterChemist,
) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Helpers.Window (windowSkillTestId)
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.TheStrangeCaseOfDrJekyllAndMrHyde.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Item))

newtype SinisterChemist = SinisterChemist EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinisterChemist :: EnemyCard SinisterChemist
sinisterChemist = enemy SinisterChemist Cards.sinisterChemist

instance HasAbilities SinisterChemist where
  getAbilities (SinisterChemist a) =
    extend
      a
      [ scenarioI18n
          $ withI18nTooltip "sinisterChemist.intellect"
          $ mkAbility a 1
          $ SilentForcedAbility
          $ AttemptToFight #when You (be a)
      , scenarioI18n
          $ withI18nTooltip "sinisterChemist.discard"
          $ mkAbility a 2
          $ forced
          $ EnemyAttacks #when You AnyEnemyAttack (be a)
      ]

instance RunMessage SinisterChemist where
  runMessage msg e@(SinisterChemist attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (windowSkillTestId -> sid) _ -> do
      skillTestModifiers sid (attrs.ability 1) iid
        $ map (`UseSkillInPlaceOf` #intellect) [#willpower, #combat, #agility]
      pure e
    -- "Discard an Item asset you control or an Item asset in your hand."
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      assets <- selectTargets $ assetControlledBy iid <> AssetWithTrait Item <> DiscardableAsset
      cards <- selectTargets $ inHandOf NotForPlay iid <> basic (#asset <> CardWithTrait Item)
      unless (null assets && null cards)
        $ chooseTargetM iid (assets <> cards)
        $ toDiscardBy iid (attrs.ability 2)
      pure e
    _ -> SinisterChemist <$> liftRunMessage msg attrs
