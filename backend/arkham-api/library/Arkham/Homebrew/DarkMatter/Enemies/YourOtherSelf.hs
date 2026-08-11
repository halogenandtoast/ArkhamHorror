module Arkham.Homebrew.DarkMatter.Enemies.YourOtherSelf (yourOtherSelf) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Investigator (baseSkillValueFor)
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Homebrew.DarkMatter.CardDefs.Enemies qualified as Cards
import Arkham.Investigator.Types (Field (InvestigatorHealth))
import Arkham.Matcher
import Arkham.Projection

newtype YourOtherSelf = YourOtherSelf EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

yourOtherSelf :: EnemyCard YourOtherSelf
yourOtherSelf = enemy YourOtherSelf Cards.yourOtherSelf

{- | "Cannot be disengaged (but can be exhausted). / Your Other Self's fight,
health and evade values are equal to the engaged investigator's base [combat],
health, and [agility] values."
-}
instance HasModifiersFor YourOtherSelf where
  getModifiersFor (YourOtherSelf a) = do
    modifySelf a [CannotBeDisengagedBy AnySource]
    engaged <- selectOne $ InvestigatorEngagedWith (EnemyWithId a.id)
    for_ engaged \iid -> do
      combat <- baseSkillValueFor #combat Nothing iid
      agility <- baseSkillValueFor #agility Nothing iid
      health <- field InvestigatorHealth iid
      modifySelf a [EnemyFight combat, EnemyEvade agility, HealthModifier health]

{- | "Forced - When you deal damage to Your Other Self, if it is ready: The
engaged investigator also takes that amount of damage. You may spend 1 clue to
cancel that damage."
-}
instance HasAbilities YourOtherSelf where
  getAbilities (YourOtherSelf a) =
    extend1 a
      $ restricted a 1 (thisExists a ReadyEnemy)
      $ forced
      $ EnemyDealtDamage #when AnyDamageEffect (be a) AnySource

instance RunMessage YourOtherSelf where
  runMessage msg e@(YourOtherSelf attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assignDamage iid (attrs.ability 1) 1
      pure e
    _ -> YourOtherSelf <$> liftRunMessage msg attrs
