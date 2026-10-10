module Arkham.Homebrew.AgesUnwound.Enemies.EagerSphinx (eagerSphinx) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.SkillTest.Lifted (parley)
import Arkham.I18n
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Locations qualified as Locations
import Arkham.Homebrew.AgesUnwound.Scenarios.Unstuck.Helpers
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype EagerSphinx = EagerSphinx EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | "__Spawn__ - Banks of the Nile." Alert and Hunter are on the card def.

Only one of the two /Banks of the Nile/ printings is in play, so the spawn
matcher names both and the ring decides which one exists.
-}
eagerSphinx :: EnemyCard EagerSphinx
eagerSphinx =
  enemyWith EagerSphinx Cards.eagerSphinx
    $ spawnAtL
    ?~ SpawnAt
      (mapOneOf locationIs [Locations.banksOfTheNile_077, Locations.banksOfTheNile_078])

{- | "[action] If Eager Sphinx is ready and engaged with you: __Parley.__ You
attempt the riddle of the sphinx. Test [intellect] (4). If you fail, Eager
Sphinx attacks you. If you succeed, choose one: defeat Eager Sphinx, draw 2
cards or gain 3 resources. (Group limit once per round.)"
-}
instance HasAbilities EagerSphinx where
  getAbilities (EagerSphinx a) =
    extend1 a
      $ groupLimit PerRound
      $ restricted a 1 (exists $ be a <> #ready <> EnemyIsEngagedWith You)
      $ ActionAbility #parley Nothing (ActionCost 1)

instance RunMessage EagerSphinx where
  runMessage msg e@(EagerSphinx attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      parley sid iid (attrs.ability 1) attrs #intellect (Fixed 4)
      pure e
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      initiateEnemyAttack attrs (attrs.ability 1) iid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      chooseOneM iid $ unstuckI18n $ scope "eagerSphinx" do
        labeled "defeatEagerSphinx" $ push $ DefeatEnemy attrs.id iid (attrs.ability 1)
        labeled "drawTwoCards" $ drawCards iid (attrs.ability 1) 2
        labeled "gainThreeResources" $ gainResources iid (attrs.ability 1) 3
      pure e
    _ -> EagerSphinx <$> liftRunMessage msg attrs
