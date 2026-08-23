module Arkham.Location.Cards.ChildrenOfBlood.BloodMoney.Balcony (balcony) where

import Arkham.Ability
import Arkham.Helpers.Modifiers hiding (gameModifier)
import Arkham.Location.CardDefs.ChildrenOfBlood.BloodMoney qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (CrimeScene, Humanoid))

newtype Balcony = Balcony LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

balcony :: LocationCard Balcony
balcony = symbolLabel $ location Balcony Cards.balcony 1 (Static 0)

instance HasAbilities Balcony where
  getAbilities (Balcony a) =
    extendRevealed1 a
      $ skillTestAbility
      $ restricted a 1 (Here <> exists (EnemyWithTrait Humanoid <> enemyAt a)) actionAbility

{- | The three tests chain: each stage carries its number in an 'IndexedSource' so
the success handler knows which test just passed, and the chosen enemy rides along
as the skill test's target.
-}
stage :: ReverseQueue m => LocationAttrs -> Int -> InvestigatorId -> EnemyId -> m ()
stage attrs n iid eid = do
  sid <- getRandom
  let source = IndexedSource n (attrs.ability 1)
  case n of
    1 -> beginSkillTest sid iid source eid #intellect (Fixed 3)
    2 -> beginSkillTest sid iid source eid #agility (Fixed 4)
    _ -> beginSkillTest sid iid source eid #combat (Fixed 5)

instance RunMessage Balcony where
  runMessage msg l@(Balcony attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ EnemyWithTrait Humanoid <> enemyAt attrs
      chooseTargetM iid enemies $ stage attrs 1 iid
      pure l
    -- the initiator target is what carries the chosen enemy; the bare EnemyTarget
    -- copies are the per-subscriber pushes
    PassedSkillTest
      iid
      _
      (IndexedSource n (isAbilitySource attrs 1 -> True))
      (SkillTestInitiatorTarget (EnemyTarget eid))
      _
      _ -> do
      if n >= 3
        then do
          defeatEnemy eid iid (attrs.ability 1)
          gameModifier (attrs.ability 1) attrs (AddTrait CrimeScene)
        else stage attrs (n + 1) iid eid
      pure l
    _ -> Balcony <$> liftRunMessage msg attrs
