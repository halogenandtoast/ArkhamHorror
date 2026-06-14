module Arkham.Treachery.Cards.HungryWalls (hungryWalls) where

import Arkham.Ability
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest (getSkillTestSkillTypes)
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Message.Lifted.Choose
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype HungryWalls = HungryWalls TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hungryWalls :: TreacheryCard HungryWalls
hungryWalls = treachery HungryWalls Cards.hungryWalls

instance HasAbilities HungryWalls where
  getAbilities (HungryWalls a) =
    [forcedAbility a 1 $ Leaves #when You $ locationWithTreachery a]

instance RunMessage HungryWalls where
  runMessage msg t@(HungryWalls attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      withLocationOf iid $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid do
        for_ [#agility, #combat] \kind ->
          skillLabeled kind $ beginSkillTest sid iid (attrs.ability 1) attrs kind (Fixed 4)
      pure t
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      let source = attrs.ability 1
      skillTypes <- getSkillTestSkillTypes
      if SkillAgility `elem` skillTypes
        then do
          chooseAndDiscardAsset iid source
          assignDamage iid source 1
        else directDamage iid source 2
      toDiscard source attrs
      pure t
    _ -> HungryWalls <$> liftRunMessage msg attrs
