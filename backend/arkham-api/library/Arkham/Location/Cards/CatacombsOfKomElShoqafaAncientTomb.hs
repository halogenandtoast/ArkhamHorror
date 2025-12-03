module Arkham.Location.Cards.CatacombsOfKomElShoqafaAncientTomb (catacombsOfKomElShoqafaAncientTomb) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.SkillTest.Lifted
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Scenarios.DogsOfWar.Helpers

newtype CatacombsOfKomElShoqafaAncientTomb = CatacombsOfKomElShoqafaAncientTomb LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

catacombsOfKomElShoqafaAncientTomb :: LocationCard CatacombsOfKomElShoqafaAncientTomb
catacombsOfKomElShoqafaAncientTomb =
  symbolLabel
    $ location CatacombsOfKomElShoqafaAncientTomb Cards.catacombsOfKomElShoqafaAncientTomb 5 (PerPlayer 1)

instance HasAbilities CatacombsOfKomElShoqafaAncientTomb where
  getAbilities (CatacombsOfKomElShoqafaAncientTomb a) =
    extendRevealed1 a
      $ scenarioI18n
      $ withI18nTooltip "catacombsOfKomElShoqafa.investigate"
      $ groupLimit PerRound
      $ restricted a 1 Here investigateAction_

instance RunMessage CatacombsOfKomElShoqafaAncientTomb where
  runMessage msg l@(CatacombsOfKomElShoqafaAncientTomb attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ scenarioI18n do
        labeled' "catacombsOfKomElShoqafa.reduceDifficulty" do
          skillTestModifier sid (attrs.ability 1) sid (Difficulty (-2))
          doStep 1 msg
        labeled' "catacombsOfKomElShoqafa.doNotReduce" nothing
      investigate_ sid iid (attrs.ability 1)
      pure $ CatacombsOfKomElShoqafaAncientTomb $ attrs & setMeta False
    DoStep 1 (UseThisAbility _iid (isSource attrs -> True) 1) -> do
      pure $ CatacombsOfKomElShoqafaAncientTomb $ attrs & setMeta True
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      let reduced = toResultDefault False attrs.meta
      drawCards iid (attrs.ability 1) (if reduced then 2 else 3)
      pure l
    _ -> CatacombsOfKomElShoqafaAncientTomb <$> liftRunMessage msg attrs
