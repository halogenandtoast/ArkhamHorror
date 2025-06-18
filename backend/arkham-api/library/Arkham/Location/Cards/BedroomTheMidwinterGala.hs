module Arkham.Location.Cards.BedroomTheMidwinterGala (bedroomTheMidwinterGala) where

import Arkham.Ability
import Arkham.Fight
import Arkham.Helpers.Cost
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (SecondFloor))

newtype BedroomTheMidwinterGala = BedroomTheMidwinterGala LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bedroomTheMidwinterGala :: LocationCard BedroomTheMidwinterGala
bedroomTheMidwinterGala = location BedroomTheMidwinterGala Cards.bedroomTheMidwinterGala 3 (PerPlayer 2)

instance HasAbilities BedroomTheMidwinterGala where
  getAbilities (BedroomTheMidwinterGala a) =
    extendRevealed1 a
      $ playerLimit PerTurn
      $ restricted a 1 (Here <> exists (EnemyAt $ LocationWithTrait SecondFloor)) actionAbility

instance RunMessage BedroomTheMidwinterGala where
  runMessage msg l@(BedroomTheMidwinterGala attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOneM iid $ withI18n do
        chooseUseSkill #agility do
          chooseFightEnemyMatchEdit
            sid
            iid
            (attrs.ability 1)
            (EnemyAt $ LocationWithTrait SecondFloor)
            (withSkillType #agility)
        chooseUseSkill #combat do
          chooseFightEnemyMatch sid iid (attrs.ability 1) (EnemyAt $ LocationWithTrait SecondFloor)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      clues <- getSpendableClueCount [iid]
      when (clues > 0) do
        chooseOrRunOneM iid $ withI18n do
          countVar 1 $ labeled' "spendClues" do
            spendClues iid 1
            withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DamageDealt 2)
          labeled' "skip" nothing
      pure l
    _ -> BedroomTheMidwinterGala <$> liftRunMessage msg attrs
