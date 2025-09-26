module Arkham.Location.Cards.The9thWard (the9thWard) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.I18n
import Arkham.Investigate (withSkillType)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Modifier
import Arkham.Scenarios.TheSecretName.Helpers
import Arkham.Trait (Trait (Extradimensional))

newtype The9thWard = The9thWard LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

the9thWard :: LocationCard The9thWard
the9thWard = location The9thWard Cards.the9thWard 5 (PerPlayer 1)

instance HasAbilities The9thWard where
  getAbilities (The9thWard a) =
    extendRevealed
      a
      [ scenarioI18n
          $ skillTestAbility
          $ withI18nTooltip "the9thWard.investigate"
          $ mkAbility a 1 investigateAction_
      , scenarioI18n $ hauntedI "the9thWard.haunted" a 2
      ]

instance RunMessage The9thWard where
  runMessage msg l@(The9thWard attrs) = runQueueT $ case msg of
    Msg.RevealLocation _ (is attrs -> True) -> do
      The9thWard <$> liftRunMessage msg (attrs & labelL .~ "the9thWard")
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 1) (withSkillType #agility)
      pure l
    Successful (Action.Investigate, _) iid (isAbilitySource attrs 1 -> True) _ _ -> do
      others <- select $ LocationWithTrait Extradimensional <> not_ (be attrs)
      chooseOrRunOneM iid do
        withI18n $ labeled' "doNotMove" nothing
        targets others (moveTo attrs iid)
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      roundModifier (attrs.ability 2) iid CannotMove
      isTurn <- matches iid TurnInvestigator
      if isTurn
        then turnModifier iid (attrs.ability 2) attrs (ShroudModifier (-1))
        else nextTurnModifier iid (attrs.ability 2) attrs (ShroudModifier (-1))

      pure l
    _ -> The9thWard <$> liftRunMessage msg attrs
