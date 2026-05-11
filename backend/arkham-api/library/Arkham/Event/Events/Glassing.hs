module Arkham.Event.Events.Glassing (glassing) where

import Arkham.Ability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest.Lifted (withSkillTest)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Modifier

newtype Glassing = Glassing EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

glassing :: EventCard Glassing
glassing = event Glassing Cards.glassing

instance HasAbilities Glassing where
  getAbilities (Glassing a) = case a.attachedTo.location of
    Just lid ->
      [ controlled_ a 1
          $ triggeredAction
            #investigate
            (EnemyEnters #after (LocationWithId lid <> InvestigatableLocation) AnyEnemy)
            (exhaust a)
      ]
    _ -> []

instance RunMessage Glassing where
  runMessage msg e@(Glassing attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      for_ attrs.attachedTo.location \lid -> do
        sid <- getRandom
        modifyAnySkill sid (attrs.ability 1) iid 1
        investigateLocationWithSkillChoice sid iid (attrs.ability 1) [#intellect, #agility] lid
      pure e
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      priority $ chooseOneM iid $ cardI18n $ scope "glassing" do
        labeled' "discard" do
          toDiscardBy iid attrs attrs
          withSkillTest \sid -> skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
        labeled' "doNotDiscard" nothing
      pure e
    _ -> Glassing <$> liftRunMessage msg attrs
