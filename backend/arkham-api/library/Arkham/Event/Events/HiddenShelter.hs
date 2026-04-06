module Arkham.Event.Events.HiddenShelter (hiddenShelter) where

import Arkham.Ability
import Arkham.Capability
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.I18n
import Arkham.Matcher

newtype HiddenShelter = HiddenShelter EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

hiddenShelter :: EventCard HiddenShelter
hiddenShelter = event HiddenShelter Cards.hiddenShelter

instance HasAbilities HiddenShelter where
  getAbilities (HiddenShelter a) = case a.attachedTo of
    Just (LocationTarget lid) ->
      [ restricted a 1 OwnsThis $ triggered_ $ RoundEnds #when
      , mkAbility a 2 $ forced $ EnemyEnters #after (LocationWithId lid) AnyEnemy
      ]
    _ -> []

instance RunMessage HiddenShelter where
  runMessage msg e@(HiddenShelter attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      withLocationOf iid $ place attrs . AttachedToLocation
      pure e
    UseThisAbility _iid (isSource attrs -> True) 1 -> withI18n do
      let source = attrs.ability 1
      for_ attrs.attachedTo.location \lid -> do
        selectEach (InvestigatorAt (LocationWithId lid)) \iid' -> do
          drawOk <- can.draw.cards iid'
          resourceOk <- can.gain.resources iid'
          canHealDamage <- iid' <=~> HealableInvestigator source #damage (InvestigatorWithId iid')
          canHealHorror <- iid' <=~> HealableInvestigator source #horror (InvestigatorWithId iid')
          chooseOneM iid' $ countVar 1 do
            labeledValidate' drawOk "drawCards" $ drawCards iid' source 1
            labeledValidate' resourceOk "gainResources" $ gainResources iid' source 1
            labeledValidate' canHealDamage "healDamage" $ healDamage iid' source 1
            labeledValidate' canHealHorror "healHorror" $ healHorror iid' source 1
      pure e
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      toDiscardBy iid (attrs.ability 2) attrs
      pure e
    _ -> HiddenShelter <$> liftRunMessage msg attrs
