module Arkham.Location.Cards.BrethrenOfAsh.Arkham.UptownStMarysHospital (uptownStMarysHospital) where

import Arkham.Ability
import Arkham.GameValue
import Arkham.Location.CardDefs.BrethrenOfAsh.Arkham qualified as Cards (uptownStMarysHospital)
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype UptownStMarysHospital = UptownStMarysHospital LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

uptownStMarysHospital :: LocationCard UptownStMarysHospital
uptownStMarysHospital = location UptownStMarysHospital Cards.uptownStMarysHospital 2 (PerPlayer 2)

instance HasAbilities UptownStMarysHospital where
  getAbilities (UptownStMarysHospital a) =
    extendRevealed1 a
      $ playerLimit PerGame
      $ restricted
        a
        1
        ( Here
            <> oneOf
              [ exists $ HealableInvestigator (a.ability 1) #damage $ investigatorAt a
              , exists $ HealableAsset (a.ability 1) #damage $ #ally <> assetAt a
              ]
        )
        actionAbility

instance RunMessage UptownStMarysHospital where
  runMessage msg l@(UptownStMarysHospital attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      doStep 2 msg
      pure l
    DoStep n msg'@(UseThisAbility iid (isSource attrs -> True) 1) | n > 0 -> do
      let source = UseAbilitySource iid (toSource attrs) 1
      investigators <- select $ HealableInvestigator source #damage $ investigatorAt attrs
      allies <- select $ HealableAsset source #damage $ #ally <> assetAt attrs
      unless (null investigators && null allies) do
        chooseOneM iid do
          targets investigators \i -> healDamage i source 1 >> doStep (n - 1) msg'
          targets allies \asset -> healDamage asset source 1 >> doStep (n - 1) msg'
          labeledI "done" nothing
      pure l
    _ -> UptownStMarysHospital <$> liftRunMessage msg attrs
