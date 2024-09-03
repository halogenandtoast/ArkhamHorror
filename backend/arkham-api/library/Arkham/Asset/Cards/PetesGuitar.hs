module Arkham.Asset.Cards.PetesGuitar (petesGuitar, PetesGuitar (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Capability
import Arkham.Helpers.Investigator (canHaveHorrorHealed)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype PetesGuitar = PetesGuitar AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

petesGuitar :: AssetCard PetesGuitar
petesGuitar = asset PetesGuitar Cards.petesGuitar

instance HasAbilities PetesGuitar where
  getAbilities (PetesGuitar a) =
    [ controlledAbility
        a
        1
        ( exists
            $ NonEliteEnemy
            <> EnemyAt (oneOf [YourLocation, ConnectedFrom YourLocation])
            <> EnemyCanMove
        )
        $ FastAbility (exhaust a)
    ]

instance RunMessage PetesGuitar where
  runMessage msg a@(PetesGuitar attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      selectOneToHandle iid (attrs.ability 1)
        $ NonEliteEnemy
        <> EnemyAt (oneOf [locationWithInvestigator iid, ConnectedFrom $ locationWithInvestigator iid])
        <> EnemyCanMove
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget eid) -> do
      choices <- select $ ConnectedFrom (locationWithEnemy eid) <> LocationCanBeEnteredBy eid
      chooseOne iid $ targetLabels choices $ only . EnemyMove eid
      doStep 1 msg
      pure a
    DoStep 1 (HandleTargetChoice iid (isAbilitySource attrs 1 -> True) _) -> do
      noEnemies <- selectNone $ enemyAtLocationWith iid
      canHeal <- canHaveHorrorHealed (attrs.ability 1) iid
      canGainResources <- can.gain.resources iid
      when (noEnemies && (canHeal || canGainResources)) do
        chooseOrRunOneM iid do
          when canHeal $ labeled "Heal 1 Horror" do
            healHorror iid (attrs.ability 1) 1
          when canGainResources $ labeled "Gain 1 resource" do
            gainResourcesIfCan iid (attrs.ability 1) 1
      pure a
    _ -> PetesGuitar <$> liftRunMessage msg attrs
