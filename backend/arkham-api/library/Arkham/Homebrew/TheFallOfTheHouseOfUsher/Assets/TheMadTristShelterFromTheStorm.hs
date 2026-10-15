module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Assets.TheMadTristShelterFromTheStorm (
  theMadTristShelterFromTheStorm,
) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.ForMovement
import Arkham.Helpers.Modifiers (ModifierType (..), controllerGets)
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Cards
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move (moveTo)

newtype TheMadTristShelterFromTheStorm = TheMadTristShelterFromTheStorm AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theMadTristShelterFromTheStorm :: AssetCard TheMadTristShelterFromTheStorm
theMadTristShelterFromTheStorm = asset TheMadTristShelterFromTheStorm Cards.theMadTristShelterFromTheStorm

instance HasModifiersFor TheMadTristShelterFromTheStorm where
  getModifiersFor (TheMadTristShelterFromTheStorm a) =
    controllerGets a [SkillModifier #willpower 1, SkillModifier #combat 1]

instance HasAbilities TheMadTristShelterFromTheStorm where
  getAbilities (TheMadTristShelterFromTheStorm a) =
    [ noAOO
        $ restricted a 1 ControlsThis
        $ actionAbilityWithCost (assetUseCost a Secret 1 <> exhaust a)
    ]

instance RunMessage TheMadTristShelterFromTheStorm where
  runMessage msg a@(TheMadTristShelterFromTheStorm attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      ls <- select $ accessibleFromMatch ForMovement (locationWithInvestigator iid)
      unless (null ls) do
        chooseTargetM iid ls \lid -> do
          moveTo (attrs.ability 1) iid lid
          doStep 1 msg
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      let source = attrs.ability 1
      let allyHere = at_ (locationWithInvestigator iid) <> #ally
      healable <-
        nub
          . concat
          <$> sequence
            [ selectMap InvestigatorTarget $ HealableInvestigator source #damage (colocatedWith iid)
            , selectMap InvestigatorTarget $ HealableInvestigator source #horror (colocatedWith iid)
            , selectMap AssetTarget $ HealableAsset source #damage allyHere
            , selectMap AssetTarget $ HealableAsset source #horror allyHere
            ]
      unless (null healable) do
        chooseTargetM iid healable \target -> do
          healDamage target source 1
          healHorror target source 1
      pure a
    _ -> TheMadTristShelterFromTheStorm <$> liftRunMessage msg attrs
