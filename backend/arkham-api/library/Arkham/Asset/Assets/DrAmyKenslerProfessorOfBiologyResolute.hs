module Arkham.Asset.Assets.DrAmyKenslerProfessorOfBiologyResolute (
  drAmyKenslerProfessorOfBiologyResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Capability
import Arkham.Helpers.Query (getInvestigators)
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Strategy

newtype DrAmyKenslerProfessorOfBiologyResolute = DrAmyKenslerProfessorOfBiologyResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drAmyKenslerProfessorOfBiologyResolute :: AssetCard DrAmyKenslerProfessorOfBiologyResolute
drAmyKenslerProfessorOfBiologyResolute =
  allyWith
    DrAmyKenslerProfessorOfBiologyResolute
    Cards.drAmyKenslerProfessorOfBiologyResolute
    (3, 4)
    noSlots

instance HasAbilities DrAmyKenslerProfessorOfBiologyResolute where
  getAbilities (DrAmyKenslerProfessorOfBiologyResolute a) =
    [ restricted a 1 (ControlsThis <> DuringTurn You)
        $ FastAbility'
          (assetUseCost a Secret 1 <> exhaust a)
          [#investigate]
    ]

instance RunMessage DrAmyKenslerProfessorOfBiologyResolute where
  runMessage msg a@(DrAmyKenslerProfessorOfBiologyResolute attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (BaseSkillOf #intellect 6)
      investigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isSource attrs -> True) -> do
      investigators <- getInvestigators
      chooseOneM iid do
        targeting EncounterDeckTarget do
          lookAt
            iid
            (attrs.ability 1)
            EncounterDeckTarget
            [(FromTopOfDeck 1, PutBack)]
            #any
            (defer attrs IsNotDraw)
        targets investigators \x -> do
          lookAt iid (attrs.ability 1) x [(FromTopOfDeck 1, PutBack)] #any (defer attrs IsNotDraw)
      pure a
    SearchFound iid (isTarget attrs -> True) _deck cards -> do
      canAffectOtherPlayers <- can.affect.otherPlayers iid
      focusCards cards \unfocus -> do
        chooseOneM iid do
          when canAffectOtherPlayers do
            labeled "Discard Card" do
              push unfocus
              for_ cards (discardCard iid (attrs.ability 1))
          labeled "Leave card" $ push unfocus
      pure a
    _ -> DrAmyKenslerProfessorOfBiologyResolute <$> liftRunMessage msg attrs
