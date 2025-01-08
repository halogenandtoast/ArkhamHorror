module Arkham.Asset.Assets.ProfessorWilliamDyerProfessorOfGeologyResolute (
  professorWilliamDyerProfessorOfGeologyResolute,
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ProfessorWilliamDyerProfessorOfGeologyResolute
  = ProfessorWilliamDyerProfessorOfGeologyResolute AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamDyerProfessorOfGeologyResolute
  :: AssetCard ProfessorWilliamDyerProfessorOfGeologyResolute
professorWilliamDyerProfessorOfGeologyResolute =
  allyWith
    ProfessorWilliamDyerProfessorOfGeologyResolute
    Cards.professorWilliamDyerProfessorOfGeologyResolute
    (1, 6)
    noSlots

instance HasAbilities ProfessorWilliamDyerProfessorOfGeologyResolute where
  getAbilities (ProfessorWilliamDyerProfessorOfGeologyResolute a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ HealableInvestigator (a.ability 1) #horror (at_ YourLocation)
            , exists $ HealableAsset (a.ability 1) #horror (at_ YourLocation <> not_ (be a))
            ]
        )
        $ FastAbility (assetUseCost a Secret 1 <> exhaust a)
    ]

instance RunMessage ProfessorWilliamDyerProfessorOfGeologyResolute where
  runMessage msg a@(ProfessorWilliamDyerProfessorOfGeologyResolute attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid
      assets <-
        select
          $ HealableAsset (toSource attrs) #horror
          $ not_ (be attrs)
          <> at_ (locationWithInvestigator iid)

      chooseOneM iid do
        targets investigators \x -> healHorror x (attrs.ability 1) 2
        targets assets \x -> healHorror x (attrs.ability 1) 2
      pure a
    _ -> ProfessorWilliamDyerProfessorOfGeologyResolute <$> liftRunMessage msg attrs
