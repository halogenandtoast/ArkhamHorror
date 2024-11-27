module Arkham.Asset.Assets.ProfessorWilliamDyerProfessorOfGeology (
  professorWilliamDyerProfessorOfGeology,
  ProfessorWilliamDyerProfessorOfGeology (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ProfessorWilliamDyerProfessorOfGeology = ProfessorWilliamDyerProfessorOfGeology AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

professorWilliamDyerProfessorOfGeology :: AssetCard ProfessorWilliamDyerProfessorOfGeology
professorWilliamDyerProfessorOfGeology =
  allyWith
    ProfessorWilliamDyerProfessorOfGeology
    Cards.professorWilliamDyerProfessorOfGeology
    (1, 5)
    noSlots

instance HasAbilities ProfessorWilliamDyerProfessorOfGeology where
  getAbilities (ProfessorWilliamDyerProfessorOfGeology a) =
    [ controlled
        a
        1
        ( oneOf
            [ exists $ HealableInvestigator (a.ability 1) #horror (at_ YourLocation)
            , exists $ HealableAsset (a.ability 1) #horror (at_ YourLocation <> not_ (be a))
            ]
        )
        $ actionAbilityWithCost (assetUseCost a Secret 1 <> exhaust a)
    ]

instance RunMessage ProfessorWilliamDyerProfessorOfGeology where
  runMessage msg a@(ProfessorWilliamDyerProfessorOfGeology attrs) = runQueueT $ case msg of
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
    _ -> ProfessorWilliamDyerProfessorOfGeology <$> liftRunMessage msg attrs
