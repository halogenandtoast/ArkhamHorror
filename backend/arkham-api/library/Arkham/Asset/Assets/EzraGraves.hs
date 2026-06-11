module Arkham.Asset.Assets.EzraGraves (ezraGraves) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Card (putCardIntoPlay)
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ally))

newtype EzraGraves = EzraGraves AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ezraGraves :: AssetCard EzraGraves
ezraGraves = ally EzraGraves Cards.ezraGraves (1, 2)

instance HasAbilities EzraGraves where
  getAbilities (EzraGraves a) =
    [ restricted a 1 (OnSameLocation <> DuringTurn Anyone)
        $ FastAbility (exhaust a <> ResourceCost 1)
    , groupLimit PerGame
        $ restricted
          a
          2
          (OnSameLocation <> exists (InDiscardOf You <> basic (CardWithTrait Ally)))
          actionAbility
    ]

instance RunMessage EzraGraves where
  runMessage msg a@(EzraGraves attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      takeActionAsIfTurn iid (attrs.ability 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      allies <- select $ InDiscardOf (InvestigatorWithId iid) <> basic (CardWithTrait Ally)
      focusCards allies \unfocus -> do
        chooseOneM iid do
          for_ allies \card -> cardLabeled card do
            unfocus
            putCardIntoPlay iid card
      pure a
    _ -> EzraGraves <$> liftRunMessage msg attrs
