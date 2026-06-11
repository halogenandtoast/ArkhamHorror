module Arkham.Asset.Assets.EixodolonsNote (eixodolonsNote) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose

newtype EixodolonsNote = EixodolonsNote AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eixodolonsNote :: AssetCard EixodolonsNote
eixodolonsNote = asset EixodolonsNote Cards.eixodolonsNote

instance HasAbilities EixodolonsNote where
  getAbilities (EixodolonsNote a) =
    [ restricted a 1 (ControlsThis <> exists (NotInvestigator You))
        $ forced
        $ Matcher.InvestigatorDefeated #when ByAny You
    ]

instance RunMessage EixodolonsNote where
  runMessage msg a@(EixodolonsNote attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ NotInvestigator $ InvestigatorWithId iid
      chooseOrRunOneM iid do
        targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    _ -> EixodolonsNote <$> liftRunMessage msg attrs
