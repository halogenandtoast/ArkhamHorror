module Arkham.Asset.Assets.RotDiagram (rotDiagram) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Query (getSetAsideCard)
import Arkham.Helpers.SkillTest.Lifted (investigateEdit_)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Matcher qualified as Matcher
import Arkham.Message.Lifted.Choose

newtype RotDiagram = RotDiagram AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rotDiagram :: AssetCard RotDiagram
rotDiagram = asset RotDiagram Cards.rotDiagram

instance HasAbilities RotDiagram where
  getAbilities (RotDiagram a) =
    [ controlled a 1 (exists $ NotInvestigator You)
        $ forced
        $ Matcher.InvestigatorDefeated #when ByAny You
    , investigateAbility a 2 mempty
        $ ControlsThis
        <> youExist (InvestigatorAt $ locationIs Locations.chamberOfRot)
        <> exists (SetAsideCardMatch $ cardIs Cards.mysteriousSyringe)
    ]

instance RunMessage RotDiagram where
  runMessage msg a@(RotDiagram attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      investigators <- select $ not_ $ InvestigatorWithId iid
      chooseOrRunOneM iid $ targets investigators (`takeControlOfAsset` attrs.id)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      investigateEdit_ sid iid (attrs.ability 2) (setTarget attrs)
      pure a
    SuccessfulInvestigationWith iid (isTarget attrs -> True) -> do
      card <- getSetAsideCard Cards.mysteriousSyringe
      takeControlOfSetAsideAsset iid card
      pure a
    _ -> RotDiagram <$> liftRunMessage msg attrs
