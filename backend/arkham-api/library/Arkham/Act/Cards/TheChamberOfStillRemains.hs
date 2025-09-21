module Arkham.Act.Cards.TheChamberOfStillRemains (theChamberOfStillRemains) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.I18n
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype TheChamberOfStillRemains = TheChamberOfStillRemains ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities TheChamberOfStillRemains where
  getAbilities (TheChamberOfStillRemains a) = extend1 a $ mkAbility a 1 exploreAction_

theChamberOfStillRemains :: ActCard TheChamberOfStillRemains
theChamberOfStillRemains =
  act
    (2, A)
    TheChamberOfStillRemains
    Cards.theChamberOfStillRemains
    (Just $ GroupClueCost (PerPlayer 2) "Chamber of Time")

instance RunMessage TheChamberOfStillRemains where
  runMessage msg a@(TheChamberOfStillRemains attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      runExplore iid (attrs.ability 1)
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      chamberOfTime <- selectJust $ locationIs Locations.chamberOfTime
      relicOfAges <- selectJust $ assetIs Assets.relicOfAgesRepossessThePast
      investigators <- select $ investigatorAt chamberOfTime

      leadChooseOneM $ withI18n do
        nameVar Assets.relicOfAgesRepossessThePast $ questionLabeled' "takeControlOf"
        questionLabeledCard Assets.relicOfAgesRepossessThePast
        portraits investigators (`takeControlOfAsset` relicOfAges)

      yig <- genCard Enemies.yig
      createEnemyAt_ yig chamberOfTime
      addToVictory (toTarget attrs)
      advanceActDeck attrs
      pure a
    _ -> TheChamberOfStillRemains <$> liftRunMessage msg attrs
