module Arkham.Act.Cards.TheChamberOfStillRemains (TheChamberOfStillRemains (..), theChamberOfStillRemains) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Card
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Helpers.Location
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude

newtype TheChamberOfStillRemains = TheChamberOfStillRemains ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

instance HasAbilities TheChamberOfStillRemains where
  getAbilities (TheChamberOfStillRemains a) = withBaseAbilities a [mkAbility a 1 exploreAction_]

theChamberOfStillRemains :: ActCard TheChamberOfStillRemains
theChamberOfStillRemains =
  act
    (2, A)
    TheChamberOfStillRemains
    Cards.theChamberOfStillRemains
    (Just $ GroupClueCost (PerPlayer 2) "Chamber of Time")

instance RunMessage TheChamberOfStillRemains where
  runMessage msg a@(TheChamberOfStillRemains attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationSymbols <- toConnections =<< getJustLocation iid
      let source = toAbilitySource attrs 1
      push $ Explore iid source (oneOf $ map CardWithPrintedLocationSymbol locationSymbols)
      pure a
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      chamberOfTime <- selectJust $ locationIs Locations.chamberOfTime
      relicOfAges <- selectJust $ assetIs Assets.relicOfAgesRepossessThePast
      investigators <- selectList $ investigatorAt chamberOfTime
      yig <- genCard Enemies.yig
      createYig <- createEnemyAt_ yig chamberOfTime Nothing
      pushAll
        $ [ chooseOrRunOne lead [targetLabel iid [TakeControlOfAsset iid relicOfAges] | iid <- investigators]
          , createYig
          , AddToVictory (toTarget attrs)
          , advanceActDeck attrs
          ]
      pure a
    _ -> TheChamberOfStillRemains <$> runMessage msg attrs
