module Arkham.Homebrew.DarkMatter.Assets.K11SurveyUnit (k11SurveyUnit) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (scan, scanAction)
import Arkham.Location.Types (Field (LocationPrintedSymbol))
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype K11SurveyUnit = K11SurveyUnit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

k11SurveyUnit :: AssetCard K11SurveyUnit
k11SurveyUnit = asset K11SurveyUnit Cards.k11SurveyUnit

{- | "[action] Scan. Choose a connecting location and exhaust K-11 Survey Unit:
Search for the topmost card in the scanning deck with an icon matching the chosen
location and draw it. Shuffle the scanning deck."
-}
instance HasAbilities K11SurveyUnit where
  getAbilities (K11SurveyUnit a) =
    [controlled a 1 ControlsThis $ scanAction (exhaust a)]

instance RunMessage K11SurveyUnit where
  runMessage msg a@(K11SurveyUnit attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      investigators <- select $ colocatedWith iid
      chooseTargetM iid investigators \bearer -> putCardIntoPlay bearer attrs
      pure a
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      connected <- select $ connectedTo (locationWithInvestigator iid)
      chooseTargetM iid connected \lid -> do
        symbol <- field LocationPrintedSymbol lid
        scan iid (attrs.ability 1) [symbol]
      pure a
    _ -> K11SurveyUnit <$> liftRunMessage msg attrs
