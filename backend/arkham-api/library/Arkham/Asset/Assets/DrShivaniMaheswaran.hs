module Arkham.Asset.Assets.DrShivaniMaheswaran (drShivaniMaheswaran) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Window
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Prelude
import Arkham.Story.Cards qualified as Stories

newtype DrShivaniMaheswaran = DrShivaniMaheswaran AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drShivaniMaheswaran :: AssetCard DrShivaniMaheswaran
drShivaniMaheswaran = ally DrShivaniMaheswaran Cards.drShivaniMaheswaran (1, 3)

suspiciousOrderlyMatcher :: EnemyMatcher
suspiciousOrderlyMatcher = enemyIs Enemies.suspiciousOrderly <> not_ ExhaustedEnemy <> EnemyIsEngagedWith Anyone

instance HasAbilities DrShivaniMaheswaran where
  getAbilities (DrShivaniMaheswaran a) =
    [ controlled a 1 (exists suspiciousOrderlyMatcher) $ FastAbility (exhaust a)
    , controlled a 2 (exists $ storyIs Stories.theInfestationBegins)
        $ triggered
          ( oneOf
              [ ScenarioEvent #when Nothing ("revealInfestationToken:" <> tshow face)
              | face <- [Skull, Cultist]
              ]
          )
          (removeCost a)
    ]

instance RunMessage DrShivaniMaheswaran where
  runMessage msg a@(DrShivaniMaheswaran attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      suspiciousOrderlies <- select suspiciousOrderlyMatcher
      player <- getPlayer iid
      push $ chooseOrRunOne player $ targetLabels suspiciousOrderlies (only . EnemyEvaded iid)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 (getChaosToken -> token) _ -> do
      theInfestationBegins <- selectJust $ storyIs Stories.theInfestationBegins
      push
        $ SendMessage (StoryTarget theInfestationBegins) (ChaosTokenCanceled iid (toSource attrs) token)
      pure a
    _ -> DrShivaniMaheswaran <$> runMessage msg attrs
