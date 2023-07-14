module Arkham.Story.Cards.SickeningReality_66 (
  SickeningReality_66 (..),
  sickeningReality_66,
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Card
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Projection
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner
import Arkham.Token

newtype SickeningReality_66 = SickeningReality_66 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_66 :: StoryCard SickeningReality_66
sickeningReality_66 = story SickeningReality_66 Cards.sickeningReality_66

instance RunMessage SickeningReality_66 where
  runMessage msg s@(SickeningReality_66 attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      let
        (asset, enemy) =
          (Assets.jordanPerry, Enemies.jordanPerry)

      assetId <- selectJust (assetIs asset)
      enemyCard <- genCard enemy
      lid <- fieldJust AssetLocation assetId
      iids <- selectList $ investigatorAt lid
      clues <- field AssetClues assetId
      enemyCreation <- createEnemyAt_ enemyCard lid Nothing
      pushAll $
        [ InvestigatorAssignDamage
          iid
          (toSource attrs)
          DamageAny
          0
          1
        | iid <- iids
        ]
          <> [ RemoveTokens (toSource attrs) (AssetTarget assetId) Clue clues
             , PlaceTokens (toSource attrs) (LocationTarget lid) Clue clues
             , RemoveFromGame (AssetTarget assetId)
             , enemyCreation
             ]
      pure s
    _ -> SickeningReality_66 <$> runMessage msg attrs
