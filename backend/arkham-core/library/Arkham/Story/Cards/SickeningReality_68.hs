module Arkham.Story.Cards.SickeningReality_68 (
  SickeningReality_68 (..),
  sickeningReality_68,
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

newtype SickeningReality_68 = SickeningReality_68 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_68 :: StoryCard SickeningReality_68
sickeningReality_68 = story SickeningReality_68 Cards.sickeningReality_68

instance RunMessage SickeningReality_68 where
  runMessage msg s@(SickeningReality_68 attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      let
        (asset, enemy) =
          (Assets.sebastienMoreau, Enemies.sebastienMoreau)

      assetId <- fromJustNote "missing" <$> selectOne (assetIs asset)
      enemyCard <- genCard enemy
      lid <-
        fieldMap
          AssetLocation
          (fromJustNote "must be at a location")
          assetId
      iids <- selectList $ InvestigatorAt $ LocationWithId lid
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
          <> [ RemoveClues (AssetTarget assetId) clues
             , PlaceClues (LocationTarget lid) clues
             , RemoveFromGame (AssetTarget assetId)
             , enemyCreation
             ]
      pure s
    _ -> SickeningReality_68 <$> runMessage msg attrs
