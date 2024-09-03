module Arkham.Story.Cards.SickeningReality_65 where

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

newtype SickeningReality_65 = SickeningReality_65 StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sickeningReality_65 :: StoryCard SickeningReality_65
sickeningReality_65 = story SickeningReality_65 Cards.sickeningReality_65

instance RunMessage SickeningReality_65 where
  runMessage msg s@(SickeningReality_65 attrs) = case msg of
    ResolveStory _ _ story' | story' == toId attrs -> do
      let
        (asset, enemy) =
          (Assets.constanceDumaine, Enemies.constanceDumaine)

      assetId <- selectJust (assetIs asset)
      enemyCard <- genCard enemy
      lid <- fieldJust AssetLocation assetId
      iids <- select $ investigatorAt lid
      clues <- field AssetClues assetId
      enemyCreation <- createEnemyAt_ enemyCard lid Nothing
      pushAll
        $ [ InvestigatorAssignDamage
            iid
            (toSource attrs)
            DamageAny
            0
            1
          | iid <- iids
          ]
        <> [ RemoveTokens (toSource attrs) (toTarget assetId) Clue clues
           , PlaceTokens (toSource attrs) (toTarget lid) Clue clues
           , RemoveFromGame (toTarget assetId)
           , enemyCreation
           ]
      pure s
    _ -> SickeningReality_65 <$> runMessage msg attrs
