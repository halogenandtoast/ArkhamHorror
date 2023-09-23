module Arkham.Asset.Cards.DetectivesColt1911s (
  detectivesColt1911s,
  DetectivesColt1911s (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Investigator.Deck
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Projection
import Arkham.SkillType
import Arkham.Trait (Trait (Insight, Tool))

newtype DetectivesColt1911s = DetectivesColt1911s AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectivesColt1911s :: AssetCard DetectivesColt1911s
detectivesColt1911s = asset DetectivesColt1911s Cards.detectivesColt1911s

instance HasModifiersFor DetectivesColt1911s where
  getModifiersFor (AssetTarget aid) (DetectivesColt1911s a) = do
    case assetController a of
      Nothing -> pure []
      Just iid -> do
        toolAssetsWithHands <-
          selectList
            $ assetControlledBy iid
            <> AssetWithTrait Tool
            <> AssetInSlot HandSlot
        pure
          $ toModifiers
            a
            [DoNotTakeUpSlot HandSlot | aid `elem` take 2 toolAssetsWithHands]
  getModifiersFor _ _ = pure []

instance HasAbilities DetectivesColt1911s where
  getAbilities (DetectivesColt1911s a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility (Just Action.Fight)
        $ ActionCost 1
        <> UseCost (AssetWithId $ toId a) Ammo 1
    ]

instance RunMessage DetectivesColt1911s where
  runMessage msg a@(DetectivesColt1911s attrs) = case msg of
    UseCardAbility iid source 1 _ _
      | isSource attrs source ->
          a
            <$ pushAll
              [ skillTestModifiers
                  attrs
                  (InvestigatorTarget iid)
                  [DamageDealt 1, SkillModifier SkillCombat 1]
              , ChooseFightEnemy iid (AbilitySource source 1) Nothing SkillCombat mempty False
              ]
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ (assetController attrs) $ \iid -> do
        insights <-
          filter (`cardMatch` (CardWithTrait Insight <> CardWithType EventType))
            <$> field InvestigatorDiscard iid
        unless (null insights) $ do
          push
            $ chooseOne iid
            $ Label "Do not move an insight" []
            : [ TargetLabel
                (CardIdTarget $ toCardId insight)
                [PutCardOnBottomOfDeck iid (Deck.InvestigatorDeckByKey iid HunchDeck) $ PlayerCard insight]
              | insight <- insights
              ]
      pure a
    _ -> DetectivesColt1911s <$> runMessage msg attrs
