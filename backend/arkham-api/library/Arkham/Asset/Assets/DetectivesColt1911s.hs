module Arkham.Asset.Assets.DetectivesColt1911s (detectivesColt1911s, DetectivesColt1911s (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Fight
import Arkham.Investigator.Deck
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Insight, Tool))

newtype DetectivesColt1911s = DetectivesColt1911s AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

detectivesColt1911s :: AssetCard DetectivesColt1911s
detectivesColt1911s = asset DetectivesColt1911s Cards.detectivesColt1911s

instance HasModifiersFor DetectivesColt1911s where
  getModifiersFor (DetectivesColt1911s a) = do
    case a.controller of
      Nothing -> pure mempty
      Just iid -> do
        toolAssetsWithHands <-
          take 2 <$> select (assetControlledBy iid <> AssetWithTrait Tool <> AssetInSlot HandSlot)
        modifyEach a toolAssetsWithHands [DoNotTakeUpSlot HandSlot]

instance HasAbilities DetectivesColt1911s where
  getAbilities (DetectivesColt1911s a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage DetectivesColt1911s where
  runMessage msg a@(DetectivesColt1911s attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 1]
      pushAll [enabled, chooseFight]
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 1 -> True) _ -> do
      for_ attrs.controller \iid -> do
        insights <- filter (`cardMatch` (CardWithTrait Insight <> #event)) <$> field InvestigatorDiscard iid
        player <- getPlayer iid
        pushIfAny insights
          $ chooseOne player
          $ Label "Do not move an insight" []
          : [ TargetLabel
              (CardIdTarget $ toCardId insight)
              [PutCardOnBottomOfDeck iid (Deck.InvestigatorDeckByKey iid HunchDeck) $ PlayerCard insight]
            | insight <- insights
            ]
      pure a
    _ -> DetectivesColt1911s <$> runMessage msg attrs
