module Arkham.Location.Cards.WealdOfEffigiesB (wealdOfEffigiesB) where

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher hiding (AssetCard)
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Strategy

newtype WealdOfEffigiesB = WealdOfEffigiesB LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wealdOfEffigiesB :: LocationCard WealdOfEffigiesB
wealdOfEffigiesB =
  symbolLabel
    $ locationWith WealdOfEffigiesB Cards.wealdOfEffigiesB 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WealdOfEffigiesB where
  getAbilities (WealdOfEffigiesB a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after You (be a)
      , groupLimit PerGame
          $ restricted
            a
            2
            ( Here
                <> thisExists a LocationWithoutClues
                <> oneOf [LocationCount 2 (LocationWithPlacement InTheShadows), ConcealedCardCount 2 ConcealedCardAny]
            )
          $ FastAbility Free
      ]

instance RunMessage WealdOfEffigiesB where
  runMessage msg l@(WealdOfEffigiesB attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> pure ()
          MiddlePosition -> do
            cards <- select $ inHandOf NotForPlay iid <> basic NonWeakness
            unless (null cards) do
              exposedInShadows iid attrs $ chooseTargetM iid cards $ hollow iid
          RightPosition -> do
            assets <- selectWithField AssetCard $ assetControlledBy iid <> AssetCanLeavePlayByNormalMeans
            unless (null assets) do
              exposedInShadows iid attrs do
                chooseOneM iid $ for assets \(aid, card) -> targeting aid $ hollow iid card
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lookAt iid (attrs.ability 1) iid [fromTopOfDeck 1] #any (defer attrs IsNotDraw)
      pure l
    SearchFound iid (isTarget attrs -> True) _ cards | notNull cards -> do
      focusCards cards do
        continue_ iid
        for_ cards \card -> do
          when (cardMatch card NonWeakness) $ hollow iid card
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      miniCards <- select ConcealedCardAny
      locations <- select $ LocationWithPlacement InTheShadows
      chooseOneM iid $ campaignI18n do
        labeledValidate' (notNull miniCards) "wealdOfEffigies.miniCards" do
          chooseOneM iid do
            for_ (eachWithRest miniCards) \(card, rest) -> do
              targeting card do
                chooseTargetM iid rest \other -> do
                  scenarioSpecific "swapMiniCards" (card.id, other.id)
                  do_ $ PlaceConcealedCard iid card.id other.placement
                  do_ $ PlaceConcealedCard iid other.id card.placement
        labeled' "wealdOfEffigies.locations" do
          chooseOneM iid do
            for_ (eachWithRest locations) \(x, rest) -> do
              targeting x do
                chooseTargetM iid rest \y -> do
                  scenarioSpecific "swapLocations" (x, y)
      pure l
    _ -> WealdOfEffigiesB <$> liftRunMessage msg attrs
