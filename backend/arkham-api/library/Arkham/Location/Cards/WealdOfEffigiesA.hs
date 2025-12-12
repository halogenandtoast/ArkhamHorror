module Arkham.Location.Cards.WealdOfEffigiesA (wealdOfEffigiesA) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Helpers
import Arkham.Card
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement
import Arkham.Scenarios.WithoutATrace.Helpers
import Arkham.Strategy

newtype WealdOfEffigiesA = WealdOfEffigiesA LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wealdOfEffigiesA :: LocationCard WealdOfEffigiesA
wealdOfEffigiesA =
  symbolLabel
    $ locationWith WealdOfEffigiesA Cards.wealdOfEffigiesA 4 (PerPlayer 1) connectsToAdjacent

instance HasAbilities WealdOfEffigiesA where
  getAbilities (WealdOfEffigiesA a) =
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

instance RunMessage WealdOfEffigiesA where
  runMessage msg l@(WealdOfEffigiesA attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnants]" v -> do
      let (iid, dir, lid) = toResult v
      when (lid == attrs.id) do
        case dir of
          LeftPosition -> do
            whenMatch iid InvestigatorWithAnyResources do
              exposedInShadows iid attrs $ loseResources iid (attrs.ability (-1)) 2
          MiddlePosition -> do
            whenMatch iid InvestigatorWithAnyActionsRemaining do
              exposedInShadows iid attrs $ loseActions iid (attrs.ability (-1)) 1
          RightPosition -> pure ()
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
      chooseOneM iid $ scenarioI18n do
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
    _ -> WealdOfEffigiesA <$> liftRunMessage msg attrs
