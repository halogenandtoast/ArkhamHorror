module Arkham.Location.Cards.ResearchSite (researchSite) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Location)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ResearchSite = ResearchSite LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

researchSite :: LocationCard ResearchSite
researchSite =
  locationWith ResearchSite Cards.researchSite 6 (Static 0)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities ResearchSite where
  getAbilities (ResearchSite a) =
    extendRevealed
      a
      [ groupLimit PerGame
          $ restricted a 1 (exists $ "Catacombs" <> UnrevealedLocation)
          $ freeReaction
          $ SkillTestResult #after You (whileInvestigating a) #success
      , mkAbility a 2 $ forced $ RevealLocation #when Anyone (be a)
      ]

instance RunMessage ResearchSite where
  runMessage msg l@(ResearchSite attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseSelectM iid ("Catacombs" <> UnrevealedLocation) reveal
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      case drewCards.cards of
        [_] ->
          chooseSelectM
            iid
            (mapOneOf (`LocationWithSpaceInDirection` not_ (be attrs)) [Above, Below, RightOf])
            (`forTarget` msg)
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    ForTarget (LocationTarget lid) (DrewCards iid drewCards) | maybe False (isTarget attrs) drewCards.target -> do
      attrs' <- getAttrs @Location lid
      case drewCards.cards of
        [card] -> do
          aboveEmpty <- directionEmpty attrs' Above
          belowEmpty <- directionEmpty attrs' Below
          rightEmpty <- directionEmpty attrs' RightOf
          chooseOrRunOneM iid $ scenarioI18n do
            when aboveEmpty $ labeled' "above" $ placeAtDirection_ Above attrs' card
            when belowEmpty $ labeled' "below" $ placeAtDirection_ Below attrs' card
            when rightEmpty $ labeled' "right" $ placeAtDirection_ RightOf attrs' card
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> ResearchSite <$> liftRunMessage msg attrs
