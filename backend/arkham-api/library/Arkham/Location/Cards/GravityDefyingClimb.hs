module Arkham.Location.Cards.GravityDefyingClimb (gravityDefyingClimb) where

import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
import Arkham.Cost
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.I18n
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Placement

newtype GravityDefyingClimb = GravityDefyingClimb LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gravityDefyingClimb :: LocationCard GravityDefyingClimb
gravityDefyingClimb =
  symbolLabel
    $ locationWith GravityDefyingClimb Cards.gravityDefyingClimb 4 (Static 0)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 4) "The Knotted Tower"

instance HasAbilities GravityDefyingClimb where
  getAbilities (GravityDefyingClimb a) =
    extendRevealed a []

instance HasModifiersFor GravityDefyingClimb where
  getModifiersFor (GravityDefyingClimb a) = do
    when a.revealed do
      modifySelect
        a
        (ConcealedCardOneOf $ map (ConcealedCardWithPlacement . InPosition) [Pos (-1) 1, Pos 1 1])
        [ScenarioModifier "doNotRemove"]

instance RunMessage GravityDefyingClimb where
  runMessage msg l@(GravityDefyingClimb attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnantsL]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      if c.placement `elem` [InPosition (Pos (-1) 1), InPosition (Pos 1 1)]
        then do
          let next = getLocationMetaDefault CityOfRemnantsL attrs
          if next == CityOfRemnantsL
            then do
              otherCards <- select $ ConcealedCardAny <> NotConcealedCard (ConcealedCardWithId c.id)
              chooseOneM iid do
                for_ otherCards \otherCard ->
                  targeting otherCard.id do
                    exposeConcealed iid attrs otherCard.id
                    do_ $ PlaceConcealedCard iid otherCard.id otherCard.placement
                withI18n skip_
              do_ $ PlaceConcealedCard iid c.id c.placement
              pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsM
            else do
              allCards <- selectMap (.id) ConcealedCardAny
              for_ allCards $ doStep1 1 . lookAtRevealed iid ScenarioSource
              do_ $ PlaceConcealedCard iid c.id c.placement
              pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsL
        else pure l
    ScenarioSpecific "exposed[CityOfRemnantsM]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      if c.placement `elem` [InPosition (Pos (-1) 1), InPosition (Pos 1 1)]
        then do
          let next = getLocationMetaDefault CityOfRemnantsL attrs
          if next == CityOfRemnantsM
            then do
              otherCards <- select $ ConcealedCardAny <> NotConcealedCard (ConcealedCardWithId c.id)
              chooseOneM iid do
                for_ otherCards \otherCard ->
                  targeting otherCard.id do
                    exposeConcealed iid attrs otherCard.id
                    do_ $ PlaceConcealedCard iid otherCard.id otherCard.placement
                withI18n skip_
              do_ $ PlaceConcealedCard iid c.id c.placement
              pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsR
            else do
              allCards <- selectMap (.id) ConcealedCardAny
              for_ allCards $ doStep1 1 . lookAtRevealed iid ScenarioSource
              do_ $ PlaceConcealedCard iid c.id c.placement
              pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsL
        else pure l
    ScenarioSpecific "exposed[CityOfRemnantsR]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      if c.placement `elem` [InPosition (Pos (-1) 1), InPosition (Pos 1 1)]
        then do
          let next = getLocationMetaDefault CityOfRemnantsL attrs
          if next == CityOfRemnantsR
            then do
              theToweringVertex <- selectJust $ locationIs Cards.theToweringVertexRuinousConflux
              gameModifier ScenarioSource theToweringVertex (ScenarioModifier "canEnter")
              do_ $ PlaceConcealedCard iid c.id c.placement
              pure l
            else do
              allCards <- selectMap (.id) ConcealedCardAny
              for_ allCards $ doStep1 1 . lookAtRevealed iid ScenarioSource
              do_ $ PlaceConcealedCard iid c.id c.placement
              pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsL
        else pure l
    ScenarioSpecific "exposed[decoy]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      if c.placement `elem` [InPosition (Pos (-1) 1), InPosition (Pos 1 1)]
        then do
          allCards <- selectMap (.id) ConcealedCardAny
          for_ allCards $ doStep1 1 . lookAtRevealed iid ScenarioSource
          do_ $ PlaceConcealedCard iid c.id c.placement
          pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsL
        else pure l
    _ -> GravityDefyingClimb <$> liftRunMessage msg attrs
