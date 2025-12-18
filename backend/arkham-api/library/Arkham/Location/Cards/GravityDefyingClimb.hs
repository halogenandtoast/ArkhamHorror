module Arkham.Location.Cards.GravityDefyingClimb (gravityDefyingClimb) where

import Arkham.Ability
import Arkham.Campaigns.TheScarletKeys.Concealed
import Arkham.Campaigns.TheScarletKeys.Concealed.Helpers
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
  locationWith GravityDefyingClimb Cards.gravityDefyingClimb 4 (Static 0)
    $ costToEnterUnrevealedL
    .~ GroupClueCost (PerPlayer 4) "The Knotted Tower"

instance HasAbilities GravityDefyingClimb where
  getAbilities (GravityDefyingClimb a) =
    extendRevealed a []

concealedPositions :: [Pos]
concealedPositions = [Pos (-1) 1, Pos 1 1]

instance HasModifiersFor GravityDefyingClimb where
  getModifiersFor (GravityDefyingClimb a) = do
    when a.revealed do
      modifySelect
        a
        (ConcealedCardOneOf $ map (ConcealedCardWithPlacement . InPosition) concealedPositions)
        [ScenarioModifier "doNotRemove"]

withAdjacentConcealed
  :: ReverseQueue m
  => LocationAttrs
  -> Value
  -> ConcealedCardKind
  -> (InvestigatorId -> ConcealedCard -> m LocationAttrs)
  -> m GravityDefyingClimb
withAdjacentConcealed attrs v ckind f = do
  let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
  if maybe True (`notElem` concealedPositions) c.position
    then pure $ GravityDefyingClimb attrs
    else do
      chooseOneM iid $ abilityLabeled_ iid (mkAbility attrs (-1) $ forced AnyWindow)
      attrs' <-
        if getLocationMetaDefault CityOfRemnantsL attrs == ckind
          then f iid c
          else do
            allCards <- selectMap (.id) ConcealedCardAny
            for_ allCards $ doStep1 1 . lookAtRevealed iid ScenarioSource
            pure $ attrs & setMeta CityOfRemnantsL
      do_ $ PlaceConcealedCard iid c.id c.placement -- fix concealed placement
      pure $ GravityDefyingClimb attrs'

instance RunMessage GravityDefyingClimb where
  runMessage msg l@(GravityDefyingClimb attrs) = runQueueT $ case msg of
    ScenarioSpecific "exposed[CityOfRemnantsL]" v -> do
      withAdjacentConcealed attrs v CityOfRemnantsL \iid c -> do
        otherCards <- select $ ConcealedCardAny <> NotConcealedCard (ConcealedCardWithId c.id)
        chooseOneM iid do
          for_ otherCards \otherCard ->
            targeting otherCard.id do
              exposeConcealed iid attrs otherCard.id
              do_ $ PlaceConcealedCard iid otherCard.id otherCard.placement
          withI18n skip_
        pure $ attrs & setMeta CityOfRemnantsM
    ScenarioSpecific "exposed[CityOfRemnantsM]" v -> do
      withAdjacentConcealed attrs v CityOfRemnantsM \iid c -> do
        otherCards <- select $ ConcealedCardAny <> NotConcealedCard (ConcealedCardWithId c.id)
        chooseOneM iid do
          for_ otherCards \otherCard ->
            targeting otherCard.id do
              exposeConcealed iid attrs otherCard.id
              do_ $ PlaceConcealedCard iid otherCard.id otherCard.placement
          withI18n skip_
        pure $ attrs & setMeta CityOfRemnantsR
    ScenarioSpecific "exposed[CityOfRemnantsR]" v -> do
      withAdjacentConcealed attrs v CityOfRemnantsR \_iid _c -> do
        theToweringVertex <- selectJust $ locationIs Cards.theToweringVertexRuinousConflux
        gameModifier ScenarioSource theToweringVertex (ScenarioModifier "canEnter")
        pure attrs
    ScenarioSpecific "exposed[decoy]" v -> do
      let (iid, c) :: (InvestigatorId, ConcealedCard) = toResult v
      if maybe False (`elem` concealedPositions) c.position
        then do
          allCards <- selectMap (.id) ConcealedCardAny
          for_ allCards $ doStep1 1 . lookAtRevealed iid ScenarioSource
          do_ $ PlaceConcealedCard iid c.id c.placement
          pure $ GravityDefyingClimb $ attrs & setMeta CityOfRemnantsL
        else pure l
    _ -> GravityDefyingClimb <$> liftRunMessage msg attrs
