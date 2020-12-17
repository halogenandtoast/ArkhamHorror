{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Act.Cards.RicesWhereabouts
  ( RicesWhereabouts(..)
  , ricesWhereabouts
  )
where

import Arkham.Import

import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Helpers
import Arkham.Types.Act.Runner

newtype RicesWhereabouts = RicesWhereabouts Attrs
  deriving newtype (Show, ToJSON, FromJSON)

ricesWhereabouts :: RicesWhereabouts
ricesWhereabouts =
  RicesWhereabouts $ baseAttrs "02046" "Rice's Whereabouts" (Act 2 A) Nothing

ability :: Attrs -> Ability
ability attrs = mkAbility (toSource attrs) 1 (ActionAbility 1 Nothing)

instance ActionRunner env => HasActions env RicesWhereabouts where
  getActions iid NonFast (RicesWhereabouts x) = do
    canAffordClues <- getCanAffordCost iid (toSource x) (ClueCost 1)
    canAffordActions <- getCanAffordCost
      iid
      (toSource x)
      (ActionCost 1 Nothing mempty)
    pure
      [ ActivateCardAbilityAction iid (ability x)
      | canAffordClues && canAffordActions
      ]
  getActions iid window (RicesWhereabouts x) = getActions iid window x

instance ActRunner env => RunMessage env RicesWhereabouts where
  runMessage msg a@(RicesWhereabouts attrs@Attrs {..}) = case msg of
    TakeControlOfAsset _ assetId -> do
      cardCode <- getId assetId
      a <$ when
        (cardCode == CardCode "02060")
        (unshiftMessage $ AdvanceAct actId (toSource attrs))
    Discarded (InvestigatorTarget iid) card
      | getCardCode card == CardCode "02060" -> case card of
        EncounterCard ec -> a <$ unshiftMessages
          [RemoveFromEncounterDiscard ec, InvestigatorDrewEncounterCard iid ec]
        PlayerCard _ -> throwIO $ InvalidState "not a player card"
    UseCardAbility iid source _ 1 | isSource attrs source -> do
      playerCount <- getPlayerCount
      let discardCount = if playerCount == 1 then 10 else 5
      a <$ unshiftMessages
        [SpendClues 1 [iid], DiscardTopOfEncounterDeck iid discardCount Nothing]
    AdvanceAct aid _ | aid == actId && not actFlipped -> do
      unshiftMessage (AdvanceAct aid $ toSource attrs)
      pure . RicesWhereabouts $ attrs & sequenceL .~ Act 2 B & flippedL .~ True
    AdvanceAct aid _ | aid == actId && actFlipped -> do
      alchemyLabsInPlay <- elem (LocationName "Alchemy Labs") <$> getList ()
      agendaStep <- asks $ unAgendaStep . getStep
      completedTheHouseAlwaysWins <-
        elem "02062" . map unCompletedScenarioId <$> getSetList ()

      unshiftMessages
        $ [ PlaceLocationNamed "Alchemy Labs" | not alchemyLabsInPlay ]
        <> [ CreateEnemyAtLocationNamed "02058" (LocationName "Alchemy Labs")
           | agendaStep <= 2
           ]
        <> [ CreateStoryAssetAtLocationNamed
               "02059"
               (LocationName "Alchemy Labs")
           | completedTheHouseAlwaysWins
           ]
      leadInvestigatorId <- getLeadInvestigatorId
      unshiftMessage $ chooseOne leadInvestigatorId [NextAct aid "02047"]
      pure
        $ RicesWhereabouts
        $ attrs
        & (sequenceL .~ Act 1 B)
        & (flippedL .~ True)
    _ -> RicesWhereabouts <$> runMessage msg attrs
