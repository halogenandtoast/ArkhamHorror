module Arkham.Act.Cards.WhatMustBeDone (
  WhatMustBeDone (..),
  whatMustBeDone,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Movement
import Arkham.Scenario.Deck
import Data.List qualified as List

newtype WhatMustBeDone = WhatMustBeDone ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatMustBeDone :: ActCard WhatMustBeDone
whatMustBeDone = act (3, A) WhatMustBeDone Cards.whatMustBeDone Nothing

instance HasAbilities WhatMustBeDone where
  getAbilities (WhatMustBeDone attrs) =
    withBaseAbilities attrs
      $ [ mkAbility attrs 1
            $ ActionAbility []
            $ ActionCost 1
            <> ClueCostX
        , restrictedAbility
            attrs
            2
            ( InvestigatorExists
                $ LeadInvestigator
                <> InvestigatorAt (LocationWithTitle "The Black Throne" <> LocationWithoutClues)
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]

getClueCount :: Payment -> Int
getClueCount (CluePayment _ n) = n
getClueCount (Payments ps) = sum $ map getClueCount ps
getClueCount _ = 0

instance RunMessage WhatMustBeDone where
  runMessage msg a@(WhatMustBeDone attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (getClueCount -> x) -> do
      push $ DrawFromScenarioDeck iid CosmosDeck (toTarget attrs) x
      pure a
    DrewFromScenarioDeck iid _ (isTarget attrs -> True) cards -> do
      cardsWithMsgs <- traverse (traverseToSnd placeLocation) cards
      player <- getPlayer iid
      pushAll
        [ FocusCards $ map flipCard cards
        , chooseOrRunOne
            player
            [ targetLabel
              (toCardId card)
              [ UnfocusCards
              , ShuffleCardsIntoDeck (Deck.ScenarioDeckByKey CosmosDeck) (List.delete card cards)
              , placement
              , RevealLocation (Just iid) lid
              , RunCosmos iid lid [Move $ move (toAbilitySource attrs 1) iid lid]
              ]
            | (card, (lid, placement)) <- cardsWithMsgs
            ]
        ]
      pure a
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      youAcceptedYourFate <- getHasRecord YouHaveAcceptedYourFate
      haveMesmerizingFlute <- MesmerizingFlute `inRecordSet` MementosDiscovered
      haveRitualComponents <- RitualComponents `inRecordSet` MementosDiscovered

      if count id [youAcceptedYourFate, haveMesmerizingFlute, haveRitualComponents] >= 2
        then push R2
        else do
          youRejectedYourFate <- getHasRecord YouHaveRejectedYourFate
          haveScrapOfTornShadow <- ScrapOfTornShadow `inRecordSet` MementosDiscovered
          haveWispOfSpectralMist <- WispOfSpectralMist `inRecordSet` MementosDiscovered
          push
            $ if count id [youRejectedYourFate, haveScrapOfTornShadow, haveWispOfSpectralMist] >= 2
              then R3
              else R4

      pure a
    _ -> WhatMustBeDone <$> runMessage msg attrs
