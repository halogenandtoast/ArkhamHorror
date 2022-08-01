module Arkham.Act.Cards.HuntressOfTheEztli
  ( HuntressOfTheEztli(..)
  , huntressOfTheEztli
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Criteria
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message hiding ( EnemyDefeated )
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Source
import Arkham.Timing qualified as Timing
import Arkham.Token
import Arkham.Trait ( Trait (Ruins) )

newtype HuntressOfTheEztli = HuntressOfTheEztli ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntressOfTheEztli :: ActCard HuntressOfTheEztli
huntressOfTheEztli =
  act (2, A) HuntressOfTheEztli Cards.huntressOfTheEztli Nothing

instance HasAbilities HuntressOfTheEztli where
  getAbilities (HuntressOfTheEztli x) =
    [ mkAbility x 1 $ Objective $ ForcedAbility
      (EnemyDefeated Timing.When Anyone $ EnemyWithTitle "Ichtaca")
    , restrictedAbility
        x
        2
        (EnemyCriteria $ EnemyExists $ EnemyWithTitle "Ichtaca" <> EnemyWithClues
          (AtLeast $ PerPlayer 1)
        )
      $ Objective
      $ ForcedAbility AnyWindow
    ]

instance RunMessage HuntressOfTheEztli where
  runMessage msg a@(HuntressOfTheEztli attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithOther)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      ichtacaHasCluesOnHer <- selectAny $ EnemyWithTitle "Ichtaca"
      ruins <- getSetAsideCardsMatching $ CardWithTrait Ruins
      if ichtacaHasCluesOnHer
        then do
          pushAll
            [ Remember IchtachaIsLeadingTheWay
            , AddToken Cultist
            , ShuffleCardsIntoDeck
              (Deck.ScenarioDeckByKey ExplorationDeck)
              ruins
            , AdvanceToAct
              (actDeckId attrs)
              Acts.searchForTheRuins
              A
              (toSource attrs)
            ]
        else do
          leadInvestigatorId <- getLeadInvestigatorId
          investigatorIds <- getInvestigatorIds
          alejandroVela <-
            fromJustNote "The Organist was not set aside"
            . listToMaybe
            <$> getSetAsideCardsMatching (CardWithTitle "Alejandro Vela")
          pushAll
            [ Remember YouFoughtWithIchtaca
            , chooseOne
              leadInvestigatorId
              [ targetLabel
                  iid
                  [TakeControlOfSetAsideAsset iid alejandroVela]
              | iid <- investigatorIds
              ]
            , AddToken Tablet
            , ShuffleCardsIntoDeck
              (Deck.ScenarioDeckByKey ExplorationDeck)
              ruins
            , AdvanceToAct
              (actDeckId attrs)
              Acts.theGuardedRuins
              A
              (toSource attrs)
            ]
      pure a
    _ -> HuntressOfTheEztli <$> runMessage msg attrs
