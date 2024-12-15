module Arkham.Act.Cards.HuntressOfTheEztli (
  HuntressOfTheEztli (..),
  huntressOfTheEztli,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Trait (Trait (Ruins))

newtype HuntressOfTheEztli = HuntressOfTheEztli ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

huntressOfTheEztli :: ActCard HuntressOfTheEztli
huntressOfTheEztli =
  act (2, A) HuntressOfTheEztli Cards.huntressOfTheEztli Nothing

instance HasAbilities HuntressOfTheEztli where
  getAbilities (HuntressOfTheEztli x)
    | onSide A x =
        [ mkAbility x 1 $ Objective $ forced $ EnemyDefeated #after Anyone ByAny $ EnemyWithTitle "Ichtaca"
        , restrictedAbility
            x
            2
            (exists $ EnemyWithTitle "Ichtaca" <> EnemyWithClues (AtLeast $ PerPlayer 1))
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage HuntressOfTheEztli where
  runMessage msg a@(HuntressOfTheEztli attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithOther)
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      a <$ push (AdvanceAct (toId a) (InvestigatorSource iid) AdvancedWithOther)
    AdvanceAct aid _ _ | aid == toId a && onSide B attrs -> do
      ichtacaDefeated <- inVictoryDisplay "Ichtaca"
      ruins <- getSetAsideCardsMatching $ CardWithTrait Ruins
      if ichtacaDefeated
        then do
          lead <- getLeadPlayer
          investigatorIds <- getInvestigators
          alejandroVela <-
            fromJustNote "Alejandro Vela was not set aside"
              . listToMaybe
              <$> getSetAsideCardsMatching (CardWithTitle "Alejandro Vela")
          pushAll
            [ Remember YouFoughtWithIchtaca
            , chooseOne
                lead
                [ targetLabel iid [TakeControlOfSetAsideAsset iid alejandroVela]
                | iid <- investigatorIds
                ]
            , AddChaosToken Tablet
            , ShuffleCardsIntoDeck
                (Deck.ScenarioDeckByKey ExplorationDeck)
                ruins
            , AdvanceToAct
                (actDeckId attrs)
                Acts.theGuardedRuins
                A
                (toSource attrs)
            ]
        else do
          itchtaca <- selectJust $ enemyIs Enemies.ichtaca
          pushAll
            [ AddToVictory (EnemyTarget itchtaca)
            , Remember IchtachaIsLeadingTheWay
            , AddChaosToken Cultist
            , ShuffleCardsIntoDeck
                (Deck.ScenarioDeckByKey ExplorationDeck)
                ruins
            , AdvanceToAct
                (actDeckId attrs)
                Acts.searchForTheRuins
                A
                (toSource attrs)
            ]
      pure a
    _ -> HuntressOfTheEztli <$> runMessage msg attrs
