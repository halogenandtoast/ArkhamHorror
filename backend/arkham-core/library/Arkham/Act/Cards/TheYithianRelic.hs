module Arkham.Act.Cards.TheYithianRelic (
  TheYithianRelic (..),
  theYithianRelic,
  theYithianRelicEffect,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Types (Field (..))
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.Classes
import Arkham.Deck
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Ability
import Arkham.Helpers.Log
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Timing qualified as Timing

newtype TheYithianRelic = TheYithianRelic ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYithianRelic :: ActCard TheYithianRelic
theYithianRelic = act (3, A) TheYithianRelic Cards.theYithianRelic Nothing

instance HasAbilities TheYithianRelic where
  getAbilities (TheYithianRelic a)
    | onSide A a =
        withBaseAbilities a
          $ [ restrictedAbility
                a
                1
                ( InvestigatorExists
                    $ You
                    <> AnyInvestigator
                      [ DeckWith (HasCard $ CardWithTitle "Relic of Ages")
                      , DiscardWith (HasCard $ CardWithTitle "Relic of Ages")
                      ]
                )
                $ ActionAbility Nothing
                $ ActionCost 1
            , restrictedAbility
                a
                2
                (AssetExists $ AssetAt (YourLocation <> LocationWithoutClues) <> AssetWithTitle "Relic of Ages")
                $ ActionAbility Nothing
                $ ActionCost 1
            , restrictedAbility
                a
                3
                ( InvestigatorExists
                    $ HasMatchingAsset
                    $ AssetWithTitle
                      "Relic of Ages"
                )
                $ Objective
                $ ForcedAbility AnyWindow
            ]
  getAbilities _ = []

instance RunMessage TheYithianRelic where
  runMessage msg a@(TheYithianRelic attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      mDiscard <-
        selectOne $ InDiscardOf (InvestigatorWithId iid) <> BasicCardMatch (CardWithTitle "Relic of Ages")
      player <- getPlayer iid
      push $ case mDiscard of
        Just relic -> chooseOne player [targetLabel (toCardId relic) [addToHand iid relic]]
        Nothing -> search iid attrs iid [fromDeck] (CardWithTitle "Relic of Ages") (DrawFound iid 1)
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 _ _ -> do
      relic <- selectJust $ AssetWithTitle "Relic of Ages"
      push $ TakeControlOfAsset iid relic
      pure a
    UseCardAbility _ (isSource attrs -> True) 3 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ AdvanceActDeck (actDeckId attrs) (toSource attrs)
      whenHasRecord IchtacaIsSetAgainstYou $ do
        nexus <- selectJust $ locationIs Locations.nexusOfNKai
        ichtaca <- getSetAsideCard Enemies.ichtacaScionOfYig
        pushM $ createEnemyAt_ ichtaca nexus Nothing
      whenHasRecord AlejandroIsSetAgainstYou $ do
        aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
        alejandro <- getSetAsideCard Enemies.alejandroVela
        pushM $ createEnemyAt_ alejandro aPocketInTime Nothing
      push
        $ createCardEffect
          Cards.theYithianRelic
          Nothing
          (toSource attrs)
          ScenarioTarget

      pure a
    _ -> TheYithianRelic <$> runMessage msg attrs

newtype TheYithianRelicEffect = TheYithianRelicEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYithianRelicEffect :: EffectArgs -> TheYithianRelicEffect
theYithianRelicEffect = cardEffect TheYithianRelicEffect Cards.theYithianRelic

instance HasAbilities TheYithianRelicEffect where
  getAbilities (TheYithianRelicEffect a) =
    [ mkAbility a 1
        $ SilentForcedAbility
        $ AssetLeavesPlay Timing.AtIf
        $ AssetWithTitle "Relic of Ages"
    ]

instance RunMessage TheYithianRelicEffect where
  runMessage msg e@(TheYithianRelicEffect attrs@EffectAttrs {..}) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ _ -> do
      relic <- selectJust $ AssetWithTitle "Relic of Ages"
      locations <- field AssetCardsUnderneath relic
      pushAll
        [ DisableEffect effectId
        , ShuffleCardsIntoDeck (ScenarioDeckByKey ExplorationDeck) locations
        , ResetActDeckToStage 3
        ]
      pure e
    _ -> TheYithianRelicEffect <$> runMessage msg attrs
