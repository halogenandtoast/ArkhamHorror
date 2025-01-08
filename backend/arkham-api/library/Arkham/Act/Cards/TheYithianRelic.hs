module Arkham.Act.Cards.TheYithianRelic (theYithianRelic, theYithianRelicEffect) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Types (Field (..))
import Arkham.Campaigns.TheForgottenAge.Key
import Arkham.Effect.Import
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Strategy

newtype TheYithianRelic = TheYithianRelic ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYithianRelic :: ActCard TheYithianRelic
theYithianRelic = act (3, A) TheYithianRelic Cards.theYithianRelic Nothing

instance HasAbilities TheYithianRelic where
  getAbilities (TheYithianRelic a) =
    extend a
      $ guard (onSide A a)
      *> [ restricted
            a
            1
            (youExist $ oneOf [DeckWith (HasCard "Relic of Ages"), DiscardWith (HasCard "Relic of Ages")])
            actionAbility
         , restricted
            a
            2
            (exists $ asset_ $ at_ (YourLocation <> LocationWithoutClues) <> "Relic of Ages")
            actionAbility
         , restricted a 3 (exists $ HasMatchingAsset "Relic of Ages")
            $ Objective
            $ ForcedAbility AnyWindow
         ]

instance RunMessage TheYithianRelic where
  runMessage msg a@(TheYithianRelic attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      selectOne (inDiscardOf iid <> basic "Relic of Ages") >>= \case
        Just relic -> focusCard relic \unfocus -> do
          chooseOneM iid $ targeting relic do
            push unfocus
            addToHand iid (only relic)
        Nothing -> search iid attrs iid [fromDeck] (basic "Relic of Ages") (DrawFound iid 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      takeControlOfAsset iid =<< selectJust (AssetWithTitle "Relic of Ages")
      pure a
    UseThisAbility _ (isSource attrs -> True) 3 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      whenHasRecord IchtacaIsSetAgainstYou do
        nexus <- selectJust $ locationIs Locations.nexusOfNKai
        ichtaca <- getSetAsideCard Enemies.ichtacaScionOfYig
        createEnemyAt_ ichtaca nexus
      whenHasRecord AlejandroIsSetAgainstYou do
        aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
        alejandro <- getSetAsideCard Enemies.alejandroVela
        createEnemyAt_ alejandro aPocketInTime
      createCardEffect Cards.theYithianRelic Nothing attrs ScenarioTarget
      advanceActDeck attrs
      pure a
    _ -> TheYithianRelic <$> liftRunMessage msg attrs

newtype TheYithianRelicEffect = TheYithianRelicEffect EffectAttrs
  deriving anyclass (IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theYithianRelicEffect :: EffectArgs -> TheYithianRelicEffect
theYithianRelicEffect = cardEffect TheYithianRelicEffect Cards.theYithianRelic

instance HasAbilities TheYithianRelicEffect where
  getAbilities (TheYithianRelicEffect a) =
    [mkAbility a 1 $ SilentForcedAbility $ AssetLeavesPlay #at "Relic of Ages"]

instance RunMessage TheYithianRelicEffect where
  runMessage msg e@(TheYithianRelicEffect attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      disable attrs
      relic <- selectJust $ AssetWithTitle "Relic of Ages"
      shuffleCardsIntoDeck ExplorationDeck =<< field AssetCardsUnderneath relic
      push $ ResetActDeckToStage 3
      pure e
    _ -> TheYithianRelicEffect <$> liftRunMessage msg attrs
