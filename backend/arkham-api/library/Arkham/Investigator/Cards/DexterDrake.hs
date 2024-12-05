module Arkham.Investigator.Cards.DexterDrake (dexterDrake, dexterDrakeEffect, DexterDrake (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Effect.Import
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (Discarded, DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Name hiding (labeled)
import Arkham.Placement

newtype DexterDrake = DexterDrake InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

dexterDrake :: InvestigatorCard DexterDrake
dexterDrake =
  investigator DexterDrake Cards.dexterDrake
    $ Stats {health = 6, sanity = 8, willpower = 5, intellect = 2, combat = 3, agility = 2}

instance HasAbilities DexterDrake where
  getAbilities (DexterDrake a) =
    [ playerLimit PerRound
        $ restrictedAbility
          a
          1
          ( Self
              <> DuringTurn You
              <> oneOf
                [ PlayableCardExistsWithCostReduction
                    (Reduce 1)
                    (HandCardWithDifferentTitleFromAtLeastOneAsset You AnyAsset AnyCard)
                , ExtendedCardExists (InHandOf You <> basic (cardIs Assets.occultScraps))
                ]
          )
        $ FastAbility
        $ DiscardAssetCost
        $ AssetWithDifferentTitleFromAtLeastOneCardInHand
          You
          (PlayableCardWithCostReduction NoAction 1 #asset)
          (AssetControlledBy You)
    ]

instance HasChaosTokenValue DexterDrake where
  getChaosTokenValue iid ElderSign (DexterDrake attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 2
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

toCardPaid :: Payment -> Card
toCardPaid (DiscardPayment [(_, c)]) = c
toCardPaid _ = error "Invalid payment"

instance RunMessage DexterDrake where
  runMessage msg i@(DexterDrake attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (toCardPaid -> card) -> do
      cards <-
        select
          $ ExtendedCardWithOneOf
            [ PlayableCardWithCostReduction
                NoAction
                1
                (inHandOf attrs.id <> basic (#asset <> not_ (CardWithTitle $ toTitle card)))
            , inHandOf iid <> basic (cardIs Assets.occultScraps)
            ]
      chooseOrRunOneM iid do
        targets cards \c -> do
          createCardEffect Cards.dexterDrake Nothing attrs (toCardId c)
          playCardPayingCost iid c
      pure i
    ElderSignEffect iid | iid == toId attrs -> do
      assets <- select $ AssetWithPlacement (InPlayArea iid) <> AssetCanLeavePlayByNormalMeans
      when (notNull assets) do
        chooseOneM iid do
          labeled "Do no return an asset" nothing
          targets assets \asset -> do
            push $ ReturnToHand iid (toTarget asset)
            drawCardsIfCan iid attrs 1
      pure i
    _ -> DexterDrake <$> liftRunMessage msg attrs

newtype DexterDrakeEffect = DexterDrakeEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

dexterDrakeEffect :: EffectArgs -> DexterDrakeEffect
dexterDrakeEffect = cardEffect DexterDrakeEffect Cards.dexterDrake

instance HasModifiersFor DexterDrakeEffect where
  getModifiersFor (DexterDrakeEffect a) = case a.target of
    CardIdTarget cid -> do
      card <- getCard cid
      modified_ a a.target
        $ [ReduceCostOf (CardWithId cid) 1]
        <> [CanPlayWithOverride (CriteriaOverride NoRestriction) | card `cardMatch` cardIs Assets.occultScraps]
    _ -> pure mempty

instance RunMessage DexterDrakeEffect where
  runMessage msg e@(DexterDrakeEffect attrs) = runQueueT $ case msg of
    ResolvedCard _ card | CardIdTarget (toCardId card) == attrs.target -> do
      disableReturn e
    _ -> DexterDrakeEffect <$> liftRunMessage msg attrs
