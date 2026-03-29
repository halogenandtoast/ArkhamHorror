module Arkham.Investigator.Cards.TommyMuldoon2 (tommyMuldoon2) where

import Arkham.Ability
import Arkham.Asset.Uses
import {-# SOURCE #-} Arkham.GameEnv (getCard)
import Arkham.GameValue
import Arkham.Helpers.Playable (getPlayableCardsMatch)
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.Trait (Trait (Firearm))
import Arkham.Window (duringTurnWindow)

newtype TommyMuldoon2 = TommyMuldoon2 InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

tommyMuldoon2 :: InvestigatorCard TommyMuldoon2
tommyMuldoon2 =
  investigator TommyMuldoon2 Cards.tommyMuldoon2
    $ Stats {health = 9, sanity = 6, willpower = 3, intellect = 3, combat = 4, agility = 2}

instance HasAbilities TommyMuldoon2 where
  getAbilities (TommyMuldoon2 a) =
    [ restricted
        a
        1
        ( Self
            <> oneOf
              [ exists (#firearm <> AssetControlledBy You <> DiscardableAsset)
                  <> PlayableCardExists
                    (UnpaidCost NoAction)
                    (InHandOf ForPlay You <> basic #firearm)
              , youExist (InvestigatorWithSpendableResources $ AtLeast $ Static 1)
                  <> PlayableCardExists
                    (AuxiliaryCost (ResourceCost 1) $ UnpaidCost NoAction)
                    (InHandOf ForPlay You <> basic #firearm)
              ]
        )
        $ actionAbilityWithCost
        $ OrCost
          [ CostOnlyWhen
              ( PlayableCardExists
                  (AuxiliaryCost (ResourceCost 1) $ UnpaidCost NoAction)
                  (InHandOf ForPlay You <> basic #firearm)
              )
              $ ResourceCost 1
          , DiscardAssetCost (#firearm <> AssetControlledBy You)
          ]
    ]

instance HasChaosTokenValue TommyMuldoon2 where
  getChaosTokenValue iid ElderSign (TommyMuldoon2 attrs)
    | iid == toId attrs =
        pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage TommyMuldoon2 where
  runMessage msg i@(TommyMuldoon2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      cards <-
        getPlayableCardsMatch attrs iid (UnpaidCost NoAction) [duringTurnWindow iid] Firearm
      chooseTargetM iid cards $ handleTarget iid (attrs.ability 1)
      pure i
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (CardIdTarget cid) -> do
      card <- getCard cid
      cardResolutionModifier card (attrs.ability 1) card (AdditionalStartingUses 2)
      playCardPayingCost iid card
      pure i
    ElderSignEffect iid | iid == attrs.id -> do
      assets <- select (assetControlledBy iid <> AssetWithUseType Ammo)
      unless (null assets) do
        chooseTargetM iid assets \asset -> addUses ElderSign asset Ammo 1
      pure i
    _ -> TommyMuldoon2 <$> liftRunMessage msg attrs
