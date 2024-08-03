module Arkham.Investigator.Cards.DaisyWalkerParallel (DaisyWalkerParallel (..), daisyWalkerParallel) where

import Arkham.Ability
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Strategy

newtype DaisyWalkerParallel = DaisyWalkerParallel InvestigatorAttrs
  deriving anyclass IsInvestigator
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

daisyWalkerParallel :: InvestigatorCard DaisyWalkerParallel
daisyWalkerParallel =
  investigator DaisyWalkerParallel Cards.daisyWalkerParallel
    $ Stats {health = 5, sanity = 7, willpower = 1, intellect = 5, combat = 2, agility = 2}

instance HasModifiersFor DaisyWalkerParallel where
  getModifiersFor target (DaisyWalkerParallel attrs) | attrs `isTarget` target = do
    maybeModified attrs do
      tomeCount <- lift $ selectCount $ assetControlledBy attrs.id <> #tome
      guard $ tomeCount > 0
      pure [SkillModifier #willpower tomeCount, SanityModifier tomeCount]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue DaisyWalkerParallel where
  getChaosTokenValue iid ElderSign (DaisyWalkerParallel attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities DaisyWalkerParallel where
  getAbilities (DaisyWalkerParallel attrs) =
    [ playerLimit PerGame
        $ restrictedAbility attrs 1 (Self <> exists (AssetControlledBy You <> #tome))
        $ FastAbility Free
    ]

instance RunMessage DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      tomeAssets <- select $ assetControlledBy iid <> #tome
      allAbilities <- getAllAbilities
      let abilitiesForAsset aid = filter (isSource aid . abilitySource) allAbilities
      let pairs' = filter (notNull . snd) $ map (toSnd abilitiesForAsset) tomeAssets
      let toLabel a = AbilityLabel iid a windows' [] []
      chooseOneAtATimeM iid do
        for_ pairs' \(tome, actions) -> do
          targeting tome do
            chooseOne iid $ map toLabel actions
      pure i
    ElderSignEffect iid | attrs `is` iid -> do
      chooseOneM iid do
        targeting iid $ search iid attrs attrs [fromDiscard] (basic $ #asset <> #tome) $ DrawFound iid 1
        labeled "Do not use Daisy's ability" nothing
      pure i
    _ -> DaisyWalkerParallel <$> liftRunMessage msg attrs
