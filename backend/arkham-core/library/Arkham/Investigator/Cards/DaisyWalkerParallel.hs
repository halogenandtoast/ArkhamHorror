module Arkham.Investigator.Cards.DaisyWalkerParallel (
  DaisyWalkerParallel (..),
  daisyWalkerParallel,
) where

import Arkham.Prelude

import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher

newtype DaisyWalkerParallel = DaisyWalkerParallel InvestigatorAttrs
  deriving anyclass (IsInvestigator)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

daisyWalkerParallel :: InvestigatorCard DaisyWalkerParallel
daisyWalkerParallel =
  investigator DaisyWalkerParallel Cards.daisyWalkerParallel
    $ Stats {health = 5, sanity = 7, willpower = 1, intellect = 5, combat = 2, agility = 2}

instance HasModifiersFor DaisyWalkerParallel where
  getModifiersFor target (DaisyWalkerParallel attrs) | attrs `isTarget` target = do
    tomeCount <- selectCount $ assetControlledBy attrs.id <> withTrait Tome
    pure
      $ toModifiers attrs
      $ guard (tomeCount > 0)
      *> [SkillModifier #willpower tomeCount, SanityModifier tomeCount]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue DaisyWalkerParallel where
  getChaosTokenValue iid ElderSign (DaisyWalkerParallel attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities DaisyWalkerParallel where
  getAbilities (DaisyWalkerParallel attrs) =
    [ playerLimit PerGame
        $ restrictedAbility attrs 1 (Self <> exists (AssetControlledBy You <> withTrait Tome))
        $ FastAbility Free
    ]

instance RunMessage DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      tomeAssets <- select $ assetControlledBy iid <> withTrait Tome
      allAbilities <- getAllAbilities
      let abilitiesForAsset aid = filter (isSource aid . abilitySource) allAbilities
      let pairs' = filter (notNull . snd) $ map (toSnd abilitiesForAsset) tomeAssets
      let toLabel a = AbilityLabel iid a windows' []
      player <- getPlayer iid
      pushIfAny pairs'
        $ chooseOneAtATime player
        $ map (\(tome, actions) -> targetLabel tome [chooseOne player $ map toLabel actions]) pairs'
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel iid [search iid attrs attrs [fromDiscard] (#asset <> withTrait Tome) $ DrawFound iid 1]
          , Label "Do not use Daisy's ability" []
          ]
      pure i
    _ -> DaisyWalkerParallel <$> runMessage msg attrs
