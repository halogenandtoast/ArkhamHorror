module Arkham.Investigator.Cards.DaisyWalkerParallel (
  DaisyWalkerParallel (..),
  daisyWalkerParallel,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Types (Field (..))
import Arkham.Card.CardType
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Zone qualified as Zone

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
      $ guard (tomeCount > 0) *> [SkillModifier #willpower tomeCount, SanityModifier tomeCount]
  getModifiersFor _ _ = pure []

instance HasChaosTokenValue DaisyWalkerParallel where
  getChaosTokenValue iid ElderSign (DaisyWalkerParallel attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 1)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance HasAbilities DaisyWalkerParallel where
  getAbilities (DaisyWalkerParallel attrs) =
    [ playerLimit PerGame
        $ restrictedAbility attrs 1 (Self <> AssetExists (AssetControlledBy You <> AssetWithTrait Tome))
        $ FastAbility Free
    ]

instance RunMessage DaisyWalkerParallel where
  runMessage msg i@(DaisyWalkerParallel attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 windows' _ -> do
      tomeAssets <- filterByField AssetTraits (member Tome) (setToList attrs.assets)
      allAbilities <- getAllAbilities
      let abilitiesForAsset aid = filter ((AssetSource aid ==) . abilitySource) allAbilities
      let pairs' = filter (notNull . snd) $ map (\a -> (a, abilitiesForAsset a)) tomeAssets
      unless (null pairs')
        $ push
        $ chooseOneAtATime iid
        $ map
          ( \(tome, actions) -> targetLabel tome [chooseOne iid $ map ((\f -> f windows' []) . AbilityLabel iid) actions]
          )
          pairs'
      pure i
    ResolveChaosToken _ ElderSign iid | attrs `is` iid -> do
      push
        $ chooseOne iid
        $ [ targetLabel iid
              $ [ search iid attrs attrs [(Zone.FromDiscard, PutBack)] (CardWithType AssetType <> CardWithTrait Tome)
                    $ DrawFound iid 1
                ]
          , Label "Do not use Daisy's ability" []
          ]
      pure i
    _ -> DaisyWalkerParallel <$> runMessage msg attrs
