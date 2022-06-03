module Arkham.Location.Cards.EasttownArkhamPoliceStation
  ( EasttownArkhamPoliceStation(..)
  , easttownArkhamPoliceStation
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (easttownArkhamPoliceStation)
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message
import Arkham.Target

newtype EasttownArkhamPoliceStation = EasttownArkhamPoliceStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownArkhamPoliceStation :: LocationCard EasttownArkhamPoliceStation
easttownArkhamPoliceStation = location
  EasttownArkhamPoliceStation
  Cards.easttownArkhamPoliceStation
  4
  (PerPlayer 2)
  Moon
  [Circle, Triangle]

instance HasAbilities EasttownArkhamPoliceStation where
  getAbilities (EasttownArkhamPoliceStation attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here (ActionAbility Nothing $ ActionCost 1)
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      ammoAssets <- map (Ammo, )
        <$> selectList (AssetControlledBy You <> AssetWithUseType Ammo)
      supplyAssets <- map (Supply, )
        <$> selectList (AssetControlledBy You <> AssetWithUseType Supply)
      l <$ push
        (chooseOne
          iid
          [ AddUses (AssetTarget assetId) useType' 2
          | (useType', assetId) <- ammoAssets <> supplyAssets
          ]
        )
    _ -> EasttownArkhamPoliceStation <$> runMessage msg attrs
