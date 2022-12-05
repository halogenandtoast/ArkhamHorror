module Arkham.Location.Cards.EasttownArkhamPoliceStation
  ( EasttownArkhamPoliceStation(..)
  , easttownArkhamPoliceStation
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Uses
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( easttownArkhamPoliceStation )
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message

newtype EasttownArkhamPoliceStation = EasttownArkhamPoliceStation LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

easttownArkhamPoliceStation :: LocationCard EasttownArkhamPoliceStation
easttownArkhamPoliceStation = location
  EasttownArkhamPoliceStation
  Cards.easttownArkhamPoliceStation
  4
  (PerPlayer 2)

instance HasAbilities EasttownArkhamPoliceStation where
  getAbilities (EasttownArkhamPoliceStation attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (PlayerLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ ActionAbility Nothing
          $ ActionCost 1
        | locationRevealed attrs
        ]

instance RunMessage EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      ammoAssets <- map (Ammo, )
        <$> selectList (AssetControlledBy You <> AssetWithUseType Ammo)
      supplyAssets <- map (Supply, )
        <$> selectList (AssetControlledBy You <> AssetWithUseType Supply)
      push $ chooseOne
        iid
        [ targetLabel assetId [AddUses assetId useType' 2]
        | (useType', assetId) <- ammoAssets <> supplyAssets
        ]
      pure l
    _ -> EasttownArkhamPoliceStation <$> runMessage msg attrs
