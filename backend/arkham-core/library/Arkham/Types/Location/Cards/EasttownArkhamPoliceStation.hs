module Arkham.Types.Location.Cards.EasttownArkhamPoliceStation
  ( EasttownArkhamPoliceStation(..)
  , easttownArkhamPoliceStation
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (easttownArkhamPoliceStation)
import Arkham.Types.Ability
import Arkham.Types.Asset.Uses
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Target

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
      [ mkAbility attrs 1 (ActionAbility Nothing $ ActionCost 1)
          & (abilityLimitL .~ PlayerLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env EasttownArkhamPoliceStation where
  runMessage msg l@(EasttownArkhamPoliceStation attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      ammoAssets <- map (Ammo, )
        <$> selectList (AssetOwnedBy You <> AssetWithUseType Ammo)
      supplyAssets <- map (Supply, )
        <$> selectList (AssetOwnedBy You <> AssetWithUseType Supply)
      l <$ push
        (chooseOne
          iid
          [ AddUses (AssetTarget assetId) useType' 2
          | (useType', assetId) <- ammoAssets <> supplyAssets
          ]
        )
    _ -> EasttownArkhamPoliceStation <$> runMessage msg attrs
